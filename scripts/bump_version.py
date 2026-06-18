#!/usr/bin/env python3
import os
import re

def get_all_cargo_toml_files(root_dir):
    cargo_files = []
    for root, dirs, files in os.walk(root_dir):
        # Exclude directories
        if 'target' in dirs:
            dirs.remove('target')
        if '.git' in dirs:
            dirs.remove('.git')
        if 'example' in dirs:
            dirs.remove('example')
            
        for file in files:
            if file == 'Cargo.toml':
                cargo_files.append(os.path.join(root, file))
    return cargo_files

def parse_package_info(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
        
    package_name = None
    package_version = None
    current_section = None
    
    for line in content.splitlines():
        line_stripped = line.strip()
        if line_stripped.startswith('[') and line_stripped.endswith(']'):
            current_section = line_stripped[1:-1].strip()
        elif current_section == 'package':
            if line_stripped.startswith('name'):
                parts = line_stripped.split('=', 1)
                package_name = parts[1].strip().strip('"').strip("'")
            elif line_stripped.startswith('version'):
                parts = line_stripped.split('=', 1)
                package_version = parts[1].strip().strip('"').strip("'")
                
    return package_name, package_version

def bump_version(version_str):
    parts = version_str.split('.')
    if len(parts) >= 2:
        major = parts[0]
        minor = int(parts[1])
        return f"{major}.{minor + 1}.0"
    return version_str

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    root_dir = os.path.abspath(os.path.join(script_dir, '..'))
    
    print(f"Scanning for Cargo.toml files in: {root_dir}")
    cargo_files = get_all_cargo_toml_files(root_dir)
    # Exclude root Cargo.toml as it has no package section
    root_cargo = os.path.join(root_dir, 'Cargo.toml')
    if root_cargo in cargo_files:
        cargo_files.remove(root_cargo)
        
    print(f"Found {len(cargo_files)} project Cargo.toml files (excluding examples).")
    
    package_map = {}
    for file_path in cargo_files:
        name, version = parse_package_info(file_path)
        if name and version:
            new_version = bump_version(version)
            package_map[name] = {
                'file_path': file_path,
                'old_version': version,
                'new_version': new_version
            }
            
    print("\nPlanned version bumps:")
    for name, info in package_map.items():
        print(f"  {name}: {info['old_version']} -> {info['new_version']}")
        
    # Perform the updates
    for name, info in package_map.items():
        file_path = info['file_path']
        old_ver = info['old_version']
        new_ver = info['new_version']
        
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            
        new_lines = []
        current_section = None
        
        for line in lines:
            line_stripped = line.strip()
            # Track current section
            if line_stripped.startswith('[') and line_stripped.endswith(']'):
                current_section = line_stripped[1:-1].strip()
                new_lines.append(line)
                continue
                
            # Update package version
            if current_section == 'package' and line_stripped.startswith('version'):
                # Replace version = "old_ver" with version = "new_ver"
                pattern = rf'(version\s*=\s*")[^"]+(")'
                updated_line = re.sub(pattern, rf'\g<1>{new_ver}\g<2>', line)
                new_lines.append(updated_line)
                continue
                
            # Update dependency version if applicable
            updated = False
            for dep_name, dep_info in package_map.items():
                if dep_name in line:
                    # Look for: dep_name = { ... version = "old_ver" ... }
                    # Or: dep_name = "old_ver"
                    dep_old_ver = dep_info['old_version']
                    dep_new_ver = dep_info['new_version']
                    
                    # Pattern for table dependency (e.g., dep = { version = "X.Y.Z", ... })
                    table_pattern = rf'(\b{re.escape(dep_name)}\b.*?version\s*=\s*")[^"]+(")'
                    if re.search(table_pattern, line):
                        line = re.sub(table_pattern, rf'\g<1>{dep_new_ver}\g<2>', line)
                        updated = True
                        
                    # Pattern for simple dependency (e.g., dep = "X.Y.Z")
                    simple_pattern = rf'(\b{re.escape(dep_name)}\b\s*=\s*")[^"]+(")'
                    if not updated and re.search(simple_pattern, line):
                        line = re.sub(simple_pattern, rf'\g<1>{dep_new_ver}\g<2>', line)
                        updated = True
                        
            new_lines.append(line)
            
        with open(file_path, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)
            
        print(f"Updated {os.path.relpath(file_path, root_dir)}")
        
    # Also update target_rusty_lr_version() in rusty_lr_parser/src/lib.rs
    rusty_lr_info = package_map.get('rusty_lr')
    if rusty_lr_info:
        old_ver = rusty_lr_info['old_version']
        new_ver = rusty_lr_info['new_version']
        
        old_parts = old_ver.split('.')
        new_parts = new_ver.split('.')
        if len(old_parts) >= 3 and len(new_parts) >= 3:
            old_tuple = f"({old_parts[0]}, {old_parts[1]}, {old_parts[2]})"
            new_tuple = f"({new_parts[0]}, {new_parts[1]}, {new_parts[2]})"
            
            lib_rs_path = os.path.join(root_dir, 'rusty_lr_parser', 'src', 'lib.rs')
            if os.path.exists(lib_rs_path):
                with open(lib_rs_path, 'r', encoding='utf-8') as f:
                    lib_content = f.read()
                    
                # Regex matching (old_major, old_minor, old_patch) tuple
                pattern = r'\(\s*' + re.escape(old_parts[0]) + r'\s*,\s*' + re.escape(old_parts[1]) + r'\s*,\s*' + re.escape(old_parts[2]) + r'\s*\)'
                if re.search(pattern, lib_content):
                    lib_content = re.sub(pattern, new_tuple, lib_content)
                    with open(lib_rs_path, 'w', encoding='utf-8') as f:
                        f.write(lib_content)
                    print(f"Updated target_rusty_lr_version in {os.path.relpath(lib_rs_path, root_dir)} to {new_tuple}")
                else:
                    print(f"Warning: Could not find matching version tuple {old_tuple} in {lib_rs_path}")
            else:
                print(f"Warning: {lib_rs_path} does not exist.")
                
    print("\nVersion bumping complete!")

if __name__ == '__main__':
    main()
