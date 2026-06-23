#!/usr/bin/env python3
"""
RustyLR Release Manager Script
This script merges the functionality of bumping crate versions, running bootstrap tests,
and publishing crates to crates.io with strict verification steps.

WARNING:
  Since the version bumping step modifies Cargo.toml files in place, the git working tree
  will be dirty. It is highly recommended to pass '--allow-dirty' to 'publish' or 'all' commands.

Usage:
  python3 scripts/release.py bump
  python3 scripts/release.py publish [cargo_publish_args...]
  python3 scripts/release.py publish --allow-dirty
  python3 scripts/release.py all [cargo_publish_args...]
  python3 scripts/release.py all --allow-dirty
"""

import argparse
import os
import re
import sys
import subprocess
import shutil

# The publishing order of packages based on internal dependencies
PUBLISH_ORDER = [
    "rusty_lr_core",
    "rusty_lr_parser",
    "rusty_lr_derive",
    "rusty_lr_buildscript",
    "rusty_lr_executable",
    "rusty_lr"
]

# ==============================================================================
# INTENT 1: Version Bumping
# Automatically increments the minor version by 1 and resets the patch version to 0
# for all workspace packages (excluding examples), updating all dependency versions
# and internal references dynamically.
# ==============================================================================

def get_all_cargo_toml_files(root_dir):
    """Recursively search for Cargo.toml files in the workspace, excluding target, git, and examples."""
    cargo_files = []
    for root, dirs, files in os.walk(root_dir):
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
    """Parse name and version from a Cargo.toml file under the [package] section."""
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

def calculate_bumped_version(version_str):
    """Increment minor version by 1 and set patch version to 0 (e.g., 3.36.0 -> 3.37.0)."""
    parts = version_str.split('.')
    if len(parts) >= 2:
        major = parts[0]
        minor = int(parts[1])
        return f"{major}.{minor + 1}.0"
    return version_str

def update_vscode_extension_versions(root_dir, old_version, new_version):
    """Update the VSCode extension version, required rustylr server version, and matching docs."""
    package_json_path = os.path.join(root_dir, 'editors', 'vscode-rustylr', 'package.json')
    readme_path = os.path.join(root_dir, 'editors', 'vscode-rustylr', 'README.md')
    
    if os.path.exists(package_json_path):
        with open(package_json_path, 'r', encoding='utf-8') as f:
            package_content = f.read()
            
        updated_package = False
        version_pattern = r'("version"\s*:\s*")[^"]+(")'
        if re.search(version_pattern, package_content):
            package_content = re.sub(version_pattern, rf'\g<1>{new_version}\g<2>', package_content, count=1)
            updated_package = True
            print(
                f"Updated VSCode extension package version in "
                f"{os.path.relpath(package_json_path, root_dir)}: {old_version} -> {new_version}"
            )
        else:
            print(
                f"Warning: Could not find extension version in "
                f"{os.path.relpath(package_json_path, root_dir)}"
            )
            
        server_pattern = r'("requiredServerVersion"\s*:\s*")[^"]+(")'
        if re.search(server_pattern, package_content):
            package_content = re.sub(server_pattern, rf'\g<1>{new_version}\g<2>', package_content)
            updated_package = True
            print(
                f"Updated VSCode extension requiredServerVersion in "
                f"{os.path.relpath(package_json_path, root_dir)}: {old_version} -> {new_version}"
            )
        else:
            print(
                f"Warning: Could not find requiredServerVersion in "
                f"{os.path.relpath(package_json_path, root_dir)}"
            )
            
        if updated_package:
            with open(package_json_path, 'w', encoding='utf-8') as f:
                f.write(package_content)
    else:
        print(f"Warning: {package_json_path} does not exist.")
        
    if os.path.exists(readme_path):
        with open(readme_path, 'r', encoding='utf-8') as f:
            readme_content = f.read()
            
        pattern = r'(cargo install rustylr --version\s+)[^\s]+(\s+--force)'
        if re.search(pattern, readme_content):
            readme_content = re.sub(pattern, rf'\g<1>{new_version}\g<2>', readme_content)
            with open(readme_path, 'w', encoding='utf-8') as f:
                f.write(readme_content)
            print(
                f"Updated VSCode extension README server install command in "
                f"{os.path.relpath(readme_path, root_dir)}: {old_version} -> {new_version}"
            )
        else:
            print(
                f"Warning: Could not find VSCode extension server install command in "
                f"{os.path.relpath(readme_path, root_dir)}"
            )
    else:
        print(f"Warning: {readme_path} does not exist.")

def run_bump(root_dir):
    """Bumps all project versions and updates their inter-dependencies."""
    print("=== STARTING VERSION BUMP ===")
    cargo_files = get_all_cargo_toml_files(root_dir)
    
    # Exclude the workspace root Cargo.toml itself
    root_cargo = os.path.join(root_dir, 'Cargo.toml')
    if root_cargo in cargo_files:
        cargo_files.remove(root_cargo)
        
    print(f"Found {len(cargo_files)} project Cargo.toml manifests.")
    
    # Map each package name to its current and bumped versions
    package_map = {}
    for file_path in cargo_files:
        name, version = parse_package_info(file_path)
        if name and version:
            new_version = calculate_bumped_version(version)
            package_map[name] = {
                'file_path': file_path,
                'old_version': version,
                'new_version': new_version
            }
            
    print("\nPlanned version bumps:")
    for name, info in package_map.items():
        print(f"  {name}: {info['old_version']} -> {info['new_version']}")
        
    # Update Cargo.toml files (package version and dependencies)
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
            if line_stripped.startswith('[') and line_stripped.endswith(']'):
                current_section = line_stripped[1:-1].strip()
                new_lines.append(line)
                continue
                
            # Update package version
            if current_section == 'package' and line_stripped.startswith('version'):
                pattern = rf'(version\s*=\s*")[^"]+(")'
                updated_line = re.sub(pattern, rf'\g<1>{new_ver}\g<2>', line)
                new_lines.append(updated_line)
                continue
                
            # Update dependency version if any workspace members are referenced
            updated = False
            for dep_name, dep_info in package_map.items():
                if dep_name in line:
                    dep_old_ver = dep_info['old_version']
                    dep_new_ver = dep_info['new_version']
                    
                    # Pattern for table style: dep_name = { version = "X.Y.Z", ... }
                    table_pattern = rf'(\b{re.escape(dep_name)}\b.*?version\s*=\s*")[^"]+(")'
                    if re.search(table_pattern, line):
                        line = re.sub(table_pattern, rf'\g<1>{dep_new_ver}\g<2>', line)
                        updated = True
                        
                    # Pattern for string style: dep_name = "X.Y.Z"
                    simple_pattern = rf'(\b{re.escape(dep_name)}\b\s*=\s*")[^"]+(")'
                    if not updated and re.search(simple_pattern, line):
                        line = re.sub(simple_pattern, rf'\g<1>{dep_new_ver}\g<2>', line)
                        updated = True
                        
            new_lines.append(line)
            
        with open(file_path, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)
            
        print(f"Updated package version in {os.path.relpath(file_path, root_dir)}")
        
    # Also update target_rusty_lr_version() inside rusty_lr_parser/src/lib.rs
    # to maintain consistency with the new version of rusty_lr.
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
                
    rustylr_info = package_map.get('rustylr')
    if rustylr_info:
        update_vscode_extension_versions(
            root_dir,
            rustylr_info['old_version'],
            rustylr_info['new_version']
        )
                
    print("\n=== SUMMARY OF VERSION CHANGES ===")
    for name, info in package_map.items():
        print(f"  {name: <25}: {info['old_version']} -> {info['new_version']}")
    print("===================================")
    print("=== VERSION BUMP COMPLETE ===\n")
    return package_map


# ==============================================================================
# INTENT 2: Bootstrap Testing
# Runs bootstrap test suite. If tests fail, exits immediately with non-zero code.
# ==============================================================================

def run_bootstrap(root_dir):
    """Runs the workspace bootstrap test script to ensure all tests pass."""
    print("=== STARTING BOOTSTRAP TEST ===")
    cmd = ["./scripts/bootstrap_test.sh"]
    print(f"Running '{' '.join(cmd)}' in {root_dir}...")
    result = subprocess.run(cmd, cwd=root_dir)
    if result.returncode != 0:
        print("\nError: Bootstrap tests failed. Aborting pipeline.")
        sys.exit(1)
    print("=== BOOTSTRAP TEST PASSED ===\n")


# ==============================================================================
# INTENT 3: Automatic Git Commit
# Automatically stages and commits all changes (Cargo.toml manifests, lib.rs)
# after a successful release run, with a detailed summary message.
# ==============================================================================

def run_git_commit(root_dir, package_map):
    """Stages and commits all version changes to git and tags the release with the new version of rusty_lr."""
    print("=== STARTING GIT COMMIT & TAG ===")
    
    # Construct a detailed commit message outlining the bumped versions
    commit_msg = "Release: Bump versions and execute cargo publish\n\nVersion updates:\n"
    for name, info in package_map.items():
        commit_msg += f"- {name}: {info['old_version']} -> {info['new_version']}\n"
    rustylr_info = package_map.get('rustylr')
    if rustylr_info:
        commit_msg += (
            f"- vscode extension: "
            f"{rustylr_info['old_version']} -> {rustylr_info['new_version']}\n"
        )
        
    try:
        # Run 'git add .'
        print("Running 'git add .'...")
        subprocess.run(["git", "add", "."], cwd=root_dir, check=True)
        
        # Run 'git commit -m ...'
        print("Running 'git commit -m ...'")
        subprocess.run(["git", "commit", "-m", commit_msg], cwd=root_dir, check=True)
        print("Git commit completed successfully!")
        
        # Tag the release with the new version of rusty_lr
        rusty_lr_info = package_map.get('rusty_lr')
        if rusty_lr_info:
            new_version = rusty_lr_info['new_version']
            tag_name = f"v{new_version}"
            print(f"Creating git tag: {tag_name}...")
            subprocess.run(["git", "tag", "-a", tag_name, "-m", f"Release {tag_name}"], cwd=root_dir, check=True)
            print(f"Git tag {tag_name} created successfully!")
            
    except subprocess.CalledProcessError as e:
        print(f"Error during git operations: {e}")
    print("=== GIT COMMIT & TAG COMPLETE ===\n")


# ==============================================================================
# INTENT 4: Workspace Crate Publishing
# Cargo publish requires that dependencies are either published or can be resolved.
# When publishing multiple interdependent workspace packages, we temporarily
# comment out all other members from the root Cargo.toml workspace definition,
# publish the current package, and move to the next.
# A backup of root Cargo.toml is automatically created and restored at the end
# (or in case of errors) to ensure clean repository state.
# ==============================================================================

def comment_out_workspace_members(content, keep_member):
    """Modify the workspace members array, commenting out all members except keep_member."""
    lines = content.splitlines()
    new_lines = []
    in_members = False
    for line in lines:
        line_stripped = line.strip()
        if 'members' in line_stripped and '=' in line_stripped:
            in_members = True
            new_lines.append(line)
            continue
        if in_members and line_stripped.startswith(']'):
            in_members = False
            new_lines.append(line)
            continue
            
        if in_members:
            match = re.search(r'["\']([^"\']+)["\']', line_stripped)
            if match:
                member = match.group(1)
                indent = line[:len(line) - len(line.lstrip())]
                content_part = line.lstrip()
                if member != keep_member:
                    if not content_part.startswith('#'):
                        new_lines.append(f"{indent}# {content_part}")
                    else:
                        new_lines.append(line)
                else:
                    if content_part.startswith('#'):
                        uncommented = content_part[1:].lstrip()
                        new_lines.append(f"{indent}{uncommented}")
                    else:
                        new_lines.append(line)
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)
    return '\n'.join(new_lines) + '\n'

def run_publish(root_dir, cargo_args):
    """Publishes all workspace packages in the correct dependency order."""
    print("=== STARTING WORKSPACE PUBLISH ===")
    root_cargo_path = os.path.join(root_dir, 'Cargo.toml')
    backup_cargo_path = os.path.join(root_dir, 'Cargo.toml.bak')
    
    # Clean up previous backup if any exists
    if os.path.exists(backup_cargo_path):
        print(f"Warning: backup file {backup_cargo_path} already exists. Restoring it first...")
        shutil.copyfile(backup_cargo_path, root_cargo_path)
        os.remove(backup_cargo_path)
        
    # Create backup of the original root Cargo.toml
    shutil.copyfile(root_cargo_path, backup_cargo_path)
    
    try:
        for project in PUBLISH_ORDER:
            # Retrieve version of this specific project
            project_cargo_path = os.path.join(root_dir, project, 'Cargo.toml')
            _, version = parse_package_info(project_cargo_path)
            
            cmd = ["cargo", "publish"] + cargo_args
            
            print(f"\n==========================================")
            print(f"Preparing to publish package: {project}")
            print(f"==========================================")
            print(f"  Package Name     : {project}")
            print(f"  Version          : {version}")
            print(f"  Command          : {' '.join(cmd)}")
            print(f"  Target Directory : {os.path.join(root_dir, project)}")
            print(f"==========================================")
            
            # Request user confirmation
            print("\nWARNING: This action will publish to crates.io (unless --dry-run is active).")
            try:
                response = input(f"Confirm: Do you want to publish '{project} v{version}'? [y/N]: ").strip().lower()
            except EOFError:
                # Fallback if standard input is not available/interactive
                print("EOFError: standard input is not available. Aborting for safety.")
                sys.exit(1)
                
            if response != 'y':
                print(f"Publishing of {project} cancelled by user. Aborting publish loop.")
                sys.exit(0)
            
            # Read backup content (original) and comment out other workspace members
            with open(backup_cargo_path, 'r', encoding='utf-8') as f:
                original_content = f.read()
            modified_content = comment_out_workspace_members(original_content, project)
            with open(root_cargo_path, 'w', encoding='utf-8') as f:
                f.write(modified_content)
                
            # Run cargo publish
            print(f"Running '{' '.join(cmd)}' in {os.path.join(root_dir, project)}...")
            result = subprocess.run(cmd, cwd=os.path.join(root_dir, project))
            if result.returncode != 0:
                print(f"\nError: Failed to publish {project}. Aborting publish loop.")
                sys.exit(1)
                
            print(f"Successfully processed {project}.")
            
        print("\nAll projects processed successfully!")
        
    finally:
        # Guarantee the restoration of the root Cargo.toml
        if os.path.exists(backup_cargo_path):
            print(f"\nRestoring original Cargo.toml from backup...")
            shutil.copyfile(backup_cargo_path, root_cargo_path)
            os.remove(backup_cargo_path)
            print("Restore complete.")
    print("=== WORKSPACE PUBLISH COMPLETE ===")

# ==============================================================================
# INTENT 5: Pre-flight Git Status Check
# Enforces that the repository working directory is clean of unstaged/staged
# changes before starting the version bump and publish pipeline, preventing
# accidental or incorrect commits.
# ==============================================================================

def check_git_clean(root_dir):
    """Exit immediately if there are any uncommitted or unstaged changes in the repository."""
    # Check for unstaged changes to tracked files
    unstaged_check = subprocess.run(["git", "diff", "--quiet"], cwd=root_dir)
    # Check for staged changes to tracked files
    staged_check = subprocess.run(["git", "diff", "--cached", "--quiet"], cwd=root_dir)
    
    if unstaged_check.returncode != 0 or staged_check.returncode != 0:
        print("\n" + "!" * 80)
        print("ERROR: Git working directory is not clean (uncommitted or unstaged changes exist).")
        print("Please commit, revert, or stash your changes before running this script.")
        print("!" * 80 + "\n")
        sys.exit(1)


# ==============================================================================
# MAIN ENTRYPOINT & CLI PARSING
# ==============================================================================

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    root_dir = os.path.abspath(os.path.join(script_dir, '..'))
    
    # SAFETY WARNING: Bumping versions modifies Cargo.toml files, which makes the git directory dirty.
    # Therefore, 'cargo publish' will fail unless the '--allow-dirty' flag is specified.
    parser = argparse.ArgumentParser(
        description="RustyLR Workspace Release Manager - Combines version bumping, testing, and package publishing.\n\n"
                    "WARNING: Publishing modifies Cargo.toml files in place, making the git working directory dirty.\n"
                    "You should specify '--allow-dirty' when running 'publish' or 'all' commands to prevent cargo publish from failing.",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "command",
        choices=["bump", "publish", "all"],
        help="Action to perform: 'bump' (version bump), 'publish' (sequence publish), or 'all' (bump -> bootstrap -> publish)."
    )
    
    # parse_known_args returns the parsed args namespace and a list of remaining arguments
    args, cargo_args = parser.parse_known_args()
    
    # Check for --allow-dirty warning if command involves publishing
    if args.command in ["publish", "all"]:
        if "--allow-dirty" not in cargo_args:
            print("\n" + "!" * 80)
            print("WARNING: '--allow-dirty' is not specified.")
            print("Since the release script modifies Cargo.toml files in place, the git working tree")
            print("is dirty, and 'cargo publish' will fail unless '--allow-dirty' is provided.")
            print("!" * 80 + "\n")
            
    # Check git status for commands modifying versions/code
    if args.command in ["bump", "all"]:
        check_git_clean(root_dir)
            
    if args.command == "bump":
        run_bump(root_dir)
    elif args.command == "publish":
        run_publish(root_dir, cargo_args)
    elif args.command == "all":
        package_map = run_bump(root_dir)
        run_bootstrap(root_dir)
        run_publish(root_dir, cargo_args)
        run_git_commit(root_dir, package_map)

if __name__ == '__main__':
    main()
