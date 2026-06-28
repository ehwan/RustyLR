#[test]
fn context_clone_bounds_are_conditional() {
    let tests = trybuild::TestCases::new();
    tests.pass("tests/ui/context_clone_non_clone_storage_compiles.rs");
    tests.pass("tests/ui/context_clone_clone_storage_clones.rs");
    tests.compile_fail("tests/ui/context_clone_non_clone_storage_fails.rs");
}
