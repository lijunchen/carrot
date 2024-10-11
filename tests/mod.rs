use std::path::PathBuf;

use carrot::grammar;
use carrot::typer;
#[test]
fn e2e() -> anyhow::Result<()> {
    let p = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let test_dir = p.join("tests/test_data");
    let parser = grammar::FileParser::new();
    for entry in std::fs::read_dir(test_dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
        {
            let file = &entry.path();
            dbg!(&file);
            let content = std::fs::read_to_string(file).unwrap();
            let ast = parser.parse(&content).unwrap();
            expect_test::expect_file![file.with_extension("src.ast")].assert_debug_eq(&ast);
            let mut typer = typer::Typer::new();
            let tast = typer.infer(&ast);
            expect_test::expect_file![file.with_extension("src.tast")].assert_debug_eq(&tast);
        }
    }
    Ok(())
}
