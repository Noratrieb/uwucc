use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let input_file = std::env::args().nth(1).expect("first argument");
    let src = std::fs::read_to_string(input_file)?;

    parser::parse_file(src);

    Ok(())
}
