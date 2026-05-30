mod lexer;
use lexer::Lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len()<1 {
        eprintln!("needs a path");
        return;
    }
    let file_data = std::fs::read_to_string(&args[1].clone()).expect("Failed to read file");
    let mut lexer = Lexer::new(&file_data);
    let lexed = lexer.main();
}
