mod lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len()<1 {
        eprintln!("needs a path");
        return;
    }
    let fileData = std::fs::read_to_string(args[0].clone()).expect("Failed to read file");
}
