fn main() {
    println!("{:?}", std::env::current_dir());
    lalrpop::Configuration::new()
        .process_file("src/lexer/lexer_grammar.lalrpop")
        // .process_file("src/tokenizer/tokenizer_grammar.lalrpop")
        .unwrap();
}
