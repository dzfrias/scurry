use parser::Position;

pub mod ast;
pub mod lexer;
pub mod parser;

pub fn format_position(source: &str, pos: Position) -> String {
    let mut res = String::new();
    let line = pos.line();
    res.push_str(&format!(
        "{}: {}",
        line,
        source
            .lines()
            .nth(line - 1)
            .expect("Line should be in source")
    ));
    res.push('\n');
    let range = pos.range();
    res.push_str(&(" ".repeat(line.to_string().len()) + "  "));
    res.push_str(&" ".repeat(range.start));
    res.push_str(&"^".repeat(range.len()));
    res
}
