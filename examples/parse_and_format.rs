fn main() {
    let sql = "select id, name from users where active = true";
    let parse = mold::parser::parse(sql);
    if !parse.errors().is_empty() {
        eprintln!("parse errors: {:?}", parse.errors());
        return;
    }

    let formatted = mold_format::format_sqlstyle(sql);
    println!("{formatted}");
}
