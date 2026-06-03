fn main() {
    let sql = "select id, name from users where active = true";
    let parse = banshee::parser::parse(sql);
    if !parse.errors().is_empty() {
        eprintln!("parse errors: {:?}", parse.errors());
        return;
    }

    let formatted = banshee_format::format_sqlstyle(sql);
    println!("{formatted}");
}
