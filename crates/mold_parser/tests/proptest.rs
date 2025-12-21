use proptest::prelude::*;

proptest! {
    #[test]
    fn parse_never_panics(data in proptest::collection::vec(any::<u8>(), 0..2048)) {
        if let Ok(sql) = std::str::from_utf8(&data) {
            let _ = mold_parser::parse(sql);
        }
    }
}
