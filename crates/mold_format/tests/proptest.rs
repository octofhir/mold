use proptest::prelude::*;

proptest! {
    #[test]
    fn format_is_idempotent(data in proptest::collection::vec(any::<u8>(), 0..2048)) {
        if let Ok(sql) = std::str::from_utf8(&data) {
            let once = mold_format::format_sqlstyle(sql);
            let twice = mold_format::format_sqlstyle(&once);
            prop_assert_eq!(once, twice);
        }
    }

    #[test]
    fn format_roundtrip_preserves_parse_success(data in proptest::collection::vec(any::<u8>(), 0..2048)) {
        if let Ok(sql) = std::str::from_utf8(&data) {
            let parse = mold_parser::parse(sql);
            if parse.errors().is_empty() {
                let formatted = mold_format::format_sqlstyle(sql);
                let formatted_parse = mold_parser::parse(&formatted);
                prop_assert!(formatted_parse.errors().is_empty());
            }
        }
    }
}
