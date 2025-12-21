use proptest::prelude::*;
use text_size::TextSize;

proptest! {
    #[test]
    fn completion_never_panics(
        data in proptest::collection::vec(any::<u8>(), 0..2048),
        offset in 0usize..2048,
    ) {
        if let Ok(sql) = std::str::from_utf8(&data) {
            let parse = mold_parser::parse(sql);
            let clamped = offset.min(sql.len()) as u32;
            let request =
                mold_completion::CompletionRequest::new(sql, TextSize::new(clamped))
                    .with_parse(&parse);
            let _ = mold_completion::complete(request);
        }
    }
}
