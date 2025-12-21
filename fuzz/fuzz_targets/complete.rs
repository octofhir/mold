#![no_main]

use libfuzzer_sys::fuzz_target;
use text_size::TextSize;

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    let (offset_bytes, sql_bytes) = if data.len() >= 4 {
        data.split_at(4)
    } else {
        (data, &[][..])
    };

    let mut buf = [0u8; 4];
    for (idx, byte) in offset_bytes.iter().enumerate() {
        buf[idx] = *byte;
    }
    let offset = u32::from_le_bytes(buf);

    if let Ok(sql) = std::str::from_utf8(sql_bytes) {
        let parse = mold_parser::parse(sql);
        let offset = (offset as usize).min(sql.len()) as u32;
        let request =
            mold_completion::CompletionRequest::new(sql, TextSize::new(offset)).with_parse(&parse);
        let _ = mold_completion::complete(request);
    }
});
