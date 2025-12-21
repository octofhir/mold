use criterion::{Criterion, black_box, criterion_group, criterion_main};
use text_size::TextSize;

fn bench_completion(c: &mut Criterion) {
    let sql = "SELECT id, name FROM users WHERE ";
    let parse = mold_parser::parse(sql);

    let offset_select = TextSize::new("SELECT ".len() as u32);
    let offset_from = TextSize::new("SELECT id, name FROM ".len() as u32);
    let offset_where = TextSize::new(sql.len() as u32);

    c.bench_function("complete_select_list", |b| {
        b.iter(|| {
            let request = mold_completion::CompletionRequest::new(black_box(sql), offset_select)
                .with_parse(&parse);
            let _ = mold_completion::complete(request);
        })
    });

    c.bench_function("complete_from", |b| {
        b.iter(|| {
            let request = mold_completion::CompletionRequest::new(black_box(sql), offset_from)
                .with_parse(&parse);
            let _ = mold_completion::complete(request);
        })
    });

    c.bench_function("complete_where", |b| {
        b.iter(|| {
            let request = mold_completion::CompletionRequest::new(black_box(sql), offset_where)
                .with_parse(&parse);
            let _ = mold_completion::complete(request);
        })
    });
}

criterion_group!(benches, bench_completion);
criterion_main!(benches);
