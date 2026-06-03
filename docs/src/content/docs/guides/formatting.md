---
title: Formatting
description: Two formatter engines — sqlstyle and pgFormatter.
sidebar:
  order: 3
---

`banshee format` ships two engines, selected by `[format] style`:

- **`sqlstyle`** — river alignment per the sqlstyle.guide (leading keywords
  right-aligned into a "river").
- **`pgformatter`** — a pgFormatter-style layout, including DDL.
- **`compact`** — minimal, single-line-leaning output.

```sh
banshee format --write .     # rewrite files in place
banshee format --check .     # exit 1 if anything is unformatted (for CI)
banshee format -             # read stdin, write stdout
```

Shared knobs (indent, width, comma style, keyword case, …) map across both
engines — see [Configuration](/banshee/guides/configuration/).
