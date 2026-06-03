---
title: Templating
description: Lint application SQL that contains parameter placeholders.
sidebar:
  order: 5
---

Application SQL often contains placeholders that aren't valid PostgreSQL on their
own (`:name`, `?`, `%(name)s`). Enable the templater to substitute them before
parsing so the statement still lints; findings map back to the original text and
the placeholders survive `banshee fix` untouched.

```toml
[templater]
style = "colon"   # colon (:name / :1) | question-mark (?) | percent (%s, %(name)s)
```

Native Postgres parameters (`$1`, `$name`) are already valid and are never
touched, and placeholders inside strings, dollar-quotes and comments are left
alone.
