---
title: Install
description: Install banshee on macOS, Linux or Windows.
---

## Prebuilt binary (recommended)

The installer detects your platform and verifies the download's SHA-256:

```sh
curl -fsSL https://raw.githubusercontent.com/octofhir/banshee/main/install.sh | sh
```

It installs to `~/.local/bin` by default; override with `BANSHEE_INSTALL_DIR`.
Windows users can download the `.zip` from the
[releases page](https://github.com/octofhir/banshee/releases/latest).

## With a Rust toolchain

```sh
cargo binstall banshee     # fetch the prebuilt release archive
cargo install banshee      # or build from source via crates.io
```

## From source

```sh
git clone https://github.com/octofhir/banshee
cd banshee
cargo build --release -p banshee                 # parser, formatter, linter, LSP
cargo build --release -p banshee --features db   # + live schema introspection
```

The binary is `target/release/banshee`. The `db` feature pulls in `sqlx`/`tokio`;
leave it off and the binary stays async-free, using a cached schema if present.
