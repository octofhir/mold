//! Stamps the build with the current git short hash and build date so
//! `mold --version` reports more than the crate version.

use std::process::Command;

fn main() {
    let sha = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    // ISO date without pulling in a date crate.
    let date = Command::new("date")
        .args(["+%Y-%m-%d"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=MOLD_GIT_SHA={sha}");
    println!("cargo:rustc-env=MOLD_BUILD_DATE={date}");
    // Re-stamp when HEAD moves.
    println!("cargo:rerun-if-changed=../../.git/HEAD");
}
