#!/bin/sh
cargo build --release --target wasm32-unknown-unknown
wasm-pack build --target web --out-dir ./web --no-typescript
