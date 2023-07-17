use std::{
    fs::File,
    io::{Read, BufWriter, Write},
};

use wasmlang::parser::{generate_ast, generate_wasm};

fn main() {
    let source = read_source();
    let ast = generate_ast(&source).unwrap();

    let wasm = generate_wasm(ast);

    let mut dest = output_file();
    dest.write_all(&wasm).unwrap();
    
}

fn read_source() -> String {
    let mut file = File::open("source/index.wasl").unwrap();

    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();

    buf
}

fn output_file() -> impl Write {
    let file = File::create("outputs/test.wasm").unwrap();

    BufWriter::new(file)
}
