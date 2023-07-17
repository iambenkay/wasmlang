const MAGIC_WASM_HEADER: [u8; 4] = *b"\0asm";
const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

/* Section codes */
const TYPE_SECTION: u8 = 0x01;
const FUNC_SECTION: u8 = 0x03;
const EXPORT_SECTION: u8 = 0x07;
const CODE_SECTION: u8 = 0x0a;

/* EXPORT TYPES */
pub const EXPORT_FUNC: u8 = 0x00;
pub const EXPORT_TABLE: u8 = 0x01;
pub const EXPORT_MEM: u8 = 0x02;
pub const EXPORT_GLOBAL: u8 = 0x03;

/* CONST CODES */
pub const I32_CONST: u8 = 0x41;
pub const I64_CONST: u8 = 0x42;
pub const F32_CONST: u8 = 0x43;
pub const F64_CONST: u8 = 0x44;

/* Type codes */
const FUNC_TYPE: u8 = 0x60;
pub const NUM_TYPE_I64: u8 = 0x7e;
pub const NUM_TYPE_F64: u8 = 0x7c;

/* Instruction codes */
const END_INSTR: u8 = 0x0b;
const IF_INSTR: u8 = 0x04;
const ELSE_INSTR: u8 = 0x05;
const LOCAL_GET_INSTR: u8 = 0x20;
const LOCAL_SET_INSTR: u8 = 0x21;
pub const I64_ADD: u8 = 0x7c;
pub const I64_SUB: u8 = 0x7d;
pub const I64_MUL: u8 = 0x7e;
pub const I64_DIV: u8 = 0x7f;
pub const F64_ADD: u8 = 0xa0;
pub const F64_SUB: u8 = 0xa1;
pub const F64_MUL: u8 = 0xa2;
pub const F64_DIV: u8 = 0xa3;
pub const RETURN_INSTR: u8 = 0x0f;

pub trait Vector {
    fn bytes(&self) -> Vec<u8>;
}

impl Vector for Vec<u8> {
    fn bytes(&self) -> Vec<u8> {
        let mut bytes = vec![self.len() as u8];
        bytes.append(&mut self.to_owned());
        bytes
    }
}

impl Vector for Vec<Vec<u8>> {
    fn bytes(&self) -> Vec<u8> {
        let mut bytes = vec![self.len() as u8];
        bytes.append(&mut flatten(self.to_owned()));
        bytes
    }
}

pub struct Functype {
    pub params: Vec<u8>,
    pub returns: Vec<u8>,
}

impl ToBytes for Functype {
    fn to_bytes(&self) -> Vec<u8> {
        functype(self.params.to_owned(), self.returns.to_owned())
    }
}

pub struct Instr {
    pub result_type: u8,
    pub body: Vec<u8>,
}

pub struct Code {
    pub locals: Vec<u8>,
    pub body: Vec<u8>,
}

impl ToBytes for Code {
    fn to_bytes(&self) -> Vec<u8> {
        func(self.locals.to_owned(), self.body.to_owned())
    }
}

pub struct Codesection {
    pub codes: Vec<Code>,
}

impl ToBytes for Codesection {
    fn to_bytes(&self) -> Vec<u8> {
        let codes = self
            .codes
            .iter()
            .map(|code| code.to_bytes())
            .collect::<Vec<Vec<u8>>>();

        codesec(vector(codes))
    }
}

pub struct Export {
    pub name: String,
    pub desc: Vec<u8>,
}

impl ToBytes for Export {
    fn to_bytes(&self) -> Vec<u8> {
        export(&self.name, self.desc.to_owned())
    }
}

pub struct Exportsection {
    pub exports: Vec<Export>,
}

impl ToBytes for Exportsection {
    fn to_bytes(&self) -> Vec<u8> {
        let exports = self
            .exports
            .iter()
            .map(|export| export.to_bytes())
            .collect::<Vec<Vec<u8>>>();

        exportsec(vector(exports))
    }
}

pub struct Functionsection {
    pub indices: Vec<u8>,
}

impl ToBytes for Functionsection {
    fn to_bytes(&self) -> Vec<u8> {
        funcsec(vector(self.indices.to_owned()))
    }
}

pub struct Typesection {
    pub types: Vec<Functype>,
}

impl ToBytes for Typesection {
    fn to_bytes(&self) -> Vec<u8> {
        let funcs = self
            .types
            .iter()
            .map(|func| func.to_bytes())
            .collect::<Vec<Vec<u8>>>();

        typesec(vector(funcs))
    }
}

pub trait ToBytes {
    fn to_bytes(&self) -> Vec<u8>;
}

pub fn magic() -> Vec<u8> {
    let mut beginning: Vec<u8> = vec![];

    beginning.append(&mut MAGIC_WASM_HEADER.to_vec());
    beginning.append(&mut WASM_VERSION.to_vec());

    beginning
}

fn functype(params: Vec<u8>, returns: Vec<u8>) -> Vec<u8> {
    let mut func: Vec<u8> = vec![FUNC_TYPE];
    let mut params = vector(params);
    let mut returns = vector(returns);

    func.append(&mut params);
    func.append(&mut returns);

    func
}

fn vector(content: impl Vector) -> Vec<u8> {
    content.bytes()
}

fn section(section_id: u8, mut content: Vec<u8>) -> Vec<u8> {
    let mut section: Vec<u8> = vec![section_id, content.len() as u8];

    section.append(&mut content);

    section
}

fn typesec(content: Vec<u8>) -> Vec<u8> {
    section(TYPE_SECTION, content)
}

fn funcsec(content: Vec<u8>) -> Vec<u8> {
    section(FUNC_SECTION, content)
}

fn exportsec(content: Vec<u8>) -> Vec<u8> {
    section(EXPORT_SECTION, content)
}

fn codesec(content: Vec<u8>) -> Vec<u8> {
    section(CODE_SECTION, content)
}

pub fn valtype(typename: &str) -> Option<u8> {
    match typename {
        "Int" => Some(NUM_TYPE_I64),
        "Float" => Some(NUM_TYPE_F64),
        _ => None,
    }
}

fn export(name: &str, mut content: Vec<u8>) -> Vec<u8> {
    let mut exp: Vec<u8> = vector(name.as_bytes().to_vec());

    exp.append(&mut content);
    exp
}

pub fn local_get(index: u8) -> Vec<u8> {
    vec![LOCAL_GET_INSTR, index]
}

pub fn local_set(index: u8) -> Vec<u8> {
    vec![LOCAL_SET_INSTR, index]
}

fn flatten(elems: Vec<Vec<u8>>) -> Vec<u8> {
    elems
        .iter()
        .flat_map(|array| array.into_iter().cloned())
        .collect()
}

fn func(locals: Vec<u8>, mut instructions: Vec<u8>) -> Vec<u8> {
    let mut fnc: Vec<u8> = vec![];
    let locals: Vec<Vec<u8>> = locals.iter().map(|i| vec![0x01, *i]).collect();

    fnc.append(&mut vector(locals));
    fnc.append(&mut instructions);
    fnc.push(END_INSTR);

    let mut bytes = vec![fnc.len() as u8];

    bytes.append(&mut fnc);
    bytes
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_vector() {
        let vec = vec![1, 2, 3];
        let bytes = super::vector(vec);

        assert_eq!(bytes, vec![3, 1, 2, 3]);
    }

    #[test]
    fn test_vector_nested() {
        let arr = vec![vec![1, 2], vec![2, 3], vec![3, 4]];
        let bytes = super::vector(arr);

        assert_eq!(bytes, vec![3, 1, 2, 2, 3, 3, 4]);
    }

    #[test]
    fn test_flatten() {
        let arr = vec![vec![1, 2], vec![2, 3], vec![3, 4]];

        let bytes = super::flatten(arr);
        assert_eq!(bytes, vec![1, 2, 2, 3, 3, 4])
    }

    #[test]
    fn test_export() {
        let name = "test";
        let desc = vec![0x00, 0x00];

        let bytes = super::export(name, desc);
        assert_eq!(bytes, vec![4, 116, 101, 115, 116, 0, 0]);
    }

    #[test]
    fn test_functype() {
        let params = vec![0x7f, 0x7e];
        let returns = vec![0x7f];

        let bytes = super::functype(params, returns);
        assert_eq!(bytes, vec![0x60, 0x02, 0x7f, 0x7e, 0x01, 0x7f]);
    }
}
