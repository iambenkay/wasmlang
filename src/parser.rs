use leb128;

use std::{collections::HashMap, hash::Hash};

use pest::{iterators::Pair, Parser};

use crate::functions::{
    local_get, local_set, magic, valtype, Code, Codesection, Export, Exportsection,
    Functionsection, Functype, Instr, ToBytes, Typesection, EXPORT_FUNC, F64_ADD, F64_CONST,
    F64_DIV, F64_MUL, F64_SUB, I64_ADD, I64_CONST, I64_DIV, I64_MUL, I64_SUB, NUM_TYPE_F64,
    NUM_TYPE_I64, RETURN_INSTR,
};

#[derive(Parser)]
#[grammar = "ast/grammar.pest"]
struct SyntaxParser;

pub type ParserResult<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    Message(String),
}

impl From<pest::error::Error<Rule>> for ParserError {
    fn from(error: pest::error::Error<Rule>) -> Self {
        ParserError::Message(error.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Function {
        name: String,
        params: HashMap<String, (i32, String)>,
        return_type: Option<String>,
        body: Vec<AstNode>,
    },
    Assignment {
        value_type: String,
        left_hand: String,
        right_hand: Box<AstNode>,
    },
    Return(Option<Box<AstNode>>),
    Standalone(Box<AstNode>),
    BinaryOperation {
        operator: String,
        left_operand: Box<AstNode>,
        right_operand: Box<AstNode>,
    },
    UnaryOperation {
        operator: String,
        operand: Box<AstNode>,
    },
    Identifier(String),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
}

pub fn generate_wasm(ast: Vec<AstNode>) -> Vec<u8> {
    let mut wasm: Vec<u8> = magic();

    let mut typesection = Typesection { types: vec![] };
    let mut functionsection = Functionsection { indices: vec![] };
    let mut exportsection = Exportsection { exports: vec![] };
    let mut codesection = Codesection { codes: vec![] };

    for (index, node) in ast.into_iter().enumerate() {
        match node {
            AstNode::Function {
                name,
                params,
                return_type,
                body,
            } => {
                let params_types: Vec<u8> = params
                    .clone()
                    .into_values()
                    .map(|(_, type_name)| valtype(&type_name).expect("Type is not valid"))
                    .collect();

                let mut returns = vec![];
                if let Some(return_type) = return_type {
                    returns.push(valtype(&return_type).expect("Type is not valid"));
                }

                let mut locals = params_types.clone();
                let mut assignment_index = params.len() as u8;

                let declarations: Vec<(String, u8)> = params
                    .into_iter()
                    .map(|(name, (index, _))| (name, index as u8))
                    .collect();
                let mut declarations = HashMap::from_iter(declarations);

                let mut instructions = vec![];

                for node in body {
                    match node {
                        AstNode::Assignment {
                            value_type,
                            left_hand,
                            right_hand,
                        } => {
                            let type_code = valtype(&value_type).expect("Type is not valid");
                            let mut right_instr = reduce_tree(*right_hand, &declarations, &locals);

                            if right_instr.result_type != type_code {
                                unreachable!("Type does not match")
                            }
                            instructions.append(&mut right_instr.body);

                            if declarations.contains_key(&left_hand) {
                                instructions.append(&mut local_set(declarations[&left_hand]));
                            } else {
                                locals.push(type_code);
                                declarations.insert(left_hand, assignment_index);
                                instructions.append(&mut local_set(assignment_index));
                                assignment_index += 1;
                            }
                        }
                        AstNode::Return(value) => {
                            match value {
                                Some(node) => {
                                    let mut return_instr =
                                        reduce_tree(*node, &declarations, &locals);
                                    if returns.len() == 0 {
                                        unreachable!("Return type does not match")
                                    }
                                    if return_instr.result_type != returns[0] {
                                        unreachable!("Return type does not match")
                                    }
                                    instructions.append(&mut return_instr.body);
                                },
                                None => {
                                    if returns.len() != 0 {
                                        unreachable!("Expected value to be returned")
                                    }
                                }
                            }

                            instructions.push(RETURN_INSTR);
                        }
                        _ => unreachable!(),
                    }
                }

                typesection.types.push(Functype {
                    params: params_types.clone(),
                    returns,
                });
                functionsection.indices.push(index as u8);
                exportsection.exports.push(Export {
                    name,
                    desc: vec![EXPORT_FUNC, index as u8],
                });
                codesection.codes.push(Code {
                    locals: locals[params_types.len()..].to_vec(),
                    body: instructions,
                });
            }
            _ => {}
        }
    }

    wasm.append(&mut typesection.to_bytes());
    wasm.append(&mut functionsection.to_bytes());
    wasm.append(&mut exportsection.to_bytes());
    wasm.append(&mut codesection.to_bytes());
    wasm
}

fn reduce_tree(ast: AstNode, declarations: &HashMap<String, u8>, locals: &Vec<u8>) -> Instr {
    match ast {
        AstNode::Float64(value) => {
            let mut bytes = vec![F64_CONST];

            bytes.append(&mut value.to_le_bytes().to_vec());
            Instr {
                body: bytes,
                result_type: NUM_TYPE_F64,
            }
        }
        AstNode::Int64(value) => {
            let mut bytes = vec![I64_CONST];
            let mut num_buf = vec![];
            leb128::write::signed(&mut num_buf, value).expect("Number could not be written to leb128");
            bytes.append(&mut num_buf);
            Instr {
                body: bytes,
                result_type: NUM_TYPE_I64,
            }
        }
        AstNode::Identifier(name) => {
            if !declarations.contains_key(&name) {
                unreachable!("Identifier not declared");
            }
            let identifier_index = declarations[&name];
            let bytes = local_get(identifier_index);

            Instr {
                body: bytes,
                result_type: locals[identifier_index as usize],
            }
        }
        AstNode::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        } => {
            let mut left_instr = reduce_tree(*left_operand, declarations, locals);
            let mut right_instr = reduce_tree(*right_operand, declarations, locals);

            if left_instr.result_type != right_instr.result_type {
                unreachable!("Types don't match")
            }

            let mut bytes = vec![];
            bytes.append(&mut left_instr.body);
            bytes.append(&mut right_instr.body);

            if left_instr.result_type == NUM_TYPE_F64 {
                match operator.as_str() {
                    "+" => bytes.push(F64_ADD),
                    "-" => bytes.push(F64_SUB),
                    "*" => bytes.push(F64_MUL),
                    "/" => bytes.push(F64_DIV),
                    _ => unreachable!(),
                }
            } else if left_instr.result_type == NUM_TYPE_I64 {
                match operator.as_str() {
                    "+" => bytes.push(I64_ADD),
                    "-" => bytes.push(I64_SUB),
                    "*" => bytes.push(I64_MUL),
                    "/" => bytes.push(I64_DIV),
                    _ => unreachable!(),
                }
            } else {
                unreachable!("Type not supported")
            }
            Instr {
                body: bytes,
                result_type: left_instr.result_type,
            }
        }
        _ => unreachable!(),
    }
}

fn optimize_node(ast: AstNode) -> AstNode {
    match ast.clone() {
        AstNode::Standalone(node) => AstNode::Standalone(Box::new(optimize_node(*node))),
        AstNode::Return(node) => {
            match node {
                Some(ast) => AstNode::Return(Some(Box::new(optimize_node(*ast)))),
                None => AstNode::Return(None),
            }
        },
        AstNode::Assignment {
            value_type,
            left_hand,
            right_hand,
        } => {
            let right_hand = optimize_node(*right_hand);

            AstNode::Assignment {
                value_type,
                left_hand,
                right_hand: Box::new(right_hand),
            }
        }
        AstNode::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        } => {
            let left_operand = optimize_node(*left_operand);
            let right_operand = optimize_node(*right_operand);

            match (left_operand, right_operand) {
                (AstNode::Int64(left), AstNode::Int64(right)) => match operator.as_str() {
                    "+" => AstNode::Int64(left + right),
                    "-" => AstNode::Int64(left - right),
                    "*" => AstNode::Int64(left * right),
                    "/" => AstNode::Int64(left / right),
                    _ => unreachable!(),
                },
                (AstNode::Int64(left), AstNode::Float64(right)) => match operator.as_str() {
                    "+" => AstNode::Float64(left as f64 + right),
                    "-" => AstNode::Float64(left as f64 - right),
                    "*" => AstNode::Float64(left as f64 * right),
                    "/" => AstNode::Float64(left as f64 / right),
                    _ => unreachable!(),
                },
                (AstNode::Float64(left), AstNode::Int64(right)) => match operator.as_str() {
                    "+" => AstNode::Float64(left + right as f64),
                    "-" => AstNode::Float64(left - right as f64),
                    "*" => AstNode::Float64(left * right as f64),
                    "/" => AstNode::Float64(left / right as f64),
                    _ => unreachable!(),
                },
                (AstNode::Float64(left), AstNode::Float64(right)) => match operator.as_str() {
                    "+" => AstNode::Float64(left + right),
                    "-" => AstNode::Float64(left - right),
                    "*" => AstNode::Float64(left * right),
                    "/" => AstNode::Float64(left / right),
                    _ => unreachable!(),
                },
                _ => ast,
            }
        }
        _ => ast,
    }
}

pub fn generate_ast(source_code: &str) -> ParserResult<Vec<AstNode>> {
    fn generate_ast_of_function(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();

        let token = pairs.peek()?;

        let mut return_type: Option<String> = None;

        if token.as_rule() == Rule::ReturnType {
            return_type = Some(pairs.next()?.as_str().to_owned());
        }

        let function_name = pairs.next()?.as_str().to_owned();

        let mut params = HashMap::new();

        let mut function_body = vec![];

        let mut param_index = 0;
        while let Some(pair) = pairs.peek() {
            if pair.as_rule() != Rule::ArgumentExpr {
                break;
            }

            let mut param_pairs = pair.into_inner();
            let param_type = param_pairs.next()?.as_str().to_owned();
            let param_name = param_pairs.next()?.as_str().to_owned();

            params.insert(param_name, (param_index, param_type));
            param_index += 1;

            pairs.next();
        }

        for pair in pairs {
            let node = match pair.as_rule() {
                Rule::ReturnExpr => generate_ast_of_return(pair)?,
                Rule::StandaloneExpr => generate_ast_of_standalone(pair)?,
                Rule::AssignmentExpr => generate_ast_of_assignment(pair)?,
                _ => unreachable!(),
            };

            function_body.push(optimize_node(node));
        }

        Some(AstNode::Function {
            name: function_name,
            params,
            return_type,
            body: function_body,
        })
    }

    fn generate_ast_of_return(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();
        let mut value = None;

        if let Some(_) = pairs.peek() {
            value = generate_ast_of_add_expr(pairs.next()?);
        }

        match value {
            Some(ast) => Some(AstNode::Return(Some(Box::new(ast)))),
            None => Some(AstNode::Return(None)),
        }
    }

    fn generate_ast_of_standalone(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();
        let value = generate_ast_of_add_expr(pairs.next()?);

        Some(AstNode::Standalone(Box::new(value?)))
    }

    fn generate_ast_of_assignment(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();

        let value_type = pairs.next()?.as_str().to_owned();
        let left_hand = pairs.next()?.as_str().to_owned();
        let right_hand = generate_ast_of_add_expr(pairs.next()?);

        Some(AstNode::Assignment {
            value_type,
            left_hand,
            right_hand: Box::new(right_hand?),
        })
    }

    fn generate_ast_of_add_expr(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();

        let mut node = generate_ast_of_mul_expr(pairs.next()?)?;

        if let Some(pair) = pairs.next() {
            let add_op = pair.as_str().to_owned();
            let right = generate_ast_of_add_expr(pairs.next()?);
            let binary_ast = AstNode::BinaryOperation {
                operator: add_op,
                left_operand: Box::new(node),
                right_operand: Box::new(right?),
            };
            node = binary_ast;
        }

        Some(node)
    }

    fn generate_ast_of_mul_expr(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();

        let mut node = generate_ast_of_unit_expr(pairs.next()?)?;

        if let Some(pair) = pairs.next() {
            let mul_op = pair.as_str().to_owned();
            let right = generate_ast_of_mul_expr(pairs.next()?);
            let binary_ast = AstNode::BinaryOperation {
                operator: mul_op,
                left_operand: Box::new(node),
                right_operand: Box::new(right?),
            };
            node = binary_ast;
        }

        Some(node)
    }

    fn generate_ast_of_unit_expr(pair: Pair<Rule>) -> Option<AstNode> {
        let mut pairs = pair.into_inner();

        let pair = pairs.next()?;

        if pair.as_rule() == Rule::UnaryExpr {
            let mut pairs = pair.into_inner();

            let op = pairs.next()?.as_str().to_owned();
            let operand = generate_ast_of_unit_expr(pairs.next()?);

            return Some(AstNode::UnaryOperation {
                operator: op,
                operand: Box::new(operand?),
            });
        }

        if pair.as_rule() == Rule::Identifier {
            return Some(AstNode::Identifier(pair.as_str().to_owned()));
        }

        if pair.as_rule() == Rule::Float {
            return Some(AstNode::Float64(pair.as_str().parse::<f64>().ok()?));
        }

        if pair.as_rule() == Rule::Integer {
            return Some(AstNode::Int64(pair.as_str().parse::<i64>().ok()?));
        }

        if pair.as_rule() == Rule::AddExpr {
            return generate_ast_of_add_expr(pair);
        }

        None
    }

    let mut pairs = SyntaxParser::parse(Rule::Program, source_code)?;

    let mut functions = vec![];

    while let Some(pair) = pairs.next() {
        let func = generate_ast_of_function(pair);
        if let Some(func) = func {
            functions.push(func);
        }
    }

    Ok(functions)
}

#[cfg(test)]
mod tests {
    use super::optimize_node;
    use super::AstNode;

    #[test]
    fn test_optimize_node() {
        let ast = AstNode::BinaryOperation {
            operator: "+".to_owned(),
            left_operand: Box::new(AstNode::Int64(1)),
            right_operand: Box::new(AstNode::Int64(2)),
        };

        let ast = optimize_node(ast);

        assert_eq!(ast, AstNode::Int64(3));
    }
}
