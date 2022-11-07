mod env;
mod object;

use self::env::Env;
use self::object::*;
use crate::ast::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
        }
    }

    pub fn eval(&mut self, program: Program) -> Result<(), RuntimeError> {
        for stmt in program.0 {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn eval_repl(&mut self, program: Program) -> EvalResult {
        let mut result = Object::Nil;
        for stmt in program.0 {
            result = self.eval_stmt(stmt)?;
        }
        Ok(result)
    }

    fn eval_block(&mut self, block: Block) -> Result<(), RuntimeError> {
        for stmt in block.0 {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> EvalResult {
        match stmt {
            Stmt::Assign(AssignStmt { name, value }) => {
                let val = self.eval_expr(value)?;
                self.env.borrow_mut().set(name.0, val);
                Ok(Object::Nil)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::If(IfStmt {
                condition,
                true_block,
                else_block,
                elifs,
            }) => self.eval_if_stmt(condition, true_block, else_block, elifs),
            _ => todo!(),
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> EvalResult {
        match expr {
            Expr::Literal(Literal::Nil) => Ok(Object::Nil),
            Expr::Literal(Literal::Integer(i)) => Ok(Object::Int(i)),
            Expr::Literal(Literal::Boolean(b)) => Ok(Object::Bool(b)),
            Expr::Literal(Literal::String(s)) => Ok(Object::String(s)),
            Expr::Literal(Literal::Float(f)) => Ok(Object::Float(f)),
            Expr::Literal(Literal::Array(arr)) => {
                let mut array = Vec::new();
                for expr in arr {
                    array.push(self.eval_expr(expr)?);
                }
                Ok(Object::Array(Rc::new(RefCell::new(array))))
            }
            Expr::Ident(Ident(name)) => self.eval_ident(&name),
            Expr::Index(IndexExpr { left, index }) => {
                let expr = self.eval_expr(*left)?;
                let index = self.eval_expr(*index)?;
                self.eval_index_expr(expr, index)
            }
            Expr::Prefix(PrefixExpr { left, op, line }) => {
                let left = self.eval_expr(*left)?;
                self.eval_prefix_expr(op, left, line)
            }
            Expr::Infix(InfixExpr {
                left,
                op,
                right,
                line,
            }) => {
                let left = self.eval_expr(*left)?;
                let right = self.eval_expr(*right)?;
                self.eval_infix_expr(op, left, right, line)
            }
            _ => todo!(),
        }
    }

    fn eval_ident(&self, name: &str) -> EvalResult {
        let val = self.env.borrow().get(name);
        if let Some(val) = val {
            Ok(val)
        } else {
            Err(RuntimeError::VariableNotFound {
                name: name.to_owned(),
                line: 0,
            })
        }
    }

    fn eval_prefix_expr(&mut self, op: PrefixOp, expr: Object, line: usize) -> EvalResult {
        match op {
            PrefixOp::Bang => self.eval_bang_op(expr, line),
            PrefixOp::Minus => self.eval_minus_op(expr, line),
            PrefixOp::Plus => self.eval_plus_op(expr, line),
        }
    }

    fn eval_bang_op(&self, expr: Object, line: usize) -> EvalResult {
        match expr {
            Object::Bool(b) => Ok(Object::Bool(!b)),
            Object::Nil => Ok(Object::Bool(true)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Bang,
                operand: expr.scurry_type(),
                line,
            }),
        }
    }

    fn eval_minus_op(&self, expr: Object, line: usize) -> EvalResult {
        match expr {
            Object::Int(i) => Ok(Object::Int(-i)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Minus,
                operand: expr.scurry_type(),
                line,
            }),
        }
    }

    fn eval_plus_op(&self, expr: Object, line: usize) -> EvalResult {
        match expr {
            Object::Int(_) => Ok(expr),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Plus,
                operand: expr.scurry_type(),
                line,
            }),
        }
    }

    fn eval_infix_expr(&self, op: InfixOp, left: Object, right: Object, line: usize) -> EvalResult {
        match (&left, &right) {
            (Object::Int(x), Object::Int(y)) => self.eval_int_infix_expr(op, *x, *y, line),
            (Object::Float(x), Object::Float(y)) => self.eval_float_infix_expr(op, *x, *y, line),
            (Object::Int(x), Object::Float(y)) => {
                self.eval_float_infix_expr(op, *x as f32, *y, line)
            }
            (Object::Float(x), Object::Int(y)) => {
                self.eval_float_infix_expr(op, *x, *y as f32, line)
            }
            (Object::Bool(left), Object::Bool(right)) => {
                self.eval_bool_infix_expr(op, *left, *right, line)
            }
            (Object::String(s1), Object::String(s2)) => {
                self.eval_string_infix_expr(op, s1, s2, line)
            }
            _ => match op {
                InfixOp::Eq => Ok(Object::Bool(left == right)),
                InfixOp::NotEq => Ok(Object::Bool(left != right)),
                _ => Err(RuntimeError::InvalidBinaryOperand {
                    op,
                    left: left.scurry_type(),
                    right: right.scurry_type(),
                    line,
                }),
            },
        }
    }

    fn eval_int_infix_expr(&self, op: InfixOp, x: i32, y: i32, line: usize) -> EvalResult {
        macro_rules! check_overflow {
            ($op:ident) => {
                x.$op(y).map_or(
                    Err(RuntimeError::IntegerOverflow {
                        op,
                        left: x,
                        right: y,
                        line,
                    }),
                    |x| Ok(Object::Int(x)),
                )
            };
        }
        macro_rules! check_zero_div {
            ($op:ident) => {
                x.$op(y).map_or(
                    Err(RuntimeError::DivisionByZero {
                        op,
                        left: Number::Int(x),
                        right: Number::Int(y),
                        line,
                    }),
                    |x| Ok(Object::Int(x)),
                )
            };
        }

        match op {
            InfixOp::Plus => check_overflow!(checked_add),
            InfixOp::Minus => check_overflow!(checked_sub),
            InfixOp::Asterisk => check_overflow!(checked_mul),
            InfixOp::Slash => check_zero_div!(checked_div),
            InfixOp::Modulo => check_zero_div!(checked_rem),
            InfixOp::Eq => Ok(Object::Bool(x == y)),
            InfixOp::NotEq => Ok(Object::Bool(x != y)),
            InfixOp::Gt => Ok(Object::Bool(x > y)),
            InfixOp::Lt => Ok(Object::Bool(x < y)),
            InfixOp::Ge => Ok(Object::Bool(x >= y)),
            InfixOp::Le => Ok(Object::Bool(x <= y)),
            InfixOp::LogicalOr => Err(RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalOr,
                left: Type::Int,
                right: Type::Int,
                line,
            }),
            InfixOp::LogicalAnd => Err(RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalAnd,
                left: Type::Int,
                right: Type::Int,
                line,
            }),
        }
    }

    fn eval_float_infix_expr(&self, op: InfixOp, x: f32, y: f32, line: usize) -> EvalResult {
        match op {
            InfixOp::Plus => Ok(Object::Float(x + y)),
            InfixOp::Minus => Ok(Object::Float(x - y)),
            InfixOp::Asterisk => Ok(Object::Float(x * y)),
            InfixOp::Slash => {
                let result = x / y;
                if result.is_infinite() {
                    Err(RuntimeError::DivisionByZero {
                        op: InfixOp::Slash,
                        left: Number::Float(x),
                        right: Number::Float(y),
                        line,
                    })
                } else {
                    Ok(Object::Float(result))
                }
            }
            InfixOp::Modulo => {
                let result = x % y;
                if result.is_nan() {
                    Err(RuntimeError::DivisionByZero {
                        op: InfixOp::Modulo,
                        left: Number::Float(x),
                        right: Number::Float(y),
                        line,
                    })
                } else {
                    Ok(Object::Float(result))
                }
            }
            InfixOp::Eq => Ok(Object::Bool(x == y)),
            InfixOp::NotEq => Ok(Object::Bool(x != y)),
            InfixOp::Gt => Ok(Object::Bool(x > y)),
            InfixOp::Lt => Ok(Object::Bool(x < y)),
            InfixOp::Ge => Ok(Object::Bool(x >= y)),
            InfixOp::Le => Ok(Object::Bool(x <= y)),
            InfixOp::LogicalOr => Err(RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalOr,
                left: Type::Float,
                right: Type::Float,
                line,
            }),
            InfixOp::LogicalAnd => Err(RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalAnd,
                left: Type::Float,
                right: Type::Float,
                line,
            }),
        }
    }

    fn eval_bool_infix_expr(
        &self,
        op: InfixOp,
        left: bool,
        right: bool,
        line: usize,
    ) -> EvalResult {
        match op {
            InfixOp::LogicalAnd => Ok(Object::Bool(left && right)),
            InfixOp::LogicalOr => Ok(Object::Bool(left || right)),
            InfixOp::Eq => Ok(Object::Bool(left == right)),
            InfixOp::NotEq => Ok(Object::Bool(left != right)),
            _ => Err(RuntimeError::InvalidBinaryOperand {
                op,
                left: Type::Bool,
                right: Type::Bool,
                line,
            }),
        }
    }

    fn eval_string_infix_expr(&self, op: InfixOp, s1: &str, s2: &str, line: usize) -> EvalResult {
        match op {
            InfixOp::Plus => Ok(Object::String(s1.to_owned() + s2)),
            InfixOp::Eq => Ok(Object::Bool(s1 == s2)),
            InfixOp::NotEq => Ok(Object::Bool(s1 != s2)),
            _ => Err(RuntimeError::InvalidBinaryOperand {
                op,
                left: Type::String,
                right: Type::String,
                line,
            }),
        }
    }

    fn eval_if_stmt(
        &mut self,
        condition: Expr,
        true_block: Block,
        else_block: Option<Block>,
        elifs: Vec<ElifStmt>,
    ) -> EvalResult {
        if self.eval_expr(condition)?.is_truthy() {
            self.eval_block(true_block)?;
        } else {
            for elif in elifs {
                if self.eval_expr(elif.condition)?.is_truthy() {
                    self.eval_block(elif.block)?;
                    return Ok(Object::Nil);
                }
            }
            if let Some(block) = else_block {
                self.eval_block(block)?;
            }
        }
        Ok(Object::Nil)
    }

    fn eval_index_expr(&mut self, obj: Object, index: Object) -> EvalResult {
        match (&obj, &index) {
            (Object::Array(arr), Object::Int(i)) => {
                if arr.borrow().is_empty() {
                    return Err(RuntimeError::IndexOutOfRange { obj, index: *i });
                }
                if *i < 0 {
                    if i.abs() as usize > arr.borrow().len() {
                        return Err(RuntimeError::IndexOutOfRange { obj, index: *i });
                    }
                    return match arr.borrow().iter().rev().take(i.abs() as usize).last() {
                        Some(item) => Ok(item.clone()),
                        None => Err(RuntimeError::IndexOutOfRange {
                            obj: obj.clone(),
                            index: *i,
                        }),
                    };
                }
                let idx = *i as usize;
                if idx > arr.borrow().len() - 1 {
                    Err(RuntimeError::IndexOutOfRange { obj, index: *i })
                } else {
                    Ok(arr.borrow()[idx].clone())
                }
            }
            (Object::String(s), Object::Int(i)) => {
                if s.is_empty() {
                    return Err(RuntimeError::IndexOutOfRange { obj, index: *i });
                }
                if *i < 0 {
                    if i.abs() as usize > s.len() {
                        return Err(RuntimeError::IndexOutOfRange { obj, index: *i });
                    }
                    return match s.chars().rev().take(i.abs() as usize).last() {
                        Some(item) => Ok(Object::String(item.to_string())),
                        None => Err(RuntimeError::IndexOutOfRange { obj, index: *i }),
                    };
                }
                match s.chars().nth(*i as usize) {
                    Some(c) => Ok(Object::String(c.to_string())),
                    None => Err(RuntimeError::IndexOutOfRange { obj, index: *i }),
                }
            }
            _ => Err(RuntimeError::IndexOperatorOutOfRange {
                obj: obj.scurry_type(),
                index_type: index.scurry_type(),
            }),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    macro_rules! test_eval {
        ($inputs:expr, $expecteds:expr) => {
            for (input, expected) in $inputs.iter().zip($expecteds) {
                let parser = Parser::new(input);
                let program = parser.parse().expect("Should have no parser errors");
                let mut interpreter = Interpreter::new();
                assert_eq!(
                    expected,
                    interpreter
                        .eval_repl(program)
                        .expect("Should evaluate with no errors")
                );
            }
        };
    }

    macro_rules! runtime_error_eval {
        ($inputs:expr, $errs:expr) => {
            for (input, err) in $inputs.iter().zip($errs) {
                let parser = Parser::new(input);
                let program = parser.parse().expect("Should have no parser errors");
                let mut interpreter = Interpreter::new();
                assert_eq!(
                    err,
                    interpreter
                        .eval_repl(program)
                        .expect_err("Should evaluate with an error")
                );
            }
        };
    }

    #[test]
    fn eval_integer_literal() {
        let inputs = ["1;", "333;"];
        let expecteds = [Object::Int(1), Object::Int(333)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_float_literal() {
        let inputs = ["1.3;", "333.333;"];
        let expecteds = [Object::Float(1.3), Object::Float(333.333)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_string_literal() {
        let inputs = ["\"test\";", "\"string literal\";"];
        let expecteds = [
            Object::String("test".to_owned()),
            Object::String("string literal".to_owned()),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_boolean_literal() {
        let inputs = ["True;", "False;"];
        let expecteds = [Object::Bool(true), Object::Bool(false)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_array_literal() {
        let inputs = ["[1, 2, 3];"];
        let expecteds = [array![Object::Int(1), Object::Int(2), Object::Int(3)]];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_ident() {
        let inputs = ["x = 3; x;"];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_integer_prefix_ops() {
        let inputs = ["-1;", "+2;"];
        let expecteds = [Object::Int(-1), Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_boolean_prefix_ops() {
        let inputs = ["!True;", "!False;"];
        let expecteds = [Object::Bool(false), Object::Bool(true)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_integer_binary_ops() {
        let inputs = [
            "1 + 1;", "2 - 2;", "3 * 3;", "4 / 4;", "20 % 2;", "3 == 3;", "4 != 4;", "10 > 3;",
            "6 < 0;", "30 >= 9;", "7 <= 7;",
        ];
        let expecteds = [
            Object::Int(2),
            Object::Int(0),
            Object::Int(9),
            Object::Int(1),
            Object::Int(0),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(true),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_boolean_binary_ops() {
        let inputs = [
            "True == True;",
            "False != True;",
            "True && False;",
            "False || True;",
        ];
        let expecteds = [
            Object::Bool(true),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_string_binary_ops() {
        let inputs = ["\"test\" == \"test\";", "\"testing\" != \"testing\";"];
        let expecteds = [Object::Bool(true), Object::Bool(false)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn invalid_unary_operands() {
        let inputs = ["-True;", "!3;", "+False;"];
        let errs = [
            RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Minus,
                operand: Type::Bool,
                line: 1,
            },
            RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Bang,
                operand: Type::Int,
                line: 1,
            },
            RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Plus,
                operand: Type::Bool,
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn invalid_binary_operands() {
        let inputs = [
            "\"test\" + 1;",
            "False - 3;",
            "True && 3;",
            "1 || 1;",
            "1.1 && 1.1;",
        ];
        let errs = [
            RuntimeError::InvalidBinaryOperand {
                op: InfixOp::Plus,
                left: Type::String,
                right: Type::Int,
                line: 1,
            },
            RuntimeError::InvalidBinaryOperand {
                op: InfixOp::Minus,
                left: Type::Bool,
                right: Type::Int,
                line: 1,
            },
            RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalAnd,
                left: Type::Bool,
                right: Type::Int,
                line: 1,
            },
            RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalOr,
                left: Type::Int,
                right: Type::Int,
                line: 1,
            },
            RuntimeError::InvalidBinaryOperand {
                op: InfixOp::LogicalAnd,
                left: Type::Float,
                right: Type::Float,
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_float_infix_expr() {
        let inputs = [
            "1.1 + 2.0;",
            "4.4 - 1.2;",
            "2.2 / 2.0;",
            "2.2 * 2.0;",
            "2.0 == 1.0;",
            "3.1 != 4.2;",
        ];
        let expecteds = [
            Object::Float(3.1),
            Object::Float(3.2),
            Object::Float(1.1),
            Object::Float(4.4),
            Object::Bool(false),
            Object::Bool(true),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_num_infix_expr_mixed() {
        let inputs = ["2 * 2.3;", "7.6 - 2;", "4.0 == 4;", "30.0 % 2;"];
        let expecteds = [
            Object::Float(4.6),
            Object::Float(5.6),
            Object::Bool(true),
            Object::Float(0.0),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_string_infix_expr() {
        let inputs = ["\"hello\" + \" world\";"];
        let expecteds = [Object::String("hello world".to_owned())];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_if_stmt() {
        let inputs = [
            "if True { x = 3; }; x;",
            "if False {} else { x = 3; }; x;",
            "if False {} elif True { x = 3; } else {}; x;",
            "if False {} elif False {} elif False {} else {}",
        ];
        let expecteds = [Object::Int(3), Object::Int(3), Object::Int(3), Object::Nil];

        test_eval!(inputs, expecteds);
    }

    #[test]
    fn eval_array_index_expr() {
        let inputs = [
            "[1, 2, 3][1];",
            "[1, 2, 3][-1];",
            "[1, 2, 3][0];",
            "[1, 2, 3][-1];",
            "[1, 2, 3][-2];",
        ];
        let expecteds = [
            Object::Int(2),
            Object::Int(3),
            Object::Int(1),
            Object::Int(3),
            Object::Int(2),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn array_out_of_range_indices() {
        let inputs = ["[1, 2, 3][3];", "[][0];", "[1, 2, 3][-4];"];
        let errs = [
            RuntimeError::IndexOutOfRange {
                obj: array![Object::Int(1), Object::Int(2), Object::Int(3)],
                index: 3,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::Array(Rc::new(RefCell::new(Vec::new()))),
                index: 0,
            },
            RuntimeError::IndexOutOfRange {
                obj: array![Object::Int(1), Object::Int(2), Object::Int(3)],
                index: -4,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_string_index_expr() {
        let inputs = ["\"test\"[0];", "\"test\"[2];", "\"test\"[-1];"];
        let expecteds = [
            Object::String("t".to_owned()),
            Object::String("s".to_owned()),
            Object::String("t".to_owned()),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn string_out_of_range_indices() {
        let inputs = ["\"test\"[10];", "\"test\"[-10];", "\"\"[0];"];
        let errs = [
            RuntimeError::IndexOutOfRange {
                obj: Object::String("test".to_owned()),
                index: 10,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::String("test".to_owned()),
                index: -10,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::String("".to_owned()),
                index: 0,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }
}
