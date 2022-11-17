mod builtin;
mod env;
pub mod object;

use self::env::Env;
use self::object::*;
use crate::ast::*;
use crate::parser::Parser;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

macro_rules! loop_control {
    ($interpreter:expr, $block:expr) => {{
        let result = $interpreter.eval_block($block.clone())?;
        if matches!(result, Object::ControlChange(ControlChange::Return(_))) {
            return Ok(result);
        }
        if matches!(result, Object::ControlChange(ControlChange::Continue)) {
            continue;
        }
        if matches!(result, Object::ControlChange(ControlChange::Break)) {
            break;
        }
    }};
}

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
            // Get rid of return val wrappers, no longer needed
            while let Object::ControlChange(ControlChange::Return(val)) = result {
                result = *val
            }
        }
        Ok(result)
    }

    fn eval_block(&mut self, block: Block) -> EvalResult {
        for stmt in block.0 {
            let result = self.eval_stmt(stmt)?;
            if matches!(result, Object::ControlChange(_)) {
                return Ok(result);
            }
        }
        Ok(Object::AbsoluteNil)
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> EvalResult {
        match stmt {
            Stmt::Assign(AssignStmt {
                name,
                value,
                line,
                operator,
                type_checked,
                var_type,
            }) => {
                let value = self.eval_expr(value)?;
                match name {
                    Expr::Ident(ident) => {
                        if let Some(op) = operator {
                            let prev_val = self.eval_ident(&ident.0)?;
                            let result = self.eval_infix_expr(op.into(), prev_val, value, line)?;
                            if type_checked && !result.fits_type(var_type.clone()) {
                                return Err(RuntimeError::MismatchedAssignType {
                                    name: ident.0.clone(),
                                    expected: var_type,
                                    got: result.scurry_type().into(),
                                });
                            }
                            self.env.borrow_mut().set(ident.0, result);
                        } else {
                            if type_checked && !value.fits_type(var_type.clone()) {
                                return Err(RuntimeError::MismatchedAssignType {
                                    name: ident.0.clone(),
                                    expected: var_type,
                                    got: value.scurry_type().into(),
                                });
                            }
                            self.env.borrow_mut().set(ident.0, value);
                        }
                    }
                    Expr::Dot(DotExpr { left, field, line }) => {
                        let left = self.eval_expr(*left)?;
                        let Object::Instance(instance) = left else {
                            return Err(RuntimeError::DotOperatorNotSupported { obj: left.scurry_type(), line });
                        };
                        if instance.visibility != Visibility::Private {
                            return Err(RuntimeError::UnrecognizedField {
                                field: field.0,
                                obj: Type::Instance(instance.component.name.0.clone()),
                                line,
                            });
                        }
                        if !instance.field_values.borrow().contains_key(&field.0) {
                            return Err(RuntimeError::UnrecognizedField {
                                field: field.0,
                                obj: Type::Instance(instance.component.name.0.clone()),
                                line,
                            });
                        }
                        if let Some(op) = operator {
                            let prev_val = self.eval_dot_expr(
                                Object::Instance(instance.clone()),
                                field.0.clone(),
                                line,
                            )?;
                            let result = self.eval_infix_expr(op.into(), prev_val, value, line)?;
                            instance.field_values.borrow_mut().insert(field.0, result);
                        } else {
                            instance.field_values.borrow_mut().insert(field.0, value);
                        }
                    }
                    Expr::Index(IndexExpr { left, index, line }) => {
                        let left = self.eval_expr(*left)?;
                        let index = self.eval_expr(*index)?;
                        match (&left, &index) {
                            (
                                Object::Map(map),
                                Object::Int(_) | Object::String(_) | Object::Bool(_),
                            ) => {
                                if let Some(op) = operator {
                                    let prev_val = self.eval_index_expr(
                                        Object::Map(map.clone()),
                                        index.clone(),
                                        line,
                                    )?;
                                    let result =
                                        self.eval_infix_expr(op.into(), prev_val, value, line)?;
                                    map.borrow_mut().insert(index, result);
                                } else {
                                    map.borrow_mut().insert(index, value);
                                }
                            }
                            (Object::Array(arr), Object::Int(i)) => {
                                if *i < 0 {
                                    if i.unsigned_abs() as usize > arr.borrow().len() {
                                        return Err(RuntimeError::IndexOutOfRange {
                                            obj: left,
                                            index: *i,
                                            line,
                                        });
                                    }
                                    let rev_idx = arr.borrow().len() - 1;
                                    if let Some(op) = operator {
                                        let prev_val = self.eval_index_expr(
                                            Object::Array(arr.clone()),
                                            index.clone(),
                                            line,
                                        )?;
                                        let result =
                                            self.eval_infix_expr(op.into(), prev_val, value, line)?;
                                        arr.borrow_mut()[rev_idx] = result;
                                    } else {
                                        arr.borrow_mut()[rev_idx] = value;
                                    }
                                    return Ok(Object::AbsoluteNil);
                                }
                                let idx = *i as usize;
                                if arr.borrow().len() == 0 || idx > arr.borrow().len() - 1 {
                                    return Err(RuntimeError::IndexOutOfRange {
                                        obj: left,
                                        index: *i,
                                        line,
                                    });
                                } else if let Some(op) = operator {
                                    let prev_val = self.eval_index_expr(
                                        Object::Array(arr.clone()),
                                        index.clone(),
                                        line,
                                    )?;
                                    let result =
                                        self.eval_infix_expr(op.into(), prev_val, value, line)?;
                                    arr.borrow_mut()[idx] = result;
                                } else {
                                    arr.borrow_mut()[idx] = value;
                                }
                            }
                            _ => {
                                return Err(RuntimeError::IndexOperatorNotSupported {
                                    obj: left.scurry_type(),
                                    index_type: index.scurry_type(),
                                    line,
                                });
                            }
                        }
                    }
                    // Cannot assign in this manner
                    _ => return Err(RuntimeError::CannotAssign { expr: name, line }),
                }
                Ok(Object::AbsoluteNil)
            }
            Stmt::While(WhileStmt { condition, block }) => self.eval_while_stmt(condition, block),
            Stmt::Return(ReturnStmt { value }) => Ok(Object::ControlChange(ControlChange::Return(
                Box::new(self.eval_expr(value)?),
            ))),
            Stmt::Break => Ok(Object::ControlChange(ControlChange::Break)),
            Stmt::Continue => Ok(Object::ControlChange(ControlChange::Continue)),
            Stmt::Function(FunctionStmt {
                name,
                params,
                block,
                visibility,
                ..
            }) => {
                let func = Object::Function {
                    params,
                    body: block,
                    env: Rc::clone(&self.env),
                    bound: None,
                    visibility: Some(visibility),
                };
                self.env.borrow_mut().set(name.0, func);
                Ok(Object::AbsoluteNil)
            }
            Stmt::Declaration(DeclarationStmt {
                name,
                methods,
                fields,
                embeds,
                visibility,
            }) => {
                let embedded = {
                    let mut embedded = Vec::new();
                    for embed in embeds {
                        let embed_component = match self.env.borrow().get(&embed.name.0) {
                            Some(Object::Component(component)) => component,
                            _ => {
                                return Err(RuntimeError::InvalidEmbed {
                                    name: embed.name.0,
                                    line: embed.line,
                                });
                            }
                        };
                        embedded.push((embed_component, embed.assigned));
                    }
                    embedded
                };
                let mut exports = Vec::new();
                let mut comp_methods = HashMap::new();
                for func in methods {
                    comp_methods.insert(
                        func.name.0.clone(),
                        Object::Function {
                            params: func.params,
                            body: func.block,
                            env: Rc::clone(&self.env),
                            bound: None,
                            visibility: None,
                        },
                    );
                    if func.visibility == Visibility::Public {
                        exports.push(func.name.0)
                    }
                }
                self.env.borrow_mut().set(
                    name.0.clone(),
                    Object::Component(Component {
                        name,
                        fields,
                        methods: comp_methods,
                        exports,
                        embeds: embedded,
                        visibility,
                    }),
                );
                Ok(Object::AbsoluteNil)
            }
            Stmt::For(ForStmt {
                iter_ident,
                expr,
                block,
                line,
            }) => self.eval_for_stmt(iter_ident, expr, block, line),
            Stmt::If(IfStmt {
                condition,
                true_block,
                else_block,
                elifs,
            }) => self.eval_if_stmt(condition, true_block, else_block, elifs),
            Stmt::Switch(SwitchStmt {
                expr,
                cases,
                default,
            }) => {
                let switch = self.eval_expr(expr)?;
                self.eval_switch_stmt(switch, cases, default)
            }
            Stmt::Import(ImportStmt {
                target,
                alias,
                line,
            }) => self.eval_import_stmt(target, alias, line),
            Stmt::Expr(expr) => self.eval_expr(expr),
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
            Expr::Literal(Literal::Map(map)) => {
                let mut pairs = HashMap::new();
                for (key, value) in map {
                    pairs.insert(self.eval_expr(key)?, self.eval_expr(value)?);
                }
                Ok(Object::Map(Rc::new(RefCell::new(pairs))))
            }
            Expr::Ident(Ident(name)) => self.eval_ident(&name),
            Expr::Index(IndexExpr { left, index, line }) => {
                let expr = self.eval_expr(*left)?;
                let index = self.eval_expr(*index)?;
                self.eval_index_expr(expr, index, line)
            }
            Expr::Function(FunctionExpr { params, block, .. }) => Ok(Object::Function {
                params,
                body: block,
                env: Rc::clone(&self.env),
                bound: None,
                visibility: None,
            }),
            Expr::Dot(DotExpr { left, field, line }) => {
                let expr = self.eval_expr(*left)?;
                self.eval_dot_expr(expr, field.0, line)
            }
            Expr::Call(CallExpr {
                func,
                args,
                line,
                type_checked,
            }) => {
                let func = self.eval_expr(*func)?;
                let args = {
                    let mut result = Vec::new();
                    for arg in args {
                        result.push(self.eval_expr(arg)?);
                    }
                    result
                };
                self.eval_call_expr(func, args, type_checked, line)
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
        }
    }

    fn eval_ident(&self, name: &str) -> EvalResult {
        let val = self.env.borrow().get(name);
        if let Some(val) = val {
            Ok(val)
        } else if let Some(func) = builtin::get_builtin_func(name) {
            Ok(Object::Builtin(func))
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
            Object::Float(f) => Ok(Object::Float(-f)),
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
            Object::Float(_) => Ok(expr),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                op: PrefixOp::Plus,
                operand: expr.scurry_type(),
                line,
            }),
        }
    }

    fn eval_infix_expr(
        &mut self,
        op: InfixOp,
        left: Object,
        right: Object,
        line: usize,
    ) -> EvalResult {
        macro_rules! special_op {
            ($instance:expr, $name:ident) => {
                if let Some(add) = $instance.get_special(SpecialMethod::$name) {
                    let mut args = Vec::new();
                    args.push(Object::Instance($instance.clone_with_private()));
                    args.push(right);
                    self.eval_call_expr(add.clone(), args, false, line)
                } else {
                    unreachable!()
                }
            };
        }
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
            (Object::Instance(instance), _) if instance.has_special(SpecialMethod::Add) => {
                special_op!(instance, Add)
            }
            (Object::Instance(instance), _) if instance.has_special(SpecialMethod::Sub) => {
                special_op!(instance, Sub)
            }
            (Object::Instance(instance), _) if instance.has_special(SpecialMethod::Div) => {
                special_op!(instance, Div)
            }
            (Object::Instance(instance), _) if instance.has_special(SpecialMethod::Mul) => {
                special_op!(instance, Mul)
            }
            (Object::Instance(instance), _) if instance.has_special(SpecialMethod::Mod) => {
                special_op!(instance, Mod)
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
            self.eval_block(true_block)
        } else {
            for elif in elifs {
                if self.eval_expr(elif.condition)?.is_truthy() {
                    return self.eval_block(elif.block);
                }
            }
            if let Some(block) = else_block {
                self.eval_block(block)
            } else {
                Ok(Object::AbsoluteNil)
            }
        }
    }

    fn eval_index_expr(&mut self, obj: Object, index: Object, line: usize) -> EvalResult {
        match (&obj, &index) {
            (Object::Array(arr), Object::Int(i)) => {
                if arr.borrow().is_empty() {
                    return Err(RuntimeError::IndexOutOfRange {
                        obj,
                        index: *i,
                        line,
                    });
                }
                if *i < 0 {
                    if i.unsigned_abs() as usize > arr.borrow().len() {
                        return Err(RuntimeError::IndexOutOfRange {
                            obj,
                            index: *i,
                            line,
                        });
                    }
                    return match arr
                        .borrow()
                        .iter()
                        .rev()
                        .take(i.unsigned_abs() as usize)
                        .last()
                    {
                        Some(item) => Ok(item.clone()),
                        None => Err(RuntimeError::IndexOutOfRange {
                            obj: obj.clone(),
                            index: *i,
                            line,
                        }),
                    };
                }
                let idx = *i as usize;
                if idx > arr.borrow().len() - 1 {
                    Err(RuntimeError::IndexOutOfRange {
                        obj,
                        index: *i,
                        line,
                    })
                } else {
                    Ok(arr.borrow()[idx].clone())
                }
            }
            (Object::String(s), Object::Int(i)) => {
                if s.is_empty() {
                    return Err(RuntimeError::IndexOutOfRange {
                        obj,
                        index: *i,
                        line,
                    });
                }
                if *i < 0 {
                    if i.unsigned_abs() as usize > s.len() {
                        return Err(RuntimeError::IndexOutOfRange {
                            obj,
                            index: *i,
                            line,
                        });
                    }
                    return match s.chars().rev().take(i.unsigned_abs() as usize).last() {
                        Some(item) => Ok(Object::String(item.to_string())),
                        None => Err(RuntimeError::IndexOutOfRange {
                            obj,
                            index: *i,
                            line,
                        }),
                    };
                }
                match s.chars().nth(*i as usize) {
                    Some(c) => Ok(Object::String(c.to_string())),
                    None => Err(RuntimeError::IndexOutOfRange {
                        obj,
                        index: *i,
                        line,
                    }),
                }
            }
            (Object::Map(map), Object::Int(_) | Object::Bool(_) | Object::String(_)) => {
                match map.borrow().get(&index) {
                    Some(obj) => Ok(obj.clone()),
                    None => Err(RuntimeError::KeyNotFound {
                        obj: obj.clone(),
                        key: index,
                        line,
                    }),
                }
            }
            _ => Err(RuntimeError::IndexOperatorNotSupported {
                obj: obj.scurry_type(),
                index_type: index.scurry_type(),
                line,
            }),
        }
    }

    fn eval_while_stmt(&mut self, condition: Expr, block: Block) -> EvalResult {
        while self.eval_expr(condition.clone())?.is_truthy() {
            loop_control!(self, block.clone());
        }
        Ok(Object::AbsoluteNil)
    }

    fn eval_for_stmt(
        &mut self,
        iter_ident: Ident,
        expr: Expr,
        block: Block,
        line: usize,
    ) -> EvalResult {
        let obj = self.eval_expr(expr)?;
        match obj {
            Object::Array(arr) => {
                for obj in arr.borrow().iter() {
                    self.env.borrow_mut().set(iter_ident.0.clone(), obj.clone());
                    loop_control!(self, block.clone());
                }
                Ok(Object::AbsoluteNil)
            }
            Object::Map(map) => {
                for (key, _) in map.borrow().iter() {
                    self.env.borrow_mut().set(iter_ident.0.clone(), key.clone());
                    loop_control!(self, block.clone());
                }
                Ok(Object::AbsoluteNil)
            }
            Object::String(string) => {
                for char in string.chars() {
                    self.env
                        .borrow_mut()
                        .set(iter_ident.0.clone(), Object::String(char.to_string()));
                    loop_control!(self, block.clone());
                }
                Ok(Object::AbsoluteNil)
            }
            _ => Err(RuntimeError::CannotIterate {
                obj: obj.scurry_type(),
                line,
            }),
        }
    }

    fn eval_switch_stmt(
        &mut self,
        switch: Object,
        cases: Vec<Case>,
        default: Option<Block>,
    ) -> EvalResult {
        for case in cases {
            for condition in case.conditions {
                if switch == self.eval_expr(condition)? {
                    return self.eval_block(case.block);
                }
            }
        }
        if let Some(block) = default {
            self.eval_block(block)
        } else {
            Ok(Object::AbsoluteNil)
        }
    }

    fn eval_call_expr(
        &mut self,
        func: Object,
        mut args: Vec<Object>,
        type_checked: bool,
        line: usize,
    ) -> EvalResult {
        match func {
            Object::Function {
                params,
                body,
                env,
                bound,
                ..
            } => {
                if let Some(obj) = bound {
                    args.insert(0, Object::Instance((*obj).clone_with_private()));
                }
                if params.len() != args.len() {
                    return Err(RuntimeError::NotEnoughArgs {
                        got: args.len(),
                        want: params.len(),
                        line,
                    });
                }
                if type_checked {
                    for (arg, (name, ty)) in args.iter().zip(&params) {
                        if !arg.fits_type(ty.clone()) {
                            return Err(RuntimeError::WrongArgType {
                                name: name.0.clone(),
                                expected: ty.clone(),
                                got: arg.scurry_type().into(),
                            });
                        }
                    }
                }
                let outer = Rc::clone(&self.env);
                let func_env = {
                    let mut scope = Env::new_enclosed(Rc::clone(&env));
                    for (arg, param) in args.iter().zip(params) {
                        scope.set(param.0 .0, arg.clone());
                    }
                    scope
                };
                self.env = Rc::new(RefCell::new(func_env));
                let result = self.eval_block(body)?;
                self.env = outer;
                if let Object::ControlChange(ControlChange::Return(mut val)) = result {
                    if let Object::Instance(ref mut instance) = *val {
                        instance.visibility = Visibility::Public;
                    }
                    Ok(*val)
                } else if result.is_absnil() {
                    Ok(Object::Nil)
                } else {
                    Ok(result)
                }
            }
            Object::Component(component) => {
                let rc_component = Rc::new(component);
                let mut instance = Instance {
                    component: Rc::clone(&rc_component),
                    field_values: Rc::new(RefCell::new(
                        Rc::clone(&rc_component)
                            .fields
                            .iter()
                            .map(|name| (name.0.to_owned(), Object::Nil))
                            .collect(),
                    )),
                    embeds: Vec::new(),
                    visibility: Visibility::Public,
                };
                if let Some(func) = instance.get_special(SpecialMethod::New) {
                    args.insert(0, Object::Instance(instance.clone_with_private()));
                    self.eval_call_expr(func.clone(), args, type_checked, line)?;
                }
                for (embed, assigned) in &Rc::clone(&rc_component).embeds {
                    let mut args = Vec::new();
                    for expr in assigned {
                        if let Expr::Ident(field) = expr {
                            let arg = if let Some(field_val) =
                                instance.field_values.borrow().get(&field.0)
                            {
                                field_val.clone()
                            } else {
                                self.eval_expr(expr.clone())?
                            };
                            args.push(arg)
                        } else {
                            args.push(self.eval_expr(expr.clone())?)
                        }
                    }
                    let embed_instance =
                        self.eval_call_expr(Object::Component(embed.clone()), args, false, line)?;
                    if let Object::Instance(inst) = embed_instance {
                        instance.embeds.push(inst);
                    } else {
                        unreachable!()
                    }
                }
                Ok(Object::Instance(instance))
            }
            Object::Builtin(func) => func(args, line),
            Object::BuiltinMethod { bound, function } => {
                function(*bound, args, line).expect("bound type should match")
            }
            Object::Instance(instance) if instance.has_special(SpecialMethod::Call) => {
                if let Some(func) = instance.get_special(SpecialMethod::Call) {
                    args.insert(0, Object::Instance(instance.clone_with_private()));
                    self.eval_call_expr(func.clone(), args, type_checked, line)
                } else {
                    unreachable!()
                }
            }
            // Not a function
            _ => Err(RuntimeError::NotCallable {
                obj: func.scurry_type(),
                line,
            }),
        }
    }

    fn eval_dot_expr(&self, left: Object, field: String, line: usize) -> EvalResult {
        macro_rules! builtin_methods {
            ($t:ident, $func:ident) => {
                match builtin::$func(&field) {
                    Some(method) => Ok(Object::BuiltinMethod {
                        bound: Box::new(left),
                        function: method,
                    }),
                    None => Err(RuntimeError::UnrecognizedField {
                        field,
                        obj: Type::$t,
                        line,
                    }),
                }
            };
        }

        match left {
            Object::Instance(Instance {
                ref component,
                ref field_values,
                ref embeds,
                ref visibility,
            }) => match field_values.borrow().get(&field) {
                // Field is there and correct visibility
                Some(value) if visibility == &Visibility::Private => Ok(value.clone()),
                // Field is there, but visibility is wrong and there is no method with the same
                // name
                Some(_)
                    if visibility == &Visibility::Public
                        && component.methods.get(&field).is_none() =>
                {
                    Err(RuntimeError::UnrecognizedField {
                        field,
                        obj: left.scurry_type(),
                        line,
                    })
                }
                // Field is not there or any of the above cases were not activated
                None | Some(_) => match component.methods.get(&field) {
                    Some(method) => {
                        if let Object::Function {
                            params, body, env, ..
                        } = method
                        {
                            if visibility == &Visibility::Public
                                && !component.exports.contains(&field)
                            {
                                Err(RuntimeError::UnrecognizedField {
                                    field,
                                    obj: left.scurry_type(),
                                    line,
                                })
                            } else {
                                Ok(Object::Function {
                                    params: params.clone(),
                                    body: body.clone(),
                                    env: env.clone(),
                                    bound: Some(Rc::new(Instance {
                                        component: component.clone(),
                                        field_values: field_values.clone(),
                                        embeds: embeds.clone(),
                                        visibility: visibility.clone(),
                                    })),
                                    visibility: None,
                                })
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    None => {
                        for embed in embeds {
                            match embed.component.methods.get(&field) {
                                Some(Object::Function {
                                    params, body, env, ..
                                }) => {
                                    if !embed.component.exports.contains(&field) {
                                        return Err(RuntimeError::UnrecognizedField {
                                            field,
                                            obj: left.scurry_type(),
                                            line,
                                        });
                                    }
                                    return Ok(Object::Function {
                                        params: params.clone(),
                                        body: body.clone(),
                                        env: env.clone(),
                                        bound: Some(Rc::new(embed.clone())),
                                        visibility: None,
                                    });
                                }
                                _ => unreachable!(),
                            }
                        }
                        Err(RuntimeError::UnrecognizedField {
                            field,
                            obj: left.scurry_type(),
                            line,
                        })
                    }
                },
            },

            Object::Module { ref exports, .. } => {
                exports
                    .get(&field)
                    .cloned()
                    .ok_or(RuntimeError::UnrecognizedField {
                        field,
                        obj: left.scurry_type(),
                        line,
                    })
            }

            Object::Array(_) => builtin_methods!(Array, get_array_method),
            Object::Map(_) => builtin_methods!(Map, get_map_method),
            Object::String(_) => builtin_methods!(String, get_string_method),
            Object::Int(_) => builtin_methods!(Int, get_int_method),
            Object::Float(_) => builtin_methods!(Float, get_float_method),

            _ => Err(RuntimeError::DotOperatorNotSupported {
                obj: left.scurry_type(),
                line,
            }),
        }
    }

    fn eval_import_stmt(
        &mut self,
        target: String,
        alias: Option<Ident>,
        line: usize,
    ) -> EvalResult {
        let path = PathBuf::from(target);
        // Check that no extension was given
        if path.extension().is_some() {
            return Err(RuntimeError::CouldNotReadFile { name: path, line });
        }
        let contents = fs::read_to_string(path.with_extension("scy")).map_err(|_| {
            RuntimeError::CouldNotReadFile {
                name: path.clone(),
                line,
            }
        })?;
        let parser = Parser::new(&contents);
        let program = parser.parse().map_err(|err| RuntimeError::ParserErrors {
            contents,
            errs: err,
        })?;
        let mut module_interpreter = Interpreter::new();
        module_interpreter.eval(program)?;
        let name = {
            if let Some(alias) = alias {
                alias.0
            } else {
                path.display().to_string()
            }
        };
        self.env.borrow_mut().set(
            name,
            Object::Module {
                file: path,
                exports: Rc::new(module_interpreter.env.borrow_mut().symbols()),
            },
        );
        Ok(Object::AbsoluteNil)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Write};

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
        let expecteds = [
            Object::Int(3),
            Object::Int(3),
            Object::Int(3),
            Object::AbsoluteNil,
        ];

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
                line: 1,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::Array(Rc::new(RefCell::new(Vec::new()))),
                index: 0,
                line: 1,
            },
            RuntimeError::IndexOutOfRange {
                obj: array![Object::Int(1), Object::Int(2), Object::Int(3)],
                index: -4,
                line: 1,
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
                line: 1,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::String("test".to_owned()),
                index: -10,
                line: 1,
            },
            RuntimeError::IndexOutOfRange {
                obj: Object::String("".to_owned()),
                index: 0,
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_map_literal() {
        let inputs = ["{1: 2, \"test\": 3, True: 33};", "{}"];
        let map = {
            let mut map = HashMap::new();
            map.insert(Object::Int(1), Object::Int(2));
            map.insert(Object::String("test".to_owned()), Object::Int(3));
            map.insert(Object::Bool(true), Object::Int(33));
            map
        };
        let expecteds = [
            Object::Map(Rc::new(RefCell::new(map))),
            Object::Map(Rc::new(RefCell::new(HashMap::new()))),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn map_index_expr() {
        let inputs = [
            "{43: 33}[43];",
            "{\"test\": 44}[\"test\"];",
            "{True: 2}[True];",
        ];
        let expecteds = [Object::Int(33), Object::Int(44), Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn invalid_key_in_map_index_expr() {
        let inputs = ["{}[44];", "{1: 3}[33];"];
        let map = {
            let mut map = HashMap::new();
            map.insert(Object::Int(1), Object::Int(3));
            map
        };
        let errs = [
            RuntimeError::KeyNotFound {
                obj: Object::Map(Rc::new(RefCell::new(HashMap::new()))),
                key: Object::Int(44),
                line: 1,
            },
            RuntimeError::KeyNotFound {
                obj: Object::Map(Rc::new(RefCell::new(map))),
                key: Object::Int(33),
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_while_statement() {
        let inputs = [
            "x = 0; while x != 3 { x = x + 1; }; x;",
            "x = 2; while False { x = 0; }; x;",
        ];
        let expecteds = [Object::Int(3), Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_for_stmt() {
        let inputs = [
            "for x in [1, 2, 3] { y = x; }; y;",
            "for x in {44: 3} { y = x; }; y;",
            "for x in \"test\" { y = x; }; y;",
        ];
        let expecteds = [
            Object::Int(3),
            Object::Int(44),
            Object::String("t".to_owned()),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn cannot_iterate_through_some_types() {
        let inputs = ["for x in 1 {}", "for x in True {}", "for x in 3.3 {}"];
        let errs = [
            RuntimeError::CannotIterate {
                obj: Type::Int,
                line: 1,
            },
            RuntimeError::CannotIterate {
                obj: Type::Bool,
                line: 1,
            },
            RuntimeError::CannotIterate {
                obj: Type::Float,
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_function_expr() {
        let inputs = ["fn(x, y) {}", "fn() {}"];
        let expecteds = [
            Object::Function {
                params: vec![
                    (Ident("x".to_owned()), TypeAnnotation::default()),
                    (Ident("y".to_owned()), TypeAnnotation::default()),
                ],
                body: Block(Vec::new()),
                env: Rc::new(RefCell::new(Env::new())),
                bound: None,
                visibility: None,
            },
            Object::Function {
                params: Vec::new(),
                body: Block(Vec::new()),
                env: Rc::new(RefCell::new(Env::new())),
                bound: None,
                visibility: None,
            },
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_function_calls() {
        let inputs = ["fn(x, y) { return x + y; }(1, 2);"];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_function_calls_with_outer_env() {
        let inputs = ["x = 3; fn(y) { return x - y; }(2);"];
        let expecteds = [Object::Int(1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn err_on_uncallable_object() {
        let inputs = ["1();"];
        let errs = [RuntimeError::NotCallable {
            obj: Type::Int,
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_basic_decl_statement() {
        let inputs = [
            "decl Test { field }; Test;",
            "decl Test { fn x(self, x) {} }; Test;",
        ];
        let expecteds = [
            Object::Component(Component {
                name: Ident("Test".to_owned()),
                fields: vec![Ident("field".to_owned())],
                methods: HashMap::new(),
                embeds: Vec::new(),
                exports: Vec::new(),
                visibility: Visibility::Private,
            }),
            Object::Component(Component {
                name: Ident("Test".to_owned()),
                fields: Vec::new(),
                methods: {
                    let mut map = HashMap::new();
                    map.insert(
                        "x".to_owned(),
                        Object::Function {
                            params: vec![
                                (Ident("self".to_owned()), TypeAnnotation::default()),
                                (Ident("x".to_owned()), TypeAnnotation::default()),
                            ],
                            body: Block(Vec::new()),
                            env: Rc::new(RefCell::new(Env::new())),
                            bound: None,
                            visibility: None,
                        },
                    );
                    map
                },
                embeds: Vec::new(),
                exports: Vec::new(),
                visibility: Visibility::Private,
            }),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_embed_in_decl_statement() {
        let inputs = [
            "decl Test { fn x(self, y) {} }; decl Test2 { [Test] {} }; Test2;",
            "decl Test { fn $new(self, param) {} }; decl Test2 { field [Test] { field } }; Test2;",
        ];
        let expecteds = [
            Object::Component(Component {
                name: Ident("Test2".to_owned()),
                fields: Vec::new(),
                methods: HashMap::new(),
                embeds: vec![(
                    Component {
                        name: Ident("Test".to_owned()),
                        fields: Vec::new(),
                        methods: {
                            let mut map = HashMap::new();
                            map.insert(
                                "x".to_owned(),
                                Object::Function {
                                    params: vec![
                                        (Ident("self".to_owned()), TypeAnnotation::default()),
                                        (Ident("y".to_owned()), TypeAnnotation::default()),
                                    ],
                                    body: Block(Vec::new()),
                                    env: Rc::new(RefCell::new(Env::new())),
                                    bound: None,
                                    visibility: None,
                                },
                            );
                            map
                        },
                        embeds: Vec::new(),
                        exports: Vec::new(),
                        visibility: Visibility::Private,
                    },
                    Vec::new(),
                )],
                exports: Vec::new(),
                visibility: Visibility::Private,
            }),
            Object::Component(Component {
                name: Ident("Test2".to_owned()),
                fields: vec![Ident("field".to_owned())],
                methods: HashMap::new(),
                embeds: vec![(
                    Component {
                        name: Ident("Test".to_owned()),
                        fields: Vec::new(),
                        methods: {
                            let mut map = HashMap::new();
                            map.insert(
                                "$new".to_owned(),
                                Object::Function {
                                    params: vec![
                                        (Ident("self".to_owned()), TypeAnnotation::default()),
                                        (Ident("param".to_owned()), TypeAnnotation::default()),
                                    ],
                                    body: Block(Vec::new()),
                                    env: Rc::new(RefCell::new(Env::new())),
                                    bound: None,
                                    visibility: None,
                                },
                            );
                            map
                        },
                        embeds: Vec::new(),
                        exports: Vec::new(),
                        visibility: Visibility::Private,
                    },
                    vec![Expr::Ident(Ident("field".to_owned()))],
                )],
                exports: Vec::new(),
                visibility: Visibility::Private,
            }),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_exprs_in_assigned_fields() {
        let inputs = ["decl Test { field fn $new(self, field) { self.field = field; } exp fn test(self) { return self.field; } }; decl Test2 { [Test] { 1 + 1 } }; Test2().test();"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_object_instance() {
        let inputs = [
            "decl Test { field }; Test();",
            "decl Test { fn $new(self, x) {} }; Test(3);",
        ];
        let expecteds = [
            Object::Instance(Instance {
                component: Rc::new(Component {
                    name: Ident("Test".to_owned()),
                    fields: vec![Ident("field".to_owned())],
                    methods: HashMap::new(),
                    embeds: Vec::new(),
                    exports: Vec::new(),
                    visibility: Visibility::Private,
                }),
                field_values: Rc::new(RefCell::new({
                    let mut map = HashMap::new();
                    map.insert("field".to_owned(), Object::Nil);
                    map
                })),
                embeds: Vec::new(),
                visibility: Visibility::Public,
            }),
            Object::Instance(Instance {
                component: Rc::new(Component {
                    name: Ident("Test".to_owned()),
                    fields: Vec::new(),
                    methods: {
                        let mut map = HashMap::new();
                        map.insert(
                            "$new".to_owned(),
                            Object::Function {
                                params: vec![
                                    (Ident("self".to_owned()), TypeAnnotation::default()),
                                    (Ident("x".to_owned()), TypeAnnotation::default()),
                                ],
                                body: Block(Vec::new()),
                                env: Rc::new(RefCell::new(Env::new())),
                                bound: None,
                                visibility: None,
                            },
                        );
                        map
                    },
                    embeds: Vec::new(),
                    exports: Vec::new(),
                    visibility: Visibility::Private,
                }),
                field_values: Rc::new(RefCell::new(HashMap::new())),
                visibility: Visibility::Public,
                embeds: Vec::new(),
            }),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_methods_and_dot_expr() {
        let inputs = [
            "decl Test { exp fn x(self, y) { return y; } }; Test().x(3);",
            "decl Test { exp fn x(self) { return 3; } }; Test().x();",
            "decl Test { field exp fn x(self) { return self.field; } }; Test().x();",
        ];
        let expecteds = [Object::Int(3), Object::Int(3), Object::Nil];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_embedded_methods_in_component_instances() {
        let inputs = [
            "decl Test { exp fn x(self) { return 3; } }; decl Test2 { [Test] {} }; Test2().x();",
        ];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn private_methods_usable_in_component() {
        let inputs = ["decl Test { exp fn x(self) { return self.y(); } fn y(self) { return 3; } }; Test().x();"];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn invalid_use_of_dot_operator() {
        let inputs = ["True.field;"];
        let errs = [RuntimeError::DotOperatorNotSupported {
            obj: Type::Bool,
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn unrecognized_field_in_component_instance() {
        let inputs = ["decl Test { field }; Test().invalid;"];
        let errs = [RuntimeError::UnrecognizedField {
            field: "invalid".to_owned(),
            obj: Type::Instance("Test".to_owned()),
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn invalid_embed() {
        let inputs = ["decl Test { [DoesNotExist] {} }; Test();"];
        let errs = [RuntimeError::InvalidEmbed {
            name: "DoesNotExist".to_owned(),
            line: 1,
        }];

        runtime_error_eval!(inputs, errs);
    }

    #[test]
    fn field_assignment() {
        let inputs =
            ["decl Test { field fn $new(self) { self.field = 5; } exp fn field(self) { return self.field; } }; x = Test(); x.field();"];
        let expecteds = [Object::Int(5)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn index_assignment_on_maps() {
        let inputs = ["x = {}; x[\"test\"] = 55; x[\"test\"];"];
        let expecteds = [Object::Int(55)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn index_assignment_on_arrays() {
        let inputs = ["x = [1, 2, 3]; x[2] = 8; x[2];"];
        let expecteds = [Object::Int(8)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn cannot_index_assign_on_out_of_bounds_array() {
        let inputs = ["x = [1, 2, 3]; x[5] = 3;"];
        let errs = [RuntimeError::IndexOutOfRange {
            obj: Object::Array(Rc::new(RefCell::new(vec![
                Object::Int(1),
                Object::Int(2),
                Object::Int(3),
            ]))),
            index: 5,
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_switch_stmt() {
        let inputs = [
            "switch 3 { case 3 { x = 3; } }; x;",
            "switch 3 { case 9 | 3 { x = 4; } }; x;",
            "switch \"hello\" { case \"helo\" { x = 5; } case 3 { x = 3; } default { x = 8; } }; x;"
        ];
        let expecteds = [Object::Int(3), Object::Int(4), Object::Int(8)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn return_from_nested_block() {
        let inputs = [
            "fn x(y) { for i in y { return i; } }; x([1, 2, 3]);",
            "fn x() { while True { return 3; } }; x();",
            "fn x() { if True { return 3; } }; x();",
            "fn x() { switch 3 { case 3 { return 4; } } }; x();",
        ];
        let expecteds = [
            Object::Int(1),
            Object::Int(3),
            Object::Int(3),
            Object::Int(4),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_break_and_continue() {
        let inputs = [
            "y = 0; for i in [1, 2, 3] { break; y = y + 1; }; y;",
            "y = 0; for i in [1, 2, 3] { if i == 2 { continue; }; y = y + 1; }; y;",
        ];
        let expecteds = [Object::Int(0), Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_type() {
        let inputs = ["type(3);", "type([1, 2, 3]);"];
        let expecteds = [
            Object::String("Int".to_owned()),
            Object::String("Array".to_owned()),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtins_error_with_wrong_number_of_args() {
        let inputs = ["type(3, 3);"];
        let errs = [RuntimeError::NotEnoughArgs {
            got: 2,
            want: 1,
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn builtin_array_push() {
        let inputs = ["x = [1, 2, 3]; x.push(3); x;", "[1, 2, 3].push(4);"];
        let expecteds = [
            Object::Array(Rc::new(RefCell::new(vec![
                Object::Int(1),
                Object::Int(2),
                Object::Int(3),
                Object::Int(3),
            ]))),
            Object::Nil,
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_array_len() {
        let inputs = ["[1, 2, 2].len();"];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_array_pop() {
        let inputs = ["x = [1, 2, 3]; x.pop(); x;", "[1, 2, 3].pop();"];
        let expecteds = [
            Object::Array(Rc::new(RefCell::new(vec![Object::Int(1), Object::Int(2)]))),
            Object::Int(3),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_string_len() {
        let inputs = ["\"test\".len();"];
        let expecteds = [Object::Int(4)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_string_trim() {
        let inputs = ["\"test     \".trim();"];
        let expecteds = [Object::String("test".to_owned())];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_int_abs() {
        let inputs = ["(-1).abs();", "1.abs();"];
        let expecteds = [Object::Int(1), Object::Int(1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_int_to_float() {
        let inputs = ["1.to_float();"];
        let expecteds = [Object::Float(1.0)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_float_abs() {
        let inputs = ["(-1.0).abs();", "1.0.abs();"];
        let expecteds = [Object::Float(1.0), Object::Float(1.0)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_float_to_int() {
        let inputs = ["1.4.to_int();", "1.0.to_int();", "1.8.to_int();"];
        let expecteds = [Object::Int(1), Object::Int(1), Object::Int(1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn builtin_map_remove() {
        let inputs = ["x = {1: 3}; x.remove(1); x;"];
        let expecteds = [Object::Map(Rc::new(RefCell::new(HashMap::new())))];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn assign_stmt_works_with_any_expression() {
        let inputs = ["x = [1, 2, [1]]; x[2][0] = 3; x[2];"];
        let expecteds = [Object::Array(Rc::new(RefCell::new(vec![Object::Int(3)])))];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn cannot_assign_to_wrong_expr_type() {
        let inputs = ["1 = 3;"];
        let errs = [RuntimeError::CannotAssign {
            expr: Expr::Literal(Literal::Integer(1)),
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_operator_assignment() {
        let inputs = [
            "x = 0; x += 1; x;",
            "x = 0; x -= 1; x;",
            "x = 2; x *= 2; x;",
            "x = 2; x /= 2; x;",
            "x = 20; x %= 2; x;",
        ];
        let expecteds = [
            Object::Int(1),
            Object::Int(-1),
            Object::Int(4),
            Object::Int(1),
            Object::Int(0),
        ];

        test_eval!(inputs, expecteds);
    }

    #[test]
    fn eval_operator_assignment_in_hashmap() {
        let inputs = ["x = {\"hello\": 0}; x[\"hello\"] -= 1; x[\"hello\"];"];
        let expecteds = [Object::Int(-1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_operator_assignment_in_array() {
        let inputs = ["x = [1, 2, 3]; x[0] *= 4; x[0];"];
        let expecteds = [Object::Int(4)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_operator_assignment_in_struct_field() {
        let inputs = ["decl Test { test fn $new(self) { self.test = 0; self.test += 1; } exp fn show(self) { return self.test; } }; x = Test(); x.show();"];
        let expecteds = [Object::Int(1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_import_stmt() {
        let mut file = File::create("test.scy").expect("should create file correctly");
        file.write_all(b"exp fn test() { return 1; }")
            .expect("should write bytes");

        let inputs = ["import \"test\"; test.test();"];
        let expecteds = [Object::Int(1)];

        test_eval!(inputs, expecteds);

        fs::remove_file("test.scy").expect("should remove file");
    }

    #[test]
    fn import_stmt_errors_with_invalid_file() {
        let inputs = [
            "import \"doesnotexist\";",
            "import \"\";",
            "import \"test.txt\";",
        ];
        let errs = [
            RuntimeError::CouldNotReadFile {
                name: PathBuf::from("doesnotexist"),
                line: 1,
            },
            RuntimeError::CouldNotReadFile {
                name: PathBuf::from(""),
                line: 1,
            },
            RuntimeError::CouldNotReadFile {
                name: PathBuf::from("test.txt"),
                line: 1,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn plus_op_overload() {
        let inputs =
            ["decl Test { fn $add(self, other) { return other + 1; } }; x = Test(); x + 1;"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn minus_op_overload() {
        let inputs =
            ["decl Test { fn $sub(self, other) { return other + 1; } }; x = Test(); x - 1;"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn mul_op_overload() {
        let inputs =
            ["decl Test { fn $mul(self, other) { return other + 1; } }; x = Test(); x * 1;"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn div_op_overload() {
        let inputs =
            ["decl Test { fn $div(self, other) { return other + 1; } }; x = Test(); x / 1;"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn mod_op_overload() {
        let inputs =
            ["decl Test { fn $mod(self, other) { return other + 1; } }; x = Test(); x % 1;"];
        let expecteds = [Object::Int(2)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn non_operator_overloaded_components_throws_runtime_error() {
        let inputs = ["decl Test {}; Test() + 1;"];
        let errs = [RuntimeError::InvalidBinaryOperand {
            op: InfixOp::Plus,
            left: Type::Instance("Test".to_owned()),
            right: Type::Int,
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn eval_type_checked_function_call() {
        let inputs = ["fn x(y: Int) { return 1; }; x(1)!;"];
        let expecteds = [Object::Int(1)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn type_checked_function_call_with_mismatched_type_throws_error() {
        let inputs = [
            "fn x(y: Int) {}; x(3.3)!;",
            "fn x(y: Int | Float) {}; x([1])!;",
        ];
        let errs = [
            RuntimeError::WrongArgType {
                name: "y".to_owned(),
                expected: TypeAnnotation::from_iter([AstType::Int]),
                got: AstType::Float,
            },
            RuntimeError::WrongArgType {
                name: "y".to_owned(),
                expected: TypeAnnotation::from_iter([AstType::Int, AstType::Float]),
                got: AstType::Array,
            },
        ];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn type_checked_function_call_works_with_instances() {
        let inputs = ["decl Test {}; fn x(y: Test) {}; x(Test())!;"];
        let expecteds = [Object::Nil];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn type_checked_assignment() {
        let inputs = ["x!: Int = 3;"];
        let expecteds = [Object::AbsoluteNil];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn type_checked_assignment_with_mismatched_type_throws_error() {
        let inputs = ["x!: Int = 3.3;"];
        let errs = [RuntimeError::MismatchedAssignType {
            name: "x".to_owned(),
            expected: TypeAnnotation::from_iter([AstType::Int]),
            got: AstType::Float,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn cannot_return_public_access_from_method() {
        let inputs = ["decl Test { field fn $new(self) { self.field = 3; } exp fn test(self) { return self; } }; Test().test().field;"];
        let errs = [RuntimeError::UnrecognizedField {
            field: "field".to_owned(),
            obj: Type::Instance("Test".to_owned()),
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }
}
