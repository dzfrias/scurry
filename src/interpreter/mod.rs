mod builtin;
mod env;
mod object;

use self::env::Env;
use self::object::*;
use crate::ast::*;
use std::cell::RefCell;
use std::collections::HashMap;
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
            // Get rid of return val wrappers, no longer needed
            if let Object::ReturnVal(val) = result {
                result = *val
            }
        }
        Ok(result)
    }

    fn eval_block(&mut self, block: Block) -> EvalResult {
        for stmt in block.0 {
            let result = self.eval_stmt(stmt)?;
            if matches!(result, Object::ReturnVal(_)) {
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
                index,
                field,
                line,
            }) => {
                let val = self.eval_expr(value)?;
                if index.is_none() && field.is_none() {
                    self.env.borrow_mut().set(name.0, val);
                    return Ok(Object::AbsoluteNil);
                }
                let env_obj = self.env.borrow().get(&name.0);
                if let Some(obj) = env_obj {
                    if let Some(field_name) = field {
                        if let Object::Instance(instance) = obj {
                            if !instance.field_values.borrow().contains_key(&field_name.0) {
                                return Err(RuntimeError::InvalidAssignedField {
                                    field: field_name.0,
                                    line,
                                });
                            }
                            instance.field_values.borrow_mut().insert(field_name.0, val);
                            return Ok(Object::AbsoluteNil);
                        } else {
                            return Err(RuntimeError::DotOperatorNotSupported {
                                obj: obj.scurry_type(),
                                line,
                            });
                        }
                    }
                    if let Some(index) = index {
                        let idx = self.eval_expr(index)?;
                        return match (&obj, &idx) {
                            (
                                Object::Map(map),
                                Object::Int(_) | Object::String(_) | Object::Bool(_),
                            ) => {
                                map.borrow_mut().insert(idx, val);
                                Ok(Object::AbsoluteNil)
                            }
                            (Object::Array(arr), Object::Int(i)) => {
                                if *i < 0 {
                                    if i.unsigned_abs() as usize > arr.borrow().len() {
                                        return Err(RuntimeError::IndexOutOfRange {
                                            obj,
                                            index: *i,
                                            line,
                                        });
                                    }
                                    let rev_idx = arr.borrow().len() - 1;
                                    arr.borrow_mut()[rev_idx] = val;
                                    return Ok(Object::AbsoluteNil);
                                }
                                let idx = *i as usize;
                                if arr.borrow().len() == 0 || idx > arr.borrow().len() - 1 {
                                    Err(RuntimeError::IndexOutOfRange {
                                        obj,
                                        index: *i,
                                        line,
                                    })
                                } else {
                                    arr.borrow_mut()[idx] = val;
                                    Ok(Object::AbsoluteNil)
                                }
                            }
                            _ => Err(RuntimeError::IndexOperatorNotSupported {
                                obj: obj.scurry_type(),
                                index_type: idx.scurry_type(),
                                line,
                            }),
                        };
                    }
                } else {
                    return Err(RuntimeError::VariableNotFound { name: name.0, line });
                };
                Ok(Object::AbsoluteNil)
            }
            Stmt::While(WhileStmt { condition, block }) => self.eval_while_stmt(condition, block),
            Stmt::Return(ReturnStmt { value }) => {
                Ok(Object::ReturnVal(Box::new(self.eval_expr(value)?)))
            }
            Stmt::Function(FunctionStmt {
                name,
                params,
                block,
            }) => {
                let func = Object::Function {
                    params,
                    body: block,
                    env: Rc::clone(&self.env),
                    bound: None,
                };
                self.env.borrow_mut().set(name.0, func);
                Ok(Object::AbsoluteNil)
            }
            Stmt::Declaration(DeclarationStmt {
                name,
                methods,
                fields,
                embeds,
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
                        for assigned in &embed.assigned {
                            if !fields.contains(assigned) {
                                return Err(RuntimeError::InvalidAssignedField {
                                    field: assigned.0.clone(),
                                    line: embed.line,
                                });
                            }
                        }
                        embedded.push((embed_component, embed.assigned));
                    }
                    embedded
                };
                self.env.borrow_mut().set(
                    name.0.clone(),
                    Object::Component(Component {
                        name,
                        fields,
                        methods: methods
                            .into_iter()
                            .map(|func| {
                                (
                                    func.name.0,
                                    Object::Function {
                                        params: func.params,
                                        body: func.block,
                                        env: Rc::clone(&self.env),
                                        bound: None,
                                    },
                                )
                            })
                            .collect(),
                        embeds: embedded,
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
            Stmt::Expr(expr) => self.eval_expr(expr),
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
            Expr::Function(FunctionExpr { params, block }) => Ok(Object::Function {
                params,
                body: block,
                env: Rc::clone(&self.env),
                bound: None,
            }),
            Expr::Dot(DotExpr { left, field, line }) => {
                let expr = self.eval_expr(*left)?;
                self.eval_dot_expr(expr, field.0, line)
            }
            Expr::Call(CallExpr { func, args, line }) => {
                let func = self.eval_expr(*func)?;
                let args = {
                    let mut result = Vec::new();
                    for arg in args {
                        result.push(self.eval_expr(arg)?);
                    }
                    result
                };
                self.eval_call_expr(func, args, line)
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
        } else if let Some(func) = builtin::get_builtin(name) {
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
                    return Ok(Object::AbsoluteNil);
                }
            }
            if let Some(block) = else_block {
                self.eval_block(block)?;
            }
        }
        Ok(Object::AbsoluteNil)
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
            self.eval_block(block.clone())?;
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
                    self.eval_block(block.clone())?;
                }
                Ok(Object::AbsoluteNil)
            }
            Object::Map(map) => {
                for (key, _) in map.borrow().iter() {
                    self.env.borrow_mut().set(iter_ident.0.clone(), key.clone());
                    self.eval_block(block.clone())?;
                }
                Ok(Object::AbsoluteNil)
            }
            Object::String(string) => {
                for char in string.chars() {
                    self.env
                        .borrow_mut()
                        .set(iter_ident.0.clone(), Object::String(char.to_string()));
                    self.eval_block(block.clone())?;
                }
                Ok(Object::AbsoluteNil)
            }
            _ => Err(RuntimeError::CannotIterate {
                obj: obj.scurry_type(),
                line,
            }),
        }
    }

    fn eval_call_expr(&mut self, func: Object, mut args: Vec<Object>, line: usize) -> EvalResult {
        match func {
            Object::Function {
                params,
                body,
                env,
                bound,
            } => {
                if let Some(obj) = bound {
                    args.insert(0, Object::Instance((*obj).clone()));
                }
                if params.len() != args.len() {
                    return Err(RuntimeError::NotEnoughArgs {
                        got: args.len(),
                        want: params.len(),
                        line,
                    });
                }
                let outer = Rc::clone(&self.env);
                let func_env = {
                    let mut scope = Env::new_enclosed(Rc::clone(&env));
                    for (arg, param) in args.iter().zip(params) {
                        scope.set(param.0, arg.clone());
                    }
                    scope
                };
                self.env = Rc::new(RefCell::new(func_env));
                let result = self.eval_block(body)?;
                self.env = outer;
                Ok(result)
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
                };
                if let Some(func) = Rc::clone(&rc_component).methods.get("$new") {
                    let mut new_args = args.clone();
                    new_args.insert(0, Object::Instance(instance.clone()));
                    self.eval_call_expr(func.clone(), new_args, 1)?;
                }
                for (embed, assigned) in &Rc::clone(&rc_component).embeds {
                    let mut args = Vec::new();
                    for field in assigned {
                        args.push(
                            instance
                                .field_values
                                .borrow()
                                .get(&field.0)
                                .expect("all assigned fields should be in field list")
                                .clone(),
                        )
                    }
                    let embed_instance =
                        self.eval_call_expr(Object::Component(embed.clone()), args, 1)?;
                    if let Object::Instance(inst) = embed_instance {
                        instance.embeds.push(inst);
                    } else {
                        unreachable!()
                    }
                }
                Ok(Object::Instance(instance))
            }
            Object::Builtin(func) => func(args, line),
            // Not a function
            _ => Err(RuntimeError::NotCallable {
                obj: func.scurry_type(),
                line,
            }),
        }
    }

    fn eval_dot_expr(&self, left: Object, field: String, line: usize) -> EvalResult {
        match left {
            Object::Instance(Instance {
                ref component,
                ref field_values,
                ref embeds,
            }) => match field_values.borrow().get(&field) {
                Some(value) => Ok(value.clone()),
                None => match component.methods.get(&field) {
                    Some(method) => {
                        if let Object::Function {
                            params, body, env, ..
                        } = method
                        {
                            Ok(Object::Function {
                                params: params.clone(),
                                body: body.clone(),
                                env: env.clone(),
                                bound: Some(Rc::new(Instance {
                                    component: component.clone(),
                                    field_values: field_values.clone(),
                                    embeds: embeds.clone(),
                                })),
                            })
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
                                    return Ok(Object::Function {
                                        params: params.clone(),
                                        body: body.clone(),
                                        env: env.clone(),
                                        bound: Some(Rc::new(embed.clone())),
                                    })
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
            _ => Err(RuntimeError::DotOperatorNotSupported {
                obj: left.scurry_type(),
                line,
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
                params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                body: Block(Vec::new()),
                env: Rc::new(RefCell::new(Env::new())),
                bound: None,
            },
            Object::Function {
                params: Vec::new(),
                body: Block(Vec::new()),
                env: Rc::new(RefCell::new(Env::new())),
                bound: None,
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
            }),
            Object::Component(Component {
                name: Ident("Test".to_owned()),
                fields: Vec::new(),
                methods: {
                    let mut map = HashMap::new();
                    map.insert(
                        "x".to_owned(),
                        Object::Function {
                            params: vec![Ident("self".to_owned()), Ident("x".to_owned())],
                            body: Block(Vec::new()),
                            env: Rc::new(RefCell::new(Env::new())),
                            bound: None,
                        },
                    );
                    map
                },
                embeds: Vec::new(),
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
                                    params: vec![Ident("self".to_owned()), Ident("y".to_owned())],
                                    body: Block(Vec::new()),
                                    env: Rc::new(RefCell::new(Env::new())),
                                    bound: None,
                                },
                            );
                            map
                        },
                        embeds: Vec::new(),
                    },
                    Vec::new(),
                )],
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
                                        Ident("self".to_owned()),
                                        Ident("param".to_owned()),
                                    ],
                                    body: Block(Vec::new()),
                                    env: Rc::new(RefCell::new(Env::new())),
                                    bound: None,
                                },
                            );
                            map
                        },
                        embeds: Vec::new(),
                    },
                    vec![Ident("field".to_owned())],
                )],
            }),
        ];

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
                }),
                field_values: Rc::new(RefCell::new({
                    let mut map = HashMap::new();
                    map.insert("field".to_owned(), Object::Nil);
                    map
                })),
                embeds: Vec::new(),
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
                                params: vec![Ident("self".to_owned()), Ident("x".to_owned())],
                                body: Block(Vec::new()),
                                env: Rc::new(RefCell::new(Env::new())),
                                bound: None,
                            },
                        );
                        map
                    },
                    embeds: Vec::new(),
                }),
                field_values: Rc::new(RefCell::new(HashMap::new())),
                embeds: Vec::new(),
            }),
        ];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_methods_and_dot_expr() {
        let inputs = [
            "decl Test { fn x(self, y) { return y; } }; Test().x(3);",
            "decl Test { fn x(self) { return 3; } }; Test().x();",
            "decl Test { field fn x(self) { return self.field; } }; Test().x();",
        ];
        let expecteds = [Object::Int(3), Object::Int(3), Object::Nil];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn eval_embedded_methods_in_component_instances() {
        let inputs =
            ["decl Test { fn x(self) { return 3; } }; decl Test2 { [Test] {} }; Test2().x();"];
        let expecteds = [Object::Int(3)];

        test_eval!(inputs, expecteds)
    }

    #[test]
    fn invalid_use_of_dot_operator() {
        let inputs = ["\"hello\".world;", "3.3.test;"];
        let errs = [
            RuntimeError::DotOperatorNotSupported {
                obj: Type::String,
                line: 1,
            },
            RuntimeError::DotOperatorNotSupported {
                obj: Type::Float,
                line: 1,
            },
        ];

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
    fn invalid_assigned_field() {
        let inputs =
            ["decl Test { fn $new(self, x) {} }; decl Test2 { [Test] { field } }; Test2();"];
        let errs = [RuntimeError::InvalidAssignedField {
            field: "field".to_owned(),
            line: 1,
        }];

        runtime_error_eval!(inputs, errs)
    }

    #[test]
    fn field_assignment() {
        let inputs =
            ["decl Test { field fn $new(self) { self.field = 5; } }; x = Test(); x.field;"];
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
}
