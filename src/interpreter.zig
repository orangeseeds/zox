const std = @import("std");
const expr = @import("expression.zig");
const Expr = expr.Expr;
const Stmt = expr.Stmt;
const scanner = @import("scanner.zig");
const Token = scanner.Token;
const parser = @import("parser.zig");
const Literal = scanner.Literal;
const TokenType = @import("scanner.zig").TokenType;
const env = @import("environment.zig");
const Environment = env.Environment;

const EvalErrors = error{
    TypeErrorNumbers,
    TypeErrorStrings,
    IllegalOperator,
    UndefinedVar,
} || std.mem.Allocator.Error;

pub const ValueType = enum {
    Number,
    String,
    Bool,
    Nil,
};

pub const Value = union(ValueType) {
    const Self = @This();
    Number: f64,
    String: []const u8,
    Bool: bool,
    Nil,

    pub fn equal(self: Value, other: Value) bool {
        const same_type = switch (self) {
            .Number => @as(ValueType, other) == .Number,
            .String => @as(ValueType, other) == .String,
            .Bool => @as(ValueType, other) == .Bool,
            .Nil => @as(ValueType, other) == .Nil,
        };
        if (!same_type) return false;

        return switch (self) {
            .Number => self.Number == other.Number,
            .String => std.mem.eql(u8, self.String, other.String),
            .Bool => self.Bool == other.Bool,
            .Nil => true,
        };
    }

    fn is_truthy(value: Value) bool {
        return switch (value) {
            .Bool => |val| val,
            .Nil => false,
            else => true,
        };
    }
};

fn check_op_type(expected: ValueType, val: Value) bool {
    return @as(ValueType, val) == expected;
}

fn check_op_types(expected: ValueType, left: Value, right: Value) bool {
    return check_op_type(expected, left) and check_op_type(expected, right);
}

pub const Interpreter = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    runtime_error: struct { message: []const u8, token: Token } = .{
        .message = undefined,
        .token = undefined,
    },
    has_error: bool = false,
    environment: Environment,

    fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .environment = Environment.init(allocator),
        };
    }
    fn deinit(self: *Self) void {
        try self.environment.deinit();
    }

    fn raise_err(self: *Self, err: EvalErrors, message: []const u8, token: Token) EvalErrors {
        self.runtime_error = .{ .message = message, .token = token };
        self.has_error = true;
        return err;
    }

    fn evaluate_literal(expression: expr.LiteralExpr) Value {
        if (expression.value) |val| {
            return switch (val) {
                .String => |str| Value{ .String = str },
                .Number => |num| Value{ .Number = num },
                .True => Value{ .Bool = true },
                .False => Value{ .Bool = false },
                .Nil => Value{ .Nil = undefined },
            };
        } else {
            unreachable;
        }
    }

    fn evaluate_unary(self: *Self, expression: expr.UnaryExpr) EvalErrors!Value {
        const right = try self.evaluate_expr(expression.right.*);

        switch (expression.operator.token_type) {
            .MINUS => {
                if (!check_op_type(.Number, right))
                    return self.raise_err(error.TypeErrorNumbers, "Operand must be a number", expression.operator);
                return Value{ .Number = -right.Number };
            },
            .BANG => return Value{ .Bool = !right.is_truthy() },
            else => return self.raise_err(error.IllegalOperator, "Invalid Operator for unary expr", expression.operator),
        }
    }

    fn evaluate_binary(self: *Self, expression: expr.BinaryExpr) EvalErrors!Value {
        const left = try self.evaluate_expr(expression.left.*);
        const right = try self.evaluate_expr(expression.right.*);

        if (!check_op_types(.Number, left, right))
            return self.raise_err(error.TypeErrorNumbers, "Operand must be a number", expression.operator);

        return switch (expression.operator.token_type) {
            .MINUS => Value{ .Number = left.Number - right.Number },
            .SLASH => Value{ .Number = left.Number / right.Number },
            .STAR => Value{ .Number = left.Number * right.Number },
            .PLUS => Value{ .Number = left.Number + right.Number },
            .GREATER => Value{ .Bool = left.Number > right.Number },
            .GREATER_EQUAL => Value{ .Bool = left.Number >= right.Number },
            .LESS => Value{ .Bool = left.Number < right.Number },
            .LESS_EQUAL => Value{ .Bool = left.Number <= right.Number },
            .BANG_EQUAL => Value{ .Bool = !left.equal(right) },
            .EQUAL_EQUAL => Value{ .Bool = left.equal(right) },
            else => return self.raise_err(error.IllegalOperator, "Illegal Operator for binary expr", expression.operator),
        };
    }

    fn evaluate_group(self: *Self, expression: expr.GroupExpr) EvalErrors!Value {
        return try self.evaluate_expr(expression.expression.*);
    }

    fn eval_var_expr(self: *Self, expression: expr.VariableExpr) EvalErrors!Value {
        return self.environment.get(expression.name.lexeme) catch |err| switch (err) {
            env.EnvError.NotDeclaredAndUndefined => return self.raise_err(error.UndefinedVar, "Not Declared and Undefined", expression.name),
            env.EnvError.DeclaredButUndefined => return self.raise_err(error.UndefinedVar, "Declared But Undefined", expression.name),
        };
    }

    fn eval_asgn_expr(self: *Self, expression: expr.AssignExpr) EvalErrors!Value {
        // TODO: handle error better write tests
        const value = try self.evaluate_expr(expression.value.*);
        self.environment.assign(expression.name, value) catch
            return self.raise_err(error.UndefinedVar, "Not Declared and Undefined", expression.name);
        return value;
    }

    pub fn evaluate_expr(self: *Self, expression: expr.Expr) EvalErrors!Value {
        return switch (expression) {
            .Binary => |bin| try self.evaluate_binary(bin),
            .Unary => |un| try self.evaluate_unary(un),
            .Group => |grp| try self.evaluate_group(grp),
            .Literal => |lit| evaluate_literal(lit),
            .Variable => |v| try self.eval_var_expr(v),
            .Assign => |a| try self.eval_asgn_expr(a),
        };
    }

    fn execute_blk(self: *Self, stmts: std.ArrayList(Stmt), environment: Environment) EvalErrors!void {
        // TODO: write tests for block
        const prev = self.environment;

        self.environment = environment;
        for (stmts) |stmt| {
            try self.evaluate_stmt(stmt);
        }

        self.environment = prev;
    }

    // Evaluating Statements
    fn eval_print_stmt(self: *Self, expression: expr.PrintStmt) EvalErrors!void {
        const val = try self.evaluate_expr(expression);
        std.debug.print("{}\n", .{val});
    }
    fn eval_expr_stmt(self: *Self, expression: expr.ExprStmt) EvalErrors!Value {
        return try self.evaluate_expr(expression);
    }
    fn eval_var_stmt(self: *Self, expression: expr.VarStmt) EvalErrors!void {
        var val: ?Value = null;
        if (expression.initializer) |exp| {
            val = try self.evaluate_expr(exp);
        }
        try self.environment.define(expression.name.lexeme, val);
    }
    fn eval_blk_stmt(self: *Self, stmt: expr.BlockStmt) EvalErrors!void {
        self.execute_blk(stmt, Environment.with_enclosing(self.allocator, self.environment));
    }

    fn evaluate_stmt(self: *Self, stmt: expr.Stmt) EvalErrors!void {
        switch (stmt) {
            .PrintStmt => |s| try self.eval_print_stmt(s),
            .ExprStmt => |e| _ = try self.eval_expr_stmt(e),
            .VarStmt => |v| try self.eval_var_stmt(v),
            else => unreachable,
        }
    }
};

test "Assignment Expression" {}
test "Variable Expression" {}
test "General Expression Evaluation" {}

test "Variable Expression Evaluations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "var value = (20 * 20);";
    var lexer = try scanner.Scanner.init(allocator, src);
    defer lexer.deinit();
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());
    defer par.deinit();

    var stmts = try par.parse_stmts(allocator);
    defer stmts.deinit();

    const var_stmt = stmts.pop();

    var interpreter = Interpreter.init(allocator);
    try interpreter.evaluate_stmt(var_stmt);

    const val = try interpreter.eval_var_expr(expr.VariableExpr{ .name = var_stmt.VarStmt.name });

    try std.testing.expect(val.equal(Value{ .Number = 400 }));
}

test "Testing Expression Stmt Evaluations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const table = [_]struct { []const u8, Value }{
        .{ "\"true\";", val_str("true") },
    };

    for (table) |value| {
        const result = try eval_str(allocator, value[0]);
        std.debug.print("\n{s} {s}\n", .{ result.String, value[1].String });
        try std.testing.expect(result.equal(value[1]));
    }
}

fn val_bool(val: bool) Value {
    return Value{ .Bool = val };
}
fn val_str(val: []const u8) Value {
    return Value{ .String = val };
}
fn val_num(val: f32) Value {
    return Value{ .Number = val };
}

fn eval_str(allocator: std.mem.Allocator, str: []const u8) !Value {
    var lexer = try scanner.Scanner.init(allocator, str);
    defer lexer.deinit();
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());
    defer par.deinit();

    var stmts = try par.parse_stmts(allocator);
    defer stmts.deinit();

    var interpreter = Interpreter.init(allocator);
    const val = try allocator.create(Value);
    val.* = try interpreter.eval_expr_stmt(stmts.pop().ExprStmt);
    return val.*;
}
