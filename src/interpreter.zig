const std = @import("std");
const expr = @import("expression.zig");
const parser = @import("parser.zig");
const env = @import("environment.zig");

const Allocator = std.mem.Allocator;

const Expr = expr.Expr;
const LiteralExpr = expr.LiteralExpr;
const UnaryExpr = expr.UnaryExpr;
const BinaryExpr = expr.BinaryExpr;
const GroupExpr = expr.GroupExpr;
const AssignExpr = expr.AssignExpr;
const VariableExpr = expr.VariableExpr;

const Stmt = expr.Stmt;
const ExprStmt = expr.ExprStmt;
const PrintStmt = expr.PrintStmt;
const VarStmt = expr.VarStmt;
const BlockStmt = expr.BlockStmt;

const scanner = @import("scanner.zig");
const Token = scanner.Token;

const Literal = scanner.Literal;
const TokenType = scanner.TokenType;
const Environment = env.Environment;
const EnvError = env.EnvError;

const RuntimeError = error{
    BinaryOperandMismatch,
    UnaryOperandMismatch,
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
    const Error = struct {
        err: RuntimeError,
        message: []const u8,
        token: Token,
    };

    allocator: Allocator,
    runtime_error: Error = .{ .err = undefined, .token = undefined, .message = undefined },
    has_error: bool = false,
    environment: Environment,

    fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .environment = Environment.init(allocator),
        };
    }
    fn deinit(self: *Self) void {
        try self.environment.deinit();
    }

    fn raise_err(self: *Self, err: RuntimeError, token: Token, comptime fmt: []const u8, args: anytype) RuntimeError!void {
        self.has_error = true;
        self.runtime_error = .{
            .err = err,
            .message = try std.fmt.allocPrint(self.allocator, fmt, args),
            .token = token,
        };
        return err;
    }

    fn evaluate_literal(e: LiteralExpr) Value {
        if (e.value) |val| {
            return switch (val) {
                .String => |str| Value{ .String = str },
                .Number => |num| Value{ .Number = num },
                .True => Value{ .Bool = true },
                .False => Value{ .Bool = false },
                .Nil => Value{ .Nil = undefined },
            };
        }
        unreachable;
    }

    fn evaluate_unary(self: *Self, e: UnaryExpr) RuntimeError!Value {
        const right = try self.evaluate_expr(e.right.*);

        switch (e.operator.token_type) {
            .MINUS => {
                if (!check_op_type(.Number, right)) {
                    try self.raise_err(
                        error.UnaryOperandMismatch,
                        e.operator,
                        "invalid operand for unary op {s} on type {s} at line={d},col={d}",
                        .{ e.operator.lexeme, @tagName(right), e.operator.line_num, e.operator.col },
                    );
                }
                return Value{ .Number = -right.Number };
            },
            .BANG => return Value{ .Bool = !right.is_truthy() },
            else => {
                try self.raise_err(
                    error.IllegalOperator,
                    e.operator,
                    "illegal unary op {s} at line={d},col={d}",
                    .{ e.operator.lexeme, e.operator.line_num, e.operator.col },
                );
                return error.IllegalOperator;
            },
        }
    }

    fn evaluate_binary(self: *Self, e: BinaryExpr) RuntimeError!Value {
        const left = try self.evaluate_expr(e.left.*);
        const right = try self.evaluate_expr(e.right.*);

        if (!check_op_types(.Number, left, right))
            try self.raise_err(
                error.BinaryOperandMismatch,
                e.operator,
                "invalid operands for binary op {s} on types {s} and {s} at line={d},col={d}",
                .{
                    e.operator.lexeme,
                    @tagName(left),
                    @tagName(right),
                    e.operator.line_num,
                    e.operator.col,
                },
            );

        return switch (e.operator.token_type) {
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
            else => {
                try self.raise_err(
                    error.IllegalOperator,
                    e.operator,
                    "illegal binary op {s} at line={d},col={d}",
                    .{ e.operator.lexeme, e.operator.line_num, e.operator.col },
                );
                return error.IllegalOperator;
            },
        };
    }

    fn evaluate_group(self: *Self, e: GroupExpr) RuntimeError!Value {
        return try self.evaluate_expr(e.expression.*);
    }

    fn eval_var_expr(self: *Self, e: VariableExpr) RuntimeError!Value {
        const val = self.environment.get(e.name.lexeme) catch |err| {
            switch (err) {
                error.NotDeclaredAndUndefined => try self.raise_err(
                    error.UndefinedVar,
                    e.name,
                    "use of undefined var '{s}' at line={d},col={d}. {s} was never declared.",
                    .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
                ),
                error.DeclaredButUndefined => try self.raise_err(
                    error.UndefinedVar,
                    e.name,
                    "use of undefined var '{s}' at line={d},col={d}. But {s} was previously declared.",
                    .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
                ),
            }
            return error.UndefinedVar;
        };
        return val;
    }

    fn eval_asgn_expr(self: *Self, e: AssignExpr) RuntimeError!Value {
        // TODO: write tests
        const value = try self.evaluate_expr(e.value.*);
        self.environment.assign(e.name, value) catch
            try self.raise_err(
            error.UndefinedVar,
            e.name,
            "use of undefined var '{s}' at line={d},col={d}. {s} was never declared.",
            .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
        );
        return value;
    }

    pub fn evaluate_expr(self: *Self, e: Expr) RuntimeError!Value {
        return switch (e) {
            .Binary => |bin| try self.evaluate_binary(bin),
            .Unary => |un| try self.evaluate_unary(un),
            .Group => |grp| try self.evaluate_group(grp),
            .Literal => |lit| evaluate_literal(lit),
            .Variable => |v| try self.eval_var_expr(v),
            .Assign => |a| try self.eval_asgn_expr(a),
        };
    }

    fn execute_blk(self: *Self, stmts: std.ArrayList(Stmt), envr: Environment) RuntimeError!void {
        // TODO: write tests for block
        const prev = self.environment;

        self.environment = envr;
        for (stmts) |stmt| {
            try self.evaluate_stmt(stmt);
        }

        self.environment = prev;
    }

    // Evaluating Statements
    fn eval_print_stmt(self: *Self, e: PrintStmt) RuntimeError!void {
        const val = try self.evaluate_expr(e);
        std.debug.print("{}\n", .{val});
    }
    fn eval_expr_stmt(self: *Self, e: ExprStmt) RuntimeError!Value {
        return try self.evaluate_expr(e);
    }
    fn eval_var_stmt(self: *Self, e: VarStmt) RuntimeError!void {
        var val: ?Value = null;
        if (e.initializer) |in| {
            val = try self.evaluate_expr(in);
        }
        try self.environment.define(e.name.lexeme, val);
    }
    fn eval_block_stmt(self: *Self, stmt: BlockStmt) RuntimeError!void {
        self.execute_blk(stmt, Environment.with_enclosing(self.allocator, self.environment));
    }

    fn evaluate_stmt(self: *Self, stmt: Stmt) RuntimeError!void {
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

    const val = try interpreter.eval_var_expr(VariableExpr{ .name = var_stmt.VarStmt.name });

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
