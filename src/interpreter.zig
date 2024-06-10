const std = @import("std");
const expr = @import("expression.zig");
const parser = @import("parser.zig");
const env = @import("environment.zig");
const scanner = @import("scanner.zig");
const callable = @import("callable.zig");
const Callable = callable.Callable;

const Parser = parser.Parser;
const Scanner = scanner.Scanner;

const Allocator = std.mem.Allocator;

const Expr = expr.Expr;
const LiteralExpr = expr.LiteralExpr;
const UnaryExpr = expr.UnaryExpr;
const BinaryExpr = expr.BinaryExpr;
const GroupExpr = expr.GroupExpr;
const AssignExpr = expr.AssignExpr;
const VariableExpr = expr.VariableExpr;
const LogicalExpr = expr.LogicalExpr;

const Stmt = expr.Stmt;
const ExprStmt = expr.ExprStmt;
const PrintStmt = expr.PrintStmt;
const VarStmt = expr.VarStmt;
const BlockStmt = expr.BlockStmt;
const IfStmt = expr.IfStmt;
const WhileStmt = expr.WhileStmt;
const FuncStmt = expr.FuncStmt;

const Token = scanner.Token;

const Literal = scanner.Literal;
const TokenType = scanner.TokenType;
const Environment = env.Environment;
const EnvError = env.EnvError;

pub const RuntimeError = error{
    BinaryOperandMismatch,
    UnaryOperandMismatch,
    IllegalOperator,
    UndefinedVar,
    InvalidCallAttempt,
    MismatchedArity,
} || std.mem.Allocator.Error;

pub const ValueType = enum {
    Number,
    String,
    Bool,
    Func,
    Nil,
};

pub const Value = union(ValueType) {
    const Self = @This();
    Number: f64,
    String: []const u8,
    Bool: bool,
    Func: Callable,
    Nil,

    pub fn equal(self: Value, other: Value) bool {
        const same_type = switch (self) {
            .Number => @as(ValueType, other) == .Number,
            .String => @as(ValueType, other) == .String,
            .Bool => @as(ValueType, other) == .Bool,
            .Nil => @as(ValueType, other) == .Nil,
            .Func => false,
        };
        if (!same_type) return false;

        return switch (self) {
            .Number => self.Number == other.Number,
            .String => std.mem.eql(u8, self.String, other.String),
            .Bool => self.Bool == other.Bool,
            .Nil => true,
            .Func => false,
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

fn def_clock_native_fun() Callable {
    const clock_native = struct {
        const Self = @This();

        fn arity(_: *anyopaque) usize {
            return 0;
        }

        fn call(_: *anyopaque, _: *Interpreter, _: []Value) RuntimeError!Value {
            return Value{ .Number = @floatFromInt(std.time.nanoTimestamp()) };
        }

        fn callable(self: *Self) Callable {
            return Callable{
                .ptr = self,
                .call_func = call,
                .arity_func = arity,
            };
        }
    };
    var clock = clock_native{};
    return clock.callable();
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
    globals: Environment,
    environment: Environment,

    fn init(allocator: Allocator) Self {
        var globals = Environment.init(allocator);

        const call_func = def_clock_native_fun();
        globals.define("clock", Value{ .Func = call_func }) catch {
            @panic("Error defining native functions.");
        };

        // try globals.define("clock", callable.Callable)
        return Self{
            .allocator = allocator,
            .globals = globals,
            .environment = globals,
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
            .Call => |c| try self.eval_call_expr(c),
            else => unreachable,
        };
    }

    // print "hi" or 2; // "hi".
    // print nil or "yes"; // "yes".
    fn eval_logical_expr(self: *Self, e: LogicalExpr) RuntimeError!Value {
        // TODO: Test logical expression
        const left = try self.evaluate_expr(e.left.*);

        switch (e.operator.token_type) {
            .OR => if (left.is_truthy()) return left,
            else => if (!left.is_truthy()) return left,
        }

        return try self.evaluate_expr(e.right.*);
    }

    pub fn eval_block(self: *Self, stmts: std.ArrayList(Stmt), envr: Environment) RuntimeError!void {
        const prev = self.environment;

        self.environment = envr;
        for (stmts.items) |stmt| {
            try self.evaluate_stmt(stmt);
        }

        self.environment = prev;
    }

    // Evaluating Statements
    fn eval_print_stmt(self: *Self, e: PrintStmt) RuntimeError!void {
        const val = try self.evaluate_expr(e);
        switch (val) {
            .String => |str| std.debug.print("{s}", .{str}),
            .Number => |num| std.debug.print("{d}", .{num}),
            .Bool => |b| std.debug.print("{?}", .{b}),
            .Nil => std.debug.print("nil", .{}),
            .Func => std.debug.print("func", .{}),
        }
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
        const enclosing = try self.allocator.create(Environment);
        enclosing.* = self.environment;
        try self.eval_block(stmt, Environment.with_enclosing(self.allocator, enclosing));
    }

    fn eval_if_stmt(self: *Self, stmt: IfStmt) RuntimeError!void {
        var condition = try self.evaluate_expr(stmt.condition);
        if (condition.is_truthy()) {
            try self.evaluate_stmt(stmt.then_br.*);
            return;
        }
        if (stmt.else_br) |e| {
            try self.evaluate_stmt(e.*);
            return;
        }
    }

    fn eval_while_stmt(self: *Self, stmt: WhileStmt) RuntimeError!void {
        while ((try self.evaluate_expr(stmt.condition)).is_truthy()) {
            try self.evaluate_stmt(stmt.body.*);
        }
    }

    fn eval_call_expr(self: *Self, stmt: expr.CallExpr) RuntimeError!Value {
        // const callee = self.environment.get("clock") catch @panic("err");
        const callee = try self.evaluate_expr(stmt.callee.*);

        var args = std.ArrayList(Value).init(self.allocator);
        for (stmt.args.items) |arg| {
            try args.append(try self.evaluate_expr(arg));
        }

        var func = switch (callee) {
            .Func => |c| c,
            else => {
                try self.raise_err(error.InvalidCallAttempt, stmt.paren, "Can only call functions and classes.", .{});
                return error.InvalidCallAttempt;
            },
        };

        // checking if number of expected parameters = number of passed arguements in a function call
        if (args.items.len != func.arity()) {
            try self.raise_err(error.InvalidCallAttempt, stmt.paren, "Expected {d} arguments but got {d}", .{
                func.arity(),
                args.items.len,
            });
            return error.MismatchedArity;
        }

        // TODO: Implement Callable
        return try func.call(self, args.items);
    }

    fn eval_func_stmt(self: *Self, stmt: FuncStmt) RuntimeError!void {
        var func = try callable.Function.init(stmt, self.allocator);
        try self.environment.define(stmt.name.lexeme, Value{ .Func = func.callable() });
    }

    fn evaluate_stmt(self: *Self, stmt: Stmt) RuntimeError!void {
        switch (stmt) {
            .PrintStmt => |s| try self.eval_print_stmt(s),
            .ExprStmt => |e| _ = try self.eval_expr_stmt(e),
            .VarStmt => |v| try self.eval_var_stmt(v),
            .BlockStmt => |b| try self.eval_block_stmt(b),
            .IfStmt => |i| try self.eval_if_stmt(i),
            .WhileStmt => |w| try self.eval_while_stmt(w),
            .FuncStmt => |f| try self.eval_func_stmt(f),
        }
    }
};

test "Defined Func Stmt" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ var x = 10;
        \\ var y = 20;
        \\ fun apple(x,y) {
        \\   print x+y;
        \\ }
        \\
        \\ apple(x,y);
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    for (stmts.items) |stmt| {
        interpreter.evaluate_stmt(stmt) catch {
            if (interpreter.has_error) {
                std.debug.print("{s}", .{interpreter.runtime_error.message});
            }
        };
    }
    std.debug.print("\n", .{});
}

test "Native Func Stmt" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ print clock();
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    for (stmts.items) |stmt| {
        try interpreter.evaluate_stmt(stmt);
    }
    std.debug.print("\n", .{});
}

test "While Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ for(var i=0;i<2;i=i+1) print(i);
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    for (stmts.items) |stmt| {
        try interpreter.evaluate_stmt(stmt);
    }
    std.debug.print("\n", .{});
}

test "If Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ if (10 != 10){ 10; } else { 20; }
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        std.debug.print("\n{s}\n", .{par.errors.items[0].message});
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    for (stmts.items) |stmt| {
        try interpreter.evaluate_stmt(stmt);
    }
}

test "Block Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ var outer = 10;
        \\ { 
        \\    var value = (20 * 20); 
        \\    outer;
        \\    value;
        \\ }
        \\ outer;
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = try par.parse_stmts(allocator);
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);

    for (stmts.items) |stmt| {
        try interpreter.evaluate_stmt(stmt);
    }
}

test "Declaration & Assignment Expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ var value = (20 * 20);
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = try par.parse_stmts(allocator);

    var interpreter = Interpreter.init(allocator);

    try interpreter.evaluate_stmt(stmts.items[0]);
    var val = try interpreter.environment.get("value");
    try std.testing.expect(val.equal(val_num(400)));

    // try interpreter.evaluate_stmt(stmts.items[1]);
    // val = try interpreter.environment.get("value");
    // try std.testing.expect(val.equal(val_num(400)));
}

test "Expression Statement Evaluations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const table = [_]struct { []const u8, Value }{
        .{ "true;", val_bool(true) },
        .{ "false;", val_bool(false) },
        .{ "!false;", val_bool(true) },
        .{ "!true;", val_bool(false) },
        .{ "1;", val_num(1) },
        .{ "-1;", val_num(-1) },

        .{ "10 > 11;", val_bool(false) },
        .{ "10 < 11;", val_bool(true) },
        .{ "10 >= 10;", val_bool(true) },
        .{ "10 <= 10;", val_bool(true) },
        .{ "10 <= 11;", val_bool(true) },
        .{ "10 <= 9;", val_bool(false) },

        .{ "10 == 9;", val_bool(false) },
        .{ "10 == 10;", val_bool(true) },
        .{ "10 != 10;", val_bool(false) },
        .{ "10 != 9;", val_bool(true) },

        .{ "1+1;", val_num(2) },
        .{ "2*3;", val_num(6) },
        .{ "6/3;", val_num(2) },
        .{ "6-3;", val_num(3) },

        .{ "\"hello world\";", val_str("hello world") },
    };
    for (table) |val| {
        const evaluated = try eval_str(alloc, val[0]);
        // std.debug.print("\n{} {}\n", .{ evaluated, val[1] });
        try std.testing.expect(evaluated.equal(val[1]));
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

/// Only works for expression statements.
fn eval_str(alloc: std.mem.Allocator, str: []const u8) !Value {
    var lex = try Scanner.init(alloc, str);
    try lex.scan_tokens();
    var p = Parser.init(alloc, try lex.tokens.clone());
    var stmts = try p.parse_stmts(alloc);

    var interpreter = Interpreter.init(alloc);
    const val = try alloc.create(Value);
    val.* = try interpreter.eval_expr_stmt(stmts.pop().ExprStmt);
    return val.*;
}
