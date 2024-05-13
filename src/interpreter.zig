const std = @import("std");
const expr = @import("expression.zig");
const Expr = expr.Expr;
const Stmt = expr.Stmt;
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const Literal = scanner.Literal;
const TokenType = @import("scanner.zig").TokenType;

const Value = union(enum) {
    const Self = @This();
    Number: f64,
    String: []const u8,
    Bool: bool,
    Nil: void,

    fn equals(self: Self, comp: Value) bool {
        if (@TypeOf(self) != @TypeOf(comp)) return false;

        return switch (self) {
            .Number => self.Number == comp.Number,
            .String => std.mem.eql(u8, self.String, comp.String),
            .Bool => self.Bool == comp.Bool,
            .Nil => true,
        };
    }
};

pub const Interpreter = struct {
    fn is_truthy(value: Value) bool {
        return switch (value) {
            .Bool => |val| val,
            .Nil => false,
            else => true,
        };
    }

    fn is_equal(left: Value, right: Value) bool {
        if (@TypeOf(left) == @TypeOf(left.Nil) and @TypeOf(right) == @TypeOf(left.Nil)) return true;
        if (@TypeOf(left) == @TypeOf(left.Nil)) return false;

        return left.equals(right);
    }

    fn checkNumberOperand() void {} // TODO: add runtime typecheck
    fn checkNumberOperands() void {}

    fn evaluate_literal(expression: expr.LiteralExpr) Value {
        if (expression.value) |val| {
            return switch (val) {
                .String => Value{ .String = val.String },
                .Number => Value{ .Number = val.Number },
                .True => Value{ .Bool = true },
                .False => Value{ .Bool = false },
                .Nil => Value{ .Nil = undefined },
            };
        } else {
            return Value{ .Nil = undefined };
        }
    }

    fn evaluate_unary(expression: expr.UnaryExpr) Value {
        const right = evaluate(expression.right.*);

        return switch (expression.operator.token_type) {
            .MINUS => Value{ .Number = -right.Number },
            .BANG => Value{ .Bool = !is_truthy(right) },
            else => Value{ .Nil = undefined },
        };
    }

    fn evaluate_binary(expression: expr.BinaryExpr) Value {
        const left = evaluate(expression.left.*);
        const right = evaluate(expression.right.*);

        return switch (expression.operator.token_type) {
            .MINUS => Value{ .Number = left.Number - right.Number },
            .SLASH => Value{ .Number = left.Number / right.Number },
            .STAR => Value{ .Number = left.Number * right.Number },
            .PLUS => {
                return Value{ .Number = left.Number + right.Number };
                // if (@TypeOf(left.Number) == @TypeOf(Value.Number) and @TypeOf(right.Number) == @TypeOf(Value.Number)) {
                //     return Value{ .Number = left.Number + right.Number };
                // } else if (@TypeOf(left.String) == @TypeOf(Value.String) and @TypeOf(right.String) == @TypeOf(Value.Number)) {
                //     return Value{ .String = left.String + right.String };
                // } else {
                //     return Value{ .Nil = undefined };
                // }
            },

            .GREATER => Value{ .Bool = left.Number > right.Number },
            .GREATER_EQUAL => Value{ .Bool = left.Number >= right.Number },
            .LESS => Value{ .Bool = left.Number < right.Number },
            .LESS_EQUAL => Value{ .Bool = left.Number <= right.Number },
            .BANG_EQUAL => Value{ .Bool = !is_equal(left, right) },
            .EQUAL_EQUAL => Value{ .Bool = is_equal(left, right) },
            else => Value{ .Nil = undefined },
        };
    }

    fn evaluate_group(expression: expr.GroupExpr) Value {
        return evaluate(expression.expression.*);
    }

    // Evaluating expressions
    fn eval_expr_stmt(expression: Expr) void {
        _ = evaluate(expression);
        // TODO: Maybe you need to do something else;
    }
    fn eval_print_stmt(expression: Expr) void {
        const val = evaluate(expression);
        switch (val) {
            .Number => |num| std.debug.print("{d}", .{num}),
            .String => |str| std.debug.print("{s}", .{str}),
            .Bool => |b| std.debug.print("{}", .{b}),
            .Nil => std.debug.print("nil", .{}),
        }
    }
    pub fn evaluate(expression: expr.Expr) Value {
        return switch (expression) {
            .Binary => evaluate_binary(expression.Binary),
            .Unary => evaluate_unary(expression.Unary),
            .Group => evaluate_group(expression.Group),
            .Literal => evaluate_literal(expression.Literal),
        };
    }

    fn execute(stmt: expr.Stmt) !void {
        switch (stmt) {
            .Print => |p| eval_print_stmt(p.expression),
            .Expr => |e| eval_expr_stmt(e.expression),
            // else => std.debug.print("{s}", .{"runtime error"}),
        }
    }

    pub fn interpret(statements: []Stmt) !void {
        for (statements) |value| try execute(value);
    }
};

test "Testing Expression Evaluation" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = 130;
    const input =
        \\ (((10 + (10 * (100 / 10))) + 10) + 10) ;
    ;

    var lexer = try scanner.Scanner.init(allocator, input);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, lexer.tokens);
    const exp = try par.parse();

    const val = Interpreter.evaluate(exp);
    switch (val) {
        .Number => |num| try std.testing.expect(expected == num),
        else => unreachable,
    }
}

test "Testing Print Expression\n" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ print("apple");
    ;

    var lexer = try scanner.Scanner.init(allocator, input);
    try lexer.scan_tokens();
    var par = parser.Parser.init(allocator, lexer.tokens);
    const stmts = try par.parse_stmts(allocator);
    try Interpreter.interpret(stmts.items);
    std.debug.print("\n", .{});
}
