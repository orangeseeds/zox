const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const Literal = scanner.Literal;
const Token = scanner.Token;

pub const BinaryExpr = struct { left: *Expr, operator: Token, right: *Expr };
pub const UnaryExpr = struct { operator: Token, right: *Expr };
pub const GroupExpr = struct { expression: *Expr };
pub const LiteralExpr = struct { value: ?Literal };

pub const Expr = union(enum) {
    const Self = @This();
    Binary: BinaryExpr,
    Unary: UnaryExpr,
    Group: GroupExpr,
    Literal: LiteralExpr,

    fn parenthesize(allocator: std.mem.Allocator, name: []const u8, exprs: []const Expr) ![]const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        try buf.append('(');
        try buf.appendSlice(name);
        for (exprs) |expr| {
            try buf.append(' ');
            const result = switch (expr) {
                .Binary => |b_expr| try parenthesize(allocator, b_expr.operator.lexeme, &[_]Expr{ b_expr.left.*, b_expr.right.* }),
                .Unary => |u_expr| try parenthesize(allocator, u_expr.operator.lexeme, &[_]Expr{u_expr.right.*}),
                .Group => |grp| try parenthesize(allocator, "group", &[_]Expr{grp.expression.*}),
                .Literal => |lit| if (lit.value) |val| try val.to_string(allocator) else "literal_none",
            };
            try buf.appendSlice(result);
        }
        try buf.append(')');
        return buf.items;
    }

    pub fn to_string(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        const result = switch (self) {
            .Binary => |b_expr| try parenthesize(allocator, b_expr.operator.lexeme, &[_]Expr{ b_expr.left.*, b_expr.right.* }),
            .Unary => |u_expr| try parenthesize(allocator, u_expr.operator.lexeme, &[_]Expr{u_expr.right.*}),
            .Group => |grp| try parenthesize(allocator, "group", &[_]Expr{grp.expression.*}),
            .Literal => |lit| if (lit.value) |val| try val.to_string(allocator) else "literal_none",
        };
        try buf.appendSlice(result);
        return buf.items;
    }
};

pub const ExprStmt = struct { expression: Expr };
pub const PrintStmt = struct { expression: Expr };
pub const Stmt = union(enum) {
    Expr: ExprStmt,
    Print: PrintStmt,
};

test "Expression Visitor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var inner = Expr{ .Literal = .{ .value = .{ .Number = 100 } } };
    var outer = Expr{ .Unary = .{ .operator = Token.init(scanner.TokenType.MINUS, "-", null, 1), .right = &inner } };

    var left = Expr{ .Literal = .{ .value = .{ .Number = 10 } } };
    var right = Expr{ .Group = .{ .expression = &outer } };
    const expr: Expr = .{ .Binary = .{
        .left = &left,
        .operator = Token.init(scanner.TokenType.SLASH, "/", null, 1),
        .right = &right,
    } };

    try std.testing.expectFmt("(/ 10 (group (- 100)))", "{s}", .{try expr.to_string(allocator)});
}
