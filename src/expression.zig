const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const Literal = scanner.Literal;
const Token = scanner.Token;

pub const BinaryExpr = struct { left: *Expr, operator: Token, right: *Expr };
pub const UnaryExpr = struct { operator: Token, right: *Expr };
pub const GroupExpr = struct { expression: *Expr };
pub const LiteralExpr = struct { value: Literal };

pub const Expr = union(enum) {
    const Self = @This();
    binary: BinaryExpr,
    unary: UnaryExpr,
    group: GroupExpr,
    literal: LiteralExpr,

    fn parenthesize(allocator: std.mem.Allocator, name: []const u8, exprs: []const Expr) ![]const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        try buf.append('(');
        try buf.appendSlice(name);
        for (exprs) |expr| {
            try buf.append(' ');
            const result = switch (expr) {
                .binary => |b_expr| try parenthesize(allocator, b_expr.operator.lexeme, &[_]Expr{ b_expr.left.*, b_expr.right.* }),
                .unary => |u_expr| try parenthesize(allocator, u_expr.operator.lexeme, &[_]Expr{u_expr.right.*}),
                .group => |grp| try parenthesize(allocator, "group", &[_]Expr{grp.expression.*}),
                .literal => |lit| try lit.value.to_string(),
            };
            try buf.appendSlice(result);
        }
        try buf.append(')');
        return buf.items;
    }

    fn to_string(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        const result = switch (self) {
            .binary => |b_expr| try parenthesize(allocator, b_expr.operator.lexeme, &[_]Expr{ b_expr.left.*, b_expr.right.* }),
            .unary => |u_expr| try parenthesize(allocator, u_expr.operator.lexeme, &[_]Expr{u_expr.right.*}),
            .group => |grp| try parenthesize(allocator, "group", &[_]Expr{grp.expression.*}),
            .literal => |lit| try lit.value.to_string(),
        };
        try buf.appendSlice(result);
        return buf.items;
    }
};

test "Expression Visitor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var inner = Expr{ .literal = .{ .value = .{ .Number = 100 } } };
    var outer = Expr{ .unary = .{ .operator = Token.init(scanner.TokenType.MINUS, "-", null, 1), .right = &inner } };

    var left = Expr{ .literal = .{ .value = .{ .Number = 10 } } };
    var right = Expr{ .group = .{ .expression = &outer } };
    const expr: Expr = .{ .binary = .{
        .left = &left,
        .operator = Token.init(scanner.TokenType.SLASH, "/", null, 1),
        .right = &right,
    } };
    try std.testing.expectFmt("(/ 10 (group (- 100)))", "{s}", .{try expr.to_string(allocator)});
}
