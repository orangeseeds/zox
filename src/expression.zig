const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const Literal = scanner.Literal;
const Token = scanner.Token;

pub const BinaryExpr = struct { left: *Expr, operator: Token, right: *Expr };
pub const UnaryExpr = struct { operator: Token, right: *Expr };
pub const GroupExpr = struct { expression: *Expr };
pub const LiteralExpr = struct { value: ?Literal };
pub const VariableExpr = struct { name: Token };
pub const AssignExpr = struct { name: Token, value: *Expr };

pub const Expr = union(enum) {
    const Self = @This();
    Binary: BinaryExpr,
    Unary: UnaryExpr,
    Group: GroupExpr,
    Literal: LiteralExpr,
    Variable: VariableExpr,
    Assign: AssignExpr,

    fn parenthesize_recur(self: Self, allocator: std.mem.Allocator, buff: *std.ArrayList(u8), expr: Expr) !void {
        // std.debug.print("Test: {any}\n", .{expr});
        switch (expr) {
            .Literal => |lit| {
                if (lit.value) |value| {
                    // try buff.appendSlice(try value.to_string(allocator));
                    switch (value) {
                        .Number => |val| try buff.writer().print("{d}", .{val}),
                        .String => |val| try buff.writer().print("\"{s}\"", .{val}),
                        .True => |val| try buff.writer().print("{?}", .{val}),
                        .False => |val| try buff.writer().print("{?}", .{val}),
                        .Nil => try buff.appendSlice("nil"),
                    }
                }
                return;
            },
            .Binary => |bin| {
                try buff.writer().print("({s} ", .{bin.operator.lexeme});
                try self.parenthesize_recur(allocator, buff, bin.left.*);
                try buff.append(' ');
                try self.parenthesize_recur(allocator, buff, bin.right.*);
                try buff.append(')');
            },
            .Unary => |un| {
                try buff.writer().print("({s} ", .{un.operator.lexeme});
                try self.parenthesize_recur(allocator, buff, un.right.*);
                try buff.append(')');
            },
            .Group => |grp| {
                try buff.writer().print("(group ", .{});
                try self.parenthesize_recur(allocator, buff, grp.expression.*);
                try buff.append(')');
            },
            .Variable => |exp| {
                try buff.writer().print("(var {s} ", .{exp.name.lexeme});
                // try self.parenthesize_recur(allocator, buff, exp.expression);
                try buff.append(')');
            },
            .Assign => |asgn| {
                try buff.writer().print("({s} = ", .{asgn.name.lexeme});
                try self.parenthesize_recur(allocator, buff, asgn.value.*);
                try buff.append(')');
            },
        }
    }

    pub fn to_string(self: Self, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        var buff = std.ArrayList(u8).init(allocator);
        try self.parenthesize_recur(allocator, &buff, self);
        return buff;
    }
};

pub const PrintStmt = Expr;
pub const ExprStmt = Expr;
pub const VarStmt = struct { name: Token, initializer: ?Expr };
pub const BlockStmt = std.ArrayList(Stmt);
pub const Stmt = union(enum) {
    PrintStmt: Expr,
    ExprStmt: Expr,
    VarStmt: VarStmt,
    BlockStmt: BlockStmt,

    fn parenthesize_recur(self: Stmt, allocator: std.mem.Allocator, buff: *std.ArrayList(u8), stmt: Stmt) !void {
        // std.debug.print("Test: {any}\n", .{expr});
        switch (stmt) {
            .PrintStmt => |print| {
                const str = try print.to_string(allocator);
                try buff.writer().print("print {s};", .{str.items});
            },
            .ExprStmt => |expr| {
                const str = try expr.to_string(allocator);
                try buff.writer().print("{s};", .{str.items});
            },
            .VarStmt => |var_st| {
                const str = try var_st.initializer.?.to_string(allocator);
                try buff.writer().print("var {s} = {s};", .{ var_st.name.lexeme, str.items });
            },
            .BlockStmt => |blk| {
                try buff.append('{');
                for (blk.items) |value| {
                    try self.parenthesize_recur(allocator, buff, value);
                }
                try buff.append('}');
            },
        }
    }
    pub fn to_string(self: Stmt, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        var buff = std.ArrayList(u8).init(allocator);
        try self.parenthesize_recur(allocator, &buff, self);
        return buff;
    }
};

test "Expression Visitor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    {
        var inner = Expr{ .Literal = .{ .value = .{ .Number = 100 } } };
        var outer = Expr{ .Unary = .{ .operator = Token.init(scanner.TokenType.MINUS, "-", null, 1, 1), .right = &inner } };

        var left = Expr{ .Literal = .{ .value = .{ .Number = 10 } } };
        var right = Expr{ .Group = .{ .expression = &outer } };
        const expr: Expr = .{ .Binary = .{
            .left = &left,
            .operator = Token.init(scanner.TokenType.SLASH, "/", null, 1, 1),
            .right = &right,
        } };

        const got = try expr.to_string(allocator);
        defer got.deinit();
        try std.testing.expectFmt("(/ 10 (group (- 100)))", "{s}", .{got.items});
    }
}
