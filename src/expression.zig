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
pub const GetExpr = struct { object: *Expr, name: Token };

// TODO: make FuncExpr work like var expr
pub const FuncExpr = struct { name: Token };
pub const AssignExpr = struct { name: Token, value: *Expr };
pub const LogicalExpr = struct { left: *Expr, operator: Token, right: *Expr };
pub const CallExpr = struct { callee: *Expr, paren: Token, args: std.ArrayList(Expr) };
pub const BlockExpr = std.ArrayList(Expr);
pub const SetExpr = struct { object: *Expr, name: Token, val: *Expr };
pub const ThisExpr = struct { keyword: Token };

pub const Expr = union(enum) {
    const Self = @This();
    Binary: BinaryExpr,
    Unary: UnaryExpr,
    Group: GroupExpr,
    Literal: LiteralExpr,
    Variable: VariableExpr,
    Assign: AssignExpr,
    Logical: LogicalExpr,
    Call: CallExpr,
    Block: BlockExpr,
    GetExpr: GetExpr,
    SetExpr: SetExpr,
    ThisExpr: ThisExpr,

    fn parenthesize_recur(self: Self, allocator: std.mem.Allocator, buff: *std.ArrayList(u8), expr: Expr) !void {
        // std.debug.print("Test: {any}\n", .{expr});
        switch (expr) {
            .Literal => |lit| {
                if (lit.value) |value| {
                    // try buff.appendSlice(try value.to_string(allocator));
                    switch (value) {
                        .Number => |val| try buff.writer().print("{d}", .{val}),
                        .String => |val| try buff.writer().print("\"{s}\"", .{val}),
                        .True => try buff.writer().print("true", .{}),
                        .False => try buff.writer().print("false", .{}),
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
            .Logical => |log| {
                try buff.writer().print("({s} ", .{log.operator.lexeme});
                try self.parenthesize_recur(allocator, buff, log.left.*);
                try buff.append(' ');
                try self.parenthesize_recur(allocator, buff, log.right.*);
                try buff.append(')');
            },
            .Block => |b| {
                try buff.append('{');
                for (b.items) |value|
                    try self.parenthesize_recur(allocator, buff, value);
                try buff.append('}');
            },
            .Call => |f| {
                try buff.writer().print("( call {s}", .{(try f.callee.to_string(allocator)).items});
                try buff.append('(');
                for (f.args.items, 0..f.args.items.len) |e, idx| {
                    try self.parenthesize_recur(allocator, buff, e);
                    if (idx < f.args.items.len - 1) try buff.writer().print(", ", .{});
                }
                try buff.append(')');
                try buff.append(')');
            },
            .GetExpr => |g| {
                // std.debug.print("{any}", .{g.object.Call.callee.GetExpr.object});
                try self.parenthesize_recur(allocator, buff, g.object.*);
                try buff.writer().print(".(getter {s})", .{g.name.lexeme});
            },
            .SetExpr => |s| {
                try self.parenthesize_recur(allocator, buff, s.object.*);
                try buff.writer().print(".(setter {s})", .{s.name.lexeme});
                try buff.writer().print("=", .{});
                try self.parenthesize_recur(allocator, buff, s.val.*);
            },
            .ThisExpr => {
                try buff.writer().print(" this ", .{});
            },
        }
    }

    pub fn to_string(self: Self, allocator: std.mem.Allocator) std.mem.Allocator.Error!std.ArrayList(u8) {
        var buff = std.ArrayList(u8).init(allocator);
        try self.parenthesize_recur(allocator, &buff, self);
        return buff;
    }
};

const Error = std.mem.Allocator.Error;

pub const PrintStmt = Expr;
pub const ExprStmt = Expr;
pub const VarStmt = struct { name: Token, initializer: ?Expr };
pub const BlockStmt = std.ArrayList(Stmt);
pub const IfStmt = struct { condition: Expr, then_br: *Stmt, else_br: ?*Stmt };
pub const WhileStmt = struct { condition: Expr, body: *Stmt };
pub const FuncStmt = struct { name: Token, params: std.ArrayList(Token), body: std.ArrayList(Stmt) };
pub const ReturnStmt = struct { keyword: Token, expression: ?Expr };
pub const ClassStmt = struct { name: Token, methods: std.ArrayList(Stmt) };
pub const Stmt = union(enum) {
    PrintStmt: Expr,
    ExprStmt: Expr,
    VarStmt: VarStmt,
    BlockStmt: BlockStmt,
    IfStmt: IfStmt,
    WhileStmt: WhileStmt,
    FuncStmt: FuncStmt,
    ReturnStmt: ReturnStmt,
    ClassStmt: ClassStmt,

    fn parenthesize_recur(self: Stmt, allocator: std.mem.Allocator, buff: *std.ArrayList(u8), stmt: Stmt) Error!void {
        // std.debug.print("Test: {any}\n", .{expr});
        switch (stmt) {
            .PrintStmt => |print| {
                const str = try print.to_string(allocator);
                try buff.writer().print("print ( {s} );", .{str.items});
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
                for (blk.items) |value|
                    try self.parenthesize_recur(allocator, buff, value);
                try buff.append('}');
            },
            .IfStmt => |i| {
                const condition = try i.condition.to_string(allocator);
                try buff.writer().print("if ({s}) ", .{condition.items});

                const then = try i.then_br.to_string(allocator);
                try buff.writer().print("{s} {s} {s}", .{ "{", then.items, "}" });
                if (i.else_br) |el| {
                    const el_str = try el.to_string(allocator);
                    try buff.writer().print(" else {s} {s} {s}", .{ "{", el_str.items, "}" });
                }
            },
            .WhileStmt => |w| {
                const condition = try w.condition.to_string(allocator);
                try buff.writer().print("while ({s}) ", .{condition.items});

                const body = try w.body.to_string(allocator);
                try buff.writer().print("{s} {s} {s}", .{ "{", body.items, "}" });
            },
            .FuncStmt => |fnc| {
                try buff.writer().print("func {s}( ", .{fnc.name.lexeme});
                for (fnc.params.items, 0..) |value, i| {
                    _ = try buff.writer().write(value.lexeme);

                    if (i < fnc.params.items.len - 1) {
                        _ = try buff.writer().write(", ");
                    }
                }
                _ = try buff.writer().write(" )");

                try self.parenthesize_recur(allocator, buff, Stmt{ .BlockStmt = fnc.body });
            },
            .ReturnStmt => |r| {
                if (r.expression) |e| {
                    const str = try e.to_string(allocator);
                    try buff.writer().print("return {s};", .{str.items});
                } else {
                    _ = try buff.writer().write("return;");
                }
            },
            .ClassStmt => unreachable,
        }
    }
    pub fn to_string(self: Stmt, allocator: std.mem.Allocator) Error!std.ArrayList(u8) {
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
