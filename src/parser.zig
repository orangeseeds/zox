const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");

const BinaryExpr = struct { left: Expr, operator: scanner.Token, right: Expr };
const UnaryExpr = struct { operator: scanner.Token, right: Expr };
const GroupExpr = struct { expression: Expr };
const LiteralExpr = struct { value: scanner.Literal };

const Expr = union(enum) {
    binary: BinaryExpr,
    unary: UnaryExpr,
    group: GroupExpr,
    literal: LiteralExpr,
};

const Parser = struct {
    const Self = @This();
    tokens: std.ArrayList(scanner.Token),
    current: u32 = 0,

    fn init(tokens: std.ArrayList(scanner.Token)) Self {
        return Self{ .tokens = tokens };
    }
};
