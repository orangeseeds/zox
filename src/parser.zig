const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const Token = scanner.Token;

const Parser = struct {
    const Self = @This();
    tokens: std.ArrayList(Token),
    current: u32 = 0,

    fn init(tokens: std.ArrayList(Token)) Self {
        return Self{ .tokens = tokens };
    }
};
