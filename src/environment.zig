const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("interpreter.zig").Value;
const Token = @import("scanner.zig").Token;

pub const EnvError = error{
    DeclaredButUndefined,
    NotDeclaredAndUndefined,
};

pub const Environment = struct {
    const Self = @This();
    const EnvMap = std.StringArrayHashMap(?Value);
    values: EnvMap,
    enclosing: *Environment,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .enclosing = undefined,
            .values = EnvMap.init(allocator),
        };
    }
    pub fn with_enclosing(allocator: Allocator, enclosing: *Environment) Self {
        return Self{
            .enclosing = enclosing,
            .values = EnvMap.init(allocator),
        };
    }
    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn get(self: *Self, name: []const u8) EnvError!Value {
        if (self.values.getKey(name)) |key| {
            if (self.values.get(key)) |val| return val orelse
                error.DeclaredButUndefined;
        }

        if (self.enclosing == undefined) return try self.enclosing.get(name);

        return error.NotDeclaredAndUndefined;
    }

    pub fn define(self: *Self, name: []const u8, val: ?Value) !void {
        try self.values.put(name, val);
    }

    pub fn assign(self: *Self, name: Token, val: Value) !void {
        if (self.values.getKey(name.lexeme)) |key| {
            try self.values.put(key, val);
            return;
        }

        if (self.enclosing != undefined) return try self.enclosing.assign(name, val);

        return error.NotDeclaredAndUndefined;
    }
};
