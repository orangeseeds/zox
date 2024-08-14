const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("interpreter.zig").Value;
const Token = @import("scanner.zig").Token;

pub const EnvError = error{
    DeclaredButUndefined,
    NotDeclaredAndUndefined,
    VariableAlreadyDefined,
};

pub const Environment = struct {
    const Self = @This();
    const EnvMap = std.StringArrayHashMap(?Value);
    values: EnvMap,
    /// outer environment
    enclosing: ?*Environment,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .enclosing = null,
            .values = EnvMap.init(allocator),
        };
    }
    pub fn with_enclosing(allocator: Allocator, enclosing: *Environment) Self {
        return Self{
            .enclosing = enclosing,
            .values = EnvMap.init(allocator),
        };
    }
    pub fn get(self: *Self, name: []const u8) EnvError!Value {
        // BUG: no address available when key search err, should return
        // NotDeclaredAndUndefined but returns no address available.
        if (self.values.getKey(name)) |key| {
            if (self.values.get(key)) |val| return val orelse
                error.DeclaredButUndefined;
        }

        if (self.enclosing) |enclosing| {
            return try enclosing.get(name);
        }

        return error.NotDeclaredAndUndefined;
    }

    pub fn get_ptr(self: *Self, name: []const u8) EnvError!*Value {
        if (self.values.getKey(name)) |key| {
            if (self.values.getPtr(key)) |val| return val orelse
                error.DeclaredButUndefined;
        }

        if (self.enclosing) |enclosing| {
            return try enclosing.get(name);
        }

        return error.NotDeclaredAndUndefined;
    }

    pub fn local_exists(self: *Self, name: []const u8) bool {
        if (self.values.getKey(name)) |_| {
            return true;
        }
        return false;
    }

    pub fn define(self: *Self, name: []const u8, val: ?Value) !void {
        try self.values.put(name, val);
    }

    pub fn assign(self: *Self, name: Token, val: Value) !void {
        if (self.values.getKey(name.lexeme)) |key| {
            try self.values.put(key, val);
            return;
        }

        if (self.enclosing) |enclosing| {
            return try enclosing.assign(name, val);
        }

        return error.NotDeclaredAndUndefined;
    }

    pub fn clone(self: *Self) !Environment {
        var enclosing: ?*Environment = null;
        if (self.enclosing) |enc| {
            var resolved = try enc.clone();
            enclosing = &resolved;
        }
        return Environment{
            .values = try self.values.clone(),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }
};
