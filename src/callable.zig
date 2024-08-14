const std = @import("std");
const Stmt = @import("expression.zig").Stmt;
const Value = @import("interpreter.zig").Value;
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeErr = @import("interpreter.zig").RuntimeError;
const ArrayList = @import("std").ArrayList;
const Expr = @import("expression.zig").Expr;
const Environment = @import("environment.zig").Environment;
const Error = error{UndefinedProperty} || RuntimeErr;
const FuncStmt = @import("expression.zig").FuncStmt;
const Token = @import("scanner.zig").Token;

pub const CallableType = enum {
    NativeFunction,
    DefinedFunction,
    Class,
    ClassInstance,
};
pub const Callable = union(CallableType) {
    const Self = @This();
    NativeFunction: NativeFunction,
    DefinedFunction: DefinedFunction,
    Class: Class,
    ClassInstance: ClassInstance,

    pub fn arity(self: Self, interpreter: *Interpreter) usize {
        return switch (self) {
            .NativeFunction => |n| n.arity(interpreter),
            .DefinedFunction => |d| d.arity(interpreter),
            .Class => |c| c.arity(interpreter),
            .ClassInstance => |i| i.arity(interpreter),
        };
    }

    pub fn call(self: Self, interpreter: *Interpreter, args: []Value) Error!Value {
        return switch (self) {
            .NativeFunction => |n| n.call(interpreter, args),
            .DefinedFunction => |d| d.call(interpreter, args),
            .Class => |c| c.call(interpreter, args),
            .ClassInstance => |i| i.call(interpreter, args),
        };
    }

    pub fn name(self: Self) []const u8 {
        return switch (self) {
            .NativeFunction => |n| n.name,
            .DefinedFunction => |f| f.name,
            .Class => |c| c.name,
            .ClassInstance => |i| i.class.name,
        };
    }
};

pub const NativeFunction = struct {
    const Self = @This();
    name: []const u8,
    arity_val: usize,
    callable: *const fn (self: NativeFunction, interpreter: *Interpreter, values: []Value) Error!Value,

    pub fn arity(self: Self, interpreter: *Interpreter) usize {
        _ = interpreter; // autofix
        return self.arity_val;
    }

    pub fn call(self: Self, interpreter: *Interpreter, value: []Value) Error!Value {
        return self.callable(self, interpreter, value);
    }
};

pub const DefinedFunction = struct {
    const Self = @This();
    id: u64,
    name: []const u8,
    params: std.ArrayList(Token),
    body: std.ArrayList(Stmt),
    closure: *Environment,
    instance_id: ?u64 = null,
    is_init: bool = false,

    pub fn arity(self: Self, _: *Interpreter) usize {
        return self.params.items.len;
    }

    pub fn bind(self: Self, allocator: std.mem.Allocator, instance: ClassInstance) Error!DefinedFunction {
        var env = Environment.with_enclosing(allocator, self.closure);
        try env.define("this", Value{ .ClassInstance = instance });
        return DefinedFunction{
            .id = self.id,
            .name = self.name,
            .params = self.params,
            .body = self.body,
            .closure = &env,
            .instance_id = instance.id,
        };
    }

    pub fn call(self: Self, interpreter: *Interpreter, args: []Value) Error!Value {
        var env = Environment.with_enclosing(interpreter.allocator, self.closure);
        for (self.params.items, 0..) |val, i| {
            try env.define(val.lexeme, args[i]);
        }

        const prev = self.closure.*;

        try interpreter.callstack.append(Interpreter.CallStackEntry{
            .id = self.id,
            .name = self.name,
        });

        interpreter.environment = env;
        for (self.body.items) |stmt| {
            if (interpreter.return_val) |_| break;
            try interpreter.execute_stmt(stmt);
        }

        _ = interpreter.callstack.pop();

        interpreter.environment = prev;

        if (interpreter.return_val) |val| {
            return val;
        }

        return Value{ .Nil = undefined };
    }
};

pub const ClassInstance = struct {
    const Self = @This();
    id: u64,
    class: Class,
    fields: std.StringArrayHashMap(Value),

    pub fn init(id: u64, class: Class, allocator: std.mem.Allocator) Self {
        return Self{
            .id = id,
            .class = class,
            .fields = std.StringArrayHashMap(Value).init(allocator),
        };
    }

    pub fn arity(_: Self, _: *Interpreter) usize {
        return 0;
    }

    pub fn call(_: Self, _: *Interpreter, _: []Value) Error!Value {
        return Value{ .Nil = undefined };
    }

    pub fn get(self: Self, allocator: std.mem.Allocator, name: []const u8) Error!Value {
        if (self.fields.get(name)) |val| {
            return val;
        }

        if (self.class.find_method(name)) |method| {
            const func = switch (method) {
                .DefFunc => |f| try f.bind(allocator, self),
                else => unreachable,
            };
            return Value{ .DefFunc = func };
        }
        return error.UndefinedProperty;
    }

    pub fn set(self: *Self, name: []const u8, value: Value) Error!void {
        try self.fields.put(name, value);
    }
};

pub const Class = struct {
    const Self = @This();
    id: u64,
    name: []const u8,
    methods: std.StringArrayHashMap(DefinedFunction),

    pub fn init(id: u64, name: []const u8, allocator: std.mem.Allocator) Self {
        return Self{
            .id = id,
            .name = name,
            .methods = std.StringArrayHashMap(DefinedFunction).init(allocator),
        };
    }
    pub fn arity(self: Self, interpreter: *Interpreter) usize {
        if (self.find_method("init")) |initializer| {
            switch (initializer) {
                .DefFunc => |f| {
                    return f.arity(interpreter);
                },
                else => unreachable,
            }
        }
        return 0;
    }

    pub fn find_method(self: Self, name: []const u8) ?Value {
        if (self.methods.get(name)) |method| {
            return Value{ .DefFunc = method };
        } else {
            return null;
        }
    }

    pub fn call(self: Self, instance_id: u64, interpreter: *Interpreter, args: []Value) Error!Value {
        const instance = ClassInstance.init(instance_id, self, interpreter.allocator);

        if (self.find_method("init")) |initializer| {
            switch (initializer) {
                .DefFunc => |f| {
                    var binded = try f.bind(interpreter.allocator, instance);
                    _ = try binded.call(interpreter, args);
                },
                else => unreachable,
            }
        }

        // try instance.set("native_prop", Value{ .String = "test" });
        return Value{ .ClassInstance = instance };
    }
};

pub fn def_clock() NativeFunction {
    const clock_native = struct {
        fn call(_: NativeFunction, _: *Interpreter, _: []Value) Error!Value {
            return Value{ .Number = @floatFromInt(std.time.nanoTimestamp()) };
        }
    };

    // const clock = clock_native{};
    const native = NativeFunction{
        .arity_val = 0,
        .name = "clock",
        .callable = clock_native.call,
    };
    return native;
}

test "Class and Attributes" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var int = Interpreter.init(allocator);
    const args = [_]Value{};

    var class = Class.init(0, "test", allocator);
    const callable = try class.call(1, &int, &args);

    var instance = callable.ClassInstance;
    try instance.set("test_prop", Value{ .String = "Value" });
    const val = try instance.get(allocator, "test_prop");

    try std.testing.expectEqualStrings("Value", val.String);
}
