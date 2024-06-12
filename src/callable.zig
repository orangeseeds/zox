const std = @import("std");
const Stmt = @import("expression.zig").Stmt;
const Value = @import("interpreter.zig").Value;
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeErr = @import("interpreter.zig").RuntimeError;
const ArrayList = @import("std").ArrayList;
const Expr = @import("expression.zig").Expr;
const Environment = @import("environment.zig").Environment;
const Error = error{} || RuntimeErr;
const FuncStmt = @import("expression.zig").FuncStmt;
const Token = @import("scanner.zig").Token;

pub const CallableType = enum {
    NativeFunction,
    DefinedFunction,
};
pub const Callable = union(CallableType) {
    const Self = @This();
    NativeFunction: NativeFunction,
    DefinedFunction: DefinedFunction,

    pub fn arity(self: Self, interpreter: *Interpreter) usize {
        return switch (self) {
            .NativeFunction => |n| n.arity(interpreter),
            .DefinedFunction => |d| d.arity(interpreter),
        };
    }

    pub fn call(self: Self, interpreter: *Interpreter, args: []Value) Error!Value {
        return switch (self) {
            .NativeFunction => |n| n.call(interpreter, args),
            .DefinedFunction => |d| d.call(interpreter, args),
        };
    }
};

pub const NativeFunction = struct {
    const Self = @This();
    name: []const u8,
    arity_val: usize,
    callable: *const fn (self: NativeFunction, interpreter: *Interpreter, values: []Value) Error!Value,

    fn arity(self: Self, interpreter: *Interpreter) usize {
        _ = interpreter; // autofix
        return self.arity_val;
    }

    fn call(self: Self, interpreter: *Interpreter, value: []Value) Error!Value {
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

    fn arity(self: Self, _: *Interpreter) usize {
        return self.params.items.len;
    }

    fn call(self: Self, interpreter: *Interpreter, args: []Value) Error!Value {
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

pub fn def_clock() Callable {
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
    return Callable{ .NativeFunction = native };
}
