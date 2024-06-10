const std = @import("std");
const Value = @import("interpreter.zig").Value;
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeErr = @import("interpreter.zig").RuntimeError;
const ArrayList = @import("std").ArrayList;
const Expr = @import("expression.zig").Expr;
const Environment = @import("environment.zig").Environment;
const Error = error{} || RuntimeErr;
const FuncStmt = @import("expression.zig")
    .FuncStmt;
pub const Callable = struct {
    ptr: *anyopaque,
    call_func: *const fn (ptr: *anyopaque, interpreter: *Interpreter, values: []Value) Error!Value,
    arity_func: *const fn (ptr: *anyopaque) usize,

    pub fn call(self: Callable, interpreter: *Interpreter, args: []Value) Error!Value {
        return try self.call_func(self.ptr, interpreter, args);
    }

    pub fn arity(self: Callable) usize {
        return self.arity_func(self.ptr);
    }
};

pub const Function = struct {
    const Self = @This();
    declaration: FuncStmt,

    pub fn init(declaration: FuncStmt, alloc: std.mem.Allocator) !Function {
        const decl = try std.mem.Allocator.create(alloc, FuncStmt);
        decl.* = declaration;
        return Function{
            .declaration = decl.*,
        };
    }

    // TODO: implement this thing using NativeFunc and Func struct;
    pub fn call(self: *anyopaque, interpreter: *Interpreter, args: []Value) Error!Value {
        var env = Environment.with_enclosing(interpreter.allocator, &interpreter.environment);
        const func: *Function = @ptrCast(@alignCast(self));
        for (func.declaration.params.items, 0..) |value, i| {
            try env.define(value.lexeme, args[i]);
        }

        // TODO: instead of panic handle error
        try interpreter.eval_block(func.declaration.body, env);
        return Value{ .Number = 10 };
    }

    fn arity(self: *anyopaque) usize {
        const func: *Function = @ptrCast(@alignCast(self));
        std.debug.print("\nname:{s} arity: {d}\n", .{ func.declaration.name.lexeme, func.declaration.params.items.len });
        return func.declaration.params.items.len;
    }

    pub fn callable(self: *Self) Callable {
        return Callable{
            .ptr = self,
            .call_func = call,
            .arity_func = arity,
        };
    }
};
