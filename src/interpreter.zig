const std = @import("std");
const expr = @import("expression.zig");
const parser = @import("parser.zig");
const env = @import("environment.zig");
const scanner = @import("scanner.zig");
const callable = @import("callable.zig");
const testing = std.testing;

const Allocator = std.mem.Allocator;

pub const RuntimeError = error{
    BinaryOperandTypeMismatch,
    UnaryOperandTypeMismatch,
    IllegalOperator,
    UndefinedVar,
    ReturnNotInsideFunction,
    VarAlreadyDefinedInLocalScope,
    InvalidCallAttempt,
    MismatchedArity,
    OnlyInstancesHaveProperties,
    UndefinedProperty,
    InstanceNotFound,
} || std.mem.Allocator.Error;

pub const ValueType = enum {
    Number,
    String,
    Bool,
    NativeFunc,
    DefFunc,
    Class,
    ClassInstance,
    Nil,
};

pub const Value = union(ValueType) {
    const Self = @This();
    Number: f64,
    String: []const u8,
    Bool: bool,
    NativeFunc: callable.NativeFunction,
    DefFunc: callable.DefinedFunction,
    Class: callable.Class,
    ClassInstance: callable.ClassInstance,
    Nil,

    pub fn equal(self: Value, other: Value) bool {
        const same_type = switch (self) {
            .Number => @as(ValueType, other) == .Number,
            .String => @as(ValueType, other) == .String,
            .Bool => @as(ValueType, other) == .Bool,
            .Nil => @as(ValueType, other) == .Nil,
            else => false,
        };
        if (!same_type) return false;

        return switch (self) {
            .Number => self.Number == other.Number,
            .String => std.mem.eql(u8, self.String, other.String),
            .Bool => self.Bool == other.Bool,
            .Nil => true,
            else => false,
        };
    }

    fn is_truthy(value: Value) bool {
        return switch (value) {
            .Bool => |val| val,
            .Nil => false,
            else => true,
        };
    }
};

fn check_op_type(expected: ValueType, val: Value) bool {
    return @as(ValueType, val) == expected;
}

fn check_op_types(expected: ValueType, left: Value, right: Value) bool {
    return check_op_type(expected, left) and check_op_type(expected, right);
}

pub const ClassType = enum { CLASS, NONE };

pub const Interpreter = struct {
    const Self = @This();
    const Error = struct {
        err: RuntimeError,
        message: []const u8,
        token: scanner.Token,
    };

    pub const CallStackEntry = struct { id: u64, name: []const u8 };
    /// id=0 is for starting script
    id_counter: u64 = 0,
    allocator: Allocator,
    runtime_error: ?Error,
    globals: env.Environment,
    environment: env.Environment,
    callstack: std.ArrayList(CallStackEntry),
    return_val: ?Value = null,

    classes: std.AutoArrayHashMap(u64, callable.Class),
    class_instances: std.AutoArrayHashMap(u64, callable.ClassInstance),
    functions: std.AutoArrayHashMap(u64, callable.DefinedFunction),

    curr_class: ClassType = .NONE,

    pub fn init(allocator: Allocator) Self {
        var globals = env.Environment.init(allocator);
        const clock = callable.def_clock();
        globals.define("clock", Value{ .NativeFunc = clock }) catch {
            @panic("Error defining native functions.");
        };

        // try globals.define("clock", callable.Callable)
        return Self{
            .allocator = allocator,
            .globals = globals,
            .environment = globals,
            .callstack = std.ArrayList(CallStackEntry).init(allocator),
            .runtime_error = null,

            .classes = std.AutoArrayHashMap(u64, callable.Class).init(allocator),
            .class_instances = std.AutoArrayHashMap(u64, callable.ClassInstance).init(allocator),
            .functions = std.AutoArrayHashMap(u64, callable.DefinedFunction).init(allocator),
        };
    }

    fn generate_id(self: *Self) u64 {
        self.id_counter = self.id_counter + 1;
        return self.id_counter;
    }
    pub fn deinit(self: *Self) void {
        try self.environment.deinit();
    }

    fn raise_err(self: *Self, err: RuntimeError, token: scanner.Token, comptime fmt: []const u8, args: anytype) RuntimeError!void {
        self.runtime_error = .{
            .err = err,
            .message = try std.fmt.allocPrint(self.allocator, fmt, args),
            .token = token,
        };
        return err;
    }

    fn interpret_literal(e: expr.LiteralExpr) Value {
        if (e.value) |val| {
            return switch (val) {
                .String => |str| Value{ .String = str },
                .Number => |num| Value{ .Number = num },
                .True => Value{ .Bool = true },
                .False => Value{ .Bool = false },
                .Nil => Value{ .Nil = undefined },
            };
        }
        unreachable;
    }

    fn interpret_unary(self: *Self, e: expr.UnaryExpr) RuntimeError!Value {
        const right = try self.interpret_expr(e.right.*);

        switch (e.operator.token_type) {
            .MINUS => {
                if (!check_op_type(.Number, right)) {
                    try self.raise_err(
                        error.UnaryOperandTypeMismatch,
                        e.operator,
                        "invalid operand for unary op {s} on type {s} at line={d},col={d}",
                        .{ e.operator.lexeme, @tagName(right), e.operator.line_num, e.operator.col },
                    );
                }
                return Value{ .Number = -right.Number };
            },
            .BANG => return Value{ .Bool = !right.is_truthy() },
            else => {
                try self.raise_err(
                    error.IllegalOperator,
                    e.operator,
                    "illegal unary op {s} at line={d},col={d}",
                    .{ e.operator.lexeme, e.operator.line_num, e.operator.col },
                );
                return error.IllegalOperator;
            },
        }
    }

    fn interpret_binary(self: *Self, e: expr.BinaryExpr) RuntimeError!Value {
        const left = try self.interpret_expr(e.left.*);
        const right = try self.interpret_expr(e.right.*);

        if (!check_op_types(.Number, left, right))
            try self.raise_err(
                error.BinaryOperandTypeMismatch,
                e.operator,
                "invalid operands for binary op {s} on types {s} and {s} at line={d},col={d}",
                .{
                    e.operator.lexeme,
                    @tagName(left),
                    @tagName(right),
                    e.operator.line_num,
                    e.operator.col,
                },
            );

        return switch (e.operator.token_type) {
            .MINUS => Value{ .Number = left.Number - right.Number },
            .SLASH => Value{ .Number = left.Number / right.Number },
            .STAR => Value{ .Number = left.Number * right.Number },
            .PLUS => Value{ .Number = left.Number + right.Number },
            .GREATER => Value{ .Bool = left.Number > right.Number },
            .GREATER_EQUAL => Value{ .Bool = left.Number >= right.Number },
            .LESS => Value{ .Bool = left.Number < right.Number },
            .LESS_EQUAL => Value{ .Bool = left.Number <= right.Number },
            .BANG_EQUAL => Value{ .Bool = !left.equal(right) },
            .EQUAL_EQUAL => Value{ .Bool = left.equal(right) },
            else => {
                try self.raise_err(
                    error.IllegalOperator,
                    e.operator,
                    "illegal binary op {s} at line={d},col={d}",
                    .{ e.operator.lexeme, e.operator.line_num, e.operator.col },
                );
                return error.IllegalOperator;
            },
        };
    }

    fn interpret_group(self: *Self, e: expr.GroupExpr) RuntimeError!Value {
        return try self.interpret_expr(e.expression.*);
    }

    fn interpret_var_expr(self: *Self, e: expr.VariableExpr) RuntimeError!Value {
        const val = self.environment.get(e.name.lexeme) catch |err| {
            switch (err) {
                error.NotDeclaredAndUndefined => try self.raise_err(
                    error.UndefinedVar,
                    e.name,
                    "use of undefined var '{s}' at line={d},col={d}. '{s}' was never declared.",
                    .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
                ),
                error.DeclaredButUndefined => try self.raise_err(
                    error.UndefinedVar,
                    e.name,
                    "use of undefined var '{s}' at line={d},col={d}. But '{s}' was previously declared.",
                    .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
                ),
                else => {},
            }
            return error.UndefinedVar;
        };
        return val;
    }

    fn interpret_asgn_expr(self: *Self, e: expr.AssignExpr) RuntimeError!Value {
        const value = try self.interpret_expr(e.value.*);
        self.environment.assign(e.name, value) catch
            try self.raise_err(
            error.UndefinedVar,
            e.name,
            "use of undefined var '{s}' at line={d},col={d}. {s} was never declared.",
            .{ e.name.lexeme, e.name.line_num, e.name.col, e.name.lexeme },
        );
        return value;
    }

    fn interpret_get_expr(self: *Self, e: expr.GetExpr) RuntimeError!Value {
        const obj = try self.interpret_expr(e.object.*);
        switch (obj) {
            .ClassInstance => |instance| {
                if (self.class_instances.get(instance.id)) |i| {
                    return i.get(self.allocator, e.name.lexeme) catch {
                        try self.raise_err(error.UndefinedProperty, e.name, "Undefined property line={d},col={d}", .{
                            e.name.line_num,
                            e.name.col,
                        });
                        return error.UndefinedProperty;
                    };
                } else {
                    try self.raise_err(error.UndefinedProperty, e.name, "Instance of Class '{s}' of id={d} at line={d},col={d} not found", .{
                        instance.class.name,
                        instance.id,
                        e.name.line_num,
                        e.name.col,
                    });
                    return error.InstanceNotFound;
                }
            },
            else => {},
        }
        try self.raise_err(error.OnlyInstancesHaveProperties, scanner.Token.init(.FUN, "fun", null, 100, 100), "Only instances have properties line={d},col={d}", .{ 100, 100 });
        return error.OnlyInstancesHaveProperties;
    }

    fn interpret_set_expr(self: *Self, e: expr.SetExpr) RuntimeError!Value {
        const obj = try self.interpret_expr(e.object.*);
        switch (obj) {
            .ClassInstance => |ci| {
                const value = try self.interpret_expr(e.val.*);

                if (self.class_instances.getPtr(ci.id)) |instance| {
                    try instance.set(e.name.lexeme, value);
                } else {
                    return error.InstanceNotFound;
                }
                // e.object = obj;
                // self.environment.assign(scanner.Token.init(.IDENTIFIER, "test", null, 10, 10), obj) catch unreachable;
                return value;
            },
            else => unreachable,
        }
        try self.raise_err(error.OnlyInstancesHaveProperties, scanner.Token.init(.FUN, "fun", null, 100, 100), "Only instances have fields line={d},col={d}", .{ 100, 100 });
        return error.OnlyInstancesHaveProperties;
    }

    pub fn interpret_expr(self: *Self, e: expr.Expr) RuntimeError!Value {
        return switch (e) {
            .Binary => |bin| try self.interpret_binary(bin),
            .Unary => |un| try self.interpret_unary(un),
            .Group => |grp| try self.interpret_group(grp),
            .Literal => |lit| interpret_literal(lit),
            .Variable => |v| try self.interpret_var_expr(v),
            .Assign => |a| try self.interpret_asgn_expr(a),
            .Call => |c| try self.interpret_call_expr(c),
            .GetExpr => |g| try self.interpret_get_expr(g),
            .SetExpr => |s| try self.interpret_set_expr(s),
            .ThisExpr => |t| try self.interpret_this_expr(t),
            else => unreachable,
        };
    }

    // print "hi" or 2; // "hi".
    // print nil or "yes"; // "yes".
    fn interpret_logical_expr(self: *Self, e: expr.LogicalExpr) RuntimeError!Value {
        // TODO: Test logical expression
        const left = try self.interpret_expr(e.left.*);

        switch (e.operator.token_type) {
            .OR => if (left.is_truthy()) return left,
            else => if (!left.is_truthy()) return left,
        }

        return try self.interpret_expr(e.right.*);
    }

    pub fn interpret_block(self: *Self, block: std.ArrayList(expr.Stmt), envr: env.Environment) RuntimeError!void {
        const prev = self.environment;
        defer self.environment = prev;

        self.environment = envr;
        for (block.items) |stmt| {
            // if (self.return_val) |val| return val;
            try self.execute_stmt(stmt);
        }
    }

    fn check_arity(self: *Self, token: scanner.Token, args_len: usize, fun_arity: usize) RuntimeError!void {
        if (args_len != fun_arity) {
            try self.raise_err(error.InvalidCallAttempt, token, "Expected {d} arguments but got {d}", .{
                fun_arity,
                args_len,
            });
            return error.MismatchedArity;
        }
    }

    fn interpret_call_expr(self: *Self, e: expr.CallExpr) RuntimeError!Value {
        // const callee = self.environment.get("clock") catch @panic("err");
        std.debug.print("Here!!\n", .{});
        // std.debug.print("{any}", .{self.environment.get("fib")});
        const callee = try self.interpret_expr(e.callee.*);

        var args = std.ArrayList(Value).init(self.allocator);
        for (e.args.items) |arg| {
            try args.append(try self.interpret_expr(arg));
        }

        // this could either be a function or a class constructor.
        switch (callee) {
            .DefFunc => |f| {
                if (f.instance_id) |_| {
                    const enc_class = self.curr_class;
                    self.curr_class = .CLASS;

                    // checking if number of expected parameters = number of passed arguements in a function call
                    try self.check_arity(e.paren, args.items.len, f.arity(self));
                    const result = try f.call(self, args.items);
                    self.curr_class = enc_class;
                    return result;
                } else {
                    return try f.call(self, args.items);
                }
            },
            .NativeFunc => |f| {
                try self.check_arity(e.paren, args.items.len, f.arity(self));
                return try f.call(self, args.items);
            },
            .Class => |f| {
                const enc_class = self.curr_class;
                self.curr_class = .CLASS;

                try self.check_arity(e.paren, args.items.len, f.arity(self));
                const instance = try f.call(self.generate_id(), self, args.items);

                try self.class_instances.put(instance.ClassInstance.id, instance.ClassInstance);

                self.curr_class = enc_class;
                return instance;
            },
            else => {
                try self.raise_err(error.InvalidCallAttempt, e.paren, "Can only call functions and classes.", .{});
                return error.InvalidCallAttempt;
            },
        }
    }

    pub fn interpret_this_expr(self: *Self, e: expr.ThisExpr) RuntimeError!Value {
        switch (self.curr_class) {
            .CLASS => {
                const val = self.environment.get(e.keyword.lexeme) catch {
                    try self.raise_err(
                        error.UndefinedVar,
                        e.keyword,
                        "'this' is not defined in this context line={d},col={d}.",
                        .{ e.keyword.line_num, e.keyword.col },
                    );
                    return error.UndefinedVar;
                };
                return val;
            },
            .NONE => {
                try self.raise_err(error.InvalidCallAttempt, e.keyword, "Can't use 'this' outside of a class line={d}, col={d}", .{
                    e.keyword.line_num,
                    e.keyword.col,
                });
                return error.InvalidCallAttempt;
            },
        }
    }

    pub fn execute_stmt(self: *Self, stmt: expr.Stmt) RuntimeError!void {
        switch (stmt) {
            .BlockStmt => |e| {
                const enclosing = try self.allocator.create(env.Environment);
                enclosing.* = self.environment;
                try self.interpret_block(e, env.Environment.with_enclosing(self.allocator, enclosing));
            },
            .ClassStmt => |c| {
                // std.debug.print("{any}", .{c.methods.items});
                try self.environment.define(c.name.lexeme, null);
                var class = callable.Class.init(self.generate_id(), c.name.lexeme, self.allocator);

                for (c.methods.items) |method| {
                    switch (method) {
                        .FuncStmt => |f| {
                            const closure = try self.allocator.create(env.Environment);
                            try class.methods.put(f.name.lexeme, callable.DefinedFunction{
                                .id = self.generate_id(),
                                .name = f.name.lexeme,
                                .params = try f.params.clone(),
                                .closure = closure,
                                .body = try f.body.clone(),
                                .instance_id = 10,
                                .is_init = std.mem.eql(u8, f.name.lexeme, "init"),
                            });
                        },
                        else => unreachable,
                    }
                }
                try self.environment.define(c.name.lexeme, Value{ .Class = class });
                try self.classes.put(self.generate_id(), class);
            },
            .ExprStmt => |e| {
                _ = try self.interpret_expr(e);
            },
            .FuncStmt => |f| {
                const closure = try self.allocator.create(env.Environment);
                closure.* = self.environment;
                const func = callable.DefinedFunction{
                    .id = self.generate_id(),
                    .name = f.name.lexeme,
                    .params = try f.params.clone(),
                    .closure = closure,
                    .body = try f.body.clone(),
                };

                try self.environment.define(f.name.lexeme, Value{ .DefFunc = func });
                try func.closure.define(f.name.lexeme, Value{ .DefFunc = func });
                try self.functions.put(self.generate_id(), func);
            },
            .IfStmt => |i| {
                var condition = try self.interpret_expr(i.condition);
                if (condition.is_truthy()) {
                    try self.execute_stmt(i.then_br.*);
                    return;
                }
                if (i.else_br) |e| {
                    try self.execute_stmt(e.*);
                    return;
                }
            },
            .PrintStmt => |e| {
                const val = try self.interpret_expr(e);
                switch (val) {
                    .String => |str| std.debug.print("{s}", .{str}),
                    .Number => |num| std.debug.print("{d}", .{num}),
                    .Bool => |b| std.debug.print("{?}", .{b}),
                    .Nil => std.debug.print("nil", .{}),
                    .NativeFunc => |f| std.debug.print("func {s}", .{f.name}),
                    .DefFunc => |f| std.debug.print("func {s}", .{f.name}),
                    .Class => |c| std.debug.print("class {s}", .{c.name}),
                    .ClassInstance => |i| std.debug.print("instance of {s}", .{i.class.name}),
                }
            },
            .ReturnStmt => |r| {
                if (self.callstack.getLastOrNull()) |_| {
                    if (r.expression) |e| {
                        self.return_val = try self.interpret_expr(e);
                        return;
                    }
                    self.return_val = Value{ .Nil = undefined };
                }

                try self.raise_err(error.ReturnNotInsideFunction, r.keyword, "return not called from inside a function, line={d},col={d}", .{ r.keyword.line_num, r.keyword.col });
            },
            .VarStmt => |e| {
                var val: ?Value = null;
                if (e.initializer) |in| {
                    val = try self.interpret_expr(in);
                    // std.debug.print("\nHas Initializer: {any}\n", .{val});
                }

                try self.environment.define(e.name.lexeme, val);
            },
            .WhileStmt => |w| {
                while ((try self.interpret_expr(w.condition)).is_truthy()) {
                    try self.execute_stmt(w.body.*);
                }
            },
        }
    }

    pub fn interpret(self: *Self, stmts: []expr.Stmt) RuntimeError!void {
        for (stmts) |value| {
            self.execute_stmt(value) catch |err| switch (err) {
                error.OutOfMemory => @panic("Error out of memory"),
                else => return err,
            };
        }
    }
};

test "Defined Func Stmt" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ class Person{
        \\ init(){
        \\ this.name = "class_name";
        \\ }
        \\ sayName(){
        \\ print this.name;
        \\ }
        \\ }
        \\ 
        \\
        \\ var person = Person();
        \\ person.sayName();
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    interpreter.interpret(stmts.items) catch |err| {
        if (interpreter.runtime_error) |e| {
            std.debug.print("{s}", .{e.message});
            // std.debug.print("{any}", .{interpreter.callstack.items});
        } else {
            std.debug.print("{any}", .{err});
        }
    };

    // const inst = try interpreter.environment.get("test");
    //
    // for (interpreter.class_instances.get(inst.ClassInstance.id).?.fields.keys()) |field| {
    //     const instance = try interpreter.class_instances.get(inst.ClassInstance.id).?.get(field);
    //     const child_name = try interpreter.class_instances.get(instance.ClassInstance.id).?
    //         .get("name");
    //     std.debug.print("\nKey: {s} , Val: {s}\n", .{ field, child_name.String });
    // }

    std.debug.print("\n", .{});
}

test "Native Func Stmt" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ print clock();
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    for (stmts.items) |stmt| {
        try interpreter.execute_stmt(stmt);
    }
    std.debug.print("\n", .{});
}

test "While Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ for(var i=0;i<2;i=i+1) print(i);
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        for (try par.get_errors()) |err| {
            std.debug.print("\n{s}{s}\n", .{ src, err });
        }
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    std.debug.print("\n", .{});
    for (stmts.items) |stmt| {
        try interpreter.execute_stmt(stmt);
    }
    std.debug.print("\n", .{});
}

test "If Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ if (10 != 10){ 10; } else { 20; }
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = par.parse_stmts(allocator) catch {
        std.debug.print("\n{s}\n", .{par.errors.items[0].message});
        return;
    };
    // std.debug.print("{s}", .{(try stmts.items[0].to_string(allocator)).items});

    var interpreter = Interpreter.init(allocator);
    for (stmts.items) |stmt| {
        try interpreter.execute_stmt(stmt);
    }
}

test "Block Statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ var outer = 10;
        \\ { 
        \\    var value = (20 * 20); 
        \\ }
        \\ outer;
    ;

    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();

    var par = parser.Parser.init(allocator, try lexer.tokens.clone());

    const stmts = try par.parse_stmts(allocator);

    var interpreter = Interpreter.init(allocator);

    for (stmts.items) |stmt| {
        try interpreter.execute_stmt(stmt);
    }
}

test "Declaration & Assignment Expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\ var value = (20 * 20);
    ;
    var lexer = try scanner.Scanner.init(allocator, src);
    try lexer.scan_tokens();
    var par = parser.Parser.init(allocator, try lexer.tokens.clone());
    const stmts = try par.parse_stmts(allocator);
    var interpreter = Interpreter.init(allocator);
    try interpreter.execute_stmt(stmts.items[0]);

    var val = try interpreter.environment.get("value");
    try std.testing.expect(val.equal(Value{ .Number = 400 }));
}

test "Expression Statement Evaluations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const table = [_]struct { []const u8, Value }{
        .{ "true;", Value{ .Bool = true } },
        .{ "false;", Value{ .Bool = false } },
        .{ "!false;", Value{ .Bool = true } },
        .{ "!true;", Value{ .Bool = false } },
        .{ "1;", Value{ .Number = 1 } },
        .{ "-1;", Value{ .Number = -1 } },

        .{ "10 > 11;", Value{ .Bool = false } },
        .{ "10 < 11;", Value{ .Bool = true } },
        .{ "10 >= 10;", Value{ .Bool = true } },
        .{ "10 <= 10;", Value{ .Bool = true } },
        .{ "10 <= 11;", Value{ .Bool = true } },
        .{ "10 <= 9;", Value{ .Bool = false } },

        .{ "10 == 9;", Value{ .Bool = false } },
        .{ "10 == 10;", Value{ .Bool = true } },
        .{ "10 != 10;", Value{ .Bool = false } },
        .{ "10 != 9;", Value{ .Bool = true } },

        .{ "1+1;", Value{ .Number = 2 } },
        .{ "2*3;", Value{ .Number = 6 } },
        .{ "6/3;", Value{ .Number = 2 } },
        .{ "6-3;", Value{ .Number = 3 } },

        .{ "\"hello world\";", Value{ .String = "hello world" } },
    };
    for (table) |val| {
        var lex = try scanner.Scanner.init(alloc, val[0]);
        try lex.scan_tokens();
        var p = parser.Parser.init(alloc, try lex.tokens.clone());
        var stmts = try p.parse_stmts(alloc);

        var interpreter = Interpreter.init(alloc);
        const result = try alloc.create(Value);
        result.* = try interpreter.interpret_expr(stmts.pop().ExprStmt);
        // std.debug.print("\n{} {}\n", .{ interpretd, val[1] });
        try std.testing.expect(result.equal(val[1]));
    }
}
