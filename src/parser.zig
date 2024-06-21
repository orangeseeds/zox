const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const expression_z = @import("expression.zig");
const Expr = expression_z.Expr;
const Stmt = expression_z.Stmt;
const Token = scanner.Token;
const Literal = scanner.Literal;
const TokenType = scanner.TokenType;

pub const Error = error{
    UnexpectedToken,
    ParserError,
    ExpectedExpression,
    InvalidAssignmentTarget,
    ExceededMaxNumArgs,
    ParameterNumExceeded,
} || std.mem.Allocator.Error;

pub const DetErr = struct {
    token: Token,
    message: []const u8,
    fn init(token: Token, message: []const u8) DetErr {
        return DetErr{
            .token = token,
            .message = message,
        };
    }
};

pub const Parser = struct {
    const Self = @This();
    current: u32 = 0,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(DetErr),
    tokens: std.ArrayList(Token),

    pub fn init(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) Self {
        return Self{
            .tokens = tokens,
            .allocator = allocator,
            .errors = std.ArrayList(DetErr).init(allocator),
        };
    }

    pub fn get_errors(self: *Self) ![][]u8 {
        var v_errs = std.ArrayList([]u8).init(self.allocator);
        for (self.errors.items) |err| {
            try v_errs.append(try std.fmt.allocPrint(self.allocator, "{s} at line={d},col={d}", .{
                err.message,
                err.token.line_num,
                err.token.col,
            }));
        }
        return v_errs.items;
    }

    /// peek at current token
    fn peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    /// peek at previous token
    fn previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    /// check if current token is EOF
    fn at_end(self: *Self) bool {
        return self.peek().token_type == .EOF;
    }

    /// advance one token forward
    fn advance(self: *Self) Token {
        if (!self.at_end()) self.current += 1;
        return self.previous();
    }

    /// check if current token of given TokenType
    fn check(self: *Self, token_type: TokenType) bool {
        if (self.at_end()) return false;
        return self.peek().token_type == token_type;
    }

    fn match_one_of(self: *Self, slice: []const TokenType) bool {
        for (slice) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    /// check if current token matches any of the given TokenTypes.
    /// if a match is found advance one token forward.
    fn match(self: *Self, token_types: anytype) bool {
        const arr: [token_types.len]TokenType = token_types;
        return self.match_one_of(&arr);
    }

    /// check if current token matches the given TokenType.
    /// If not add a new error to parser error.
    fn consume(self: *Self, token_type: TokenType, message: []const u8) Error!Token {
        if (self.check(token_type)) return self.advance();
        self.push_error(self.peek(), message);
        return error.UnexpectedToken;
    }

    /// Pushes an error into the errors, stack to later see.
    fn push_error(self: *Self, token: Token, message: []const u8) void {
        self.errors.append(DetErr.init(token, message)) catch {
            @panic("Error: Out Of Memory, appending parse error!");
        };
        // TODO: handle error properly
        // https://craftinginterpreters.com/parsing-expressions.html#entering-panic-mode
    }

    fn primary(self: *Self) Error!Expr {
        if (self.match(.{.FALSE})) return Expr{ .Literal = .{ .value = .False } };
        if (self.match(.{.TRUE})) return Expr{ .Literal = .{ .value = .True } };
        if (self.match(.{.NIL})) return Expr{ .Literal = .{ .value = .Nil } };

        if (self.match(.{ .NUMBER, .STRING })) return Expr{ .Literal = .{ .value = self.previous().literal.? } };

        if (self.match(.{.IDENTIFIER})) return Expr{ .Variable = .{ .name = self.previous() } };

        // check if the start is a LEFT_PAREN, read the immediate expr
        // Then expecte the next token to be a RIGHT_PAREN
        if (self.match(.{.LEFT_PAREN})) {
            const expr = try self.allocator.create(Expr);
            expr.* = try self.expression();
            _ = try self.consume(.RIGHT_PAREN, "Expected ')' after the expression.");
            const new_expr = Expr{ .Group = .{ .expression = expr } };
            return new_expr;
        }

        self.push_error(self.peek(), "Expected expression, current token cannot be the start of any defined expression.");
        return error.ExpectedExpression;
    }

    fn finish_call(self: *Self, callee: Expr) Error!Expr {
        var args = std.ArrayList(Expr).init(self.allocator);
        if (!self.check(.RIGHT_PAREN)) {
            while (true) { // basically do while.
                if (args.items.len >= 255) {
                    self.push_error(self.peek(), "Number of arguements in the function call exeeded max amount.");
                    return error.ExceededMaxNumArgs;
                }
                try args.append(try self.expression());

                if (!self.match(.{.COMMA})) break;
            }
        }

        const paren = try self.consume(.RIGHT_PAREN, "expect ')' after arguments");
        const expr = try self.allocator.create(Expr);
        expr.* = callee;
        return Expr{ .Call = .{ .callee = expr, .paren = paren, .args = args } };
    }

    fn call(self: *Self) Error!Expr {
        const expr = try self.allocator.create(Expr);
        expr.* = try self.primary();

        const curr: *Expr = expr;
        while (true) {
            // TODO: check alloc after free
            if (self.match(.{.LEFT_PAREN})) {
                const new = try self.allocator.create(Expr);
                new.* = try self.finish_call(expr.*);
                curr.* = new.*;
            } else if (self.match(.{.DOT})) {
                const name = try self.consume(.IDENTIFIER, "Expected property name after '.',");
                const new = try self.allocator.create(Expr);

                const new_obj = try self.allocator.create(Expr);
                new_obj.* = expr.*;
                new.* = Expr{ .GetExpr = .{ .name = name, .object = new_obj } };
                curr.* = new.*;
            } else {
                break;
            }
        }

        return expr.*;
    }

    fn unary(self: *Self) Error!Expr {
        if (self.match(.{ .BANG, .MINUS })) {
            const operator = self.previous();
            const right_exp = try self.allocator.create(Expr);
            right_exp.* = try self.unary();
            return Expr{ .Unary = .{ .right = right_exp, .operator = operator } };
        }

        return try self.call();
    }

    fn factor(self: *Self) Error!Expr {
        var expr = try self.allocator.create(Expr);
        expr.* = try self.unary();

        while (self.match(.{ .SLASH, .STAR })) {
            const operator = self.previous();
            const right = try self.allocator.create(Expr);
            right.* = try self.unary();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr.*;
    }

    fn addition(self: *Self) Error!Expr {
        var expr = try self.allocator.create(Expr);
        expr.* = try self.factor();

        while (self.match(.{ .MINUS, .PLUS })) {
            const operator = self.previous();
            const right = try self.allocator.create(Expr);
            right.* = try self.factor();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr.*;
    }

    fn comparison(self: *Self) Error!Expr {
        var expr = try self.allocator.create(Expr);
        expr.* = try self.addition();

        while (self.match(.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.allocator.create(Expr);
            right.* = try self.addition();

            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = new_expr;
        }

        return expr.*;
    }

    fn equality(self: *Self) Error!Expr {
        const left = try self.allocator.create(Expr);
        left.* = try self.comparison();

        while (self.match(.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = self.previous();
            const right = try self.allocator.create(Expr);
            right.* = try self.comparison();
            const new_expr = Expr{ .Binary = .{ .left = left, .operator = operator, .right = right } };
            return new_expr;
        }

        return left.*;
    }

    fn and_expr(self: *Self) Error!Expr {
        const exp = try self.allocator.create(Expr);
        exp.* = try self.equality();

        while (self.match(.{.AND})) {
            const operator = self.previous();
            const right = try self.allocator.create(Expr);
            right.* = try self.equality();
            exp.* = Expr{ .Logical = .{ .left = exp, .operator = operator, .right = right } };
        }
        return exp.*;
    }

    fn or_expr(self: *Self) Error!Expr {
        const exp = try self.allocator.create(Expr);
        exp.* = try self.equality();

        const curr: *Expr = exp;
        while (true) {
            if (!self.match(.{.OR})) break;
            const new = try self.allocator.create(Expr);
            const operator = self.previous();
            const new_and = try self.allocator.create(Expr);
            new_and.* = try self.and_expr();
            new.* = Expr{ .Logical = .{ .left = exp, .operator = operator, .right = new_and } };
            curr.* = new.*;
        }
        return exp.*;
    }

    fn assignment(self: *Self) Error!Expr {
        // TODO: remaining to write tests for assignment exressions
        const exp = try self.allocator.create(Expr);
        exp.* = try self.equality();

        if (self.match(.{.EQUAL})) {
            const equal_op = self.previous();
            const value = try self.allocator.create(Expr);
            value.* = try self.assignment();

            switch (exp.*) {
                .Variable => |v| {
                    const name = v.name;
                    return Expr{ .Assign = .{ .name = name, .value = value } };
                },
                .GetExpr => |g| {
                    return Expr{ .SetExpr = .{ .object = g.object, .name = g.name, .val = value } };
                },
                else => {
                    self.push_error(equal_op, "Invalid assignment target");
                    return error.InvalidAssignmentTarget;
                },
            }
        }

        return exp.*;
    }

    fn block(self: *Self) Error!std.ArrayList(Stmt) {
        var stmts = std.ArrayList(Stmt).init(self.allocator);

        while (!self.check(.RIGHT_BRACE) and !self.at_end()) {
            try stmts.append(try self.declaration());
            // std.debug.print("{}\n", .{self.peek()});
        }

        _ = try self.consume(.RIGHT_BRACE, "Expected '}' after block.");
        return stmts;
    }

    fn expression(self: *Self) Error!Expr {
        return try self.assignment();
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.at_end()) {
            if (self.previous().token_type == .SEMICOLON) return;
            switch (self.peek().token_type) {
                TokenType.CLASS => {},
                TokenType.FUN => {},
                TokenType.VAR => {},
                TokenType.FOR => {},
                TokenType.IF => {},
                TokenType.WHILE => {},
                TokenType.PRINT => {},
                TokenType.RETURN => return,
                else => {},
            }
        }

        _ = self.advance();
    }

    pub fn parse(self: *Self) Error!Expr {
        return try self.expression();
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errors.deinit();
    }

    // Handling Statements -------------------------------------------------

    fn print_stmt(self: *Self) Error!Stmt {
        const val = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .PrintStmt = val };
    }
    fn expr_stmt(self: *Self) Error!Stmt {
        const val = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .ExprStmt = val };
    }
    pub fn var_decl_stmt(self: *Self) Error!Stmt {
        const name = try self.consume(.IDENTIFIER, "Expect variable name.");

        var initializer: ?Expr = undefined;
        if (self.match(.{.EQUAL})) {
            initializer = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
        return Stmt{ .VarStmt = .{ .initializer = initializer, .name = name } };
    }

    fn declaration(self: *Self) Error!Stmt {
        if (self.match(.{.VAR})) {
            const stmt = try self.allocator.create(Stmt);
            stmt.* = self.var_decl_stmt() catch |err|
                switch (err) {
                Error.OutOfMemory => return err,
                else => {
                    self.synchronize();
                    return error.UnexpectedToken;
                },
            };
            return stmt.*;
        }
        return try self.parse_stmt();
    }

    // parsing if statements
    // since else is optional, if () if() else(), can be parsed in two ways.
    // we parse using the nearest if to the else
    fn if_stmt(self: *Self) Error!Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expected '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expected ')' after if condition.");

        const then_branch = try self.allocator.create(Stmt);
        then_branch.* = try self.parse_stmt();

        var else_branch: ?*Stmt = null;
        if (self.match(.{.ELSE})) {
            else_branch = try self.allocator.create(Stmt);
            else_branch.?.* = try self.parse_stmt();
        }

        return Stmt{ .IfStmt = .{
            .condition = condition,
            .then_br = then_branch,
            .else_br = else_branch,
        } };
    }

    fn while_stmt(self: *Self) Error!Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expected '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expected ')' after if condition.");

        const body = try self.allocator.create(Stmt);
        body.* = try self.parse_stmt();

        return Stmt{ .WhileStmt = .{ .condition = condition, .body = body } };
    }

    fn for_stmt(self: *Self) Error!Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expected '(' after for.");
        // Initializer
        var initializer: ?Stmt = null;
        if (self.match(.{.SEMICOLON}))
            initializer = null
        else if (self.match(.{.VAR}))
            initializer = try self.var_decl_stmt()
        else
            initializer = try self.expr_stmt();

        // Condition
        var condition: ?Expr = null;
        if (!self.match(.{.SEMICOLON})) condition = try self.expression();

        _ = try self.consume(.SEMICOLON, "Expected ';' after for loop condition.");

        // Increment
        var increment: ?Expr = null;
        if (!self.check(.RIGHT_PAREN)) increment = try self.expression();

        _ = try self.consume(.RIGHT_PAREN, "Expected ')' to close the for clause.");

        // Body
        const body = try self.allocator.create(Stmt);
        body.* = try self.parse_stmt();

        // Desugaring for loop into a while loop ------
        const with_incr = try self.allocator.create(Stmt);
        if (increment) |val| {
            var new_body = std.ArrayList(Stmt).init(self.allocator);
            try new_body.append(body.*);
            try new_body.append(Stmt{ .ExprStmt = val });
            with_incr.* = Stmt{ .BlockStmt = new_body };
        }

        if (condition == null) {
            condition = Expr{ .Literal = .{ .value = .True } };
        }

        const desug_while = Stmt{ .WhileStmt = .{ .condition = condition.?, .body = with_incr } };
        var with_decl = desug_while;
        if (initializer) |val| {
            var new_body = std.ArrayList(Stmt).init(self.allocator);
            try new_body.append(val);
            try new_body.append(desug_while);
            with_decl = Stmt{ .BlockStmt = new_body };
        }

        return with_decl;
    }

    fn fun_stmt(self: *Self, kind: []const u8) Error!Stmt {
        // TODO: test fun stmt parsing
        const name = try self.consume(
            .IDENTIFIER,
            try std.fmt.allocPrint(self.allocator, "Expect {s} name.", .{kind}),
        );

        _ = try self.consume(
            .LEFT_PAREN,
            try std.fmt.allocPrint(self.allocator, "Expect '(' after {s} name.", .{kind}),
        );

        var parameters = std.ArrayList(Token).init(self.allocator);
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                if (parameters.items.len >= 255) {
                    self.push_error(self.peek(), "Can't have more than 255 parameters.");
                    return error.ParameterNumExceeded;
                }
                try parameters.append(try self.consume(.IDENTIFIER, "Expect parameter name."));

                if (!self.match(.{.COMMA})) break;
            }
        }
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after parameters.");

        _ = try self.consume(
            .LEFT_BRACE,
            try std.fmt.allocPrint(self.allocator, "Expect LEFT_BRACE before {s} body.", .{kind}),
        );

        const body = try self.block();
        return Stmt{ .FuncStmt = .{ .name = name, .params = parameters, .body = body } };
    }

    fn return_stmt(self: *Self) Error!Stmt {
        const keyword = self.previous();
        var value: ?Expr = null;
        if (!self.check(.SEMICOLON)) {
            value = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expected ';' after return value.");
        return Stmt{ .ReturnStmt = .{ .keyword = keyword, .expression = value } };
    }

    fn class_decl(self: *Self) Error!Stmt {
        const name = try self.consume(.IDENTIFIER, "Expected class name after 'class' keyword.");
        _ = try self.consume(.LEFT_BRACE, "Expected '{' before class body.");

        var methods = std.ArrayList(expression_z.Stmt).init(self.allocator);

        while (!self.check(.RIGHT_BRACE) and !self.at_end()) {
            try methods.append(try self.fun_stmt("method"));
        }

        _ = try self.consume(.RIGHT_BRACE, "Expected '{' after class body.");

        return Stmt{ .ClassStmt = .{ .name = name, .methods = methods } };
    }

    fn parse_stmt(self: *Self) Error!Stmt {
        if (self.match(.{.CLASS})) return self.class_decl();
        if (self.match(.{.FUN})) return self.fun_stmt("function");
        if (self.match(.{.FOR})) return self.for_stmt();
        if (self.match(.{.IF})) return try self.if_stmt();
        if (self.match(.{.PRINT})) return try self.print_stmt();
        if (self.match(.{.RETURN})) return try self.return_stmt();
        if (self.match(.{.WHILE})) return try self.while_stmt();
        if (self.match(.{.LEFT_BRACE})) return Stmt{ .BlockStmt = try self.block() };
        return try self.expr_stmt();
    }

    pub fn parse_stmts(self: *Self, alloc: std.mem.Allocator) Error!std.ArrayList(Stmt) {
        var stmts = std.ArrayList(Stmt).init(alloc);
        while (!self.at_end()) {
            // try stmts.append(try self.statement());
            try stmts.append(try self.declaration());
        }
        return stmts;
    }
};

fn print_verbose_err(case: []const u8, errs: []DetErr) void {
    std.debug.print("\n", .{});
    for (errs) |err| {
        std.debug.print("{s}: {s} at line={d},col={d}", .{
            case,
            err.message,
            err.token.line_num,
            err.token.col,
        });
    }
    std.debug.print("\n", .{});
}

test "Test Parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const cases = [_]struct { []const u8, []const u8 }{ // case, expected
        .{ "10 * 10 + 10 / 10;", "(+ (* 10 10) (/ 10 10));" },

        .{ "var apple = 10;", "var apple = 10;" },

        .{ "print (10);", "print ( (group 10) );" },

        .{ "if (true) if (false) 10; else 20;", "if (true) { if (false) { 10; } else { 20; } }" },

        .{ "while (true) print(10);", "while (true) { print ( (group 10) ); }" },

        .{ "for ( var i = 10 ; i < 10 ; i = i + 1 ) print 10;", "{var i = 10;while ((< (var i ) 10)) { {print ( 10 );(i = (+ (var i ) 1));} }}" },

        .{ "caller( apple, ball, cat );", "( call (var caller )((var apple ), (var ball ), (var cat )));" },

        .{ "fun new(x,y){ x+y; }", "func new( x, y ){(+ (var x ) (var y ));}" },

        // .{ "{ apple; };", "" },

        .{ "return;", "return;" },
        .{ "return 10;", "return 10;" },
        .{ "return apple;", "return (var apple );" },

        // .{ "a.b = 10;", "" },
    };

    for (cases) |case| {
        var lexer = try Scanner.init(alloc, case[0]);
        try lexer.scan_tokens();
        var parser = Parser.init(alloc, try lexer.tokens.clone());
        const stmts = parser.parse_stmts(alloc) catch {
            for (try parser.get_errors()) |value|
                std.debug.print("\ncase: {s}, {s}\n", .{ case[0], value });

            try std.testing.expect(false);
            return;
        };
        try std.testing.expectEqualStrings(case[1], (try stmts.getLastOrNull().?.to_string(alloc)).items);
    }
}
