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

    /// check if current token matches any of the given TokenTypes.
    /// if a match is found advance one token forward.
    fn match_one_of(self: *Self, slice: []const TokenType) bool {
        for (slice) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

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
        // TODO: handler error properly
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

        self.push_error(self.peek(), "Expect expression.");
        return error.ExpectedExpression;
    }

    fn unary(self: *Self) Error!Expr {
        if (self.match(.{ .BANG, .MINUS })) {
            const operator = self.previous();
            const right_exp = try self.allocator.create(Expr);
            right_exp.* = try self.unary();
            return Expr{ .Unary = .{ .right = right_exp, .operator = operator } };
        }

        return try self.primary();
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

    fn assignment(self: *Self) Error!Expr {
        // TODO: remaining to write tests for assignment exressions
        const exp = try self.allocator.create(Expr);
        exp.* = try self.equality();

        if (self.match(.{.Equal})) {
            const equal_op = self.previous();
            const value = try self.allocator.create(Expr);
            value.* = self.assignment();

            switch (exp.*) {
                .Variable => |v| {
                    const name = v.name;
                    return Expr{ .Assign = .{ .name = name, .value = value } };
                },
                else => try self.push_error(equal_op, "Invalid assignment target"),
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
        return try self.equality();
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

    /// Handling Statements
    ///
    fn print_stmt(self: *Self) !Stmt {
        const val = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .PrintStmt = val };
    }
    fn expression_stmt(self: *Self) !Stmt {
        const val = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .ExprStmt = val };
    }
    pub fn var_declaration(self: *Self) !Stmt {
        const name = try self.consume(.IDENTIFIER, "Expect variable name.");

        var initializer: ?Expr = undefined;
        if (self.match(.{.EQUAL})) {
            initializer = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
        return Stmt{ .VarStmt = .{ .initializer = initializer, .name = name } };
    }

    fn declaration(self: *Self) !Stmt {
        if (self.match(.{.VAR})) {
            const stmt = try self.allocator.create(Stmt);
            stmt.* = self.var_declaration() catch |err|
                switch (err) {
                Error.OutOfMemory => return err,
                else => {
                    self.synchronize();
                    return error.UnexpectedToken;
                },
            };
            return stmt.*;
        }
        return try self.statement();
    }

    fn statement(self: *Self) !Stmt {
        if (self.match(.{.PRINT})) return try self.print_stmt();
        if (self.match(.{.LEFT_BRACE})) return Stmt{ .BlockStmt = try self.block() };
        return try self.expression_stmt();
    }

    pub fn parse_stmts(self: *Self, alloc: std.mem.Allocator) !std.ArrayList(Stmt) {
        var stmts = std.ArrayList(Stmt).init(alloc);
        while (!self.at_end()) {
            // try stmts.append(try self.statement());
            try stmts.append(try self.declaration());
        }
        return stmts;
    }
};

test "Testing the Parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ "apple" == (100 - 10);
    ;

    var lexer = try Scanner.init(allocator, input);
    defer lexer.deinit();
    try lexer.scan_tokens();

    var parser = Parser.init(allocator, try lexer.tokens.clone());
    defer parser.deinit();
    const expr = try parser.parse();

    // catch |err| {
    //         switch (err) {
    //             Error.ParserError => std.debug.print("\nParse Error!\n", .{}),
    //             Error.UnexpectedToken => std.debug.print("\nUnexpeceted Token!\n", .{}),
    //             Error.ExpectedToken => std.debug.print("\nExpected Token!\n", .{}),
    //         }
    //
    //         std.debug.print("{s}", .{parser.errors.getLast().message});
    //         return;
    //     };

    const got = try expr.to_string(allocator);
    const expected = "(== \"apple\" (group (- 100 10)))";
    try std.testing.expect(std.mem.eql(u8, got.items, expected));
}

test "Parsing Declaration Stmt" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ var val = "apple";
    ;

    var lexer = try Scanner.init(allocator, input);
    try lexer.scan_tokens();

    var parser = Parser.init(allocator, try lexer.tokens.clone());
    const stmt = try parser.declaration();
    const str = try stmt.to_string(allocator);
    // std.debug.print("\nexpected: {s}, got:{s}\n", .{ input, str.items });
    try std.testing.expect(std.mem.eql(u8, "var val = \"apple\";", str.items));
}
