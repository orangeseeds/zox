const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const expression_z = @import("expression.zig");
const Expr = expression_z.Expr;
const Token = scanner.Token;
const Literal = scanner.Literal;
const TokenType = scanner.TokenType;

pub const Error = error{
    UnexpectedToken,
    ParserError,
    ExpectedToken,
};

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

const Parser = struct {
    const Self = @This();
    current: u32 = 0,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(DetErr),
    tokens: std.ArrayList(Token),

    fn init(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) Self {
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
    fn match(self: *Self, token_types: []const TokenType) bool {
        for (token_types) |token_type| {
            if (self.check(token_type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
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
        // https://craftinginterpreters.com/parsing-expressions.html#Entering panic mode
    }

    fn primary(self: *Self) Error!Expr {
        if (self.match(&[_]TokenType{.FALSE})) return Expr{ .literal = .{ .value = .False } };
        if (self.match(&[_]TokenType{.TRUE})) return Expr{ .literal = .{ .value = .True } };
        if (self.match(&[_]TokenType{.NIL})) return Expr{ .literal = .{ .value = .Nil } };

        if (self.match(&[_]TokenType{ .NUMBER, .STRING })) {
            return Expr{ .literal = .{ .value = self.previous().literal } }; // TODO: Handle Error Here!
        }

        // check if the start is a LEFT_PAREN, read the immediate expr
        // Then expecte the next token to be a RIGHT_PAREN
        if (self.match(&[_]TokenType{.LEFT_PAREN})) {
            var expr = try self.expression();
            _ = try self.consume(.RIGHT_PAREN, "Expected ')' after the expression.");
            const new_expr = Expr{ .group = .{ .expression = &expr } };
            return new_expr;
        }

        self.push_error(self.peek(), "Expect expression.");
        return error.ExpectedToken;
    }

    fn unary(self: *Self) Error!Expr {
        if (self.match(&[_]TokenType{ .BANG, .MINUS })) {
            const operator = self.previous();
            var right_exp = try self.unary();
            return Expr{ .unary = .{ .right = &right_exp, .operator = operator } };
        }

        return try self.primary();
    }

    fn factor(self: *Self) Error!Expr {
        var expr = try self.unary();

        while (self.match(&[_]TokenType{ .SLASH, .STAR })) {
            const operator = self.previous();
            var right = try self.unary();
            const new_expr = Expr{ .binary = .{ .left = &expr, .operator = operator, .right = &right } };
            return new_expr;
        }

        return expr;
    }

    fn term(self: *Self) Error!Expr {
        var expr = try self.factor();

        while (self.match(&[_]TokenType{ .MINUS, .PLUS })) {
            const operator = self.previous();
            var right = try self.factor();
            const new_expr = Expr{ .binary = .{ .left = &expr, .operator = operator, .right = &right } };
            return new_expr;
        }

        return expr;
    }

    fn comparison(self: *Self) Error!Expr {
        var expr = try self.term();

        while (self.match(&[_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            var right = try self.term();
            const new_expr = Expr{ .binary = .{ .left = &expr, .operator = operator, .right = &right } };
            return new_expr;
        }

        return expr;
    }

    fn equality(self: *Self) Error!Expr {
        var expr = try self.comparison();

        while (self.match(&[_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = self.previous();
            var right = try self.comparison();
            const new_expr = Expr{ .binary = .{ .left = &expr, .operator = operator, .right = &right } };
            return new_expr;
        }

        return expr;
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

    fn parse(self: *Self) Error!Expr {
        return try self.expression();
    }

    fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errors.deinit();
    }
};

test "Testing the Parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ "apple"==(100 - 10);
    ;

    var lexer = try scanner.Scanner.init(allocator, input);
    try lexer.scan_tokens();

    var parser = Parser.init(allocator, lexer.tokens);
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
    const expected = "(== apple (group (- 100 10)))";
    try std.testing.expect(std.mem.eql(u8, got, expected));
}
