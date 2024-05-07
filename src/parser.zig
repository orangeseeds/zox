const std = @import("std");
const expect = std.testing.expect;
const scanner = @import("scanner.zig");
const expression_z = @import("expression.zig");
const Expr = expression_z.Expr;
const Token = scanner.Token;
const Literal = scanner.Literal;
const TokenType = scanner.TokenType;

const Errors = error{
    ParserError,
};

const ParserError = struct {
    line: u32,
    message: []const u8,
    fn init(line: u32, msg: []const u8) ParserError {
        return ParserError{
            .line = line,
            .message = msg,
        };
    }
};

const Parser = struct {
    const Self = @This();
    tokens: std.ArrayList(Token),
    current: u32 = 0,
    errors: std.ArrayList(ParserError),
    has_error: bool = false,

    fn init(tokens: std.ArrayList(Token)) Self {
        return Self{ .tokens = tokens };
    }

    fn peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn at_end(self: *Self) bool {
        return self.peek().token_type == .EOF;
    }

    fn advance(self: *Self) Token {
        if (!self.at_end()) self.current += 1;
        return self.previous();
    }

    fn check(self: Self, token_type: TokenType) bool {
        if (self.at_end()) return false;
        return self.peek().token_type == token_type;
    }

    fn match(self: *Self, token_types: []const TokenType) bool {
        for (token_types) |token_type| {
            if (self.check(token_type)) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) return self.advance();
        self.push_error(self.peek(), message);
        return error.ParserError;
    }

    fn push_error(self: *Self, token: Token, message: []const u8) void {
        self.has_error = true;
        try self.errors.append(ParserError.init(token.line_num, message));
        // TODO: handler error properly https://craftinginterpreters.com/parsing-expressions.html#Entering panic mode
    }

    fn primary(self: *Self) !Expr {
        if (self.match(.FALSE)) return Expr{ .literal = .{ .value = .False } };
        if (self.match(.TRUE)) return Expr{ .literal = .{ .value = .True } };
        if (self.match(.NIL)) return Expr{ .literal = .{ .value = .Nil } };

        if (self.match([]const TokenType{ .NUMBER, .STRING })) {
            return Expr{ .literal = .{ .value = self.previous().literal } };
        }

        if (self.match(.LEFT_PAREN)) {
            const expr: Expr = undefined; // TODO: call self.expression() here

            // consume the expr before RIGHT_PAREN, if not RIGHT_PARAM then Error.
            try self.consume(.RIGHT_PAREN, "Expected ')' after the expression.");
            return Expr{ .group = .{ .expression = expr } };
        }
    }

    fn unary(self: *Self) !Expr {
        if (self.match([_]TokenType{ .BANG, .MINUS })) {
            const operator = try self.previous();
            const right_exp = try unary();
            return Expr{ .unary = .{ .right = right_exp, .operator = operator } };
        }

        return self.primary();
    }

    fn factor(self: *Self) !Expr {
        var expr = try self.unary();

        while (self.match([]TokenType{ .SLASH, .STAR })) {
            const operator = self.previous();
            const right = try self.unary();
            expr = Expr{ .binary = .{ .left = expr, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn term(self: *Self) !Expr {
        var expr = try self.factor();

        while (self.match([]TokenType{ .MINUS, .PLUS })) {
            const operator = self.previous();
            const right = try self.factor();
            expr = Expr{ .binary = .{ .left = expr, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn comparison(self: *Self) !Expr {
        var expr = try self.term();

        while (self.match([]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.term();
            expr = Expr{ .binary = .{ .left = expr, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn equality(self: *Self) Expr {
        var expr = try self.comparison();

        while (self.match([]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.comparison();
            expr = Expr{ .binary = .{ .left = expr, .operator = operator, .right = right } };
        }

        return expr;
    }

    fn expression(self: *Self) Expr {
        return self.equality();
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
            }
        }

        _ = self.advance();
    }
};
