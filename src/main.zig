const std = @import("std");
const Buffer = std.ArrayList;

const TOKEN = enum(u64){
	IDENTIFIER,
	TURN,
	OPEN,
	CLOSE,
	PIPE,
	SEMI,
	UNBIND,
	ESCAPE
};

const Token = struct {
	tag: TOKEN,
	pos: u64,
	text: []u8
};

pub fn main() void {
	const mem = std.heap.page_allocator;
	const filename = "test.lsd";
	var infile = std.fs.cwd().openFile(filename, .{}) catch {
		std.debug.print("File not found: {s}\n", .{filename});
		return;
	};
	defer infile.close();
	const stat = infile.stat() catch {
		std.debug.print("Errored file stat: {s}\n", .{filename});
		return;
	};
	const contents = infile.readToEndAlloc(mem, stat.size+1) catch {
		std.debug.print("Error reading file: {s}\n", .{filename});
		return;
	};
	defer mem.free(contents);
	const tokens = tokenize(&mem, contents);
	const graph = parse_file(&mem, tokens.items);
	show_graph(graph);
}

pub fn tokenize(mem: *const std.mem.Allocator, text: []u8) Buffer(Token) {
	var i:u64 = 0;
	var tokens = Buffer(Token).init(mem.*);
	while (i < text.len){
		var c = text[i];
		while (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
			i += 1;
			if (i >= text.len){
				return tokens;
			}
			c = text[i];
		}
		if (c == '|'){
			c = text[i+1];
			if (c == '-'){
				tokens.append(Token{
					.pos = i,
					.text = text[i..i+2],
					.tag = .TURN
				}) catch unreachable;
				i += 2;
				continue;
			}
			else if (c == '/'){
				c = text[i+2];
				if (c == '-'){
					tokens.append(Token{
						.pos = i,
						.text = text[i..i+3],
						.tag = .UNBIND
					}) catch unreachable;
					i += 3;
					continue;
				}
			}
			tokens.append(Token{
				.pos = i,
				.text = text[i..i+1],
				.tag = .PIPE
			}) catch unreachable;
			i += 1;
			continue;
		}
		else if (c == '\\'){
			tokens.append(Token{
				.pos = i,
				.text = text[i..i+1],
				.tag = .ESCAPE
			}) catch unreachable;
			i += 1;
			continue;
		}
		else if (c == ';'){
			tokens.append(Token{
				.pos = i,
				.text = text[i..i+1],
				.tag = .SEMI
			}) catch unreachable;
			i += 1;
			continue;
		}
		else if (c == '('){
			tokens.append(Token{
				.pos = i,
				.text = text[i..i+1],
				.tag = .OPEN
			}) catch unreachable;
			i += 1;
			continue;
		}
		else if (c == ')'){
			tokens.append(Token{
				.pos = i,
				.text = text[i..i+1],
				.tag = .CLOSE
			}) catch unreachable;
			i += 1;
			continue;
		}
		if (std.ascii.isAlphanumeric(c) or c == '_'){
			const start = i;
			while (std.ascii.isAlphanumeric(c) or c == '_'){
				i += 1;
				if (i == text.len){
					return tokens;
				}
				c = text[i];
			}
			tokens.append(Token{
				.pos = start,
				.text = text[start .. i],
				.tag = .IDENTIFIER
			}) catch unreachable;
			continue;
		}
		tokens.append(Token{
			.pos = i,
			.text = text[i..i+1],
			.tag = .IDENTIFIER
		}) catch unreachable;
		i += 1;
	}
	return tokens;
}

const Side = Buffer(Alt);

const Alt = struct {
	name: Token,
	args: Buffer(*Judgement)
};

const Judgement = union(enum){
	bind: struct {
		left: Side,
		right: ?Side,
		body: Buffer(*Judgement),
	},
	unbind: Side
};

const Error = error {
	BrokenParse
};

pub fn parse_file(mem: *const std.mem.Allocator, tokens: []Token) std.StringHashMap(Buffer(Judgement)){
	var i: u64 = 0;
	const rules = std.StringHashMap(Buffer(Judgement)).init(mem.*);
	while (i < tokens.len){
		const token = tokens[i];
		if (token.tag == .IDENTIFIER){
			const pause = i;
			const judgement = parse_judgement(mem, tokens, &i, .SEMI) catch {
				i = pause;
				_ = parse_call(mem, rules, tokens, &i) catch {
					i = pause+1;
					continue;
				};
				//TODO run call
				continue;
			};
			switch (judgement){
				.bind => {
					//TODO rules.put(judgement.bind.left_name.text, judgement)
						//catch unreachable;
					//TODO semantic checks
				},
				.unbind => {
					// if (rules.get(judgement.unbind.name)) |_| {
						// rules.remove(judgement.unbind.name)
							// catch unreachable;
						// //TODO is it possible to remove remove based on more specific descriminant criteria?
					// }
				}
			}
		}
		i += 1;
	}
	return rules;
}

pub fn parse_judgement(mem: *const std.mem.Allocator, tokens: []Token, i: *u64, end: TOKEN) Error!Judgement {
	const left = try parse_side(mem, tokens, i, .TURN);
	if (tokens[i.*].tag == .UNBIND){
		return Judgement {
			.unbind = left
		};
	}
	var rule = Judgement{
		.bind = .{
			.left=left,
			.right=null,
			.body = Buffer(*Judgement).init(mem.*)
		}
	};
	while (true){
		if (tokens[i.*].tag == end){
			return rule;
		}
		const pause = i.*;
		const candidate = parse_side(mem, tokens, i, end) catch {
			i.* = pause;
			const subrule = try parse_judgement(mem, tokens, i, .SEMI);
			const slot = mem.create(Judgement)
				catch unreachable;
			slot.* = subrule;
			rule.bind.body.append(slot)
				catch unreachable;
			continue;
		};
		rule.bind.right = candidate;
		return rule;
	}
	unreachable;
}

pub fn parse_side(mem: *const std.mem.Allocator, tokens: []Token, i: *u64, end: TOKEN) Error!Side {
	var side = Buffer(Alt).init(mem.*);
	while (true){
		if (tokens[i.*].tag != .IDENTIFIER){
			return Error.BrokenParse;
		}
		var alt = Alt{
			.name=tokens[i.*],
			.args=Buffer(*Judgement).init(mem.*)
		};
		i.* += 1;
		while (tokens[i.*].tag == .OPEN){
			const rule = try parse_judgement(mem, tokens, i, .CLOSE);
			const loc = mem.create(Judgement)
				catch unreachable;
			loc.* = rule;
			alt.args.append(loc)
				catch unreachable;
		}
		if (tokens[i.*].tag == end){
			side.append(alt)
				catch unreachable;
			return side;
		}
		if (tokens[i.*].tag == .PIPE){
			side.append(alt)
				catch unreachable;
			continue;
		}
	}
	return Error.BrokenParse;
}

pub fn parse_call(_: *const std.mem.Allocator, _: std.StringHashMap(Buffer(Judgement)), _: []Token, _: *u64) Error!Judgement {
	return Error.BrokenParse;
}

pub fn show_graph(rules: std.StringHashMap(Buffer(Judgement))) void {
	var it = rules.iterator();
	while (it.next()) |pair| {
		for (pair.value_ptr.items) |rule| {
			show_judgement(rule);
		}
	}
} 

pub fn show_judgement(rule: Judgement) void {
	switch (rule){
		.bind => {
			show_side(rule.bind.left);
			std.debug.print(" |- ", .{});
			for (rule.bind.body.items)|line| {
				show_judgement(line.*);
			}
			if (rule.bind.right) |right|{
				show_side(right);
			}
			std.debug.print(";\n", .{});
		},
		.unbind => {
			show_side(rule.unbind);
			std.debug.print("|/-\n", .{});
		}
	}
}

pub fn show_side(side: Side) void {
	for (side.items, 0..) |alt, i| {
		if (i != 0){
			std.debug.print("| ", .{});
		}
		show_alt(alt);
	}
}

pub fn show_alt(alt: Alt) void {
	std.debug.print("{s} ", .{alt.name.text});
	for (alt.args.items)|rule| {
		std.debug.print("(", .{});
		show_judgement(rule.*);
		std.debug.print(") ", .{});
	}
}
