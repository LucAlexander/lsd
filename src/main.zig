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
	for (tokens.items) |token| {
		std.debug.print("{s} ", .{token.text});
	}
	std.debug.print("\n", .{});
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
	var rules = std.StringHashMap(Buffer(Judgement)).init(mem.*);
	while (i < tokens.len){
		const token = tokens[i];
		if (token.tag == .IDENTIFIER){
			const pause = i;
			const judgement = parse_judgement(mem, tokens, &i, .SEMI) catch {
				i = pause;
				parse_call(mem, &rules, tokens, &i) catch {
					i = pause+1;
					continue;
				};
				continue;
			};
			switch (judgement){
				.bind => {
					add_judgement(mem, &rules, judgement);
					continue;
				},
				.unbind => {
					remove_judgement(&rules, judgement);
					continue;
				}
			}
		}
		i += 1;
	}
	return rules;
}

pub fn add_judgement(mem: *const std.mem.Allocator, rules: *std.StringHashMap(Buffer(Judgement)), judgement: Judgement) void {
	//TODO semantic checks
	for (judgement.bind.left.items)|alt| {
		var lone_alt_judgement = Judgement {
			.bind = .{
				.right = judgement.bind.right,
				.body = judgement.bind.body,
				.left = Buffer(Alt).init(mem.*)
			}
		};
		lone_alt_judgement.bind.left.append(alt)
			catch unreachable;
		if (rules.get(alt.name.text)) |_| {
			var buffer = rules.get(alt.name.text).?;
			buffer.append(lone_alt_judgement)
				catch unreachable;
			continue;
		}
		var buffer = Buffer(Judgement).init(mem.*);
		buffer.append(lone_alt_judgement)
			catch unreachable;
		rules.put(alt.name.text, buffer)
			catch unreachable;
	}
}

pub fn remove_judgement(rules: *std.StringHashMap(Buffer(Judgement)), judgement: Judgement) void {
	outer: for (judgement.unbind.items) |alt| {
		if (rules.get(alt.name.text)) |_| {
			var buffer = rules.get(alt.name.text).?;
			for (buffer.items, 0..) |overload, k| {
				if (overload == .bind){
					for (overload.bind.left.items)|overload_alt|{
						if (arg_types_match(alt, overload_alt)){
							_ = buffer.swapRemove(k);
							continue :outer;
						}
					}
				}
			}
			_ = rules.remove(alt.name.text);
		}
	}
}

pub fn invoke(mem: *const std.mem.Allocator, rules: *std.StringHashMap(Buffer(Judgement)), application: AppliedJudgement, rule: Judgement, generic_map: std.StringHashMap(Token)) void {
	for (rule.bind.body.items) |generation| {
		const generated = apply_generics(mem, generation.*, generic_map);
		switch (generated){
			.bind => {
				add_judgement(mem, rules, generated);
			},
			.unbind => {
				remove_judgement(rules, generated);
			}
		}
	}
	if (rule.bind.right) |right| {
		if (right.items.len != 1){
			return;
		}
		const term = right.items[0];
		var new_application = AppliedJudgement{
			.name=term.name,
			.left=rule.bind.left,
			.value=application.value,
			.args = Buffer(*AppliedJudgement).init(mem.*)
		};
		for (term.args.items) |arg| {
			for (application.args.items) |candidate| {
				if (arg.bind.right) |hasname| {
					if (hasname.items.len != 0){
						//TODO error case
						return;
					}
					const argname = hasname.items[0].name;
					if (std.mem.eql(u8, argname.text, candidate.name.text)){
						if (prove_constraint(rules, arg.bind.left, candidate.left) == false){
							//TODO this is a legit error case
							return;
						}
						new_application.args.append(candidate)
							catch unreachable;
					}
				}
			}
		}
		if (rules.get(term.name.text)) |node| {
			for (node.items) |edge| {
				std.debug.assert(edge.bind.left.items.len == 1);
				std.debug.assert(new_application.left.items.len == 1);
				if (compare_args_for_invocation(mem, rules, new_application.left.items[0], edge.bind.left.items[0])) |new_generic_map| {
					invoke(mem, rules, new_application, edge, new_generic_map);
					return;
				}
			}
		}
		//TODO save for later invocation?
	}
}

pub fn compare_args_for_invocation(mem: *const std.mem.Allocator, rules: *std.StringHashMap(Buffer(Judgement)), left: Alt, right: Alt) ?std.StringHashMap(Token) {
	var generic_map = std.StringHashMap(Token).init(mem.*);
	if (left.args.items.len != right.args.items.len){
		return null;
	}
	for (left.args.items, right.args.items) |arg, candidate| {
		std.debug.assert(arg.* == .bind);
		std.debug.assert(candidate.* == .bind);
		const proves = prove_constraint(rules, arg.bind.left, candidate.bind.left);
		for (arg.bind.left.items, candidate.bind.left.items) |arg_left, candidate_left| {
			if (std.mem.eql(u8, arg_left.name.text, candidate_left.name.text) == false){
				if (rules.get(candidate_left.name.text)) |_| {
					generic_map.put(candidate_left.name.text, arg_left.name)
						catch unreachable;
					continue;
				}
				if (proves == false){
					return null;
				}
				continue;
			}
			if (compare_args_for_invocation(mem, rules, arg_left, candidate_left)) |map| {
				var it = map.iterator();
				while (it.next()) |entry| {
					generic_map.put(entry.key_ptr.*, entry.value_ptr.*)
						catch unreachable;
				}
				continue;
			}
			return null;
		}
	}
	return generic_map;
}

pub fn prove_constraint(rules: *std.StringHashMap(Buffer(Judgement)), left: Side, right: Side) bool {
	for (left.items) |lalt| {
		for (right.items) |ralt| {
			if (std.mem.eql(u8, lalt.name.text, ralt.name.text)){
				return true;
			}
		}
		if (rules.get(lalt.name.text)) |node| {
			for (node.items) |edge| {
				std.debug.assert(edge == .bind);
				if (edge.bind.right) |impl_right| {
					if (compare_args_for_proof(lalt, edge.bind.left.items[0])){
						if (prove_constraint(rules, impl_right, right)) {
							return true;
						}
					}
				}
			}
		}
	}
	return false;
}

pub fn compare_args_for_proof(left: Alt, right: Alt) bool {
	if (left.args.items.len > right.args.items.len){
		return false;
	}
	for (left.args.items, right.args.items) |arg, candidate| {
		std.debug.assert(arg.* == .bind);
		std.debug.assert(candidate.* == .bind);
		for (arg.bind.left.items, candidate.bind.left.items) |arg_left, candidate_left| {
			if (std.mem.eql(u8, arg_left.name.text, candidate_left.name.text) == false){
				return false;
			}
			if (compare_args_for_proof(arg_left, candidate_left) == false){
				return false;
			}
		}
	}
	return true;
}

pub fn apply_generics(mem: *const std.mem.Allocator, template: Judgement, generic_map: std.StringHashMap(Token)) Judgement {
	switch (template){
		.bind => {
			var generated = Judgement{
				.bind=.{
					.left = Buffer(Alt).init(mem.*),
					.body = Buffer(*Judgement).init(mem.*),
					.right = null
				}
			};
			for (template.bind.left.items) |lalt| {
				var new = Alt{
					.name = lalt.name,
					.args = Buffer(*Judgement).init(mem.*)
				};
				if (generic_map.get(new.name.text)) |replacement| {
					new.name = replacement;
				}
				for (lalt.args.items) |target| {
					const new_arg = apply_generics(mem, target.*, generic_map);
					const loc = mem.create(Judgement)
						catch unreachable;
					loc.* = new_arg;
					new.args.append(loc)
						catch unreachable;
				}
				generated.bind.left.append(new)
					catch unreachable;
			}
			for (template.bind.body.items) |subjudge| {
				const new_judge = apply_generics(mem, subjudge.*, generic_map);
				const loc = mem.create(Judgement)
					catch unreachable;
				loc.* = new_judge;
				generated.bind.body.append(loc)
					catch unreachable;
			}
			if (template.bind.right) |right| {
				generated.bind.right = Buffer(Alt).init(mem.*);
				for (right.items) |ralt| {
					var new = Alt{
						.name = ralt.name,
						.args = Buffer(*Judgement).init(mem.*)
					};
					if (generic_map.get(new.name.text)) |replacement| {
						new.name = replacement;
					}
					for (ralt.args.items) |target| {
						const new_arg = apply_generics(mem, target.*, generic_map);
						const loc = mem.create(Judgement)
							catch unreachable;
						loc.* = new_arg;
						new.args.append(loc)
							catch unreachable;
					}
					generated.bind.right.?.append(new)
						catch unreachable;
				}
			}
			return generated;
		},
		.unbind => {
			var generated = Judgement{
				.unbind=Buffer(Alt).init(mem.*)
			};
			for (template.unbind.items) |alt| {
				var new = Alt{
					.name=alt.name,
					.args=Buffer(*Judgement).init(mem.*)
				};
				if (generic_map.get(new.name.text)) |replacement| {
					new.name = replacement;
				}
				for (alt.args.items) |target| {
					const new_arg = apply_generics(mem, target.*, generic_map);
					const loc = mem.create(Judgement)
						catch unreachable;
					loc.* = new_arg;
					new.args.append(loc)
						catch unreachable;
				}
				generated.unbind.append(new)
					catch unreachable;
			}
			return generated;
		}
	}
}

pub fn arg_types_match(a: Alt, b: Alt) bool {
	if (a.args.items.len != b.args.items.len){
		return false;
	}
	for (a.args.items, b.args.items) |real, candidate| {
		if (real.* == .bind and candidate.* == .bind){
			if (real.bind.right) |real_right| {
				if (candidate.bind.right) |candidate_right| {
					if (real_right.items.len == candidate_right.items.len){
						for (real_right.items, candidate_right.items) |rri, cri| {
							if (std.mem.eql(u8, rri.name.text, cri.name.text)){
								continue;
							}
							return false;
						}
						continue;
					}
				}
			}
		}
		return false;
	}
	return true; 
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
			i.* += 1;
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
			i.* += 1;
			const rule = try parse_judgement(mem, tokens, i, .CLOSE);
			const loc = mem.create(Judgement)
				catch unreachable;
			loc.* = rule;
			alt.args.append(loc)
				catch unreachable;
		}
		if (tokens[i.*].tag == end){
			i.* += 1;
			side.append(alt)
				catch unreachable;
			return side;
		}
		if (tokens[i.*].tag == .PIPE){
			i.* += 1;
			side.append(alt)
				catch unreachable;
			continue;
		}
	}
	return Error.BrokenParse;
}

pub fn parse_call(mem: *const std.mem.Allocator, rules: *std.StringHashMap(Buffer(Judgement)), tokens: []Token, i: *u64) Error!void{
	const pause = i.*;
	if (rules.get(tokens[i.*].text)) |entry| {
		for (entry.items) |rule| {
			i.* = pause;
			var generic_map = std.StringHashMap(Token).init(mem.*);
			if (parse_judgement_call(mem, rules, rule, tokens, i, true, &generic_map)) |application| {
				invoke(mem, rules, application, rule, generic_map);
			}
		}
	}
	return Error.BrokenParse;
}

const AppliedJudgement = struct {
	name: Token,
	left: Side,
	value: []Token,
	args: Buffer(*AppliedJudgement)
};

pub fn parse_judgement_call(mem: *const std.mem.Allocator, rules: *std.StringHashMap(Buffer(Judgement)), arg: Judgement, tokens: []Token, i: *u64, firsthand: bool, generic_map: *std.StringHashMap(Token)) ?AppliedJudgement {
	std.debug.assert(arg == .bind);
	if (arg.bind.right) |right| {
		std.debug.assert(right.items.len == 1);
	}
	outer: for (arg.bind.left.items) |internal_alt| {
		if (firsthand == false){
			if (rules.get(internal_alt.name.text)) |buffer| {
				for (buffer.items) |entry| {
					if (parse_judgement_call(mem, rules, entry, tokens, i, true, generic_map)) |success| {
						return success;
					}
				}
			}
			var applied = AppliedJudgement {
				.name=arg.bind.right.?.items[0].name,
				.left=arg.bind.left,
				.value = tokens[i.*..i.*+1],
				.args = Buffer(*AppliedJudgement).init(mem.*)
			};
			if (generic_map.get(internal_alt.name.text)) |exists| {
				if (std.mem.eql(u8, applied.value[0].text, exists.text) == false){
					return null;
				}
			}
			else{
				generic_map.put(internal_alt.name.text, tokens[i.*])
					catch unreachable;
			}
			i.* += 1;
			for (internal_alt.args.items) |argument| {
				if (parse_judgement_call(mem, rules, argument.*, tokens, i, false, generic_map)) |application| {
					const loc = mem.create(AppliedJudgement)
						catch unreachable;
					loc.* = application;
					applied.args.append(loc)
						catch unreachable;
					 continue;
				}
				continue :outer;
			}
			return applied;
		}
		if (std.mem.eql(u8, tokens[i.*].text, internal_alt.name.text)){
			var applied = AppliedJudgement {
				.name=arg.bind.right.?.items[0].name,
				.left = arg.bind.left,
				.value=undefined,
				.args = Buffer(*AppliedJudgement).init(mem.*)
			};
			const start = i.*;
			i.* += 1;
			for (internal_alt.args.items) |argument| {
				if (parse_judgement_call(mem, rules, argument.*, tokens, i, false, generic_map)) |application| {
					const loc = mem.create(AppliedJudgement)
						catch unreachable;
					loc.* = application;
					applied.args.append(loc)
						catch unreachable;
					 continue;
				}
				continue :outer;
			}
			applied.value = tokens[start .. i.*];
			return applied;
		}
	}
	return null;
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
