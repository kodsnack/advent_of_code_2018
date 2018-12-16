// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;

using IntList = vector<int>;
using Registers = IntList;
using Instruction = IntList;

struct Execution
{
	Registers before;
	IntList instr;
	Registers after;
};

pair<vector<Execution>, vector<Instruction>> parseLines(const vector<string>& lines)
{
	vector<Execution> e;
	vector<Instruction> instrs;
	int i;
	for(i = 0; i < lines.size();)
	{
		auto s = lines[i];
		if(s.substr(0, 7) == "Before:")
		{
			auto before = splitNumbers(lines[i++]);
			before.erase(before.begin());
			auto opcode = splitNumbers(lines[i++]);
			auto after = splitNumbers(lines[i++]);
			after.erase(after.begin());
			e.push_back({before, opcode, after});
			i++;
		} else
		{
			break;
		}
	}
	i += 2;
	for(; i < lines.size(); i++)
	{
		auto instr = splitNumbers(lines[i]);
		instrs.push_back(instr);
	}

	return make_pair(e, instrs);
}

enum Opcode : int
{
	addr,
	addi,
	mulr,
	muli,
	banr,
	bani,
	borr,
	bori,
	setr,
	seti,
	gtir,
	gtri,
	gtrr,
	eqir,
	eqri,
	eqrr
};

Registers execute(Instruction i, const Registers& rin)
{
	auto op = i[0];
	auto a = i[1];
	auto b = i[2];
	auto c = i[3];
	auto r = rin;
	switch(op)
	{
		case addr:
			r[c] = r[a] + r[b];
			break;
		case addi:
			r[c] = r[a] + b;
			break;
		case mulr:
			r[c] = r[a] * r[b];
			break;
		case muli:
			r[c] = r[a] * b;
			break;
		case banr:
			r[c] = r[a] & r[b];
			break;
		case bani:
			r[c] = r[a] & b;
			break;
		case borr:
			r[c] = r[a] | r[b];
			break;
		case bori:
			r[c] = r[a] | b;
			break;
		case setr:
			r[c] = r[a];
			break;
		case seti:
			r[c] = a;
			break;
		case gtir:
			r[c] = a > r[b] ? 1 : 0;
			break;
		case gtri:
			r[c] = r[a] > b ? 1 : 0;
			break;
		case gtrr:
			r[c] = r[a] > r[b] ? 1 : 0;
			break;
		case eqir:
			r[c] = a == r[b] ? 1 : 0;
			break;
		case eqri:
			r[c] = r[a] == b ? 1 : 0;
			break;
		case eqrr:
			r[c] = r[a] == r[b] ? 1 : 0;
			break;
		default:
			break;
	}
	return r;
}

void solve_part1()
{
	auto [parsedInput, program] = parseLines(readLines(string(inputFile)));
	int n = 0;
	for(auto e : parsedInput)
	{
		int m = 0;
		auto instr = e.instr;
		for(int op = 0; op < 16; op++)
		{
			instr[0] = op;
			if(execute(instr, e.before) == e.after)
			{
				m++;
			}
		}
		if(m >= 3)
			n++;
	}
	cout << dayName << " - part 1: " << n << endl;
}

void solve_part2()
{
	auto [parsedInput, program] = parseLines(readLines(string(inputFile)));
	int known = 0;
	array<int, 16> opmap;
	array<unordered_set<int>, 16> candidates;
	for(int i = 0; i < 16; i++)
	{
		for(int j = 0; j < 16; j++)
			candidates[i].insert(j);
	}
	while(known < 16)
	{
		for(auto e : parsedInput)
		{
			auto instr = e.instr;
			for(int op = 0; op < 16; op++)
			{
				instr[0] = op;
				if(execute(instr, e.before) != e.after)
				{
					candidates[e.instr[0]].erase(op);
				}
			}
		}

		for(int i = 0; i < 16; i++)
		{
			if(candidates[i].size() == 1)
			{
				int r = *(candidates[i].begin());
				// i maps to r. r no longer cand for any else
				for(int j = 0; j < 16; j++)
				{
					candidates[j].erase(r);
				}
				opmap[i] = r;
				known++;
			}
		}
	}

	Registers r(4, 0);
	for(int pc = 0; pc < program.size(); ++pc)
	{
		program[pc][0] = opmap[program[pc][0]];
		r = execute(program[pc], r);
	}

	cout << dayName << " - part 2: " << r[0] << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
