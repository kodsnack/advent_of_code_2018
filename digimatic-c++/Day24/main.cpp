// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <numeric>
#include <regex>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

bool verbose = false;

struct Unit
{
	int hitPoints;
	int attackDamage;
	int attackType;
	string type;
	int initiative;
	vector<string> weakness;
	vector<string> immunities;
};

struct Group
{
	Unit unit;
	int unitCount;
	int effectivePower() const
	{
		return unitCount * unit.attackDamage;
	}
};

struct Army
{
	string name;
	vector<Group> groups;
};

using Armies = vector<Army>;

int parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	return n;
}

Armies parseLines(const vector<string>& lines)
{
	regex re(R"raw(^(\d+)\D+(\d+)[\w\s]+((\([^)]+\))?)\D+(\d+)\s(\w+)\D+(\d+))raw");
	Armies armies;
	int currentArmy = -1;
	vector<int> parsedLines;
	for(auto& line : lines)
	{
		if(line.size() == 0)
			continue;
		else if(line[line.size() - 1] == ':')
		{
			currentArmy++;
			armies.push_back({});
			armies[currentArmy].name = line.substr(0, line.size() - 1);
		} else
		{
			smatch m;
			regex_match(line, m, re);
			Group g;
			g.unitCount = stoi(m[1].str());
			g.unit.hitPoints = stoi(m[2].str());
			if(m[4].matched)
			{
				auto ws = m[3].str();
				ws = ws.substr(1, ws.size() - 2);

				auto matchTypes = [](string ws) {
					regex re2(R"raw(((weak|immune) to (\w+)(((, (\w+))*))))raw");
					smatch m2;
					regex_search(ws, m2, re2);
					auto weakOrImmune = m2[2].str();
					vector<string> weakness;
					auto e1 = m2[3];
					weakness.push_back(e1);
					if(m2[4].matched)
					{
						auto rest = m2[4].str();
						while(rest.size() > 0)
						{
							auto e = rest.substr(2);
							auto comma = e.find(',');
							if(comma != string::npos)
							{
								rest = e.substr(comma);
								e = e.substr(0, comma);
							} else
							{
								rest.clear();
							}
							weakness.push_back(e);
						}
					}
					return make_pair(weakOrImmune, weakness);
				};
				auto w1 = matchTypes(ws);
				if(w1.first == "weak")
					g.unit.weakness = w1.second;
				else
					g.unit.immunities = w1.second;

				auto n = ws.find(';');
				if(n != string::npos)
				{
					ws = ws.substr(n + 2);
					w1 = matchTypes(ws);
					if(w1.first == "weak")
						g.unit.weakness = w1.second;
					else
						g.unit.immunities = w1.second;
				}
			}
			g.unit.attackDamage = stoi(m[5].str());
			g.unit.type = m[6].str();
			g.unit.initiative = stoi(m[7].str());
			armies[currentArmy].groups.push_back(g);
		}
	}
	return armies;
}

void print(const vector<Group>& gs)
{
	for(int i = 0; i < gs.size(); i++)
	{
		auto& u = gs[i];
		if(gs[i].unitCount > 0)
			cout << "Group " << (i + 1) << " contains " << u.unitCount << "\n";
	}
	cout << flush;
}

void print(const Armies& as)
{
	for(auto& a : as)
	{
		cout << a.name << ":\n";
		print(a.groups);
	}
}

struct Attack
{
	int attackingArmy;
	int attackingGroupIdx;
	int selectedTargetGroupIdx;
};

bool play(Armies& armies)
{
	bool anyKilled = false;
	if(verbose)
		print(armies);

	vector<Attack> attacks;

	auto computeDamage = [](const Group& attackingGroup, const Group& defendingGroup) {
		bool isWeakFor =
		    find(begin(defendingGroup.unit.weakness), end(defendingGroup.unit.weakness),
		         attackingGroup.unit.type) != end(defendingGroup.unit.weakness);
		auto attackPower = attackingGroup.effectivePower() * (isWeakFor ? 2 : 1);
		return attackPower;
	};
	//
	// Target seletion phase
	//
	for(int attackingArmyIdx = armies.size() - 1; attackingArmyIdx >= 0; attackingArmyIdx--)
	{
		auto& army = armies[attackingArmyIdx];
		auto& groups = army.groups;
		vector<int> groupIds(groups.size());
		iota(begin(groupIds), end(groupIds), 0);

		int defendingArmyIdx = (attackingArmyIdx + 1) % 2;
		auto& defendingArmy = armies[defendingArmyIdx];
		auto& defendingGroups = defendingArmy.groups;

		// target selection order
		sort(begin(groupIds), end(groupIds), [&groups](auto& ai, auto& bi) {
			// In decreasing order of effective power, groups choose their targets; in a tie, the
			// group with the higher initiative chooses first.
			auto& a = groups[ai];
			auto& b = groups[bi];
			auto apower = a.effectivePower();
			auto bpower = b.effectivePower();
			return tie(apower, a.unit.initiative) > tie(bpower, b.unit.initiative);
		});

		unordered_set<int> availabeDefendingGroups;
		for(int i = 0; i < defendingGroups.size(); ++i)
			availabeDefendingGroups.insert(i);

		auto chooseTarget = [&](int attackingGroupIdx) {
			const auto& ag = groups[attackingGroupIdx];
			// vector af candidate groups index, damage
			vector<pair<int, int>> targetCandidates;
			for(auto& targetIndex : availabeDefendingGroups)
			{
				auto& tg = defendingGroups[targetIndex];
				if(tg.unitCount == 0)
					continue; // dead group
				if(find(begin(tg.unit.immunities), end(tg.unit.immunities), ag.unit.type) !=
				   end(tg.unit.immunities))
					continue; // immune
				auto damage = computeDamage(ag, tg);

				targetCandidates.push_back(make_pair(targetIndex, damage));

				//
				if(verbose)
				{
					cout << army.name << " group " << (attackingGroupIdx + 1)
					     << " would deal defending";
					cout << " group " << (targetIndex + 1) << " ";
					cout << damage << " damage\n";
				}
			}
			auto targetIt = max_element(
			    begin(targetCandidates), end(targetCandidates), [&](auto& p1, auto& p2) {
				    // largest damage
				    // then largest effectivePower
				    // then lergest initiative
				    auto p1power = defendingGroups[p1.first].effectivePower();
				    auto p2power = defendingGroups[p2.first].effectivePower();
				    auto p1initiative = defendingGroups[p1.first].unit.initiative;
				    auto p2initiative = defendingGroups[p2.first].unit.initiative;
				    return tie(p1.second, p1power, p1initiative) <
				           tie(p2.second, p2power, p2initiative);
			    });
			if(targetIt != end(targetCandidates))
			{
				availabeDefendingGroups.erase(targetIt->first);
				return *targetIt;
			} else
				return make_pair(-1, 0);
		};

		vector<pair<int, int>> selectedTargetsAndPower(groups.size());
		for(int i = 0; i < groupIds.size(); i++)
		{
			int ai = groupIds[i];
			if(groups[ai].unitCount <= 0)
				continue; // dead group
			auto [targetGroupIdx, damage] = chooseTarget(ai);
			if(targetGroupIdx != -1)
			{
				attacks.push_back(Attack{attackingArmyIdx, ai, targetGroupIdx});
			}
		}
	}

	//
	// Attack phase
	//
	if(verbose)
		cout << "\n";

	// attack order by decreasing attacking group initiative
	sort(begin(attacks), end(attacks), [&](auto& a1, auto& a2) {
		auto& attackArmy1 = armies[a1.attackingArmy];
		auto& attackArmy2 = armies[a2.attackingArmy];
		auto& a = attackArmy1.groups[a1.attackingGroupIdx];
		auto& b = attackArmy2.groups[a2.attackingGroupIdx];
		return a.unit.initiative > b.unit.initiative;
	});
	for(auto& attack : attacks)
	{
		auto& attackArmy = armies[attack.attackingArmy];
		auto& attackGroup = armies[attack.attackingArmy].groups[attack.attackingGroupIdx];
		if(attackGroup.unitCount <= 0)
			continue;
		int defendingArmyIdx = (attack.attackingArmy + 1) % 2;
		auto& targetGroup = armies[defendingArmyIdx].groups[attack.selectedTargetGroupIdx];
		if(targetGroup.unitCount <= 0)
			continue;

		auto damage = computeDamage(attackGroup, targetGroup);
		int numKilled = damage / targetGroup.unit.hitPoints;
		if(numKilled > targetGroup.unitCount)
			numKilled = targetGroup.unitCount;
		if(numKilled > 0)
			anyKilled = true;

		if(verbose)
		{
			cout << attackArmy.name << " group " << (attack.attackingGroupIdx + 1);
			cout << " attacks defending group " << (attack.selectedTargetGroupIdx + 1)
			     << ", killing ";
			cout << numKilled << " units." << endl;
		}

		targetGroup.unitCount -= numKilled;
		if(targetGroup.unitCount < 0)
			targetGroup.unitCount = 0;
	}

	return anyKilled;
}

void solve_part1()
{
	auto armies = parseLines(readLines(string(inputFile)));
	while(play(armies))
	{
		if(verbose)
			cout << endl;
	};

	int n = accumulate(begin(armies), end(armies), 0, [](int v, auto& a) {
		return accumulate(begin(a.groups), end(a.groups), v,
		                  [](int v, auto& g) { return v + g.unitCount; });
	});

	cout << dayName << " - part 1: " << n << endl;
	assert(n == 26753);
}

vector<int> sumArmyUnits(const Armies& armies)
{
	vector<int> units;
	for(auto& a : armies)
	{
		units.push_back(accumulate(begin(a.groups), end(a.groups), 0,
		                           [](int v, auto& g) { return v + g.unitCount; }));
	}
	return units;
}

void solve_part2()
{
	auto armies0 = parseLines(readLines(string(inputFile)));
	int immuneBoost = 1;
	int immuneSystemIndex =
	    distance(begin(armies0), find_if(begin(armies0), end(armies0),
	                                     [](const Army& a) { return a.name == "Immune System"; }));
	int infectionSystemIndex = (immuneSystemIndex + 1) % 2;
	while(true)
	{
		auto armies = armies0;
		for(auto& g : armies[immuneSystemIndex].groups)
			g.unit.attackDamage += immuneBoost;
		while(play(armies))
			;
		auto units = sumArmyUnits(armies);
		auto immuneSystemUnits = units[immuneSystemIndex];
		auto infectionSystemUnits = units[infectionSystemIndex];
		if(immuneSystemUnits > infectionSystemUnits)
		{
			assert(infectionSystemUnits == 0);
			cout << dayName << " - part 2: " << immuneSystemUnits << endl;
			assert(immuneSystemUnits == 1852); // boost 79
			return;
		}
		++immuneBoost;
	}
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
