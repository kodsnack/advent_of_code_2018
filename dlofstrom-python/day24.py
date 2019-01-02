import sys
import re
from itertools import product, combinations
from math import ceil

# Read input
input = sys.stdin.read().strip()
input = input.split('\n\n')


def select(enemies, attack_power, attack_type):
    best_attack = 0
    chosen = 0
    enemy_power = 0
    enemy_initiative = 0
    for i in range(len(enemies)):
        damage = attack_power
        if attack_type in enemies[i]['immune']:
            damage = 0
        elif attack_type in enemies[i]['weak']:
            damage *= 2

        if damage > best_attack:
            best_attack = damage
            chosen = i
            enemy_power = enemies[i]['units']*enemies[i]['attack'][1]
            enemy_initiative = enemies[i]['initiative']
        elif damage == best_attack:
            if enemy_power < enemies[i]['units']*enemies[i]['attack'][1]:
                best_attack = damage
                chosen = i
                enemy_power = enemies[i]['units']*enemies[i]['attack'][1]
                enemy_initiative = enemies[i]['initiative']
            elif enemy_power == enemies[i]['units']*enemies[i]['attack'][1]:
                if enemy_initiative < enemies[i]['initiative']:
                    best_attack = damage
                    chosen = i
                    enemy_power = enemies[i]['units']*enemies[i]['attack'][1]
                    enemy_initiative = enemies[i]['initiative']
    if best_attack != 0:
        return enemies.pop(chosen), best_attack
    else:
        return {},0



def solve(boost):
    # Recreate armies
    opposite = {'Immune System':'Infection', 'Infection':'Immune System'}
    armies = []
    for army in input:
        lines = army.split('\n')
        army_type = lines.pop(0)[:-1]
        count = 1
        for line in lines:
            #print(line)
            numbers = re.findall('-?\d+',line)
            numbers = [int(x) for x in numbers]
            attributes = re.findall('(?<=\()[a-z ;,]+(?=\))',line)
            if attributes:
                attributes = attributes.pop()
                attributes = attributes.split('; ')
                attributes = [a.split(' to ') for a in attributes]
                attributes = {a[0]:a[1].split(', ') for a in attributes}
            else:
                attributes = {}
                
            attack = re.findall('(?<=\d )[a-z]+(?= damage)',line).pop()
            
            attributes['units'] = numbers[0]
            attributes['hit points'] = numbers[1]

            if army_type == 'Immune System':
                numbers[2] += boost
            
            attributes['attack'] = (attack,numbers[2])
            attributes['initiative'] = numbers[3]
    
            attributes['type'] = army_type
            attributes['enemy'] = opposite[army_type]
    
            attributes['number'] = count
            #print(attributes)
    
            if 'weak' not in attributes:
                attributes['weak'] = []
    
            if 'immune' not in attributes:
                attributes['immune'] = []
            
            armies.append(attributes)
            count += 1

    # Solve
    round = 0
    while True:
        health = sum([x['units'] for x in armies])
        immune_system = sorted([x for x in armies if x['type'] == 'Immune System' and x['initiative'] > 0], key=lambda d: d['initiative'])
        infection = sorted([x for x in armies if x['type'] == 'Infection' and x['initiative'] > 0], key=lambda d: d['initiative'])
    
        if not immune_system or not infection:
            break
        
        attacking_power = [(x['units']*x['attack'][1],x['initiative']) for x in armies]
        for s in sorted(attacking_power, reverse=True):
            a = armies[attacking_power.index(s)]
                    
            chosen = {}
            a['chosen'] = (chosen,0)
            if a['type'] == 'Immune System':
                if infection:
                    attack_type, damage = a['attack']
                    a['chosen'] = select(infection, damage*a['units'], attack_type)
                    
            else:
                if immune_system:
                    attack_type, damage = a['attack']
                    a['chosen'] = select(immune_system, damage*a['units'], attack_type)
                    
        initiatives = [x['initiative'] for x in armies if x['initiative'] > 0]
        for initiative in sorted(initiatives, reverse=True):
            a = armies[initiatives.index(initiative)]
            if 'chosen' in a and a['chosen'][0]:
                chosen, damage = a['chosen']
    
                damage = a['units']*a['attack'][1]
                if a['attack'][0] in chosen['immune']:
                    damage = 0
                elif a['attack'][0] in chosen['weak']:
                    damage *= 2
                            
                if chosen and chosen in armies:
                    #print(chosen['units'], chosen['hit points'], damage)
                    killed = chosen['units'] - int((chosen['units']*chosen['hit points'] - damage) / chosen['hit points'] + 1)
                    #print(a['type'], a['number'], damage, chosen['type'], chosen['number'], killed)
                    if killed > 0:
                        chosen['units'] -= killed
                    if chosen['units'] <= 0:
                        armies[armies.index(chosen)] = {'type':'dead'}#, 'units':0, 'attack':('',0), 'initiative':-1,'number':0}
    
        armies = [a for a in armies if a['type'] != 'dead']
                        
        round += 1
        
        # Someone needs to win
        if sum([x['units'] for x in armies]) == health:
            return 'No Winner',sum([a['units'] for a in armies if a['type'] == 'Immune System']), sum([a['units'] for a in armies if a['type'] == 'Infection'])
    
    return armies[0]['type'],sum([a['units'] for a in armies if a['type'] == 'Immune System']), sum([a['units'] for a in armies if a['type'] == 'Infection'])


winner, isleft, ileft = solve(0)
print('Part 1:', ileft)

b = 0
while winner != 'Immune System':
    b += 1
    winner, isleft, ileft = solve(b)
print('Part 2:', isleft)
