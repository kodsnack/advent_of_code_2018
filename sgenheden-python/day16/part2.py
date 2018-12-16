
from utils import read_input, operations


def find_opcode(befores, instructions, afters, op_map):

    for be, ins, aft in zip(befores, instructions, afters):
        nmatches = 0
        matched_op = None
        for op, func in operations.items():
            if op in op_map:
                continue
            try:
                res = func(be, ins)
                if res == aft:
                    nmatches += 1
                    matched_op = op
            except IndexError:
                pass
        if nmatches == 1:
            op_map[matched_op] = ins[0]
    if len(op_map) != len(operations):
        find_opcode(befores, instructions, afters, op_map)


if __name__ == "__main__":

    befores, instructions, afters, examples = read_input()
    op_map = {}
    find_opcode(befores, instructions, afters, op_map)
    op_list = [None]*len(op_map)
    for op, nr in op_map.items():
        op_list[nr] = op

    registry = [0, 0, 0, 0]
    for example in examples:
        op = op_list[example[0]]
        registry = operations[op](registry, example)
    print(registry)
