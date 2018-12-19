
from utils import operations, read_input


if __name__ == "__main__":
    bound, instructions = read_input()
    pointer = 0
    registry = [0]*6
    registry[0] = 1

    while pointer < len(instructions):
        registry[bound] = pointer
        instr = instructions[pointer]
        operation = operations[instr[0]]
        registry = operation(registry, instr[1])

        if pointer == 3:
            for x in range(1, registry[2]+1):
                registry[1] = x
                if registry[2] % registry[1] == 0:
                    registry[0] += registry[2] // registry[1]
            break
        pointer = registry[bound]
        pointer += 1
    print(registry)
