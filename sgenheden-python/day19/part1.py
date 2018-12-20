
from utils import operations, read_input


if __name__ == "__main__":
    bound, instructions = read_input()
    pointer = 0
    registry = [0]*6

    while pointer < len(instructions):
        registry[bound] = pointer
        instr = instructions[pointer]
        operation = operations[instr[0]]
        registry = operation(registry, instr[1])
        pointer = registry[bound]
        pointer += 1
    print(registry)
