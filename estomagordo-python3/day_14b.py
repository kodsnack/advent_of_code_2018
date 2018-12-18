def solve(pattern):
    patstring = str(pattern)
    patlen = len(patstring)
    patlist = list(map(int, patstring))
    recipes = [3, 7]
    elf_a = 0
    elf_b = 1

    removed = 0

    while True:
        rec_a = recipes[elf_a]
        rec_b = recipes[elf_b]

        val = rec_a + rec_b
        
        if val > 9:
            recipes.append(1)
            recipes.append(val - 10)
        else:
            recipes.append(val)

        elf_a += 1 + rec_a
        while elf_a >= len(recipes):
            elf_a -= len(recipes)
        
        elf_b += 1 + rec_b
        while elf_b >= len(recipes):
            elf_b -= len(recipes)
        
        if patlist == recipes[-patlen-1:-1] or patlist == recipes[-patlen:]:
            return len(recipes) - patlen + removed - (1 if patlist == recipes[-patlen-1:-1] else 0)


def read_and_solve():
    return solve(681901)

if __name__ == '__main__':
    print(read_and_solve())
