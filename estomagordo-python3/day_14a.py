def solve(recipe_count):    
    recipes = [3, 7]
    elf_a = 0
    elf_b = 1

    while True:
        rec_a = recipes[elf_a]
        rec_b = recipes[elf_b]

        val = rec_a + rec_b

        if val > 9:
            recipes.append(1)
            if len(recipes) == recipe_count + 10:
                break
            recipes.append(val - 10)
            if len(recipes) == recipe_count + 10:
                break
        else:
            recipes.append(val)
            if len(recipes) == recipe_count + 10:
                break

        elf_a += 1 + rec_a
        while elf_a >= len(recipes):
            elf_a -= len(recipes)
        
        elf_b += 1 + rec_b
        while elf_b >= len(recipes):
            elf_b -= len(recipes)

    return ''.join(str(r) for r in recipes[-10:])


def read_and_solve():
    return solve(681901)

if __name__ == '__main__':
    print(read_and_solve())
