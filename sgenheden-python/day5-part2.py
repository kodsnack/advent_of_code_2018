
import string
import fileinput


if __name__ == "__main__":

    reactive = [l+u for l, u in zip(string.ascii_lowercase, string.ascii_uppercase)]
    input_str0 = "".join(line.strip() for line in fileinput.input())

    min_len = len(input_str0)
    min_unit = None
    for unit in reactive[::-1]:
        changed = True
        input_str = input_str0.replace(unit[0], "").replace(unit[1], "")
        start = 0
        while changed:
            changed = False
            for i in range(start, len(input_str)-1):
                curr = input_str[i:i+2]
                curr_rev = curr[::-1]
                if curr in reactive or curr_rev in reactive:
                    changed = True
                    input_str = input_str[:i]+input_str[i+2:]
                    start = i - 1
                    break
        print(f"Removing {unit} produced polymer of length {len(input_str)}")
        if len(input_str) < min_len:
            min_len = len(input_str)
            min_unit = unit
    print(f"\nRemoving {min_unit} produced polymer of length {min_len}")
