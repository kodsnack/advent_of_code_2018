
import string
import fileinput


if __name__ == "__main__":

    reactive = [l+u for l, u in zip(string.ascii_lowercase, string.ascii_uppercase)]

    input_str = "".join(line.strip() for line in fileinput.input())

    changed = True
    start = 0
    while changed:
        changed = False
        for i in range(start, len(input_str)-1):
            if input_str[i:i+2] in reactive or input_str[i:i+2][::-1] in reactive:
                changed = True
                input_str = input_str[:i]+input_str[i+2:]
                start = i - 1
                break
    print(f"Len is {len(input_str)}")
