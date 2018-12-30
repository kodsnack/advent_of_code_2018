
from utils import stdin2ndarray


if __name__ == "__main__":
    data = stdin2ndarray()
    print(f"Frequency is {data.sum()}")
