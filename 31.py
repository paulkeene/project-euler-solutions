import itertools
import math

def main():
    s = soln2()
    print s

# still weird
def soln2():
    coins = [100, 50, 20, 10, 5, 2, 1]
    combos = []
    for i in range(1, len(coins + 1)):
        combos.append(itertools.combinations(coins, i))

    return count_combos_opt(coins, 200)

def count_combos_opt(coins, value):
    if not coins:
        return 0
    if len(coins) == 1:
        if value % coins[0] == 0:
            return 1
        return 0

    c = coins[0]
    m = int(math.ceil(float(value) / c))
    result = 0
    for i in range(1, m):
        result += count_combos_opt(coins[1:], (value - i * c))
    return result

def soln():
    coins = [100, 50, 20, 10, 5, 2, 1]
    result = 8

    combos = itertools.combinations(coins, 7)
    result += count_combos(combos, combos_7, 200)

    combos = itertools.combinations(coins, 6)
    result += count_combos(combos, combos_6, 200)

    combos = itertools.combinations(coins, 5)
    result += count_combos(combos, combos_5, 200)

    combos = itertools.combinations(coins, 4)
    result += count_combos(combos, combos_4, 200)

    combos = itertools.combinations(coins, 3)
    result += count_combos(combos, combos_3, 200)

    combos = itertools.combinations(coins, 2)
    result += count_combos(combos, combos_2, 200)

    return result

def count_combos(combos, func, value):
    result = 0
    for c in combos:
        result += func(*c, value=value)
    return result

def combos_2(a, b, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        if (value - (a * i)) % b == 0:
            result += 1
    return result

def combos_3(a, b, c, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        r = combos_2(b, c, value - (i * a))
        result += r
    return result

def combos_4(a, b, c, d, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        r = combos_3(b, c, d, value - (i * a))
        result += r
    return result

def combos_5(a, b, c, d, e, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        r = combos_4(b, c, d, e, value - (i * a))
        result += r
    return result

def combos_6(a, b, c, d, e, f, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        r = combos_5(b, c, d, e, f, value - (i * a))
        result += r
    return result

def combos_7(a, b, c, d, e, f, g, value):
    m = int(math.ceil(float(value) / a))
    result = 0
    for i in range(1, m):
        r = combos_6(b, c, d, e, f, g, value - (i * a))
        result += r
    return result

if __name__ == '__main__':
    main()
