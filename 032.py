import itertools
import math

def main():
    print soln()

def soln():
    nums = range(1, 10)
    set_nums = set(nums)
    perms = itertools.permutations(nums, 4)
    products = set([])
    for c in perms:
        product = to_int(c)
        remainders = set_nums.difference(c)
        divisors = get_divisors(product)
        for d1, d2 in itertools.combinations(divisors, 2):
            if remainders == set(to_list(d1) + to_list(d2)):
                if product == (d1 * d2):
                    products.add(product)
    return sum(products)

def to_int(l):
    mult = 1
    result = 0
    for x in l[::-1]:
        result += x * mult
        mult *= 10
    return result

def to_list(num):
    result = []
    curr = num
    while curr:
        result.insert(0, curr % 10)
        curr /= 10
    return result

def get_divisors(num):
    divisors = set([])
    for i in range(2, int(math.ceil(math.sqrt(num))) + 1):
        if num % i == 0:
            divisors.add(i)
            divisors.add(num / i)
    divisors.add(1)
    divisors.add(num)
    return divisors

if __name__ == '__main__':
    main()
