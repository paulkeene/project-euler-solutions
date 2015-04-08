import math
import sympy


def corner_values(side_length):
    vals = []
    bottom_right = side_length ** 2
    vals.append(bottom_right)
    for i in range(1, 4, 1):
        vals.append(bottom_right -
                    ((side_length - 1) * i))
    return vals

def generate_primes(max_prime):
    is_prime = [True] * max_prime
    i = 2
    while i < int(math.ceil(math.sqrt(max_prime))):
        if is_prime[i]:
            for j in range(i*2, max_prime, i):
                is_prime[j] = False
        i += 1

    primes = []
    for i in range(2, max_prime):
        if is_prime[i]:
            primes.append(i)
    return primes

def solution():
    side_length = 1
    diagonal_count = 1
    prime_count = 0
    while True:
        side_length += 2
        corner_vals = corner_values(side_length)
        for cv in corner_vals:
            if sympy.isprime(cv):
                prime_count += 1
        diagonal_count += 4
        prime_ratio = prime_count / float(diagonal_count)
        if prime_ratio < 0.1:
            return side_length

if __name__ == '__main__':
    print solution()
