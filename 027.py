import math

def generate_primes(n):
    """ Generates all the prime numbers from 2 to n """
    candidates = [True for x in range(0, n + 1)]
    candidates[0] = False
    candidates[1] = False

    end = int(math.ceil(math.sqrt(n))) + 1
    for i in range(2, end):
        for j in range(i * 2, n + 1, i):
            candidates[j] = False

    indexed_candidates = zip(range(0, n + 1), candidates)
    return [c[0] for c in indexed_candidates if c[1]]

def main():
    primes = set(generate_primes(1000000))

    potential_as = [a for a in range(-1000, 2001) if a % 2 != 0]
    potential_bs = [b for b in primes if b <= 1000]

    best_a = None
    best_b = None
    max_primes_found = 0
    for a in potential_as:
        for b in potential_bs:
            primes_found = 0
            x = 0
            while True:
                result = (x ** 2) + (a * x) + b
                if result not in primes:
                    break
                primes_found += 1
                x += 1
            if primes_found > max_primes_found:
                max_primes_found = primes_found
                best_a = a
                best_b = b

    print "a: %s" % best_a
    print "b: %s" % best_b
    print "primes found: %s" % max_primes_found


if __name__ == '__main__':
    main()
