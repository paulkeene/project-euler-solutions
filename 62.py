import math

def signature(value):
    return ''.join(sorted(str(value)))

def smallest_cube_with_n_permutations(n):
    i = 1
    while True:
        cube = _smallest_cube_with_n_permutations(n, i)
        if cube:
            return cube
        i += 1

def _smallest_cube_with_n_permutations(n, num_digits):
    smallest_cube = 10 ** (num_digits - 1)
    smallest_base = int(math.ceil(smallest_cube ** (1/3.0)))

    biggest_cube = (10 ** num_digits) - 1
    biggest_base = int(math.floor(biggest_cube ** (1/3.0)))

    cubes = {}
    for x in range(smallest_base, biggest_base + 1, 1):
        cube = x ** 3
        sig = signature(cube)
        if sig in cubes:
            (smallest, count) = cubes[sig]
            cubes[sig] = (smallest, count + 1)
        else:
            cubes[sig] = (cube, 1)

    cubes_with_n_perms = []
    for _, (smallest, count) in cubes.iteritems():
        if count == n:
            cubes_with_n_perms.append(smallest)

    if cubes_with_n_perms:
        return min(cubes_with_n_perms)
    return None

if __name__ == '__main__':
    print smallest_cube_with_n_permutations(5)
