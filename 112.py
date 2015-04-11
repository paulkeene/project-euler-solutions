def is_bouncy(n):
    n_str = str(n)
    return ((n_str != ''.join(sorted(n_str))) and
            (n_str != ''.join(sorted(n_str, reverse=True))))

def solution():
    n = 1
    total = 0
    bouncy = 0
    while True:
        if is_bouncy(n):
            bouncy += 1
        total += 1
        ratio = bouncy / float(total)
        if ratio == (99 / 100.0):
            return n
        n += 1

if __name__ == '__main__':
    print solution()
