from collections import Counter
from random import seed, randrange
seed()
salt = randrange(2**30)

for tcase in range(int(input())):
    n, k = map(int, input().split())
    a = [int(x) ^ salt for x in input().split()]
    s = Counter(a).most_common()

    while k > 0 and len(s) > 1:
        x, cx = s.pop()
        mn = min(k, cx)
        k -= mn
        cx -= mn
        if cx > 0:
            s.append((x, cx))

    print(len(s))
