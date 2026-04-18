from collections import defaultdict
from random import seed, randrange

seed()
h0 = randrange(2**30)

for tcase in range(int(input())):
    n, k, l, r = map(int, input().split())
    a = [int(x) ^ h0 for x in input().split()]

    fl, fr = defaultdict(int), defaultdict(int)
    bl = br = ans = 0

    for c, x in enumerate(a):
        fl[x] += 1
        fr[x] += 1

        while len(fl) >= k:
            y = a[bl]
            fl[y] -= 1
            if fl[y] == 0: fl.pop(y, 0)
            bl += 1

        while len(fr) > k:
            y = a[br]
            fr[y] -= 1
            if fr[y] == 0: fr.pop(y, 0)
            br += 1

        ans += max(0, min(c + 1 - br, r) - max(c + 1 - bl + 1, l) + 1)

    print(ans)
