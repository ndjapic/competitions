from heapq import *

for tcase in range(int(input())):
    x, y = map(int, input().split())
    ans = inf = 10**18
    dist = {(x, y): 0}
    pq = [(abs(x-y), 0, x, y)]

    while len(pq) > 0:
        a, d, x, y = heappop(pq)
        d = min(d, dist.get((x, y), inf))
        dist[(x, y)] = d

        if a == 0:
            ans = min(ans, d)
        else:
            p2 = 2
            d2 = d + p2
            while d2 <= ans:
                if d & p2 == 0:
                    if x > 0:
                        x2 = x // p2
                        heappush(pq, (abs(x2-y), d2, x2, y))
                    if y > 0:
                        y2 = y // p2
                        heappush(pq, (abs(x-y2), d2, x, y2))
                p2 *= 2
                d2 = d + p2

    print(ans)
