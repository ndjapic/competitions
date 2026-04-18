from heapq import heappush, heappop

h, w, x = [int(_) for _ in input().split()]
p, q = [int(_)-1 for _ in input().split()]
s = [[int(_) for _ in input().split()] for i in range(h)]

pq = []
strength = [0]
inf = 2**62

def enque(r, c):
    if s[r][c] < inf:
        strength[0] += s[r][c]
        s[r][c] = inf
        if r > 0: heappush(pq, (s[r-1][c], r-1, c))
        if r < h-1: heappush(pq, (s[r+1][c], r+1, c))
        if c > 0: heappush(pq, (s[r][c-1], r, c-1))
        if c < w-1: heappush(pq, (s[r][c+1], r, c+1))
    
enque(p, q)
while len(pq) > 0 and pq[0][0] < (strength[0]+x-1) // x:
    _, r, c = heappop(pq)
    enque(r, c)

print(strength[0])
