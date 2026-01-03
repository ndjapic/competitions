n = int(input())
lk = [input() for l in range(n)]
m = int(input())
dk = [input() for d in range(m)]

l = d = 0
while l < n or d < m:
    if l < n:
        print(lk[l])
        l += 1
    if d < m:
        print(dk[d])
        d += 1
