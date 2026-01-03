INF = 10**9

n = int(input())

d = {}
for i in range(n):
    a, c = map(int, input().split())
    d[c] = min(d.get(c, INF), a)

ans = max(d[c] for c in d)
print(ans)
