n, m = map(int, input().split())

d = {}
for i in range(m):
    x, y, c = input().split()
    x, y = int(x), int(y)
    if x not in d:
        d[x] = []
    d[x].append([y, c])
keys = sorted(d.keys())

r = n+1
ans = True
for x in keys:
    if ans:
        d[x].sort()
        while len(d[x]) > 0 and d[x][-1][1] == 'W':
            y, c = d[x].pop()
            r = min(r, y)
        while len(d[x]) > 0 and ans:
            y, c = d[x].pop()
            ans = y < r and c == 'B'

print('Yes' if ans else 'No')            
