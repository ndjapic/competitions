n, q = [int(t) for t in input().split()]

s = []
for x in [int(t) for t in input().split()]:
    s.append(set())
    s[-1].add(x)

for i in range(q):
    a, b = [int(t)-1 for t in input().split()]
    s[b].update(s[a])
    s[a].clear()
    print(len(s[b]))
