x, y = [], []

for i in range(4):
    s, v = map(int, input().split())
    x.append(s)
    y.append(v)

print(sum(x), max(y))
