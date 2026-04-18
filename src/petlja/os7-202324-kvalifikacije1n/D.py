a = [0, 0, 0, 0]

for i in range(4):
    a[i] = int(input())

a.sort()

inf = 10**7
d = [inf, inf, inf, inf]

for i in range(3):
    d[i] = min(d[i], a[i+1] - a[i])
    d[i+1] = min(d[i+1], a[i+1] - a[i])

m = max(d)

for i in range(4):
    if d[i] == m:
        print(a[i])
