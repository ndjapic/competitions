n = int(input())
c = [int(deo) for deo in input().split()]
d = {0: 1}
a = [d]

for i in range(n):
    d = a[i].copy()
    for key, val in a[i].items():
        d[key + c[i]] = a[i].get(key + c[i], 0) + val
    a.append(d)

print(a[n][0])
