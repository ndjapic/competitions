inf = 10**6 + 1
z, c = [0]*inf, [0]*inf

for n in range(1, inf):
    kol, ost = divmod(n, 10)
    z[n] = z[kol] + ost
    c[n] = c[n-1]
    if n % z[n] == 0:
        c[n] += 1

q = int(input())
for i in range(q):
    a, b = [int(x) for x in input().split()]
    print(c[b] - c[a-1])
