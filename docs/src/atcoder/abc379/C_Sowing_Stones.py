n, m = [int(v) for v in input().split()]
x = [int(v) for v in input().split()]
a = [int(v) for v in input().split()]

decor = sorted(zip(x, a))

for i in range(m):
    x[i], a[i] = decor[i]
x.append(n+1)
a.append(0)

i = moves = 0
x0 = x[0]
while i < m and x0 <= n+1:
    moves += a[i] * (x0 - x[i]) + a[i] * (a[i] - 1) // 2
    x0 = max(x0 + a[i], x[i+1])
    i += 1

print(moves if x0 <= n+1 else -1)
