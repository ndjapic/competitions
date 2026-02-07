n, m = [int(deo) for deo in input().split()]

dec = [input() for i in range(n)]
dev = [input() for j in range(m)]

for i in range(n):
    for j in range(m):
        print(dec[i], dev[j])
