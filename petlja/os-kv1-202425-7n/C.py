n = int(input())
a, b = [0]*n, [0]*n

for i in range(n):
    a[i], b[i] = [int(deo) for deo in input().split()]

odg = min(b) - max(a) + 1
print(max(odg, 0))
