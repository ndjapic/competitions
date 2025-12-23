n = int(input())
a = [int(t) for t in input().split()]

odg = 0
l = 0
for r in range(n):
    if a[r] == 0: l = r+1
    odg += l

print(odg)
