a, b, c = (int(t) for t in input().split())
n = int(input())

odg = 0
for i in range(n):
    x, y, z = (int(t) for t in input().split())
    if a <= x and b <= y and c <= z:
        odg += 1
print(odg)
