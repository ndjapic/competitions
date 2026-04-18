n = int(input())
a, b = [0]*n, [0]*n

for i in range(n):
    a[i], b[i] = map(int, input().split())
    a[i] += i

need = n-1
for i in reversed(range(n)):
    if need <= a[i] + b[i]:
        if need - b[i] >= i:
            need -= b[i]

print(need)
