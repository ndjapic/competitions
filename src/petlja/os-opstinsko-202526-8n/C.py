n = int(input())

xniz = []
yniz = []
for i in range(n):
    x, y = map(int, input().split())
    if x > 0 and y > 0:
        xniz.append(x)
        yniz.append(y)

print(min(xniz), min(yniz))
print(max(xniz), min(yniz))
print(min(xniz), max(yniz))
print(max(xniz), max(yniz))
