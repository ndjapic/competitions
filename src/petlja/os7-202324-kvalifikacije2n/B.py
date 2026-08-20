a = int(input())
b = int(input())
c = int(input())
d = int(input())

odg = 0

if a < c:
    odg = max(odg, c-a)
elif a < d:
    odg = max(odg, 0)
else:
    odg = max(odg, a-d)

if b < c:
    odg = max(odg, b-a)
elif b < d:
    odg = max(odg, 0)
else:
    odg = max(odg, b-d)

print(odg)
