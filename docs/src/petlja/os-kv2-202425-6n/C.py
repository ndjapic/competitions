pok = [int(x) for x in input().split()]
lon = [int(x) for x in input().split()]

p = odg = 0
for l in range(3):
    if pok[p] >= lon[l]:
        odg += 1
        p += 1

print(odg)
