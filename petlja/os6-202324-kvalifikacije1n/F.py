n = int(input())
sijalice = [int(x) for x in input().split()]

ukljucenih = sum(sijalice)

m = int(input())
paja = [int(x) for x in input().split()]

milica = 0
for p in paja:
    if sijalice[p] == 1:
        sijalice[p] = 0
        ukljucenih -= 1
        if ukljucenih == 0:
            milica += 1
    else:
        sijalice[p] = 1
        ukljucenih += 1

print(milica)
