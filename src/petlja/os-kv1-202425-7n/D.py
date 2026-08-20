n = int(input())
zbir, broj = dict(), dict() # Sabiramo i brojimo ocene svakog jezika.

for i in range(n):
    jezik, ocena = input().split()
    zbir[jezik] = zbir.get(jezik, 0) + int(ocena)
    broj[jezik] = broj.get(jezik, 0) + 1

for jezik in sorted(zbir):
    prosek = zbir[jezik] / broj[jezik]
    print('{0} {1:.2f}'.format(jezik, prosek))
