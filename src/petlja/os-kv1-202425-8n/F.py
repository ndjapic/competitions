n = int(input())

brojac = dict()
sjaj = broj = odg = 0

for deo in input().split():
    brojac[sjaj] = broj + 1
    sjaj += int(deo)
    broj = brojac.get(sjaj, 0)
    odg += broj

print(odg)
