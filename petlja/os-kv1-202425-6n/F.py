k, n, m = [int(deo) for deo in input().split()]
ski = sorted(int(input()) for i in range(n))
pan = sorted(int(input()) for j in range(m))

platiti = [] # spisak mogućih iznosa za plaćanje
j = m-1 # redni broj najskupljih pancerica

for i in range(n):
    while j >= 0 and ski[i] + pan[j] > k:
        j -= 1
    if j >= 0:
        platiti.append(ski[i] + pan[j])

ostalo = k - max(platiti)
print(ostalo)
