from itertools import permutations
from math import sqrt

def dist(dx, dy):
    return sqrt(dx*dx + dy*dy)

def redosled(lst):
    odg = []
    for j in range(len(lst)):
        if j > 0: lst[0], lst[j] = lst[j], lst[0]

        dmin = 5000
        for p in permutations(lst[1:]):
            i0, x0, y0 = lst[0]
            d, red = 0, [lst[0]]

            for i1, x1, y1 in p:
                d += dist(x0-x1, y0-y1)
                red.append((i1, x1, y1))
                i0, x0, y0 = i1, x1, y1

            if d < dmin:
                dmin, redmin = d, red

        if j > 0: lst[0], lst[j] = lst[j], lst[0]
        odg.append((redmin, dmin))

    return odg

n = int(input())
neg, poz = [], []
for i in range(n):
    y, m = input().split()
    x, y = 126 / float(m), int(y)
    if x < 0:
        neg.append((i, x, y))
    else:
        poz.append((i, x, y))

dmin = 60000

for negredmin, negdmin in redosled(neg):
    i0, x0, y0 = negredmin[0]
    for pozredmin, pozdmin in redosled(poz):
        i1, x1, y1 = pozredmin[0]
        d = negdmin + dist(x0-x1, y0-y1) + pozdmin
        if d < dmin:
            dmin, redmin = d, list(reversed([i for i, x, y in negredmin])) + [i for i, x, y in pozredmin]

redmin = [str(z+1) for z in redmin]
print(' '.join(redmin))
