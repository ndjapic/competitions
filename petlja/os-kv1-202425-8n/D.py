n, k = [int(deo) for deo in input().split()]
osm = [input() for i in range(n)]

def ea(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i][j+t] for t in range(d)
        ) for i in range(n) for j in range(n+1-d)
    )

def we(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i][j-t] for t in range(d)
        ) for i in range(n) for j in range(d-1, n)
    )

def so(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i+t][j] for t in range(d)
        ) for i in range(n+1-d) for j in range(n)
    )

def no(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i-t][j] for t in range(d)
        ) for i in range(d-1, n) for j in range(n)
    )

def se(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i+t][j+t] for t in range(d)
        ) for i in range(n+1-d) for j in range(n+1-d)
    )

def ne(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i-t][j+t] for t in range(d)
        ) for i in range(d-1, n) for j in range(n+1-d)
    )

def sw(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i+t][j-t] for t in range(d)
        ) for i in range(n+1-d) for j in range(d-1, n)
    )

def nw(rec):
    d = len(rec)
    return any(
        all(
            rec[t] == osm[i-t][j-t] for t in range(d)
        ) for i in range(d-1, n) for j in range(d-1, n)
    )

odg = 0
for _ in range(k):
    r = input()
    if any([ea(r), we(r), so(r), no(r), se(r), ne(r), sw(r), nw(r)]):
        odg += 1

print(odg)
