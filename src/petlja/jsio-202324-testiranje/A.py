n, m = [int(x) for x in input().split()]
t = [int(x) for x in input().split()]

l = ukupno = odg = 0
for r in range(n):
    ukupno += t[r]
    while ukupno > m:
        ukupno -= t[l]
        l += 1
    odg = max(odg, r-l+1)

print(odg)
