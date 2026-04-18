n = int(input())
a = [int(deo) for deo in input().split()]

par = 0
l = 0
duzine = [] # Spisak dužina traženih podnizova

for d in range(n):
    if a[d] % 2 == 0:
        par += 1
    while par > 3:
        if a[l] % 2 == 0:
            par -= 1
        l += 1
    duzine.append(d-l+1)

print(max(duzine))
