n = int(input())
sib = [int(x) for x in input().split()]
obim = sum(sib)
a = obim // 4

def moze(i, zbir):
    uradjeno = i == n
    if not uradjeno:
        for j in range(i, n):

            if j > i:
                sib[i], sib[j] = sib[j], sib[i]

            if (zbir + sib[i]) % a == 0 or zbir // a == (zbir + sib[i]) // a:
                uradjeno = uradjeno or moze(i+1, zbir + sib[i])

            if j > i:
                sib[i], sib[j] = sib[j], sib[i]

    return uradjeno

if obim % 4 > 0:
    print(0)
else:
    print(1 if moze(0, 0) else 0)
