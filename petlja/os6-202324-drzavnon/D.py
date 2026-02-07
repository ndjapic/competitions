from bisect import bisect_left

n = int(input())
ponuda = []

for i in range(n):
    patika = input()
    x = int(patika.split()[-1])
    ponuda.append((x, patika))

ponuda.sort()
s = []
k = int(input())

for j in range(k):
    x = int(input())
    r = bisect_left(ponuda, (x+1, ''))
    if r == 0:
        s.append('nema')
    else:
        x = ponuda[r-1][0]
        l = bisect_left(ponuda, (x, ''))
        for i in range(l, r):
            s.append(ponuda[i][1])

s.append('')
print('\n'.join(s))
