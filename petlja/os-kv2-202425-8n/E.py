n, q = [int(x) for x in input().split()]
s = list(input())

d = 0
for i in range(n//2):
    if s[i] != s[n-1-i]:
        d += 1

for k in range(q):
    i, c = input().split()
    i = int(i)-1
    if i != n-1-i and s[i] != s[n-1-i]:
        d -= 1
    s[i] = c
    if i != n-1-i and s[i] != s[n-1-i]:
        d += 1
    print('DA' if d == 0 else 'NE')
