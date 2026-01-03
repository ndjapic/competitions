k = int(input())
n = 2**k
pobede = [0]*n

for i in range(n-1):
    a, b = input().split()
    a, b = int(a), int(b)
    pobede[a] += 1

igraci = list(range(n))
igraci.sort(key=lambda x: -pobede[x])
igraci = [str(x) for x in igraci]

print(igraci[0])
stepen = 1
while stepen < n:
    print(' '.join(igraci[stepen:2*stepen]))
    stepen *= 2
