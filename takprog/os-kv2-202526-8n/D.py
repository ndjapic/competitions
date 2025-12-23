k, d, n = input().split()
k, d, n = int(k), int(d), int(n)

x = input().split()
odg = c = 0

for i in range(n):
	if i >= d: c -= x[i-d]
	x[i] = min(int(x[i]), k-c)
	c += x[i]
	odg += x[i]

print(odg)
