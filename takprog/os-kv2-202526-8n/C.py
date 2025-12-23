n = int(input())

odg = []
for i in range(1, n+1):
	for j in range(1, i):
		if (i+j) % 3 == 0:
			odg.append((i, j))

if len(odg) == 0:
	print(-1)
else:
	for i, j in odg:
		print(i, j)
