# The 500,000th prime is 7,368,787.

n = 7368787+1
jeProst = [True] * n
zp = [0]

# Sledi Eratostenovo sito:

for p in range(2, n):
	if jeProst[p]:
		zp.append(zp[-1] + p)
		for x in range(p*p, n, p):
			jeProst[x] = False

q = int(input())
tok = []
for i in range(q):
	a, b = [int(x) for x in input().split()]
	tok.append(str(zp[b] - zp[a-1]))

tok.append('')
print('\n'.join(tok))
