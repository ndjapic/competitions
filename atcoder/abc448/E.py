prime = 10**4+7
inv9 = pow(9, prime-2, prime)
n, m = map(int, input().split())
c, l = [0]*k, [0]*k

n = 0
for i in range(k):
	c[i], l[i] = map(int, input().split())
	n *= pow(10, l[i], prime)
	n += (pow(10, l[i], prime) + prime-1) * inv9 * c[i]
	n %= prime
