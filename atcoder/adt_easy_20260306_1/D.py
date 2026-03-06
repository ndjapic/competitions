from math import sqrt

n = int(input())

ans = 0
x, y = [0]*n, [0]*n

for i in range(n):
	x[i], y[i] = map(int, input().split())
	if i > 0:
		ans = max(ans,
			max(
				[
					(x[i]-x[j])**2 + (y[i]-y[j])**2 for j in range(i)
				]
			)
		)
	
print(sqrt(ans))
