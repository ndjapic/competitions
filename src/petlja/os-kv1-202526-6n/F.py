from bisect import bisect

n = int(input())
m = int(input())
a = list(map(int, input().split()))
a.append(0)
a.append(n+1)
a.sort()

r = int(input())
for i in range(r):
	x = int(input())
	b = bisect(a, x)
	print(a[b] - a[b-1] - 1)
