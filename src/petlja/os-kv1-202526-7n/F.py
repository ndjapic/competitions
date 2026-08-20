n = int(input())
a = sorted(map(int, input().split()))

i = 2
while i < n and a[i-2] + a[i-1] <= a[i]:
	i += 1

print('da' if i < n else 'ne')