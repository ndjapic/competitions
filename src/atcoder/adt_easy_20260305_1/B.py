n = int(input())
a = list(map(int, input().split()))

equal, i = True, 1
while equal and i < n:
	equal = a[i] == a[i-1]
	i += 1

print('Yes' if equal else 'No')
