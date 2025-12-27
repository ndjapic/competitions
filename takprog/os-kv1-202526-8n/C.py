n = int(input())

arr = []
for i in range(n):
	a, b = map(int, input().split())
	arr.append(abs(a-b))

print(max(arr))
