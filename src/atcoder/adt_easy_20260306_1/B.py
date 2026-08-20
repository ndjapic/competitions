n = int(input())

s = []
for i in range(n):
	s.append(input())

x, y = input().split()
x = int(x) - 1

print('Yes' if s[x] == y else 'No')
