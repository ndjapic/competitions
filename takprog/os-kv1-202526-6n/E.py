n, k = map(int, input().split())
rec = list(input())
neprikladno = set(input().split())

for i in range(n):
	if rec[i] in neprikladno:
		rec[i] = '#'

print(''.join(rec))
