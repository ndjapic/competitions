from collections import deque

n = int(input())
s = input()

a = deque()
rev = False

for k in range(n):
	if rev:
		a.appendleft(k+1)
	else:
		a.append(k+1)

	if s[k] == 'o':
		rev = not rev

if rev:
	a.reverse()
print(' '.join(map(str, a)))
