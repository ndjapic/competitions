n = int(input())
s = input()

t = a = 0
for x in s:
	if x == 'T':
		t += 1
		if t > a:
			winner = 'T'
	else:
		a += 1
		if a > t:
			winner = 'A'

if t > a:
	winner = 'T'
elif a > t:
	winner = 'A'

print(winner)
