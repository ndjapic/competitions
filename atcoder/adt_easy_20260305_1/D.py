x, y = map(int, input().split())

outcomes = 0
for i in range(6):
	for j in range(6):
		if i+j+2 >= x or abs(i-j) >= y:
			outcomes += 1

print(outcomes / 36)
