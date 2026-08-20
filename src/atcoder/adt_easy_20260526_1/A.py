a, b = map(int, input().split())
if a == b:
	print(-1)
else:
	ate = [True]*4
	ate[a] = ate[b] = False
	if ate[1]:
		print(1)
	elif ate[2]:
		print(2)
	elif ate[3]:
		print(3)
