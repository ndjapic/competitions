n, s = map(int, input().split())
t = [0] + list(map(int, input().split()))

awake, i = True, 1
while awake and i < n+1:
	awake = t[i] - t[i-1] <= s
	i += 1

print('Yes' if awake else 'No')
