n = int(input())
a = list(map(int, input().split()))
x = [0]

for ai in a:
	x.append(x[-1] + ai)

print(-min(x))
print(max(x))
