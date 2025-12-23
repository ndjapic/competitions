d = int(input() + input())
m = int(input() + input())
y = 1000 + int(input() + input() + input())

if y < 1100:
	y += 1000

age = 2025 - y

if m > 11 or m == 11 and d > 15:
	age -= 1

print(age)