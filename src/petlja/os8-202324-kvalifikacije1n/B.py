s = input()
n = len(s)

x = 0
for i in range(n):
    x = x * 26 + 1

y = 0
for zn in s:
    y = y * 26 + ord(zn) - ord('A')

print(x+y)
