a, b = input().split()
a, b = int(a), int(b)
print(0 if b > a else (a-b) // 15 + 1)
