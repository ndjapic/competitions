a, b, c = input().split()
a, b, c = int(a), int(b), int(c)

odg = min(x for x in [c+b-a, a+c-b, b+a-c] if x > 0)
print(odg)
