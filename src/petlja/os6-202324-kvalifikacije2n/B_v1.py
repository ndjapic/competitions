n = int(input())
n += 8
n *= 2
n -= 10

if n > 0 and n % 8 == 0:
    n //= 8
    print(n)
else:
    print('greska')
