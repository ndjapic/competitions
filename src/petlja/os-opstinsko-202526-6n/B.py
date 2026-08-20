n = int(input())
n //= 10
n, c = divmod(n, 10)
b = n % 10

if b > c:
    print('>')
elif b < c:
    print('<')
else:
    print('=')
