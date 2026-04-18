r = int(input())
l = int(input())

if 1990 <= r <= 1999 and not 1990 <= l <= 1999:
    print('R')
elif not 1990 <= r <= 1999 and 1990 <= l <= 1999:
    print('L')
elif r > l:
    print('R')
elif r < l:
    print('L')
else:
    print('N')
