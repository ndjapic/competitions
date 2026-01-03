for tcase in range(int(input())):
    n = int(input())
    h = n // 2
    l, r = h, h+1

    a = [[0]*n for i in range(n)]

    while l > 0:
        a[l-1][l-1] = a[r-1][r-1] = h
        a[l-1][r-1] = a[r-1][l-1] = h+1
        for i in range(2, r-l, 2):
            a[l-1][l+i-1] = h - i//2
            a[l-1][r-i-1] = h+1 + i//2
            a[r-1][r-i-1] = h - i//2
            a[r-1][l+i-1] = h+1 + i//2
            a[l+i-1][r-1] = r
            a[r-i-1][l-1] = r
            a[r-i-1][r-1] = l
            a[l+i-1][l-1] = l
        l -= 1
        r += 1

    for i in range(n):
        print(' '.join(map(str, a[i])))
