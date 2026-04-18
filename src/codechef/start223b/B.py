for tcase in range(int(input())):
    n = int(input())
    a = list(map(int, input().split()))
    m = mx = 0

    for x in a:
        if x-1 <= mx:
            m += 1
            mx = max(mx, x)

    print(m)
