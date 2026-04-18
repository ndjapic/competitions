for tcase in range(int(input())):
    n, x, k = map(int, input().split())

    ans = x % k
    if x - ans + k <= n:
        ans = min(ans, k - ans)

    print(ans)
