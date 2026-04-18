n, k = map(int, input().split())
s = list(map(int, input().split()))

print(sum(1 for x in s if x >= k))
