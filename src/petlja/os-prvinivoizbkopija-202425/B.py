n = int(input())
a = list(map(int, input().split()))
s = int(input())

l, r = 1, max(a)
while r-l > 1:
    m = (l+r) // 2
    if sum((x-1) // m + 1 for x in a) > s:
        l = m
    else:
        r = m

print(r)
