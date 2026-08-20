n = int(input()) // 4 - 1

t = [int(x) for x in input().split()]
t3 = sorted(t[:4])[2]

if input() == 'max':
    iza = len([x for x in t[4:] if x > t3]) // 2
    isp = max(n - iza, 0)
else:
    isp = len([x for x in t[4:] if x < t3]) // 3
    isp = min(isp, n)

print(isp + 1)
