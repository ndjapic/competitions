n = int(input())

kutije = []
for i in range(n):
    w, d, h = [int(x) for x in input().split()]
    if w > d:
        w, d = d, w
    kutije.append([h, w, d])
kutije.sort()

dp = [0]*n
for i, (hi, wi, di) in enumerate(kutije):
    dp[i] = max([dp[j] for j, (hj, wj, dj) in enumerate(kutije[:i]) if hj <= hi and wj <= wi and dj <= di], default=0) + 1

print(max(dp))
