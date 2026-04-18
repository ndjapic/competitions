class Solution:
    def distributeCandies(self, n: int, limit: int) -> int:
        l1, r1 = max(0, n - 2*limit), min(limit, n)
        ans = 0

        for c in range(l1, r1+1):
            l2, r2 = max(0, n-c - limit), min(limit, n-c)
            ans += max(0, r2 - l2 + 1)
        return ans

n, limit = int(input()), int(input())
print(Solution().distributeCandies(n, limit))
