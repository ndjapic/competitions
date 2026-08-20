def rank(a, b, p):
	s = len(a)
	j = len(b) - 1
	for i in range(len(a)):
		while j >= 0 and a[i] * b[j] > p:
			j -= 1
		s += j
	return s

def sol(a1, b1, a2, b2, k):
	l, r = 0, 10**10
	while r-l > 1:
		m = (l+r) // 2
		if rank(a1, b1, m) + rank(a2, b2, m) < k:
			l = m
		else:
			r = m
	return r
	
class Solution:
	def kthSmallestProduct(self, nums1: List[int], nums2: List[int], k: int) -> int:
		n1, n2 = len(nums1), len(nums2)

		neg1, pos1 = [], []
		for x in nums1:
			if x < 0:
				neg1.append(-x)
			elif x > 0:
				pos1.append(x)
		neg1.reverse()

		neg2, pos2 = [], []
		for x in nums2:
			if x < 0:
				neg2.append(-x)
			elif x > 0:
				pos2.append(x)
		neg2.reverse()

		neg = len(neg1) * len(pos2) + len(pos1) * len(neg2)
		pos = len(neg1) * len(neg2) + len(pos1) * len(pos2)
		zer = n1 * n2 - neg - pos

		if k < neg + 1:
			return -sol(neg1, pos2, pos1, neg2, neg + 1 - k)
		elif k > neg + zer:
			return sol(neg1, neg2, pos1, pos2, k - neg - zer)
		else:
			return 0
