class FenwickTree:
	def __init__(self, size):
		# Низ фиксиране дужине који замењује SortedList
		self.tree = [0] * (size + 1)
		self.size = size

	def add(self, idx, val):
		# Еквивалент додавању елемента
		while idx <= self.size:
			self.tree[idx] += val
			idx += idx & -idx

	def query(self, idx):
		# Еквивалент методи bisect_left
		s = 0
		while idx > 0:
			s += self.tree[idx]
			idx -= idx & -idx
		return s

def solve(nums, target):
	n = len(nums)
	# Опсег за d је од -n до n. Ширина је 2*n + 1.
	# Пошто Fenwick користи индексе од 1, додајемо помак (offset)
	offset = n + 1
	tree = FenwickTree(2 * n + 2)

	d = ans = 0
	# Иницијално убацујемо почетну нулу, баш као sl = SortedList([0])
	tree.add(0 + offset, 1)

	for x in nums:
		d += 1 if x == target else -1
		
		# tree.query(d + offset - 1) даје тачан број елемената који су СТРИКТНО МАЊИ од d
		# Ово је савршен еквивалент акцији sl.bisect_left(d) пре него што се d уметне!
		ans += tree.query(d + offset - 1)
		
		# Убацујемо тренутно d у стабло
		tree.add(d + offset, 1)

	return ans
