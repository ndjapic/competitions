class SegmentTree:
    def __init__(self, arr):
        self.n = len(arr)
        # Tree size is typically 2 * 2^k where 2^k >= n, rounded up to nearest power of 2
        # For simplicity, we can use 2 * n, but 4 * n is a safer general size for recursive implementations.
        # For iterative, 2*n is often sufficient if we carefully manage indices.
        self.tree = [0] * (2 * self.n) # Initialize with zeros

        # Build the segment tree
        self._build(arr)

    def _build(self, arr):
        # Copy original array elements to the leaves of the tree
        for i in range(self.n):
            self.tree[self.n + i] = arr[i]
        # Build up the tree by summing children
        for i in range(self.n - 1, 0, -1):
            self.tree[i] = self.tree[2 * i] + self.tree[2 * i + 1]

    def update(self, index, delta):
        """
        Increments the value at 'index' by 'delta'.
        """
        # Go to the leaf node corresponding to 'index'
        index += self.n
        self.tree[index] += delta

        # Propagate the update up to the root
        while index > 1:
            index //= 2 # Move to parent
            self.tree[index] = self.tree[2 * index] + self.tree[2 * index + 1]

    def query(self, left, right):
        """
        Queries the sum of elements in the range [left, right) (exclusive right).
        """
        res = 0
        left += self.n
        right += self.n

        while left < right:
            if left % 2 == 1: # If left child, include its value and move to next sibling
                res += self.tree[left]
                left += 1
            if right % 2 == 1: # If right child, include its value and move to previous sibling
                right -= 1
                res += self.tree[right]
            left //= 2 # Move up to parent
            right //= 2 # Move up to parent
        return res

