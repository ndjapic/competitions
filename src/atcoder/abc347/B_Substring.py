s = input()
n = len(s)

a = [s[l:r+1] for l in range(n) for r in range(l, n)]
print(len(set(a)))
