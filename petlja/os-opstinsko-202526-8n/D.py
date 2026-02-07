from itertools import product

n1, s1 = int(input()), input().split()
n2, s2 = int(input()), input().split()
n3, s3 = int(input()), input().split()

nacini = [v for v in product(s1, s2, s3) if len(set(v)) == 3]
print(len(nacini))
