from itertools import product

n1 = int(input())
s1 = input().split()
n2 = int(input())
s2 = input().split()
n3 = int(input())
s3 = input().split()

nacini = 0
for m1, m2, m3 in product(s1, s2, s3):
	if len(set([m1, m2, m3])) == 3:
		nacini += 1

print(nacini)
