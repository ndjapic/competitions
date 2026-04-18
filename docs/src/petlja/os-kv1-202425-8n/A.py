s1 = input()
a1 = list(s1)

a2, a3 = a1.copy(), a1.copy()

a2[0], a2[3] = a2[3], a2[0]
a3[1], a3[2] = a3[2], a3[1]

s2, s3 = ''.join(a2), ''.join(a3)
p = (int(s1) + int(s2) + int(s3)) % 10000
print(str(p).zfill(4))
