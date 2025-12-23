n = int(input())
k = int(input())
ne_vaze = sum(int(input()) for i in range(k))

med = float(input())
ukupno = round(med * n)
gim = (ukupno - ne_vaze) / (n-k)

print('{:.2f}'.format(gim))
