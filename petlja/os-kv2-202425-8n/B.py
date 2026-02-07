x1, y1 = [int(s) for s in input().split()]
x2, y2 = [int(s) for s in input().split()]
xp, yp = [int(s) for s in input().split()]
xk, yk = [int(s) for s in input().split()]

if xp > xk: xp, xk = xk, xp
if yp > yk: yp, yk = yk, yp

d = xk-xp + yk-yp

if x1 <= xp <= xk <= x2 and yp <= y1 <= y2 <= yk:
        d += 2 * min(xp-x1+1, x2-xk+1)

if y1 <= yp <= yk <= y2 and xp <= x1 <= x2 <= xk:
        d += 2 * min(yp-y1+1, y2-yk+1)

print(d)
