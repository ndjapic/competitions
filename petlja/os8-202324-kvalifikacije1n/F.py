def dfs(i, n):
    global p
    if p > 0 and ( i < 4 or len(set(a[i-4:i])) >= 3 ):
        if i < n:
            for j in range(1, 5):
                a[i] = j
                dfs(i+1, n)
        else:
            print(''.join([str(x) for x in a]))
            p -= 1

n = int(input())
p = int(input())
a = [0]*n

dfs(0, n)
