reci = input().split(' ')
recenica = []

for r in reci:
    if r[0] == '[':
        if r[-1] == ']':
            print(r[1:-1])
        else:
            recenica.append(r[1:])
    elif r[-1] == ']':
        recenica.append(r[:-1])
        print(' '.join(recenica))
        recenica = []
    elif len(recenica) > 0:
        recenica.append(r)
    else:
        print(r)
