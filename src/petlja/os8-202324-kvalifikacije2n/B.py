a = int(input())
b = int(input())
c = int(input())
d = int(input())

a, b, c, d = sorted([a, b, c, d])

razlika = abs(a+d-b-c)
if razlika > 8:
    print('ne')
elif razlika % 2 == 1:
    print('ne')
else:
    print('da')
    print(razlika // 2)
