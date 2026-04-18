a, b, c, d = map(int, input().split())
b += a * 60
d += c * 60
print('Takahashi' if b <= d else 'Aoki')