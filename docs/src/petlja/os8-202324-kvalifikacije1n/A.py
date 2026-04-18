stranice = [ input(), input(), input() ]

temena_l = [ s[0] for s in stranice ]
temena_d = [ s[1] for s in stranice ]

for t in temena_d:
    if t not in temena_l:
        teme_l = t

for t in temena_l:
    if t not in temena_d:
        teme_d = t

print(teme_l + teme_d)
