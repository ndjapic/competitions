def manachers_table(s):
    t = "#" + "#".join(s) + "#"
    n = len(t)
    p = [0] * n
    c = r = 0
    for i in range(n):
        mirror = 2 * c - i
        if i < r:
            p[i] = min(r - i, p[mirror])
        while i + 1 + p[i] < n and i - 1 - p[i] >= 0 and t[i + 1 + p[i]] == t[i - 1 - p[i]]:
            p[i] += 1
        if i + p[i] > r:
            c = i
            r = i + p[i]
    return t, p

s = "abaaba"
t, p = manachers_table(s)
print(f"{'Index i':<8} | {'t[i]':<5} | {'p[i]':<5} | {'Original (s)':<15}")
print("-" * 45)
for i in range(len(t)):
    start_s = (i - p[i]) // 2
    length_s = p[i]
    sub_s = s[start_s:start_s + length_s] if length_s > 0 else ""
    print(f"{i:<8} | {t[i]:<5} | {p[i]:<5} | {sub_s:<15}")

################ cut here

def manacher(s):
    # Претварамо "aba" у "#a#b#a#" да бисмо подржали парне палиндроме
    t = "#" + "#".join(s) + "#"
    n = len(t)
    p = [0] * n  # Полупречници палиндрома
    centar = 0   # Центар палиндрома који се највише шири удесно
    desno = 0    # Десна ивица тог палиндрома

    for i in range(n):
        # Користимо симетрију за иницијализацију p[i]
        if i < desno:
            ogledalo = 2 * centar - i
            p[i] = min(desno - i, p[ogledalo])

        # Покушавамо да проширимо палиндром око центра i
        while i + p[i] + 1 < n and i - p[i] - 1 >= 0 and \
              t[i + p[i] + 1] == t[i - p[i] - 1]:
            p[i] += 1

        # Ако се нови палиндром шири даље од тренутног 'desno', ажурирамо центар
        if i + p[i] > desno:
            centar = i
            desno = i + p[i]

    # Издвајање максималних поднизова из низа p
    rezultat = []
    for i in range(n):
        duzina = p[i]
        if duzina > 0:
            start = (i - duzina) // 2
            rezultat.append(s[start : start + duzina])

    return rezultat
