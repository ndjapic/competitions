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
