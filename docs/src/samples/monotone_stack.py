def najveci_pravougaonik(visine):
    stek = [] # Чувамо индексе
    maks_povrsina = 0
    n = len(visine)
    
    for i in range(n + 1):
        # Користимо "dummy" висину 0 на крају да бисмо испразнили стек
        trenutna_visina = visine[i] if i < n else 0
        
        # Док је тренутна висина мања од оне на врху стека,
        # нашли смо десну границу за стубић са врха стека
        while stek and trenutna_visina < visine[stek[-1]]:
            visina = visine[stek.pop()]
            
            # Ако је стек празан, ширина је цео пут до индекса i
            # Ако није, ширина је растојање између i и претходног елемента у стеку
            sirina = i if not stek else i - stek[-1] - 1
            
            maks_povrsina = max(maks_povrsina, visina * sirina)
            
        stek.append(i)
        
    return maks_povrsina

# Тест
histogram = [2, 1, 5, 6, 2, 3]
print(f"Хистограм: {histogram}")
print(f"Највећа површина: {najveci_pravougaonik(histogram)}")
