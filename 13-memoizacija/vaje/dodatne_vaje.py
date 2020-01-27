from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################


def najdaljse_narascajoce_podzaporedje(sez):
    @lru_cache(maxsize=None)
    def najdaljse(spodnja_meja, i):
        # i označuje indeks trenutnega elementa
        if i >= len(sez):
            # Robni pogoj
            return []
        elif sez[i] < spodnja_meja:
            # Neprimeren element
            return najdaljse(spodnja_meja, i + 1)
        else:
            # Razvejitev in agregacija glede na dolzino
            z_prvim = [sez[i]] + najdaljse(sez[i], i + 1)
            brez_prvega = najdaljse(spodnja_meja, i + 1)
            if len(z_prvim) > len(brez_prvega):
                return z_prvim
            else:
                return brez_prvega
    # Zazenemo
    if len(sez) == 0:
        return []
    else:
        return najdaljse(sez[0], 0)
###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):    

    @lru_cache(maxsize=None)
    def pobegni(vrsta, stolpec, koraki):
        # Padli smo iz sobe
        if not (0 <= vrsta < max_vrsta) or not (0 <= stolpec < max_stolpec):
            return False
        # Pobeg uspesen! All hail our robot overlords!!!
        elif soba[vrsta][stolpec] == 1:
            return True
        # Lahko bezimo naprej
        elif soba[vrsta][stolpec] == 0 and koraki > 0:
            return any(
                [pobegni(vrsta + 1, stolpec, koraki - 1),
                 pobegni(vrsta - 1, stolpec, koraki - 1),
                 pobegni(vrsta, stolpec + 1, koraki - 1),
                 pobegni(vrsta, stolpec - 1, koraki - 1)])
        # Pristali smo na oviri ali pa nam je zmanjkalo korakov
        else:
            return False
    return pobegni(pozicija[0], pozicija[1], koraki)
