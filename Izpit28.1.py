#### 3 naloga ###

## a ##

from functools import lru_cache
@lru_cache(maxsize=None)
def postavi_korita (n, m, l):
    if n == 0 and m > 0 :
        return 0
    if n == 0:
        return 0
    for i in range(n): #pogledamo ali je na zacetku korito
        return 1 + postavi_korita (n - m - i, m - 1, l)
    
