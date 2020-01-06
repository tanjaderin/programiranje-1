from functools import lru_cache
test_matrix = 
  [ [1 , 2 , 0 ],
     [2 , 4 , 5 ],
     [ 7 , 0 , 1 ] ]

def max_sir (matrika):#i vrstica, j je stolpec

    try:
        dim_j = len(matrika[0])
    except IndexError:
        return 0

    dim_i = len(matrika)
    @lru_cache(maxsize=None)
    def max_index (i, j):
        if i = dim_i or j = dim_j:
            return 0
        return matrika[i][j] + max( max_index(i + 1, j), max_index(i, j + 1))
    return max_index(0,0) #zacnemo na zacetku


print([[j for j in range(20)] for _ in range(20])