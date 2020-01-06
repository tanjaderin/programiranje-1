from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n <=1 :
        return n
    else:
        return fib(n-1) + fib(n-2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.

@lru_cache()
def fib_cache(n):
    if n <=1 :
        return n
    else:
        return fib_cache(n-1) + fib_cache(n-2)


# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.
def fib_memo_rec(n):
    fib_zaporedje = [None] * max(2, n + 1)
    def pomozna(n):
        if n <= 1:
            return n
        else:
            if fib_zaporedje[n] == None :
                fib_zaporedje[n] = fib_zaporedje[n - 1] + fib_zaporedje[n - 2]
            return fib_zaporedje[n]
        return pomozna(n)

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
def fib_memo_iter(n):
    fib_memo = []
    if n <= 1:
        return n
    else:
        for i in range (2, n + 1):
            fib_memo.append( fib_memo[i-1] + fib_memo[i-2])
    return fib_memo[n]

# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
def fib_iter(n):
    if n <= 1:
        return n
    else:
        x = 0
        y = 1
        for i in range(2, n + 1):
            x, y = y, x + y
        return y

