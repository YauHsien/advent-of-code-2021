input0 = [('V','P')]
rules = {
    ('C','O'): 'B',
    ('C','V'): 'N',
    ('H','V'): 'H',
    ('O','N'): 'O',
    ('F','S'): 'F',
    ('N','S'): 'S',
    ('V','K'): 'C',
    ('B','V'): 'F',
    ('S','C'): 'N',
    ('N','V'): 'V',
    ('N','C'): 'F',
    ('N','H'): 'B',
    ('B','O'): 'K',
    ('F','C'): 'H',
    ('N','B'): 'H',
    ('H','O'): 'F',
    ('S','B'): 'N',
    ('K','P'): 'V',
    ('O','S'): 'C',
    ('O','B'): 'P',
    ('S','H'): 'N',
    ('B','C'): 'H',
    ('C','K'): 'H',
    ('S','O'): 'N',
    ('S','P'): 'P',
    ('C','F'): 'P',
    ('K','V'): 'F',
    ('C','S'): 'V',
    ('F','F'): 'P',
    ('V','S'): 'V',
    ('C','P'): 'S',
    ('P','H'): 'V',
    ('O','P'): 'K',
    ('K','H'): 'B',
    ('F','B'): 'S',
    ('C','N'): 'H',
    ('K','S'): 'P',
    ('F','N'): 'O',
    ('P','V'): 'O',
    ('V','C'): 'S',
    ('H','F'): 'N',
    ('O','C'): 'O',
    ('P','K'): 'V',
    ('K','C'): 'C',
    ('H','K'): 'C',
    ('P','O'): 'N',
    ('O','O'): 'S',
    ('V','H'): 'N',
    ('C','C'): 'K',
    ('B','P'): 'K',
    ('H','C'): 'K',
    ('F','V'): 'K',
    ('K','F'): 'V',
    ('V','F'): 'C',
    ('H','N'): 'S',
    ('V','P'): 'B',
    ('H','H'): 'O',
    ('F','O'): 'O',
    ('P','C'): 'N',
    ('K','K'): 'C',
    ('P','N'): 'P',
    ('N','N'): 'C',
    ('F','H'): 'N',
    ('V','V'): 'O',
    ('O','K'): 'V',
    ('C','B'): 'N',
    ('S','N'): 'H',
    ('V','O'): 'H',
    ('B','B'): 'C',
    ('P','B'): 'F',
    ('N','F'): 'P',
    ('K','O'): 'S',
    ('P','P'): 'K',
    ('N','O'): 'O',
    ('S','F'): 'N',
    ('K','N'): 'S',
    ('P','S'): 'O',
    ('V','N'): 'V',
    ('S','S'): 'N',
    ('B','F'): 'O',
    ('H','P'): 'H',
    ('H','S'): 'N',
    ('B','S'): 'S',
    ('V','B'): 'F',
    ('P','F'): 'K',
    ('S','V'): 'V',
    ('B','H'): 'P',
    ('F','P'): 'O',
    ('C','H'): 'P',
    ('O','H'): 'K',
    ('O','F'): 'F',
    ('H','B'): 'V',
    ('F','K'): 'V',
    ('B','N'): 'V',
    ('S','K'): 'F',
    ('O','V'): 'C',
    ('N','P'): 'S',
    ('N','K'): 'S',
    ('B','K'): 'C',
    ('K','B'): 'F'
}
scores = {
    'B': 0,
    'C': 0,
    'F': 0,
    'H': 0,
    'K': 0,
    'N': 0,
    'O': 0,
    'P': 0,
    'S': 0,
    'V': 0
}
env = {}

if __name__ == '__main__':
    import sys
    n = int(sys.argv[1])
    for i in range(1, n+1):
        env[i] = []
    #env = { i: [] for i in range(1, n+1) }
    for p in input0:
        #print(p, env)
        (x0,y0) = p
        r0 = rules[(x0,y0)]
        env[1].insert(0, (x0,r0,y0))
        g = 1
        while g > 0:
            #print(g, env) 
            if g == n:
                (x,y,_) = env[g].pop(0)
                scores[x] = scores[x] + 1
                scores[y] = scores[y] + 1
                g = g - 1
            else:
                t = env[g].pop(0)
                if type(t)==tuple and 3==len(t):
                    (x,y,z) = t
                    env[g+1].insert(0, (x,rules[(x,y)],y))
                    env[g].insert(0, (y,z))
                    g = g + 1
                elif type(t)==tuple and 2==len(t):
                    (x,y) = t
                    env[g+1].insert(0, (x,rules[(x,y)],y))
                    env[g].insert(1, g)
                    g = g + 1
                elif type(t)==int and g == t:
                    g = g - 1
        #print(g, env, scores)
    (_,y) = p
    scores[y] = scores[y] + 1
    for k in scores.keys():
        print(k, ':', scores[k])
