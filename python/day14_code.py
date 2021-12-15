input0 = [('N','N'), ('N','C'), ('C','B')]
rules = {
    ('C', 'H'): 'B',
    ('H', 'H'): 'N',
    ('C', 'B'): 'H',
    ('N', 'H'): 'C',
    ('H', 'B'): 'C',
    ('H', 'C'): 'B',
    ('H', 'N'): 'C',
    ('N', 'N'): 'C',
    ('B', 'H'): 'H',
    ('N', 'C'): 'B',
    ('N', 'B'): 'B',
    ('B', 'N'): 'B',
    ('B', 'B'): 'N',
    ('B', 'C'): 'B',
    ('C', 'C'): 'N',
    ('C', 'N'): 'C'
}
scores = {
    'B': 0,
    'C': 0,
    'H': 0,
    'N': 0
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
