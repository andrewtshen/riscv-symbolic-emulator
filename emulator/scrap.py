import cProfile

def loop():
    total = 0
    for i in range(0x20000):
        for j in range(16):
            total += 1
    return total

cProfile.run('loop()')
print("TOTAL: ", loop())
