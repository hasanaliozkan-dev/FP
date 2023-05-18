def calc1(l):
    return [x**2 + x/2 for x in l]
def calc2(l):
    return [3*x + 2/x for x in l]
    
    
obs = [3.4,5.8,7.3,8.8,8.1,12.5,11.0]
x = [0.7,1.6,1.9,2.4,2.8,3.3,3.7]
print("1 ",calc1(x))
print("2 ",calc2(x))
res1 = [x-y for x,y in zip(obs,calc1(x))]
res2 = [x-y for x,y in zip(obs,calc2(x))]

print("1 r",res1)
print("2 r",res2)