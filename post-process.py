#script(python)
import clingo
import sys
count = 0
aSets = {}

def sorter(x):
    if len(x.arguments) == 0:
        return sys.maxsize
    return x.arguments[-1].number

def setExists(z):
    global aSets
    for x in aSets:
        if isSame(z,aSets[x]):
            return True
    return False
    
def termExists(x,set):
    for term in set:
        if str(x) == str(term):
            return True
    return False

def isSame(xSet,ySet):
    for x in xSet:
        if termExists(x,ySet) == False:
            return False
    return True      

def onModel(x):
    global count
    #print count
    #print (x.symbols(shown=True)[0].arguments[0].number)
    z = x.symbols(shown=True)
    #z.sort(key=sorter)
    #print str(z)
    if aSets == {}:
        aSets[count] = z
        count += 1
        print('model(' + str(z) + ').\n')
    else:
        if setExists(z) == False:
            aSets[count] = z
            count += 1
            print('model(' + str(z) + ').\n')
           
            
def main(prg):
    prg.ground([("base", [])])
    prg.solve(on_model=onModel)
#end.
