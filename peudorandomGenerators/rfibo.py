import rlcg

#Random Lagged Fibonocci Generator

class Rfibo(): 
    #Base Class for instancing various indepent generators

    #Initialization, NOTES: v<u, seed has as many elements as v value
    def __init__(self,a,b,c,u,v,m,seed) -> None:
        self.a = a
        self.b = b
        self.c = c
        self.u = u
        self.v = v 
        self.m = m
        self.nold = seed
        pass

    #Reassign seed, the argument is given to rlgc as its seed and the rfibo seed is calculated by rlgc.gen() element by element
    def seed(self,s):
        rlcg.seed(s)
        for i in range(0,self.v):
            self.nold[i]=rlcg.gen()
        pass


    #Generate a random number, uses the nold list to get previous values and shifts all of them to the right to insert the new generated
    def gen(self):
        n = self.a*self.nold[self.u-1]+self.b*self.nold[self.v-1]+self.c
        while n > self.m:
            n -= self.m
            pass
        self.nold.insert(0,n)
        self.nold.pop(self.v)
        return n

#Creates an instance of the Base Class and assign its instance methods to the module methods
_inst = Rfibo(1230,1201,639,1,3,1001,[32,89,3]) #Default propperties, if wanted you can create an instance with custom ones
seed = _inst.seed
gen = _inst.gen