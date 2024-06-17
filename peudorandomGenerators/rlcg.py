#Random Linear Congruential Generator

class Rlcg(): 
    #Base Class for instancing various indepent generators

    #Initialization
    def __init__(self,a,b,m,seed) -> None:
        self.a = a
        self.b = b 
        self.m = m
        self.nold = seed
        pass

    #Reassign seed
    def seed(self,s):
        self.nold = s
        pass

    #Generate a random number
    def gen(self):
        n = self.a*self.nold+self.b
        while n > self.m:
            n -= self.m
            pass
        self.nold = n
        return n

#Creates an instance of the Base Class and assign its instance methods to the module methods
_inst = Rlcg(1230,1201,1001,1)
seed = _inst.seed
gen = _inst.gen