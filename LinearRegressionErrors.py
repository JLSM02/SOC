
import numpy as np

def mErr(m, R2, N):

    return abs(m/R2 * np.sqrt((1-R2**2)/(N-2)))



def nErr(m, R2, N, x):
    return abs(m/R2 * np.sqrt((1-R2**2)*(np.dot(x,x))/(N-2)))

