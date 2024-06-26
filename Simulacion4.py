import sandpilefort as sdf
import numpy as np
import os

############################
#
# Obtencion de un cambio de fase, precharges, con condiciones cerradas, saca tamaños de frame
# Distribución ordenada a los 4 vecinos y a si mismo.
#
###########################

l = [32]
name="sim004"
limit = 12
maxframes = 1000
nPoints = 150
min = 0.3
max = 1
completationLevels = np.linspace(min, max, nPoints)

try:
    os.mkdir("Datos/"+name)
finally:
    try:
        os.remove("temp/FrameData"+name+".dat")
    finally:
           for k in l: 
            amount = k**2 * limit * completationLevels
            for i in amount:
                i = int(i)
                sdf.prechargedsimulation(l, limit, i, "ciclic",  True, name, maxframes)
                os.replace("temp/FrameData"+name+".dat", "Datos/"+name+"/FrameData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")
