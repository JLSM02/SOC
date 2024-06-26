import sandpilefort as sdf
import os

############################
#
# 1º apartado variaciones, Complete con CC abiertas, saca tmaños , duraciones y areas.
# Distribución aleatoria con límite 3.
#
###########################

statistics = [1000]
l = [64]
bdt = "open"
name="sim002"
limit = 3

try:
    os.mkdir("Datos/"+name)
finally:
    try:
        os.remove("temp/FrameData"+name+".dat")
        os.remove("temp/AdditionData"+name+".dat")
    finally:

        for i in statistics:
            for k in l:
                sdf.completesimulation(k, limit, limit, i, bdt, True, name)
                print(l)
                os.replace("temp/FrameData"+name+".dat", "Datos/"+name+"/FrameData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")
                os.replace("temp/AdditionData"+name+".dat", "Datos/"+name+"/AdditionData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")

