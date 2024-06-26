import sandpilefort as sdf
import os
import time

############################
#
# 2º apartado variaciones, Standard-> CC abiertas, solo saca tamaños y duraciones
# Distribución aleatoria a 5 celdas.
#
###########################

statistics = [10000]
l = [8, 16, 32]
name="sim003"
limit = 5

try:
    os.mkdir("Datos/"+name)
finally:
    try:
        os.remove("temp/FrameData"+name+".dat")
        os.remove("temp/AdditionData"+name+".dat")
    finally:

        for i in statistics:
            for k in l:
                sdf.standardsimulation(k, limit, i, True, name)
                time.sleep(1)
                print(l)
                os.replace("temp/FrameData"+name+".dat", "Datos/"+name+"/FrameData_limit"+str(limit)+"Random_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")
                os.replace("temp/AdditionData"+name+".dat", "Datos/"+name+"/AdditionData_limit"+str(limit)+"Random_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")