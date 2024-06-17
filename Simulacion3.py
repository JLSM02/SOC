import sandpilefort as sdf
import os
import time

############################
#
# 2º apartado variaciones, Standard -> CC abiertas, saca tmaños , duraciones y areas.
# Distribución ordenada a los 8 vecinos.
#
###########################

statistics = [10000]
l = [64]
name="sim003"
limit = 8

try:
    os.mkdir("Datos/"+name)
finally:
    try:
        os.remove("temp/FrameData"+name+".dat")
        os.remove("temp/AdditionData"+name+".dat")
    finally:

        for i in statistics:
            for k in l:
                sdf.standardsimulation(k, limit, i, False, name)
                time.sleep(1)
                print(l)
                os.replace("temp/FrameData"+name+".dat", "Datos/"+name+"/FrameData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")
                os.replace("temp/AdditionData"+name+".dat", "Datos/"+name+"/AdditionData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")

