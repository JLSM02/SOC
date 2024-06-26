import sandpilefort as sdf
import os

############################
#
# Simulación principal, Standard-> CC abiertas, solo saca tamaños y duraciones
# Distribución aleatoria con límite 3.
#
###########################

statistics = [10000]
l = [4, 8, 16, 32]
name="sim001"
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
                sdf.standardsimulation(k, limit, i, True, name)
                print(l)
                os.replace("temp/FrameData"+name+".dat", "Datos/"+name+"/FrameData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")
                os.replace("temp/AdditionData"+name+".dat", "Datos/"+name+"/AdditionData_limit"+str(limit)+"_Statistic"+str(i)+"_"+str(k)+"x"+str(k)+".dat")

