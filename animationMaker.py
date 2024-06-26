import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import sandpilefort as sdf
import os 

limit = 4
l = 12
bdt = "closed"
frames = 4*12**2-1

#try:
    #os.remove("temp/AnimDataM.dat")
    #os.remove("temp/AnimDataA.dat")
    #os.remove("temp/AnimDataF.dat")
#finally:
    #pass

#sdf.genanimdata(l, limit, frames, bdt, False)

M = np.zeros((l, l), dtype=int)
Mcsv = np.loadtxt("temp/AnimDataM.dat", delimiter=",", max_rows=20000)

A = np.zeros((l, l), dtype=int)
Acsv = np.loadtxt("temp/AnimDataA.dat", delimiter=",", max_rows=20000)

F = np.zeros((l, l), dtype=bool)
Fcsv = np.loadtxt("temp/AnimDataF.dat", delimiter=",", max_rows=20000)


fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
cmap1 = plt.colormaps['jet']
cmap2 = mpl.colors.ListedColormap(["white", "red", "blue"])
norm1 = mpl.colors.BoundaryNorm(np.linspace(0, limit, num=limit+1), ncolors=cmap1.N)
norm2 = mpl.colors.BoundaryNorm((0, 1, 2, 3), ncolors=cmap2.N)
norm3 = mpl.colors.BoundaryNorm(np.linspace(0, 15, num=16), ncolors=cmap1.N)
fig.colorbar(mpl.cm.ScalarMappable(cmap=cmap1, norm=norm1), ax=ax1)
fig.colorbar(mpl.cm.ScalarMappable(cmap=cmap2, norm=norm2), ax=(ax2, ax3))

for frame in range(0,20000):
    for j in range(0, l):
        M[:, j] = Mcsv[frame, j*l : (j+1)*l]
        A[:, j] = Acsv[frame, j*l : (j+1)*l]
        F[:, j] = Fcsv[frame, j*l : (j+1)*l]
    ax1.clear()
    ax2.clear()
    ax3.clear()

    ax1.imshow(M, cmap=cmap1, norm = norm1)
    ax2.imshow(A, cmap=cmap1, norm = norm3)
    ax3.imshow(F, cmap=cmap2, norm = norm2)
    plt.pause(0.1)

plt.show()