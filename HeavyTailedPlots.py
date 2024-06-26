import numpy as np
import matplotlib.pyplot as plt


x = np.linspace(1,1000,1001)
beta = 1.5
Pareto = x**-beta

mu1 = 1
sigma1 = 3
LogNormal = 1/x *np.exp(-(np.log(x)-mu1)**2/2/sigma1**2)

mu2=-80
sigma2=5
LogCauchy = 1/x/(1+(np.log(x)-mu2)**2/2/sigma2**2)

plt.plot(x, Pareto, linewidth=2, label="Pareto")
plt.plot(x, LogNormal,"--", linewidth=2, label="Log-normal")
plt.plot(x, LogCauchy,"-.", linewidth=2, label="Log-Cauchy")

plt.xscale("log")
plt.yscale("log")
plt.legend()
plt.title("Escalas Heavy-tailed")
plt.xlabel(r"$x$")
plt.ylabel(r"$p(x)$")
plt.show()
