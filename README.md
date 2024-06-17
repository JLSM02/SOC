This repository aims to be a portfolio of my work during my Bachelor's Final Project in the Physics Degree.

-The .f90 file is a Fortran file, containig some subroutines which are the same physics simulation with different properties.

 	·standardsimulation: only stores data for size and time, it always use open boundary conditions. Faster
   
	·completesimulation: can adjust boundary conditions, stores all the variables. Slower
  
	·genanimdata: to be used with animationmaker.py
  
  	·nohomogsimulation: work in progress.

-The simulationX.py files are use to call the fortran subroutine several times and manage the data.

-The .pynb files are data analysis of physics simulations.

For compiling Fortran files to python modules type: python -m numpy.f2py -c --fcompiler=gnu95 --compiler=mingw32 <programName>.f90 -m <moduleName>
