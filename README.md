This repository aims to be a portfolio of my work during my Bachelor's Final Project in the Physics Degree.

-The .f90 file is a Fortran file, containig some subroutines which are the same physics simulation with different properties:

 	·standardsimulation: only stores data for size and time, it always use open boundary conditions. Faster
   
	·completesimulation: can adjust boundary conditions, stores all the variables. Slower
  
	·genanimdata: to be used with animationmaker.py
  
  	·prechargedsimulation: used to study phase transitions, first initilize the system with a given amount of sand, then simulate during some frames

-The simulationX.py files are used to call the fortran subroutine several times and manage the data.

-The analisisX.ipynb notebooks are used for data analysis of physics simulations.

For compiling Fortran files to python modules on Windows type: python -m numpy.f2py -c --fcompiler=gnu95 --compiler=mingw32 <programName>.f90 -m <moduleName>


Here are some resources that I found useful during the development:

	·Information about  RANDOM_NUMBER subroutine in fortran: https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fNUMBER.html

	·Information about data types and formatting, Fortran: https://www.math.hawaii.edu/~hile/fortran/fort3.htm

	·How to write on files, Fortran: https://www.tutorialspoint.com/fortran/fortran_file_input_output.htm
