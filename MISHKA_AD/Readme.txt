*MISHKA-AD
based on MISHKA-F / MISHKA-2

src : source file of MISHKA-A
lib : all other dependent libraries except libblas

namelist, output, mapping, plot : store input and output files


*Compile MISHKA:

$ make

If successful, an executable 'mishka.exe' will be automatically generated

NOTE: gfortran and blas library is required. (Included in PRL_PTM virtual machines)
If you don't have them, you can install gfortran and blas library by

$ sudo apt-get install gfortran
$ sudo apt-get install libblas-dev

*UPDATE - Compile MISHKA
We now switch to Intel FORTRAN Compiler, you can get it free online


*Run MISHKA:

1. copy mapping file from HELENA mapping directory into 'mapping' under this directory
2. make the namelist and store it into 'namelist' under this directory
3. run MISHKA

$ ./runmis mappingfile namelistfile

for example,

$ ./runmis tens_noflow.nl tens_noflow



