#makefile

#This makefile written to compile the unbiasing program
#and writting unbiased probabilities

reweight_tass.exe : GetSteps.o Prepare_Inputs.o TASS_REWIGHTING.o
	gfortran -o reweight_tass.exe GetSteps.o Prepare_Inputs.o TASS_REWIGHTING.o

GetSteps.o : GetSteps.f90
	gfortran -c GetSteps.f90

Prepare_Inputs.o : Prepare_Inputs.f90
	gfortran -c Prepare_Inputs.f90

TASS_REWIGHTING.o : TASS_REWIGHTING.f90
	gfortran -c TASS_REWIGHTING.f90

clean :
	rm *.o reweight_tass.exe
