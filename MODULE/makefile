#makefile
#===============================================================
#This makefile written to compile the unbiasing program	      
#and writting unbiased probabilities			      
#===============Written by Rahul Verma==========================
BASH = /bin/bash
INSTDIR = $(shell cd ../ && pwd)
FC = gfortran
FFLAGS = -g -O0 -fcheck=all
OMP = -fopenmp

OUTPUT =  GetSteps.o welcome.o Check_File.o Print_Data.o Plumed_Read.o Read_Files.o Prepare_Inputs.o Reweight_METAD.o Probability.o  TASS_REWIGHTING.o

reweight_tass.exe : $(OUTPUT)
	$(FC) $(FFLAGS) $(OMP) -o reweight_tass.exe $(OUTPUT) 

GetSteps.o : GetSteps.f90
	$(FC) -c GetSteps.f90

welcome.o : welcome.f90
	$(FC) -c welcome.f90

Check_File.o : Check_File.f90
	$(FC) -c Check_File.f90

Plumed_Read.o : Plumed_Read.f90
	$(FC) -c Plumed_Read.f90

Print_Data.o : Print_Data.f90
	$(FC) -c $(FFLAGS) Print_Data.f90

Read_Files.o : Read_Files.f90
	$(FC) -c $(FFLAGS) Read_Files.f90

Prepare_Inputs.o : Prepare_Inputs.f90
	$(FC) -c $(FFLAGS) Prepare_Inputs.f90

Reweight_METAD.o : Reweight_METAD.f90
	$(FC) -c $(FFLAGS) $(OMP) Reweight_METAD.f90

Probability.o : Probability.f90
	$(FC) -c $(FFLAGS) Probability.f90

TASS_REWIGHTING.o : TASS_REWIGHTING.f90
	$(FC) -c TASS_REWIGHTING.f90

clean   :
	rm *.o *.mod *.exe

distclean:
	rm $(INSTDIR)/src/*.o
	rm $(INSTDIR)/src/*.mod
	rm $(INSTDIR)/bin/reweight_tass.exe

install : reweight_tass.exe
	bash -c "mkdir -p $(INSTDIR)/bin"
	bash -c "mkdir -p $(INSTDIR)/src"
	@if [ -d $(INSTDIR) ];\
		then \
		cp reweight_tass.exe $(INSTDIR)/bin;\
		mv *.o *.mod $(INSTDIR)/src;\
		echo "Installed in $(INSTDIR)";\
	else \
		echo "Sorry, $(INSTDIR) does not exist";\
	fi
