#!/bin/bash
gfortran -c Read_Files.f90 GetSteps.f90 Prepare_Inputs.f90 TASS_REWIGHTING.f90
gfortran Read_Files.o GetSteps.o Prepare_Inputs.o TASS_REWIGHTING.o -o reweight_tass.exe

case $1 in
    "clean") 
rm *.o *.mod *.exe admin_input.inp
echo "Cleaned";;

esac

#
