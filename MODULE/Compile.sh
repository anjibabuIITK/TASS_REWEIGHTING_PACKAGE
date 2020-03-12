#!/bin/bash
gfortran -c GetSteps.f90 Prepare_Inputs.f90 TASS_REWIGHTING.f90
gfortran GetSteps.o Prepare_Inputs.o TASS_REWIGHTING.o -o reweight_tass.exe

case $1 in
    "clean") 
rm *.o *.mod *.exe admin_input.inp
echo "Cleaned";;

esac

#
