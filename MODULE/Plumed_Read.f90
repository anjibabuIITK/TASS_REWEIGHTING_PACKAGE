MODULE Plumed_Read
CONTAINS
!---------------------------------
SUBROUTINE Read_Plumed_Input()
IMPLICIT NONE
LOGICAL::fexist

!Remove previous;y existed admin_input.inp file
inquire (file="admin_input.inp",exist=fexist)
       if(fexist) then
         write(*,*) " The file admin_input.inp has exists!"
      CALL SYSTEM("rm admin_input.inp")
       end if
!
OPEN(unit=100,file='ReadPlumedInput.sh',status='unknown')
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)"# This script is a part of TASS reweighting codes"
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)'#'
WRITE(100,*)'# Authour: Anji Babu Kapakayala'
WRITE(100,*)'#          C/O Prof. Nisanth N. Nair'
WRITE(100,*)'#          Dept. Of Chemistry, IIT Kanpur, India.'
WRITE(100,*)'#'
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)'#'
WRITE(100,*)'#!/bin/bash'
WRITE(100,*)'cp plumed.dat dummy.dat'
WRITE(100,*)'sed -i "/^#/d" dummy.dat'
WRITE(100,*)"awk -F",'" "', " '{"
WRITE(100,*)'       for(i=1;i<=NF;i++) {'
WRITE(100,*)'{ split($i, a, "=")'
WRITE(100,*)'  if(a[1] == "BIASFACTOR" || a[1] == "PACE" ||a[1] == "STRIDE" ) {'
WRITE(100,*)'#               print a[1] " = "  a[2]'
WRITE(100,*)'                print a[1] " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[1] == "METAD"){'
WRITE(100,*)'print a[1] " = "  "ON" >> "admin_input.inp"'
WRITE(100,*)'} else if (a[2] == "COLVAR") {'
WRITE(100,*)'print "CVFILE" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[2] == "HILLS") {'
WRITE(100,*)'print "HILLSFILE" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[1] == "TEMP") {'
WRITE(100,*)'print "CV_TEMP" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'}'
WRITE(100,*)
WRITE(100,*)'}'
WRITE(100,*)'  }'
WRITE(100,*)"}' dummy.dat"
WRITE(100,*)"rm dummy.dat"
WRITE(100,*)'# --------------------------------------------------# '

CLOSE(100)
CALL SYSTEM("sh ReadPlumedInput.sh")
CALL SYSTEM("cat input.inp >> admin_input.inp")

END SUBROUTINE
END MODULE Plumed_Read
