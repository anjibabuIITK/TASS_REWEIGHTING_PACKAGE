MODULE Check_File
CONTAINS
!Subroutine to check existance of given file
SUBROUTINE Is_File_Exist(givenfile)
IMPLICIT NONE
CHARACTER(len=10)::givenfile
LOGICAL::fexist

inquire (file=givenfile,exist=fexist)
if(fexist) then
         write(*,*) " The file"," ",givenfile," exists!"
       else
         write(*,*) " The file"," ",givenfile," does not exist!"
         stop
       end if
END SUBROUTINE
END MODULE Check_File
