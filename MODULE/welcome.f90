MODULE welcome
CONTAINS        
!USE Prepare_Inputs        
SUBROUTINE Welcome_Message()
IMPLICIT NONE
WRITE(*,*)
WRITE(*,*)"!------------------------------------"
WRITE(*,*)"      WELCOME TO TASS REWEIGHTING    "  
WRITE(*,*)"!------------------------------------"
WRITE(*,*)
END SUBROUTINE Welcome_Message
END MODULE welcome
