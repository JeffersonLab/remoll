
C
C Routines to provide access to FORTRAN common block values
C via C-callable routines
C

      SUBROUTINE BG_GETVERTEX(MYVERTEX)
C
C---     Get vertex values specified in confguration file
C
      IMPLICIT NONE
C
      INCLUDE 'bg_ctrl.inc'

      REAL MYVERTEX(3)

      MYVERTEX(1) = VERTEX(1)
      MYVERTEX(2) = VERTEX(2)
      MYVERTEX(3) = VERTEX(3)
C
 999  RETURN
      END

