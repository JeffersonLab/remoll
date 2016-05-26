* $Header:$
* $Log:$
*
      SUBROUTINE OMROTV(V1,ROT,V2)
C
C     ******************************************************************
C     *                                                                *
C     *       Vector rotation V1 ==> V2 using ROT matrix               *
C     *                                                                *
C     *    ==>Called by : OMKINE                                       *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
      REAL     V1(3),ROT(3,3),V2(3)
C
C#if defined OMGEANT_VERSION
C      CHARACTER*80 VersionString
C      DATA VersionString /
C     & '$Id:$'/
C#endif
C
      INTEGER   i,j
C
C     ------------------------------------------------------------------
C
      DO i=1,3
         V2(i)=0.
         DO j=1,3
            V2(i)=V2(i)+ROT(j,i)*V1(j)
         ENDDO
      ENDDO
C
      RETURN
      END






