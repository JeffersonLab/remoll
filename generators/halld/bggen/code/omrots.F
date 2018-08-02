* $Header:$
* $Log:$
*
      SUBROUTINE OMROTS(V,ROT)
C
C     ******************************************************************
C     *                                                                *
C     *       Fill a rotation matrix V=ROT*V1                          *
C     *  INPUT: V - 3 vector                                           *
C     * OUTPUT: ROT  - rotation matrix (V1 looks along Z)              *
C     *                                                                *
C     *    ==>Called by : kinematics programs                          *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
      REAL     V(3),ROT(3,3)
C
C#if defined OMGEANT_VERSION
C      CHARACTER*80 VersionString
C      DATA VersionString /
C     & '$Id:$'/
C#endif
C
      INTEGER   i,j,mn,mx,m3
      REAL ptot,vn(3),vx(3),q
C
C     ------------------------------------------------------------------
C
      ptot=SQRT(V(1)**2+V(2)**2+V(3)**2)
      IF(ptot.GT.0.) THEN

         DO i=1,3
            vn(i)=V(i)/ptot
            ROT(3,i)=vn(i)
         ENDDO
C
C---     Define X-Y (arbitrary) - a vector normal to vn
C
         mn=1
         DO i=2,3
            IF(ABS(vn(i)).LT.ABS(vn(mn))) mn=i
         ENDDO
         mx=3
         DO i=1,2
            IF(ABS(vn(i)).GT.ABS(vn(mx))) mx=i
         ENDDO
         m3=mn+1
         IF(m3.GT.3) m3=m3-3
         IF(m3.EQ.mx) m3=m3+1
         IF(m3.GT.3) m3=m3-3
C---       condition:   vx*vn=0.
         vx(mx)=0.
         vx(mn)=1.
         vx(m3)=0.
         IF(vn(m3).NE.0.) vx(m3)=-vn(mn)*vx(mn)/vn(m3)
         q=SQRT(1.+vx(m3)**2)
         DO i=1,3
            vx(i)=vx(i)/q
         ENDDO
C
         DO i=1,3
            ROT(1,i)=vx(i)
         ENDDO
C
C---       Y-coord
C
         ROT(2,1)= vn(2)*vx(3)-vn(3)*vx(2)
         ROT(2,2)=-vn(1)*vx(3)+vn(3)*vx(1)
         ROT(2,3)= vn(1)*vx(2)-vn(2)*vx(1)
C
      ELSE
         DO i=1,3
            DO j=1,3
               IF(j.EQ.i) THEN
                  ROT(j,i)=1.
               ELSE
                  ROT(j,i)=0.
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END
