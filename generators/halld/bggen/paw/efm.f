      REAL FUNCTION EFM(K1,K2,K3,K4)
C
C---      Eff mass of the particles K1-K4 (=0 - not used)
C
      IMPLICIT NONE
      INTEGER K1,K2,K3,K4
C
      INCLUDE ?
C
      INTEGER nm,im(4),i,j,k
      REAL bm(4),pm(4,4),pouta(4),ef
C
      EFM=0.
      nm=0
      DO i=1,4
         IF(i.EQ.1) k=k1
         IF(i.EQ.2) k=k2
         IF(i.EQ.3) k=k3
         IF(i.EQ.4) k=k4
         IF(k.GT.0.AND.k.LE.NP) THEN
            nm=nm+1
            bm(nm)=AM(k)
            DO j=1,4
               pm(j,nm)=POUT(j,k)
            ENDDO
         ENDIF
      ENDDO
C
      IF(nm.GE.2) THEN
         CALL EFMASS(nm,bm(1),pm(1,1),ef,pouta)
         EFM=ef
      ENDIF
C
      END
C
      INCLUDE 'efmass.f'
