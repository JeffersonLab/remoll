C
C---  Simulates "background" photoproduction by a coherent Bremsstrahlung beam
C     Reaction: gamma+p
C     Control flags are read from fort.15 (FFREAD)
C     E>3 GeV (can be changed in fort.15) - use PYTHIA
C     E<3 GeV - a coctail of several dominating photoproduction processes        
C
C     Includes: a) calculation of the coherent+incoherent photon energy spectra
C               b) parametrization for the cross sections:
C                  - full (formula fit to data)
C                  - p pi0, n pi+ - using SAID
C                  - p 2pi, n 2pi, p eta, p 3pi, n 3pi (formula fit to data)
C               c)  simulation if unbiased (equal weight) events in a given beam energy range: 
C                  - beam energy simulated (beam spectrum times the total cross section)
C                  - the process is chosen randomly accordingly to the their probabilities 
C
C---  Input: file "fort.15" 
C                      the number of events, the beam energy range, the distance to the collimator etc
C            file "particle.dat" contains a table for particle masses (GEANT numbering)
C
C
      SUBROUTINE BGGEN
C
      IMPLICIT NONE
C
      INCLUDE 'bg_ctrl.inc'
      INCLUDE 'bg_proc.inc'
      INCLUDE 'bg_evec.inc'
C
      INTEGER ierr,iev,ipri,i
      INTEGER nproc(0:MXPROC)
      CHARACTER cnam(0:MXPROC)*16,cmom*18
C
C     ------------------------------------------------------------------
C
      CALL BG_INI(ierr)
      IF(ierr.NE.0) GO TO 999
C
      DO i=0,MXPROC
         nproc(i)=0
      ENDDO
      IEVENT=0
C
      DO iev=1,NEVENT
C
         IEVENT=iev
         ipri=0
         IF(iev.LE.NPRIEV) ipri=1
C
         CALL BG_EVE(ipri)
C         
         IF(IEVPROC.GE.0.AND.IEVPROC.LE.MXPROC) THEN
            nproc(IEVPROC)=nproc(IEVPROC)+1
         ENDIF
C
      ENDDO
C
      WRITE(6,1980) 
 1980 FORMAT(///1X,100('='))
      WRITE(6,1990) NEVENT,RATESEC/1000.
 1990 FORMAT(' Events Simulated: ',I9,5X,'Reference interaction rate:'
     +      ,F12.2,' kHz')
      IF(NEVENT.GT.0) THEN
         cnam(0)='PYTHIA '
         DO i=1,MXPROC
            cnam(i)=CNPROC(i)
         ENDDO
         WRITE(6,2000) 
 2000    FORMAT('  process ',16X,'   events     fraction   range')
         WRITE(6,2005) 
 2005    FORMAT(1X,100('-'))
         DO i=0,MXPROC
            cmom=' '
            IF(i.EQ.0) THEN
               WRITE(cmom,2006) EPYMIN,EPH_LIM(2)
 2006          FORMAT(F5.2,'<E<',F5.2,' GeV')
            ELSE IF(i.EQ.1) THEN
               WRITE(6,2005) 
               WRITE(cmom,2006) EPH_LIM(1),EPYMIN
            ENDIF
            WRITE(6,2010) i,cnam(i),nproc(i)
     +                ,nproc(i)/REAL(nevent)*100.,cmom
         ENDDO
 2010    FORMAT(3X,I4,2X,A16,3X,I7,5X,F5.1,' %',4X,A18)
      ENDIF
      WRITE(6,2020) 
 2020 FORMAT(1X,100('-')///)
C
      CALL BG_END
C
 999  CONTINUE
C
      END

