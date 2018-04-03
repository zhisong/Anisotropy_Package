      PROGRAM MAPPINGCONV
      PARAMETER(NGMAX=1001, NCHIMAX=512, NPNC=NGMAX*NCHIMAX)
      PARAMETER(NMAP=10, NOUT=20)
      PARAMETER(IEQ=4, DSURF=0.D0, DSURF1=0.0D0)
      PARAMETER(DSURF2=0.D0, DSURF3=0.D0, DSURF4=0.D0)

      INTEGER      IARG
      CHARACTER*16 OUTFILENAME, INFILENAME

      INTEGER JS0, NCHI, NPSI
      DOUBLE PRECISION DQEC, DJ0, DJE, CPSURF, RADIUS, RAXIS, EPS,
     >                 DRBPHI0, DRBPHIE, DP0, DPE, ASPI
      DOUBLE PRECISION CS(NGMAX), QS(NGMAX), DQS(NGMAX), CURJ(NGMAX)
      DOUBLE PRECISION P0(NGMAX), RBPHI(NGMAX),RHO(NGMAX)
      DOUBLE PRECISION GEM11(NPNC),GEM12(NPNC), GEM33(NPNC)
      DOUBLE PRECISION CHI(NCHIMAX), VX(NCHIMAX), VY(NCHIMAX)

      DOUBLE PRECISION DATAOUT(NPNC)

      IARG = IARGC()
      IF (IARG.GE.1) THEN
         CALL GETARG(1, INFILENAME)
      ELSE
         INFILENAME = 'mapping.in'
      ENDIF
      IF (IARG.GE.2) THEN
         CALL GETARG(2, OUTFILENAME)
      ELSE
         OUTFILENAME = 'mapping.out'
      ENDIF
      WRITE(*,*) 'READ FROM FILE : ', INFILENAME
      WRITE(*,*) 'WRITE TO  FILE : ', OUTFILENAME

      OPEN(UNIT=NMAP,FILE=INFILENAME ,ACTION='READ')
      OPEN(UNIT=NOUT,FILE=OUTFILENAME,ACTION='WRITE')
C-------------- READING MAPPING FILE ----------------------
      READ(NMAP,*) JS0,(CS(JS),JS=1,JS0+1),
     >             (QS(JS),JS=1,JS0+1),DQS(1),DQEC,(DQS(JS),JS=2,JS0+1),
     >             (CURJ(JS),JS=1,JS0+1),DJ0,DJE,
     >             NCHI,(CHI(JC),JC=1,NCHI),
     >             (GEM11(J),J=NCHI+1,(JS0+1)*NCHI),
     >             (GEM12(J),J=NCHI+1,(JS0+1)*NCHI),
     >             CPSURF,RADIUS
      NPSI = JS0+1
      READ(NMAP,*) (GEM33(J),J=NCHI+1,(JS0+1)*NCHI),RAXIS
      READ(NMAP,*) (P0(JS),JS=1,NPSI),DP0,DPE,
     >             (RBPHI(JS),JS=1,NPSI),DRBPHI0,DRBPHIE
      READ(NMAP,*) (VX(JS),JS=1,NCHI)
      READ(NMAP,*) (VY(JS),JS=1,NCHI)
      READ(NMAP,*) EPS
C-------------- CALCULATE DENSITY --------------------------
      DO J = 1, JS0+1
         IF(IEQ.EQ.2) THEN
            RHO(J) = (1.-DSURF1*CS(J)**2)*(1.+DSURF*CS(J)**2)
         ELSEIF(IEQ.EQ.3) THEN
            RHO(J) = 1.+DSURF*CS(J)**2+DSURF1*CS(J)**4
     >           +DSURF2*CS(J)**6+DSURF3*CS(J)**8
         ELSEIF(IEQ.EQ.4) THEN
            RHO(J) = 1.+DSURF*CS(J)     + DSURF1*CS(J)**2
     >           +DSURF2*CS(J)**3 + DSURF3*CS(J)**4
         ELSE
            RHO(J)  = 1.
         ENDIF
      ENDDO
C--------------WRITING MAPPING FILE -----------------------
      WRITE(NOUT,8) JS0
      WRITE(NOUT,8) NCHI
      WRITE(NOUT,7) CPSURF,RADIUS
      WRITE(NOUT,9) RAXIS

      WRITE(NOUT,6) (CS(JS),JS=1,JS0+1)
      WRITE(NOUT,6) (CHI(JS),JS=1,NCHI)
      WRITE(NOUT,6) (QS(JS),JS=1,JS0+1)   
      
C     WRITE(NOUT,*) '===== GEM11 ====='
      WRITE(NOUT,6) (GEM11(JS),JS=NCHI+1,(JS0+1)*NCHI)
C     WRITE(NOUT,*) '===== GEM12 ====='
      WRITE(NOUT,6) (GEM12(JS),JS=NCHI+1,(JS0+1)*NCHI)
C     WRITE(NOUT,*) '===== GEM33 ====='
      WRITE(NOUT,6) (GEM33(JS),JS=NCHI+1,(JS0+1)*NCHI)
      
C     WRITE(NOUT,*) '===== DENSITY ====='
      DO J = 1, NPSI
         DO I = 1, NCHI
            DATAOUT((J-1)*NCHI+I) = RHO(J)
         ENDDO
      ENDDO
      WRITE(NOUT,6) (DATAOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
      WRITE(NOUT,9) DATAOUT(1)
C     WRITE(NOUT,*) '===== PPAR ====='
      DO J = 1, NPSI
         DO I = 1, NCHI
            DATAOUT((J-1)*NCHI+I) = P0(J)
         ENDDO
      ENDDO
      WRITE(NOUT,6) (DATAOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
      WRITE(NOUT,9) DATAOUT(1)
C     WRITE(NOUT,*) '===== PPER ====='
      WRITE(NOUT,6) (DATAOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
      WRITE(NOUT,9) DATAOUT(1)
C     WRITE(NOUT,*) '===== RBPHI ====='
      DO J = 1, NPSI
         DO I = 1, NCHI
            DATAOUT((J-1)*NCHI+I) = RBPHI(J)
         ENDDO
      ENDDO
      WRITE(NOUT,6) (DATAOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
      WRITE(NOUT,9) DATAOUT(1)
c     WRITE(NOUT,*) '===== OMEGA ====='
      DO J = 1, NPSI
         DATAOUT(J) = 0.D0
      ENDDO
      WRITE(NOUT,6) (DATAOUT(JS), JS=1, JS0+1)
      WRITE(NOUT,7) 0.D0, 0.D0
c      WRITE(NOUT,*)
C----------------------------------------- ADDITIONAL DATA FOR VACUUM --
      WRITE(NOUT,6) (VX(JS),JS=1,NCHI)
      WRITE(NOUT,6) (VY(JS),JS=1,NCHI)
      WRITE(NOUT,9) EPS

      CLOSE(NMAP)
      CLOSE(NOUT)

    6 FORMAT(4E16.8)
    7 FORMAT(2E16.8)
    8 FORMAT(I5)
    9 FORMAT(E16.8)
 11   FORMAT(3E16.8)      
      END PROGRAM MAPPINGCONV
