      MODULE PARAM
      PARAMETER (NRMAX = 151,  NPMAX = 301)                            
      PARAMETER (MAXNODE=NRMAX*NPMAX,MBMAX=1026)         
      PARAMETER (NRMMAX = 1601, NPMMAX =1026)                            
      PARAMETER (MAXMNODE = NRMMAX*NPMMAX)                              
      PARAMETER (NPTSMAX = 1601)
      PARAMETER (NBOUNDMAX = 2048)
      END  
C-----------------------------------------------------------------------
      MODULE COMMAX
      PARAMETER (NPSIMAX=2001, NCHIMAX=1026, NMAX=NPSIMAX)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX)
      END
C-----------------------------------------------------------------------
      MODULE COMPIO
      PARAMETER (NIN=10, NOUT=20, NMAP=12)
      END                             
C-----------------------------------------------------------------------
      MODULE CORNERS
      USE PARAM
      DOUBLE PRECISION     RS(4,2)
      INTEGER IJ(4,2), NODENO(MAXMNODE,4)
      END
C-----------------------------------------------------------------------
      MODULE GAUDSINT
      DOUBLE PRECISION  XGAUSS(4),WGAUSS(4)
      DOUBLE PRECISION  H(4,4,4,4),HR(4,4,4,4),HS(4,4,4,4),HRS(4,4,4,4)
      END
C-----------------------------------------------------------------------
      MODULE COMDAT
      DOUBLE PRECISION     ELLIP,TRIA,QUAD,PAR1,PAR2,PAR3,PAR4,
     >        AGA,BGA,CGA,DGA,EGA,FGA,GGA,HGA,
     >        API,BPI,CPI,DPI,EPI,FPI,GPI,HPI,
     >        AOM,BOM,COM,DOM,EOM,FOM,GOM,HOM,
     >        ATE,BTE,CTE,DTE,ETE,FTE,GTE,HTE,
     >        ATH,BTH,CTH,DTH,ETH,FTH,GTH,HTH,
     >        ACUR,BCUR,CCUR,DCUR,ECUR,FCUR,GCUR,HCUR,
     >        ERRIT,ERRCUR,EPS,ALFA,B,C,XIAB,Q95,BETAP,AMIX,BMIX,
     >        ABB, BBB, AMPL, RVAC,BVAC,ZEFF,ZN0,RPE,ETAEI,
     >        OMGOT, HOT, THTOF
      INTEGER IAS,IAV,ICUR,NRCUR,NPCUR,NMESH,NBB,NQB,
     >        MHARM,ISHAPE,ISOL,IGAM,IPAI,NR,NP,NRMAP,NPMAP,NITER,
     >        IOMG, ITE, ITH
      END
C-----------------------------------------------------------------------
      MODULE COMMAP
      USE PARAM
      DOUBLE PRECISION  CS(NRMMAX),QS(NRMMAX),
     >     DQS(NRMMAX),CURJ(NRMMAX),CHI(NPMMAX),   
     >     GEM11(MAXMNODE),GEM12(MAXMNODE),GEM33(MAXMNODE),  
     >     CHIKN(NPMMAX),PPEROUT(MAXMNODE),PPAROUT(MAXMNODE),
     >     RHOOUT(MAXMNODE),RBPHI(MAXMNODE),P0(MAXMNODE),
     >     DP(NRMMAX),DRBPHI(NRMMAX),RBPHIOUT(MAXMNODE), OMGOUT(NPMMAX),
     >     DQEC,DJ0,DJE,CPSURF,RADIUS,RAXIS,RBPHI0,OMGOUT0,OMGOUTE
     >     RHO0, PPER0, PPAR0
      INTEGER JS0,NCHI,NPSI  
      END                             
C-----------------------------------------------------------------------
      MODULE COMPRI
      INTEGER NPR1,NPR2,NROUT,NDIAG,NFCHECK,NAVG
      END
C-----------------------------------------------------------------------
      MODULE COMPLO
      INTEGER       NPL1
      INTEGER       NXGRID, NYGRID
      CHARACTER*100 TXTOUT(40)
      END
C-----------------------------------------------------------------------
      MODULE COMANG
      DOUBLE PRECISION  ANGLE
      END
C-----------------------------------------------------------------------
      MODULE MESH
      USE PARAM
      DOUBLE PRECISION XXOLD(4,MAXNODE),YYOLD(4,MAXNODE),
     >                 PSIOLD(4*MAXNODE)
      END
C-----------------------------------------------------------------------
      MODULE MESHAC
      USE PARAM
      DOUBLE PRECISION     AMESH,BMESH,CMESH
      DOUBLE PRECISION     XR1,SIG1,XR1DONE,SIG1DONE
      DOUBLE PRECISION     SG(NRMMAX),DSG(NRMMAX),DDSG(NRMMAX)
      INTEGER IMESH,IARC,NRDONE
      END
C-----------------------------------------------------------------------
      MODULE NODES
      USE PARAM
      DOUBLE PRECISION     PSIKN(NRMMAX),THTKN(NPMMAX),RADPSI(NRMMAX) 
      DOUBLE PRECISION     DPSIKN(NRMMAX),DDPSIKN(NRMMAX)
      END                               
C-----------------------------------------------------------------------
      MODULE TOLERA
      DOUBLE PRECISION     PSITOL,THTTOL,TOL
      END
C-----------------------------------------------------------------------
      MODULE FF
      DOUBLE PRECISION     CPSI,CTHT,XAXIS,YAXIS,XAXISOLD,YAXISOLD
      INTEGER N1,N2,N3,N4
      END
C-----------------------------------------------------------------------
      MODULE FAXIS
      USE PARAM
      DOUBLE PRECISION     PSI(4*MAXMNODE)
      INTEGER NAXIS
      END
C-----------------------------------------------------------------------
      MODULE COMOUT
      DOUBLE PRECISION     BETAPL,BETA
      END
C-----------------------------------------------------------------------
      MODULE COMPROF
      USE PARAM
      DOUBLE PRECISION VH(NPTSMAX),VF2(NPTSMAX),VTH(NPTSMAX),
     >        VOM2(NPTSMAX),VTE(NPTSMAX),QIN(NPTSMAX),
     >        DPRES(1001),DGAM(1001),PINT(1001),GINT(1001),
     >        DOMG(1001),DTEM(1001),OMINT(1001),TEINT(1001),
     >        THINT(1001),DTHE(1001)
      INTEGER NPTS
      END          
C-----------------------------------------------------------------------
      MODULE COMSOLV
c      USE PARAM
c      DOUBLE PRECISION  KKBIG(KKLDA,4*MAXNODE)
      DOUBLE PRECISION, ALLOCATABLE :: KKBIG(:,:)
      INTEGER KKLDA
      END
C-----------------------------------------------------------------------
      MODULE COMB02
      USE COMMAX
      DOUBLE PRECISION     B02(NPNC), DTB02(NPNC), DSB02(NPNC)
      END    
C-----------------------------------------------------------------------
      MODULE COMPQ
      USE COMMAX
      DOUBLE PRECISION CP0(NPNC),CP1(NPNC),CP2(NPNC),CQ0(NPNC),CQ1(NPNC)
      INTEGER NCPQ
      END
C-----------------------------------------------------------------------
      MODULE COMSPL
      USE COMMAX
      DOUBLE PRECISION  Q1(NPSIMAX), Q2(NPSIMAX), Q3(NPSIMAX), 
     >         Q4(NPSIMAX),
     >         P1(NPSIMAX), P2(NPSIMAX), P3(NPSIMAX), P4(NPSIMAX),
     >         RBP1(NPSIMAX), RBP2(NPSIMAX), RBP3(NPSIMAX),
     >         RBP4(NPSIMAX)
      END   
C-----------------------------------------------------------------------
      MODULE COMNAM
      DOUBLE PRECISION      QAXIS,TBB,TBF
      END
C-----------------------------------------------------------------------
      MODULE COMPIE
      DOUBLE PRECISION  PI
      END
C      
************************************************************************
*DECK HELENA
      PROGRAM HELENA 
C-----------------------------------------------------------------------
C
C MAIN PROGRAM HELENA :        (VERSION 9  DATE 28-09-95)
C                          WITH ANISOTROPY AND T-FLOW   DATE MAR 2013
C ---------------------
C
C      - SOLVES THE 2D GRAD-SHAFRANOV EQUATION FOR  ARBITRARY UP/DOWN
C        SYMMETRIC CONTINUOUS PLASMA BOUNDARIES AND EQUILIBRIUM PRESSURE
C        AND GAMMA PROFILES.
C      - 2D CUBIC ISOPARAMETRIC FINITE ELEMENTS ARE USED FOR AN ACCURATE
C        REPRESENTATION OF THE SOLUTION.
C      - THE FINAL SOLUTION IS OBTAINED AN A FLUXSURFACE GRID.
C
C      - HELENA RUNNING ON CRAY
C
C
C PROGRAM ORGANIZATION :
C ----------------------
C
C        BLOCKDATA                       : INITIALIZE NAMELIST VAR.
C        HELENA                          : MAIN PROGRAM
C          (XUFLOW)                      : IBM ERROR HANDLING
C          INIVAL                        : INITIALIZE VALUES
C       x  OUT                           : PRINT OUTPUT UNIT 20
C          GAUSS                         : INITIALIZE GAUSSIAN POINTS
C            CUBICH                      : DEF. CUBIC ELEMENTS
C          SOLSHP                        : BOUNDARY SHAPE SOLOVIEV EQ.
C            (ZERO)                      : HGOLIB ROUTINE
C            FSOL
C              (RFT2)                    : HGOLIB ROUTINE
C          FSHAPE
C            (GRIDINV)                   : HGOLIB ROUTINE
C            (RFT2)
C          ELMNO                         : INITIALIZE ELEMENT NUMBERING
C          INIGRID                       : DEF. INITIAL GRID
C            RADB                        : RADIAL PROFILE OF ELEMENTS
C          (BEGPLT)                      : PPPLIB ROUTINE
C          (LBLTOP)                      : PPPLIB ROUTINE
C          (LBLBOT)                      : PPPLIB ROUTINE
C          PLOTGR                        : PLOT A ISOPARAMETRIC GRID
C            (NFRAME)                    : PPPLIB ROUTINE
C            PLOTCU                      : PLOT CUBIC LINE IN X,Y PLANE
C              CUB1D                     : 1D CUBIC INTERPOLATION
C              (LPLOT6)                  : PPPLIB ROUTINE
C          INITKQ                        : INIT. MATRIX KK AND VECTOR Q
C          FORMKQ                        : CALC. MATIX KK AND VECTOR Q
C           +CALCRJPHI                   : CALC. RHS OF GSE
C           +CALCBTPD                    : CALC. BTOT, TPER, DELTA
C            DPDPSI                      : DERIVATIVE OF PRESSURE
C            DGDPSI                      :     ,,     OF GAMMA
C            INTERP                      : CUBIC ELM. INTERPOLATION
C          BOUNKQ                        : INSERT BOUNDARY COND.
C            INTERP
C            DELRC                       : DELETE ROWS AND COLUMNS
C          SOLVE                         : SOLVE MATRIX EQUATIONS
C            CONJGR                      : CONJUGATE GRADIENTS
C              SHRINK                    : REMOVE ROWS/COLUMNS B.C.
C              SCALE                     : SCALE MATRIX PROBLEM
C              ASUB                      : MATRIX VECTOR INPRODUCT
C                DEXPAND                  : INSERT ROWS/COLUMNS B.C.
C                SHRINK
C           RESTORE                      : INSERT B.C. IN SOLUTION
C           FINDAXIS                     : FIND MAGNETIC AXIS
C             ROOT                       : SOLVE QUADRATIC EQUATION
C             CUB1D
C           NORMAL                       : NORMALIZE PSI SOLUTION
C           REMESH                       : CALC. NEW ISOPARAMETRIC MESH
C             RADMESH
C               RPACK
C               DRPACK
C               DDRPACK
C             PSIMIMA                    : FIND MINIMUM PSI IN ELM.
C               CUB1D
C             THTMIMA                    : FIND MINIMUM THETA IN ELM.
C             POSITION                   : FIND PSI/THETA POINT IN ELM.
C               (C05ZAF)                 : NAG ROUTINE
C               FZERO
C                 INTERP
C               (C05PBF)                 : NAG ROUTINE
C             POSBND                     : FIND PSI/THETA POINT BOUNDARY
C               SOLVP3                   : SOLVE CUBIC EQUATION
C             INTERP
C             RADMESH
C           MAPPING                      : CALC. METRIC FLUX COORD.
C             PROFILES                   : EQ. PROFILES
C               PRES                     : PRESSURE
C               XGAMMA                   : GAMMA
C             RADMESH
C             INTERP
C             PLOTM
C             SOLVP3
C             (LPLOT6)                   : PPPLIB ROUTINE
C           (FINPLT)                     : PPPLIB ROUTINE
C           DIAGNO                       : CALC. BETA, BETAPL
C
C
C THE NUMBERING USED FOR THE INTERPOLATING FUNCTIONS H :
C
C                R0,S0
C         H    H
C         H    H        WITH I,J = 0 OR 1 INDICATES A DERIVATIVE
C         HHHHHH        WITH RESPECT TO R,S. R0 AND S0 REFER TO THE
C         H    H        4 NODES OF ONE ELEMENT AND CAN BE +1. OR -1.
C         H    H
C                I,J
C                                S
C                                .
C                             +1 .
C                    N2----------.-----------N3
C                     |          .           |
C                     |          .           |
C                     |          .           |        --> R
C           .......-1.|......................|.+1.......
C                     |          .           |
C                     |          .           |
C                     |          .           |
C                     |          .           |
C                    N1----------.-----------N4
C                                . -1
C                                .
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMMAP
      USE COMPRI
      USE COMPLO
      USE CORNERS
      USE COMPROF
      USE TOLERA
      USE MESH
      USE MESHAC
      USE FAXIS
      USE COMNAM
      USE COMSOLV
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,MAXMNODE),YY(4,MAXMNODE),FR(2*MBMAX+2),
     >                  RBOUND(2*MBMAX+2)
      DOUBLE PRECISION  QQ(4*MAXNODE),DIAG(4*MAXNODE)
      DOUBLE PRECISION  PSPLOT(101),DPPLOT(101),DGPLOT(101),
     >                  OMGPLOT(101), TEMPLOT(101),
     >                  ZJPLOT(101),QPLOT(101)
      DOUBLE PRECISION  DF2OLD(NPTSMAX)
      DOUBLE PRECISION  FM(MBMAX),XBOUND(NBOUNDMAX),YBOUND(NBOUNDMAX)
      DOUBLE PRECISION  FCIRC(NRMMAX),B02AV(NRMMAX),
     >                  B0MAX(NRMMAX),RAV(NRMMAX)
      DOUBLE PRECISION  DRMERC(NRMMAX),DIMERC(NRMMAX),
     >                  HH(NRMMAX),QPROF(NRMMAX)
      DOUBLE PRECISION  DQPROF(NRMMAX),GEONC(NRMMAX),ZJPAR(NRMMAX)
      DOUBLE PRECISION  DUMMY1(NRMMAX),DUMMY2(NRMMAX),ZVOL(NRMMAX),
     >                  ZVOLP(NRMMAX)

C------------------------------------------- DEFINE INPUT NAMELISTS ----
      NAMELIST/SHAPE/  ELLIP,TRIA,QUAD,MHARM,ISHAPE,ISOL,FM,MFM,
     >                 XR1,SIG1,XAXIS,YAXIS,
     >                 PAR1,PAR2,PAR3,PAR4,AMESH,BMESH,CMESH,IMESH,
     >                 IAS,IARC,NBOUND,XBOUND,YBOUND 
      NAMELIST/PROFILE/AGA,BGA,CGA,DGA,EGA,FGA,GGA,HGA,
     >                 API,BPI,CPI,DPI,EPI,FPI,GPI,HPI,
     >                 ACUR,BCUR,CCUR,DCUR,ECUR,FCUR,GCUR,HCUR,
     >                 AOM,BOM,COM,DOM,EOM,FOM,GOM,HOM,
     >                 ATE,BTE,CTE,DTE,ETE,FTE,GTE,HTE,
     >                 ATH,BTH,CTH,DTH,ETH,FTH,GTH,HTH,
     >                 VH,VF2,VTE,VOM2,VTH,QIN,
     >                 ICUR,IGAM,IPAI,NPTS,IAV,IOMG,ITE,ITH
      NAMELIST/PHYS/   EPS,ALFA,B,C,OMGOT,HOT,XIAB,Q95,BETAP,
     >                 RVAC,BVAC,ZEFF,ZN0,RPE,ETAEI, THTOF
      NAMELIST/NUM/    NR,NP,NRMAP,NPMAP,NCHI,NITER,NMESH,AMIX,BMIX,
     >                 ERRIT,ERRCUR,NRCUR,NPCUR,
     >                 NBB,ABB,BBB,AMPL
      NAMELIST/PRI/    NPR1,NPR2,NROUT,NDIAG,NFCHECK,NAVG
      NAMELIST/PLOT/   NPL1, NXGRID, NYGRID
      NAMELIST/BALL/   QB1,QB2,NQB

C----------------------------------------- READ INPUT PARAMETERS -------
      CALL INIVAL
C----------------------------------------- REMOVE OPEN STAT. ON IBM ----

      READ(10,NUM)
      READ(10,PLOT)
      READ(10,PHYS)
      READ(10,PROFILE)
      READ(10,SHAPE)

      IF (ICUR.EQ.0) NMESH = 1
      IF ((IAS.EQ.1) .AND. (MOD(NCHI,2).NE.0)) THEN
        STOP ' NCHI MUST BE 2^n'
      ENDIF
      IF ((IAS.EQ.0) .AND. (MOD(NCHI,2).NE.1)) THEN 
        STOP ' NCHI MUSt BE 2^n + 1'
      ENDIF
C------------------------------------------ALLOCATE LARGE MATRIX
      IF ((NR.GT.NRMAX).OR.(NP.GT.NPMAX)) THEN
        WRITE(*,*) ' NR or NP too large, NRMAX =',NRMAX,' NPMAX=',NPMAX
        STOP
      ENDIF
      KKLDA = 4*NP+9

      WRITE(*,*)
      WRITE(*,*)'@@@@ START HELENA @@@@'
      WRITE(*,*)
      ALLOCATE(KKBIG(KKLDA,4*(NR+2)*NP))
      
C-------------------------------- INITIALIZE INTERPOLATING FUNCTIONS ---
      CALL GAUSS
C-------------------------------- INITIAL GUESS FOR A ------------------
      A = 4. * B/DABS(B) 
C------------------------------------ CALCULATE SHAPE OF SOLOVIEV ------
c      IF (ISOL.EQ.1) THEN
c        A = 2*(1.D0 + (1.D0- EPS**2 /4.)/ELLIP**2)
c        B = 2*EPS + TRIA/ELLIP**2/(1.D0+ (1.D0- EPS**2 /4.D0)/ELLIP**2)
c        CALL SOLSHP(FR,MHARM)
c        CPSURF=.5*EPS**2/(1.D0+EPS**2)**2*ELLIP/DSQRT(1.D0-EPS**2/4.D0)
c        RADIUS = DSQRT(EPS**2/(1.D0+EPS**2))
c        B0 = DSQRT(1.D0+EPS**2)
c        ALFA = RADIUS**2 * B0 / CPSURF
c        NITER = 1
C        AGA = 0.
C	BGA = 0.
C	CGA = 0.
C	DGA = 0.
C	EGA = 0.
C	FGA = 0.
C	GGA = 0.
C	HGA = 0.
C	API = 0.
C	BPI = 0.
C	CPI = 0.
C	DPI = 0.
C	EPI = 0.
CC	FPI = 0.
C	GPI = 0.
C	HPI = 0.
C	IPAI = 1
C	IGAM = 1
c      ELSE
C------------------------------------ PLASMA BOUNDARY IN FOURIER SERIES
      WRITE(*,*)'INITIALIZING PROFILE AND BOUNDARY'
        IF (ABS(ISHAPE).LT.2) THEN
          CALL FSHAPE(FR,MHARM)
        ELSE
           IF (ISHAPE.EQ.3) THEN
              CALL FMBOUND(NBOUND,XBOUND,YBOUND,MHARM,FM)
              MFM = MHARM / 2
           ENDIF
          IF (IAS.EQ.1) THEN
            DO M=1,MFM
              FR(M) = FM(M)
            ENDDO
          ELSEIF (ISHAPE.GT.0) THEN
            DO M=1,MFM
              FR(2*M-1) = FM(M)
              FR(2*M) = 0.
            ENDDO
          ELSE
            DO M=1,MFM
              FR(2*M-1) = FM(2*M-1)
              FR(2*M)   = 0.
            ENDDO
          ENDIF
        ENDIF
	TXTOUT(1) = ' '
c      ENDIF
C------------------------------------ ROTATE BOUNDARY

      CALL ROTATE_BND(FR,MFM,PAR1,IAS)


C------------------------------------ NORMALIZE PROFILES ---------------
      IF ((IGAM.EQ.2).OR.(IGAM.EQ.3)) THEN
         FSCALE = VF2(1)
         DO I=1,NPTS
            VF2(I) = VF2(I) / FSCALE
         ENDDO
      ENDIF
      IF ((IPAI.EQ.2).OR.(IPAI.EQ.3)) THEN
         PSCALE = VH(1)
         IF (PSCALE.EQ.0.D0) PSCALE = 1.D0
         DO I=1,NPTS
            VH(I) = VH(I) / PSCALE
         ENDDO
      ENDIF
      IF ((ITE.EQ.2).OR.(ITE.EQ.3)) THEN
         CSCALE = VTE(1)
         DO I=1,NPTS
            VTE(I) = VTE(I) / CSCALE
         ENDDO
      ENDIF
      IF ((ITH.EQ.2).OR.(ITH.EQ.3)) THEN
         CSCALE = VTH(1)
         DO I=1,NPTS
            VTH(I) = VTH(I) / CSCALE
         ENDDO
      ENDIF
      IF ((IOMG.EQ.2).OR.(ITH.EQ.3)) THEN
         CSCALE = VOM2(1)
         DO I=1,NPTS
            VOM2(I) = VOM2(I) / CSCALE
         ENDDO
      ENDIF

C--------------------------------- INITIALIZE PRES AND GAM PROFILES ----
      CALL INIPRES
      CALL INIGAM
      CALL INIOMG
      CALL INITE
      CALL INITHE
c      B = B / DPDPSI(0.D0)
      IF (IOMG.LE.0) OMGOT = 0.D0
      IF (ITH.LE.0) THTOF = 0.D0
C------------------------------------------- LOOP OVER CURRENT PROFILE
      NRTMP = NR
      NPTMP = NP

C---------------------------------------------- PROFILES -------------      
      DO I=1,101
         SS = (I-1)*0.01D0
         PS = SS*SS
	 PSPLOT(I) = SS
         OMGPLOT(I) = OMGS(PS)
         TEMPLOT(I) = TEMS(PS)
	 DPPLOT(I) = DPDPSI(PS)
	 DGPLOT(I) = DGDPSI(PS)
      ENDDO
      CALL OUT(1)
      IF (NPR1.NE.0) THEN
        WRITE(20,*) '***********************************************'
        WRITE(20,*) '*        INPUT PROFILES :                     *'
	WRITE(20,*) '***********************************************'
        WRITE(20,*) '*  PSI,   H(psi),  F^2  ,  OM , T, THE'
	WRITE(20,*) '***********************************************'  
        NR_PR = 21
        DO I=1,NR_PR
          PS = FLOAT(I-1)/FLOAT(NR_PR-1)
          IF (ICUR.EQ.0) THEN
             WRITE(20,622) PS,PRES(PS),XGAMMA(PS)**2, 
     >            OMGS(PS), TEMS(PS),THES(PS)
          ENDIF
        ENDDO
       WRITE(20,*)
      ENDIF
      IF (NPL1.NE.0) THEN
        NR_PR = 1001
        DO I=1,NR_PR
           PS = (FLOAT(I-1)/FLOAT(NR_PR-1))
         WRITE(30,*)(PS),PRES(PS),XGAMMA(PS)**2, 
     >            OMGS(PS), TEMS(PS),THES(PS)
        ENDDO
      ENDIF
 622  FORMAT(6E12.4)

C----------------------------------- START OF MAIN LOOP 555 -----------

C------------------------------------ DEFINE NUMBERING OF ELEMENTS ----
 555  CALL ELMNO(NR,NP,NODENO)
C------------------------------------ INITIALIZE PSI ------------------
C------------------------------------ DEFINE INITIAL GRID X,Y ---------

C      CALL BUILDBOUND(RBOUND,FR,MFM)
      IF (IAS.LT.1) YAXIS = 0.D0
C      IF ((DABS(XAXIS).GT.1.D-4).OR.(DABS(YAXIS).GT.1.D-4)) THEN
C         CALL REBUILDFR(RBOUND,FR,MFM,XAXIS,YAXIS)
C      ENDIF
      CALL INIGRID(XX,YY,PSI,NR,NP,FR,MHARM,IAS,XAXIS,YAXIS)
      XAXISOLD = XAXIS
      YAXISOLD = YAXIS
C------------------------------------ PLOT THE INITIAL GRID
      WRITE(*,*)'PLOT INITIAL GRID'
      IF ((NPL1.NE.0)) THEN
        CALL PLOTGR(0,XX,YY,NR,NP,IAS,31)
      ENDIF
      WRITE(*,*)'START MAIN LOOP'
      DO 888 NMG = 1,NMESH
         WRITE(*,*)'LOOP ', NMG, ' TOTAL=', NMESH
         WRITE(*,*)'NR, NP ', NR, NP

         WRITE(20,*) '***************************************'
         WRITE(20,22) NR,NP
         WRITE(20,*) '***************************************'    
 22      FORMAT(' * ITERATION, NR=',I3,' NP=',I3,'      *')
         NIT = NITER
         CALL INIGRID(XX,YY,PSI,NR,NP,FR,MHARM,IAS,0.D0,0.D0)
         XAXISOLD = XAXIS
         YAXISOLD = YAXIS
      DO 10 NI = 1, NIT
         DO J=1,4*NR*NP
           QQ(J) = 0.
         ENDDO
C------------------------------------ INITIALIZE KK AND QQ TO ZERO -----
         DO I=1,4*NR*NP
            PSIOLD(I) = PSI(I)
         ENDDO
C------------------------------------ FORM MATRIX, NO CONDITIONS -------
         CALL FORMKQ(XX,YY,PSI,NR,NP,QQ,A,B,C,OMGOT,HOT,EPS,IGAM,ITH
     >        ,ISOL,NI,IAS)
C------------------------------------ SOLVE SET OF EQUATIONS -----------
        CALL SOLVE2(QQ,NR,NP,PSI,NI,IAS,ITH)
C------------------------------------ FIND MAGN. AXIS AND PSI AT AXIS --
        CALL FINDAXIS(XX,YY,NR,NP,PSAXIS,XAXIS,YAXIS,NAX,RAX,SAX,IAS)
        A = A / (1.-PSAXIS)
        WRITE(*,13) XAXIS,YAXIS, PSIAXIS
C------------------------------------ NORMALIZE FLUX TO ZERO ON AXIS ---
        CALL NORMAL(PSI,NR,NP,PSAXIS) 
        ERR1 = 0.
	ERR2 = 0.
	ERR3 = 0.
	ERR4 = 0.
        DO 16 I=1,NR*NP
          NBASE = 4*(I-1)+1
          ERR1 = ERR1 + DABS(PSI(NBASE)  - PSIOLD(NBASE))  
          NBASE2 = 4*(I-1)+2
          ERR2 = ERR2 + DABS(PSI(NBASE2)  - PSIOLD(NBASE2))
          NBASE3 = 4*(I-1)+3
          ERR3 = ERR3 + DABS(PSI(NBASE3)  - PSIOLD(NBASE3))
          NBASE4 = 4*(I-1)+4
          ERR4 = ERR4 + DABS(PSI(NBASE4)  - PSIOLD(NBASE4))
   16   CONTINUE
        ERR1 = ERR1 / FLOAT(NR*NP)
        ERR2 = ERR2 / FLOAT(NR*NP)
        ERR3 = ERR3 / FLOAT(NR*NP)
        ERR4 = ERR4 / FLOAT(NR*NP)
        IF (ISOL.EQ.1) THEN
C         CALL ERRSOL(XX,YY,PSI,NR,NP)
        ENDIF
      IF (ERR1+ERR2+ERR3+ERR4.LT.ERRIT) GOTO 777
   10 CONTINUE
  777 CONTINUE
      WRITE(20,19) ERR1+ERR2+ERR3+ERR4,NI
      IF ((DABS(XAXISOLD-XAXIS).LT.1.D-4).AND.
     >     (DABS(YAXISOLD-YAXIS).LT.1.D-4)) THEN
         WRITE(*,*)
         WRITE(*,*) 'MESH CONVERGE, NO NEED TO REMESH'
         WRITE(*,*)
         GOTO 999
      ENDIF
      IF (NMG.NE.NMESH) THEN
           NRM = NR
           NPM = NP
        CALL REMESH(XX,YY,PSI,A,B,C,EPS,NR,NP,NRM,NPM,MESHNO,
     >              CX,CY,XAXIS,XAXISOLD,YAXIS,NAX,RAX,SAX,IGAM,IAS)
        YAXISOLD = YAXIS
        WRITE(*,*) 'DONE REMESH', NMG
C        CALL PLOTGR(0,XX,YY,NR,NP,IAS,40)
        IF (ICUR.EQ.99) THEN
         CALL FLXINT2(XAXIS,XX,YY,PSI,NR,NP,A,B,EPS,IGAM
     >        ,CX,CY,IAS,AMPL,SUMDQ)
         DO II = 0, 10
            WRITE(*,*) XGAMMA(II*0.1D0)
         ENDDO
      ENDIF
C------------------------------------ DEFINE NUMBERING OF ELEMENTS ----
      ENDIF
 888  CONTINUE
      GOTO 999
 666  NMESH = 1
      NR = NRTMP
      NP = NPTMP
      IMESH = IMESHTMP
      IARC = IARCTMP
      ICUR = 0

      GOTO 555
 999  CONTINUE
      DEALLOCATE(KKBIG)
C
C-------------------------------------- REMESH ON FLUX COORDINATES ----- 
C
      WRITE(*,*)'FINAL REMESH'
C      IF (NDIAG.EQ.1) write(*,*) ' final remesh'
      
      CALL REMESH(XX,YY,PSI,A,B,C,EPS,NR,NP,NRMAP,NPMAP,MESHNO,
     >            CX,CY,XAXIS,XAXISOLD,YAXIS,NAX,RAX,SAX,IGAM,IAS)
      IF (NFCHECK .GE. 1) THEN
         DIFFFORCEB = CHECKFORCE(XX,YY,PSI,A,NR,NP)
      ENDIF
C      DO JJ = 1, NR
C         WRITE(*,*)XX(2,(NP)*JJ+1),XX(3,(NP)*JJ+1),XX(4,(NP)*JJ+1)
C      ENDDO
C
C-------------------------------------- PLOT FINAL GRID ----------------
C
      IF (NPL1.EQ.1) THEN
C        CALL LBLBOT('HELENA EQUILIBRIUM Version 12',34)
        CALL PLOTGR(0,XX,YY,NR,NP,IAS,32)
        CALL PLOTPSIPRES(XX,YY,PSI,XAXIS,YAXIS,A,NR,NP,IAS,36)
      ENDIF
C
C-------------------------------------- SCALE ALFA TO INPUT CURRENT ----
C

C      WRITE(*,*)'DEBUG: SCALE Q95'
C      IF (Q95.GT.0.) THEN
C        IF (NDIAG.EQ.1) WRITE(*,*) 'Q95 IS SPECIFIED TO ',Q95
C        CALL PROFQ(XAXIS,XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,
C     >                   CX,CY,DUMMY1,DUMMY2,Q95OUT,Q1OUT,IAS)
C        IF (NDIAG.EQ.1) WRITE(*,*) Q1OUT,Q95OUT,Q95,ALFA
C        ALFA = ALFA * Q95/Q95OUT
C        CALL CURRENT(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,CUR,IAS)
C        XIAB = CUR
C      ELSEIF (XIAB.GT.0.) THEN      
C        IF (NDIAG.EQ.1) WRITE(*,*) '  current',XIAB      
C        CALL CURRENT(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,CUR,IAS)
C        ALFA = DABS(CUR / XIAB)
C        IF (NDIAG.EQ.1) WRITE(*,*) '   alfa : ',ALFA
C      ENDIF       
C      CALL MOMENTS(XX,YY,PSI,NR,NP,XIAB,ALFA,EPS,IAS)     
C--------------------------------------- CALCULATE SOME QUANTITIES ------
C      WRITE(TXTOUT(13),13) XAXIS,YAXIS

C      ICUR = ICURTMP
      CALL DIAGNO(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,IAS,XAXIS,
     >            YAXIS,IAV,ZVOL,ZVOLP, RVAC, BVAC)

!--------------------------------------- WRITE RESTART FILE ------------
C      IF (NPR2 .EQ.1) THEN
C        CALL INOUT(XAXIS,CX,CY,XX,YY,PSI,FM,MFM,A,NRTMP,NPTMP,
C     >             ICURTMP,NMESHTMP)
C      ENDIF      
C
C-------------------------------------- MAPPING OF FLUX COORDINATES ----
C
      CALL CALCBTPD(XAXIS,0.D0,0.D0,A,BTOT0,TPER0,DET0)
      TEMP0 = TEMS(0.D0)
      YITA0 = (TPER0 - TEMP0) / TPER0
      VMACH0 = (1+EPS*XAXIS)**2*OMGS(0.D0)/TPER0*OMGOT*2
c$$$      WRITE(*,*)'SHAFRANOV SHIFT :', XAXIS
c$$$      WRITE(*,*)'YITA0           :', YITA0
c$$$      WRITE(*,*)'MACH0           :', DSQRT(VMACH0)
c$$$      WRITE(22,*)A,YITA0,XAXIS,1./DSQRT(CY/CX)
c$$$      WRITE(*,*)'ELLIPTICITY     :', 1./DSQRT(CY/CX)
c$$$      WRITE(*,*)'A               :', A
      WRITE(*,*)'MAPPING'
      CALL MAPPING(XX,YY,PSI,CX,CY,XAXIS,A)

      IF (NAVG.GE.1) THEN
         WRITE(*,*) 'CALC AVERAGE'
         CALL FLUXAVERAGE(XX, YY, PSI, XAXIS, A)
      ENDIF
c      DO K=1,80,2
c        node =K*NP+1
c        WRITE(20,*) NP,PSI(4*(NODE-1)+1)
c        WRITE(20,81) (XX(1,node+i-1),YY(1,node+i-1),I=1,NP)
c      ENDDO
c   81 format(2e16.8)

C
C--------------------------------------- CALC. BALLOONING STABILITY -----
C

C      WRITE(*,*)'DEBUG: MERCIER'
C      CALL MERCIER(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,IAS,
C     >             DIMERC,DRMERC,HH,QPROF,DQPROF,GEONC,ZJPAR)
C      WRITE(*,*)'DEBUG: BALLONING'
C      IF (NQB.NE.0) THEN 

C        CALL HELBAL(ZVOL,ZVOLP,XAXIS)
C      ENDIF

C      IF (NPR1.NE.0) THEN
c        CALL CIRCUL(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,IAS,
c     >            FCIRC,B02AV,B0MAX,RAV)

c        CALL WORLD(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,IAS,
c     >             RVAC,BVAC,ZN0,ZEFF,RPE,ETAEI,FCIRC,QPROF,DQPROF,
c     >             RAV,GEONC,ZJPAR,DIMERC,DRMERC,HH)

C      ENDIF
      
C      IF (NPR2 .EQ.1) THEN
!--------------------------------------- WRITE EQDSK FILE --------------
C      CALL EQDSK(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,IAS,XIAB,RVAC,BVAC,
C     >           QPROF)
C      ENDIF
C--------------------------------------- CLOSE PLOTFILE ----------------
C      IF (NPL1.EQ.1) THEN
c        CALL FINPLT
C      ENDIF
      

C
C--------------------------------------- FORMAT STATEMENTS --------------      
    2 FORMAT('A   : ',E12.4)
    3 FORMAT('B   : ',E14.6)
    4 FORMAT('ALFA   : ',E14.6)
    5 FORMAT('RADIUS : ',E14.6)
    6 FORMAT('B0     : ',E14.6)
    7 FORMAT('CPSURF : ',E14.6)
   11 FORMAT('ITERATION     : NO  = ',I3)
   13 FORMAT('MAGNETIC AXIS : X= ',E14.6,' Y= ',E14.6, ' PSI ',E14.6)
   15 FORMAT('PSI AT AXIS   : PSI = ',E14.6)
   17 FORMAT('AMPLITUDE     : A   = ',E14.6)
   19 FORMAT('    REST = ',1PE10.3,' ITERATIONS : ',I3)
 876  FORMAT(6e10.3)
      WRITE(*,*)'@@@@@ FINISHED HELENA @@@@@'
      RETURN
      END

***********************************************************************
*DECK INIVAL
      SUBROUTINE INIVAL
C-----------------------------------------------------------------------
C SUBROUTINE TO INITIALIZE THE NAMELIST INPUT VARIABLES
C-----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMDAT
      USE COMPRI
      USE COMPLO
      USE TOLERA
      USE COMMAP
      USE COMPIE
      USE COMPROF
      USE MESHAC
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)


      RS(1,1) = -1.
      RS(1,2) = -1.
      RS(2,1) = -1.
      RS(2,2) = +1.
      RS(3,1) = +1.
      RS(3,2) = +1.
      RS(4,1) = +1.
      RS(4,2) = -1.
      IJ(1,1) = 0
      IJ(1,2) = 0
      IJ(2,1) = 1
      IJ(2,2) = 0
      IJ(3,1) = 0
      IJ(3,2) = 1
      IJ(4,1) = 1
      IJ(4,2) = 1
      XGAUSS(1) = -0.86113 63115 94053
      XGAUSS(2) = -0.33998 10435 84856
      XGAUSS(3) =  0.33998 10435 84856
      XGAUSS(4) =  0.86113 63115 94053
      WGAUSS(1) =  0.34785 48451 37454
      WGAUSS(2) =  0.65214 51548 62546
      WGAUSS(3) =  0.65214 51548 62546
      WGAUSS(4) =  0.34785 48451 37454 
      
      PI = 2.D0*DASIN(1.D0)
      IAS = 0
      IAV = 0
      ELLIP = 1.0
      TRIA  = 0.0
      QUAD  = 0.0
      MHARM = 128
      ISHAPE = 1
      IMESH = 0
      AMESH = 0.
      BMESH = 0.
      ISOL = 0
      IARC = 0
      XR1 = 99.
      SIG1 = 99.
      AGA = -1.
      BGA = 0.
      CGA = 0.
      DGA = 0.
      EGA = 0.
      API = -1.
      BPI = 0.
      CPI = 0.
      DPI = 0.
      EPI = 0.
      ACUR=-1.
      BCUR=0.
      CCUR=0.
      DCUR=0.
      ECUR=1.
      ICUR = 0
      IPAI = 1
      IGAM = 1
      EPS = 0.1
      ALFA = 1.
      XIAB = -1.
      Q95 = -1.
      BETAP = -1.
      B = 0.1
      C = 1.
      AMIX = 0.
      BMIX = 0.
      NR=33
      NP=33
      NRMAP = 50
      NPMAP = 65
      NCHI  = 65
      NITER = 40
      NMESH = 1
      NPTS = 100
      NBOUND = 256

      NBB = 4
      ABB = 1.
      BBB = 1.
      AMPL = 1.
      NPR1 = 1
      NPR2 = 0
      NPL1 = 1
      ERRCUR = 1.D-5
      ERRIT  = 1.D-8
      PSITOL = 1.D-8
      THTTOL = 1.D-8
      TOL    = 1.D-8
      NQB = 1

      RVAC = 1.0
      BVAC = 1.0
      ZEFF = 1.0
      ZN0  = 1.0
      RPE  = 0.5
      ETAEI = 1.0      
      NROUT = 21

C@--- NEW NAMELIST ELEMENTS---
      XAXIS = 0.D0
      YAXIS = 0.D0
      NXGRID = 201
      NYGRID = 201

      OMGOT = 0.D0
      HOT = 1.D0
      THTOF = 0.D0

      AOM = 0.
      BOM = 0.
      COM = 0.
      DOM = 0.
      EOM = 0.
      FOM = 0.
      GOM = 0.
      HOM = 0.

      ATE = -1.
      BTE = 0.
      CTE = 0.
      DTE = 0.
      ETE = 0.
      FTE = 0.
      GTE = 0.
      HTE = 0

      ATH = 0.
      BTH = 0.
      CTH = 0.
      DTH = 0.
      ETH = 0.
      FTH = 0.
      GTH = 0.
      HTH = 0.

      ITE = 1
      IOMG = 0
      ITH = 0

      NFCHECK = 0
      NAVG = 1

      RETURN
      END

************************************************************************
*DECK OUT
      SUBROUTINE OUT(I)
C-----------------------------------------------------------------------
C SUBROUTINE TO PRINT THE OUTPUT ON UNIT 20
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPRI
      USE COMPLO
      USE COMMAP
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IF (I.EQ.1) THEN
        WRITE(20,*) '*****************************************'
        WRITE(20,*) '  HELENA : GRAD-SHAFRANOV EQUILIBRIUM    '
        WRITE(20,*) '*****************************************'
        WRITE(20,*) '*          VERSION 12 (30-7-1999)       *'
        WRITE(20,*) '* WITH ANISOTROPY AND T-FLOW (JUN 2013) *'
        WRITE(20,*) '*****************************************'
      ENDIF
      RETURN
      END

************************************************************************
*DECK INOUT
      SUBROUTINE INOUT(XAXIS,CX,CY,XX,YY,PSI,FM,MFM,A,NRTMP,NPTMP,
     >                 ICURTMP,NMESHTMP)
C-----------------------------------------------------------------------
C SUBROUTINE TO PRINT THE NAMELIST VALUES UNIT 30 USED FOR RESTARTING
C-----------------------------------------------------------------------
      USE PARAM
      USE COMMAX
      USE COMDAT
      USE COMPROF
      USE COMPRI
      USE COMPLO
      USE COMMAP
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
       DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*),QPRF(NRMMAX),FM(*)

c      NAMELIST/SHAPE/  ELLIP,TRIA,QUAD,MHARM,ISHAPE,ISOL,FM,MFM,
c     >                 PAR1,PAR2,PAR3,PAR4,AMESH,BMESH,CMESH,IMESH
c      NAMELIST/PROFILE/AGA,BGA,CGA,DGA,EGA,FGA,GGA,HGA,
c     >                 API,BPI,CPI,DPI,EPI,FPI,GPI,HPI,
c     >                 ACUR,BCUR,CCUR,DCUR,ECUR,FCUR,GCUR,HCUR,
c     >                 ICUR,
c     >                 IGAM,IPAI,NPTS,DPR,DF2,ZJZ
c      NAMELIST/PHYS/   EPS,ALFA,B,C,XIAB,BETAP
c      NAMELIST/NUM/    NR,NP,NRMAP,NPMAP,NCHI,NITER,NMESH,AMIX,BMIX,
c     >                 ERRIT,ERRCUR,NRCUR,NPCUR,
c     >                 ABB,BBB,NBB
c      NAMELIST/PRI/    NPR1
c      NAMELIST/PLOT/   NPL1
c      NAMELIST/BALL/

      
C      CALL PROFQ(XAXIS,XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,
C     >                   CX,CY,QPRF,ZJZ,Q95OUT,Q1OUT,IAS)

C      CSCALE = ZJZ(1)
      DO I=1,NR
C        ZJZ(I) = ZJZ(I) / CSCALE
      ENDDO

      ID=30
      WRITE(ID,*)  ' HELENA INPUT-OUTPUT FILE'
      WRITE(ID,*)
      WRITE(ID,20) ELLIP,TRIA,QUAD
      WRITE(ID,21) MFM,MHARM,ISHAPE,ISOL
      WRITE(ID,22) IMESH,AMESH,BMESH
      WRITE(ID,23) PAR1,PAR2,PAR3,PAR4
      IF (ISHAPE.EQ.2) THEN
        DO M=1,MFM
          WRITE(ID,24) M,FM(M)
	ENDDO
      ENDIF
      WRITE(ID,*)  '$END'
      WRITE(ID,30) IGAM,AGA,BGA,CGA
      WRITE(ID,31) DGA,EGA,FGA
      WRITE(ID,32) GGA,HGA
      WRITE(ID,33) IPAI,API,BPI,CPI
      WRITE(ID,34) DPI,EPI,FPI
      WRITE(ID,35) GPI,HPI
      WRITE(ID,36) ICURTMP,ACUR,BCUR,CCUR
      WRITE(ID,37) DCUR,ECUR,FCUR
      WRITE(ID,38) GCUR,HCUR

      WRITE(ID,39) NROUT
     
      ICUR = 11

      DO I=1,NROUT
        SS   = FLOAT(I-1)/FLOAT(NROUT-1)
        ZPSI = SS * SS
        DPRI = DPDPSI(ZPSI)
        DF2I = DGDPSI(ZPSI)
C        ZJZI = CURPHI(ZPSI)
	QOUT = QPRFL(ZPSI)
!         WRITE(ID,392)  I,DPRI,I,DF2I,I,ZJZI,I,QIN(I)
!         WRITE(ID,392)  I,DPRI,I,DF2I,I,ZJZI,I,QOUT
         WRITE(ID,392)  I,DPRI,I,DF2I,I,ZJZI,I,QPRF(I)

      ENDDO

      WRITE(ID,*)  '$END'

      WRITE(ID,40) EPS,ALFA,B,C
      WRITE(ID,41) XIAB,BETAP
      WRITE(ID,*)  '$END'

      WRITE(ID,50) NRTMP,NPTMP,NRMAP,NPMAP,NCHI,NITER
      WRITE(ID,55) NRCUR,NPCUR,NMESHTMP
      WRITE(ID,56) ERRCUR,AMIX
      WRITE(ID,57) ABB,BBB,NBB,AMPL
      WRITE(ID,*)  '$END'

      WRITE(ID,60) NPR1
      WRITE(ID,70) NPL1

      WRITE(ID,71) NQB

   20 FORMAT(/1X,'$SHAPE    ELLIP =',F6.4,', TRIA  =',F6.4,
     >           ', QUAD  =',F6.4,',')
   21 FORMAT(11X,'MFM = ',I4,', MHARM =',I4,', ISHAPE =',I2,
     >           ', ISOL =',I2,',')
   22 FORMAT(11X,'IMESH =',I3,', AMESH=',F6.4,', BMESH=',F6.4,',')
   23 FORMAT(11X,'PAR1 = ',F8.4,', PAR2 = ',F8.4,', PAR3 = ',F8.4
     >          ', PAR4 = ',F8.4)
   24 FORMAT(11X,' FM(',I3,') = ',1PE12.4,',')
   30 FORMAT(/1X,'$PROFILE  IGAM =',I2,', AGA =',1PE12.4,', BGA =',
     >        E12.4,', CGA =',E12.4,',')
   31 FORMAT(11X,'DGA =',1PE12.4,', EGA ='E12.4,', FGA =',E12.4,',')
   32 FORMAT(11X,'GGA =',1PE12.4,', HGA =',E12.4,',')
   33 FORMAT(1X,'          IPAI =',I2,', API =',1PE12.4,', BPI =',
     >            E12.4,', CPI =',E12.4,',')
   34 FORMAT(11X,'DPI =',1PE12.4,', EPI =',E12.4,', FPI =',E12.4,',')
   35 FORMAT(11X,'GPI =',1PE12.4,', HPI =',E12.4,',')
   36 FORMAT(1X,'          ICUR =',I2,', ACUR =',1PE12.4,', BCUR =',
     >            E12.4,', CCUR =',E12.4,',')
   37 FORMAT(11X,'DCUR =',1PE12.4,', ECUR =',E12.4,', FCUR =',E12.4,',')
   38 FORMAT(11X,'GCUR =',1PE12.4,', HCUR =',E12.4,',')
   39 FORMAT(11X,'NPTS = ',I4,',')
  391 FORMAT(11x,'DPR(',I3,')=',1PE13.5,', DF2(',I3,')=',E13.5,
     >          ', QIN(',I3,')=',E13.5,',') 
  392 FORMAT(3x,'DPR(',I3,')=',1PE13.5,', DF2(',I3,')=',E13.5,
     >          ', ZJZ(',I3,')=',E13.5,', QIN(',I3,')=',E13.5,',') 
   40 FORMAT(/1X,'$PHYS     EPS  =',F10.6,', ALFA =',F10.6,
     >           ', B =',E14.8,', C =',F10.6,',')
   41 FORMAT(11x,'XIAB =',F10.6,', BETAP =',F10.6)
   50 FORMAT(/1X,'$NUM      NR =',I3,', NP =',I3,', NRMAP =',I3,
     >           ', NPMAP =',I3,', NCHI =',I3,', NITER = ',I3,',')
   55 FORMAT(11X,'NRCUR =',I3,', NPCUR =',I3,', NMESH = ',I4,',')
   56 FORMAT(11X,'ERRCUR =',1PE8.2,', AMIX = ',0PF8.4',')  
   57 FORMAT(11X,'ABB = ',F8.4,', BBB = ',F8.4,', NBB = ',I4,', AMPL =',
     >       F8.4)
   60 FORMAT(/1X,'$PRI      NPR1  =',I2,' $END ')
   70 FORMAT(/1X,'$PLOT     NPL1  =',I2,' $END ')
   71 FORMAT(/1X,'$BALL     NQB = ',I4,' $END')
      RETURN
      END

************************************************************************
*DECK GAUSS
      SUBROUTINE GAUSS
C-----------------------------------------------------------------------
C CALCULATE THE SIXTHEEN FUNCTIONS AT THE SIXTEEN GAUSSIAN POINTS
C-----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION DUMRR, DUMSS

      DO 10 IRS=1,4
        R0 = RS(IRS,1)
        S0 = RS(IRS,2)
        DO 20 IM=1,4
          MI = IJ(IM,1)
          MJ = IJ(IM,2)
          DO 30 NGR=1,4
          R = XGAUSS(NGR)
            DO 40 NGS=1,4
              S  = XGAUSS(NGS)
              CALL CUBICH(MI,MJ,R0,S0,R,S,H(IM,IRS,NGR,NGS),
     >                    HR(IM,IRS,NGR,NGS),HS(IM,IRS,NGR,NGS),
     >                    HRS(IM,IRS,NGR,NGS),DUMRR,DUMSS)
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

************************************************************************
*DECK CUBICH
      SUBROUTINE CUBICH(I,J,R0,S0,R,S,H,HR,HS,HRS,HRR,HSS)

C------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE VALUE OF THE CUBIC POLYNOMIALS AND
C THE DERIVATIVES OF THE CORNER MARKED BY R0,S0 AT THE POSITION R,S
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  H,HR,HS,HRS,HI,HRI,HJ,HSJ,HRR,HSS,HRRI,HSSJ

      IF (I.EQ.0) THEN
        HI = - (R+R0)**2 * (R*R0-2.) / 4.
        HRI = - (R+R0)*(R*R0-2.)/2. - R0*(R+R0)**2 / 4.
        HRRI = - 1.5 * R * R0
      ELSE
        HI = + R0 * (R+R0)**2 * (R*R0 - 1.) / 4.
        HRI = + R0*(R+R0)*(R*R0-1.)/2. + (R+R0)**2 /4.
        HRRI = 1.5*R + .5*R0
      ENDIF
      IF (J.EQ.0) THEN
        HJ = - (S+S0)**2 * (S*S0-2.) / 4.
        HSJ = - (S+S0)*(S*S0-2.)/2. - S0*(S+S0)**2 / 4.
        HSSJ = - 1.5 * S * S0
      ELSE
        HJ = + S0 * (S+S0)**2 * (S*S0 - 1.) / 4.
        HSJ = + S0*(S+S0)*(S*S0-1.)/2. + (S+S0)**2 / 4.
        HSSJ = 1.5*S + .5*S0
      ENDIF
      H = HI * HJ
      HR = HRI * HJ
      HS = HI * HSJ
      HRS = HRI * HSJ
      HRR = HRRI * HJ
      HSS = HI   * HSSJ
      RETURN
      END

************************************************************************
*DECK CUBICH1
      SUBROUTINE CUBICH1(I,J,R0,S0,R,S,H)
C------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE VALUE OF THE CUBIC POLYNOMIALS AND
C THE DERIVATIVES OF THE CORNER MARKED BY R0,S0 AT THE POSITION R,S
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  H,HI,HJ

      IF (I.EQ.0) THEN
        HI = - (R+R0)**2 * (R*R0-2.) / 4.
      ELSE
        HI = + R0 * (R+R0)**2 * (R*R0 - 1.) / 4.
      ENDIF
      IF (J.EQ.0) THEN
        HJ = - (S+S0)**2 * (S*S0-2.) / 4.
      ELSE
        HJ = + S0 * (S+S0)**2 * (S*S0 - 1.) / 4.
      ENDIF
      H = HI * HJ
      RETURN
      END

************************************************************************
*DECK CUBICH2
      SUBROUTINE CUBICH2(I,J,R0,S0,R,S,H,HR,HS)
C------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE VALUE OF THE CUBIC POLYNOMIALS AND
C THE DERIVATIVES OF THE CORNER MARKED BY R0,S0 AT THE POSITION R,S
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  H,HR,HS,HI,HRI,HJ,HSJ

      IF (I.EQ.0) THEN
        HI = - (R+R0)**2 * (R*R0-2.) / 4.
        HRI = - (R+R0)*(R*R0-2.)/2. - R0*(R+R0)**2 / 4.
      ELSE
        HI = + R0 * (R+R0)**2 * (R*R0 - 1.) / 4.
        HRI = + R0*(R+R0)*(R*R0-1.)/2. + (R+R0)**2 /4.
      ENDIF
      IF (J.EQ.0) THEN
        HJ = - (S+S0)**2 * (S*S0-2.) / 4.
        HSJ = - (S+S0)*(S*S0-2.)/2. - S0*(S+S0)**2 / 4.
      ELSE
        HJ = + S0 * (S+S0)**2 * (S*S0 - 1.) / 4.
        HSJ = + S0*(S+S0)*(S*S0-1.)/2. + (S+S0)**2 / 4.
      ENDIF
      H = HI * HJ
      HR = HRI * HJ
      HS = HI * HSJ
      RETURN
      END

************************************************************************
*DECK SOLSHP
      SUBROUTINE SOLSHP(FR,MF)
C-----------------------------------------------------------------------
C THE SHAPE R=DSQRT(X**2+Y**2)=FR(THETA) OF THE PLASMA CROSS-
C SECTION IN THE Z-PLANE IS DERIVED FROM A SHAPE CORRESPONDING
C TO THE PSI=1 SURFACE OF THE SOLOVIEV EQUILIBRIUM.
C-----------------------------------------------------------------------
      USE COMDAT
      USE COMANG
      USE COMPIE
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  FR(*)
      EXTERNAL FSOL

      DO 10 J=1,MF
        ANGLE= 2*PI*(J-1.)/ MF
        F1=.5D0*(ELLIP+(1.-ELLIP)*DCOS(2.D0*ANGLE))
        FUN1=FSOL(F1)
        DO 20 N=1,20
          F2=F1+.05D0*N
          FUN2=FSOL(F2)
          IF(FUN2.GT.0.) GOTO 30
   20   CONTINUE
   30   CALL ZERO(F1,FUN1,F2,FUN2,FSOL,1.D-8,FR(J),FUN,IZERO,0)
   10 CONTINUE
CCC   CALL PRARR1('FR  :',FR,MF,203)
      CALL RFT2(FR,MF,1)
      DO 50 M=1,MF
        FR(M) = 2 * FR(M) / REAL(MF)
   50 CONTINUE
      RETURN
      END

***********************************************************************
*DECK FSOL
      FUNCTION FSOL(F)
C-----------------------------------------------------------------------
C EXTERNAL FUNCTION FSOL=PSI(F)-1 NEEDED FOR THE COMPUTATION
C OF THE RADIUS FR(ANGLE) OF THE PSI=1 SURFACE OF THE SOLOVIEV
C EQUILIBRIUM.
C-----------------------------------------------------------------------
      USE COMDAT
      USE COMANG
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      X=F*DCOS(ANGLE)
      Y=F*DSIN(ANGLE)
      PAR4 = QUAD
      PAR3 = TRIA
      PAR2 = EPS
      PAR1 = ELLIP
      FSOL= (1. -.25*PAR2**2) * (1.+PAR2*PAR3*X*(2+PAR2*X)) * 
     >      (Y/PAR1)**2 + (X - .5*PAR2*(1. - X*X))**2 - 1.
      RETURN
      END
*************************************************************************
*DECK FSHAPE
      SUBROUTINE FSHAPE(FR,MF)
C-----------------------------------------------------------------------
C  THE SHAPE R=DSQRT(X**2+Y**2)=FR(THETA) OF THE PLASMA CROSS-SECTION IN
C  Z-PLANE IS COMPUTED FROM A FORMULA WHICH GIVES ELLIPSES, D-SHAPES ETC

C     X=A*DCOS(GAMMA+C*DSIN(GAMMA)+D*DSIN(2.*GAMMA)),
C     Y=B*DSIN(GAMMA),
C  WHERE THETA IS FOUND FROM GAMMA BY INVERSION.
C  THE FOURIER COEFFICIENTS FRFNUL,FRF(M) OF FR(J) ARE ALSO CALCULATED.
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPIE
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  FR(*), THETA(2*MBMAX+2), GAMMA(2*MBMAX+2)
      DOUBLE PRECISION  XV(2*MBMAX+2),YV(2*MBMAX+2)
      DOUBLE PRECISION  XN, YN, GAMM, GA
C
      IF (ISHAPE.EQ.3) THEN
        WRITE(20,*)
        WRITE(20,*) ' SHAPE OF BOUNDARY : '
        WRITE(20,*)
        DANG = PI*PAR3/2.
        DO 200 J=1,MF
          ANGLE = 2*PI*(J-1.)/REAL(MF)
          FR(J) = 1.
          IF ((ANGLE.GT.(PAR2-DANG)).AND.(ANGLE.LT.(PAR2+DANG))) THEN
            FR(J) = FR(J) + PAR1 * DCOS((ANGLE-PAR2)/PAR3)**PAR4
          ENDIF
          WRITE(20,*) ANGLE,FR(J)
  200   CONTINUE
        WRITE(20,*)
      ELSEIF (ISHAPE.EQ.4) THEN
        WRITE(20,*)
        WRITE(20,*) ' SHAPE OF BOUNDARY : '
        WRITE(20,*)
        DANG = PAR3 /DSQRT(3.D0)
        DO 210 J=1,MF
          ANGLE = 2*PI*(J-1.)/REAL(MF)
          FR(J) = 1.
          IF ((ANGLE.GT.(PAR2-DANG)).AND.(ANGLE.LT.(PAR2+DANG))) THEN
            FR(J) = FR(J) + PAR1 * (2*PAR3/3**1.5 * PAR2**3 -
     >                         DABS((ANGLE-PAR2)*(ANGLE-PAR2-PAR3)*
     >                                          (ANGLE-PAR2+PAR3)))
          ENDIF
          WRITE(20,*) ANGLE,FR(J)
  210   CONTINUE
        WRITE(20,*)
      ELSEIF (ISHAPE.EQ.0) THEN
         CALL SOLSHP(FR,MF)
         RETURN
      ELSE
C------------------------------------------ THETA(GAMMA(J)) ------------
        DO 10 J=1,MF
          GA = 2*PI*(J-1.D0)/REAL(MF)
          XJO= DCOS(GA + TRIA*DSIN(GA) + QUAD*DSIN(2*GA))
          YJO= ELLIP * DSIN(GA + PAR4*DCOS(GA))
          XJ = XJO * DCOS(PAR1) - YJO*DSIN(PAR1)
          YJ = XJO * DSIN(PAR1) + YJO*DCOS(PAR1) + PAR2
          THETA(J) = DATAN2(YJ,XJ)
   10   CONTINUE
C-------------- INVERSION OF THETA(GAMMA(J)) TO GAMMA(THETA(J)) ------  --
        CALL GRID2NV(THETA,GAMMA,MF,1.D-8,IGRINV,0)
        DO 30 J=1,MF
          GAMM=GAMMA(J)
          XJO= DCOS(GAMM + TRIA*DSIN(GAMM) + QUAD*DSIN(2*GAMM))
          YJO= ELLIP * DSIN(GAMM + PAR4*DCOS(GAMM))
          XV(J) = XJO * DCOS(PAR1) - YJO*DSIN(PAR1)
          YV(J) = XJO * DSIN(PAR1) + YJO*DCOS(PAR1) + PAR2
   30   CONTINUE
        XEAST = XV(1)
        XWEST = XV(MF/2+1)
        X0 = (XEAST + XWEST)/2.
        XL = (XEAST - XWEST)/2.
        DO 35 J=1,MF
          XN = (XV(J) - X0) / XL
          YN =  YV(J) / XL
          FR(J) = DSQRT(XN**2 + YN**2)
   35   CONTINUE
      ENDIF
C------------------- FOURIER COEFFICIENTS FRFNUL AND FRF(M) OF FR(J).
      CALL RFT2(FR,MF,1)
      DO 50 M=1,MF
        FR(M) = 2. * FR(M) / REAL(MF)
   50 CONTINUE
ccc      CALL PRARR1('FR : ',FR,MF,203)
      RETURN
      END
************************************************************************
*DECK FMBOUND
      SUBROUTINE FMBOUND(NBOUND,XBOUND,YBOUND,MF,FR)
C-----------------------------------------------------------------------
C  CALCULATE FOURIER COEFFICIENTS FROM BOUNDARY GIVEN IN (X,Y)
C-----------------------------------------------------------------------
      USE PARAM
      USE COMPIE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XBOUND(*), YBOUND(*), FR(*)
c      DOUBLE PRECISION DUM1(NBOUNDMAX+1),DUM2(NBOUNDMAX+1)
      DOUBLE PRECISION DD1(NBOUNDMAX+1),DD2(NBOUNDMAX+1),
     >     DD3(NBOUNDMAX+1),DD4(NBOUNDMAX+1)
      DOUBLE PRECISION ALB(3)
      write(*,*) 'enter fmbound'
      DO I = 1, NBOUND
         DUMR = DSQRT(XBOUND(I)**2 + YBOUND(I)**2)
         DUMT = -DATAN2(YBOUND(I), XBOUND(I))
         IF (DUMT .LT. 0.D0) DUMT = DUMT + 2. * PI
         XBOUND(I) = DUMT
         YBOUND(I) = DUMR
      ENDDO
c      DO I = 1, NBOUND
c         DUM1(I) = XBOUND(I)
c         DUM2(I) = YBOUND(I)
c      ENDDO
C SELECTION SORT
      DO I = 1, NBOUND
         MAXPOS = 1
         DO J = 2, NBOUND+1-I
            IF (XBOUND(J) .GT. XBOUND(MAXPOS)) THEN
               MAXPOS = J
            ENDIF
         ENDDO
         XBOUND(NBOUND+2-I) = XBOUND(MAXPOS)
         YBOUND(NBOUND+2-I) = YBOUND(MAXPOS)
         XBOUND(MAXPOS) = XBOUND(NBOUND+1-I)
         YBOUND(MAXPOS) = YBOUND(NBOUND+1-I)
      ENDDO
      XBOUND(1) = XBOUND(NBOUND+1) - 2. * PI
      YBOUND(1) = YBOUND(NBOUND+1)
C      DO I = 1,NBOUND
C         IF (XBOUND(I+1).LE.XBOUND(I)) WRITE(*,*)I,XBOUND(I),XBOUND(I+1)
C      ENDDO

C---IF NBOUND IS TOO LARGE, CALLING SPLINE WILL LEAD TO 'BUS ERROR'
      IF (NBOUND.LE.256) THEN
         CALL SPLINE(NBOUND+1,XBOUND,YBOUND,0.D0,0.D0,0,DD1,DD2,DD3,DD4)
         DO I = 1, MF
            THETA0 = 2. * PI * DFLOAT(I) / DFLOAT(MF)
            IF (THETA0.GT.XBOUND(NBOUND+1)) THETA0 = THETA0 - 2*PI
            FR(I) = SPWERT(NBOUND+1,THETA0,DD1,DD2,DD3,DD4,XBOUND,ALB)
         ENDDO
      ELSE
         DO I = 1, MF
            THETA0 = 2. * PI * DFLOAT(I) / DFLOAT(MF)
            IF (THETA0.GT.XBOUND(NBOUND+1)) THETA0 = THETA0 - 2*PI
            DO 40 J = 1, NBOUND
               IF ((XBOUND(J).LE.THETA0).AND.(XBOUND(J+1).GE.THETA0))
     >              GOTO 41
 40         CONTINUE
 41         CONTINUE
            FR(I) = YBOUND(J) + (YBOUND(J+1)-YBOUND(J)) /
     >           (XBOUND(J+1)-XBOUND(J)) * (THETA0-XBOUND(J))
         ENDDO
      ENDIF
      CALL RFT2(FR,MF,1)
      DO 50 M=1,MF
        FR(M) = 2. * FR(M) / REAL(MF)
 50   CONTINUE
      RETURN
      END

************************************************************************
*DECK BUILDBOUND
      SUBROUTINE BUILDBOUND(RBOUND,FR,MF)
C-----------------------------------------------------------------------
C  REBUILD BOUNDARY FROM FOURIER COEFFICIENTS, MOVE TO NEW AXIS
C-----------------------------------------------------------------------
      USE PARAM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION FR(*),RBOUND(*)
      DO I = 1, MF
         RBOUND(I) = FR(I)
      ENDDO
      CALL RFI2(RBOUND,MF,1)
      DO I = 1, MF
         RBOUND(I) = RBOUND(I) * DFLOAT(MF) / 2.
      ENDDO
      RETURN
      END
************************************************************************
*DECK REBUILDFR
      SUBROUTINE REBUILDFR(RBOUND,FR,MF,XAXIS,YAXIS)
C-----------------------------------------------------------------------
C  REBUILD FOURIER COEFFICIENTS WITH MOVED AXIS
C-----------------------------------------------------------------------
      USE PARAM
      USE COMPIE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION FR(*),RBOUND(*),THETA(MBMAX+1),RBOUNDN(MBMAX+1)
      DOUBLE PRECISION DUM1(MBMAX+1),DUM2(MBMAX+1)
      DOUBLE PRECISION DD1(MBMAX+1),DD2(MBMAX+1),DD3(MBMAX+1),
     >     DD4(MBMAX+1)
      DOUBLE PRECISION ALB(3)
      RX = DSQRT(XAXIS**2+YAXIS**2)
      THETAX = DATAN2(YAXIS,XAXIS)
      DO I = 1, MF
         THETA0 = 2. * PI * DFLOAT(I) / DFLOAT(MF)
         R0 = RBOUND(I)
         RN = DSQRT(R0**2+RX**2-2.*RX*R0*DCOS(THETA0-THETAX))
         COSTHETAN = (RX**2+RN**2-R0**2)/2./RX/RN
         THETAN = DACOS(COSTHETAN)
         IF (COSTHETAN.LT.-1.D0) THETAN = PI
         IF (COSTHETAN.GT.1.D0) THETAN = 0.D0
         IF (((THETA0-THETAX).GT.0.D0).AND.((THETA0-THETAX).LT.PI)) THEN
            THETAN = PI - THETAN + THETAX
         ELSE
            THETAN = PI + THETAN + THETAX
         ENDIF
         IF (THETAN.GT.2.*PI) THETAN = THETAN - 2.*PI
         THETA(I) = THETAN
         RBOUNDN(I) = RN
      ENDDO
      NDROP = 0
      DO I = 1, MF
         DUM1(I) = THETA(I)
         DUM2(I) = RBOUNDN(I)
      ENDDO
      DO 30 I = 1,MF-1
         IF (THETA(I+1).LT.THETA(I)) THEN
            NDROP = I
            GOTO 31
         ENDIF
 30   CONTINUE
 31   CONTINUE
      WRITE(*,*)'NDROP',NDROP
      DO I = 1, MF - NDROP
         THETA(I+1) = DUM1(NDROP+I)
         RBOUNDN(I+1) = DUM2(NDROP+I)
      ENDDO
      DO I = MF-NDROP+1, MF
         THETA(I+1) = DUM1(I+NDROP-MF)
         RBOUNDN(I+1) = DUM2(I+NDROP-MF)
      ENDDO
      THETA(1) = THETA(MF+1) - 2. * PI
      RBOUNDN(1) = RBOUNDN(MF+1)
      CALL SPLINE(MF+1,THETA,RBOUNDN,0.D0,0.D0,0,DD1,DD2,DD3,DD4)
      DO I = 1, MF
         THETA0 = 2. * PI * DFLOAT(I) / DFLOAT(MF)
         IF (THETA0.GT.THETA(MF+1)) THETA0 = THETA0 - 2*PI
         FR(I) = SPWERT(MF+1,THETA0,DD1,DD2,DD3,DD4,THETA,ALB)
      ENDDO
      CALL RFT2(FR,MF,1)
      DO 50 M=1,MF
        FR(M) = 2. * FR(M) / REAL(MF)
   50 CONTINUE
      RETURN
      END
************************************************************************
*DECK ROTATE_BND
      SUBROUTINE ROTATE_BND(FR,MF,ANGLE,IAS)
C-------------------------------------------------------------------
C subroutine rotates the plasma boundary by the angle angle
C-------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  FR(*),ANGLE
      INTEGER MF,IAS
      DOUBLE PRECISION  RBND(MF+2)
      
      
      PI = 2.D0*DASIN(1.D0)
      
      IF (IAS .EQ. 1) THEN
             
        DO J=1,MF
        
          THTJ = 2.D0 * PI * FLOAT(J-1)/FLOAT(MF) + ANGLE      
          XP   = FR(1) * DCOS(THTJ) / 2.
          YP   = FR(1) * DSIN(THTJ) / 2.
          
          DO M=2,MF/2
            RM = FR(2*M-1)*DCOS((M-1)*THTJ)-FR(2*M)*DSIN((M-1)*THTJ) 
            XP = XP + RM * DCOS(THTJ)
            YP = YP + RM * DSIN(THTJ)
          ENDDO 

          RBND(J) = DSQRT(XP*XP + YP*YP)
                    
        ENDDO
          
        CALL RFT2(RBND,MF,1)
        
        DO M=1,MF
          FR(M) = 2. * RBND(M) / REAL(MF)
        ENDDO  
   
      ENDIF
      
      RETURN
      
      END
 
                      
************************************************************************
*DECK ELMNO
      SUBROUTINE ELMNO(NR,NP,NODENO)
C-------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE FOUR NODENUMBERS OF EVERY ELEMENT
C-------------------------------------------------------------------
      USE PARAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER NODENO(MAXMNODE,4)
      NO = 0
      DO 10 N=1,NR-1
        DO 20 M=1,NP-1
          NO = NO + 1
          NODENO(NO,1) = (N-1)*NP + M
          NODENO(NO,2) = NODENO(NO,1) + 1
          NODENO(NO,3) = NODENO(NO,2) + NP
          NODENO(NO,4) = NODENO(NO,1) + NP
   20   CONTINUE
   10 CONTINUE
      RETURN
      END
************************************************************************
*DECK MESHAC2
      SUBROUTINE MESHAC2(NR,SG,DSG,DDSG,XR1,SIG1)
C-----------------------------------------------------------------------      
C subroutine to construct non-equidisDTANt radial mesh in Helena
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      PARAMETER (NMAX=1001)
      DOUBLE PRECISION  SG(*),DSG(*),DDSG(*)
      DOUBLE PRECISION  S1(NMAX),F1(NMAX),F2(NMAX),F3(NMAX),
     >                  F4(NMAX),FSUM(NMAX)
      DOUBLE PRECISION  ABLTG(3)
      INTEGER           TYP

C--------------------------------------- set parameters of gaussians
      BGF  = 0.3
      XR2  = 9999.
      SIG2 = 1.
      FACT = 1.
C--------------------------------------- integrate gaussian
      DSI = 1. / FLOAT(NMAX-1)
      S1(1) = 0.
      FSUM(1)  = 0.
      SUM = 0.
      FINT2 = FGAUS(S1(1),BGF,XR1,XR2,SIG1,SIG2,FACT,DFG) 
      DO I=2,NMAX
        S1(I) = FLOAT(I-1) * DSI
        FINT1 = FINT2
        FINT2 = FGAUS(S1(I),BGF,XR1,XR2,SIG1,SIG2,FACT,DFG)
        SUM = SUM + (FINT1+FINT2)/2. * DSI
        FSUM(I) = SUM
      ENDDO
      
      DO I=1,NMAX-1
        FSUM(I) = FSUM(I)/FSUM(NMAX)
      ENDDO
      FSUM(NMAX) = 1.
      ALFA = 0.
      BETA = 0.
      TYP = 2
      CALL SPLINE(NMAX,FSUM,S1,ALFA,BETA,TYP,F1,F2,F3,F4)
      
      SG(1)   = 0.
      DSG(1)  = F2(1)
      DDSG(1) = F3(1)
      DO I=2,NR-1
        FI = FLOAT(I-1)/FLOAT(NR-1)
        SG(I)   = SPWERT(NMAX,FI,F1,F2,F3,F4,FSUM,ABLTG)
        DSG(I)  = ABLTG(1)
        DDSG(I) = ABLTG(2)
      ENDDO
      SG(NR)   = 1.
      DSG(NR)  = F2(NMAX)
      DDSG(NR) = F3(NMAX)
      RETURN
      END

************************************************************************
*DECK FGAUS
      FUNCTION FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT,DFGAUSS)
C-----------------------------------------------------------------------
C     BGF + (1 - BGF) * (GAUSS1 + FACT * GAUSS2) / FACT
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      ZNORM1 = 0.39894 / SIG1
      ZNORM2 = 0.39894 / SIG2
      ZEX1   = -0.5 * (ZS - XR1)**2 / SIG1**2
      ZEX2   = -0.5 * (ZS - XR2)**2 / SIG2**2
      DEX1   = -(ZS-XR1)/SIG1**2
      DEX2   = -(ZS-XR2)/SIG2**2

      F1     = ZNORM1 * DEXP(ZEX1)
      F2     = ZNORM2 * DEXP(ZEX2)
      DF1    = ZNORM1 * DEX1 * DEXP(ZEX1)
      DF2    = ZNORM2 * DEX2 * DEXP(ZEX2)
C
      FGAUS  = BGF + (1.0 - BGF) * (F1 + FACT * F2) / FACT
      DFGAUSS = (1.0-BGF) * (DF1 + FACT * DF2) / FACT
C
      RETURN
      END

************************************************************************
*DECK ARCLENGTH
      SUBROUTINE ARCLENGTH(FR,MF,THETA,DTC,NP,IAS,WR,WS)
C-----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE ARCLENGTH OF THE PLASMA BOUNDARY
C ROUTINE RETURNS THE THETA VALUES AT EQUIDISDTANT ARCLENGTH
C PARAMETERS :
C                FR    : FOURIER SERIES OF BOUNDARY (INPUT)
C                MF    : NUMBER OF HARMONICS IN FR  (INPUT)
C                THETA : RESULTING VALUEs OF THETA
C                DTC   : THE DERIVATIVE OF THETA TO EQUIDISDTANT ANGLE
C                NP    : NUMBER OF POINTS IN THETA,DTC
C                IAS   : CONTROLS SYM/ASYM (INPUT)
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      PARAMETER (NPTS=512)
      DOUBLE PRECISION  FR(*),THETA(*),DTC(*),
     >                  WR(4),WS(4),XL(NPTS),DXL(NPTS)
      INTEGER NF,NP,IAS

      PI = 2.*DASIN(1.D0)
      XL(1) = 0.
C---------------------------------- calculate lenght(theta)
      DO J=1,NPTS-1
        TI1 = FLOAT(J-1)/FLOAT(NPTS-1) * FLOAT(IAS+1)*PI
        TI2 = FLOAT(J)  /FLOAT(NPTS-1) * FLOAT(IAS+1)*PI
C---------------------------------- 4 point Gaussian integration
        DL = 0.
        DO K=1,4
          TK = TI1 + (TI2-TI1)*(WR(K)+1.)/2.
          RR = FR(1)/2.
          DR = 0.
          DO M=2,MF/2
            RR = RR + FR(2*M-1)*DCOS((M-1)*TK)
     >              + FR(2*M)  *DSIN((M-1)*TK)
            DR = DR - FLOAT(M-1)*FR(2*M-1)*DSIN((M-1)*TK)
     >              + FLOAT(M-1)*FR(2*M)  *DCOS((M-1)*TK)
          ENDDO
          DL = DL + DSQRT(RR*RR + DR*DR) * WS(K) 
        ENDDO
        XL(J+1) = XL(J) + DL * FLOAT(IAS+1)/2 *PI/FLOAT(NPTS-1)
        RR = FR(1)/2.
        DR = 0.
        DO M=2,MF/2
           RR = RR + FR(2*M-1)*DCOS((M-1)*TI2)
     >             + FR(2*M)  *DSIN((M-1)*TI2)
           DR = DR - FLOAT(M-1)*FR(2*M-1)*DSIN((M-1)*TI2)
     >             + FLOAT(M-1)*FR(2*M)  *DCOS((M-1)*TI2)
        ENDDO
      DXL(J+1) = DSQRT(RR*RR + DR*DR)
      ENDDO
      TI2 = 0.
      RR = FR(1)/2.
      DR = 0.

      DO M=2,MF/2
        RR = RR + FR(2*M-1)*DCOS((M-1)*TI2)
     >          + FR(2*M)  *DSIN((M-1)*TI2)
        DR = DR - FLOAT(M-1)*FR(2*M-1)*DSIN((M-1)*TI2)
     >          + FLOAT(M-1)*FR(2*M)  *DCOS((M-1)*TI2)
      ENDDO
      DXL(1) = DSQRT(RR*RR + DR*DR)
c      WRITE(*,2) (I,XL(I),DXL(I),I=1,NPTS)
    2 format(i4,2f10.6)

      THETA(1) = 0.
      THETA(NP) = FLOAT(IAS+1)*PI
      DTC(1)  = 1./(DXL(1)    * FLOAT(IAS+1)*PI/XL(NPTS))
      DTC(NP) = 1./(DXL(NPTS) * FLOAT(IAS+1)*PI/XL(NPTS))
      I=1
      DO J=2,NP-1
        CHIL = FLOAT(J-1)/FLOAT(NP-1) * XL(NPTS)
        DO WHILE ((CHIL.GT.XL(I)).AND.(I.LT.NPTS))
          I = I+1
         ENDDO
c--------------------------- interval located, use linear interpolation
        THETA(J) = (FLOAT(I-1) + (CHIL-XL(I))/(XL(I+1)-XL(I)) )
     >           / FLOAT(NPTS-1) * FLOAT(IAS+1)*PI
        DCT   = ( (CHIL-XL(I))/(XL(I+1)-XL(I)) * (DXL(I+1)-DXL(I))
     >           +  DXL(I) ) * FLOAT(IAS+1)*PI/XL(NPTS)
        DTC(J) =  1./ DCT
      ENDDO
      RETURN
      END
************************************************************************
*DECK INIGRID
      SUBROUTINE INIGRID(XX,YY,PSI,NR,NP,FR,MHARM,IAS,XAXIS,YAXIS)
C--------------------------------------------------------------------
C ON EXIT XX AND YY ARE FILLED WITH THE VALUES OF X,XR,XS,XRS AND
C Y,YR,YS,YRS OF EVERY NODE :
C XX(1,NODE) = X, XX(2,NODE) = XR, XX(3,NODE) = XS, XX(4,NODE) = XRS
C THE SHAPE OF THE BOUNDARY IS GIVEN BY FR(M)
C--------------------------------------------------------------------
      USE PARAM
      USE GAUDSINT
      USE MESHAC
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*),YY(4,*),FR(*),PSI(*),
     >                  THETA(NPMMAX),DTC(NPMMAX)

      PI = 2.*DASIN(1.D0)
      DT = (1.+FLOAT(IAS))*PI/REAL(NP-1)
      DR = 1./REAL(NR-1)

C--------------------------- change theta grid to consDTANt arclenght
      IF (IARC.EQ.1) THEN
        MF = 2.*MHARM
        CALL ARCLENGTH(FR,MF,THETA,DTC,NP,IAS,XGAUSS,WGAUSS)
      ELSE
        DO J=1,NP
          THETA(J) = DT * REAL(J-1)
          DTC(J) = 1.
        ENDDO
      ENDIF

      IF ((IMESH.EQ.2).AND.(XR1.LE.1.).AND.(SIG1.LT.1.)) THEN
        CALL MESHAC2(NR,SG,DSG,DDSG,XR1,SIG1)
        NRDONE = NR
        XR1DONE = XR1
	SIG1DONE = SIG1
      ELSE
        DO I=1,NR
          SG(I)  = FLOAT(I-1)/FLOAT(NR-1)
          DSG(I)  = 1.
          DDSG(I) = 0.
        ENDDO
      ENDIF

      DO 10 I=1,NR
        DO 20 J=1,NP
          NODE = NP*(I-1) + J
          THTJ = THETA(J)
          RADIUS = SG(NR-I+1) 
          XX(1,NODE) = RADIUS * FR(1) * DCOS(THTJ) / 2.
          XX(2,NODE) = FR(1) * DCOS(THTJ)          / 2.
          XX(3,NODE) = - RADIUS * FR(1) * DSIN(THTJ) / 2.
          XX(4,NODE) = - FR(1) * DSIN(THTJ)          / 2.
          YY(1,NODE) = RADIUS * FR(1) * DSIN(THTJ)   / 2.
          YY(2,NODE) = FR(1) * DSIN(THTJ)            / 2.
          YY(3,NODE) = RADIUS * FR(1) * DCOS(THTJ)   / 2.
          YY(4,NODE) = FR(1) * DCOS(THTJ)            / 2.
C---------------------------- KEEP ELLIPTICITY ON AXIS -----------
          DO 30 M = 2,MHARM
            IF (M.EQ.2) THEN
              RM = RADIUS * ( FR(2*M-1) * DCOS((M-1)*THTJ)
     >                      + FR(2*M)   * DSIN((M-1)*THTJ) )
              DRM = ( FR(2*M-1) * DCOS((M-1)*THTJ)
     >              + FR(2*M)   * DSIN((M-1)*THTJ))
              DRMT = RADIUS*(- FR(2*M-1) *REAL(M-1)*DSIN((M-1)*THTJ)
     >                       + FR(2*M)   *REAL(M-1)*DCOS((M-1)*THTJ))
              DRMTR= (-FR(2*M-1)*REAL(M-1)*DSIN((M-1)*THTJ)
     >                +FR(2*M) *REAL(M-1)*DCOS((M-1)*THTJ))
            ELSE
              RM = RADIUS**(M-1) * ( FR(2*M-1) * DCOS((M-1)*THTJ)
     >                           + FR(2*M)   * DSIN((M-1)*THTJ) )
              DRM =(M-1)*RADIUS**(M-2) * ( FR(2*M-1) * DCOS((M-1)*THTJ)
     >                                  + FR(2*M)   * DSIN((M-1)*THTJ))
              DRMT = RADIUS**(M-1)*(- FR(2*M-1) *(M-1)*DSIN((M-1)*THTJ)
     >                              + FR(2*M)   *(M-1)*DCOS((M-1)*THTJ))
            DRMTR=(M-1)*RADIUS**(M-2)*(-FR(2*M-1)*(M-1)*DSIN((M-1)*THTJ)
     >                                 +FR(2*M) *(M-1)*DCOS((M-1)*THTJ))
            ENDIF
            XX(1,NODE) = XX(1,NODE) + RM * DCOS(THTJ)
            YY(1,NODE) = YY(1,NODE) + RM * DSIN(THTJ)
            XX(2,NODE) = XX(2,NODE) + DRM * DCOS(THTJ)
            YY(2,NODE) = YY(2,NODE) + DRM * DSIN(THTJ)
            XX(3,NODE)= XX(3,NODE) - RM * DSIN(THTJ) + DRMT * DCOS(THTJ)
            YY(3,NODE)= YY(3,NODE) + RM * DCOS(THTJ) + DRMT * DSIN(THTJ)
            XX(4,NODE)= XX(4,NODE) - DRM * DSIN(THTJ)+DRMTR * DCOS(THTJ)
            YY(4,NODE)= YY(4,NODE) + DRM * DCOS(THTJ)+DRMTR * DSIN(THTJ)
   30     CONTINUE
          XX(1,NODE) = XX(1,NODE) + XAXIS
          XX(2,NODE) = - XX(2,NODE) * DR/2. * DSG(NR-I+1)
          XX(3,NODE) =   XX(3,NODE) * DT/2. * DTC(J)
          XX(4,NODE) = - XX(4,NODE) * DR/2. * DT/2. * DTC(J)*DSG(NR-I+1)
          YY(1,NODE) = YY(1,NODE) - YAXIS
          YY(2,NODE) = - YY(2,NODE) * DR/2. * DSG(NR-I+1)
          YY(3,NODE) =   YY(3,NODE) * DT/2. * DTC(J)
          YY(4,NODE) = - YY(4,NODE) * DR/2. * DT/2. * DTC(J)*DSG(NR-I+1)
          PSI(4*(NODE-1)+1) = RADIUS **2
          PSI(4*(NODE-1)+2) = - 2.* RADIUS * DR / 2. * DSG(NR-I+1)
          PSI(4*(NODE-1)+3) = 0.
          PSI(4*(NODE-1)+4) = 0.
   20   CONTINUE
   10 CONTINUE
      RETURN
      END


************************************************************************
*DECK PLOTGR
      SUBROUTINE PLOTGR(MESHNO,XX,YY,NR,NP,IAS, IFILE)
C-------------------------------------------------------------------
C THE X,Y GRID IS PLOTTED UDSING THE ISOPARAMETRIC REPRESENTATION
C-------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION          XX(4,*), YY(4,*)
      CHARACTER*3  STRING
      CHARACTER*13 LABEL
      INTEGER      IPLT, MESHNO, IAS, NR, NP
      
C      IF (IAS.EQ.1) THEN
      DO 10 IR=1,NR
        DO 20 IP=1,NP
          NBASE = IP + (IR-1)*NP
          WRITE(IFILE,*) XX(1,NBASE), YY(1,NBASE)
   20   CONTINUE
   10 CONTINUE
      DO 30 IP = 1, NP
         WRITE(IFILE,*) XX(1,IP), YY(1,IP)
         WRITE(IFILE,*) XX(1,NR*NP), YY(1,NR*NP)
 30   CONTINUE
      RETURN
      END
************************************************************************
*DECK PLOTPSIPRES
      SUBROUTINE PLOTPSIPRES(XX,YY,PSI,XAXIS,YAXIS,A,NR,NP,IAS,IFILE)
      USE COMPLO
      USE COMPIE
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION          XX(4,*), YY(4,*), PSI(*)
      YMAX = -1.D30
      YMIN =  1.D30
      XPOSMAX = 0.D0
      XPOSMIN = 0.D0

      WRITE(IFILE,*)'# X Y PSI RHO PRESURE(PAR) PRESURE(PER) DET'
C-----AT AXIS----
      CALL CALCBTPD(XAXIS,0.D0,0.D0,A,BTOT0,TPER0,DET0)
      RHO0 = CALCRHO(XAXIS, 0.D0) * TPER0 / TEMS(0.D0)
      PRESPAR0 = RHO0 * TEMS(0.D0)
      PRESPER0 = RHO0 * TPER0
C-----THE RANGE OF X AND Y----
      DO 10 IR=1, NR-1
         DO 20 IP=1,NP
            NBASE = IP + (IR-1)*NP
            IF (YY(1,NBASE).GT.YMAX) THEN
               YMAX = YY(1,NBASE)
               XPOSMAX = XX(1,NBASE)
            ENDIF
            IF (YY(1,NBASE).LT.YMIN) THEN
               YMIN = YY(1,NBASE)
               XPOSMIN = XX(1,NBASE)
            ENDIF
 20      CONTINUE
 10   CONTINUE
C-----FIND X,Y GRID------
      DO 70 IX = 1, NXGRID
         X = -1.D0 + 2.D0 / DFLOAT(NXGRID-1) * DFLOAT(IX-1)
         DO 80 IY = 1, NYGRID
            Y = YMIN + (YMAX - YMIN) * DFLOAT(IY-1) / DFLOAT(NYGRID-1)
            THETA = DATAN2(Y-YAXIS,X-XAXIS)
            IF (THETA.LT.0.D0) THETA = THETA + 2 * PI
            R = DSQRT((Y-YAXIS)**2 + (X-XAXIS)**2)
c            WRITE(*,*)'X,Y,R',X,Y,R
            IF (IAS.GT.0) THEN
               DT = 2.D0 * PI / DFLOAT(NP-1)
               NTPOS = INT(THETA / DT) + 1
               TPOS  = (THETA/DT - FLOAT(NTPOS-1)) * 2.D0 - 1.D0
            ELSE
               DT = 1.D0 * PI / DFLOAT(NP-1)
               NTPOS = INT(THETA / DT) + 1
               TPOS  = (THETA/DT - FLOAT(NTPOS-1)) * 2.D0 - 1.D0
            ENDIF
C            write(*,*)theta,ntpos,tpos,dt
            DO 30 IR = 1, NR-1
               N  = NTPOS + (IR-1) * (NP-1)
               N1 = NODENO(N,1)
               N2 = NODENO(N,2)
               N3 = NODENO(N,3)
               N4 = NODENO(N,4)
               R0 = -1.D0
               CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >              R0,TPOS,X1,XR,XS)
               CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >              R0,TPOS,Y1,YR,YS)
               R0 = 1.D0
               CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >              R0,TPOS,X2,XR,XS)
               CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >              R0,TPOS,Y2,YR,YS)
               R1 = DSQRT((Y1-YAXIS)**2 + (X1-XAXIS)**2)
               R2 = DSQRT((Y2-YAXIS)**2 + (X2-XAXIS)**2)
C------IF OUT OF BOUND, OUTPUT DEFAULT VALUE-------
               IF ((IR.EQ.1).AND.(R.GE.MAX(R1,R2))) THEN
                  WRITE(IFILE,*) X, Y, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0
                  GOTO 80
               ENDIF
               IF (((R1-R)*(R2-R)).LT.0.D0) THEN
                  RLEFT = -1.D0 - 1.D-8
                  RRIGHT = 1.D0 + 1.D-8
                  IF ((R2-R).LT.0.D0) THEN
                     RLEFT  =  1.D0 + 1.D-8
                     RRIGHT = -1.D0 - 1.D-8
                  ENDIF
                  DO 40 ITRY = 1, 1000
                     RMID = 0.5D0*(RLEFT + RRIGHT)
                     CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                    RMID,TPOS,X1,XR,XS)
                     CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                    RMID,TPOS,Y1,YR,YS)
                     RVMID = DSQRT((Y1-YAXIS)**2 + (X1-XAXIS)**2)
                     IF (DABS(RVMID - R).LT.1.D-8) GOTO 50
                     IF (RVMID.GT.R) THEN
                        RRIGHT = RMID
                     ELSE
                        RLEFT  = RMID
                     ENDIF
 40               CONTINUE
C--------CACULATE PSI, RHO, PRESURE, DET
 50               CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >                 PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),RMID,TPOS,PS,
     >                 PSR,PSS)

                  XJAC =  XR*YS - XS*YR
                  PYJAC = PSR*YS - PSS*YR
                  PXJAC = PSR*XS - PSS*XR
                  PSX = PYJAC / XJAC
                  PSY = -PXJAC / XJAC
                  SUMDPSI = DSQRT(PSX**2 + PSY**2)
                  CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
                  TEMP = TEMS(PS)
                  RHO = CALCRHO(X,PS) * TPER / TEMP
                  PRESPAR = RHO * TEMP
                  PRESPER = RHO * TPER                  
                  WRITE(IFILE,*) X, Y, PS, RHO/RHO0, PRESPAR/PRESPAR0,
     >                 PRESPER/PRESPAR0, DET
                  GOTO 80
               ENDIF
 30         CONTINUE
            WRITE(*,*)'PLOT POINT NOT FOUND', X,Y
 80      CONTINUE
         WRITE(IFILE,*)
 70   CONTINUE
      RETURN
      END
************************************************************************
*DECK PLOTCU
      SUBROUTINE PLOTCU(X1,X1S,Y1,Y1S,X2,X2S,Y2,Y2S)
C-----------------------------------------------------------------------
C PLOTS A CUBIC LINE FROM X1,Y1 TO X2,Y2 GIVEN BY THE ARRAYS XI(1..4)
C AND YI(1..4)
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XP(5),YP(5)
      DO 10 I=1,5
        S = -1.+2*(I-1)/4.
        CALL CUB1D(X1,X1S,X2,X2S,S,XP(I),DUMMY)
        CALL CUB1D(Y1,Y1S,Y2,Y2S,S,YP(I),DUMMY)
   10 CONTINUE
c      CALL LPLOT6(2,1,XP,YP,-5,' ')
      RETURN

      END
************************************************************************
*DECK PLOTPSIC
C      SUBROUTINE PLOTPSIC(XX, YY, PSI, NR, NP, IAS, IFILE)
C-----------------------------------------------------------------------
C PLOT THE CONTOUR OF PSI(X,Y)
C-----------------------------------------------------------------------
C      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
C      DOUBLE PRECISION XX(4,*), YY(4,*), PSI(4,*)
C      INTEGER          NR, NP, IAS
C
C      DO 10 IR=1,NR
C        DO 20 IP=1,NP
C          NBASE = IP + (IR-1)*NP
C          WRITE(IFILE,*) XX(1,NBASE), YY(1,NBASE), PSI(1,NBASE)
C   20   CONTINUE
C   10 CONTINUE
            
************************************************************************
*DECK CUB1D
      SUBROUTINE CUB1D(X1,X1S,X2,X2S,S,X,XS)
C-----------------------------------------------------------------------
C CUBIC HERMITE INTERPOLATION IN ONE DIMENSION
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  H0M,H0P,H1M,H1P,H0MS,H0PS,H1MS,H1PS

      H0M  =  (S-1.)**2 *(S+2.) * 0.25
      H0MS =  (S-1.)*(S+2.)/2. + (S-1.)**2 * 0.25
      H0P  = -(S+1.)**2 *(S-2.) * 0.25
      H0PS = -(S+1.)*(S-2.)/2. - (S+1.)**2 * 0.25
      H1M  =  (S-1.)**2 *(S+1.) * 0.25
      H1MS =  (S-1.)*(S+1.)/2. + (S-1.)**2 * 0.25
      H1P  =  (S+1.)**2 *(S-1.) * 0.25
      H1PS =  (S+1.)*(S-1.)/2. + (S+1.)**2 * 0.25

      X  = X1*H0M  + X1S*H1M +  X2*H0P  + X2S*H1P
      XS = X1*H0MS + X1S*H1MS + X2*H0PS + X2S*H1PS
      RETURN
      END
************************************************************************
*DECK CALCBTPD
      SUBROUTINE CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
C-----------------------------------------------------------------------
C   INPUT   SUMDPSI = |GRAD(PSI)|
C CALCULATE BTOT = SQRT(B(PHI)**2 + B(POL)**2)
C    AND    T(PERPENDICULAR)
C    AND    DET = (P(PAR) - P(PERPEND)) / B**2
C-----------------------------------------------------------------------
      USE COMDAT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      RHO = CALCRHO(X,PS)
      TEMP = TEMS(PS)
      THETAPS = THES(PS)
      FPS = XGAMMA(PS)
C-----WITHOUT ANISOTROPIC, OR OUT OF BOUND
      IF ((ITH.LT.1).OR.(PS.GE.1.D0).OR.(PS.LT.0.D0)) THEN
         DET = 0.D0
         TPER = TEMP
         BTOT = DSQRT(DABS(A)*FPS**2 + SUMDPSI**2) / (1+EPS*X)
         GOTO 20 
      ENDIF
C-----BINARY SEARCH DET
      IF (THTOF.GT.0.D0) THEN
         DET = 0.D0
         BTOT = DSQRT(DABS(A)*FPS**2/(1-DET)**2 + SUMDPSI**2)
     >        /(1+EPS*X)
         TPER = BTOT*TEMP / 
     >        DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
         BM0 = DET - TPER/TEMP * RHO * DABS(A) *B*(TEMP - TPER)/BTOT**2
         DET = -0.3D0
         BTOT = DSQRT(DABS(A)*FPS**2/(1-DET)**2 + SUMDPSI**2)
     >        /(1+EPS*X)
         TPER = BTOT*TEMP / 
     >        DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
         BM3 = DET - TPER/TEMP * RHO * DABS(A) *B*(TEMP - TPER)/BTOT**2
         IF (BM3*BM .LT.0) THEN
            DETL = -0.3
            DETR = 0
            GOTO 40
         ENDIF
         DO I = 1, 1000
            DET = DFLOAT(I)/1000.D0 * (-1)
            BTOT = DSQRT(DABS(A)*FPS**2/(1-DET)**2 + SUMDPSI**2)
     >           /(1+EPS*X)
            TPER = BTOT*TEMP / 
     >           DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
            BM   =DET - TPER/TEMP * RHO *DABS(A)*B*(TEMP - TPER)/BTOT**2
            IF (BM*BM0 .LT.0) THEN
               DETL = DET
               DETR = 0
               GOTO 40
            ENDIF
         ENDDO
         STOP 'NO SOLUTION TO DELTA'
      ELSE
         DETR = 0.8D0
         DETL = 0
      ENDIF
 40   BTOT = DSQRT(DABS(A)*FPS**2/(1-DETL)**2 + SUMDPSI**2)/(1+EPS*X)
      TPER = BTOT*TEMP / 
     >     DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
      BL = DETL - TPER/TEMP * RHO * DABS(A) * B * (TEMP - TPER) /BTOT**2

      BTOT = DSQRT(DABS(A)*FPS**2/(1-DETR)**2 +SUMDPSI**2)/(1+EPS*X)
      TPER = BTOT*TEMP / 
     >     DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
      BR = DETR - TPER/TEMP * RHO * DABS(A) * B * (TEMP - TPER) /BTOT**2
      
C---------------------------------
      DO 10 I = 1, 1000
         DET = (DETL + DETR)/2
         BTOT = DSQRT(DABS(A)*FPS**2/(1-DET)**2 + SUMDPSI**2)/(1+EPS*X)
         TPER  = BTOT*TEMP / 
     >        DABS(BTOT - DSQRT(DABS(A)) * TEMP * THETAPS * THTOF)
         BM   = DET - TPER/TEMP * RHO * DABS(A) *B*(TEMP - TPER)/BTOT**2     
         IF (BM*BR.GE.0) THEN
            DETR = DET
         ELSE
            DETL = DET
         ENDIF
         
         IF (DABS(DETL-DETR).LE.1.D-12) THEN
            DET = 0.5*(DETL+DETR)
            GOTO 20
         ENDIF
 10   CONTINUE
 20   IF (DABS(DETR).EQ.0.8D0) STOP 'MIRROR INSTABILITY OCCURS'

      RETURN
      END
************************************************************************
*DECK CALCRHO
      DOUBLE PRECISION FUNCTION CALCRHO(X,PS)
C-----------------------------------------------------------------------
C CALCULATE RHO * T(PAR)/T(PER) AS A FUNCTION OF X AND PSI
C----------------------------------------------------------------------- 
      USE COMDAT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

C     AVOID SINGULARITY
      IF ((PS.GT.1.D0 - 1.D-5).AND.(TEMS(PS).LT.1.D-5)) THEN
         PS =1.D0 - 1.D-4
      ENDIF

      RHO = DEXP(HOT*PRES(PS) / TEMS(PS))
      RHO = RHO* DEXP(OMGOT*0.5*(1.+EPS*X)**2*OMGS(PS)/TEMS(PS))
      CALCRHO = RHO
      RETURN
      END

************************************************************************
*DECK CALCRJPHI
      SUBROUTINE CALCRJPHI(X,PS,SUMDPSI,A,ARHS,DET)
c-----------------------------------------------------------------------
c FUNCTION TO CALCULATE R * JPHI(PSI,X) (=LAPLACE(*)PSI)
C PHYSICS IS SPECIFIED HERE
C INPUT SUMDPSI = |GRAD(PSI)|
C NOT INCLUDED: DIV(DET / R**2 * GRAD(PSI))
C-----------------------------------------------------------------------
      USE COMDAT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IF ((PS.GT.1.D0 - 1.D-5).AND.(TEMS(PS).LT.1.D-5)) THEN
         PS =1.D0 - 1.D-4
      ENDIF
      FLAG = DET
      RHO = CALCRHO(X,PS)
      TEMP = TEMS(PS)
      THE = THES(PS)
      H = PRES(PS)
      OMG = OMGS(PS)
      F = XGAMMA(PS)
      DTE = DTEDPSI(PS)
      DTH = DTHDPSI(PS)
      DH = DPDPSI(PS)
      DOMG = DOMDPSI(PS)
      DF = DGDPSI(PS)
      CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
      RHO = TPER / TEMP * RHO
      BMTT = BTOT - DSQRT(DABS(A)) * TEMP * THE* THTOF
      DTPER = DTE*DABS(BMTT) + THTOF *
     >     DSQRT(DABS(A))*(DTE*THE+TEMP*DTH)*DSIGN(TEMP,BMTT)
      DTPER = DTPER / BMTT**2 * BTOT
      DTOT = DTE * TPER - DTPER * TEMP
      DTOT = DTOT / TPER**2
      DWDPSI = DTE*DLOG(RHO*TEMP/TPER) + TPER * DTOT
      DTTOT = DTE + DH*HOT + (1.D0+EPS*X)**2*DOMG*OMGOT*0.5 - DWDPSI
C      WRITE(*,*)'DWDPSI DH DTE DTTOT', DWDPSI,DH,DTE,DTTOT
      ARHS = -1.D0 * DTTOT * B * RHO * (1.+EPS*X)**2
       
      ARHS = ARHS - DF * F / (1.D0 - DET)
      IF (FLAG.EQ.100.D0) ARHS = df
      
      RETURN
      END
************************************************************************
*DECK CALCRJPHIC
      SUBROUTINE CALCRJPHIC(X,PS,SUMDPSI,A,ARHSPAR,ARHSPER,ARHSMAG,
     >     PARDEV, PERDEV)
c-----------------------------------------------------------------------
c FUNCTION TO CALCULATE R * JPHI(PSI,X), EACH COMPONENT
C PHYSICS IS SPECIFIED HERE
C INPUT SUMDPSI = |GRAD(PSI)|
C NOT INCLUDED: DIV(DET / R**2 * GRAD(PSI))
C-----------------------------------------------------------------------
      USE COMDAT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      RHO0 = CALCRHO(X,PS)
      TEMP = TEMS(PS)
      THE = THES(PS)
      H = PRES(PS)
      OMG = OMGS(PS)
      F = XGAMMA(PS)
      DTE = DTEDPSI(PS)
      DTH = DTHDPSI(PS)
      DH = DPDPSI(PS)
      DOMG = DOMDPSI(PS)
      DF = DGDPSI(PS)
      CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
      RHO = TPER / TEMP * RHO0
      BMTT = BTOT - DSQRT(DABS(A)) * TEMP * THE* THTOF
      DTPER = DTE*DABS(BMTT) + THTOF *
     >     DSQRT(DABS(A))*(DTE*THE+TEMP*DTH)*DSIGN(TEMP,BMTT)
      DTPER = DTPER / BMTT**2 * BTOT
      DTOT = DTE * TPER - DTPER * TEMP
      DTOT = DTOT / TPER**2
      DWDPSI = DTE*DLOG(RHO*TEMP/TPER) + TPER * DTOT
      DTTOT = DTE + DH*HOT + (1.D0+EPS*X)**2*DOMG*OMGOT*0.5 - DWDPSI
C      WRITE(*,*)'DWDPSI DH DTE DTTOT', DWDPSI,DH,DTE,DTTOT
      ARHS = DTTOT * B * RHO
      PARDEV = -ARHS * (1+EPS*X)**2;
      ARHSPAR = -(SUMDPSI/BTOT/(1+EPS*X))**2 * ARHS * (1+EPS*X)**2
      
      FBPSI = BTOT/BMTT
      PSPI = B * (DTE + HOT*DH + (1+EPS*X)**2*DOMG*OMGOT*0.5 - 
     >   HOT*DTE*H/TEMP - 0.5*(1+EPS*X)**2*OMGOT*OMG*DTE/TEMP) * RHO0
      ARHSPER = 2 * FBPSI * ARHS - FBPSI**2 * PSPI
      PERDEV = -ARHSPER*(1+EPS*X)**2
      ARHSPER = -(1-(SUMDPSI/BTOT/(1+EPS*X))**2)*ARHSPER*(1+EPS*X)**2
      
      ARHS = -ARHS * (1+EPS*X)**2 - DF * F / (1.D0 - DET)
      ARHSMAG = ARHS - ARHSPER - ARHSPAR
      
      RETURN
      END
************************************************************************

*DECK FORMKQ
      SUBROUTINE FORMKQ(XX,YY,PSI,NR,NP,QQ,A,B,C,OMGOT,HOT,EPS,IGAM,ITH,
     >                  ISOL,ITER,IAS)
C----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE MATRIX KK AND THE RIGHT HAND SIDE ARRAY QQ
C NO BOUNDARY CONDITION ARE YET USED.
C NUMBER OF ROWS AND COLUMNS : 4*NR*NP
C NR : NUMBER OF RADIAL NODES
C NP : NUMBER OF POLOIDAL NODES
C A  : THE TOTAL AMPLITUDE OF THE RHS (HBT DEFINITION)
C B  : MEASURE OF THE TOTAL PRESSURE   (HBT DEFINITION)
C EPS : THE INVERS ASPECT RATIO
C IGAM=1-4 : HBT GAMMA PROFILE INPUT, 5- : FF' AS INPUT PROFILE.
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMSOLV
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  QQ(*)
      DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*)
      DOUBLE PRECISION  GPX(4,4,MAXNODE),GPJAC(4,4,MAXNODE)
      DOUBLE PRECISION  GXR(4,4,MAXNODE),GXS(4,4,MAXNODE)
      DOUBLE PRECISION  GYR(4,4,MAXNODE),GYS(4,4,MAXNODE)
      INTEGER INDEX(MAXNODE)

      SAVE GPX,GPJAC,INDEX,GXR,GXS,GYR,GYS
      NELM = (NP-1)*(NR-1)
      IF (ITER.EQ.1) THEN

C-----------------------------------------------------------------------
C CALCULATE THE INDEX IN THE MATRIX. IT IS REORDERED FROM 
C CLOCKWISE ORDERING IN THE REST OF HELENA TO SAVE REDUCE THE MATRIX SIZE
C BY A FACTOR OF 2.
C THE POLOIDAL INDEX J CHANGES TO:
C          JN = 1           (J=1, NP)
C          JN = 2*(J-1),    (2 .LE. J .GE. (NP-1)/2+1)     
C          JN = 2*(NP-J)+1, ((NP-1)/2 + 2 .LE. J .GE. NP-1)
C-----------------------------------------------------------------------
         DO 10 I=1,4*(NR+2)*NP
            DO 20 J=1,KKLDA
               KKBIG(J,I) = 0.
 20         CONTINUE
 10      CONTINUE
      IF (IAS.EQ.1) THEN
        DO I=1,NR
          J  = 1
          JN = 1
          IJ1 = (I-1)*NP     + J 
          IJN = (I-1)*(NP-1) + JN
          INDEX(IJ1) = IJN
          DO J=2,(NP-1)/2+1
            JN = 2*(J-1)
            IJ1 = (I-1)*NP     + J
	    IJN = (I-1)*(NP-1) + JN
	    INDEX(IJ1) = IJN
          ENDDO
	  DO J=(NP-1)/2+2,NP
	    JN = 2*(NP-J)+1
	    IJ1 = (I-1)*NP     + J
	    IJN = (I-1)*(NP-1) + JN
	    INDEX(IJ1) = IJN
          ENDDO
        ENDDO
      ELSE
        DO I=1,NR
	  DO J=1,NP
	    IJ1 = (I-1)*NP + J
	    INDEX(IJ1) = IJ1
	  ENDDO
	ENDDO
      ENDIF	
C------------------------------------- NELM ELEMENTS ------------------        
      DO 50 N=1,NELM
        N1 = NODENO(N,1)
        N2 = NODENO(N,2)
        N3 = NODENO(N,3)
        N4 = NODENO(N,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN R -----
        DO 60 NGR=1,4
          R = XGAUSS(NGR)
          WR = WGAUSS(NGR)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
          DO 70 NGS=1,4
            S  = XGAUSS(NGS)
            WS = WGAUSS(NGS)
            WRS = WR * WS
!            CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
!     >                  R,S,X,XR,XS)
!            CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
!     >                  R,S,Y,YR,YS)
            CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1)
     >                  ,PSI(4*(N4-1)+1),R,S,PS,PSR,PSS)
            CALL INTERP3(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                   YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                PSI(4*(N4-1)+1),R,S,X,XR,XS,YR,YS,PS)

            XJAC =  XR*YS - XS*YR
            GPX(NGR,NGS,N) = X
            GPJAC(NGR,NGS,N) = XJAC

            GXR(NGR,NGS,N) = XR
            GXS(NGR,NGS,N) = XS
            GYR(NGR,NGS,N) = YR
            GYS(NGR,NGS,N) = YS
            PYJAC = PSR*YS - PSS*YR
            PXJAC = PSR*XS - PSS*XR
            PSX = PYJAC / XJAC
            PSY = -PXJAC / XJAC
            SUMDPSI = DSQRT(PSX**2 + PSY**2)
C----------------------------------------- CALCULATE RIGHT HAND SIDE ---

C           CALCRJPHI(X,PS,SUMDPSI,A,ARHS,DET)
            CALL CALCRJPHI(X,PS,SUMDPSI,A,ARHS,DET)
            ARHS = A * ARHS / (1.+EPS*X)
C------------------------------------- 4 NODES PER ELEMENT ------------
            DO 80 I=1,4
C------------------------------------- 4 FUNCTIONS V PER NODE ---------
              DO 90 J=1,4
                NROW = 4*(INDEX(NODENO(N,I)) - 1) + J
                SUMQ =  ARHS * H(J,I,NGR,NGS) * XJAC
                VX =  YS*HR(J,I,NGR,NGS)  - YR*HS(J,I,NGR,NGS)
                VY = -XS*HR(J,I,NGR,NGS)  + XR*HS(J,I,NGR,NGS)
c                IF (ITH.EQ.1) THEN
c                   DETTERM = - DET/(1+EPS*X)*(PSIX*VX+PSIY*VY)
c                   SUMQ = SUMQ + DETTERM
c                   DET = 0.
c                ENDIF
                QQ(NROW) = QQ(NROW) + WRS * SUMQ     
C------------------------------------- 4 NODES OF FUNCTION PSI --------
                DO 100 K=1,4
C------------------------------------- 4 FUNCTIONS H IN PSI -----------
                  DO 110 L=1,4
                    NCOL = 4*(INDEX(NODENO(N,K)) - 1) + L
                    NOFF = NROW - NCOL
                    IF (NOFF.GE.0) THEN
                      PSIX =  YS*HR(L,K,NGR,NGS)  - YR*HS(L,K,NGR,NGS)
                      PSIY = -XS*HR(L,K,NGR,NGS)  + XR*HS(L,K,NGR,NGS)
                      SUMK = - 1./(1.+EPS*X) * (PSIX*VX+PSIY*VY) / XJAC
                      KKBIG(NOFF+1,NCOL) = KKBIG(NOFF+1,NCOL)
     >                                   + WRS * SUMK * (1.-DET)
                    ENDIF                     
  110             CONTINUE
  100           CONTINUE
   90         CONTINUE
   80       CONTINUE
   70     CONTINUE
   60   CONTINUE
   50 CONTINUE
C------------------------------------------- REMOVE EMPTY COLUMNS (BND.)
      NEND = NP - 1
      IF (IAS.EQ.0) NEND=NP
      DO J=1,NEND
          KKBIG(1,4*J-3) = 1.D20
          KKBIG(1,4*J-1) = 1.D20
      ENDDO
C------------------------------------------- IF MATRIX EXISTS THEN
      ELSE
         IF (ITH.GE.1) THEN
            DO I=1,4*(NR+2)*NP
               DO J=1,KKLDA
                  KKBIG(J,I) = 0.D0
               ENDDO
            ENDDO
         ENDIF
       DO 150 N=1,NELM
        N1 = NODENO(N,1)
        N2 = NODENO(N,2)
        N3 = NODENO(N,3)
        N4 = NODENO(N,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN R -----
        DO 160 NGR=1,4
          R = XGAUSS(NGR)
          WR = WGAUSS(NGR)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----

          DO 170 NGS=1,4
            S  = XGAUSS(NGS)
            WS = WGAUSS(NGS)
            WRS = WR * WS
            X = GPX(NGR,NGS,N)
            XJAC = GPJAC(NGR,NGS,N)
            CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1)
     >                  ,PSI(4*(N4-1)+1),R,S,PS,PSR,PSS)
            XR = GXR(NGR,NGS,N)
            XS = GXS(NGR,NGS,N)
            YR = GYR(NGR,NGS,N)
            YS = GYS(NGR,NGS,N)
            PYJAC = PSR*YS - PSS*YR
            PXJAC = PSR*XS - PSS*XR
            PSX = PYJAC / XJAC
            PSY = -PXJAC / XJAC
            SUMDPSI = DSQRT(PSX**2 + PSY**2)
C----------------------------------------- CALCULATE RIGHT HAND SIDE ---
            CALL CALCRJPHI(X,PS,SUMDPSI,A,ARHS,DET)
            
            ARHS = A * ARHS / (1.0+EPS*X)
c            WRITE(20,*)ARHS,PS,X
C------------------------------------- 4 NODES PER ELEMENT ------------
            DO 180 I=1,4
C------------------------------------- 4 FUNCTIONS V PER NODE ---------
              DO 190 J=1,4
                NROW = 4*(INDEX(NODENO(N,I)) - 1) + J
                SUMQ =  ARHS * H(J,I,NGR,NGS) * XJAC
                VX =  YS*HR(J,I,NGR,NGS)  - YR*HS(J,I,NGR,NGS)
                VY = -XS*HR(J,I,NGR,NGS)  + XR*HS(J,I,NGR,NGS)
c                IF (ITH.EQ.1) THEN
c                   DETTERM = - DET/(1+EPS*X)*(PSIX*VX+PSIY*VY)
c                   SUMQ = SUMQ + DETTERM
c                ENDIF
                QQ(NROW) = QQ(NROW) + WRS * SUMQ
                IF (ITH.GE.1) THEN
                DO 200 K=1,4
C------------------------------------- 4 FUNCTIONS H IN PSI -----------
                  DO 210 L=1,4
                    NCOL = 4*(INDEX(NODENO(N,K)) - 1) + L
                    NOFF = NROW - NCOL
                    IF (NOFF.GE.0) THEN
                      PSIX =  YS*HR(L,K,NGR,NGS)  - YR*HS(L,K,NGR,NGS)
                      PSIY = -XS*HR(L,K,NGR,NGS)  + XR*HS(L,K,NGR,NGS)
                      SUMK = - 1./(1.+EPS*X) * (PSIX*VX+PSIY*VY) / XJAC
                      KKBIG(NOFF+1,NCOL) = KKBIG(NOFF+1,NCOL)
     >                                   + WRS * SUMK * (1.D0-DET)
                    ENDIF                     
  210             CONTINUE
  200           CONTINUE
                ENDIF
  190         CONTINUE
  180       CONTINUE
  170     CONTINUE
  160   CONTINUE
  150 CONTINUE
      IF (ITH.GE.1) THEN
         NEND = NP - 1
         IF (IAS.EQ.0) NEND=NP
         DO J=1,NEND
            KKBIG(1,4*J-3) = 1.D20
            KKBIG(1,4*J-1) = 1.D20
         ENDDO
      ENDIF
      ENDIF
      RETURN
      END
************************************************************************
*DECK INITHE
      SUBROUTINE INITHE
C-----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     TO INITIALIZE ARRAY DTH, THINT USED BY FUNCTION DTHPSI, THES
C-----------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION PSINODE(NPTSMAX), PSIDEV(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION DV1(NPTSMAX),DV2(NPTSMAX),DV3(NPTSMAX)
     >                ,DV4(NPTSMAX) 
      DOUBLE PRECISION ALG(3)
      NPT = 1001
      IF (ITH .EQ.1) THEN
         DO 10 I = 1, NPT
            PSI = REAL(I-1)/REAL(NPT-1)
            
            THPSI = 1.0 + ATH*PSI + BTH*PSI**2 + CTH*PSI**3
     >           + DTH*PSI**4 + ETH*PSI**5  + FTH*PSI**6 
     >           + GTH*PSI**7 + HTH*PSI**8
            DTHDPSI = ATH + 2*BTH*PSI + 3*CTH*PSI**2
     >           + 4*DTH*PSI**3 + 5*ETH*PSI**4 + 6*FTH*PSI**5
     >           + 7*GTH*PSI**6 + 8*HTH*PSI**7
            THINT(I) = THPSI
            DTHE(I) = DTHDPSI
 10      CONTINUE
      ELSEIF (ITH.EQ.4) THEN
         DO I = 1,NPT
            PSI = DFLOAT(I-1)/DFLOAT(NPT-1)
            THPSI = (1 - PSI)**ATH
            DTHDPSI = -ATH * (1.-PSI) ** (ATH-1.)
            DTHE(I) = DTHDPSI
            THINT(I) = THPSI
         ENDDO
      ELSEIF ((ITH.EQ.2).OR.(ITH.EQ.3)) THEN
         IF (ITH.EQ.3) THEN
            DO I = 1,NPTS
               PSINODE(I) = (DFLOAT(I-1)/REAL(NPTS-1))
            ENDDO
            CALL SPLINE(NPTS,PSINODE,VTH,0.D0,0.D0,2,DD1,DD2,DD3,DD4)
            DO I = 1, NPTS
               PSI = DSQRT(DFLOAT(I-1)/DFLOAT(NPTS-1))
               VTH(I) = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
            ENDDO
         ENDIF
         CALL ININUM(NPTS, NPT, VTH, THINT, DTHE)
      ELSE
         DO I = 1, NPT
            THINT(I) = 0.D0
            DTHE(I) = 0.D0
         ENDDO
      ENDIF
      END
****************************************************************************
*DECK DTHDPSI
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES DOMG/DPSI AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DTHDPSI(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         DTHDPSI = DTHE(NPT)
      ELSE
         DTHDPSI = DTHE(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (DTHE(NINT+1)-DTHE(NINT))
      ENDIF
      RETURN
      END
****************************************************************************
*DECK THES
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES OMG/DPSI AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION THES(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN 
         THES = THINT(NPT)
      ELSE
         THES = THINT(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >                         (THINT(NINT+1)-THINT(NINT))
      ENDIF
      RETURN
      END
************************************************************************
*DECK INIOMG
      SUBROUTINE INIOMG
C-----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     TO INITIALIZE ARRAY DOM, OMINT USED BY FUNCTION DOMDPSI, OMGS
C-----------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION PSINODE(NPTSMAX), PSIDEV(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION DV1(NPTSMAX),DV2(NPTSMAX),DV3(NPTSMAX)
     >                ,DV4(NPTSMAX) 
      DOUBLE PRECISION ALG(3)
      NPT = 1001
      IF (IOMG.EQ.1) THEN
         DO 10 I = 1, NPT
            PSI = REAL(I-1)/REAL(NPT-1)
            OMPSI = 1.0 + AOM*PSI + BOM*PSI**2 + COM*PSI**3
     >           + DOM*PSI**4 + EOM*PSI**5  + FOM*PSI**6 
     >           + GOM*PSI**7 + HOM*PSI**8
            DOMDPSI = AOM + 2*BOM*PSI + 3*COM*PSI**2
     >           + 4*DOM*PSI**3 + 5*EOM*PSI**4 + 6*FOM*PSI**5
     >           + 7*GOM*PSI**6 + 8*HOM*PSI**7
            OMINT(I) = OMPSI
            DOMG(I) = DOMDPSI
 10      CONTINUE
      ELSEIF (IOMG.EQ.4) THEN
         DO I = 1,NPT
            PSI = DFLOAT(I-1)/DFLOAT(NPT-1)
            OMPSI = (1 - PSI)**AOM
            DOMDPSI = -AOM * (1.-PSI) ** (AOM-1.)
            DOMG(I) = DOMDPSI
            OMINT(I) = OMPSI
         ENDDO
      ELSEIF ((IOMG.EQ.2).OR.(IOMG.EQ.3)) THEN
         IF (IOMG.EQ.3) THEN
            DO I = 1,NPTS
               PSINODE(I) = (DFLOAT(I-1)/REAL(NPTS-1))
            ENDDO
            CALL SPLINE(NPTS,PSINODE,VOM2,0.D0,0.D0,2,DD1,DD2,DD3,DD4)
            DO I = 1, NPTS
               PSI = DSQRT(DFLOAT(I-1)/DFLOAT(NPTS-1))
               VOM2(I) = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
            ENDDO
         ENDIF
         CALL ININUM(NPTS, NPT, VOM2, OMINT, DOMG)
      ELSE
         DO I = 1, NPT
            OMINT(I) = 0.D0
            DOMG(I) = 0.D0
         ENDDO
      ENDIF
      RETURN
      END
****************************************************************************
*DECK DOMDPSI
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES DOMG/DPSI AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DOMDPSI(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         DOMDPSI = DOMG(NPT)
      ELSE
         DOMDPSI = DOMG(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (DOMG(NINT+1)-DOMG(NINT))
      ENDIF
      RETURN
      END
****************************************************************************
*DECK OMGS
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES OMG/DPSI AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION OMGS(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         OMGS = OMINT(NPT)
      ELSE
         OMGS = OMINT(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (OMINT(NINT+1)-OMINT(NINT))
      ENDIF
      RETURN
      END
************************************************************************
*DECK INITE
      SUBROUTINE INITE
C-----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     TO INITIALIZE ARRAY DTE, TEINT USED BY FUNCTION DTEDPSI, TEMS
C-----------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION PSINODE(NPTSMAX),PSIDEV(NPTSMAX),VNEW(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION ALG(3)
      NPT = 1001
      IF (ITE.EQ.1) THEN
         DO 10 I = 1, NPT
            PSI = REAL(I-1)/REAL(NPT-1)
            
            TEPSI = 1.0 + ATE*PSI + BTE*PSI**2 + CTE*PSI**3
     >           + DTE*PSI**4 + ETE*PSI**5  + FTE*PSI**6 
     >           + GTE*PSI**7 + HTE*PSI**8
            DTEDPSI = ATE + 2*BTE*PSI + 3*CTE*PSI**2
     >           + 4*DTE*PSI**3 + 5*ETE*PSI**4 + 6*FTE*PSI**5
     >           + 7*GTE*PSI**6 + 8*HTE*PSI**7
            TEINT(I) = TEPSI
            DTEM(I) = DTEDPSI
 10      CONTINUE
      ELSEIF (ITE.EQ.5) THEN
         TEPED = 0.
         DPED = 0.
         DO 110 I = NPT, 1, -1
            PSI = REAL(I-1)/REAL(NPT-1)
            DPSI = 1. / REAL(NPT-1)
            
            TEPSI = 1.0 + ATE*PSI + BTE*PSI**2 + CTE*PSI**3
     >           + DTE*PSI**4 
            DTEDPSI = ATE + 2*BTE*PSI + 3*CTE*PSI**2
     >           + 4*DTE*PSI**3
            IF ((PSI.GT.ETE) .AND. (PSI.LT.HTE)) THEN
               DPED =  FTE*(
     >              (PSI-ETE)**2 * (3.-2*PSI-ETE)/(1.-ETE)**3 )**GTE
               DTEDPSI = DTEDPSI + DPED
               TEPED = TEPED + DPED * DPSI
               TEPSI = TEPSI + TEPED
            ELSEIF (PSI.GT.HTE) THEN
               DPED = 0.
            ELSE
               TEPSI = TEPSI + TEPED
            ENDIF
            TEINT(I) = TEPSI
            DTEM(I) = DTEDPSI
 110     CONTINUE
      ELSEIF (ITE.EQ.4) THEN
         DO I = 1,NPT
            PSI = DFLOAT(I-1)/DFLOAT(NPT-1)
            TEPSI = (1 - PSI)**ATE * BTE + (1 - BTE)
            DTEDPSI = -ATE * (1.-PSI) ** (ATE-1.) * BTE
            DTEM(I) = DTEDPSI
            TEINT(I) = TEPSI
         ENDDO
      ELSEIF ((ITE.EQ.2).OR.(ITE.EQ.3)) THEN
         IF (ITE.EQ.3) THEN
            DO I = 1,NPTS
               PSINODE(I) = (DFLOAT(I-1)/REAL(NPTS-1))
            ENDDO
            CALL SPLINE(NPTS,PSINODE,VTE,0.D0,0.D0,2,DD1,DD2,DD3,DD4)
            DO I = 1, NPTS
               PSI = DSQRT(DFLOAT(I-1)/DFLOAT(NPTS-1))
               VTE(I) = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
            ENDDO
         ENDIF
         CALL ININUM(NPTS, NPT, VTE,TEINT,DTEM)
      ENDIF
      END
****************************************************************************
*DECK DTEDPSI
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES DTEMP/DPSI AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DTEDPSI(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         DTEDPSI = DTEM(NPT)
      ELSE
         DTEDPSI = DTEM(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (DTEM(NINT+1)-DTEM(NINT))
      ENDIF
      RETURN
      END
****************************************************************************
*DECK TEMS
C----------------------------------------------------------------
C     THIS IS A NEW SUBROUTINE IMPLEMENTED TO INCLUDE TORODIAL FLOW
C     GIVES TEMP AS A FUNCTION OF PSI
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION TEMS(PSI)
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         TEMS = TEINT(NPT)
      ELSE
         TEMS = TEINT(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (TEINT(NINT+1)-TEINT(NINT))
      ENDIF
      RETURN
      END
************************************************************************
*DECK INIPRES
      SUBROUTINE INIPRES
C------------------------------------------------------------------
C  INITIALIZING THE ARRAY DPRES(1001) USED BY FUNCTION DPDPSI
C@    ,   ,               PPRES(1001)     ,     ,      PPSI
C------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION PSINODE(NPTSMAX),PSIDEV(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION ALG(3)
      NPT=1001
      IF (IPAI.EQ.1) THEN
         DO 10 I=1,NPT 
            PSI = REAL(I-1)/REAL(NPT-1)

            PPSI = 1.0D0 + API*PSI + BPI*PSI**2 + CPI*PSI**3
     >           + DPI*PSI**4 + EPI*PSI**5  + FPI*PSI**6 
     >           + GPI*PSI**7 + HPI*PSI**8
            DPDPSI = API + 2*BPI*PSI + 3*CPI*PSI**2
     >           + 4*DPI*PSI**3 + 5*EPI*PSI**4 + 6*FPI*PSI**5
     >           + 7*GPI*PSI**6 + 8*HPI*PSI**7
            DPRES(I) = DPDPSI
            PINT(I) = PPSI
 10      CONTINUE
      ELSEIF (IPAI.EQ.4) THEN
         DO I = 1,NPT
            PSI = DFLOAT(I-1)/DFLOAT(NPT-1)
            PPSI = (1 - PSI)**API * BPI + (1-BPI)
            DPDPSI = -API * (1.-PSI) ** (API-1.) * BPI
            DPRES(I) = DPDPSI
            PINT(I) = PPSI
         ENDDO
      ELSEIF ((IPAI.EQ.2).OR.(IPAI.EQ.3)) THEN
         IF (IPAI.EQ.3) THEN
            DO I = 1,NPTS
               PSINODE(I) = (DFLOAT(I-1)/REAL(NPTS-1))
            ENDDO
            CALL SPLINE(NPTS,PSINODE,VH,0.D0,0.D0,2,DD1,DD2,DD3,DD4)
            DO I = 1, NPTS
               PSI = DSQRT(DFLOAT(I-1)/DFLOAT(NPTS-1))
               VH(I) = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
            ENDDO
         ENDIF
          CALL ININUM(NPTS,NPT,VH,PINT,DPRES)
       ENDIF
C      PINT(NPT) = 0.
C      DO I=1,NPT-1
C        SUM = SUM + (DPRES(NPT-I+1)+DPRES(NPT-I))*DPS/2.
C        PINT(NPT-I) = SUM
C      ENDDO
      RETURN
      END
      
      SUBROUTINE CUBFCT(S,SL,SU,H1,H2)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION   H(4)
        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H1= 3.*Q1**2 - 2.*Q1**3
        H2= 3.*Q2**2 - 2.*Q2**3
c        H(3)= (S-SU)*Q1**2
c        H(4)= (S-SL)*Q2**2
      RETURN
      END
************************************************************************
*DECK ININUM
      SUBROUTINE ININUM(NPTS, NPT, VDATA, VINT, VDEV)
C------------------------------------------------------------------
C INTERPOLATE ON DISCRETE DATA FOR PROFILE INITIALIZING
C
C INPUT  :
C     NTP   NUMBER OF NODES ON OUTPUT
C     NTPS  NUMBER OF NODES ON INPUT
C     VDATA DATA
C OUTPUT :
C     VINT  INTERPOLATED DATA
C     VDEV  INTERPOLATED DERIVTIVE
C------------------------------------------------------------------
      USE PARAM
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      INTEGER NPTS, NPT
      DOUBLE PRECISION VDATA(*), VINT(*), VDEV(*)
      DOUBLE PRECISION PSINODE(NPTSMAX),PSIDEV(NPTSMAX)
      DOUBLE PRECISION VNEW(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION DV1(NPTSMAX),DV2(NPTSMAX),DV3(NPTSMAX)
     >                ,DV4(NPTSMAX) 
      DOUBLE PRECISION ALG(3)
      DH = 1.D0 / DFLOAT(NPTS-1)
C FIVE POINT DERIVTIVE
      DO I = 1,NPTS
         PSINODE(I) = DFLOAT(I-1)/REAL(NPTS-1)
         IF ((I.LE.NPTS-4)) THEN
            PSIDEV(I) = (-25*VDATA(I)+48*VDATA(I+1)
     >           -36*VDATA(I+2)+16*VDATA(I+3)-3*VDATA(I+4))/(12*DH)
         ELSE 
            PSIDEV(I) = -(-25*VDATA(I)+48*VDATA(I-1)
     >           -36*VDATA(I-2)+16*VDATA(I-3)-3*VDATA(I-4))/(12*DH) 
         ENDIF
      ENDDO
      CALL SPLINE(NPTS,PSINODE,PSIDEV,0.D0,0.D0,2,DV1,DV2,DV3,DV4)
      CALL SPLINE(NPTS,PSINODE,VDATA,PSIDEV(1),PSIDEV(NPTS),1
     >     ,DD1,DD2,DD3,DD4)
      DO I = 1, NPT
         PSI = REAL(I-1)/REAL(NPT-1)
         DATAPSI = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
c$$$         DEV1 = SPWERT(NPTS,PSI,DV1,DV2,DV3,DV4,PSINODE,ALG)
         VINT(I) = DATAPSI
         VDEV(I) = ALG(1)
      ENDDO
      RETURN
      END
************************************************************************
*DECK DPDPSI
      FUNCTION DPDPSI(PSI)
C------------------------------------------------------------------
C THE NORMALIZED PROFILE OF THE PRESSURE GRADIENT VERSUS FLUX
C THIS ROUTINE MUST BE INITIALIZED BY A CALL TO INIPRES
C------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) THEN
         DPDPSI = DPRES(NPT)
      ELSE
         DPDPSI = DPRES(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >        (DPRES(NINT+1)-DPRES(NINT))
      ENDIF
      RETURN
      END
************************************************************************
*DECK INIGAM
      SUBROUTINE INIGAM
C---------------------------------------------------------------------
C SUBROUTINE TO INITIALIZE THE ARRAY DGAM USED IN THE FUNCTION DGDPSI
C------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION PSINODE(NPTSMAX),PSIDEV(NPTSMAX)
      DOUBLE PRECISION VNEW(NPTSMAX)
      DOUBLE PRECISION DD1(NPTSMAX),DD2(NPTSMAX),DD3(NPTSMAX)
     >                ,DD4(NPTSMAX)
      DOUBLE PRECISION DV1(NPTSMAX),DV2(NPTSMAX),DV3(NPTSMAX)
     >                ,DV4(NPTSMAX) 
      DOUBLE PRECISION ALG(3)
      NPT=1001
      IF (IGAM.EQ.1) THEN
         DO 10 I=1,NPT
            PSI = REAL(I-1)/REAL(NPT-1)
            GAMPSI   = 1.0 + AGA*PSI + BGA*PSI**2 + CGA*PSI**3 
     >           + DGA*PSI**4
     >           + EGA*PSI**5 + FGA*PSI**6 + GGA*PSI**7
     >           + HGA*PSI**8
            DGDPSI   = AGA + 2*BGA*PSI + 3*CGA*PSI**2
     >           + 4*DGA*PSI**3 + 5*EGA*PSI**4 + 6*FGA*PSI**5
     >           + 7*GGA*PSI**6 + 8*HGA*PSI**7
            GINT(I) = DSQRT(GAMPSI)
            DGAM(I) = 0.5D0 * DGDPSI / GINT(I)
            IF (GINT(I).EQ.0.D0) DGAM(I) = 0.D0
 10      CONTINUE
      ELSEIF (IGAM.EQ.4) THEN
         DO  I=1,NPT
            PSI = REAL(I-1)/REAL(NPT-1)
            GAMPSI   = (1. - PSI) ** AGA * BGA + 1. - BGA
            DGDPSI   = -AGA * (1.-PSI) ** (AGA-1.) * BGA
            GINT(I) = DSQRT(GAMPSI)
            DGAM(I) = 0.5D0 * DGDPSI / GINT(I)
            IF (GINT(I).EQ.0.D0) DGAM(I) = 0.D0
         ENDDO
      ELSEIF ((IGAM.EQ.2).OR.(IGAM.EQ.3)) THEN
         IF (IGAM.EQ.3) THEN
            DO I = 1,NPTS
               PSINODE(I) = (DFLOAT(I-1)/REAL(NPTS-1))
            ENDDO
            CALL SPLINE(NPTS,PSINODE,VF2,0.D0,0.D0,2,DD1,DD2,DD3,DD4)
            DO I = 1, NPTS
               PSI = DSQRT(DFLOAT(I-1)/DFLOAT(NPTS-1))
               VF2(I) = SPWERT(NPTS,PSI,DD1,DD2,DD3,DD4,PSINODE,ALG)
            ENDDO
         ENDIF
         CALL ININUM(NPTS, NPT, VF2, GINT, DGAM)
         DO I = 1, NPT
            GINT(I) = DSQRT(GINT(I))
            DGAM(I) = 0.5D0 * DGAM(I)/ GINT(I)
           IF (GINT(I).EQ.0.D0) DGAM(I) = 0.D0
         ENDDO
      ENDIF
      RETURN
      END
************************************************************************
*DECK DGDPSI
      FUNCTION DGDPSI(PSI)
C------------------------------------------------------------------
C THE NORMALIZED PROFILE OF GRADIENT OF GAMMA VERSUS FLUX
C------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NPT=1001
      DPS = 1./REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      ZPSI = PSI
      IF (PSI.GE.1.) THEN
         DGDPSI = DGAM(NPT)
      ELSE
         DGDPSI = DGAM(NINT) + ((ZPSI)-DPS*(NINT-1))/DPS *
     >        (DGAM(NINT+1)-DGAM(NINT))
      ENDIF
      RETURN
      END
************************************************************************

************************************************************************
*DECK QPRFL
      FUNCTION QPRFL(PSI)
C------------------------------------------------------------------
C THE NORMALIZED PROFILE OF THE CURRENT DENSITY VERSUS FLUX
C------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DPS = 1./REAL(NPTS-1)
      NINT = INT((NPTS-1)*DSQRT(PSI)) + 1
      IF (PSI.GE.1.) NINT=NPTS-1
      QPRFL = QIN(NINT) + (DSQRT(PSI)-DPS*(NINT-1))/DPS *
     >                         (QIN(NINT+1)-QIN(NINT))
      RETURN
      END
      
      
************************************************************************
*DECK INTERP
      SUBROUTINE INTERP(XN1,XN2,XN3,XN4,R,S,X,XR,XS,XRS,XRR,XSS)
C----------------------------------------------------------------------
C SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
C BY XI(1..4) AT THE FOUR NODES UDSING BI-CUBIC HERMITE ELEMENTS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XN1(4),XN2(4),XN3(4),XN4(4)

      HI0M   = - (R-1.)**2 * (-R-2.) * 0.25
      HRI0M  = - (R-1.)*(-R-2.)*0.5 +(R-1.)**2 * 0.25
      HRRI0M = + 1.5 * R 
      HI1M   = - (R-1.)**2 * (-R-1.) * 0.25
      HRI1M  = - (R-1.)*(-R-1.)*0.5 + (R-1.)**2 *0.25
      HRRI1M = + 1.5 * R - .5

      HJ0M   = - (S-1.)**2 * (-S-2.) * 0.25
      HSJ0M  = - (S-1.)*(-S-2.)*0.5 +(S-1.)**2 * 0.25
      HSSJ0M = + 1.5 * S 
      HJ1M   = - (S-1.)**2 * (-S-1.) * 0.25
      HSJ1M  = - (S-1.)*(-S-1.)*0.5 + (S-1.)**2 * 0.25
      HSSJ1M = + 1.5 * S - .5
    
      HI0P   = - (R+1.)**2 * (R-2.) * 0.25
      HRI0P  = - (R+1.)*(R-2.)*0.5 - (R+1.)**2 * 0.25
      HRRI0P = - 1.5 * R 
      HI1P   = + (R+1.)**2 * (R-1.) * 0.25
      HRI1P  = + (R+1.)*(R-1.)*0.5 + (R+1.)**2 * 0.25
      HRRI1P = + 1.5 * R + .5

      HJ0P  = - (S+1.)**2 * (S-2.) * 0.25
      HSJ0P = - (S+1.)*(S-2.)*0.5 - (S+1.)**2 * 0.25
      HSSJ0P = - 1.5 * S 
      HJ1P  = + (S+1.)**2 * (S-1.) * 0.25
      HSJ1P = + (S+1.)*(S-1.)*0.5 + (S+1.)**2 * 0.25
      HSSJ1P = + 1.5 * S + .5
        
      X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2)
     >  + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) 
     >  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2)
     >  + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) 
     >  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2)
     >  + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) 
     >  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2)
     >  + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)
               
      XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2)
     >   + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4)  
     >   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2)
     >   + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) 
     >   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2)
     >   + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) 
     >   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2)
     >   + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)
       
      XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2)
     >   + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) 
     >   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2)
     >   + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) 
     >   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2)
     >   + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) 
     >   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2)
     >   + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)
     
      XRR = HRRI0M*HJ0M * XN1(1) + HRRI1M*HJ0M * XN1(2)
     >    + HRRI0M*HJ1M * XN1(3) + HRRI1M*HJ1M * XN1(4)  
     >    + HRRI0M*HJ0P * XN2(1) + HRRI1M*HJ0P * XN2(2)
     >    + HRRI0M*HJ1P * XN2(3) + HRRI1M*HJ1P * XN2(4) 
     >    + HRRI0P*HJ0M * XN4(1) + HRRI1P*HJ0M * XN4(2)
     >    + HRRI0P*HJ1M * XN4(3) + HRRI1P*HJ1M * XN4(4) 
     >    + HRRI0P*HJ0P * XN3(1) + HRRI1P*HJ0P * XN3(2)
     >    + HRRI0P*HJ1P * XN3(3) + HRRI1P*HJ1P * XN3(4)
       
      XSS = HI0M*HSSJ0M * XN1(1) + HI1M*HSSJ0M * XN1(2)
     >    + HI0M*HSSJ1M * XN1(3) + HI1M*HSSJ1M * XN1(4) 
     >    + HI0M*HSSJ0P * XN2(1) + HI1M*HSSJ0P * XN2(2)
     >    + HI0M*HSSJ1P * XN2(3) + HI1M*HSSJ1P * XN2(4) 
     >    + HI0P*HSSJ0M * XN4(1) + HI1P*HSSJ0M * XN4(2)
     >    + HI0P*HSSJ1M * XN4(3) + HI1P*HSSJ1M * XN4(4) 
     >    + HI0P*HSSJ0P * XN3(1) + HI1P*HSSJ0P * XN3(2)
     >    + HI0P*HSSJ1P * XN3(3) + HI1P*HSSJ1P * XN3(4) 
       
      XRS = HRI0M*HSJ0M * XN1(1) + HRI1M*HSJ0M * XN1(2)
     >    + HRI0M*HSJ1M * XN1(3) + HRI1M*HSJ1M * XN1(4) 
     >    + HRI0M*HSJ0P * XN2(1) + HRI1M*HSJ0P * XN2(2)
     >    + HRI0M*HSJ1P * XN2(3) + HRI1M*HSJ1P * XN2(4) 
     >    + HRI0P*HSJ0M * XN4(1) + HRI1P*HSJ0M * XN4(2)
     >    + HRI0P*HSJ1M * XN4(3) + HRI1P*HSJ1M * XN4(4) 
     >    + HRI0P*HSJ0P * XN3(1) + HRI1P*HSJ0P * XN3(2)
     >    + HRI0P*HSJ1P * XN3(3) + HRI1P*HSJ1P * XN3(4)
          
      RETURN
      END


************************************************************************
*DECK INTERP1
      SUBROUTINE INTERP1(XN1,XN2,XN3,XN4,R,S,X)
C----------------------------------------------------------------------
C SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
C BY XI(1..4) AT THE FOUR NODES UDSING BI-CUBIC HERMITE ELEMENTS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XN1(4),XN2(4),XN3(4),XN4(4)

      HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
      HI1M  = - (R-1.)**2 * (-R-1.) * 0.25

      HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
      HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25
    
      HI0P  = - (R+1.)**2 * (R-2.)  * 0.25
      HI1P  = + (R+1.)**2 * (R-1.)  * 0.25

      HJ0P  = - (S+1.)**2 * (S-2.)  * 0.25
      HJ1P  = + (S+1.)**2 * (S-1.)  * 0.25
        
      X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2)
     >  + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) 
     >  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2)
     >  + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) 
     >  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2)
     >  + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) 
     >  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2)
     >  + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)
               
      RETURN
      END

************************************************************************
*DECK INTERP2
      SUBROUTINE INTERP2(XN1,XN2,XN3,XN4,R,S,X,XR,XS)
C----------------------------------------------------------------------
C SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
C BY XI(1..4) AT THE FOUR NODES UDSING BI-CUBIC HERMITE ELEMENTS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XN1(4),XN2(4),XN3(4),XN4(4)

      HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
      HRI0M = - (R-1.)*(-R-2.)*0.5 +(R-1.)**2 * 0.25
      HI1M  = - (R-1.)**2 * (-R-1.) * 0.25
      HRI1M = - (R-1.)*(-R-1.)*0.5 + (R-1.)**2 * 0.25
      HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
      HSJ0M = - (S-1.)*(-S-2.)*0.5 +(S-1.)**2  * 0.25
      HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25
      HSJ1M = - (S-1.)*(-S-1.)*0.5 + (S-1.)**2 * 0.25
    
      HI0P  = - (R+1.)**2 * (R-2.) * 0.25
      HRI0P = - (R+1.)*(R-2.)*0.5 - (R+1.)**2 * 0.25
      HI1P  = + (R+1.)**2 * (R-1.) * 0.25
      HRI1P = + (R+1.)*(R-1.)*0.5 + (R+1.)**2 * 0.25

      HJ0P  = - (S+1.)**2 * (S-2.) * 0.25
      HSJ0P = - (S+1.)*(S-2.)*0.5 - (S+1.)**2 * 0.25
      HJ1P  = + (S+1.)**2 * (S-1.) * 0.25
      HSJ1P = + (S+1.)*(S-1.)*0.5 + (S+1.)**2 * 0.25
        
      X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2)
     >  + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) 
     >  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2)
     >  + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) 
     >  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2)
     >  + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) 
     >  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2)
     >  + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)
               
      XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2)
     >   + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4)  
     >   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2)
     >   + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) 
     >   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2)
     >   + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) 
     >   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2)
     >   + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)
       
      XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2)
     >   + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) 
     >   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2)
     >   + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) 
     >   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2)
     >   + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) 
     >   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2)
     >   + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)
       
      RETURN
      END

      SUBROUTINE INTERP3(XN1,XN2,XN3,XN4,
     >                   YN1,YN2,YN3,YN4,
     >                   PN1,PN2,PN3,PN4,R,S,
     >                   X,XR,XS,YR,YS,PS)
C----------------------------------------------------------------------
C SUBROUTINE CALCULATES THE INTERPOLATED VALUE OF THE FUNCTION X GIVEN
C BY XI(1..4) AT THE FOUR NODES UDSING BI-CUBIC HERMITE ELEMENTS
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XN1(4),XN2(4),XN3(4),XN4(4)
      DOUBLE PRECISION  YN1(4),YN2(4),YN3(4),YN4(4)
      DOUBLE PRECISION  PN1(4),PN2(4),PN3(4),PN4(4)

      HI0M  = - (R-1.)**2 * (-R-2.) * 0.25
      HRI0M = - (R-1.)*(-R-2.)*0.5 +(R-1.)**2 * 0.25
      HI1M  = - (R-1.)**2 * (-R-1.) * 0.25
      HRI1M = - (R-1.)*(-R-1.)*0.5 + (R-1.)**2 * 0.25
      HJ0M  = - (S-1.)**2 * (-S-2.) * 0.25
      HSJ0M = - (S-1.)*(-S-2.)*0.5 +(S-1.)**2  * 0.25
      HJ1M  = - (S-1.)**2 * (-S-1.) * 0.25
      HSJ1M = - (S-1.)*(-S-1.)*0.5 + (S-1.)**2 * 0.25
    
      HI0P  = - (R+1.)**2 * (R-2.) * 0.25
      HRI0P = - (R+1.)*(R-2.)*0.5 - (R+1.)**2 * 0.25
      HI1P  = + (R+1.)**2 * (R-1.) * 0.25
      HRI1P = + (R+1.)*(R-1.)*0.5 + (R+1.)**2 * 0.25

      HJ0P  = - (S+1.)**2 * (S-2.) * 0.25
      HSJ0P = - (S+1.)*(S-2.)*0.5 - (S+1.)**2 * 0.25
      HJ1P  = + (S+1.)**2 * (S-1.) * 0.25
      HSJ1P = + (S+1.)*(S-1.)*0.5 + (S+1.)**2 * 0.25
        
      X = HI0M*HJ0M * XN1(1) + HI1M*HJ0M * XN1(2)
     >  + HI0M*HJ1M * XN1(3) + HI1M*HJ1M * XN1(4) 
     >  + HI0M*HJ0P * XN2(1) + HI1M*HJ0P * XN2(2)
     >  + HI0M*HJ1P * XN2(3) + HI1M*HJ1P * XN2(4) 
     >  + HI0P*HJ0M * XN4(1) + HI1P*HJ0M * XN4(2)
     >  + HI0P*HJ1M * XN4(3) + HI1P*HJ1M * XN4(4) 
     >  + HI0P*HJ0P * XN3(1) + HI1P*HJ0P * XN3(2)
     >  + HI0P*HJ1P * XN3(3) + HI1P*HJ1P * XN3(4)
               
      XR = HRI0M*HJ0M * XN1(1) + HRI1M*HJ0M * XN1(2)
     >   + HRI0M*HJ1M * XN1(3) + HRI1M*HJ1M * XN1(4)  
     >   + HRI0M*HJ0P * XN2(1) + HRI1M*HJ0P * XN2(2)
     >   + HRI0M*HJ1P * XN2(3) + HRI1M*HJ1P * XN2(4) 
     >   + HRI0P*HJ0M * XN4(1) + HRI1P*HJ0M * XN4(2)
     >   + HRI0P*HJ1M * XN4(3) + HRI1P*HJ1M * XN4(4) 
     >   + HRI0P*HJ0P * XN3(1) + HRI1P*HJ0P * XN3(2)
     >   + HRI0P*HJ1P * XN3(3) + HRI1P*HJ1P * XN3(4)
       
      XS = HI0M*HSJ0M * XN1(1) + HI1M*HSJ0M * XN1(2)
     >   + HI0M*HSJ1M * XN1(3) + HI1M*HSJ1M * XN1(4) 
     >   + HI0M*HSJ0P * XN2(1) + HI1M*HSJ0P * XN2(2)
     >   + HI0M*HSJ1P * XN2(3) + HI1M*HSJ1P * XN2(4) 
     >   + HI0P*HSJ0M * XN4(1) + HI1P*HSJ0M * XN4(2)
     >   + HI0P*HSJ1M * XN4(3) + HI1P*HSJ1M * XN4(4) 
     >   + HI0P*HSJ0P * XN3(1) + HI1P*HSJ0P * XN3(2)
     >   + HI0P*HSJ1P * XN3(3) + HI1P*HSJ1P * XN3(4)

      PS= HI0M*HJ0M * PN1(1) + HI1M*HJ0M * PN1(2)
     >  + HI0M*HJ1M * PN1(3) + HI1M*HJ1M * PN1(4) 
     >  + HI0M*HJ0P * PN2(1) + HI1M*HJ0P * PN2(2)
     >  + HI0M*HJ1P * PN2(3) + HI1M*HJ1P * PN2(4) 
     >  + HI0P*HJ0M * PN4(1) + HI1P*HJ0M * PN4(2)
     >  + HI0P*HJ1M * PN4(3) + HI1P*HJ1M * PN4(4) 
     >  + HI0P*HJ0P * PN3(1) + HI1P*HJ0P * PN3(2)
     >  + HI0P*HJ1P * PN3(3) + HI1P*HJ1P * PN3(4)
                                   
      YR = HRI0M*HJ0M * YN1(1) + HRI1M*HJ0M * YN1(2)
     >   + HRI0M*HJ1M * YN1(3) + HRI1M*HJ1M * YN1(4)  
     >   + HRI0M*HJ0P * YN2(1) + HRI1M*HJ0P * YN2(2)
     >   + HRI0M*HJ1P * YN2(3) + HRI1M*HJ1P * YN2(4) 
     >   + HRI0P*HJ0M * YN4(1) + HRI1P*HJ0M * YN4(2)
     >   + HRI0P*HJ1M * YN4(3) + HRI1P*HJ1M * YN4(4) 
     >   + HRI0P*HJ0P * YN3(1) + HRI1P*HJ0P * YN3(2)
     >   + HRI0P*HJ1P * YN3(3) + HRI1P*HJ1P * YN3(4)
       
      YS = HI0M*HSJ0M * YN1(1) + HI1M*HSJ0M * YN1(2)
     >   + HI0M*HSJ1M * YN1(3) + HI1M*HSJ1M * YN1(4) 
     >   + HI0M*HSJ0P * YN2(1) + HI1M*HSJ0P * YN2(2)
     >   + HI0M*HSJ1P * YN2(3) + HI1M*HSJ1P * YN2(4) 
     >   + HI0P*HSJ0M * YN4(1) + HI1P*HSJ0M * YN4(2)
     >   + HI0P*HSJ1M * YN4(3) + HI1P*HSJ1M * YN4(4) 
     >   + HI0P*HSJ0P * YN3(1) + HI1P*HSJ0P * YN3(2)
     >   + HI0P*HSJ1P * YN3(3) + HI1P*HSJ1P * YN3(4)
       
      RETURN
      END


************************************************************************
*DECK SOLVE2
      SUBROUTINE SOLVE2(QQ,NR,NP,PSI,ITER,IAS,ITH)
C-----------------------------------------------------------------------
C SUBROUTINE TO SOLVE THE SYSTEM OF EQUATIONS UDSING GAUSSIAN ELIMINATION
C-----------------------------------------------------------------------
      USE PARAM
      USE COMSOLV
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION     QQ(*),PSI(*)
      INTEGER INDEX(MAXNODE)
      CHARACTER*1 UPLO
      EXTERNAL DPBTRF
      
      SAVE INDEX

      IF (IAS.EQ.0) THEN
         ND = 4*NR*NP
      ELSE
         ND = 4*NR*(NP-1)
      ENDIF
      NROW = 4*MAXMNODE
      NVAR = 4*NP+7
      
      IF (ITER.EQ.1) THEN
C------------------------------INVERSE OF INDEX IN FORMKQ TO RESTORE
      IF (IAS.EQ.1) THEN
        DO I=1,NR
          J  = 1
          JN = 1
          IJ1 = (I-1)*NP     + J
          IJN = (I-1)*(NP-1) + JN
	  INDEX(IJN) = IJ1
          DO J=2,(NP-1)/2+1
            JN = 2*(J-1)
	    IJ1 = (I-1)*NP     + J
	    IJN = (I-1)*(NP-1) + JN
	    INDEX(IJN) = IJ1
          ENDDO
	  DO J=(NP-1)/2+2,NP-1
	    JN = 2*(NP-J)+1
	    IJ1 = (I-1)*NP     + J
	    IJN = (I-1)*(NP-1) + JN 
	    INDEX(IJN) = IJ1
          ENDDO
        ENDDO
      ENDIF
      ENDIF
      
      IF ((ITH.GE.1).OR.(ITER.EQ.1)) THEN
c---------------------------------------------------- ESSL version
c        CALL DPBF(KKBIG,KKLDA,ND,4*NP+8)
c---------------------------------------------------- lapack version      
        UPLO='L'
        CALL DPBTRF(UPLO,ND,NVAR,KKBIG,KKLDA,INFO)
      ENDIF
      
      DO 220 I=1,4*NR*NP
        PSI(I) = QQ(I) 
  220 CONTINUE
c-------------------------------------------------- ESSL version
c      CALL DPBS(KKBIG,KKLDA,ND,4*NP+8,PSI)
c-------------------------------------------------- lapack version

      CALL DPBTRS('L',ND,NVAR,1,KKBIG,KKLDA,PSI,NROW,INFO)

c-------------- restore to simple clockwise numbering
      IF (IAS.EQ.1) THEN
        DO I=1,4*NR*NP
          QQ(I) = PSI(I)
        ENDDO
        DO I=1,NR*(NP-1)
          DO K=1,4
	    IK = 4*(I-1)+K
	    IKN = 4*(INDEX(I)-1)+K
            PSI(IKN) = QQ(IK)
          ENDDO  
        ENDDO
        DO I=1,NR
          DO K=1,4
	    IK  = 4*(I-1)*NP + K
	    IK2 = 4*(I-1)*NP + 4*(NP-1) + K
            PSI(IK2) = PSI(IK)
          ENDDO
        ENDDO
      ENDIF
c-------------------------------- fill in boundary conditions      
      DO 225 I=1,NR*NP
        PSI(4*(I-1)+1) = PSI(4*(I-1)+1) + 1.
  225 CONTINUE
      DO 100 J=1,NP
        PSI(4*J-3) = 1.
        PSI(4*J-1) = 0.
  100 CONTINUE
      IF (IAS.EQ.1) THEN
        DO 230 I=1,NR
          DO 240 K=1,4
            NBASE = 4*(I-1)*NP + K
            NBASE2 = NBASE + 4*(NP-1)
            PSI(NBASE2) = PSI(NBASE)
  240     CONTINUE
  230   CONTINUE
      ENDIF
CCC      CALL PRARR1('PSI : ',PSI,4*NR*NP,203)

      RETURN
      END


************************************************************************
*DECK FINDAXIS
      SUBROUTINE FINDAXIS(XX,YY,NR,NP,PSAXIS,XAXIS,YAXIS,
     >                    NAX,RAX,SAX,IAS)
C-----------------------------------------------------------------------
C SUBROUTINE TO LOCALIZE THE POSITION OF THE MAGNETIC AXIS ; THE MINIMUM
C OF PSI OF ALL ELEMENTS
C-----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE FAXIS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*),YY(4,*)
      DIMENSION X(2),FVEC(2),FJAC(2,2)

      PSMIN = 1.D20
      IF (IAS.EQ.1) THEN
c----------------------------------- asymmetric part -----------      
      NELM = (NR-1)*(NP-1)
      XERR = 1E-8
      XTOL = 1E-4
      TOLL = 1. + XTOL
      NEQ2 = 2
      LDFJAC = 2
      LWA = 15
      NTRIAL=50
      TOLX = 1.D-8
      TOLF = 1.D-8
      DO I=NELM,NELM/2,-1
        NAXIS = I
        IFAIL = 1
        X(1) = 0.
        X(2) = 0.
        call mnewtax(ntrial,x,neq2,tolx,tolf,xerr,ferr)
        if ((xerr.le.tolx).or.(ferr.le.tolf)) then
          ifail = 0.
        else
C         WRITE(*,*) ' accuracy not reached : ',xerr,ferr
        endif 
        R = X(1)
        S = X(2)
        IF ((IFAIL.EQ.0).AND.
     >      ((DABS(R).LE.TOLL).AND.(DABS(S).LE.TOLL))) THEN
          N1 = NODENO(NAXIS,1)
          N2 = NODENO(NAXIS,2)
          N3 = NODENO(NAXIS,3)
          N4 = NODENO(NAXIS,4)
          CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >                PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >                R,S,ZPSI,ZPSIR,ZPSIS,ZPSIRS,ZPSIRR,ZPSISS)
          IF (ZPSI.LT.PSMIN) THEN
            PSMIN = ZPSI
            NAX = NAXIS
            NNAX = NAXIS
            RAX = R
            SAX = S
          ENDIF
        ENDIF
      ENDDO
      N1 = NODENO(NNAX,1)
      N2 = NODENO(NNAX,2)
      N3 = NODENO(NNAX,3)
      N4 = NODENO(NNAX,4)
      CALL INTERP1(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),RAX,SAX,XAXIS)
      CALL INTERP1(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),RAX,SAX,YAXIS)
      PSAXIS = PSMIN

      ELSE
c**************************************** SYMMETRIC PART ***************
      DO N=1, (NR-1)*(NP-1), NP-1
        N1 = NODENO(N,1)
        N2 = NODENO(N,4)
        IF  (PSI(4*(N1-1)+2)*PSI(4*(N2-1)+2).LE.0.) THEN
C------------------------------------- QUAD. EQ FOR R VALUE AT MINIMUM -
          PSIM  = PSI(4*(N1-1)+1)
          PSIMR = PSI(4*(N1-1)+2)
          PSIP  = PSI(4*(N2-1)+1)
          PSIPR = PSI(4*(N2-1)+2)
          AA =  3. * (PSIM + PSIMR - PSIP + PSIPR ) / 4.
          BB =  ( - PSIMR + PSIPR ) / 2.
          CC =  ( - 3*PSIM - PSIMR + 3*PSIP - PSIPR) / 4.
          DET = BB*BB - 4.*AA*CC
          R  = ROOT(AA,BB,CC,DET,1.D0)
          IF (DABS(R).GT.1.+1.D-8) THEN
            R = ROOT(AA,BB,CC,DET,-1.D0)
          ENDIF
C         IF (DABS(R).LT.1.+1.D-8) THEN
C-------- THE DSIGN OF R CHANGES FOR ELEMENTS ON THE LEFT  (SEE REMESH) -
          CALL CUB1D(XX(1,N1),XX(2,N1),XX(1,N2),XX(2,N2),R,XAXIS,DUMMY)
          CALL CUB1D(PSIM,PSIMR,PSIP,PSIPR,R,PSAXIS,DUMMY)
          IF (PSAXIS.LT.PSMIN) THEN
            PSMIN = PSAXIS
            NAX  = N
            NAX1 = N1
            NAX2 = N2
            RAX = R
            SAX = -1.
C           WRITE(*,*) XAXIS, NAX, PSMIN
          ENDIF
        ENDIF
      ENDDO
      DO  N=NP-1, (NR-1)*(NP-1), NP-1
        N1 = NODENO(N,2)
        N2 = NODENO(N,3)
        IF  (PSI(4*(N1-1)+2)*PSI(4*(N2-1)+2).LT.0.) THEN
C------------------------------------- QUAD. EQ FOR R VALUE AT MINIMUM -
          PSIM  = PSI(4*(N1-1)+1)
          PSIMR = PSI(4*(N1-1)+2)
          PSIP  = PSI(4*(N2-1)+1)
          PSIPR = PSI(4*(N2-1)+2)
          AA =  3. * (PSIM + PSIMR - PSIP + PSIPR ) / 4.
          BB =  ( - PSIMR + PSIPR ) / 2.
          CC =  ( - 3*PSIM - PSIMR + 3*PSIP - PSIPR) / 4.
          DET = BB*BB - 4.*AA*CC
          R  = ROOT(AA,BB,CC,DET,1.D0)
          IF (DABS(R).GT.1.+1.D-8) THEN
            R = ROOT(AA,BB,CC,DET,-1.D0)
          ENDIF
C          WRITE(*,*) R, PSIM, PSIP
C          IF (DABS(R).LT.1.+1.D-8) THEN
C-------- THE DSIGN OF R CHANGES FOR ELEMENTS ON THE LEFT  (SEE REMESH) -
          CALL CUB1D(XX(1,N1),XX(2,N1),XX(1,N2),XX(2,N2),R,XAXIS,DUMMY)
          CALL CUB1D(PSIM,PSIMR,PSIP,PSIPR,R,PSAXIS,DUMMY)
          IF (PSAXIS.LT.PSMIN) THEN
            PSMIN = PSAXIS
            NAX  = N
            NAX1 = N1
            NAX2 = N2
            RAX = R
	    SAX = 1.
C            WRITE(*,*)XAXIS, NAX, PSMIN
          ENDIF
        ENDIF
      ENDDO
      YAXIS = 0.
      ENDIF
      RETURN
      END
      
***********************************************************************
*DECK FZERO2
      SUBROUTINE FZERO2(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C----------------------------------------------------------------------
C SOLUTION DETERMINES THE MINIMUM OF THE FLUX IN ONE ELEMENT
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE FAXIS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  X(N),FVEC(N),FJAC(LDFJAC,N)

      R = X(1)
      S = X(2)
      N1 = NODENO(NAXIS,1)
      N2 = NODENO(NAXIS,2)
      N3 = NODENO(NAXIS,3)
      N4 = NODENO(NAXIS,4)
      CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >            PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >            R,S,ZPSI,ZPSIR,ZPSIS,ZPSIRS,ZPSIRR,ZPSISS)
      IF (IFLAG.EQ.1) THEN
        FVEC(1) = ZPSIR
        FVEC(2) = ZPSIS
      ENDIF
      IF (IFLAG.EQ.2) THEN
        FJAC(1,1) = ZPSIRR
        FJAC(1,2) = ZPSIRS
        FJAC(2,1) = ZPSIRS
        FJAC(2,2) = ZPSISS
      ENDIF
      RETURN
      END

************************************************************************
*DECK ROOT
      FUNCTION ROOT(A,B,C,D,SGN)
C---------------------------------------------------------------------
C THIS FUNCTION GIVES BETTER ROOTS OF QUADRATICS BY AVOIDING
C CANCELLATION OF SMALLER ROOT
C---------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IF (B*SGN .GE. 0.0) THEN
        ROOT = -2.0*C/(B+SGN*DSQRT(D))
      ELSE
        ROOT = (-B + SGN*DSQRT(D)) / (2.0 * A)
      ENDIF
      RETURN
      END
************************************************************************
*DECK NORMAL
      SUBROUTINE NORMAL(PSI,NR,NP,PSAXIS)
C-----------------------------------------------------------------------
C SUBROUTINE TO NORMALIZE PSI TO ONE ON THE BOUNDARY AND ZERO ON AXIS
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  PSI(*)

      DO 60 I=1,NR*NP
        PSI(4*(I-1)+1) = 1. - (1.- PSI(4*(I-1)+1)) / (1.- PSAXIS)
        DO 70 L=2,4
          PSI(4*(I-1)+L) = PSI(4*(I-1)+L) / (1. - PSAXIS)
   70   CONTINUE
   60 CONTINUE
      RETURN
      END

************************************************************************
*DECK REMESH
       SUBROUTINE REMESH(XX,YY,PSI,A,B,C,EPS,NR,NP,NRNEW,NPNEW,MESHNO,
     >                  CX,CY,XAXIS,XAXISOLD,YAXIS,NAX,RAX,SAX,IGAM,IAS)
C---------------------------------------------------------------------
C FORM THE SYSTEM OF NEW FINITE ELEMENTS AS FLUX COORDINATES UDSING
C THE EXACT INTERPOLATION
C   XX,YY,PSI : ON EXIT CONTAIN THE VALUES ON THE NEW GRID
C   NR,NP     : THE NUMBER RADIAL AND POLOIDAL POINTS IN THE OLD GRID
C   NRNEW,NPNEW :         ,,                ,,         IN THE NEW GRID
C   XAXIS : POSITION OF MAGNETIC AXIS
C   NAX1,NAX2 : NODENUMBERS OF ELEMENT WITH MAGNETIC AXIS
C   RAX : R VALUE OF MAGNETIC AXIS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE MESH
      USE NODES
      USE TOLERA
      USE MESHAC
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*), YY(4,*), PSI(*)
      INTEGER NSYM(2*NR-2)
      DOUBLE PRECISION SSYM(2*NR-2)
      LOGICAL FOUND,NOBRACK,CHAGR(MAXMNODE),AXISONRIGHT

C      WRITE(*,*) 'REMESH RAX, SAX', RAX, SAX
      PI = 2. * DASIN(1.D0)
      TOLTHT = 1.D-10
      TOLPSI = 1.D-10
      
      FACTAS = 1.
      IF (IAS.EQ.0) FACTAS = 2.
      

      DO I=1,NR-1
        NSYM(I) = (I-1)*(NP-1) + 1
        SSYM(I) = -1.
        NSYM(NR-1+I) = (NR-1)*(NP-1) - (I-1)*(NP-1)
        SSYM(NR-1+I) = +1.
      ENDDO

      IF ((IMESH.EQ.2).AND.(XR1.LE.1.).AND.(SIG1.LT.1.))THEN
          IF ((NRDONE.NE.NRNEW).OR.(XR1DONE.NE.XR1)
     >                        .OR.(SIG1DONE.NE.SIG1)) THEN
            CALL MESHAC2(NRNEW,SG,DSG,DDSG,XR1,SIG1)
            NRDONE = NRNEW
	    XR1DONE = XR1
	    SIG1DONE = SIG1
	  ENDIF
          DO I=1,NRNEW
            PSIKN(NRNEW-I+1)   = SG(I)**2
            DPSIKN(NRNEW-I+1)  = 2.*SG(I) * DSG(I)
            DDPSIKN(NRNEW-I+1) = 2.*SG(I) * DDSG(I) + 2.*DSG(I)**2
            RADPSI(NRNEW-I+1)  =  REAL(I-1)/REAL(NRNEW-1)
	  ENDDO
      ELSE 
        DO  I=1,NRNEW
          RPSI =  REAL(I-1)/REAL(NRNEW-1)
          CALL RADMESH(RPSI,PSID,DPSID,DDPSID)
          PSIKN(NRNEW-I+1)   = PSID
	  DPSIKN(NRNEW-I+1)  = DPSID
	  DDPSIKN(NRNEW-I+1) = DDPSID
          RADPSI(NRNEW-I+1)  = RPSI
	ENDDO
      ENDIF
      RADPSI(NRNEW) = 0.
      PSIKN(NRNEW) = 0.
C      CALL PRARR1('PSI VALUES : ',PSIKN,NRNEW,203)
      DO 6 J=1,NPNEW
        THTKN(J) = (1.+FLOAT(IAS)) * PI * REAL(J-1)/REAL(NPNEW-1)
    6 CONTINUE
      MESHNO = MESHNO + 1
C------------------------- UPDATE OLD MESH FOR THE FIRST ITERATION ----
      DO 8 I= 1,NRNEW*NPNEW
        CHAGR(I) = .FALSE.
    8 CONTINUE
      DO 10 I = 1,NR*NP
        DO 20 K=1,4
          XXOLD(K,I) = XX(K,I)
          YYOLD(K,I) = YY(K,I)
          PSIOLD(4*(I-1)+K) = PSI(4*(I-1)+K)
   20   CONTINUE
   10 CONTINUE
      
C---IF THE AXIS IS EXTREAMLY CLOSE TO THE ORIGINAL, JUST SET IT TO ORIGINAL
C      IF ((DABS(RAX-1.D0).LT.1.D-4).AND.(NAX.GT.(NP-1)*(NR-2))) THEN
C         WRITE(*,*)'THAT IS CLOSE ENOUGH!'
C         NAX = (NP-1)*(NR-2)+1
C         RAX = 1.D0
C         SAX = -1.D0
C         XAXIS = XAXISOLD+1.D-8
C      ENDIF
C--------------------------- LOOP OVER ALL FLUXSURFACES -----------
      DO 30 I=1,NRNEW-1
        PSIVAL=PSIKN(I)
C        WRITE(*,*) ' FINDING SURFACE : ',PSIVAL,NAX
C--------------------------- FIND STARTING POINT OF FLUXCONTOUR
        DO 40 N=NAX, 1, (1-NP)
          N1 = NODENO(N,1)
          N2 = NODENO(N,2)
          N3 = NODENO(N,3)
          N4 = NODENO(N,4)
C------------------------------------- QUAD. EQ FOR R VALUE AT MINIMUM -
          RR = -1.
          CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                 PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                 RR,SAX,PSIM,PSIMR,DPSIS)
          RR = +1.
          CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                 PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                 RR,SAX,PSIP,PSIPR,DPSIS)
          AA =  3. * (PSIM + PSIMR - PSIP + PSIPR ) / 4.
          BB =  ( - PSIMR + PSIPR ) / 2.
          CC =  ( - 3*PSIM - PSIMR + 3*PSIP - PSIPR) / 4.
          DET = BB*BB - 4.*AA*CC
          R = 999.
          IF (DET .GT. 0) THEN
            R = ROOT(AA,BB,CC,DET,1.D0)
            IF (DABS(R).GT.1.+1.D-5) THEN
              R = ROOT(AA,BB,CC,DET,-1.D0)
            ENDIF
          ENDIF
          IF (DABS(R).GT.1.) THEN
            PSIMIN=MIN(PSIM,PSIP)
            PSIMAX=MAX(PSIM,PSIP)
          ELSE 
            CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                 PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                 R,SAX,PS,DPSIR,DPSIS)
            PSIMIN=MIN(MIN(PSIM,PS),PSIP)
            PSIMAX=MAX(MAX(PSIM,PS),PSIP)
          ENDIF
          IF ((PSIVAL.GE.PSIMIN-PSITOL).AND.
     >        (PSIVAL.LE.PSIMAX+PSITOL))THEN
            A3 = (PSIM+PSIMR-PSIP+PSIPR)/4.
            A2 = (- PSIMR + PSIPR)/4.
            A1=(-3*PSIM-PSIMR+3*PSIP-PSIPR)/4.
            A0=( 2*PSIM+PSIMR+2*PSIP-PSIPR)/4.-PSIVAL
            CALL SOLVP3(A0,A1,A2,A3,RR,R2,R3,IFAIL)
            CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                   PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                   RR,SAX,PS,DPSIR,DPSIS)
            CALL INTERP2(XXOLD(1,N1),XXOLD(1,N2),
     >                   XXOLD(1,N3),XXOLD(1,N4),
     >                   RR,SAX,ZX,DXR,DXS)
            IF (((XAXIS.GT.XAXISOLD).AND.(ZX.LT.XAXIS)).OR.
     >          ((XAXIS.LT.XAXISOLD).AND.(ZX.GT.XAXIS))) THEN
C              WRITE(*,*) ' NODE ON WRONG SIDE : ',ZX,XAXIS,SAX
              RR = R2
              CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                     PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                     RR,SAX,PS,DPSIR,DPSIS)
              CALL INTERP2(XXOLD(1,N1),XXOLD(1,N2),
     >                     XXOLD(1,N3),XXOLD(1,N4),
     >                     RR,SAX,ZX,DXR,DXS)
            ENDIF
            IF ((DABS(RR).LE.1.+1.D-5).AND.
     >           (((XAXIS.LT.XAXISOLD).AND.(ZX.LT.XAXIS)).OR.
     >           ((XAXIS.GT.XAXISOLD).AND.(ZX.GT.XAXIS)))) GOTO 45 
c------------------ special case psi=1.
            IF ((I.EQ.1).AND.(DABS(PS-PSIVAL).LT.1.D-5)) THEN
              RR = -1.
              GOTO 45
            ENDIF
          ENDIF
   40   CONTINUE
        WRITE(*,*) ' STARTING VALUE NOT FOUND : ',ZX,RR
   45   CONTINUE
c--------------------------- starting position found ----------------
        CALL INTERP2(YYOLD(1,N1),YYOLD(1,N2),
     >               YYOLD(1,N3),YYOLD(1,N4),RR,SAX,ZY,DYR,DYS)
        THT0 = DATAN2(ZY-YAXIS,ZX-XAXIS)
        IF (THT0.LT.0.) THT0 = THT0 + 2.*PI
C----------------------------- trace fluxsurface to find theta values
        DD = 0.25*(PSIVAL)**0.33 / FACTAS 
        IPREV = 0
        JPREV = 0
        SS = SAX 
        NN = N
        THT1 = THT0 
        THT2 = THT0
        ITMAX = 2500
        ITTEST = 0
	NOBRACK = .TRUE.
        DO 50 J=1,NPNEW
	  If (IAS.EQ.1) THEN
            JINDEX = MOD(INT(THT0/(2.*PI)*NPNEW) + J,NPNEW)+1
	  ELSE
	    JINDEX = J
	  ENDIF
          THTVAL = THTKN(JINDEX)
          FOUND = .FALSE.

c---------------------------------- treat theta=pi as special point
          IF (((J.EQ.1).OR.(J.EQ.NPNEW)).AND.(IAS.EQ.0)) THEN
C--------SPECIAL TREATMENT FOR PSI=1
            IF (I.EQ.1) THEN
               IF (J.EQ.1) THEN
                  RR = -1.
                  NN = 1
                  SS = -1.
                  FOUND = .TRUE.
                  NOBRACK = .FALSE.
               ELSE
                  RR = -1
                  NN = NPNEW-1
                  SS = 1.
                  FOUND = .TRUE.
                  NOBRACK = .FALSE.
               ENDIF
               GOTO 145
            ENDIF
                  
            DO 140 NS=1, 2*NR-2
              N  = NSYM(NS)
              SS = SSYM(NS)
              N1 = NODENO(N,1)
              N2 = NODENO(N,2)
              N3 = NODENO(N,3)
              N4 = NODENO(N,4)
              RR = -1.
              CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                 PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                 RR,SS,PSIM,PSIMR,DPSIS)
              RR = +1.
              CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                 PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                 RR,SS,PSIP,PSIPR,DPSIS)
              A3 = (PSIM+PSIMR-PSIP+PSIPR)/4.
              A2 = (- PSIMR + PSIPR)/4.
              A1=(-3*PSIM-PSIMR+3*PSIP-PSIPR)/4.
              A0=( 2*PSIM+PSIMR+2*PSIP-PSIPR)/4.-PSIVAL
              CALL SOLVP3(A0,A1,A2,A3,RR,R2,R3,IFAIL)
              CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                     PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                     RR,SS,PS,DPSIR,DPSIS)
              CALL INTERP2(XXOLD(1,N1),XXOLD(1,N2),
     >                     XXOLD(1,N3),XXOLD(1,N4),
     >                     RR,SS,ZX,DXR,DXS)
              IF (((J.NE.1).AND.(ZX.GT.XAXIS)).OR.
     >            ((J.EQ.1).AND.(ZX.LT.XAXIS))) THEN
C                WRITE(*,*) ' NODE ON WRONG SIDE : ',ZX,XAXIS,PS
                 RR = R2
                 CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                        PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                        RR,SS,PS,DPSIR,DPSIS)
                 CALL INTERP2(XXOLD(1,N1),XXOLD(1,N2),
     >                        XXOLD(1,N3),XXOLD(1,N4),
     >                        RR,SS,ZX,DXR,DXS)
              ENDIF
              IF ((DABS(RR).LE.1.+1.D-8).AND.
     >             (((J.NE.1).AND.(ZX.LT.XAXIS)).OR.
     >              ((J.EQ.1).AND.(ZX.GT.XAXIS)))
     >                                 .AND.(DABS(ZX).LE.1.+1.D-8)) THEN
                FOUND = .TRUE.
                NN = N
                NOBRACK= .FALSE.
C                WRITE(*,*)'TREAT PI=0,PI', J, I, PS, PSIVAL
                GOTO 145 
              ENDIF
  140       CONTINUE
            WRITE(*,*)'TREAT PI=0,PI FAILED ON', I, PS, PSIVAL
          ENDIF
  145     CONTINUE
                    
      	  DO ITN=1,ITMAX
            N1 = NODENO(NN,1)
            N2 = NODENO(NN,2)
            N3 = NODENO(NN,3)
            N4 = NODENO(NN,4)
            CALL INTERP2(XXOLD(1,N1),XXOLD(1,N2),
     >                   XXOLD(1,N3),XXOLD(1,N4),RR,SS,ZX,ZXR,ZXS)
            CALL INTERP2(YYOLD(1,N1),YYOLD(1,N2),
     >                   YYOLD(1,N3),YYOLD(1,N4),RR,SS,ZY,ZYR,ZYS)
            CALL INTERP2(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                   PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >                   RR,SS,PS,DPSIR,DPSIS)
            THT = DATAN2(ZY-YAXIS,ZX-XAXIS)

            IF (THT.LT.0.) THT = THT + 2*PI
            IF (NOBRACK) THEN
              THT1 = THT2
              THT2 = THT
	    ELSEIF (FOUND) THEN
	      THT1 = THT
            ENDIF
            IF (((DABS(THTVAL-THT).LT.TOLTHT) .OR.
     >           (DABS(THTVAL-THT+2*PI) .LT. TOLTHT) .OR.
     >           (DABS(THTVAL-THT-2*PI) .LT. TOLTHT)) .AND.
     >           (DABS(PSIVAL-PS).LT.TOLPSI))  THEN 
c
c----------------------------- node located ---------------------
c
c              IF ((j.eq.np)) THEN
c              WRITE(*,*) ' NODE LOCATED : '
c              WRITE(*,41) J,ZX,ZY,PS,THT,PS-PSIVAL,THT-THTVAL
c              ENDIF
              FOUND = .TRUE.
              IF ((DABS(RR).GT.1.+TOLPSI).OR.
     >            (DABS(SS).GT.1.+TOLTHT)) THEN
                WRITE(*,*) ' WARNING : ',PS,THT,RR,SS
              ENDIF
              NODE = (I-1)*NPNEW + JINDEX       
              N1 = NODENO(NN,1)
              N2 = NODENO(NN,2)
              N3 = NODENO(NN,3)
              N4 = NODENO(NN,4)
              CALL INTERP(XXOLD(1,N1),XXOLD(1,N2),XXOLD(1,N3),
     >                    XXOLD(1,N4),RR,SS,X,XR,XS,XRS,XRR,XSS)
              CALL INTERP(YYOLD(1,N1),YYOLD(1,N2),YYOLD(1,N3),
     >                    YYOLD(1,N4),RR,SS,Y,YR,YS,YRS,YRR,YSS)
              CALL INTERP(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >                    PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >               RR,SS,ZPSI,ZPSIR,ZPSIS,ZPSIRS,ZPSIRR,ZPSISS)
c-------------------------------------------------------------------
              RAD = (X-XAXIS)**2 + (Y-YAXIS)**2
              THY = (X-XAXIS) / RAD
              THX = -(Y-YAXIS) / RAD
              THXX = 2*(Y-YAXIS)*(X-XAXIS) / RAD**2
              THYY = - THXX
              THXY = ( (Y-YAXIS)**2 - (X-XAXIS)**2 ) / RAD**2
              THS = THX * XS + THY * YS
              THR = THX * XR + THY * YR
              THRR = THXX*XR*XR + 2*THXY*XR*YR + THX*XRR
     >             + THYY*YR*YR + THY*YRR
              THRS = THXX*XR*XS + THXY*XR*YS + THX*XRS
     >             + THXY*YR*XS + THYY*YR*YS + THY*YRS
              THSS = THXX*XS*XS + 2*THXY*XS*YS + THX*XSS
     >             + THYY*YS*YS + THY*YSS
              PTJAC = ZPSIR*THS - ZPSIS*THR
              RT = - ZPSIS / PTJAC
              ST =   ZPSIR / PTJAC
              PTJR = ZPSIRR*THS+ZPSIR*THRS-ZPSIRS*THR-ZPSIS*THRR
              PTJS = ZPSIRS*THS+ZPSIR*THSS-ZPSISS*THR-ZPSIS*THRS
              RPT = (-PTJR*THS/PTJAC**2 + THRS/PTJAC) * RT
     >            + (-PTJS*THS/PTJAC**2 + THSS/PTJAC) * ST
              SPT = ( PTJR*THR/PTJAC**2 - THRR/PTJAC) * RT
     >            + ( PTJS*THR/PTJAC**2 - THRS/PTJAC) * ST
              XPT = - XRR * THS*ZPSIS/PTJAC**2
     >              + XRS * (THS*ZPSIR + THR*ZPSIS)/PTJAC**2
     >              + XR  * RPT        + XS * SPT
     >              - XSS * THR*ZPSIR/PTJAC**2
              YPT = - YRR * THS*ZPSIS/PTJAC**2
     >              + YRS * (THS*ZPSIR + THR*ZPSIS)/PTJAC**2
     >              + YR  * RPT        + YS * SPT
     >              - YSS * THR*ZPSIR/PTJAC**2
              XYJAC = XR*YS - XS*YR
              PSIX = ( YS*ZPSIR - YR*ZPSIS) / XYJAC
              PSIY = (-XS*ZPSIR + XR*ZPSIS) / XYJAC
c              CALL RADMESH(RADPSI(I),DUM,DDUM,DDDUM)
              ETAP = 1./ DPSIKN(I)
              EJAC = ETAP * (PSIX*THY - PSIY*THX)
              XX(1,NODE) = X
              YY(1,NODE) = Y
C-------------------------------- WATCH MINUS DSIGN FROM R ORIENTATION --
              XX(2,NODE) = - ( THY  / EJAC)   /  (2.*(NRNEW-1.))
              YY(2,NODE) = - (-THX  / EJAC)   /  (2.*(NRNEW-1.))
              XX(3,NODE) =+(-ETAP*PSIY / EJAC) / (FACTAS*(NPNEW-1)/PI)
              YY(3,NODE) =+( ETAP*PSIX / EJAC) / (FACTAS*(NPNEW-1)/PI)
              XX(4,NODE) =-(XPT/ETAP)/(2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
              YY(4,NODE) =-(YPT/ETAP)/(2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
              PSI(4*(NODE-1)+1) = ZPSI
              PSI(4*(NODE-1)+2) = - DPSIKN(I) / (2.*(NRNEW-1.))
              PSI(4*(NODE-1)+3) = 0.
              PSI(4*(NODE-1)+4) = 0.
              CHAGR(NODE) = .TRUE.
              IF (ITN.GT.ITTEST) ITTEST = ITN
              GOTO 50
            ELSEIF ( ((THT1.LE.THTVAL+1.D-5).AND.
     >                (THT2.GE.THTVAL-1.D-5)) 
     >        .OR.
     >         ((IAS.EQ.1).AND.(THT1.GT.THT2+1.57)
     >                    .AND.(THT1.LE.THTVAL+1.D-5)
     >                    .AND.(THT2+2*PI.GE.THTVAL-1.D-5)) 
     >        .OR.
     >         ((IAS.EQ.1).AND.(THT1.GT.THT2+1.57)
     >                    .AND.(THT1-2*PI.LE.THTVAL+1.D-5)
     >                    .AND.(THT2.GE.THTVAL-1.D-5)) )  THEN
      

   47         FORMAT(' THETA BRACK : ',3E14.6)	      
c
c----------------------------- theta value bracketed ------------
c
              FVEC1 = -(ZY - YAXIS) + (ZX - XAXIS) * DTAN(THTVAL)
              FVEC2 = -PS + PSIVAL
              FJAC11 = ZYR - ZXR*DTAN(THTVAL)
              FJAC12 = ZYS - ZXS*DTAN(THTVAL)
              FJAC21 = DPSIR
              FJAC22 = DPSIS
              DIS = FJAC22*FJAC11-FJAC12*FJAC21
              DR = (FJAC22*FVEC1-FJAC12*FVEC2)/DIS
              DS = (FJAC11*FVEC2-FJAC21*FVEC1)/DIS   
              DR = DSIGN(MIN(DABS(DR),0.05),DR)
              DS = DSIGN(MIN(DABS(DS),0.05),DS)
              RR = RR + DR
              SS = SS + DS           
   46         FORMAT(' 2D NEWTON : ',1P5E12.4)
   56         FORMAT(' No brack : ',1P5E12.4)
              NOBRACK = .FALSE.
            ELSE
c
c------------------------ theta not found yet, track flux surface
c
              NOBRACK = .TRUE.
              DS = - DD * DPSIR/DABS(DPSIR)
              IF (DABS((PSIVAL-PS)/PSIVAL).GT.0.01) THEN 
                DS=0.
                DR = (PSIVAL-PS) / DPSIR
                RR = RR + DR
              ELSE
                DR = (PSIVAL-PS - DPSIS * DS ) / DPSIR
                DTOT = DSQRT(DR*DR+DS*DS)
                DR = DR * DD/DTOT
                DS = DS * DD/DTOT
                RR = RR + DR
                SS = SS + DS
              ENDIF
            ENDIF
            IN = (NN-1)/(NP-1)+1
            JN = MOD(NN-1,NP-1)+1
            IF (SS.LT.-1.) THEN 
              IF (JPREV.EQ.-1) THEN
                NN = NN - 1
                IF (JN.EQ.1) NN = NN + (NP-1)
                SS = SS + 2.
                JPREV = 0 
              ELSE
                SS = -1.
                JPREV = -1 
              ENDIF 
            ELSEIF (SS.GT.1) THEN
              IF (JPREV.EQ.1) THEN
                NN = NN + 1
                IF (JN.EQ.NP-1) NN = NN - (NP-1)
                SS = SS - 2.
                JPREV = 0
              ELSE
                SS = 1.
                JPREV = 1
              ENDIF 
            ENDIF
            IF (RR.LT.-1) THEN
              IF ((IN.GT.1).AND.(IPREV.EQ.-1))THEN
                NN = NN - NP + 1
                RR = RR + 2.
                IPREV = 0
              ELSE
                RR = -1.
                IPREV = -1
              ENDIF
            ELSEIF (RR.GT.1.) THEN
              IF ((IN.LT.NR-1).AND.(IPREV.EQ.1)) THEN
                NN = NN + NP - 1
                RR = RR - 2. 
                IPREV = 0
              ELSE
                RR = 1.
                IPREV = 1
              ENDIF
            ENDIF
          ENDDO
          WRITE(20,*) ' FATAL NODE NOT FOUND : ',I,J,PSIVAL,THTVAL
   50   CONTINUE         
C        WRITE(*,*) ' MAX NUMBER ITERATIONS : ',ITTEST,DD 
   30 CONTINUE
   41 FORMAT(i3,2e12.4,4e14.6)
   42 FORMAT(i3,6e12.4)

      IF (IAS.EQ.1) THEN
C------------------------------------ copy tht=0 to tht=2PI
      DO I=1,NRNEW-1
        J = NPNEW
        NODE = (I-1)*NPNEW + J       
        J0 = 1
        N0 = (I-1)*NPNEW + J0 
        XX(1,NODE) = XX(1,N0)
        XX(2,NODE) = XX(2,N0)
        XX(3,NODE) = XX(3,N0)
        XX(4,NODE) = XX(4,N0)
        YY(1,NODE) = YY(1,N0)
        YY(2,NODE) = YY(2,N0)
        YY(3,NODE) = YY(3,N0)
        YY(4,NODE) = YY(4,N0)
        PSI(4*(NODE-1)+1) = PSI(4*(N0-1)+1) 
        PSI(4*(NODE-1)+2) = PSI(4*(N0-1)+2) 
        PSI(4*(NODE-1)+3) = PSI(4*(N0-1)+3) 
        PSI(4*(NODE-1)+4) = PSI(4*(N0-1)+4) 
        CHAGR(NODE)=.TRUE.
      ENDDO
      ENDIF
      
      N1 = NODENO(NAX,1)
      N2 = NODENO(NAX,2)
      N3 = NODENO(NAX,3)
      N4 = NODENO(NAX,4)
      S = SAX
      R = RAX
      CALL INTERP(XXOLD(1,N1),XXOLD(1,N2),XXOLD(1,N3),XXOLD(1,N4),
     >            R,S,X,XR,XS,XRS,XRR,XSS)
      CALL INTERP(YYOLD(1,N1),YYOLD(1,N2),YYOLD(1,N3),YYOLD(1,N4),
     >            R,S,Y,YR,YS,YRS,YRR,YSS)
      CALL INTERP(PSIOLD(4*(N1-1)+1),PSIOLD(4*(N2-1)+1),
     >            PSIOLD(4*(N3-1)+1),PSIOLD(4*(N4-1)+1),
     >            R,S,EQPSI,PSIR,PSIS,PSRS,PSRR,PSSS)
      
C---IF THE NEW AXIS IS VERY CLOSE TO THE OLD AXIS, EJAC WILL BE ILL BEHAVED
C---IN THIS CASE WE'LL RETAIN CX AND CY VALUE ON AXIS
      IF ((DABS(RAX-1.D0).LT.1.D-2).AND.(NAX.GT.(NP-1)*(NR-2))) GOTO 456
      EJAC = XR*YS - XS*YR
      RY = - XS / EJAC
      RX =   YS / EJAC
      SY =   XR / EJAC
      SX = - YR / EJAC
      PSIXX = PSRR*RX*RX + 2.*PSRS*RX*SX + PSSS*SX*SX
      PSIYY = PSRR*RY*RY + 2.*PSRS*RY*SY + PSSS*SY*SY
      CALL CALCRJPHI(XAXIS,0.D0,0.D0,A,ARHS,DET)
      PSIYY0 = ARHS * A / (1.D0-DET)  - PSIXX
      CX = PSIXX/2.
      CY = PSIYY0/2.
 456  DO 60 J = 1, NPNEW
        NODE = (NRNEW-1)*NPNEW + J
        XX(1,NODE) = XAXIS
        YY(1,NODE) = YAXIS
        TN = DTAN(THTKN(J))
        TN2 = TN**2
        CN = DCOS(THTKN(J))
        IF (THTKN(J).EQ.(PI/2.)) THEN
          XX(2,NODE) = 0.
          YY(2,NODE) = -1./(DSQRT(CY)*2.*(NRNEW-1))
          XX(4,NODE) = +1./(DSQRT(CY)*2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
          YY(4,NODE) = 0.
        ELSEIF (THTKN(J).EQ.(3*PI/2)) THEN
          XX(2,NODE) = 0.
          YY(2,NODE) = -1./(DSQRT(CY)*2.*(NRNEW-1))
          XX(4,NODE) = +1./(DSQRT(CY)*2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
          YY(4,NODE) = 0.
        ELSE
         XX(2,NODE)=- DSIGN(1.D0,CN)/(DSQRT(CX + CY*TN2) * 2.*(NRNEW-1))
         YY(2,NODE) = - DABS(TN) /(DSQRT(CX + CY*TN2) * 2.*(NRNEW-1))
         XX(4,NODE) = + (CX+CY*TN2)**(-1.5) * CY * DABS(TN)
     >              / (CN**2 * 2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
         YY(4,NODE) = - CX * (CX + CY*TN2)**(-1.5) / ( CN*DABS(CN)
     >              * 2.*FACTAS*(NRNEW-1)*(NPNEW-1)/PI)
        ENDIF
        IF (THTKN(J).GT.PI) THEN
          YY(2,NODE) = - YY(2,NODE)
          XX(4,NODE) = - XX(4,NODE)
        ENDIF
        XX(3,NODE) = 0.
        YY(3,NODE) = 0.
        PSI(4*(NODE-1)+1) = 0.
        PSI(4*(NODE-1)+2) = 0.
        PSI(4*(NODE-1)+3) = 0.
        PSI(4*(NODE-1)+4) = 0.
        CHAGR(NODE) = .TRUE.
C        WRITE(*,*)J,(XX(MM,NODE),MM=1,4)
   60 CONTINUE
      DO 80 I=1,NRNEW*NPNEW
        IF (.NOT. CHAGR(I)) WRITE(20,*) 'NODE MISSED AT I = ',I
   80 CONTINUE
      NR = NRNEW
      NP = NPNEW
      XAXISOLD = XAXIS
      CALL ELMNO(NR,NP,NODENO)
    3 FORMAT('ELONGATION ON AXIS : CX,CY = ',2E12.4)

c      CALL PRARR1('X : ',XX,4*NRNEW*NPNEW,203)
c      CALL PRARR1('Y : ',YY,4*NRNEW*NPNEW,203)
c      CALL PRARR1('PSI : ',PSI,4*NRNEW*NPNEW,203)

      RETURN
      END


************************************************************************
*DECK RADMESH
      SUBROUTINE RADMESH(RPSI,ZPSI,DZPSI,DDZPSI)
C-------------------------------------------------------------------
C FUNCTION TO DETERMINE A NON-EQUIDISDTANT GRID IN DSQRT(PSI). GRID
C WILL BE EQUIDISDTANT IN RPSI.
C RPSI MUST BE A MONOTONIC FUNCTION OF PSI BETWEEN 0. AND 1.
C-------------------------------------------------------------------
      USE MESHAC
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      ZPSI = RPSI**2
      DZPSI= 2*RPSI
      DDZPSI = 2.
      IF (IMESH.EQ.1) THEN
        A1 = AMESH
        A2 = BMESH

        ZPSI  = RPSI**2  + A1*RPSI**3 + A2*RPSI**4 -(A1+A2)*RPSI**5
        DZPSI = 2*RPSI   + 3*A1*RPSI**2 + 4*A2*RPSI**3
     >                 - 5*(A1+A2)*RPSI**4
        DDZPSI = 2.      + 6*A1*RPSI + 12*A2*RPSI**2
     >                 - 20*(A1+A2)*RPSI**3

      ENDIF
      RETURN
      END
************************************************************************
*DECK RPACK
      FUNCTION RPACK(X,X0,B,T,AMP)
C-------------------------------------------------------------------
C POLYNOMIAL TO DESCRIBE LOCAL CHANGES OF THE RADIAL COORDINATE
C       X0 : CENTRAL X VALUE
C       B  : THE POSITION OF THE MAXIMUM
C       T  : THE TOTAL HALFWIDTH
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DX = DABS(X-X0)
      IF (DX.LT.B) THEN
        P4 = 2*DX/B - (DX/B)**2
      ELSE
        DX = DX - B
        P4 = 1. - 3*(DX/(T-B))**2 + 2*(DX/(T-B))**3
      ENDIF
      RPACK = AMP * P4
      IF (X.LT.X0) RPACK = - RPACK
      IF (DABS(X-X0).GT.T) RPACK = 0.
      RETURN
      END
************************************************************************
*DECK CHECKFORCE
      FUNCTION CHECKFORCE(XX,YY,PSI,A,NR0,NP0)
C-----------------------------------------------------------------------
C CHECK FORCE BALANCE AT CENTER OF EACH CELL
C-----------------------------------------------------------------------
      USE COMDAT
      USE CORNERS
      USE COMPIE
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION XX(4,*),YY(4,*),PSI(*)
      DOUBLE PRECISION A, LHS
      INTEGER DEVMPS, DEVMTH,DEVMPSB,DEVMPSC,DEVMTHB,DEVMTHC
      FEDGE = XGAMMA(1.D0) * DSQRT(DABS(A))
      PSITOT  = BVAC * RVAC**2 * EPS / FEDGE
      WRITE(20,*)' '
      WRITE(20,*)'******** CHECK FORCE BALANCE *********'
      DEVMAX = 0.D0
      DEVMPS = 0
      DEVMTH = 0
      DEVMAXB = 0.D0
      DEVMPSB = 0
      DEVMTHB = 0
      DEVMAXC = 0.D0
      DEVMPSC = 0
      DEVMTHC = 0
      TOTALDEV = 0.D0
      TOTALDEVC = 0.D0
      TOTALDEVB = 0.D0
      NELM = (NP0-1) * (NR0-2)
      RR = 0.D0
      SS = 0.D0
      DR = 1.D-4
      DS = 1.D-4
      DO 10 N=(NP0),NELM+(NP0-1)
         NPS = N / (NP0-1) + 1
         NTH = N - (NPS-1)*(NP0-1)
         N1 = NODENO(N,1)
         N2 = NODENO(N,2)
         N3 = NODENO(N,3)
         N4 = NODENO(N,4)
         CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),
     >        XX(1,N4),RR,SS,X,XR,XS,XRS,XRR,XSS)
         CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),
     >        YY(1,N4),RR,SS,Y,YR,YS,YRS,YRR,YSS)
         CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >        PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >        RR,SS,PS,PSR,PSS,PSRS,PSRR,PSSS)
         X0 = X
         XJAC0 = XR * YS - XS * YR
         XJACR = XRR * YS + XR * YRS - XRS * YR - XS * YRR
         XJACS = XRS * YS + XR * YSS - XSS * YR - XS * YRS
         PYJAC0= PSR * YS - PSS * YR
         PXJAC0= PSR * XS - PSS * XR
         PYJACR = PSRR * YS + PSR * YRS  - PSRS * YR - PSS * YRR
         PYJACS = PSRS * YS + PSR * YSS  - PSSS * YR - PSS * YRS
         PXJACR = PSRR * XS + PSR * XRS  - PSRS * XR - PSS * XRR
         PXJACS = PSRS * XS + PSR * XSS  - PSSS * XR - PSS * XRS
         PSX0  = PYJAC0 / XJAC0
         PSY0  = - PXJAC0 / XJAC0
         SUMDPSI = DSQRT(PSX0**2 + PSY0**2)
         RY = - XS / XJAC0
         RX =   YS / XJAC0
         SY =   XR / XJAC0
         SX = - YR / XJAC0
         PSIXX =       (PYJACR - PYJAC0 * XJACR / XJAC0) / XJAC0 * RX
         PSIXX = PSIXX+(PYJACS - PYJAC0 * XJACS / XJAC0) / XJAC0 * SX
         PSIYY =      -(PXJACR - PXJAC0 * XJACR / XJAC0) / XJAC0 * RY
         PSIYY = PSIYY-(PXJACS - PXJAC0 * XJACS / XJAC0) / XJAC0 * SY
         YS0 = YS
         YR0 = YR
         XS0 = XS
         XR0 = XR
         PS0 = PS
C----RIGHT HAND SIDE------
         CALL CALCRJPHI(X,PS,SUMDPSI,A,ARHS,DET)
         CALL CALCBTPD (X,PS,SUMDPSI,A,BTOT,TPER,DET)
         TEMP0 = TEMS(PS)
         RHO0 = CALCRHO(X,PS)
         PPER0 = TPER**2 / TEMP0 * RHO0
         PPAR0 = TPER * RHO0
         RHS = ARHS * A
C----LEFT HAND SIDE
         LHS = (PSIXX+PSIYY-PSX0*EPS/(1+EPS*X))
         IF (ITH.LT.1) GOTO 11
C----FINITE DIFFERENCE TO CALCULATE GRAD(DET)
         CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),
     >        XX(1,N4),RR+DR,SS,X,XR,XS)
         CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),
     >        YY(1,N4),RR+DR,SS,Y,YR,YS)
         CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >        PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >        RR+DR,SS,PS,PSR,PSS)
         XJAC = XR * YS - XS * YR
         PYJAC= PSR * YS - PSS * YR
         PXJAC= PSR * XS - PSS * XR
         PSX  = PYJAC / XJAC
         PSY  = -PXJAC / XJAC
         SUMDPSI = DSQRT(PSX**2 + PSY**2)
         CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER1,DET1)
         DETR = (DET1-DET)/DR
         CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),
     >        XX(1,N4),RR,SS+DS,X,XR,XS)
         CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),
     >        YY(1,N4),RR,SS+DS,Y,YR,YS)
         CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >        PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >        RR,SS+DS,PS,PSR,PSS)
         XJAC = XR * YS - XS * YR
         PYJAC= PSR * YS - PSS * YR
         PXJAC= PSR * XS - PSS * XR
         PSX  = PYJAC / XJAC
         PSY  = -PXJAC / XJAC
         SUMDPSI = DSQRT(PSX**2 + PSY**2)
         CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER2,DET2)
         TEMP2 = TEMS(PS)
         RHO2 = CALCRHO(X,PS)
         PPER2 = TPER2**2 / TEMP2 * RHO2
         DETS = (DET2-DET)/DS
         PPERS = (PPER2-PPER0)/DS
         SX = YR0 / XJAC0
         DETX = (DETR*YS0 - DETS*YR0)/XJAC0
         DETY = -(DETR*XS0 - DETS*XR0)/XJAC0
C----LEFT HAND SIDE DEAL WITH GRAD(DET)
         RHS = RHS + LHS * DET + DETX*PSX0 + DETY*PSY0
 11      DEV = 2 * DABS(LHS-RHS)/DABS(DABS(RHS)+DABS(LHS))
c$$$         GA = XGAMMA(PS0) * A
c$$$         DGA = DGDPSI(PS0) * A
c$$$         CPX = DGA/(1-DET)*PSX0 + GA/(1-DET)**2*DETX
c$$$         CPY = DGA/(1-DET)*PSY0 + GA/(1-DET)**2*DETY
c$$$         CP = DSQRT(CPX**2+CPY**2)
c$$$         IF ((NTH.EQ.1).OR.(NTH.EQ.NP0/2)) THEN
c$$$            WRITE(*,*) X0, CP
c$$$         ENDIF
c$$$         PPER0 = PPER0*A*B*PSITOT**2 / RVAC**4 / EPS**2
c$$$         PPAR0  = PPAR0*A*B*PSITOT**2 / RVAC**4 / EPS**2
c$$$         RBPHI2 = (XGAMMA(PS)/(1-DET))**2*A*PSITOT**2/RVAC**2 /EPS**2
c$$$         BPERCENT=(SUMDPSI/BTOT/(1+EPS*X0))**2
c$$$         IF (ITH.GT.0) THEN
c$$$            WRITE(39,*)PS0, 1+EPS*X0,(XGAMMA(PS)/(1-DET))**2,
c$$$     >           (RHS)/A, DABS(XJAC0),PPER0,PPAR0,RBPHI2,BPERCENT
c$$$         ELSE
c$$$            WRITE(39,*)PS0,1+EPS*X0,XGAMMA(PS)**2, 
c$$$     >           (RHS)/A, DABS(XJAC0),PPER0,PPAR0,RBPHI2,BPERCENT
c$$$         ENDIF
C         IF (DEV.GT.5.0D-2) THEN
c            WRITE(20,*)NPS,NTH,LHS,RHS,(RHS - LHS)*2/DABS(RHS+LHS)*100.
c         ENDIF
C----ADD TO TOTAL DEV
         IF (DEV.GT.DEVMAX) THEN
            DEVMAX = DEV
            DEVMPS = NPS
            DEVMTH = NTH
         ENDIF
         IF (N-NP0+1.LT.NELM/5) THEN
            TOTALDEVB = TOTALDEVB + DEV
            IF (DEV.GT.DEVMAXB) THEN
               DEVMAXB = DEV
               DEVMPSB = NPS
               DEVMTHB = NTH
            ENDIF
         ENDIF
         IF (N-NP0+1.GT.NELM*0.7) THEN
            TOTALDEVC = TOTALDEVC + DEV
            IF (DEV.GT.DEVMAXC) THEN
               DEVMAXC = DEV
               DEVMPSC = NPS
               DEVMTHC = NTH
            ENDIF
         ENDIF           
         TOTALDEV = TOTALDEV + DEV
 10   CONTINUE
      TOTALDEV  = TOTALDEV  / DFLOAT(NELM)
      TOTALDEVC = TOTALDEVC / DFLOAT(NELM) / 0.3
      TOTALDEVB = TOTALDEVB / DFLOAT(NELM) / 0.2
      CHECKFORCE = DABS(TOTALDEV)
      WRITE(20,*),'TOTAL'
      WRITE(20,13),DEVMAX*100.,DEVMPS,DEVMTH
      WRITE(20,14)TOTALDEV * 100.
      WRITE(20,*),'CORE 30%'
      WRITE(20,13),DEVMAXC*100.,DEVMPSC,DEVMTHC
      WRITE(20,14)TOTALDEVC * 100.
      WRITE(20,*),'OUTER 20%'
      WRITE(20,13),DEVMAXB*100.,DEVMPSB,DEVMTHB
      WRITE(20,14)TOTALDEVB * 100.
      WRITE(20,*)'**************************************'
      WRITE(*,*)
      WRITE(*,*)'********* FORCE CHECK SUMMARY **********'
      WRITE(*,*),'TOTAL'
      WRITE(*,13),DEVMAX*100.,DEVMPS,DEVMTH
      WRITE(*,14)TOTALDEV * 100.
      WRITE(*,*),'CORE 30%'
      WRITE(*,13),DEVMAXC*100.,DEVMPSC,DEVMTHC
      WRITE(*,14)TOTALDEVC * 100.
      WRITE(*,*),'OUTER 20%'
      WRITE(*,13),DEVMAXB*100.,DEVMPSB,DEVMTHB
      WRITE(*,14)TOTALDEVB * 100.
      WRITE(*,*)'*****************************************'
      WRITE(*,*)
c$$$      WRITE(40,*) DEVMAX,TOTALDEV,DEVMAXC,TOTALDEVC,DEVMAXB,TOTALDEVB
      RETURN
 14   FORMAT(' AVERAGE FORCE DEV ',f9.3,'%')
 13   FORMAT('   MAX   FORCE DEV ',f9.3,'% AT ',i3,' ',i3)
 12   FORMAT(' FORCE BALANCE BAD ',I4,' ',I4,' DEV ',f9.3, '%')
      END

************************************************************************
*DECK DRPACK
      FUNCTION DRPACK(X,X0,B,T,AMP)
C-------------------------------------------------------------------
C DERIVATIVE OF FUNCTION RPACK

C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DX = DABS(X-X0)
      IF (DX.LT.B) THEN
        P4 = 2./ B - 2*DX / B**2
      ELSE
        DX = DX - B
        P4 = - 6*DX/((T-B)**2) + 6*DX**2/(T-B)**3
      ENDIF
      DRPACK = AMP * P4
      IF (X.LT.X0) DRPACK = - DRPACK
      IF (DABS(X-X0).GT.T) DRPACK = 0.
      RETURN
      END

************************************************************************
*DECK DDRPACK
      FUNCTION DDRPACK(X,X0,B,T,AMP)
C-------------------------------------------------------------------
C SECOND DERIVATIVE OF FUNCTION RPACK
C------------------------------------------------------------------
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DDRPACK NOT CONTINUOUS !!!!!!
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DX = DABS(X-X0)
      IF (DX.LT.B) THEN
        P4 =  - 2. / B**2
      ELSE
        DX = DX - B
        P4 = - 6./((T-B)**2) + 12.*DX/(T-B)**3
      ENDIF
      DDRPACK = AMP * P4
      IF (X.LT.X0) DDRPACK = - DDRPACK
      IF (DABS(X-X0).GT.T) DDRPACK = 0.
      RETURN
      END



************************************************************************
*DECK PSIMIMA
      SUBROUTINE PSIMIMA(N,PSI,PSIMIN,PSIMAX)
C-----------------------------------------------------------------------
C SUBROUTINE TO DETERMINE THE MINIMA AND MAXIMA OF PSI AT THE ELEMENT
C BOUNDARIES FOR USE IN THE REMESH SUBROUTINE
C   N : NUMBER OF THE ELEMENT
C   PSI : VECTOR WITH PSI VALUES
C   PSIMIN,PSIMAX : THE RESULTING MINIMUM AND MAXIMUM VALUES OF PSI
C-----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  PSI(*)

      PSIMIN = 1E10
      PSIMAX =-1E10
      DO 10 I=1,4
        IM = MOD(I,4) + 1
        N1 = NODENO(N,I)
        N2 = NODENO(N,IM)
        IF (ABS(N1-N2).EQ.1) THEN
           PSIM  = PSI(4*(N1-1)+1)
           PSIMR = PSI(4*(N1-1)+3)
           PSIP  = PSI(4*(N2-1)+1)
           PSIPR = PSI(4*(N2-1)+3)
        ELSE
           PSIM  = PSI(4*(N1-1)+1)
           PSIMR = PSI(4*(N1-1)+2)
           PSIP  = PSI(4*(N2-1)+1)
           PSIPR = PSI(4*(N2-1)+2)
        ENDIF
        PSMA = MAX(PSIM,PSIP)
        PSMI = MIN(PSIM,PSIP)
        AA =  3. * (PSIM + PSIMR - PSIP + PSIPR ) / 4.
        BB =  ( - PSIMR + PSIPR ) / 2.
        CC =  ( - 3*PSIM - PSIMR + 3*PSIP - PSIPR) / 4.
        DET = BB**2 - 4*AA*CC
        IF (DET.GT.0.) THEN
           R = (-BB + DSQRT(BB**2-4*AA*CC) ) / (2*AA)
           IF (DABS(R).GT.1.) THEN
              R = (-BB - DSQRT(BB**2-4*AA*CC) ) / (2*AA)
           ENDIF
           IF (DABS(R).LE.1) THEN
              CALL CUB1D(PSIM,PSIMR,PSIP,PSIPR,R,PSMIMA,DUMMY)
              IF (PSMIMA.GT.PSMA) THEN
                 PSMA = PSMIMA
              ELSE
                 PSMI = PSMIMA
              ENDIF
           ENDIF
        ENDIF
        IF (PSMI.LT.PSIMIN) PSIMIN = PSMI
        IF (PSMA.GT.PSIMAX) PSIMAX = PSMA
   10 CONTINUE
      RETURN
      END

************************************************************************
*DECK THTMIMA
      SUBROUTINE THTMIMA(N,NP,XX,YY,XAXIS,YAXIS,THTMIN,THTMAX)
C-----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*), YY(4,*)

      THTMIN = 1E10
      THTMAX =-1E10
      DO 10 I=1,4
        NODE = NODENO(N,I)
        THETA = DATAN2(YY(1,NODE)-YAXIS,XX(1,NODE)-XAXIS)
        IF (THETA.LT.-1.D-3) THEN
          THETA = THETA + 6.28318530717958624
        ENDIF
        IF (THETA.LT.THTMIN) THTMIN = THETA
        IF (THETA.GT.THTMAX) THTMAX = THETA
   10 CONTINUE
      IF ((THTMIN.LT.1.57).AND.(THTMAX.GT.4.71)) THEN
        THTMIN = 1E10
        THTMAX =-1E10
        DO 20 I=1,4
          NODE = NODENO(N,I)
          THETA = DATAN2(YY(1,NODE)-YAXIS,XX(1,NODE)-XAXIS)
          IF (THETA.LT.THTMIN) THTMIN = THETA
          IF (THETA.GT.THTMAX) THTMAX = THETA
   20   CONTINUE
      ENDIF
      RETURN
      END

************************************************************************
*DECK SOLVP3
      SUBROUTINE SOLVP3(C0,C1,C2,C3,X1,X2,X3,IFAIL)
C-----------------------------------------------------------------------
C SOLVES A CUBIC EQUATION WITH A SOLUTION WITH -1.< X < 1
C CN : THE COEFFICIENT OF X**N, X : THE DOUBLE PRECISION  SOLUTION WITH -1.< X < 1.
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  C0,C1,C2,C3,X1,X2,X3

      X1 = 99.
      X2 = 999.
      X3 = 9999.
      TOL = 1D-4
      IFAIL = 0
c------------------------------------- 2nd order poly for small c3
      IF (DABS(C3)/(DABS(C1)+DABS(C2)+DABS(C3)).LT.1.D-9) THEN
        AA = C2
        BB = C1
        CC = C0
        DET = BB**2 - 4*AA*CC
        IF (DET.GT.0.) THEN
          X1 = ROOT(AA,BB,CC,DET,1.D0)
          IF (DABS(X1).GT.1.+TOL) THEN
            X1 = ROOT(AA,BB,CC,DET,-1.D0)
          ENDIF
        ELSE
           IFAIL = 1
        ENDIF

      ELSE
c------------------------------------- 3rd order poly solution
      PI = 2*DASIN(1.D0)
      A0 = C0 / C3
      A1 = C1 / C3
      A2 = C2 / C3
      P = - (A2**2)/3. + A1
      Q = 2.D0/27.D0*(A2**3) - A2 * A1/3.D0 + A0
      DET = (P/3.D0)**3 + (Q/2.D0)**2
      IF (DET.GE.0) THEN
        U=DSIGN(1.D0,-Q/2.+DSQRT(DET))*DABS(-Q/2. + DSQRT(DET))**(1./3.)
        V=DSIGN(1.D0,-Q/2.-DSQRT(DET))*DABS(-Q/2. - DSQRT(DET))**(1./3.)
        X1 =  U + V - A2/3.
        IF (DABS(X1).GE.(1+TOL)) IFAIL = 1
      ELSE
        P = -P
        ANGLE = DSIGN(1.D0,P)*DACOS((Q/2.)/DSQRT(DABS(P)/3.)**3)
        X1 = -2.*DSQRT(DABS(P)/3)*DCOS(ANGLE/3.) - A2/3.
        X2 = -2.*DSQRT(DABS(P)/3.)*DCOS(2*PI/3. - ANGLE/3.) - A2/3.
        X3 = -2.*DSQRT(DABS(P)/3.)*DCOS(2*PI/3. + ANGLE/3.) - A2/3.
      ENDIF
      IF (DABS(X1).GT.DABS(X2)) THEN
        DUM = X1
        X1 = X2
        X2 = DUM
      ENDIF
      IF (DABS(X2).GT.DABS(X3)) THEN
        DUM = X2
        X2 = X3
        X3 = DUM
      ENDIF
      IF (DABS(X1).GT.DABS(X2)) THEN
        DUM = X1
        X1 = X2
        X2 = DUM
      ENDIF
      ENDIF
      IF (DABS(X1).GT.(1.+TOL)) IFAIL=1
      RETURN
      END

************************************************************************
*DECK MAPPING
      SUBROUTINE MAPPING(XX,YY,PSI,CX,CY,XAXIS,A)
C-----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE METRIC COEFFICIENTS NEEDED FOR CASTOR
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE NODES
      USE COMPRI
      USE COMPLO
      USE GAUDSINT
      USE COMMAP
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      COMMON/COMLOCAL/CCHI,SCHI,XCHI,YCHI,XOUT,YOUT,IJCHI
      DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*),MAXERR
      DOUBLE PRECISION  CCHI(4,2*MAXMNODE),SCHI(2*MAXMNODE)
      DOUBLE PRECISION  XCHI(2*MAXMNODE),YCHI(2*MAXMNODE)
      DOUBLE PRECISION  XOUT(2*MAXMNODE),YOUT(2*MAXMNODE)
      INTEGER IJCHI(2*MAXMNODE)
      LOGICAL CHIN
      DOUBLE PRECISION  XPLOT(2*NRMMAX),PPLOT(2*NRMMAX),
     >                  PSIPLOT(2*NRMMAX), RHOPLOT(2*NRMMAX),
     >                  PPARPLOT(2*NRMMAX),PPERPLOT(2*NRMMAX),
     >                  DETPLOT(2*NRMMAX),VJPHIPLOT(2*NRMMAX)
      DOUBLE PRECISION  QPLOT(2*NRMMAX),CUPLOT(2*NRMMAX),
     >                  DQPLOT(2*NRMMAX),BPPLOT(2*NRMMAX)
      DOUBLE PRECISION  JPER(2*NRMMAX), JPAR(2*NRMMAX), JMAG(2*NRMMAX),
     >                  PARDEV(2*NRMMAX),PERDEV(2*NRMMAX)
C--------------------------------------------- VARIABLES FOR VACUUM --
      DOUBLE PRECISION  VX(2*NPMMAX-1),VY(2*NPMMAX-1)
C---------------------------------------------------------------------
      PI = 2.*DASIN(1.D0)
      MAXERR = -1.D20
      FACTAS = 2.
      DR = 1.D-5
      IF (IAS.EQ.1) FACTAS=1.
C--------------------------------------------- NORM FACTORS ----------
C#################
C     CALCULATE SCALED QUANTITIES (NORMALIZED TO R(AXIS)=1, B(AXIS)=1)
C     ALL THE FORMULATION COMMENTED ARE IN S.I. UNIT
C#################

      RADIUS = EPS / (1 + EPS * XAXIS)
      RAXIS = 1

      CALL CALCBTPD(XAXIS,0.D0,0.D0,A,BTOT0,TPER0,DET0)

C#### CPSURF #####
C     CPSURF = PSI(0) / RBPHI(AXIS) / R(AXIS)
      CPSURF = EPS * (1.-DET0) / (1.+EPS*XAXIS) / DSQRT(DABS(A))
C#################

C#### RBPHI* #########
C     RBPHI* = RBPHI / RBPHI(AXIS) = F * (1-DET0) / F(0) / (1-DET)
C     HOWEVER, _F(0) = DSQRT(A) * XGAMMA(0), _F(PS) = DSQRT(A) * XGAMMA(PS)
C     SO RBPHI* = RBSCALE * XGAMMA(PS) / (1-DET), RBSCALE = (1-DET0) / XGAMMA(0)
      RBSCALE = (1.-DET0) / XGAMMA(0.D0)
C#################

C#### RHO ########
C     RHO* = RHO / RHO(AXIS) = RHO * RHOSCALE
C     RHOSCALE = 1 / RHO(AXIS)
      RHOSCALE = TPER0/TEMS(0.D0) * CALCRHO(XAXIS,0.D0)
      RHOSCALE = 1 / RHOSCALE

C#### P* #########
C     P* = MU * RHO  * T / B(AXIS)**2
C        = (1+EPS*XAXIS)**2 (1-DET0)**2 * _RHO * _T / _F(AXIS)**2
C     AND _P(0)/_F(0)**2 = B
C     P* = B * _T * _RHO * PSCALE, PSCALE = (1+EPS*XAXIS)**2 * (1-DET0)**2
      PSCALE = (1.+EPS*XAXIS)**2 * (1-DET0)**2
      
C#### OMG ########
C     OMG* = OMG/(V_A(AXIS)) / R(AXIS)
C     OMG*^2 = MU * RHO * RX**2 / BX**2 * OMG**2
C            = _OMG**2 * _RHOX * (1+EPS*XX)**4 * (1-DELTAX)**2 / _F(AXIS)**2
C            = OMGOT * B * OMGS(PS) * OMGSCALE
C     OMGSCALE = _RHOX * (1+EPS*XX)**4 * (1-DELTAX)**2 / _F(AXIS)**2
      OMGSCALE = 1/RHOSCALE * (1+EPS*XAXIS)**4 * RBSCALE**2
      DOMG0 = 0
      DOMGE = DOMDPSI(1.D0)
      IF (DABS(OMGS(1.D0)) .LT. 1.D-4) THEN
         DOMGE = 0
      ELSE
         DOMGE = DOMGE / DSQRT(OMGS(1.D0)) * DSQRT(OMGSCALE * B * OMGOT)
      ENDIF
      OMGOUT(1) = DSQRT(B * OMGOT * OMGS(0.D0) * OMGSCALE)
      


C#### GRAD*, GRAD' ######
C     SET R* = R / RAXIS
C     GRAD* ~ D/DR* = RAXIS * D/DR, SO GRAD* = RAXIS * GRAD
C     
C     X = (R - R0) / a
c     GRAD' ~ D/DX  = a * D/DR, SO GRAD' = a * GRAD
C
C     GRAD* = RAXIS / a * GRAD' = (1+EPS*XAXIS)/EPS
      ROA = (1.+EPS*XAXIS) / EPS
C#################


C#### GRAD*(PSI*) #######
C     PSI* = PSI / F(AXIS) / RAXIS * (1-DET0)
C
C     GRAD*(PSI*) = RAXIS * 1/a * GRAD'(PSI) / FAXIS / RAXIS * (1-DET0)
C                 = GRAD'(_PSI) / _FAXIS * (1-DET0)
C                 = GRAD'(_PSI) / DSQRT(A) * (1-DET0)
C#######################

C#### |J*| #########
C     |J*| = F(AXIS) / RAXIS**2 * |J|
C          = DSQRT(g11*g22*g33 - g12**2*g33)
C          = Q*R(*)**2*(1-DET) / F*
C###################

C---------------- FILL IN S=0 POINT ----------
      RHOOUT(1) = 1
      PPEROUT(1) = B * TPER0 / RHOSCALE * PSCALE
      PPAROUT(1) = B * TEMS(0.D0) / RHOSCALE * PSCALE
      RBPHIOUT(1) = XGAMMA(0.D0)

c$$$      WRITE(*,*)'ATCORE', RHOOUT(1),PPEROUT(1),PPAROUT(1),RBPHIOUT(1)

C--------------------------------------------- DATA FOR VECTOR PLOT ----
      WRITE(21,*) NR,NCHI,EPS
C------------------------------------------ - Q PROFILE ----------------
C                                           - DQ/DS PROFILE
C                                           - CHI VALUES AT THETA NODES
      DO 10 I = 1, NR-1
        SUMQ = 0.
        SUMQR = 0.
        ZPSI = PSIKN(I)
c        CALL RADMESH(RADPSI(I),ZPSI,DZPSI,DDZPSI)
        PS = ZPSI
        PSIR  = - DPSIKN(I)  /(2.*REAL(NR-1))
        PSIS  = 0.
        PSIRR =   DDPSIKN(I) /(2.*REAL(NR-1))**2
        CS(NR-I+1) = DSQRT(ZPSI)
        IF (DABS(OMGS(ZPSI)) .LT. 1.D-4) THEN
           OMGOUT(NR-I+1) = 0
        ELSE
           OMGOUT(NR-I+1) = DSQRT(OMGS(ZPSI) * OMGSCALE * B * OMGOT)
        ENDIF

C-------CALCULATE CHI BASED ON INTERGRATION
        DO 20 J = 1, NP-1
          N1 = (I-1)*NP + J
          N2 = N1 + 1
          N3 = N2 + NP
          N4 = N1 + NP
          DO 30 K = 1,4
            S = XGAUSS(K)
            CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),-1.D0,S,
     >                  X,XR,XS,XRS,XRR,XSS)
            CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),-1.D0,S,
     >                  Y,YR,YS,YRS,YRR,YSS)
            CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1)
     >                  ,PSI(4*(N4-1)+1),-1.D0+DR,S,PS1,PSR1,PSS1)
            CALL INTERP3(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                   YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                PSI(4*(N4-1)+1),-1.D0+DR,S,X1,XR1,XS1,YR1,YS1,PS1)
            EJAC = XR*YS - XS*YR
            PSYJAC = PSIR*YS - PSIS*YR
            PSXJAC = PSIR*XS - PSIS*XR
            PSX = PSYJAC / EJAC
            PSY = -PSXJAC / EJAC
            SUMDPSI = DSQRT(PSX**2 + PSY**2)
            EJAC1 = XR1*YS1 - XS1*YR1
            PSYJAC1 = PSR1*YS1 - PSS1*YR1
            PSXJAC1 = PSR1*XS1 - PSS1*XR1
            PSX1 = PSYJAC1 / EJAC1
            PSY1 = -PSXJAC1 / EJAC1
            SUMDPSI1 = DSQRT(PSX1**2 + PSY1**2)
            CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
            CALL CALCBTPD(X1,PS1,SUMDPSI1,A,BTOT1,TPER1,DET1)
            DETR = (DET1 - DET) /  DR
            ER = XRR*YS + XR*YRS - XRS*YR - XS*YRR
            BIGR  = (1. + EPS * X)
            SUMQ  = SUMQ -WGAUSS(K)*EJAC/( BIGR * DABS(PSIR))/(1-DET)
            SUMQR = SUMQR + PSIRR * EJAC / ((PSIR**2)*BIGR)* WGAUSS(K)
     >                    /  (1-DET)
            
            SUMQR = SUMQR - ER / (BIGR*PSIR)               * WGAUSS(K)
     >                    /  (1-DET)
            SUMQR = SUMQR + EJAC*EPS*XR/((BIGR**2)*PSIR)  * WGAUSS(K)
     >                    /  (1-DET)
            SUMQR = SUMQR - DETR*EJAC / (BIGR*DABS(PSIR))/(1-DET)**2
     >              *WGAUSS(K)
C            IF ((K.EQ.1).AND.(J.EQ.1)) WRITE(*,*) XRS, YRS
   30     CONTINUE
C---------ON THETA GRID POINTS
          CCHI(1,(I-1)*NP+J+1) = SUMQ
          CCHI(2,(I-1)*NP+J+1) = SUMQR
          CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),-1.D0,1.D0,
     >                X,XR,XS,XRS,XRR,XSS)
          CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),-1.D0,1.D0,
     >                Y,YR,YS,YRS,YRR,YSS)
          CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1)
     >                ,PSI(4*(N4-1)+1),-1.D0+DR,1.D0,PS1,PSR1,PSS1)
          CALL INTERP3(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                PSI(4*(N4-1)+1),-1.D0+DR,1.D0,
     >                X1,XR1,XS1,YR1,YS1,PS1)

          EJAC = XR*YS - XS*YR
          ER = XRR*YS + XR*YRS - XRS*YR - XS*YRR
          BIGR = (1. + EPS * X )
          PSYJAC = PSIR*YS - PSIS*YR
          PSXJAC = PSIR*XS - PSIS*XR
          PSX = PSYJAC / EJAC
          PSY = -PSXJAC / EJAC
          SUMDPSI = DSQRT(PSX**2 + PSY**2)
          EJAC1 = XR1*YS1 - XS1*YR1
          PSYJAC1 = PSR1*YS1 - PSS1*YR1
          PSXJAC1 = PSR1*XS1 - PSS1*XR1
          PSX1 = PSYJAC1 / EJAC1
          PSY1 = -PSXJAC1 / EJAC1
          SUMDPSI1 = DSQRT(PSX1**2 + PSY1**2)
          CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
          CALL CALCBTPD(X1,PS1,SUMDPSI1,A,BTOT1,TPER1,DET1)
          DETS = (DET1 - DET) / DR
          ZSUMQ  = - EJAC / ( BIGR * DABS(PSIR))/(1-DET)
          ZSUMQR = + PSIRR * EJAC / (PSIR**2 *BIGR)/(1-DET)
          ZSUMQR =  ZSUMQR - ER / (BIGR*PSIR)/(1-DET)
          ZSUMQR =  ZSUMQR + EJAC*EPS*XR/((BIGR**2) * PSIR)
     >              / (1-DET)
          ZSUMQR = ZSUMQR - DETS*EJAC / (BIGR*DABS(PSIR))/(1-DET)**2
          CCHI(3,(I-1)*NP+J+1) = ZSUMQ
          CCHI(4,(I-1)*NP+J+1) = ZSUMQR
   20   CONTINUE
C-------AT START POINT WHERE THETA = 0
        CCHI(1,(I-1)*NP+1) = 0.
        CCHI(2,(I-1)*NP+1) = 0.
        N1 = (I-1)*NP + 1
        N2 = N1 + 1
        N3 = N2 + NP
        N4 = N1 + NP
        CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),-1.D0,-1.D0,
     >              X,XR,XS,XRS,XRR,XSS)
        CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),-1.D0,-1.D0,
     >              Y,YR,YS,YRS,YRR,YSS)
        CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1)
     >       ,PSI(4*(N4-1)+1),-1.D0+DR,-1.D0,PS1,PSR1,PSS1)
        CALL INTERP3(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >       YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >       PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >       PSI(4*(N4-1)+1),-1.D0+DR,-1.D0,X1,XR1,XS1,YR1,YS1,PS1)
        EJAC = XR*YS - XS*YR
        PSYJAC = PSIR*YS - PSIS*YR
        PSXJAC = PSIR*XS - PSIS*XR
        PSX = PSYJAC / EJAC
        PSY = -PSXJAC / EJAC
        SUMDPSI = DSQRT(PSX**2 + PSY**2)
        EJAC1 = XR1*YS1 - XS1*YR1
        PSYJAC1 = PSR1*YS1 - PSS1*YR1
        PSXJAC1 = PSR1*XS1 - PSS1*XR1
        PSX1 = PSYJAC1 / EJAC1
        PSY1 = -PSXJAC1 / EJAC1
        SUMDPSI1 = DSQRT(PSX1**2 + PSY1**2)
        CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
        CALL CALCBTPD(X1,PS1,SUMDPSI1,A,BTOT1,TPER1,DET1)
        DETS = (DET1 - DET) / DR
        ER = XRR*YS + XR*YRS - XRS*YR - XS*YRR
        BIGR = (1. + EPS * X )
        ZSUMQ = - EJAC / (BIGR * DABS(PSIR))/(1-DET)
        ZSUMQR =  + PSIRR * EJAC / (PSIR**2 *BIGR)/(1-DET)
        ZSUMQR = ZSUMQR - ER / (BIGR*PSIR)/(1-DET)
        ZSUMQR = ZSUMQR + EJAC*EPS*XR/((BIGR**2) * PSIR)/(1-DET)
        ZSUMQR = ZSUMQR - DETS*EJAC / (BIGR*DABS(PSIR))/(1-DET)**2
        CCHI(3,(I-1)*NP+1) =  ZSUMQ
        CCHI(4,(I-1)*NP+1) =  ZSUMQR

c-------QS = EPS * F * CCHI(1,*)
        QS(NR-I+1) =  0.5*FACTAS * SUMQ * EPS * SQRT(DABS(A))*XGAMMA(PS)
        DQS(NR-I+1)= SUMQR*XGAMMA(PS) - SUMQ*DGDPSI(PS) * PSIR
        DQS(NR-I+1)=  0.5*FACTAS * DQS(NR-I+1) * EPS
        DQS(NR-I+1)=  DQS(NR-I+1) * DSQRT(DABS(A))
   10 CONTINUE
      CALL CALCBTPD(XAXIS,0.D0,0.D0,A,BTOT0,TPER0,DET0)

      QS(1) = XGAMMA(0.D0) * EPS * PI/(2.*DSQRT(CX*CY)*(1.+EPS*XAXIS))
c$$$      WRITE(*,*)'cxcy ', CX, CY
      QS(1) = QS(1) * DSQRT(DABS(A)) / (1.D0-DET0)

c$$$      WRITE(*,*)'Q0,DET0    :',QS(1)/PI, DET0
      WRITE(22,*) QS(1) / PI
      DQS(1) = 0.
      DO 40 I = 1, NR-1
        DO 50 J = 1, NP
          DUM = CCHI(1,I*NP)
          NO = (I-1)*NP+J
          CCHI(1,NO) = FLOAT(1+IAS)*PI*CCHI(1,NO) / DUM
          DUM2 = CCHI(2,I*NP)
          CCHI(2,NO) = FLOAT(1+IAS)*PI*CCHI(2,NO) / DUM
          CCHI(3,NO) = FLOAT(1+IAS)*PI*CCHI(3,NO) / DUM
          CCHI(4,NO) = FLOAT(1+IAS)*PI*CCHI(4,NO) / DUM
C------------------------------ FINAL VALUES OF CHI --------------------
          PS = PSIKN(I)
          PSIR  = - DPSIKN(I)  /(2.*REAL(NR-1))
          QQ  = QS(NR-I+1)
          DQQ = DQS(NR-I+1)
          F  = XGAMMA(PS)
          DF = DGDPSI(PS)  * PSIR
          CCHI(2,NO)= (DQQ/QQ + DF/F) * CCHI(1,NO) - CCHI(2,NO)
          CCHI(4,NO)= (DQQ/QQ + DF/F) * CCHI(3,NO) - CCHI(4,NO)
 50       CONTINUE     
   40 CONTINUE

c      IF (NPR1.NE.0) THEN
c        CALL PRARR1(' CHI : ',CCHI,4*NR*NP,203)
c      ENDIF
      DO 45 I=1,NR
         ZPSI = PSIKN(I)
         PSIR  = - DPSIKN(I)  /(2.*REAL(NR-1))
         DQS(I) = DQS(I) * (2.*REAL(NR-1))
c         DQS(I) = DQS(I) / PSIR * 2 * DSQRT(ZPSI)
        QS(I) = QS(I) / PI
        QPLOT(NR+1-I) = QS(I)
        DQS(I) = DQS(I) / PI
        DQPLOT(NR+1-I) = DQS(I)
   45 CONTINUE
      QS0 = QS(1)
      QAXIS = QS(1)
      WRITE(20,*)
      WRITE(20,31) QS(1)
      WRITE(20,32) QS(NR)
      WRITE(22,*) QS(NR)
   31 FORMAT('  Q ON AXIS = ',f7.4)
   32 FORMAT('  Q AT BOUNDARY = ',f7.4)

      DQEC = DQS(NR)
C--------------------------- DETERMINE POSITIONS OF EQUIDISDTANT CHI'S --
C                            AND CALCULATE MATRIX ELEMENTS -------------
   54 FORMAT('NUMBER OF CHI VALUES : ',I3)
      JS0 = NR - 1
      IF (IAS.EQ.0) THEN
        DO 55 J=1,NCHI
          CHIKN(J) =  PI * REAL(J-1)/REAL(NCHI-1)
          CHI(J)   =  PI * REAL(J-1)/REAL(NCHI-1)
   55   CONTINUE
      ELSE
        DO 56 J=1,NCHI
          CHIKN(J) = 2. * PI * REAL(J-1)/REAL(NCHI)
          CHI(J)   = 2. * PI * REAL(J-1)/REAL(NCHI)
   56   CONTINUE
      ENDIF
C      CALL PRARR1(' CHI : ',CHI,NCHI,201)
C      CALL PRARR2(' CCHI : ',' ',CCHI,4,NR*NP,4,203)
C      CALL PRARR2(' XX : ',' ',XX,4,NR*NP,4,203)
C      CALL PRARR2(' YY : ',' ',YY,4,NR*NP,4,203)
      DO 60 I=1,NR-1
        CALL RADMESH(RADPSI(I),ZPSI,DZPSI,DDZPSI)
        PSIR  = - DPSIKN(I)  /(2.*REAL(NR-1))
C-------------------------- FIRST POINT IS KNOWN -----------------------
        NO = (I-1)*NCHI + 1
        K = 1
        S = -1.
        SCHI((I-1)*NCHI+J) = S
        IJCHI((I-1)*NCHI+J) = K
        N1 =  (I-1)*NP + K
        N2 = N1 + 1
        N3 = N2 + NP
        N4 = N1 + NP
        CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >              -1.D0,S,XCHI(NO),XR,XS,XRS,XRR,XSS)
        CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >              -1.D0,S,YCHI(NO),YR,YS,YRS,YRR,YSS)
        CALL INTERP(CCHI(1,N1),CCHI(1,N2),CCHI(1,N3),CCHI(1,N4),
     >              -1.D0,S,DCHI,CHIR,CHIS,CHIRS,CHIRR,CHISS)
C--------------------------------------------- VACUUM DATA ---------
        IF (I.EQ.1) THEN
          VX(1) = XCHI(NO)
          VY(1) = YCHI(NO)
        ENDIF
C-----------------------------------------------------------------------
        EJAC  = (XR*YS-XS*YR)
        EJAC2 = EJAC**2
C----------------------------- DATA FOR VECTOR PLOT TO FILE 21 -----
        PSIX = PSIR * YS / EJAC
        PSIY = - PSIR * XS / EJAC
        CHIX = (CHIR * YS - CHIS * YR) / EJAC
        CHIY = (-CHIR * XS + CHIS * XR)/ EJAC
C        WRITE(21,61) DSQRT(ZPSI),DCHI,XCHI(NO),YCHI(NO),
C     >              PSIX,PSIY,CHIX,CHIY
C----------------------------------------------------------------------
        GRPS2 = PSIR**2 * (XS**2 + YS**2) / EJAC2
        NOG = (NR-1)*NCHI - I*NCHI + 1
        GEM11(NCHI+NOG) = GRPS2 / DABS(A) * (1-DET0)**2
        GRPGRC = ( CHIR * PSIR * (XS**2 + YS**2) -
     >             CHIS * PSIR * (XR*XS + YR*YS) ) / EJAC2
        GEM12(NCHI+NOG) = GRPGRC / DSQRT(DABS(A)) * ROA * (1-DET0)
        GEM33(NCHI+NOG) =((1.+EPS*XCHI(NO))/(1.+EPS*XAXIS))**2
        XOUT(NCHI+NOG) = XCHI(NO)
        YOUT(NCHI+NOG) = YCHI(NO)
C-------------------EQUILIBRIUM QUANTITIES---------------------------
        SUMDPSI = DSQRT(PSIX**2+PSIY**2)
        CALL CALCBTPD(XCHI(NO),CS(NR-I+1)**2,SUMDPSI,A,BTOT,TPER,DET)
        RHO0 = CALCRHO(XCHI(NO),CS(NR-I+1)**2)
        IF (TEMS(CS(NR-I+1)**2).NE.0.D0) THEN
           RHO0 = TPER/TEMS(CS(NR-I+1)**2)*RHO0
        ENDIF
        RHOOUT(NCHI+NOG) = RHO0 * RHOSCALE
        PPAROUT(NCHI+NOG) = B * TEMS(CS(NR-I+1)**2) * RHO0 * PSCALE
        PPEROUT(NCHI+NOG) = B * TPER * RHO0 * PSCALE
        RBPHIOUT(NCHI+NOG) = XGAMMA(CS(NR-I+1)**2)/(1-DET) * RBSCALE
C-----------------------DERIVTIVES AT BOUNDARY------------------------
C------------------------------------ CHECK JACOBIAN -----------------
        GRCHI2 = (CHIX**2 + CHIY**2) * ROA**2                                  
        DUM1 = XGAMMA(PSIKN(I))**2/(1-DET)**2*(1-DET0)**2/
     >       (QS(NR-I+1)**2 * GEM33(NCHI+NOG))                          
        DUM2 = GEM11(NCHI+NOG) * GRCHI2                 
        DUM3 = GEM12(NCHI+NOG)                                        
        DUM4 = (DUM2 - DUM3*DUM3)
        ERRJ = DABS(DUM4-DUM1)
        IF (ERRJ.GT.MAXERR) THEN
           MAXERR=ERRJ
           IERR = I
           JERR = 1
           SERR = DSQRT(ZPSI)
           CERR = DCHI
        ENDIF
C        WRITE(20,*) GEM11(NCHI+NOG),GEM12(NCHI+NOG),ERRJ,DET
C---------------------------------------------------------------------
        JBASE = 2
        DO 70 K = 1,NP-1
          CHIN =.FALSE.
          DO 80 J = JBASE,NCHI
            ZCHI = CHIKN(J)
            NO = (I-1)*NCHI + J
            IF ((((CCHI(1,(I-1)*NP+K).LE.ZCHI).AND.(CCHI(1,(I-1)*NP+K+1)
     >           .GE.ZCHI)) ).OR. 
     >           ((J.EQ.NCHI).AND.(K.EQ.NP-1).AND.(IAS.EQ.0))) THEN
              CHIN = .TRUE.
              NOM = (I-1)*NP + K
              NOP = NOM + 1
              A3 = (CCHI(1,NOM)+CCHI(3,NOM)-CCHI(1,NOP)+CCHI(3,NOP))/4.
              A2 = (- CCHI(3,NOM) + CCHI(3,NOP))/4.
           A1=(-3*CCHI(1,NOM)-CCHI(3,NOM)+3*CCHI(1,NOP)-CCHI(3,NOP))/4.
           A0=( 2*CCHI(1,NOM)+CCHI(3,NOM)+2*CCHI(1,NOP)-CCHI(3,NOP))/4.
     >       - ZCHI
              CALL SOLVP3(A0,A1,A2,A3,S,S2,S3,IFAIL)
	      IF ((IAS.EQ.0).AND.(J.EQ.NCHI)) THEN
	        S = 1.0
		IFAIL=0
	      ENDIF	
              IF (IFAIL.EQ.0) THEN
                SCHI((I-1)*NCHI+J) = S
                IJCHI((I-1)*NCHI+J) = K
                N1 = (I-1)*NP + K
                N2 = N1 + 1
                N3 = N2 + NP
                N4 = N1 + NP
                CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                      -1.D0,S,XCHI(NO),XR,XS,XRS,XRR,XSS)
                CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                      -1.D0,S,YCHI(NO),YR,YS,YRS,YRR,YSS)
                CALL INTERP(CCHI(1,N1),CCHI(1,N2),CCHI(1,N3),CCHI(1,N4),
     >                      -1.D0,S,DCHI,CHIR,CHIS,CHIRS,CHIRR,CHISS)
C--------------------------------------------- VACUUM DATA ---------
                IF (I.EQ.1) THEN
                  VX(J) = XCHI(NO)
                  VY(J) = YCHI(NO)
                ENDIF
C-----------------------------------------------------------------------
                EJAC  = (XR*YS-XS*YR)
                EJAC2 = EJAC**2
C-------------------------------- DATA FOR VECTOR PLOT TO FILE 21 -----
                PSIX = PSIR * YS / EJAC
                PSIY = - PSIR * XS / EJAC
                CHIX = (CHIR * YS - CHIS * YR) / EJAC
                CHIY = (-CHIR * XS + CHIS * XR)/ EJAC
                WRITE(21,61) DSQRT(ZPSI),DCHI,XCHI(NO),YCHI(NO),
     >                        PSIX,PSIY,CHIX,CHIY
C----------------------------------------------------------------------
                GRPS2 = PSIR**2 * (XS**2 + YS**2) / EJAC2
                NOG = (NR-1)*NCHI - I*NCHI + J
                GEM11(NCHI+NOG) = GRPS2 / DABS(A) * (1-DET0)**2
                GRPGRC = ( CHIR * PSIR * (XS**2 + YS**2) -
     >                     CHIS * PSIR * (XR*XS + YR*YS) ) / EJAC2
                GEM12(NCHI+NOG) = GRPGRC / DSQRT(DABS(A)) * ROA*(1-DET0)
                GEM33(NCHI+NOG) =((1.+EPS*XCHI(NO))/(1.+EPS*XAXIS))**2
                XOUT(NCHI+NOG) = XCHI(NO)
                YOUT(NCHI+NOG) = YCHI(NO)
                SUMDPSI = DSQRT(PSIX**2 + PSIY**2)
                CALL CALCBTPD(XCHI(NO),PSIKN(I),SUMDPSI,A,BTOT,TPER,DET)
                RHO0 = CALCRHO(XCHI(NO),CS(NR-I+1)**2)
                IF (TEMS(CS(NR-I+1)**2).NE.0.D0) THEN
                   RHO0 = TPER/TEMS(CS(NR-I+1)**2)*RHO0
                ENDIF
                RHOOUT(NCHI+NOG)  =RHO0 * RHOSCALE
                PPAROUT(NCHI+NOG) =B*TEMS(CS(NR-I+1)**2)* RHO0 * PSCALE
                PPEROUT(NCHI+NOG) =B * TPER * RHO0 * PSCALE
                RBPHIOUT(NCHI+NOG)=XGAMMA(CS(NR-I+1)**2)/(1-DET)*RBSCALE
C------------------------------------ CHECK JACOBIAN -----------------        
                GRCHI2 = (CHIX**2 + CHIY**2) * ROA**2
                DUM1 = XGAMMA(PSIKN(I))**2 /(1-DET)**2*(1-DET0)**2 /                                   
     >             (QS(NR-I+1)**2 * GEM33(NCHI+NOG))                          
                DUM2 = GEM11(NCHI+NOG) * GRCHI2                 
                DUM3 = GEM12(NCHI+NOG)                                        
                DUM4 = (DUM2 - DUM3*DUM3)
		ERRJ = DABS(DUM4-DUM1)/DABS(DUM4)
                IF (ERRJ.GT.MAXERR) THEN
                  MAXERR=ERRJ
                  IERR = I
                  JERR = J
                  SERR = DSQRT(ZPSI)
                  CERR = DCHI
                ENDIF
C               WRITE(20,*) GEM11(NCHI+NOG),GEM12(NCHI+NOG),ERRJ,DET
               IF (ERRJ.GT.1.D-2) WRITE(20,*) ERRJ, DCHI, SERR
C---------------------------------------------------------------------
              ELSE
                WRITE(*,*) 'ERROR IN SOLVP3 I,J,K : ',I,J,K,S,s2,s3
                WRITE(*,*) A0,A1,A2,A3,ZCHI
                WRITE(*,*) CCHI(1,(I-1)*NP+K),CCHI(1,(I-1)*NP+K+1)
              ENDIF
            ELSEIF (CHIN) THEN
              JBASE = J
              GOTO 70
            ENDIF
   80     CONTINUE
   70   CONTINUE
   60 CONTINUE
   61 FORMAT(8E16.8)  
   62 FORMAT(' MAX. ERROR IN JACOBIAN AFTER MAPPING : ',
     >       1PE10.3,0P2F10.6,2I4)
      WRITE(20,*)
      WRITE(20,*) '***************************************************'
      WRITE(20,62) MAXERR,SERR,CERR,IERR,JERR
      WRITE(20,*) '***************************************************'

C-------------------- WRITE GEOMETRIC QUANTITIES TO TAPE12 -----------
      NMAP = 12
      OPEN(NMAP)
C
C HEADINGS
      WRITE(NMAP,FMT='(A3)')  '1.1'
      WRITE(NMAP,FMT='(A11)') 'Equilibrium'
      WRITE(NMAP,FMT='(A16)') 'no poloidal flow'
      WRITE(NMAP,FMT='(A7)') 'tokamak'
      WRITE(NMAP,FMT='(L1)')  (IAS.LE.0)

C GLOBAL VARIABLES
      WRITE(NMAP,FMT='(2(E15.8,1X))') EPS, 0.0
      WRITE(NMAP,FMT='(E15.8)') CPSURF
C NUMBER OF GRIDS
      WRITE(NMAP,FMT='(2(I5,1X))') JS0+1, NCHI
C Q PROFILE
      DO JS=1,JS0+1
         WRITE(NMAP,FMT='(4(E15.8,1X))')
     >        CS(JS), 0.D0, QS(JS), 0.D0
      ENDDO
C ANGLE GRID
      WRITE(NMAP,FMT='(5(E15.8,1X))') (CHI(JS),JS=1,NCHI)    
C 2D PROFILES
      GEM11(1:NCHI) = 0.0
      GEM33(1:NCHI) = 1.0
      GEM12(1:NCHI) = GEM12(NCHI+1:2*NCHI)
      DO I=1,(JS0+1)*NCHI
         WRITE(NMAP,FMT='(8(E15.8,1X))')
     >        GEM11(I), GEM12(I), GEM33(I),RHOOUT(I),
     >        OMGOUT(INT(I/NCHI)+1), PPAROUT(I), RBPHIOUT(I), 0.D0
      ENDDO
         
c$$$      WRITE(NMAP,8) JS0
c$$$      WRITE(NMAP,8) NCHI
c$$$      WRITE(NMAP,7) CPSURF,RADIUS
c$$$      WRITE(NMAP,9) RAXIS
c$$$
c$$$      WRITE(NMAP,6) (CS(JS),JS=1,JS0+1)
c$$$C      WRITE(NMAP,*)
c$$$      WRITE(NMAP,6) (CHI(JS),JS=1,NCHI)
c$$$C      WRITE(NMAP,*)
c$$$      WRITE(NMAP,6) (QS(JS),JS=1,JS0+1)
c$$$c      WRITE(NMAP,7) DQS(1),DQEC      
c$$$      
c$$$C      WRITE(NMAP,*) '===== GEM11 ====='
c$$$      WRITE(NMAP,6) (GEM11(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$C      WRITE(NMAP,*) '===== GEM12 ====='
c$$$      WRITE(NMAP,6) (GEM12(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$C      WRITE(NMAP,*) '===== GEM33 ====='
c$$$      WRITE(NMAP,6) (GEM33(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$      
c$$$C================= ORIGINAL HELENA : P(PSI),F(PSI) =====================
c$$$C      WRITE(NMAP,6) (P0(JS),JS=1,JS0+1)
c$$$C      WRITE(NMAP,7) DP0,DPE
c$$$C      WRITE(NMAP,6) (RBPHI(JS),JS=1,JS0+1)
c$$$C      WRITE(NMAP,7)  DRBPHI0,DRBPHIE
c$$$C====== HELENA+ATF : P_{PAR,PER}(PSI,CHI), DP0, DPE
c$$$C                    RBPHI(PSI,CHI), DRBPHI0, DRBPHIE
c$$$C                    RHO(PSI,CHI), DRHO0, DRHOE
c$$$C      WRITE(NMAP,*) '===== DENSITY ====='
c$$$      WRITE(NMAP,6) (RHOOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$      WRITE(NMAP,9) RHOOUT(1)
c$$$C      WRITE(NMAP,*) '===== PPAR ====='
c$$$      WRITE(NMAP,6) (PPAROUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$      WRITE(NMAP,9) PPAROUT(1)
c$$$C      WRITE(NMAP,*) '===== PPER ====='
c$$$      WRITE(NMAP,6) (PPEROUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$      WRITE(NMAP,9) PPEROUT(1)
c$$$C      WRITE(NMAP,*) '===== RBPHI ====='
c$$$      WRITE(NMAP,6) (RBPHIOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$      WRITE(NMAP,9) RBPHIOUT(1)
c$$$c      WRITE(NMAP,*) '===== OMEGA ====='
c$$$      WRITE(NMAP,6) (OMGOUT(JS), JS=1, JS0+1)
c$$$      WRITE(NMAP,7) DOMG0, DOMGE
c$$$c      WRITE(NMAP,*)
c$$$
c$$$
c$$$C----------------------------------------- ADDITIONAL DATA FOR VACUUM --
c$$$      WRITE(NMAP,6) (VX(JS),JS=1,NCHI)
c$$$      WRITE(NMAP,6) (VY(JS),JS=1,NCHI)
c$$$      WRITE(NMAP,9) EPS
c$$$c      WRITE(NMAP,6) (XOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$c      WRITE(NMAP,6) (YOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c$$$C----------------------------------------- write profiles to vector file
c$$$C      WRITE(21,11)  (P0(JS),RBPHI(JS),QS(JS),JS=1,JS0+1)
c$$$C      WRITE(21,11)  CPSURF
c$$$C      CLOSE(21)
    6 FORMAT(4E16.8)
    7 FORMAT(2E16.8)
    8 FORMAT(I5)
    9 FORMAT(E16.8)
   11 FORMAT(3E16.8)

 
 998  IF (NPL1.NE.0) THEN     
      CALL PLOTM(XCHI,YCHI,NR,NCHI,IAS, 33)
C---------------------------------------- PLOT PROFILES --------------
      VJPHI0 = 2.*(CX+CY)/ (1+EPS*XAXIS)
      CALL CALCBTPD(XAXIS,0.D0,0.0D0,A,BTOT0,TPER0,DET0)
      RHO0 = CALCRHO(XAXIS, 0.D0) * TPER0 / TEMS(0.D0)
      PPARPLOT0   = RHO0 * TEMS(0.D0)
      PPERPLOT0   = RHO0 * TPER0
      DO 100 I=1,NR
        IF (IAS.EQ.1) THEN
          ILEFT = (I-1)*NP + (NP+1)/2 
	ELSE 
	  ILEFT = (I-1)*NP + NP
	ENDIF  
        IRIGHT= (I-1)*NP + 1
        XPLOT(I) = XX(1,ILEFT)
        XPLOT(2*NR-I) = XX(1,IRIGHT)
        PSIPLOT(I) = PSIKN(I)
        PSIPLOT(2*NR-I) = PSIKN(I)
C-------LEFT-----
        N1 = ILEFT-1
        N2 = ILEFT
        N3 = ILEFT+NP
        N4 = ILEFT+NP-1
        RR = -1.D0
        SS = 1.D0
        CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),
     >       XX(1,N4),RR,SS,X,XR,XS,XRS,XRR,XSS)
        CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),
     >       YY(1,N4),RR,SS,Y,YR,YS,YRS,YRR,YSS)
        CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >       PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >       RR,SS,PS,PSR,PSS,PSRS,PSRR,PSSS)
        XJAC0 = XR * YS - XS * YR
        XJACR = XRR * YS + XR * YRS - XRS * YR - XS * YRR
        XJACS = XRS * YS + XR * YSS - XSS * YR - XS * YRS
        PYJAC0= PSR * YS - PSS * YR
        PXJAC0= PSR * XS - PSS * XR
        PYJACR = PSRR * YS + PSR * YRS  - PSRS * YR - PSS * YRR
        PYJACS = PSRS * YS + PSR * YSS  - PSSS * YR - PSS * YRS
        PXJACR = PSRR * XS + PSR * XRS  - PSRS * XR - PSS * XRR
        PXJACS = PSRS * XS + PSR * XSS  - PSSS * XR - PSS * XRS
        PSX0  = PYJAC0 / XJAC0
        PSY0  = - PXJAC0 / XJAC0
        SUMDPSI = DSQRT(PSX0**2 + PSY0**2)
        IF (I.EQ.NR) SUMDPSI = 0.0D0
        RY = - XS / XJAC0
        RX =   YS / XJAC0
        SY =   XR / XJAC0
        SX = - YR / XJAC0
        PSIXX =       (PYJACR - PYJAC0 * XJACR / XJAC0) / XJAC0 * RX
        PSIXX = PSIXX+(PYJACS - PYJAC0 * XJACS / XJAC0) / XJAC0 * SX
        PSIYY =      -(PXJACR - PXJAC0 * XJACR / XJAC0) / XJAC0 * RY
        PSIYY = PSIYY-(PXJACS - PXJAC0 * XJACS / XJAC0) / XJAC0 * SY
        VJPHI = (PSIXX + PSIYY - EPS * PSX0 / (1+EPS * X))/(1+EPS*X)
        IF (DABS(PSIPLOT(I)).LT.1.D-8) VJPHI = VJPHI0
        VJPHIPLOT(I) = VJPHI
        CALL CALCRJPHIC(XPLOT(I),PSIPLOT(I),SUMDPSI,A,AJPAR,AJPER,AJMAG,
     >       APARDEV, APERDEV)
        JPAR(I) = AJPAR * DABS(A) / (1+EPS*X)
        JPER(I) = AJPER * DABS(A) / (1+EPS*X)
        JMAG(I) = AJMAG * DABS(A) / (1+EPS*X)
        PARDEV(I) = APARDEV * DABS(A) / (1+EPS*X)
        PERDEV(I) = APERDEV * DABS(A) / (1+EPS*X)
        CALL CALCBTPD(XPLOT(I),PSIPLOT(I),SUMDPSI,A,BOT,TPER,DET)
        RHOPLOT(I) = CALCRHO(XPLOT(I),PSIPLOT(I))
        RHOPLOT(I) = RHOPLOT(I) * TPER / TEMS(PSIPLOT(I))
        PPARPLOT(I) = RHOPLOT(I) * TEMS(PSIPLOT(I)) * DABS(A)*B
        PPERPLOT(I) = RHOPLOT(I) * TPER * DABS(A)*B
        DETPLOT(I) = DET
        BPPLOT(I) = SUMDPSI/(1+EPS*X)
C-------RIGHT----
        N1 = IRIGHT
        N2 = IRIGHT + 1
        N3 = IRIGHT + NP +1
        N4 = IRIGHT + NP 
        RR = -1.D0
        SS = -1.D0
        CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),
     >       XX(1,N4),RR,SS,X,XR,XS,XRS,XRR,XSS)
        CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),
     >       YY(1,N4),RR,SS,Y,YR,YS,YRS,YRR,YSS)
        CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >       PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >       RR,SS,PS,PSR,PSS,PSRS,PSRR,PSSS)
        XJAC0 = XR * YS - XS * YR
        XJACR = XRR * YS + XR * YRS - XRS * YR - XS * YRR
        XJACS = XRS * YS + XR * YSS - XSS * YR - XS * YRS
        PYJAC0= PSR * YS - PSS * YR
        PXJAC0= PSR * XS - PSS * XR
        PYJACR = PSRR * YS + PSR * YRS  - PSRS * YR - PSS * YRR
        PYJACS = PSRS * YS + PSR * YSS  - PSSS * YR - PSS * YRS
        PXJACR = PSRR * XS + PSR * XRS  - PSRS * XR - PSS * XRR
        PXJACS = PSRS * XS + PSR * XSS  - PSSS * XR - PSS * XRS
        PSX0  = PYJAC0 / XJAC0
        PSY0  = - PXJAC0 / XJAC0
        SUMDPSI = DSQRT(PSX0**2 + PSY0**2)
        IF (I.EQ.NR) SUMDPSI = 0.0D0
        RY = - XS / XJAC0
        RX =   YS / XJAC0
        SY =   XR / XJAC0
        SX = - YR / XJAC0
        PSIXX =       (PYJACR - PYJAC0 * XJACR / XJAC0) / XJAC0 * RX
        PSIXX = PSIXX+(PYJACS - PYJAC0 * XJACS / XJAC0) / XJAC0 * SX
        PSIYY =      -(PXJACR - PXJAC0 * XJACR / XJAC0) / XJAC0 * RY
        PSIYY = PSIYY-(PXJACS - PXJAC0 * XJACS / XJAC0) / XJAC0 * SY
        VJPHI = (PSIXX + PSIYY - EPS * PSX0 / (1+EPS * X))/(1+EPS*X)
        IF (DABS(PSIPLOT(2*NR-I)).LT.1.D-8) VJPHI = VJPHI0
        IF (I.EQ.NR) SUMDPSI = 0.0D0
        VJPHIPLOT(2*NR-I) = VJPHI
        CALL CALCBTPD(XPLOT(2*NR-I),PSIPLOT(I),SUMDPSI,A,BOT,TPER,DET)
        CALL CALCRJPHIC(XPLOT(2*NR-I),PSIPLOT(I),SUMDPSI,A,
     >       AJPAR,AJPER,AJMAG, APARDEV, APERDEV)
        JPAR(2*NR-I) = AJPAR * DABS(A) / (1+EPS*X)
        JPER(2*NR-I) = AJPER * DABS(A) / (1+EPS*X)
        JMAG(2*NR-I) = AJMAG * DABS(A) / (1+EPS*X)
        PARDEV(2*NR-I) = APARDEV * DABS(A) / (1+EPS*X)
        PERDEV(2*NR-I) = APERDEV * DABS(A) / (1+EPS*X)
        RHOPLOT(2*NR-I) = CALCRHO(XPLOT(2*NR-I),PSIPLOT(I))
        RHOPLOT(2*NR-I) = RHOPLOT(2*NR-I) * TPER / TEMS(PSIPLOT(I))
        PPARPLOT(2*NR-I) = RHOPLOT(2*NR-I) * TEMS(PSIPLOT(I))*DABS(A)*B
        PPERPLOT(2*NR-I) = RHOPLOT(2*NR-I) * TPER*DABS(A)*B
        DETPLOT(2*NR-I) = DET
        BPPLOT(2*NR-I) = SUMDPSI/(1+EPS*X)
        QPLOT(2*NR-I) = QPLOT(I)
        DQPLOT(2*NR-I) = DQPLOT(I)
  100 CONTINUE
      
C      CALL PRARR1('XPLOT : ',XPLOT,2*NR-1,203)                            
C      CALL PRARR1('PPLOT : ',PPLOT,2*NR-1,203)                            
C      CALL PRARR1('PSIPLOT : ',PSIPLOT,2*NR-1,203)                        
C      CALL PRARR1('QPLOT : ',QPLOT,2*NR-1,203)                            
 
      IF (XPLOT(1).LT.-0.999)       XPLOT(1)     =-0.999
      IF (XPLOT(2*NR-1).GT.0.999)   XPLOT(2*NR-1)= 0.999
      
      DO I = 1, NR
         WRITE(34,*)PSIPLOT(I),QPLOT(I)
      ENDDO
c      WRITE(35,*)'#  X  Q  DQ/DS  PSI  RHO  P(PAR)  P(PER)  DET JPHI'
      DO I = 1, NR * 2-1
         WRITE(35,*)XPLOT(I), QPLOT(I), DQPLOT(I), PSIPLOT(I), 
     >              RHOPLOT(I),PPARPLOT(I), PPERPLOT(I), DETPLOT(I),
     >              VJPHIPLOT(I), 
     >              XGAMMA(PSIPLOT(I))*DSQRT(DABS(A))/(1-DETPLOT(I)),
     >              BPPLOT(I)
c         WRITE(*,*) DSQRT(PSIPLOT(I)), PPARPLOT(I)/A
      ENDDO
      
c$$$      DO I = 1, NR*2-1
c$$$         WRITE(391,*) XPLOT(I), VJPHIPLOT(I), JPAR(I), JPER(I), JMAG(I),
c$$$     >        PARDEV(I),PERDEV(I)
c$$$      ENDDO

C SCREEN OUT Q PROFILE INFO
      WRITE(*,*) '********** Q PROFILE **********'
      WRITE(*,*) 'Q0  ', QS(1)
      QMIN = 1.D10
      QMINPOS = 0.D0
      DO I = 1, NR
         IF (QS(I).LT.QMIN) THEN
            QMIN = QS(I)
            QMINPOS = CS(I)**2
         ENDIF
      ENDDO
      WRITE(*,*) 'QMIN', QMIN
      WRITE(*,*) '     AT PSI = ', QMINPOS
      WRITE(*,*) 'QBD ', QS(NR)
      WRITE(*,*) '*******************************'

      ENDIF
      
      RETURN
      END

************************************************************************
*DECK FLUXAVERAGE
      SUBROUTINE FLUXAVERAGE(XX,YY,PSI,XAXIS,A)
C-----------------------------------------------------------------------
C   FLUX SURFACE AVERAGE QUANTITIES
C   TSTARAVG = <0.5(TPER + TPAR)>
C   PSTARAVG = <(2PPER + PPAR)/3>
C   DETAVG   = <DET>
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPLO
      USE GAUDSINT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      
      DOUBLE PRECISION XX(4,*), YY(4,*), PSI(*)
      DOUBLE PRECISION DETAVG(NRMMAX), TSTARAVG(NRMMAX),PSTARAVG(NRMMAX)
      DOUBLE PRECISION BPHIAVG(NRMAX), RHOAVG(NRMMAX), PPARAVG(NRMMAX)
      DOUBLE PRECISION XBOUNDLEN(NRMAX)

      DO 10 J=1, NR
         BOUNDLEN = 0
         DETAVG  (J) = 0
         TSTARAVG(J) = 0
         PSTARAVG(J) = 0
         BPHIAVG(J) = 0
         RHOAVG(J) = 0
         PPARAVG(J) = 0
         DO 20 I = 1, NP-1
            N1 = (J-1)*NP + I
            N2 = (J-1)*NP + I + 1
            N3 = (J-1)*NP + I + 1 + NP
            N4 = (J-1)*NP + I + NP
            IF (J.EQ.NR) THEN
               N1 = N1 - NP;
               N2 = N2 - NP;
               N3 = N3 - NP;
               N4 = N4 - NP;
            ENDIF
c            XLEN = DSQRT((XX(1,N1)-XX(1,N2))**2 +
c     >                   (YY(1,N1)-YY(1,N2))**2) * 
c     >                   (1+EPS*(XX(1,N1)+XX(1,N2))/2)
c            BOUNDLEN = BOUNDLEN + XLEN
            DO 30 NGS = 1, 4
               S = XGAUSS(NGS)
               WS = WGAUSS(NGS)
               R = -1.D0
               IF (J.EQ.NR) R = 1.D0-1.D-5
               CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >              PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),R,S,PS,PSR,PSS)
               CALL INTERP3(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >              YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >              PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >              PSI(4*(N4-1)+1),R,S,X,XR,XS,YR,YS,PS)
               XJAC =  XR*YS - XS*YR
               PYJAC = PSR*YS - PSS*YR
               PXJAC = PSR*XS - PSS*XR
               PSX = PYJAC / XJAC
               PSY = -PXJAC / XJAC
               XLEN = DSQRT(XS**2 + YS**2) * (1+EPS*X)
               BOUNDLEN = BOUNDLEN + XLEN * WS
               SUMDPSI = DSQRT(PSX**2 + PSY**2)
               CALL CALCBTPD(X, PS, SUMDPSI, A, BTOT, TPER, DET)
               TPAR = TEMS(PS)
               TSTARAVG(J) = TSTARAVG(J) + 0.5*(TPER+TPAR)*XLEN*WS*A*B
               RHO = TPER / TPAR * CALCRHO(X, PS)
               PPER = RHO * TPER * A * B
               PPAR = RHO * TPAR * A * B
               XBPHI = DABS(A)*XGAMMA(PS)**2/(1-DET)**2
               PSTARAVG(J) = PSTARAVG(J) + (2 * PPER+PPAR) / 3*XLEN * WS
               DETAVG(J) = DETAVG(J) + DET * XLEN * WS
               BPHIAVG(J) = BPHIAVG(J) + XBPHI * XLEN * WS
               RHOAVG(J) = RHOAVG(J) + RHO * XLEN * WS
               PPARAVG(J) = PPARAVG(J) + PPAR * XLEN * WS
 30         CONTINUE
 20      CONTINUE
         BOUNDLEN = DABS(BOUNDLEN)
         IF (THTOF.GT.0) THEN
            DETAVG(J) = -DABS(DETAVG(J)) / BOUNDLEN
         ELSE
            DETAVG(J) = DABS(DETAVG(J)) / BOUNDLEN
         ENDIF
         TSTARAVG(J) = DABS(TSTARAVG(J)) / BOUNDLEN
         PSTARAVG(J) = DABS(PSTARAVG(J)) / BOUNDLEN
         BPHIAVG(J) = DABS(BPHIAVG(J)) / BOUNDLEN
         RHOAVG(J) = DABS(RHOAVG(J)) / BOUNDLEN
         PPARAVG(J) = DABS(PPARAVG(J)) / BOUNDLEN
         XBOUNDLEN(J) = BOUNDLEN
 10   CONTINUE
      CALL CALCBTPD(XAXIS, 0.D0, 0.D0, A, BTOT, TPER, DET)
      TPAR = TEMS(0.D0)
      RHO  = TPER / TPAR * CALCRHO(XAXIS, 0.D0)
      PPER = RHO * TPER * A * B
      PPAR = RHO * TPAR * A * B
      TSTARAVG(NR) = 0.5*(TPER+TPAR) * A * B
      PSTARAVG(NR) = (2*PPER+PPAR)/3
      DETAVG(NR)   = DET
      BPHIAVG(NR) = DABS(A)*XGAMMA(0.D0)**2/(1-DET)**2
      PPARAVG(NR) = PPAR
      RHOAVG(NR) = RHO
      
      DO 40 J = NR, 1, -1
         PS = PSI(4*((J-1)*NP)+1)    
         TEMP = A * B * TEMS(PS) 
         WRITE(37,*) PS, RHOAVG(J) , PPARAVG(J), PSTARAVG(J), 
     >        DETAVG(J), BPHIAVG(J), XBOUNDLEN(J)
 40   CONTINUE
      RETURN
      END
                  
************************************************************************
*DECK PROFILES
      SUBROUTINE PROFILES(P0,RBPHI,DP,DRBPHI,A)
C-----------------------------------------------------------------------
C SUBROUTINE TO INTEGRATE THE EQUILIBRIUM PROFILES
C USING THE HBT DEFINITIONS
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE NODES
      USE FF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  P0(*),RBPHI(*),DP(*),DRBPHI(*)

C---------------------------- DERIVATIVES TO R = DSQRT(PSI) !!!! --------
      DO 10 J=1,NR
        FLUX = PSIKN(NR-J+1)
        IF ((IGAM.GE.1).AND.(IGAM.LE.4)) THEN
          P0(J) =  .5 * A * B * PRES(FLUX)
          DP(J) = - A * B * DPDPSI(FLUX) * DSQRT(FLUX)
          DGAM = - 2. * A * DGDPSI(FLUX) * DSQRT(FLUX)
          GAM = A * XGAMMA(FLUX)
          RBPHI(J) = P0(J) - EPS * GAM
          DRBPHI(J) = DP(J) - EPS * DGAM
        ELSE
          P0(J) = EPS * A * B * PRES(FLUX)
          DP(J) = -2.*EPS*A*B * DPDPSI(FLUX) * DSQRT(FLUX)
          RBPHI(J) =  EPS * A * C * XGAMMA(FLUX)
          DRBPHI(J) = EPS * 2.*A*C * DGDPSI(FLUX) * DSQRT(FLUX)
        ENDIF
        RBPHI(J) = DSQRT( 1. - 2.*EPS*RBPHI(J) /ALFA**2)
        DRBPHI(J) = - 1./(2.*RBPHI(J)) * 2.*EPS*DRBPHI(J)/ALFA**2
   10 CONTINUE
      RETURN
      END

***********************************************************************
*DECK PRES
      DOUBLE PRECISION FUNCTION PRES(PSI)
C-----------------------------------------------------------------------
C PRESSURE PROFILE AS A FUNCTION OF PSI
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
C      NPT=1001
C      NINT = INT((NPT-1)*FLUX)+1
C      IF (FLUX.GE.1.) NINT=NPT-1
C      DPS = 1./REAL(NPT-1)
C      SUM = PINT(NINT+1)
C      DPSI = NINT*DPS - FLUX
C      PSII = (NINT-1)*DPS
C      DPRI = DPRES(NINT) + (FLUX-PSII)/DPS*(DPRES(NINT+1)-DPRES(NINT))
C      SUM  = SUM + DPSI * (DPRI + DPRES(NINT+1))/2.
C      PRES = SUM
      NPT = 1001
      DPS = 1. /REAL(NPT-1)
      NINT = MAX(INT((NPT-1)*(PSI)) + 1,1)
      IF (PSI.GE.1.) NINT=NPT-1
      PRES = PINT(NINT) + ((PSI)-DPS*(NINT-1))/DPS *
     >                         (PINT(NINT+1)-PINT(NINT))
      RETURN
      END

************************************************************************
*DECK XGAMMA
      DOUBLE PRECISION FUNCTION XGAMMA(PSI)
C-----------------------------------------------------------------------
C SECOND PROFILE AS A FUNCTION OF PSI
C-----------------------------------------------------------------------
      USE PARAM
      USE COMDAT
      USE COMPROF
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NGT = 1001
      ZPSI = PSI
      DG = 1.D0 /REAL(NGT-1)
      NINT = MAX(INT((NGT-1)*(ZPSI)) + 1,1)
C      WRITE(*,*)NINT
      IF (ZPSI.GE.1.D0) THEN
         XGAMMA = GINT(NGT)
      ELSE
         XGAMMA  = GINT(NINT) + ((ZPSI)-DG*(NINT-1))/DG *
     >        (GINT(NINT+1)-GINT(NINT))
      ENDIF
      RETURN
      END

************************************************************************
*DECK PLOTM
      SUBROUTINE PLOTM(XX,YY,NR,NP,IAS,IFILE)
C-----------------------------------------------------------------------
C SIMPLE PLOTROUTINE TO PLOT A GRID KNOWN ONLY BY THE POSITIONS, NOT
C THE DERIVATIVES (SEE PLOTGR)
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(*),YY(*)
      CHARACTER LABEL*40
      WRITE(IFILE,*)'# X   Y   IR   IP'
      DO 10 IR=1,NR
         DO 20 IP=1,NP
            NBASE = IP + (IR-1)*NP
          WRITE(IFILE,*) XX(NBASE), YY(NBASE)
   20   CONTINUE
        IF (IAS.NE.0) WRITE(IFILE,*) XX((IR-1)*NP+1),YY((IR-1)*NP+1)
   10 CONTINUE
      DO 30 IP = 1, NP
         DO 40 IR=NR,1,-1
            NBASE = IP + (IR-1)*NP
            WRITE(IFILE,*) XX(NBASE), YY(NBASE)
 40      CONTINUE
         DO 50 IR=1,NR
            NBASE = IP + (IR-1)*NP
            WRITE(IFILE,*) XX(NBASE), YY(NBASE)
 50      CONTINUE
 30   CONTINUE
      RETURN
      END

************************************************************************
*DECK TRIANG
      SUBROUTINE TRIANG(XX,YY,PSI,XAXIS,NR,NP,XELL,XTRIA)
C----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE TRIANGULARITY OF FLUXSURFACES AS A FUNCTION
C OF THE MINOR RADIUS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMOUT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*),
     >     RR(8*MAXMNODE+2),RRT(4*MAXMNODE),XTRIA(*),XELL(*)

      WRITE(20,*) '**************************************************'
      WRITE(20,*) '*  ELLIPTICITY AND TRIANGULARITY   (FOURIER CO.) *' 
      WRITE(20,*) '**************************************************'
      WRITE(20,*) '* INDEX,  S,    RADIUS,   SHIFT,   ELLIP,   TRIA *'
      WRITE(20,*) '**************************************************'
      DO 10 I=1,NR-1
        DO 20 J=1, NP-1
          N1 = (I-1)*NP + J
          N2 = N1 + 1
          N3 = N2 + NP
          N4 = N1 + NP
          DO 30 NGS=1,4
            R  = -1.
            S  = -1. + 0.5 * REAL(NGS-1) 
            CALL INTERP1(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                  R,S,X)
            CALL INTERP1(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                  R,S,Y)
            CALL INTERP1(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >                   PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),R,S,PS)
            RRT(4*(J-1)+ NGS) = DSQRT((X-XAXIS)**2 + Y*Y)
   30     CONTINUE
   20   CONTINUE
        RRT(4*(NP-1)+1) = DSQRT((XX(1,I*NP)-XAXIS)**2)
        DO 35 J=1,4*(NP-1)+1
          RR(J) = RRT(4*(NP-1)-J+2)
   35   CONTINUE       
        DO 40 J=1,4*(NP-1)-1
          RR(4*(NP-1)+J+1) = RR(4*(NP-1)-J+1)
   40   CONTINUE
        NPT = 8*(NP-1)
        CALL RFT2(RR,NPT,1)
   41   FORMAT(I3,E16.8)  
        XRAD   =    RR(1)/REAL(NPT)
        XSHIFT =   -2.*RR(3)/REAL(NPT) / DSQRT(PS)
        XELL(I)  = -4.*RR(5)/REAL(NPT) / DSQRT(PS)
        XTRIA(I) =  8.*RR(7)/REAL(NPT) / PS
        WRITE(20,11) I,DSQRT(PS),XRAD,XSHIFT,XELL(I),XTRIA(I)
   10 CONTINUE
   11 FORMAT(I3,5E16.8)
c-----------------------------------------------------------------
c alternative to calculate the ellipticity and triangularity by
c finding the point dy/ds=0 on each fluxsurface.
c-----------------------------------------------------------------
      WRITE(20,*) '**************************************************'
      WRITE(20,*) '*  ELLIPTICITY AND TRIANGULARITY (GEOMETRIC CO.) *' 
      WRITE(20,*) '**************************************************'
      DO 60 I=1,NR-1
      DO 50 J=1, NP-1
        N1 = (I-1)*NP + J
        N2 = (I-1)*NP + J + 1
        IF  (YY(3,N1)*YY(3,N2).LE.0.) THEN
C------------------------------------- QUAD. EQ FOR S VALUE AT MINIMUM -
        YYM  = YY(1,N1)
        YYMR = YY(3,N1)
        YYP  = YY(1,N2)
        YYPR = YY(3,N2)
        AA =  3. * (YYM + YYMR - YYP + YYPR ) / 4.
        BB =  ( - YYMR + YYPR ) / 2.
        CC =  ( - 3*YYM - YYMR + 3*YYP - YYPR) / 4.
        DET = BB*BB - 4.*AA*CC
        S  = ROOT(AA,BB,CC,DET,1.D0)
        IF (DABS(S).GT.1.+1.D-8) THEN
          S = ROOT(AA,BB,CC,DET,-1.D0)
        ENDIF
        CALL CUB1D(XX(1,N1),XX(3,N1),XX(1,N2),XX(3,N2),S,XTOP,DUMMY)
        CALL CUB1D(YY(1,N1),YY(3,N1),YY(1,N2),YY(3,N2),S,YTOP,DUMMY)
        XLEFT = XX(1,(I-1)*NP+1)
        XRIGHT = XX(1,(I-1)*NP+NP)
        XGEO = (XLEFT + XRIGHT)/2.
        XRAD = (XRIGHT-XLEFT)/2.
        XSHIFT = (XAXIS - XGEO)/XRAD
        ELL    = YTOP/XRAD - 1.
        TRI    = -(XTOP-XGEO)/XRAD**2
        PS = PSI(4*(N1-1)+1)
        WRITE(20,51) I,DSQRT(PS),XRAD,XSHIFT,ELL,TRI
        ENDIF
   50 CONTINUE
   51 FORMAT(i3,5e16.8)
   60 CONTINUE
   
      RETURN
      END

************************************************************************
*DECK DIAGNO
      SUBROUTINE DIAGNO(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,IGAM,ISOL,IAS,
     >                  XAXIS,YAXIS,IAV,ZVOL,ZVOLP,RVAC,BVAC)
C----------------------------------------------------------------------
C SUBROUTINE TO EVALUATE OUTPUT QUANTITIES
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMPROF
      USE COMOUT
      USE COMPRI
      USE COMPLO
      USE COMPIE
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*),ZVOL(*),ZVOLP(*)
      DOUBLE PRECISION  XL(NRMMAX),ZPS(NRMMAX),AVC(NRMMAX)
      DOUBLE PRECISION  ZJAR(NRMMAX),ZPAR(NRMMAX),ZAR(NRMMAX)
      DOUBLE PRECISION  ZLEN(NRMMAX)

      PI = 2. * DASIN(1.D0)

      FACTAS = 2.
      IF (IAS.EQ.1) FACTAS=1.

      AREA = 0.
      VOLUME = 0.
      BP2VOL = 0.
      PAREA = 0.
      PAREAPER = 0.
      PVOL  = 0.
      PVOLPER = 0.
      PAR2  = 0.
      PARPER2 = 0.
      CAREA = 0.
      TMASSVOL = 0.
      DETVOL = 0.
      FEDGE = XGAMMA(1.D0) * DSQRT(DABS(A))
      PSI0  = BVAC * RVAC**2 * EPS / FEDGE
C------------------------------------- NELM ELEMENTS ------------------
      DO 10 I=NR-1,1,-1
      BOUNDLEN = 0.
      DO 15 J=1,NP-1
        NELM = (I-1)*(NP-1) + J
        N1 = NODENO(NELM,1)
        N2 = NODENO(NELM,2)
        N3 = NODENO(NELM,3)
        N4 = NODENO(NELM,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN R -----
        DO 20 NGR=1,4
          R = XGAUSS(NGR)
          WR = WGAUSS(NGR)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
          DO 30 NGS=1,4
            S  = XGAUSS(NGS)
            WS = WGAUSS(NGS)
            CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                   R,S,X,XR,XS)
            CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                   R,S,Y,YR,YS)
            CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >                   PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >                   R,S,PS,PSR,PSS)
            XJAC =  XR*YS - XS*YR
            AREA = AREA + WR * WS * XJAC
            VOLUME = VOLUME + (1.+EPS*X) * WR * WS * XJAC
            GRPS2 = PSR**2 * (XS**2 + YS**2) / XJAC**2
            PYJAC = PSR*YS - PSS*YR
            PXJAC = PSR*XS - PSS*XR
            PSX = PYJAC / XJAC
            PSY = -PXJAC / XJAC
            SUMDPSI = DSQRT(PSX**2 + PSY**2)
            CALL CALCBTPD(X, PS, SUMDPSI, A, BTOT, TPER, DET)
            RHO = CALCRHO(X, PS)
            PRESS = RHO * TPER * B * A
            PRESSPER = RHO * TPER**2 / TEMS(PS) * B * A
            TMASS = RHO * TPER / TEMS(PS)
            TMASSVOL = TMASSVOL + WR * WS * XJAC * TMASS * (1+EPS*X)
            PAREA = PAREA + WR * WS * XJAC * PRESS
            PAR2  = PAR2  + WR * WS * XJAC * PRESS**2
            PVOL  = PVOL  + WR * WS * XJAC * PRESS * (1+EPS*X)
            PVOL2 = PVOL2 + WR * WS * XJAC * PRESS**2 * (1+EPS*X)
            PAREAPER = PAREAPER + WR * WS * XJAC * PRESSPER
            PARPER2  = PARPER2  + WR * WS * XJAC * PRESSPER**2
            PVOLPER  = PVOLPER  + WR * WS * XJAC * PRESSPER * (1+EPS*X)
            PVOL2PER = PVOL2PER + WR * WS * XJAC * PRESSPER**2*(1+EPS*X)
            BP2 = SUMDPSI**2 / (1.+EPS*X)**2    
            BP2VOL = BP2VOL + (1.+EPS*X) * BP2 *WR*WS*XJAC
            CALL CALCRJPHI(X, PS, SUMDPSI, A, ARHS, DET)
            ARHS = A*ARHS/(1.D0+EPS*X) 
            CAREA = CAREA + WR * WS * XJAC * ARHS
            DETVOL = DETVOL + WR * WS * XJAC * DET * (1.+EPS*X)
   30     CONTINUE
   20   CONTINUE
        NELM = (I-1)*NP+J
        XLEN=(XX(1,NELM)-XX(1,NELM+1))**2 + (YY(1,NELM)-YY(1,NELM+1))**2
        BOUNDLEN = BOUNDLEN + DSQRT(XLEN)
   15 CONTINUE
      
      ZJAR(I) = FACTAS * DABS(CAREA)
      ZPAR(I) = FACTAS * DABS(PAREA)
      ZLEN(I) = FACTAS * DABS(BOUNDLEN)
      ZAR(I)  = FACTAS * DABS(AREA)
      ZVOL(NR-I+1) = FACTAS * DABS(VOLUME)
c$$$      WRITE(38,*) PS, TMASSVOL

   10 CONTINUE
      AREA   = FACTAS * DABS(AREA)
      VOLUME = FACTAS * DABS(VOLUME)
      CAREA  = FACTAS * DABS(CAREA)
      PVOL   = FACTAS * DABS(PVOL)
      PVOLPER= FACTAS * DABS(PVOLPER)
      PVOL2  = FACTAS * DABS(PVOL2)
      PVOL2PER= FACTAS* DABS(PVOL2PER)
      BP2VOL = FACTAS * DABS(BP2VOL)
      DETVOL = FACTAS * DABS(DETVOL)
      TMASSVOL = FACTAS * DABS(TMASSVOL)
      RAV = VOLUME / AREA
      CALL CALCBTPD(XAXIS, 0.D0, 0.D0, A, BTOT, TPER, DET)
      RHO0 = CALCRHO(XAXIS, 0.D0) * TPER / TEMS(0.D0)
      VAF2 = XGAMMA(0.D0)**2 * A / RHO0 / (1+EPS*XAXIS)**2 / (1-DET)**2
      
      XLI = DABS(BP2VOL) / VOLUME * ZLEN(1)**2 / CAREA**2
      BETA        = 2 * DABS(PVOL)     / BTOT**2  / VOLUME
      BETAPER     = 2 * DABS(PVOLPER)  / BTOT**2  / VOLUME
      BETASTAR    = 2 * DSQRT(PVOL2)   / BTOT**2  / VOLUME
      BETASTARPER = 2 * DSQRT(PVOL2PER)/ BTOT**2  / VOLUME
      BETAPL    = 2*DABS(PVOL)   * ZLEN(1)**2 / CAREA**2 /VOLUME
      BETAPLPER = 2*DABS(PVOLPER)* ZLEN(1)**2 / CAREA**2 /VOLUME
      ENERGYK = (PVOL + 2*PVOLPER) * 2 * PI /2
      TMASSVOL = TMASSVOL * 2 * PI
      CURRENT  = DABS(CAREA) 
      DETVOL = DETVOL / VOLUME
      VOLUME = 2 * PI * VOLUME

      WRITE(20,*)
      WRITE(20,*) '***************************************'
      WRITE(20,11) XAXIS,YAXIS
      WRITE(20,2) BETAPL
      WRITE(20,21)BETAPLPER
      WRITE(20,22)0.5*(BETAPL+BETAPLPER)
      WRITE(20,23)BETAPL/3.D0 + BETAPLPER/3.D0*2.D0
      WRITE(20,3) BETA
      WRITE(20,31)BETAPER
      WRITE(20,32)0.5*(BETA+BETAPER)
      WRITE(20,33)BETA/3.D0 + BETAPER/3.D0*2.D0
      WRITE(20,34)(BETAPER-BETA)/(BETA+BETAPER)*2.
      WRITE(20,8) BETASTAR
      WRITE(20,81)BETASTARPER
      WRITE(20,12) BETA/(CURRENT/(A/(1-DET)/(1+EPS*XAXIS)))
      WRITE(20,121) BETAPER/(CURRENT/(A/(1-DET)/(1+EPS*XAXIS)))
      WRITE(20,4) CURRENT
      WRITE(20,16)ENERGYK
      WRITE(20,17)TMASSVOL
      WRITE(20,5) AREA
      WRITE(20,6) VOLUME
      WRITE(20,61)ZLEN(1)
      WRITE(20,7) XLI
      WRITE(20,14) DET
c      WRITE(20,13) A,B,C
      WRITE(20,*) '***************************************'
      WRITE(20,*)
    
      RCURR = CURRENT * PSI0 / RVAC / 4 / PI * 1.D7
      VAF = DSQRT(VAF2/RVAC**4/EPS**2 * PSI0**2/ 1.D-7/4/PI/1.D-7)

      WRITE(20,*) '***************************************'
      WRITE(20,*) '  REAL WORLD QUANTITIES'
      WRITE(20,*) '***************************************'
      WRITE(20,151) RVAC
      WRITE(20,161) BVAC
      WRITE(20,171) PSI0
      WRITE(20,181) RCURR/1.D6
      WRITE(20,191) ENERGYK/4/PI*1.D4/RVAC*PSI0**2
      WRITE(20,17)  TMASSVOL * RVAC**3 * EPS**2
      WRITE(20,202) VAF / 1.0D3 / RVAC / (1.+EPS*XAXIS) / 2./PI
      WRITE(20,*) '***************************************'
      WRITE(20,*)

      WRITE(22,*) (BETAPER-BETA)*2/(BETAPER+BETA),
     >     (BETAPL+2*BETAPLPER)/3,(BETA+BETAPER*2)/3,XLI,DET,
     >     RCURR/1.D6, PSI0



    2 FORMAT('  POLOIDAL PAR BETA : ',F12.6)
 21   FORMAT('  POLOIDAL PER BETA : ',F12.6)
 22   FORMAT('  POLOIDAL MHD BETA : ',F12.6)
 23   FORMAT('  POLOIDAL ENG BETA : ',F12.6)
    3 FORMAT('  TOTAL PAR BETA    : ',F12.6)
 31   FORMAT('  TOTAL PER BETA    : ',F12.6)
 32   FORMAT('  TOTAL MHD BETA    : ',F12.6)
 33   FORMAT('  TOTAL ENG BETA    : ',F12.6)
 34   FORMAT('  XI                : ',F12.6)
    4 FORMAT('  TOTAL CURRENT     : ',F12.6)
    5 FORMAT('  TOTAL AREA        : ',F12.6)
    6 FORMAT('  TOTAL VOLUME      : ',F12.6)
 61   FORMAT('  CIRCUMFERENCE     : ',F12.6)
    7 FORMAT('  INT. INDUCDTANCE  : ',F12.6)
    8 FORMAT('  BETA STAR PAR     : ',F12.6)
 81   FORMAT('  BETA STAR PER     : ',F12.6)
    9 FORMAT('  POL. FLUX         : ',F12.6)
   11 FORMAT('  MAGNETIC AXIS     : ',2F12.6)
   12 FORMAT('  NORM. BETA PAR    : ',F12.6)
 121  FORMAT('  NORM. BETA PER    : ',F12.6)
 14   FORMAT('  ANI DELTA AT AXIS : ',F12.6)
 16   FORMAT('  THERMAL ENERGY    : ',F12.6)
 17   FORMAT('  TOTAL MASS (E-7KG): ',F12.6)
 18   FORMAT('  TOTAL ENERGY      : ',F12.6)

 151  FORMAT('  MAJOR  RADIUS     : ',F12.6)
 161  FORMAT('  BVAC AT GEO CENTRE: ',F12.6)
 171  FORMAT('  PSI0              : ',F12.6)
 181  FORMAT('  PLASMA CURRENT(MA): ',F12.6)
 191  FORMAT('  THERMAL ENERGY KJ : ',F12.6)
 201  FORMAT('  TOTAL   ENERGY KJ : ',F12.6)
 202  FORMAT('  AX ALFVEN FQC KHZ : ',F12.6)
C------------------------------------- NELM ELEMENTS ------------------
   72 FORMAT(I3,3F8.4,1PE10.2,0P,5F8.4)   
      RETURN
      END
************************************************************************
*DECK FLXINT2
      SUBROUTINE FLXINT2(XAXIS,XX,YY,PSI,NR,NP,A,B,EPS,IGAM,
     >                   CX,CY,IAS,AMPL,SUMDQ)
C----------------------------------------------------------------------
C SUBROUTINE TO EVALUATE NEW DF2 PROFILE
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMOUT
      USE COMPROF
      USE COMPIE
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION XX(4,*),YY(4,*),PSI(*)
      DOUBLE PRECISION ZPS(NRMMAX),XPLOT(NRMMAX),QPLIN(NRMMAX)
      DOUBLE PRECISION DF2OLD(NRMMAX),DF2TMP(NRMMAX)
      DOUBLE PRECISION QPLOT(NRMMAX),DELTAQ(NRMMAX)
      DOUBLE PRECISION DD1(NRMMAX),DD2(NRMMAX),DD3(NRMMAX),DD4(NRMMAX)
      DOUBLE PRECISION ABLTG(3)

      SAVE ISAVE
      
      FACTAS = 2.
      IF (IAS.EQ.1) FACTAS=1.
      R = -1.D0
C------------------------------------- NELM ELEMENTS ------------------
      DO 40 I=1,NR-1
      SUMQ  = 0.D0
      DO 50 J=1,NP-1
         N1 = (I-1)*NP + J
         N2 = N1 + 1
         N3 = N2 + NP
         N4 = N1 + NP
C-------------------------------------4 POINT GAUSSIAN INT. IN S -----
         DO 60 NGS=1,4
            S  = XGAUSS(NGS)
            CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),R,S,
     >           X,XR,XS,XRS,XRR,XSS)
            CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),R,S,
     >           Y,YR,YS,YRS,YRR,YSS)
            CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >           PSI(4*(N4-1)+1),
     >           R,S,PS,PSR,PSS,PSRS,PSRR,PSSS)
            EJAC = XR*YS - XS*YR
            SUMDPSI = DSQRT(PSR**2 + PSS**2)
            CALL CALCBTPD(X,PS,SUMDPSI,A,BTOT,TPER,DET)
            ER = XRR*YS + XR*YRS - XRS*YR - XS*YRR
            BIGR  = (1. + EPS * X)
            SUMQ  = SUMQ -WGAUSS(NGS)*EJAC/( BIGR * DABS(PSR))/(1-DET)
 60      CONTINUE
 50   CONTINUE
      ZPS(NR-I+1) = PS
      QTMP  = 0.5D0*FACTAS * SUMQ * EPS * SQRT(DABS(A))*XGAMMA(PS)/PI
      QPLIN(NR-I+1) = QPRFL(PS)
      DELTAQ(NR-I+1) = (QPLIN(NR-I+1) - QTMP )
      DF2OLD(NR-I+1) = DGDPSI(PS)
      QPLOT(NR-I+1) = QTMP
      XPLOT(NR-I+1) = DSQRT(PS)
 40   CONTINUE
      WRITE(*,*)XAXIS,A,BTOT0,TPER0,DET0
      WRITE(*,*) 'TEST LABEL1'
      CALL CALCBTPD(XAXIS,0.D0,0.D0,A,BTOT0,TPER0,DET0)
      WRITE(*,*) 'TEST LABEL2'
      QTMP0 = XGAMMA(0.D0) * EPS /(2.*DSQRT(CX*CY)*(1.+EPS*XAXIS))
      QTMP0 = QTMP0 * DSQRT(DABS(A)) / (1.D0-DET0)
      QPLOT(1) = QTMP0
      XPLOT(1) = 0.
      ZPS(1) = 0.
      QPLIN(1) = QPRFL(0.)
      DELTAQ(1) = (QPLIN(1)-QTMP0)
      DF2OLD(1) = DGDPSI(0.D0)
      
      SUMDQ = 0.D0
      DF2TMP(1)    = DF2OLD(1)    + 0.25*AMPL*(DELTAQ(1)   -DELTAQ(2))
      ZPS(1) = 0.D0 
      DF2TMP(NR) = DF2OLD(NR)
      DO I=2,NR-1
         DF2TMP(NR-I+1)=DF2OLD(NR-I+1)
     >                + AMPL*(DELTAQ(NR-I)-DELTAQ(NR-I+2))
     >                + AMPL*DELTAQ(NR-I+1)/4.

        DF2TMP(NR-I+1) = DF2TMP(NR-I+1)
      ENDDO
      DF2TMP(NR-1) = DF2OLD(NR-1)+0.25*AMPL*(DELTAQ(NR-2)-DELTAQ(NR-1))
      FEDGE = XGAMMA(1.D0)
c------------------------------------- calc equidistant gamma profile   
      CALL SPLINE(NR,XPLOT,DF2TMP,0.,0.,2,DD1,DD2,DD3,DD4)
      NPT=1001
      DPS = 1./FLOAT(NPT-1)
      GINT(NPT) = FEDGE
      DGAM(NPT) = DF2TMP(NR)
      DO 70 I=NPT-1,1,-1
        PS = FLOAT(I-1)/FLOAT(NPT-1)
        DUMMY = SPWERT(NR,SQRT(PS),DD1,DD2,DD3,DD4,XPLOT,ABLTG)
 	DGAM(I) = DUMMY
        GINT(I) = GINT(I+1) - DGAM(I) * DPS
 70   CONTINUE
      B = B / GINT(1)**2
      DO I = NPT, 1, -1
         DGAM(I) = DGAM(I) / GINT(1)
         GINT(I) = GINT(I) / GINT(1)
      ENDDO
      RETURN
      END
************************************************************************
*DECK MOMENTS
      SUBROUTINE MOMENTS(XX,YY,PSI,NR,NP,XIAB,ALFA,EPS,IAS)
C----------------------------------------------------------------------
C SUBROUTINE TO EVALUATE SOME CURRENT MOMENTS
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMOUT
      USE COMPROF
      USE COMPLO
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,MAXMNODE),YY(4,MAXMNODE),PSI(4*MAXMNODE)

      PI = 2. * DASIN(-1.D0)

      FACTAS = 2.
      IF (IAS.EQ.1) FACTAS=1.

      R = -1.
      YM1 = 0.
      YZ1 = 0.
      CUR = 0.
C------------------------------------- INTEGRAL OVER PLASMA BOUNDARY      
      DO 10 J=1,NP-1
        NELM = J
        N1 = NODENO(NELM,1)
        N2 = NODENO(NELM,2)
        N3 = NODENO(NELM,3)
        N4 = NODENO(NELM,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
        DO 20 NGS=1,4
          S  = XGAUSS(NGS)
          WS = WGAUSS(NGS)
          CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                R,S,X,XR,XS,XRS,XRR,XSS)
          CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                R,S,Y,YR,YS,YRS,YRR,YSS)
          CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                PSI(4*(N4-1)+1),
     >                R,S,PS,PSR,PSS,PSRS,PSRR,PSSS)
          XJAC =  XR*YS - XS*YR
          DL = DSQRT(XS**2+YS**2)
          BPL = EPS * PSR / XJAC * DSQRT(XS**2+YS**2)/(1.+EPS*X) 
          F1  = X * (1. + EPS*X/2.)
          F1Z = Y
          YM1 = YM1 + F1  * DABS(BPL) * DL * WS
          YZ1 = YZ1 + F1Z * DABS(BPL) * DL * WS
          CUR = CUR + BPL * DL * WS
   20   CONTINUE
   10 CONTINUE
      YM1 =  FACTAS * YM1 / (ALFA * XIAB)
      YZ1 =  FACTAS * YZ1/  (ALFA * XIAB)
      WRITE(20,*) ym1,yz1,cur
c-------------------------------- the radius of the current center RJ
      RJ = DSQRT(1. + 2.*EPS*YM1) / EPS
      ZJ = YZ1
      YM1a = 0.
      YZ1a = 0.
      YM2 = 0.
      YM3 = 0.
      YM4 = 0.
      YM5 = 0.
      YM6 = 0.
      DO 30 J=1,NP-1
        NELM = J
        N1 = NODENO(NELM,1)
        N2 = NODENO(NELM,2)
        N3 = NODENO(NELM,3)
        N4 = NODENO(NELM,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
        DO 40 NGS=1,4
          S  = XGAUSS(NGS)
          WS = WGAUSS(NGS)
          CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                R,S,X,XR,XS,XRS,XRR,XSS)
          CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                R,S,Y,YR,YS,YRS,YRR,YSS)
          CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                PSI(4*(N4-1)+1),
     >                R,S,PS,PSR,PSS,PSRS,PSRR,PSSS)
          XJAC =  XR*YS - XS*YR
          DL = DSQRT(XS**2+YS**2)
          BPL = EPS * PSR / XJAC * DSQRT(XS**2+YS**2)/(1.+EPS*X) 
          RMRJ = (1. + EPS*X - RJ * EPS) / EPS
          F1 = RMRJ * (1. + 0.5*RMRJ/RJ) 
          F1Z = Y   * (1. + RMRJ/RJ)
          F2 = F1**2 - F1Z**2
          F3 = F1**3 - 3*F1*F1Z**2 + Y**2/RJ  * F1Z**2
          F4 = F1**4 - 6.*F1**2 * F1Z**2 + F1Z**2 *(3*F1Z**2 - 2*Y**2)
     >       - 0.8 * Y**4 * F1Z**2 / RJ**2
c---------------------------------- function from mathematica mom2.ma  
      R1 = (1. + EPS * X)/EPS   
      F2=-0.5*R1**2+0.25*R1**4/RJ**2+0.25*RJ**2-1.*R1**2*Y**2/RJ**2
      F3=-0.125*(-1.*R1**6+3.*R1**4*RJ**2-3.*R1**2*RJ**4+1.*RJ**6 
     >   +12.*R1**4*Y**2-12.*R1**2*RJ**2*Y**2-8.*R1**2*Y**4)/RJ**3
      F4=0.0625*(1.*R1**8-4.*R1**6*RJ**2+6.*R1**4*RJ**4-4.*R1**2*RJ**6 
     >   + 1.*RJ**8 - 24.*R1**6*Y**2 + 48.*R1**4*RJ**2*Y**2 
     >   - 24.*R1**2*RJ**4*Y**2 
     >   + 48.*R1**4*Y**4-32.*R1**2*RJ**2*Y**4-12.8*R1**2*Y**6)/RJ**4
      F5=-0.03125*(-1.*R1**10 + 5.*R1**8*RJ**2 - 10.*R1**6*RJ**4 
     >   + 10.*R1**4*RJ**6-5.*R1**2*RJ**8+1.*RJ**10+40.*R1**8*Y**2 
     >   - 120.*R1**6*RJ**2*Y**2 + 120.*R1**4*RJ**4*Y**2 
     >   - 40.*R1**2*RJ**6*Y**2 - 160.*R1**6*Y**4 
     >   +240.*R1**4*RJ**2*Y**4-80.*R1**2*RJ**4*Y**4+128.*R1**4*Y**6  
     >   -64.*R1**2*RJ**2*Y**6 -18.2857*R1**2*Y**8)/RJ**5
      F6=0.015625*(1.*R1**12 - 6.*R1**10*RJ**2 + 15.*R1**8*RJ**4
     >   - 20.*R1**6*RJ**6+15.*R1**4*RJ**8-6.*R1**2*RJ**10+1.*RJ**12
     >   - 60.*R1**10*Y**2 + 
     >     240.*R1**8*RJ**2*Y**2 - 360.*R1**6*RJ**4*Y**2 + 
     >     240.*R1**4*RJ**6*Y**2-60.*R1**2*RJ**8*Y**2+400.*R1**8*Y**4 - 
     >     960.*R1**6*RJ**2*Y**4+720.*R1**4*RJ**4*Y**4 - 
     >     160.*R1**2*RJ**6*Y**4-640.*R1**6*Y**6+768.*R1**4*RJ**2*Y**6 - 
     >     192.*R1**2*RJ**4*Y**6+274.286*R1**4*Y**8 - 
     >     109.714*R1**2*RJ**2*Y**8-24.381*R1**2*Y**10)/RJ**6
c----------------------------------------------------------------------------     
     
          YM1a = YM1a + F1  * DABS(BPL) * DL * WS
          YZ1a = YZ1a + F1Z * DABS(BPL) * DL * WS
          YM2 = YM2 + F2 * DABS(BPL) * DL * WS
          YM3 = YM3 + F3 * DABS(BPL) * DL * WS
          YM4 = YM4 + F4 * DABS(BPL) * DL * WS
          YM5 = YM5 + F5 * DABS(BPL) * DL * WS
          YM6 = YM6 + F6 * DABS(BPL) * DL * WS
   40   CONTINUE
   30 CONTINUE
      YM1a = FACTAS * YM1a / (ALFA * XIAB)
      YZ1a = FACTAS * YZ1a / (ALFA * XIAB)
      YM2  = FACTAS * YM2  / (ALFA * XIAB)
      YM3  = FACTAS * YM3  / (ALFA * XIAB)
      YM4  = FACTAS * YM4  / (ALFA * XIAB)
      YM5  = FACTAS * YM5  / (ALFA * XIAB)
      YM6  = FACTAS * YM6  / (ALFA * XIAB)
      WRITE(20,*) '********************************************'
      WRITE(20,*) '*     CURRENT MOMENTS                      *'
      WRITE(20,*) '********************************************'
      WRITE(20,41) RJ,ZJ
      WRITE(20,42) YM1A, YZ1A
      WRITE(20,43) YM2
      WRITE(20,44) YM3
      WRITE(20,45) YM4
      WRITE(20,46) YM5
      WRITE(20,47) YM6
      WRITE(20,*)
   41 format(' RJ, ZJ : ',2e12.4)   
   42 format(' YM1, YZ1 : ',2e12.4)
   43 format(' YM2 : ',e12.4)
   44 format(' YM3 : ',e12.4)
   45 format(' YM4 : ',e12.4)
   46 format(' YM5 : ',e12.4)
   47 format(' YM6 : ',e12.4)
      RETURN
      END

************************************************************************
*DECK BETAPOL
      SUBROUTINE BETAPOL(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,
     >                   IGAM,ISOL,BPL,IAS)
C----------------------------------------------------------------------
C SUBROUTINE TO EVALUATE OUTPUT QUANTITIES
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMOUT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,MAXMNODE),YY(4,MAXMNODE),PSI(4*MAXMNODE)
      DOUBLE PRECISION  XL(NRMMAX),ZPS(NRMMAX)
      DOUBLE PRECISION  ZJAR(NRMMAX),ZPAR(NRMMAX),ZAR(NRMMAX)

      PI = 2. * DASIN(1.D0)

      FACTAS = 2.
      IF (IAS.EQ.1) FACTAS=1.

      PAREA = 0.
      CAREA = 0.
C------------------------------------- NELM ELEMENTS ------------------
      DO 10 I=NR-1,1,-1
      DO 15 J=1,NP-1
        NELM = (I-1)*(NP-1) + J
        N1 = NODENO(NELM,1)
        N2 = NODENO(NELM,2)
        N3 = NODENO(NELM,3)
        N4 = NODENO(NELM,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN R -----
        DO 20 NGR=1,4
          R = XGAUSS(NGR)
          WR = WGAUSS(NGR)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
          DO 30 NGS=1,4
            S  = XGAUSS(NGS)
            WS = WGAUSS(NGS)
            CALL INTERP2(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                  R,S,X,XR,XS)
            CALL INTERP2(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                  R,S,Y,YR,YS)
            CALL INTERP2(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >                   PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >                  R,S,PS,PSR,PSS)
            XJAC =  XR*YS - XS*YR     
            PAREA = PAREA + WR * WS * XJAC * PRES(PS)
            IF ((IGAM.GE.1).AND.(IGAM.LE.4)) THEN
              ARHS = DGDPSI(PS) + B*X*(1.+EPS*X/2.)*DPDPSI(PS)
            ELSE
              ARHS = C*DGDPSI(PS) + B*(1.+EPS*X)**2 * DPDPSI(PS)
            ENDIF
            ARHS =  ARHS / (1.+EPS*X)
C---------------------------------------------- SOLOVIEV RHS ----------
            IF (ISOL.EQ.1) ARHS = (1.+ B*X*(1.+EPS*X/2.))/(1.+EPS*X)
            CAREA = CAREA + WR * WS * XJAC * ARHS
   30     CONTINUE
   20   CONTINUE
   15   CONTINUE
   10 CONTINUE
      IF ((IGAM.GE.1).AND.(IGAM.LE.4)) THEN
        PAREA = 0.5 * FACTAS * A * B * PAREA
        CAREA =  FACTAS * A * CAREA
      ELSE
        PAREA = FACTAS * EPS * A * B * PAREA
        CAREA = FACTAS * A * CAREA
      ENDIF
      BPL = -(8*PI/EPS) * PAREA/ CAREA**2
      RETURN
      END



************************************************************************
*DECK CURRENT
      SUBROUTINE CURRENT(XX,YY,PSI,NR,NP,A,B,C,EPS,ALFA,
     >                   IGAM,ISOL,CUR,IAS)
C----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE TOTAL CURRENT
C----------------------------------------------------------------------
      USE PARAM
      USE CORNERS
      USE GAUDSINT
      USE COMOUT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,MAXMNODE),YY(4,MAXMNODE),PSI(4*MAXMNODE)

      PI = 2. * DASIN(1.D0)

      FACTAS = 2.
      IF (IAS.EQ.1) FACTAS=1.

      CAREA = 0.
C------------------------------------- NELM ELEMENTS ------------------
      DO 10 I=1,NR-1
      DO 15 J=1,NP-1
        NELM = (I-1)*(NP-1) + J      
        N1 = NODENO(NELM,1)
        N2 = NODENO(NELM,2)
        N3 = NODENO(NELM,3)
        N4 = NODENO(NELM,4)
C------------------------------------- 4 POINT GAUSSIAN INT. IN R -----
        DO 20 NGR=1,4
          R = XGAUSS(NGR)
          WR = WGAUSS(NGR)
C------------------------------------- 4 POINT GAUSSIAN INT. IN S -----
          DO 30 NGS=1,4
            S  = XGAUSS(NGS)
            WS = WGAUSS(NGS)
            CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >                  R,S,X,XR,XS,XRS,XRR,XSS)
            CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >                  R,S,Y,YR,YS,YRS,YRR,YSS)
            CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),PSI(4*(N3-1)+1),
     >                  PSI(4*(N4-1)+1),
     >                  R,S,PS,PSR,PSS,PSRS,PSRR,PSSS)
            XJAC =  XR*YS - XS*YR
            IF ((IGAM.GE.1).AND.(IGAM.LE.4)) THEN
              ARHS = C*DGDPSI(PS) + B*X*(1.+EPS*X/2.)*DPDPSI(PS)
            ELSE
              ARHS = C*DGDPSI(PS) + B*(1.+EPS*X)**2 * DPDPSI(PS)
            ENDIF
            ARHS =  ARHS / (1.+EPS*X)
C---------------------------------------------- SOLOVIEV RHS ----------
            IF (ISOL.EQ.1) ARHS = (1.+ B*X*(1.+EPS*X/2.))/(1.+EPS*X)
            CAREA = CAREA + WR * WS * XJAC * ARHS
   30     CONTINUE
   20   CONTINUE
   15 CONTINUE
   10 CONTINUE
      CUR = FACTAS * A * EPS * CAREA
C      WRITE(20,4) CUR
    4 FORMAT(' TOTAL CURRENT (ALFA=1) : ',E12.4)
      RETURN
      END






************************************************************************
*DECK SOLOV
      SUBROUTINE SOLOV(X,Y,PSI,PSIX,PSIY,PSIXY)
C-----------------------------------------------------------------------
C SOLOVIEV EQUILIBRIUM
C-----------------------------------------------------------------------
      USE COMDAT
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      Y = -Y
      PAR4 = QUAD
      PAR3 = TRIA
      PAR2 = EPS
      PAR1 = ELLIP
      PSI = (1.0 - 0.25*PAR2**2) * (1.+PAR2*X)**2 * (Y/PAR1)**2           
     >    + (X - 0.5*PAR2*(1.0 - X*X))**2                                
     >    + (4.- PAR2**2) * (X - 0.5*PAR2*(1.0-X**2)) *PAR4*Y/(2*PAR2)    

      PSIX = 2*(X - 0.5*PAR2*(1-X*X)) * (1.+PAR2*X) +
     >       (2*PAR2*(1.-0.25*PAR2**2) * (1.+PAR2*X) ) * (Y/PAR1)**2
     >     +(4.- PAR2**2) * (1.+ 0.5*PAR2*(2*X))      *PAR4*Y/(2*PAR2)

      PSIY = 2*Y / PAR1**2 * (1.-0.25*PAR2**2)*(1.+ PAR2*X)**2
     >     + (4.- PAR2**2) * (X - 0.5*PAR2*(1.0-X**2))*PAR4/(2*PAR2)      

      PSIY = - PSIY

      PSIXY= 2*Y / PAR1**2 * (1.-0.25*PAR2**2)*(1.+ PAR2*X)*2*PAR2
     >     + (4.- PAR2**2) * (1.+ 0.5*PAR2*(2*X))     *PAR4/(2*PAR2)

      PSIXY = - PSIXY

      RETURN
      END

************************************************************************
*DECK INFO
C***********************************************************************
C***********************************************************************
C**                                                                   **
C**  BELOW FOLLOW THE SOURCES OF THE HGOLIB ROUTINES AS USED IN       **
C**  HELENA :                                                         **
C**                     - GRID2NV      - ZERO                         **
C**                     - RFT2         - RTRAN2                       **
C**                     - RFT          - FFT2                         **
C**                     - PRARR1       - FSUM2                        **
C**                     - PRARR2                                      **
C**                                                                   **
C***********************************************************************
C***********************************************************************

************************************************************************
*DECK PRARR1
      SUBROUTINE PRARR1(NAME,ARRAY,ISIZE,LL)
C
C     ******************************************************************
C     * PRINTS 1D ARRAY(ISIZE) WITH THE TITLE NAME.                    *
C     * IF ISIZE.LT.0, INDEX I STARTS COUNTING FROM 0 (RATHER THAN 1). *
C     * THE ARGUMENT LL HAS THE DOUBLE FUNCTION OF COMMUNICATING THE   *
C     * OUTPUT UNIT IOUT=LL/10 AND THE FORMAT SWITCH L=LL-IOUT*10.     *
C     * IOUT=0 (L=LL): FILE IS "OUTPUT".                               *
C     * L=1: E-FORMAT, L=2: F-FORMAT (WIDTH OF 132 CHARACTERS),        *
C     * L=3: E-FORMAT, L=4: F-FORMAT (WIDTH OF 80 CHARACTERS).         *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      CHARACTER*(*) NAME
      DIMENSION ARRAY(*)
C
      IOUT=LL/10
      L=LL-IOUT*10
      IF(L.EQ.0) THEN
         RETURN
      ELSEIF(L.EQ.1) THEN
         KSTEP=8
         ASSIGN 11 TO IFM
         IF(ISIZE.GE.100) ASSIGN 111 TO IFM
      ELSEIF(L.EQ.2) THEN
         KSTEP=8
         ASSIGN 12 TO IFM
      ELSEIF(L.EQ.3) THEN
         KSTEP=4
         ASSIGN 13 TO IFM
         IF(ISIZE.GE.100) ASSIGN 113 TO IFM
      ELSEIF(L.EQ.4) THEN
         KSTEP=4
         ASSIGN 14 TO IFM
      ENDIF
C
      I0=0
      IS=ISIZE
      IF(ISIZE.LT.0) THEN
         I0=1
         IS=-ISIZE
      ENDIF
C
      IF(IOUT.EQ.0) THEN
         WRITE(   *,10) NAME
         DO 20 K=1,IS,KSTEP
         KPLUS=MIN(K+KSTEP-1,IS)
   20    WRITE(   *,IFM) (ARRAY(I),I-I0,I=K,KPLUS)
      ELSE
         WRITE(IOUT,10) NAME
         DO 200 K=1,IS,KSTEP
         KPLUS=MIN(K+KSTEP-1,IS)
  200    WRITE(IOUT,IFM) (ARRAY(I),I-I0,I=K,KPLUS)
      ENDIF
      RETURN
C
   10 FORMAT(/1X,A/)
   11 FORMAT(1X,8(1PE12.4,'(',0P,I2,')'))
  111 FORMAT(1X,8(1PE11.4,'(',0P,I3,')'))
   12 FORMAT(1X,8(F11.5,'(',I3,')'))
   13 FORMAT(1X,5(1PE12.4,'(',0P,I2,')'))
  113 FORMAT(1X,5(1PE11.4,'(',0P,I3,')'))
   14 FORMAT(1X,5(F11.5,'(',I3,')'))
      END
C
************************************************************************
*DECK PRARR2
      SUBROUTINE PRARR2(NAME,INDS,ARRAY,ISIZE,JSIZE,IMAX,LL)
C
C     ******************************************************************
C     * PRINTS 2D ARRAY(ISIZE,JSIZE) WITH TITLE AND INDICES INDICATED  *
C     * BY THE CHARACTER VARIABLES NAME AND INDS.                      *
C     * ISIZE SHOULD NOT EXCEED THE DIMENSION IMAX DECLARED IN THE     *
C     * CALLING PROGRAM.                                               *
C     * IF ISIZE.LT.0, INDEX I STARTS COUNTING FROM 0 (RATHER THAN 1). *
C     * IF JSIZE.LT.0, INDEX J STARTS COUNTING FROM 0 (RATHER THAN 1). *
C     * THE ARGUMENT LL HAS THE DOUBLE FUNCTION OF COMMUNICATING THE   *
C     * OUTPUT UNIT IOUT=LL/10 AND THE FORMAT SWITCH L=LL-IOUT*10.     *
C     * IOUT=0 (L=LL): FILE IS "OUTPUT".                               *
C     * L=1: E-FORMAT, L=2: F-FORMAT (WIDTH OF 132 CHARACTERS),        *
C     * L=3: E-FORMAT, L=4: F-FORMAT (WIDTH OF 80 CHARACTERS).         *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      CHARACTER*(*) NAME
      CHARACTER*(*) INDS
      CHARACTER*10 INDS1
      DIMENSION ARRAY(IMAX,*)
C
      INDS1=INDS
      IOUT=LL/10
      L=LL-IOUT*10
      IF(L.EQ.0) THEN
         RETURN
      ELSEIF(L.EQ.1) THEN
         KSTEP=8
         ASSIGN 11 TO IFM1
         ASSIGN 21 TO IFM2
      ELSEIF(L.EQ.2) THEN
         KSTEP=12
         ASSIGN 12 TO IFM1
         ASSIGN 22 TO IFM2
      ELSEIF(L.EQ.3) THEN
         KSTEP=5
         ASSIGN 13 TO IFM1
         ASSIGN 23 TO IFM2
      ELSEIF(L.EQ.4) THEN
         KSTEP=8
         ASSIGN 14 TO IFM1
         ASSIGN 24 TO IFM2
      ENDIF
C
      I0=0
      IS=ISIZE
      IF(ISIZE.LT.0) THEN
         I0=1
         IS=-ISIZE
      ENDIF
      J0=0
      JS=JSIZE
      IF(JSIZE.LT.0) THEN
         J0=1
         JS=-JSIZE
      ENDIF
C
      IF(IOUT.EQ.0) THEN
         WRITE(   *,10) NAME
         DO 20 K=1,JS,KSTEP
         KPLUS=MIN(K+KSTEP-1,JS)
         WRITE(   *,IFM1) INDS1,(J-J0,J=K,KPLUS)
         WRITE(   *,*)
         DO 20 I=1,IS
   20    WRITE(   *,IFM2) I-I0,(ARRAY(I,J),J=K,KPLUS)
      ELSE
         WRITE(IOUT,10) NAME
         DO 200 K=1,JS,KSTEP
         KPLUS=MIN(K+KSTEP-1,JS)
         WRITE(IOUT,IFM1) INDS1,(J-J0,J=K,KPLUS)
         WRITE(IOUT,*)
         DO 200 I=1,IS
  200    WRITE(IOUT,IFM2) I-I0,(ARRAY(I,J),J=K,KPLUS)
      ENDIF
      RETURN
C
   10 FORMAT(/1X,A)
   11 FORMAT(/2X,A10,I7,7I13)
   12 FORMAT(/2X,A10,I3,11I9)
   13 FORMAT(/2X,A10,I7,4I13)
   14 FORMAT(/2X,A10,I3,7I9)
   21 FORMAT(1X,I3,2X,1P,8E13.4)
   22 FORMAT(1X,I3,2X,12F9.5)
   23 FORMAT(1X,I3,2X,1P,5E13.4)
   24 FORMAT(1X,I3,2X,8F9.5)
      END

************************************************************************
*DECK ZERO
      SUBROUTINE ZERO(X1,Y1,X2,Y2,FUNC,ERR,X,Y,IZERO,LL)
C
C     ******************************************************************
C     * THE ZERO Y=FUNC(X)=0 ON THE INTERVAL (X1,X2) IS FOUND.         *
C     * THE FUNCTION FUNC(X) SHOULD BE PROVIDED BY AN EXTERNAL.        *
C     * UPON RETURN IZERO IS THE NUMBER OF ITERATIONS WHICH WERE       *
C     * REQUIRED TO OBTAIN DABS(Y).LE.ERR.                              *
C     * DIAGNOSTIC INFORMATION IS PRINTED IF L.NE.0.                   *
C     * THE ARGUMENT LL HAS THE DOUBLE FUNCTION OF COMMUNICATING THE   *
C     * OUTPUT UNIT IOUT=LL/10 AND THE PRINT SWITCH L=LL-IOUT*10.      *
C     * IOUT=0 (L=LL): FILE IS "OUTPUT".                               *
C     * MODIFIED BY JAN REM TO IMPROVE CONVERGENCE 25/08/84.           *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)  
      EXTERNAL FUNC
C
      DOUBLE PRECISION X1, Y1, X2, Y2, X, Y, Y0, YN, ERR
      IOUT=LL/10
      L=LL-IOUT*10
      NIZERO=50
      IF((Y1.LT.0..AND.Y2.LT.0.).OR.(Y1.GT.0..AND.Y2.GT.0.)) THEN
         IF(IOUT.EQ.0) WRITE(   *,5)
         IF(IOUT.NE.0) WRITE(IOUT,5)
         RETURN
      ENDIF
      IF(L.NE.0) THEN
         IF(IOUT.EQ.0) WRITE(   *,6)
         IF(IOUT.NE.0) WRITE(IOUT,6)
      ENDIF
      X1S=X1
      Y1S=Y1
      X2S=X2
      Y2S=Y2
      IF(X1.GT.X2) THEN
         X1=X2S
         Y1=Y2S
         X2=X1S
         Y2=Y1S
      ENDIF
      SIG=1.D0
      IF(Y1.GE.0.) THEN
         SIG=-1.D0
         Y1=-Y1
         Y2=-Y2
      ENDIF
C
C     ***BEGIN LOOP ON IZERO***
      IZERO=0
   10 X0=X1-(X2-X1)*Y1/(Y2-Y1)
      Y0=SIG*FUNC(X0)
      IZERO=IZERO+1
      IF(L.NE.0) THEN
         IF(IOUT.EQ.0) WRITE(   *,11) IZERO,X1,Y1,X2,Y2,X0,Y0
         IF(IOUT.NE.0) WRITE(IOUT,11) IZERO,X1,Y1,X2,Y2,X0,Y0
      ENDIF
      IF(DABS(Y0).LE.ERR) GOTO 20
      IF(DABS(Y0).GE.0.2*MIN(-Y1,Y2)) THEN
         A=((Y2-Y0)/(X2-X0)-(Y0-Y1)/(X0-X1))/(X2-X1)
         B=(Y2-Y1)/(X2-X1)-A*(X2+X1)
         C=Y0-A*X0*X0-B*X0
         XN=(-B+DSQRT(B*B-4.D0*A*C))/(2.D0*A)
         YN=SIG*FUNC(XN)
         IZERO=IZERO+1
         IF(L.NE.0) THEN
            IF(IOUT.EQ.0) WRITE(   *,12) IZERO,X1,Y1,X2,Y2,XN,YN
            IF(IOUT.NE.0) WRITE(IOUT,12) IZERO,X1,Y1,X2,Y2,XN,YN
         ENDIF
         IF(DABS(YN).LE.ERR) GOTO 30
         IF(YN.LT.0.) THEN
            X1=XN
            Y1=YN
            IF(Y0.GT.0.) THEN
               X2=X0
               Y2=Y0
            ENDIF
         ELSE
            X2=XN
            Y2=YN
            IF(Y0.LT.0.) THEN
               X1=X0
               Y1=Y0
            ENDIF
         ENDIF
      ELSEIF(Y0.GT.ERR) THEN
         X2=X0
         Y2=Y0
      ELSE
         X1=X0
         Y1=Y0
      ENDIF
      IF(IZERO.LT.NIZERO) GOTO 10
C     ***END LOOP ON IZERO***
      IF(IOUT.EQ.0) WRITE(   *,13) NIZERO
      IF(IOUT.NE.0) WRITE(IOUT,13) NIZERO
C
   20 X=X0
      Y=SIG*Y0
      X1=X1S
      Y1=Y1S
      X2=X2S
      Y2=Y2S
      RETURN
C
   30 X=XN
      Y=SIG*YN
      X1=X1S
      Y1=Y1S
      X2=X2S
      Y2=Y2S
      RETURN
C
C     * FORMATS.
    5 FORMAT(/1X,'***SUBROUTINE ZERO: Y1 AND Y2 VIOLATE REQUIREMENTS')
    6 FORMAT(/30X,'==== SUBROUTINE ZERO ====')
   11 FORMAT(1X,'IZERO=',I3,3X,'X1=',1PE12.4,3X,'Y1=',1PE12.4,
     A       3X,'X2=',1PE12.4,3X,'Y2=',1PE12.4,3X,'X0=',1PE12.4,
     B       3X,'Y0=',1PE12.4)
   12 FORMAT(1X,'IZERO=',I3,3X,'X1=',1PE12.4,3X,'Y1=',1PE12.4,
     A       3X,'X2=',1PE12.4,3X,'Y2=',1PE12.4,3X,'XN=',1PE12.4,
     B       3X,'YN=',1PE12.4)
   13 FORMAT(/1X,'***SUBROUTINE ZERO: NO CONVERGENCE FOR X IN ',I3,
     A       ' STEPS')
      END
C
************************************************************************
*DECK RFT2
      SUBROUTINE RFT2(DATA,NR,KR)
C
C     ******************************************************************
C     * DOUBLE PRECISION  FOURIER TRANSFORM.                                        *
C     * INPUT:  NR DOUBLE PRECISION  COEFFICIENTS                                   *
C     *             DATA(1),DATA(1+KR),....,DATA(1+(NR-1)*KR).         *
C     * OUTPUT: NR/2+1 COMPLEX COEFFICIENTS                            *
C     *            (DATA(1),      DATA(1+KR))                          *
C     *            (DATA(1+2*KR), DATA(1+3*KR))                        *
C     *             .............................                      *
C     *            (DATA(1+NR*KR),DATA(1+(NR+1)*KR).                   *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS. (I.E., NR+2 IF INCREMENT KR=1).          *
C     * LASL ROUTINE MAY 75, CALLING FFT2 AND RTRAN2.                  *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DIMENSION DATA(*)
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,-(KR+KR))
      CALL RTRAN2(DATA,NR,KR,1)
      RETURN
      END
C
C
************************************************************************
*DECK RTRAN2
      SUBROUTINE RTRAN2(DATA,NR,KR,KTRAN)
C
C     ******************************************************************
C     * INTERFACE BETWEEN RFT2, RFI2, AND FFT2.                        *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS.                                          *
C     * LASL ROUTINE MAY 75, CALLED FROM RFT2 AND RFI2.                *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION DATA(*), THETA
      KS=2*KR
      N=NR/2
      NMAX=N*KS+2
      KMAX=NMAX/2
      THETA=1.5707963267949D0/N
      DC=2.D0*DSIN(THETA)**2
      DS=DSIN(2.D0*THETA)
      WS=0.D0
      IF(KTRAN.LE.0) THEN
         WC=-1.0D0
         DS=-DS
      ELSE
         WC=1.0D0
         DATA(NMAX-1)=DATA(1)
         DATA(NMAX-1+KR)=DATA(KR+1)
      ENDIF
      DO 10 K=1,KMAX,KS
         NK=NMAX-K
         SUMR=.5D0*(DATA(K)+DATA(NK))
         DIFR=.5D0*(DATA(K)-DATA(NK))
         SUMI=.5D0*(DATA(K+KR)+DATA(NK+KR))
         DIFI=.5D0*(DATA(K+KR)-DATA(NK+KR))
         TR=WC*SUMI-WS*DIFR
         TI=WS*SUMI+WC*DIFR
         DATA(K)=SUMR+TR
         DATA(K+KR)=DIFI-TI
         DATA(NK)=SUMR-TR
         DATA(NK+KR)=-DIFI-TI
         WCA=WC-DC*WC-DS*WS
         WS=WS+DS*WC-DC*WS
         WC=WCA
   10 CONTINUE
      RETURN
      END
C
************************************************************************
*DECK FFT2
      SUBROUTINE FFT2 (DATAR,DATAI,N,INC)
C
C     ******************************************************************
C     * FFT2 FORTRAN VERSION CLAIR NIELSON MAY 75.                     *
C     ******************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION THETA, DATAR(*),DATAI(*)
      KTRAN=ISIGN(-1,INC)
      KS=IABS(INC)
      IP0=KS
      IP3=IP0*N
      IREV=1
      DO 20 I=1,IP3,IP0
         IF(I.LT.IREV) THEN
            TEMPR=DATAR(I)
            TEMPI=DATAI(I)
            DATAR(I)=DATAR(IREV)
            DATAI(I)=DATAI(IREV)
            DATAR(IREV)=TEMPR
            DATAI(IREV)=TEMPI
         ENDIF
         IBIT=IP3/2
   10    IF(IREV.GT.IBIT) THEN
            IREV=IREV-IBIT
            IBIT=IBIT/2
            IF(IBIT.GE.IP0) GOTO 10
         ENDIF
   20    IREV=IREV+IBIT
      IP1=IP0
      THETA=REAL(KTRAN)*3.1415926535898D0
   30 IF(IP1.GE.IP3) GOTO 60
      IP2=IP1+IP1
      DSINTH=DSIN(.5D0*THETA)
      WSTPR=-2.D0*DSINTH*DSINTH
      WSTPI=DSIN(THETA)
      WR=1.D0
      WI=0.D0
      DO 50 I1=1,IP1,IP0
         DO 40 I3=I1,IP3,IP2
            J0=I3
            J1=J0+IP1
            TEMPR=WR*DATAR(J1)-WI*DATAI(J1)
            TEMPI=WR*DATAI(J1)+WI*DATAR(J1)
            DATAR(J1)=DATAR(J0)-TEMPR
            DATAI(J1)=DATAI(J0)-TEMPI
            DATAR(J0)=DATAR(J0)+TEMPR
   40       DATAI(J0)=DATAI(J0)+TEMPI
         TEMPR=WR
         WR=WR*WSTPR-WI*WSTPI+WR
   50    WI=WI*WSTPR+TEMPR*WSTPI+WI
      IP1=IP2
      THETA=.5D0*THETA
      GOTO 30
   60 RETURN
      END

************************************************************************
*DECK GRID2NV
      SUBROUTINE GRID2NV(TIN,TOUT,JPTS,ACC,IGRD,LL)                       
C------------------------------------------------------------------------
C  THE FUNCTION TIN(TOUT), GIVEN ON THE GRID TOUT=2*PI*(J-1)/JPTS,        
C  IS INVERTED TO GIVE TOUT(TIN) ON THE GRID TIN=2*PI*(I-1)/JPTS.         
C  THIS IS DONE BY DETERMINING THE ZEROS OF THE FUNCTION                  
C     Y(T)=T+SUM(GF(M)*DSIN(M*T))-2*PI*(I-1)/JPTS,                         
C  WHERE GF(M) ARE THE FOURIER COEFFICIENTS OF G(T)=TIN(T)-T.             
C  DIAGNOSTIC INFORMATION IS PRINTED IF L.NE.0.                           
C  THE ARGUMENT LL HAS THE DOUBLE FUNCTION OF COMMUNICATING THE           
C  OUTPUT UNIT IOUT=LL/10 AND THE PRINT SWITCH L=LL-IOUT*10.              
C  IOUT=0 (L=LL): FILE IS "OUTPUT".                                       
C-----------------------------------------------------------------------  
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      PARAMETER (JMAX=1024,NINV=100)                                       
      DIMENSION TIN(*),TOUT(*)
      DOUBLE PRECISION T(JMAX+1),G(JMAX+1),GFDCOS(JMAX/2-1),       
     >          GFDSIN(JMAX/2-1)                                           
      EQUIVALENCE(T(1),G(1))                                              
C
      DOUBLE PRECISION PI                                                                       
      PI=3.1415926535898                                                  
      MHARM=JPTS/2-1                                                      
C                                                                         
      DO 9 JJ=2,JPTS                                                      
        IF (TIN(JJ-1).GT.TIN(JJ))  TIN(JJ)=TIN(JJ)+2*PI                   
    9 CONTINUE                                                            
      IOUT=LL/10                                                          
      L=LL-IOUT*10                                                        
      IF(L.NE.0) THEN                                                     
         IF(IOUT.EQ.0) WRITE(   *,3)                                      
         IF(IOUT.NE.0) WRITE(IOUT,3)                                      
      ENDIF                                                               
      DO 10 I=1,JPTS                                                      
   10 G(I)=TIN(I)-2.*PI*(I-1.)/JPTS                                       
      CALL RFT(G,GFNUL,GFDCOS,GFDSIN,JPTS,MHARM)                            
      IF(L.NE.0) THEN                                                     
         CALL PRARR1('TIN(J) : ',TIN,JPTS,IOUT*10+L)                      
         CALL PRARR1('G(I):',T,JPTS,IOUT*10+L)                            
         IF(IOUT.EQ.0) WRITE(20,56) GFNUL                                  
         IF(IOUT.NE.0) WRITE(IOUT,56) GFNUL                               
         CALL PRARR1('GFDCOS(M):',GFDCOS,MHARM,IOUT*10+L)                   
         CALL PRARR1('GFDSIN(M):',GFDSIN,MHARM,IOUT*10+L)                   
      ENDIF                                                               
      DO 20 I=1,JPTS+1                                                    
        T(I)=2.D0*PI*(I-1.D0)/JPTS                                            
   20 CONTINUE                                                            
      J1=1                                                                
      IGRDNV=1                                                            
      IFIRST=0                                                            
      ICIRC= - (INT(TIN(1)/(2*PI)+10000) -  9999)                         
      IF (DABS(TIN(1)).LT.1D-12) ICIRC=0                                   
      DO 80 I=1,JPTS                                                      
        J=J1                                                              
        T1=T(J) + ICIRC*2*PI                                              
        CALL FSUM2(SUM1,T1,GFNUL,GFDCOS,GFDSIN,MHARM)                       
        Y1=T1+SUM1-T(I)                                                   
   30   CONTINUE                                                          
          T0=T1                                                           
          Y0=Y1                                                           
          IF (DABS(Y0).LE.ACC) THEN                                        
            TOUT(I)=T0                                                    
            GOTO 80                                                       
          ENDIF                                                           
          IF (J.NE.JPTS+1) GOTO 31                                        
            IF (IFIRST.EQ.0)  THEN                                        
              J=1                                                         
              ICIRC=ICIRC+1                                               
              IFIRST=1                                                    
            ELSE                                                          
              WRITE(IOUT,70)                                              
              GOTO 90                                                     
            ENDIF                                                         
   31     J=J+1                                                          
          T1=T(J) + ICIRC*2*PI                                            
          CALL FSUM2(SUM1,T1,GFNUL,GFDCOS,GFDSIN,MHARM)                     
          Y1=T1+SUM1-T(I)                                                 
          IF(DSIGN(1.D0,Y0).EQ.DSIGN(1.D0,Y1)) GOTO 30                          
        J1=J-1                                                            
        DO 40 N=1,NINV                                                    
          T2=T0-(T1-T0)*Y0/(Y1-Y0)                                        
          CALL FSUM2(SUM2,T2,GFNUL,GFDCOS,GFDSIN,MHARM)                     
          Y2=T2+SUM2-T(I)                                                 
          IF(L.NE.0) THEN                                                 
             IF(IOUT.EQ.0) WRITE(   *,25) N,T0,T1,Y0,Y1,T2,Y2,J           
             IF(IOUT.NE.0) WRITE(IOUT,25) N,T0,T1,Y0,Y1,T2,Y2,J           
          ENDIF                                                           
          IF(DABS(Y2).LE.ACC) GOTO 50                                      
          IF(DSIGN(1.D0,Y2).EQ.DSIGN(1.D0,Y1)) GOTO 45                          
          T0=T2                                                           
          Y0=Y2                                                           
          GOTO 40                                                         
   45     T1=T2                                                           
          Y1=Y2                                                           
   40   CONTINUE                                                          
   50   TOUT(I)=T2                                                        
        IF(L.NE.0) THEN                                                   
          IF(IOUT.EQ.0) WRITE(   *,55) I,N                                
          IF(IOUT.NE.0) WRITE(IOUT,55) I,N                                
        ENDIF                                                             
        IF(N.GT.IGRDNV) IGRDNV=N                                          
   80 CONTINUE                                                            
   90 RETURN                                                              
C                                                                         
C     * FORMATS.                                                          
    3 FORMAT(///1X,'SUBROUTINE GRIDINV')                                  
   25 FORMAT(1X,'N=',I3,' T0=',F10.5,' T1=',F10.5,                        
     A       ' Y0=',F10.5,' Y1=',F10.5,' T2=',F10.5,' Y2=',F10.5,         
     B       ' J=',I3)                                                    
   55 FORMAT(1X,'I=',I3,5X,'N=',I3)                                       
   56 FORMAT(/1X,'GFNUL = ',1PE12.4)                                      
   70 FORMAT(/1X,'***SUBROUTINE GRIDINV: NO ZERO FOUND ')                 
      END                                                                 

************************************************************************
*DECK FSUM2
      SUBROUTINE FSUM2(F,T,FFNUL,FFDCOS,FFDSIN,MHARM)                       
C-----------------------------------------------------------------------  
C FOURIER SYNTHESIS OF GENERAL  FUNCTION F(T) AT DSINGLE POINT T.          
C-----------------------------------------------------------------------  
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DIMENSION FFDCOS(*),FFDSIN(*)                                         
      CO=DCOS(T)                                                           
      SI=DSIN(T)                                                           
      C=1.                                                                
      S=0.                                                                
      SUM=.5*FFNUL                                                        
      DO 10 M=1,MHARM                                                     
        CA=C*CO-S*SI                                                      
        S=S*CO+C*SI                                                       
        C=CA                                                              
        SUM=SUM+FFDCOS(M)*C + FFDSIN(M)*S                                   
   10 CONTINUE                                                            
      F=SUM                                                               
      RETURN                                                              
      END                                                                 
                                                                          
************************************************************************  
*DECK RFT
      SUBROUTINE RFT(F,FFNUL,FFDCOS,FFDSIN,JPTS,MHARM)                      
C-----------------------------------------------------------------------  
C  CALCULATES FOURIER CODSINE AND DSINE COEFFICIENTS FFDCOS AND              
C  FFDSIN OF THE ARRAY FF CORRESPONDING TO THE  FUNCTION                   
C  F(T)=.5*FFNUL+SUM(FFDCOS(M)*DCOS(M*T)+FFDSIN(M)*DSIN(M*T))                 
C  WHERE MHARM.LE.JPTS/2-1, FFNUL=FF(0) AND T=2*PI*(J-1)/JPTS.            
C  THE INPUT ARRAY F(J) IS NOT DESTROYED BY CALLING RFTDCOS.               
C  TYPICAL USE IS FOR MHARM MUCH SMALLER THAN JPTS/2-1, SO THAT           
C  RFT2 CANNOT BE USED DIRECTLY.                                          
C----------------------------------------------------------------------- 
      IMPLICIT DOUBLE PRECISION(A-H, O-Z) 
      PARAMETER (JMAX=1024)                                                
      DIMENSION F(*),FFDCOS(*),FFDSIN(*),FSTORE(JMAX+2)                     
      DO 10 J=1,JPTS                                                      
   10 FSTORE(J)=F(J)                                                      
      CALL RFT2(FSTORE,JPTS,1)                                            
      FAC=2./JPTS                                                         
      FFNUL=FSTORE(1)*FAC                                                 
      DO 20 M=1,MHARM                                                    
        FFDCOS(M)=FSTORE(2*M+1)*FAC                                        
        FFDSIN(M) = - FSTORE(2*M+2)*FAC                                    
   20 CONTINUE                                                            
      RETURN                                                             
      END                                                                 
      
************************************************************************
*DECK RFI2
      SUBROUTINE RFI2(DATA,NR,KR)                                       
C-----------------------------------------------------------------------
C  INVERSE OF RFT2.                                               
C  WHEN USING RFI2 IT IS NECESSARY TO HAVE VANISHING IMAGINARY   
C  PARTS OF THE FIRST AND LAST ELEMENT OF THE INPUT VECTOR: 
C  DATA(1+KR)=DATA(1+(NR+1)*KR)=0.                              
C  THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST 
C    (NR+1)*KR+1 ELEMENTS.                                          
C  LASL ROUTINE MAY 75, CALLING RTRAN2 AND FFT2.                  
C -----------------------------------------------------------------------
C                
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)                                                       
      DIMENSION DATA(*)                                                 
      CALL RTRAN2(DATA,NR,KR,-1)                                        
      MR=NR*KR                                                          
      FNI=2./NR                                                         
      DO 10 I=1,MR,KR                                                   
   10 DATA(I)=FNI*DATA(I)                                               
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,(KR+KR))                        
      RETURN                                                            
      END                                                               

**************************************************************************
*DECK MNEWTAX      
      SUBROUTINE mnewtax(ntrial,x,n,tolx,tolf,errx,errf)
C------------------------------------------------------------------------- 
C ROUTINE TO SOLVE TWO NONLINEAR EQUATIONS UDSING NEWTONS METHOD FROM
C NUMERICAL RECIPES.
C LU DECOMPOSITION REPLACED BY DEXPLICIT SOLUTION OF 2X2 MATRIX. 
C-------------------------------------------------------------------------     
      USE PARAM
      USE CORNERS
      USE FAXIS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      PARAMETER (NEQ=2)
      DOUBLE PRECISION  X(NEQ),FVEC(NEQ),FJAC(NEQ,NEQ)
      INTEGER n,ntrial
      DOUBLE PRECISION  tolf,tolx
      INTEGER i,k,indx(NEQ)
      DOUBLE PRECISION  d,errf,errx,p(NEQ)
      
      do 14  k=1,ntrial
c----------------------------- usrfun iserted here -----------------------
        R = X(1)
        S = X(2)
        N1 = NODENO(NAXIS,1)
        N2 = NODENO(NAXIS,2)
        N3 = NODENO(NAXIS,3)
        N4 = NODENO(NAXIS,4)
        CALL INTERP(PSI(4*(N1-1)+1),PSI(4*(N2-1)+1),
     >            PSI(4*(N3-1)+1),PSI(4*(N4-1)+1),
     >            R,S,ZPSI,ZPSIR,ZPSIS,ZPSIRS,ZPSIRR,ZPSISS)
        FVEC(1) = ZPSIR
        FVEC(2) = ZPSIS
        FJAC(1,1) = ZPSIRR
        FJAC(1,2) = ZPSIRS
        FJAC(2,1) = ZPSIRS
        FJAC(2,2) = ZPSISS
c-------------------------------------------------------------------------      
        errf=0.
        do 11 i=1,n
          errf=errf+DABS(fvec(i))
11      continue
        if(errf.le.tolf)return
        do 12 i=1,n
          p(i)=-fvec(i)
12      continue
        temp = p(1)
        dis = fjac(2,2)*fjac(1,1)-fjac(1,2)*fjac(2,1)
        p(1) = (fjac(2,2)*p(1)-fjac(1,2)*p(2))/dis
        p(2) = (fjac(1,1)*p(2)-fjac(2,1)*temp)/dis      
        errx=0.
        do 13 i=1,n
          errx=errx+DABS(p(i))
          x(i)=x(i)+p(i)
13      continue
        if(errx.le.tolx)return
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *N*1V45_Lt+V'.


************************************************************************
*DECK SPLINE
      SUBROUTINE SPLINE(N,X,Y,ALFA,BETA,TYP,A,B,C,D)
C-----------------------------------------------------------------------
C     INPUT:
C
C     N     ANZAHL DER KNOTEN
C     X     ARRAY DER X-WERTE
C     Y     ARRAY DER Y-WERTE
C     ALFA  RANDBEDINGUNG IN X(1)
C     BETA        "       IN X(N)
C     TYP   =  0  NOT-A-KNOT SPLINE
C              1  ALFA, BETA 1. ABLEITUNGEN VORGEGEBEN
C              2    "    "   2.     "           "
C              3    "    "   3.     "           "
C
C     BEMERKUNG: MIT TYP = 2 UND ALFA = BETA = 0 ERHAELT MAN
C           EINEN NATUERLICHEN SPLINE
C
C     OUTPUT:
C
C     A, B, C, D     ARRAYS DER SPLINEKOEFFIZIENTEN
C       S = A(I) + B(I)*(X-X(I)) + C(I)*(X-X(I))**2+ D(I)*(X-X(I))**3
C
C     BEI ANWENDUNGSFEHLERN WIRD DAS PROGRAMM MIT ENTSPRECHENDER
C     FEHLERMELDUNG ABGEBROCHEN
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER  N, TYP
      DOUBLE PRECISION      X(N), Y(N), ALFA, BETA, A(N), B(N),C(N),D(N)
      INTEGER  I, IERR
      DOUBLE PRECISION      H(1001)
      
C
      IF((TYP.LT.0).OR.(TYP.GT.3)) THEN
         WRITE(*,*) ' ERROR IN ROUTINE SPLINE: WRONG TYPE'
         STOP
      ENDIF
C
      IF (N.LT.3) THEN
         WRITE(*,*) ' ERROR IN ROUTINE  SPLINE: N < 3 '
         STOP
      ENDIF
C
C
C     BERECHNE DIFFERENZ AUFEINENDERFOLGENDER X-WERTE UND
C     UNTERSUCHE MONOTONIE
C
      DO I = 1, N-1
        H(I) = X(I+1)- X(I)
        IF ( H(I).LE. 0.0) THEN
         WRITE(*,*) ' NON-MONOTONIC COORDINATE IN SPLINE: X(I-1)>=X(I)'
         STOP
       ENDIF
      ENDDO
C
C     AUFSTELLEN DES GLEICHUNGSSYSTEMS
C
      DO I = 1, N-2
         A(I) = 3.0 * ((Y(I+2)-Y(I+1)) / H(I+1) - (Y(I+1)-Y(I)) / H(I))
         B(I) = H(I)
         C(I) = H(I+1)
         D(I) = 2.0 * (H(I) + H(I+1))
      ENDDO
C
C     BERUECKSICHTIGEN DER RANDBEDINGUNGEN
C
C     NOT-A-KNOT
C
      IF (TYP.EQ.0) THEN
         A(1)   = A(1) * H(2) / (H(1) + H(2))
         A(N-2) = A(N-2) * H(N-2) / (H(N-1) + H(N-2))
         D(1)   = D(1) - H(1)
         D(N-2) = D(N-2) - H(N-1)
         C(1)   = C(1) - H(1)
         B(N-2) = B(N-2) - H(N-1)
      ENDIF
C
C     1. ABLEITUNG VORGEGEBEN
C
      IF (TYP.EQ.1) THEN
         A(1)   = A(1) - 1.5 * ((Y(2)-Y(1)) / H(1) - ALFA)
         A(N-2) = A(N-2) - 1.5 * (BETA - (Y(N)-Y(N-1)) / H(N-1))
         D(1)   = D(1) - 0.5 * H(1)
         D(N-2) = D(N-2) - 0.5 * H(N-1)
      ENDIF
C
C     2. ABLEITUNG VORGEGEBEN
C
      IF (TYP.EQ.2) THEN
         A(1)   = A(1) - 0.5 * ALFA * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)
      ENDIF
C
C     3. ABLEITUNG VORGEGEBEN
C
      IF (TYP.EQ.3 ) THEN
         A(1)   = A(1) + 0.5 * ALFA * H(1) * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)* H(N-1)
         D(1)   = D(1) + H(1)
         D(N-2) = D(N-2) + H(N-1)
      ENDIF
C
C     BERECHNUNG DER KOEFFIZIENTEN
C
      CALL SGTSL(N-2,B,D,C,A,IERR)

      IF(IERR.NE.0) THEN
         WRITE(*,21)
         STOP
      ENDIF
C
C     UEBERSCHREIBEN DES LOESUNGSVEKTORS
C
      CALL DCOPY(N-2,A,1,C(2),1)
C
C     IN ABHAENGIGKEIT VON DEN RANDBEDINGUNGEN WIRD DER 1. UND
C     DER LETZTE WERT VON C KORRIGIERT
C
      IF (TYP.EQ.0) THEN
         C(1) = C(2) + H(1) * (C(2)-C(3)) / H(2)
         C(N) = C(N-1) + H(N-1) * (C(N-1)-C(N-2)) / H(N-2)
      ENDIF
C
      IF (TYP.EQ.1) THEN
         C(1) = 1.5*((Y(2)-Y(1)) / H(1) - ALFA) / H(1) - 0.5 * C(2)
         C(N) = -1.5*((Y(N)-Y(N-1)) / H(N-1)-BETA) / H(N-1)-0.5*C(N-1)
      ENDIF
C
      IF (TYP.EQ.2) THEN
         C(1) = 0.5 * ALFA
         C(N) = 0.5 * BETA
      ENDIF
C
      IF (TYP.EQ.3) THEN
         C(1) = C(2) - 0.5 * ALFA * H(1)
         C(N) = C(N-1) + 0.5 * BETA * H(N-1)
      ENDIF
C
      CALL DCOPY(N,Y,1,A,1)
C
      DO I = 1, N-1
         B(I) = (A(I+1)-A(I)) / H(I) - H(I) * (C(I+1)+2.0 * C(I)) / 3.0
         D(I) = (C(I+1)-C(I)) / (3.0 * H(I))
      ENDDO
C
      B(N) = (3.0 * D(N-1) * H(N-1) + 2.0 * C(N-1)) * H(N-1) + B(N-1)
C
      RETURN
C
   21 FORMAT(1X,'ERROR IN SGTSL: MATRIX DSINGULAR')
      END

************************************************************************
*DECK SPWERT
      DOUBLE PRECISION  FUNCTION SPWERT(N,XWERT,A,B,C,D,X,ABLTG)
C-----------------------------------------------------------------------
C     INPUT:
C
C     N           ANZAHL DER KNOTENPUNKTE
C     XWERT       STELLE AN DER FUNKTIONSWERTE BERECHNET WERDEN
C     A, B, C, D  ARRAYS DER SPLINEKOEFFIZIENTEN (AUS SPLINE)
C     X           ARRAY DER KNOTENPUNKTE
C
C     OUTPUT:
C
C     SPWERT   FUNKTIONSWERT AN DER STELLE XWERT
C     ABLTG(I) WERT DER I-TEN ABLEITUNG BEI XWERT
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER  N
      DOUBLE PRECISION   XWERT, A(N), B(N), C(N), D(N), X(N), ABLTG(3)
      INTEGER  I, K, M
C
C     SUCHE PASSENDES INTERVALL (BINAERE SUCHE)
C
      I = 1
      K = N
C
   10 M = (I+K) / 2
C
      IF(M.NE.I) THEN
         IF(XWERT.LT.X(M)) THEN
            K = M
         ELSE
            I = M
         ENDIF
         GOTO 10
      ENDIF
C
      XX = XWERT - X(I)
C
      ABLTG(1) = (3.0 * D(I) * XX + 2.0 * C(I)) * XX + B(I)
      ABLTG(2) = 6.0 * D(I) * XX + 2.0 * C(I)
      ABLTG(3) = 6.0 * D(I)
C
      SPWERT = ((D(I)*XX + C(I))*XX + B(I))*XX + A(I)
C
      RETURN
      END

************************************************************************
*DECK HELBAL
      SUBROUTINE HELBAL(ZVOL,ZVOLP,XAXIS)
C-----------------------------------------------------------------------
C PROGRAM TO DETERMINE THE BALLOONING STABILITY OF HELENA EQUILIBRIA
C         - READS THE MAPPING FILE AS USED BY CASTOR
C         - CALCULATES STABILITY INDEX UDSING SUYDAM METHOD
C         - FOR SYMMETRIC AND ASYMMETRIC PLASMA BOUNDARY SHAPES
C
C         VERSION : 1                 DATE : 28-09-95
C-----------------------------------------------------------------------
C STATUS : 
C
C 28/9/95   - tested for symmetric soloviev equilibrium with E=1.41
C             eps=0.382, compares well with Turnbull results.
C           - also tested for sym. soloviev asymmetric helena
C
C-----------------------------------------------------------------------
      USE PARAM
      USE COMMAX
      USE COMPIO
      USE COMDAT
      USE COMMAP
      USE COMSPL
      USE COMB02
      USE COMPQ
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  ZVOL(*),ZVOLP(*),XAXIS
      CHARACTER*25 BAL,INIBAL

      PI = 2.*DASIN(1.D0)
      CALL INIT(IAS)
C---------------------- READ HELENA MAPPING FILE -----------------------      
      CALL IODSK
C---------------------- CALCULATE B-FIELD ON GRID POINTS ---------------     
      CALL BFIELD(IAS)
C---------------------- CALCULATE P AND Q COEFF. ON ALL GRIDPOINTS -----      
      CALL PQ(IAS)
C---------------------- CALCULATE STABILITY INDEX ----------------------     
      WRITE(20,*)
      WRITE(20,*) '****************************************************'
     >            //'***************************'
      WRITE(20,*) '* I, FLUX,  RHO,   Q,    SHEAR,   SHEAR1, ALPHA,'//
     >              '  ALPHA1,  FMARG,  BALLOONING *'
      WRITE(20,*) '****************************************************'
     >            //'***************************'
      DO 10 IPSI=2,NPSI
         FACT = 1.
         CALL SUYDAM(IPSI,0.D0,TBB,TBF,NCPQ,1.D0,BAL)
C---------------------- FIND DISDTANCE FROM STABILITY BOUNDARY ----------         
         INIBAL = BAL
         IF (BAL.EQ.' STABLE') THEN
            DELF = 2.
         ELSE
            DELF = 0.5
         ENDIF      
         FACT = DELF * FACT
         DO 20 NIT=1,10
           CALL SUYDAM(IPSI,0.D0,TBB,TBF,NCPQ,FACT,BAL)
           IF (BAL.EQ.INIBAL) THEN
              FACT = DELF * FACT
           ELSE
              GOTO 30
           ENDIF  
   20   CONTINUE
C--------------------- UPPER AND LOWER LIMII ESTABLISHED --------------    
   30   CONTINUE
        IF (INIBAL.EQ.' STABLE') THEN
          FUNST = FACT
          FSTAB = FACT / 2.
        ELSE
          FSTAB = FACT
          FUNST = FACT * 2.
        ENDIF   
c------------------------------------- BISECTION TO FIND FACTOR
        DO 40 NIT = 1, 10 
           FACT = (FUNST + FSTAB)/2.
           CALL SUYDAM(IPSI,0.D0,TBB,TBF,NCPQ,FACT,BAL)
           IF (BAL.EQ.' STABLE') THEN
             FSTAB = FACT
           ELSE
             FUNST = FACT
           ENDIF               
   40   CONTINUE      
        SHEAR = CS(IPSI)/QS(IPSI) * DQS(IPSI) 
        ALPHA = - 2.*QS(IPSI)**2 * P2(IPSI)/EPS

        SHEAR1 = 2.*ZVOL(IPSI)/QS(IPSI) * DQS(IPSI)
     >         / (2.*CS(IPSI)*ZVOLP(IPSI))
c-------------------------------------------- Lao's definition
c        ALPHA1 = -P2(IPSI)/(2.*CS(IPSI)) * ZVOLP(IPSI) / CPSURF**2
c     >         * DSQRT(ZVOL(IPSI)/(4.*PI*(1.+EPS*XAXIS))) * EPS**3
c
c--------------------------------------------- use rho as radius
        RHO = DSQRT(ZVOL(IPSI)/ZVOL(NR))
        DRHODS =  CS(IPSI)/ (RHO*ZVOL(NR)) * ZVOLP(IPSI)  
        ALPHA2 = ALPHA / DRHODS

c----------------------------------------- Lao corrected? Needs check
c        ALPHA11 = -P2(IPSI)/(2.*CS(IPSI)) * ZVOLP(IPSI) / CPSURF**2
c     >          / (PI*PI*EPS) * RHO * ZVOL(NR) *EPS**4
c----------------------------------------------------------------------
	
	FMARG = (FSTAB+FUNST)/2.
C----------------------------------- temporary fix for negative shear and IAS=1
        IF ((SHEAR<0.).AND.(IAS.EQ.1)) THEN   
	  FMARG = 0.
	  INIBAL =  ' STABLE'
        ENDIF
        WRITE(20,11) IPSI,CS(IPSI)**2,RHO,QS(IPSI),SHEAR,SHEAR1,
     >               ALPHA,ALPHA2,FMARG, INIBAL
   10 CONTINUE
      WRITE(20,*) '****************************************************'
     >            //'*****************'
   11 FORMAT(' ',I3,' ',F6.3,' ',F6.3,' ',F6.3,' ',F7.4,' ',F7.4,' ',
     >       F7.4,' ',F7.4,' ',F8.3,'  ',A25)

      END

************************************************************************
*DECK INIT
      SUBROUTINE INIT(IAS)
C-----------------------------------------------------------------------
C SUBROUTINE TO INITIALIZE THE INPUT VARIABLES, ALSO WRITES THE HEADER
C OF THE OUTPUT FILE
C-----------------------------------------------------------------------
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      TBB = -100.
      IF (IAS.EQ.0) TBB = 0.
      TBF =  100.
      WRITE(20,*) '*******************************'
      WRITE(20,*) '*   BALLOONING STABILITY      *'
      WRITE(20,*) '*   PROGRAM HELBAL VERSION 1  *'
      WRITE(20,*) '*******************************'
      WRITE(20,*)  TBB,TBF
      RETURN
      END      
************************************************************************
*DECK PQ
      SUBROUTINE PQ(IAS)
C-----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE P AND Q COEFFICENTS IN THE BALLOONING
C EQUATION IN ALL GRID POINTS.
C THE TERMS DEPENDING ON THE EXTENDED BALLOONING ANGLE ARE SEPERATED.
C THE BALLOONING EQUATION (POGUSTE AND YURCHENKO) READS:
C       DT((P0 + T * P1 + T^2 P2) DT(F)) - (Q0 + T * Q1) F = 0 
C
C NOTE : THE FINAL ARRAYS CP AND CQ HAVE A DIFFERENT INDEX FROM THE
C GEMIJ ARRAYS. THE J INDEX RUNS FROM 0 TO 2PI (INCLUDING 2PI), THE
C NUMBER OF POINTS IN POLOIDAL DIRECTION IS NCHI+1 FOR IAS=1 AND 
C 2*NCHI-1 FOR IAS=0.
C-----------------------------------------------------------------------      
      USE PARAM
      USE COMMAX
      USE COMPIO
      USE COMMAP
      USE COMSPL
      USE COMB02
      USE COMPQ
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      NCPQ = 2*NCHI-1
      IF (IAS.EQ.1) NCPQ = NCHI+1
      DO 10 I=2,NPSI
        SPS2 = 2.*CPSURF*CS(I)
        ZQ = QS(I)
        ZT = RBPHI(I)
        ZDP = P2(I)
        ZDQ = DQS(I)
        DO 20 J=1,NCHI
          IJ = (I-1)*NCHI + J
          IF (IAS.EQ.0) THEN
            IJ1 = (I-1)*(2*NCHI-1) + J
          ELSE  
            IJ1 = (I-1)*(NCHI+1) + J
          ENDIF  
C----------------------------------- LOWER INDEX GEOMETRIC COEFF. -------     
c          G33 = GEM33(IJ)
c          G11 = SPS2**2 *(1. + ZQ**2/ZT**2 
c     >                       * GEM33(IJ) * GEM12(IJ)**2 ) / GEM11(IJ)  
c          G12 = - SPS2 * ZQ**2 / ZT**2 * GEM12(IJ) * GEM33(IJ)
c          G22 = ZQ**2 / ZT**2 * GEM11(IJ) * GEM33(IJ)
c          ZJ  = SPS2 * ZQ * GEM33(IJ) / ZT
C----------------------------------------------------------------------- 
          BETA =  - GEM12(IJ)/GEM11(IJ) 
          CP0(IJ1) = 1. / (GEM33(IJ)*GEM11(IJ))
     >             + ZQ**2 * GEM11(IJ) *BETA**2 /(GEM33(IJ)*B02(IJ))
          CP1(IJ1) = 2.*BETA * GEM11(IJ) * (ZDQ*ZQ)
     >                               /(SPS2*GEM33(IJ)*B02(IJ))
          CP2(IJ1) = GEM11(IJ) * ZDQ**2
     >                               /(SPS2**2 *GEM33(IJ)*B02(IJ))
          CQ0(IJ1) = -ZDP* ZQ**2 * GEM33(IJ) / (SPS2*ZT*B02(IJ))**2
     >             *( (2.*ZDP+DSB02(IJ))*B02(IJ) + 
     >                SPS2*BETA*GEM11(IJ)/GEM33(IJ) * DTB02(IJ))     
          CQ1(IJ1) = ZDP*ZDQ*ZQ /(SPS2 * B02(IJ))**2 * DTB02(IJ)   
C----------------------------------------------------------------------- 
          IF (IAS.EQ.0) THEN
             IJ2 = I*(2*NCHI-1) - J + 1 
             CP0(IJ2) = CP0(IJ1)
             CP1(IJ2) =-CP1(IJ1)
             CP2(IJ2) = CP2(IJ1)
             CQ0(IJ2) = CQ0(IJ1)
             CQ1(IJ2) =-CQ1(IJ1)
          ELSEIF ((IAS.EQ.1).AND.(J.EQ.1)) THEN
             IJ2 = I*(NCHI+1)
             CP0(IJ2) = CP0(IJ1)
             CP1(IJ2) = CP1(IJ1)
             CP2(IJ2) = CP2(IJ1)
             CQ0(IJ2) = CQ0(IJ1)
             CQ1(IJ2) = CQ1(IJ1)
          ENDIF    
   20   CONTINUE         
   10 CONTINUE
      RETURN
      END

************************************************************************
*DECK BFIELD      
      SUBROUTINE BFIELD(IAS)
C-----------------------------------------------------------------------
C SUBROUTINE TO CALCULATE :
C      - THE TOTAL MAGNETIC FIELD SQUARED
C      - THE RADIAL AND POLOIDAL DERIVATIVE OF THE TOTAL FIELD SQUARED
C    ON THE GRID POINTS OF THE STRAIGHT FIELD LINE COORDINATE SYSTEM. 
C-----------------------------------------------------------------------
      USE PARAM
      USE COMMAX
      USE COMPIO
      USE COMMAP
      USE COMSPL
      USE COMB02
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  SP(NPSIMAX),S1(NPSIMAX),
     >                  S2(NPSIMAX),S3(NPSIMAX),S4(NPSIMAX)
C---------------------------------------- B0 ON GRID POINTS -----------
      DO 10 I=1,NPSI
        DO 20 J=1,NCHI
           IJ = (I-1)*NCHI + J
           B02(IJ) = (GEM11(IJ) + RBPHI(I)**2)/GEM33(IJ)
   20   CONTINUE
   10 CONTINUE
C---------------------------------------- D(B02)/D(THETA) -------------
      DO 30 I=1,NPSI
        IJSTART = (I-1)*NCHI + 1
        CALL DERIV(B02(IJSTART),DTB02(IJSTART),NCHI,IAS)     
   30 CONTINUE
C---------------------------------------- D(B02)/DS --------------------
      DO 40 J=1,NCHI
        DO 50 I=1,NPSI
          IJ = (I-1)*NCHI + J
          SP(I) = B02(IJ)
   50   CONTINUE
        CALL SPLINE(NPSI,CS,SP,0.0D0,0.0D0,2,S1,S2,S3,S4)
        DO 60 I=1,NPSI
          IJ = (I-1)*NCHI + J
          DSB02(IJ) = S2(I)
   60   CONTINUE 
   40 CONTINUE
      RETURN
      END       
************************************************************************
*DECK IODSK
      SUBROUTINE IODSK
C-----------------------------------------------------------------------
C    - READS HELENA MAPPING FILE
C    - SCALES QUANTITIES WITH Q ON AXIS
C-----------------------------------------------------------------------
      USE PARAM
      USE COMMAX
      USE COMPIO
      USE COMMAP
      USE COMSPL
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  C1(NPSIMAX),dummy(3)

      NPSI = JS0 + 1
      NG = NPSI*NCHI

      DO 30 JC=1,NCHI
        GEM11(JC) = 0.
        GEM33(JC) = RAXIS**2
   30 CONTINUE
C
      DO 40 JC=1,NCHI
         CALL DCOPY(NPSI-1,GEM12(NCHI+JC),NCHI,C1,1)
         CALL SPLINE(NPSI-1,CS(2),C1,0.0D0,0.0D0,2,Q1,Q2,Q3,Q4)
         GEM12(JC) = SPWERT(NPSI-1,0.0D0,Q1,Q2,Q3,Q4,CS(2),DUMMY)
   40 CONTINUE

C------------------------------------------------------------------
C     SCALE QUANTITIES WITH VALUE OF Q ON AXIS (TOTAL CURRENT)
C------------------------------------------------------------------
      SCALEQ = QS(1)/QAXIS
C
      CPSURF = CPSURF*SCALEQ
      CALL DSCAL(NPSI,SCALEQ**2,P0,1)
      CALL DSCAL(NPSI*NCHI,-SCALEQ,GEM12,1)
      CALL DSCAL(NPSI*NCHI,SCALEQ**2,GEM11,1)
C
      RBPHI02 = RBPHI(1)**2
C
      DP0     = DP0*SCALEQ**2
      DPE     = DPE*SCALEQ**2
      DRBPHI0 = DRBPHI0*SCALEQ**2
      DRBPHIE = DRBPHIE*SCALEQ/DSQRT(1.+RBPHI02/RBPHI(NPSI)**2*
     >          (1./SCALEQ**2-1.))
C
      DO 50 J=1,NPSI
         WURZEL   = DSQRT(1.+RBPHI02/RBPHI(J)**2*(1./SCALEQ**2-1.))
         QS(J)    = WURZEL * QS(J)
         RBPHI(J) = WURZEL * SCALEQ * RBPHI(J)
   50 CONTINUE
C
      WRITE(20,*)  NPSI,NCHI
      WRITE(20,51) SCALEQ
      WRITE(20,52) CPSURF
      WRITE(20,53) (QS(JJ),JJ=1,JS0+1)
      WRITE(20,54) (P0(JJ),JJ=1,NPSI)
      WRITE(20,55) (RBPHI(JJ),JJ=1,NPSI)
C
C     SPLINES
C
      DQ1 = (QS(NPSI)-QS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      DQ0 = (QS(2)-QS(1))/(CS(2)-CS(1))
      
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,1,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,P0,DP0,DPE,1,P1,P2,P3,P4)
      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
C
      CALL DCOPY(NPSI,Q2,1,DQS,1)
      RETURN
C
   51 FORMAT(/' AFTER SCALE: SCALEQ=',1P,E12.4,0P)
   52 FORMAT(/' CPSURF = ',1P,E12.4,0P)
   53 FORMAT(/' QS'/(1X,1P,5E16.8,0P))
   54 FORMAT(/' P0'/(1X,1P,5E16.8,0P))
   55 FORMAT(/' RBPHI'/(1X,1P,5E16.8,0P))
      END

************************************************************************
*DECK KGS
      SUBROUTINE KGS(T,T0,CP,CQ,IPSI)
C-----------------------------------------------------------------------
C SUBROUTINE TO EVALUATE THE P AND Q COEFFICIENT, USED IN THE SUYDAM
C ROUTINE. 
C NOTE : THE ROUTINE ASUMES THAT ONLY VALUES ON GRID POINTS ARE
C REQUESTED. NO INTERPOLATION IS DONE!
C-----------------------------------------------------------------------      
      USE COMMAX
      USE COMPQ
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      TWOPI = 4.*DASIN(1.D0)
      DT = TWOPI/REAL(NCPQ-1)
      TM = MOD(MOD(T,TWOPI)+TWOPI, TWOPI)
C-------------------------- NCPQ=2*NCHI-1 (IAS=0) OR NCPQ=NCHI+1 (IAS=1)      
      JM = INT((NCPQ-1) * TM/TWOPI) + 1
      IJM = (IPSI-1)*NCPQ + JM
      FRAC = (TM - REAL(JM-1)*DT)/DT
      CP0D = CP0(IJM) + (CP0(IJM+1)-CP0(IJM))*FRAC
      CP1D = CP1(IJM) + (CP1(IJM+1)-CP1(IJM))*FRAC
      CP2D = CP2(IJM) + (CP2(IJM+1)-CP2(IJM))*FRAC
      CQ0D = CQ0(IJM) + (CQ0(IJM+1)-CQ0(IJM))*FRAC 
      CQ1D = CQ1(IJM) + (CQ1(IJM+1)-CQ1(IJM))*FRAC           
      CP = CP0D + CP1D*(T-T0) + CP2D*(T-T0)**2 
      CQ = CQ0D + CQ1D*(T-T0)
      RETURN
      END

************************************************************************
*DECK SUYDAM
      SUBROUTINE SUYDAM(IPSI,T0,TBB,TBF,NCPQ,FACT,BAL)
C-----------------------------------------------------------------------
C  SUBROUTINE TO VERIFY THE DSIGN OF THE BALLOONING ENERGY        
C  FUNCTIONAL ACCORDING TO THE SUYDAM FINITE DIFFERENCE SCHEME.  
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)  
      DOUBLE PRECISION  KP1,KP2
      CHARACTER*25 BAL    
      PI  = 2*DASIN(1.D0)
      BAL = ' STABLE'
      DBT = 4.*DASIN(1.D0)/REAL(NCPQ-1)

      N  = INT((TBF - TBB) / DBT)
      ALP= 1.
      T2 = TBB - DBT/2.

      KP2 = 0.
      GP2 = 0.
C
      DO 10 I=1,N
        T1  = T2
        KP1 = KP2
        GP1 = GP2
        T2  = T1+DBT
        CALL KGS(T2,T0,KP2,GP2TEMP,IPSI)
        GP2 = GP2TEMP * FACT
        A11=(KP2+KP1)/DBT+0.25*(GP1+GP2)*DBT       
        A01=-KP1/DBT+0.25*GP1*DBT
        IF (I.EQ.1) A01 = 0.
        ALP=A11-(A01*A01)/ALP
        IF(ALP.LE.0.) THEN
          TS = (T1+T2)/2.
          WRITE(BAL,11) TS
          RETURN
        ENDIF
   10 CONTINUE
   11 FORMAT(' UNSTABLE AT T = ',F8.3)
   20 RETURN
      END

**********************************************************************
*DECK DERIV
      SUBROUTINE DERIV(ARRIN,DARR,NCHI,IAS)
C---------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE THETA DERIVATE UDSING FFT.
C ARRIN : THE INPUT ARRAY
C DARR  : THE RESULTING THETA DERIVATIVE 
C NCHI  : THE NUMBER OF POLOIDAL POINTS
C IAS   : 0 FOR UP/DOWN SYMMETRIC EQUILIBRIA, 1 FOR ASYMMETRIC EQUIL.
C---------------------------------------------------------------------
      USE COMMAX 
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION ARRIN(*),DARR(*),
     >                 FF(2*NCHIMAX+2),DF(2*NCHIMAX+2)
      INTEGER    INDEX(2*NCHIMAX)

      PI = 2.* DASIN(1.D0)

      DO 10 J=1,NCHI
         INDEX(J) = J
   10 CONTINUE
   
      IF (IAS.EQ.0) THEN
        DO 20 J=NCHI+1,2*NCHI-2
          INDEX(J) = 2*NCHI-J
   20   CONTINUE
      ENDIF

      IF (IAS.EQ.0) THEN
         N=2*(NCHI-1)
      ELSE
         N=NCHI
      ENDIF
      DO 40 J=1,N       
        FF(J) = ARRIN(INDEX(J))
   40 CONTINUE 
                        
      CALL RFT2(FF,N,1)
      
      DO 50 J = 1,N/2
        DF(2*J-1) = - FLOAT(J-1) * FF(2*J)   
        DF(2*J)   =   FLOAT(J-1) * FF(2*J-1) 
   50 CONTINUE
   
      DF(2) = 0.
      DF(N+2) = 0.

      CALL RFI2(DF,N,1)

      DO 60 J=1,NCHI
        DARR(J) = DF(J)
   60 CONTINUE
      END

************************************************************************
*DECK DBLELM
      SUBROUTINE DBLELM(XX,YY,PSI,NODENO,NR,NP,INDEX)
C-----------------------------------------------------------------------
C SUBROUTINE TO ADJUST EXISTING GRID TO HALF THE RADIAL SIZE OF
C RADIAL ELEMENT INDEX, I.E. BETWEEN INDEX I=INDEX,INDEX+1
C-----------------------------------------------------------------------
      USE PARAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  XX(4,*),YY(4,*),PSI(*)
      DOUBLE PRECISION  XN(4,NPMAX),YN(4,NPMAX),PSN(4,NPMAX)
      INTEGER NODENO(MAXMNODE,4)

      RR =  0.
      SS = -1.
      DO J=1,NP-1
        NN = (INDEX-1)*(NP-1) + J      
        N1 = NODENO(NN,1)
        N2 = NODENO(NN,2)
        N3 = NODENO(NN,3)
        N4 = NODENO(NN,4)
        CALL INTERP(XX(1,N1),XX(1,N2),XX(1,N3),XX(1,N4),
     >	            RR,SS,X,XR,XS,XRS,XRR,XSS)
        CALL INTERP(YY(1,N1),YY(1,N2),YY(1,N3),YY(1,N4),
     >	            RR,SS,Y,YR,YS,YRS,YRR,YSS)
        CALL INTERP(PSI(4*N1-3),PSI(4*N2-3),PSI(4*N3-1),
     >              PSI(4*N4-3),RR,SS,PS,PSR,PSS,PSRS,PSRR,PSSS)
        XN(1,J) = X
	XN(2,J) = XR / 1.
	XN(3,J) = XS 
	XN(4,J) = XRS / 1.
        YN(1,J) = Y
	YN(2,J) = YR  / 1.
	YN(3,J) = YS 
	YN(4,J) = YRS / 1.
        PSN(1,J) = PS
	PSN(2,J) = PSR / 1.
	PSN(3,J) = PSS
	PSN(4,J) = PSRS / 1.
      ENDDO
      DO K=1,4
        XN(K,NP) = XN(K,1)
	YN(K,NP) = YN(K,1)
	PSN(K,NP) = PSN(K,1)
      ENDDO
      DO I=NR,INDEX+1,-1
        DO J=1,NP
	  NODE = (I-1)*NP+J
	  DO K=1,4
	    XX(K,NODE+NP) = XX(K,NODE)
	    YY(K,NODE+NP) = YY(K,NODE)
	    PSI(4*(NODE+NP-1)+K) = PSI(4*(NODE-1)+K)
	  ENDDO
	ENDDO
      ENDDO
      DO J=1,NP
        NODE = INDEX*NP + J
	DO K=1,4
	  XX(K,NODE) = XN(K,J)
	  YY(K,NODE) = YN(K,J)
	  PSI(4*(NODE-1)+K) = PSN(K,J)
        ENDDO
      ENDDO
      NR = NR + 1
      DO J=1, NP
        NODE = INDEX*NP + J
        XX(2,NODE) = XX(2,NODE) / 1.5
        YY(2,NODE) = YY(2,NODE) / 1.5
        PSI(4*(NODE-1)+2) = PSI(4*(NODE-1)+2) / 1.5
        XX(4,NODE) = XX(4,NODE) / 1.5
        YY(4,NODE) = YY(4,NODE) / 1.5
        PSI(4*(NODE-1)+4) = PSI(4*(NODE-1)+4) / 1.5
      ENDDO
      DO J=1, NP
        NODE = (INDEX+2)*NP + J
        XX(2,NODE) = XX(2,NODE) / 1.5
        YY(2,NODE) = YY(2,NODE) / 1.5
        PSI(4*(NODE-1)+2) = PSI(4*(NODE-1)+2) / 1.5
        XX(4,NODE) = XX(4,NODE) / 1.5
        YY(4,NODE) = YY(4,NODE) / 1.5
        PSI(4*(NODE-1)+4) = PSI(4*(NODE-1)+4) / 1.5
      ENDDO
      NO = 0
      DO N=1,NR-1
        DO M=1,NP-1
          NO = NO + 1
          NODENO(NO,1) = (N-1)*NP + M
          NODENO(NO,2) = NODENO(NO,1) + 1
          NODENO(NO,3) = NODENO(NO,2) + NP
          NODENO(NO,4) = NODENO(NO,1) + NP
        ENDDO
      ENDDO


      RETURN
  111 FORMAT(6E12.4)
      END
************************************************************************
*DECK GSCHECK
      SUBROUTINE GSCHECK
C-----------------------------------------------------------------------
C    - CHECKS THE ERROR IN THE GRAD-SHAFRANOV EQUATION UDSING THE 
C      STRAIGHT FIELD LINE SYSTEM
C-----------------------------------------------------------------------
      USE PARAM
      USE COMMAX
      USE COMPIO
      USE COMMAP
      USE COMSPL
      USE COMNAM
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DOUBLE PRECISION  GP(NPSIMAX),DGT(NCHIMAX),dummy(3)
      DOUBLE PRECISION  GP1(NPSIMAX), GP2(NPSIMAX), 
     >                  GP3(NPSIMAX),  GP4(NPSIMAX)
      DOUBLE PRECISION  ERROR(NPSIMAX-1,NCHIMAX),ZC(10),MAXERR
      DOUBLE PRECISION  ERROR2(NPSIMAX-1,NCHIMAX),FF(NCHI),FP(NCHI)
            
      NPSI = JS0 + 1
C
      DO JC=1,NCHI
         CALL DCOPY(NPSI-1,GEM12(NCHI+JC),NCHI,C1,1)
         CALL SPLINE(NPSI-1,CS(2),C1,0.0D0,0.0D0,2,Q1,Q2,Q3,Q4)
         GEM12(JC) = SPWERT(NPSI-1,0.0D0,Q1,Q2,Q3,Q4,CS(2),DUMMY)
      ENDDO

      CALL SPLINE(NPSI,CS,P0,DP0,DPE,1,P1,P2,P3,P4)
      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)

      MAXERR = -1.D20
      DO J=1,NCHI
        DO I=1,NPSI
          IJ1 = (I-1)*NCHI + J
	  GP(I) = GEM11(IJ1) * QS(I) / RBPHI(I)
	ENDDO
        DGP0 = (GP(2)-GP(1))/(CS(2)-CS(1))
	DGP1 = (GP(NPSI)-GP(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
        CALL SPLINE(NPSI,CS,GP,DGP0,DGP1,2,GP1,GP2,GP3,GP4)
        CALL SPLINE(NPSI,CS,GP,0.D0,0.D0,2,GP1,GP2,GP3,GP4)

        DO I=2,NPSI-1
	  SPS2 = 2.*CS(I) * CPSURF
	  IJ1 = (I-1)*NCHI + 1
	  IJT = (I-1)*NCHI + J
          CALL DERIV(GEM12(IJ1),DGT,NCHI,IAS)
          DGP = (GEM11(IJT+NCHI)*QS(I+1)/RBPHI(I+1) -
     >          GEM11(IJT-NCHI)*QS(I-1)/RBPHI(I-1))/
     >           (CS(I+1)-CS(I-1))
c          DGT2 = (GEM12(IJT+1)-GEM12(IJT-1))/(CHI(J+1)-CHI(J-1))
	  ERR = DABS(RBPHI(I)/QS(I)*DGP + DGT(J) * SPS2
     >          + (RBPHI(I)*RBP2(I) + GEM33(IJT)*P2(I)))
          IF (ERR.GT.MAXERR) THEN
	    MAXERR = ERR
	    IM = I
	    JM = J
	  ENDIF
C          WRITE(20,21) I,J,CS(I),CHI(J),ERR,DALOG(ERR)
	  ERROR(I-1,J) = DLOG(ERR)
	  ERROR2(I-1,J) = ERR
        ENDDO
      ENDDO
      WRITE(20,*)
      WRITE(20,23) MAXERR,IM,JM
      WRITE(20,*)
   21 FORMAT(2I3,2F8.3,3e12.4)
   22 FORMAT(2I3,F8.3,3e12.4)
   23 FORMAT(' MAX ERROR IN GS AFTER MAPPING : ',E12.4,2I5)

c      CALL CPLOTX(21,1,1,CS(2),CHI,NPSI-1,NCHI,1,1,ERROR,NPSIMAX-1,
c     >            ZC,-10,'ERROR IN GS',11,'X',1,'Y',1,1.D0,4,0)
c      CALL P3PLOT(21,1,CS(2),CHI,NPSI-1,NCHI,ERROR,NPSIMAX-1,
c     >            10.D0,40.D0,'ERROR IN GS',11)

c      WRITE(20,*) 
c      WRITE(20,*) ' ERROR IN GS : HARMONICS'
c      WRITE(20,*)
c      DO J=1,NCHI
c        FP(J) = FLOAT(J-1)
c      ENDDO 
c      DO I=2,NPSI
c        DO J=1,NCHI
c	  FF(J) = ERROR2(I-1,J)
c	ENDDO
c        CALL RFT2(FF,NCHI,1)
c	DO M=1,NCHI/2
c          FF(M) = 2. * FF(M) / REAL(NCHI)
c	  WRITE(20,22) I,M,CS(I),FF(M)
c        ENDDO
c     ENDDO

      RETURN
C
      END

*DECK SGTSL
*** FROM NETLIB, TUE AUG 28 08:28:34 EDT 1990 ***                               
C                                                                               
      SUBROUTINE SGTSL(N,C,D,E,B,INFO)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)                                          
      INTEGER N,INFO                                                            
      DOUBLE PRECISION  C(1),D(1),E(1),B(1)                                                  
C                                                                               
C     SGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND                 
C     SIDE WILL FIND THE SOLUTION.                                              
C                                                                               
C     ON ENTRY                                                                  
C                                                                               
C        N       INTEGER                                                        
C                IS THE ORDER OF THE TRIDIAGONAL MATRIX.                        
C                                                                               
C        C       REAL(N)                                                        
C                IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.                  
C                C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.              
C                ON OUTPUT C IS DESTROYED.                                      
C                                                                               
C        D       REAL(N)                                                        
C                IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.                     
C                ON OUTPUT D IS DESTROYED.                                      
C                                                                               
C        E       REAL(N)                                                        
C                IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.                
C                E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.          
C                ON OUTPUT E IS DESTROYED.                                      
C                                                                               
C        B       REAL(N)                                                        
C                IS THE RIGHT HAND SIDE VECTOR.                                 
C                                                                               
C     ON RETURN                                                                 
C                                                                               
C        B       IS THE SOLUTION VECTOR.                                        
C                                                                               
C        INFO    INTEGER                                                        
C                = 0 NORMAL VALUE.                                              
C                = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES                
C                    EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN                 
C                    THIS IS DETECTED.                                          
C                                                                               
C     LINPACK. THIS VERSION DATED 08/14/78 .                                    
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.                               
C                                                                               
C     NO EXTERNALS                                                              
C     FORTRAN DABS                                                               
C                                                                               
C     INTERNAL VARIABLES                                                        
C                                                                               
      INTEGER K,KB,KP1,NM1,NM2                                                  
      DOUBLE PRECISION  T                                                                    
C     BEGIN BLOCK PERMITTING ...EXITS TO 100                                    
C                                                                               
         INFO = 0                                                               
         C(1) = D(1)                                                            
         NM1 = N - 1                                                            
         IF (NM1 .LT. 1) GO TO 40                                               
            D(1) = E(1)                                                         
            E(1) = 0.0E0                                                        
            E(N) = 0.0E0                                                        
C                                                                               
            DO 30 K = 1, NM1                                                    
               KP1 = K + 1                                                      
C                                                                               
C              FIND THE LARGEST OF THE TWO ROWS                                 
C                                                                               
               IF (DABS(C(KP1)) .LT. DABS(C(K))) GO TO 10                         
C                                                                               
C                 INTERCHANGE ROW                                               
C                                                                               
                  T = C(KP1)                                                    
                  C(KP1) = C(K)                                                 
                  C(K) = T                                                      
                  T = D(KP1)                                                    
                  D(KP1) = D(K)                                                 
                  D(K) = T                                                      
                  T = E(KP1)                                                    
                  E(KP1) = E(K)                                                 
                  E(K) = T                                                      
                  T = B(KP1)                                                    
                  B(KP1) = B(K)                                                 
                  B(K) = T                                                      
   10          CONTINUE                                                         
C                                                                               
C              ZERO ELEMENTS                                                    
C                                                                               
               IF (C(K) .NE. 0.0E0) GO TO 20                                    
                  INFO = K                                                      
C     ............EXIT                                                          
                  GO TO 100                                                     
   20          CONTINUE                                                         
               T = -C(KP1)/C(K)                                                 
               C(KP1) = D(KP1) + T*D(K)                                         
               D(KP1) = E(KP1) + T*E(K)                                         
               E(KP1) = 0.0E0                                                   
               B(KP1) = B(KP1) + T*B(K)                                         
   30       CONTINUE                                                            
   40    CONTINUE                                                               
         IF (C(N) .NE. 0.0E0) GO TO 50                                          
            INFO = N                                                            
         GO TO 90                                                               
   50    CONTINUE                                                               
C                                                                               
C           BACK SOLVE                                                          
C                                                                               
            NM2 = N - 2                                                         
            B(N) = B(N)/C(N)                                                    
            IF (N .EQ. 1) GO TO 80                                              
               B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)                           
               IF (NM2 .LT. 1) GO TO 70                                         
               DO 60 KB = 1, NM2                                                
                  K = NM2 - KB + 1                                              
                  B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)                
   60          CONTINUE                                                         
   70          CONTINUE                                                         
   80       CONTINUE                                                            
   90    CONTINUE                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       

!      SUBROUTINE CPUTIME(TIME1)
!-----------------------------------------------------------------------
! DUMMY ROUTINE TO REPLACE THE SDTANDARD CALL CPU_TIME FOR MACHINES
! WHERE THIS IS NOT AVAILABLE. X05BAF() IS A NAG ROUTINE.
!-----------------------------------------------------------------------
!      DOUBLE PRECISION  TIME1
!      TIME1 = X05BAF()
!      RETURN
!      END

      SUBROUTINE OSPLINE(X,Y,N,YP1,YPN,Y2)

c     SPLINE use: given an 1D array of X data and an array of Y data,
c     both of length N, this routine computes the 2nd derivatives, Y2 at
c     each X data point.  The user needs to specify the values of YP1
c     and YP2, which flags how the Y2 are computed at the edges.  For
c     natural spline fitting (recommended), set YP1 and YPN to numbers
c     greater than 1.0E+30.

c     this routine called once, prior to using routine SPLINT, as a set
c     up for using routine SPLINT, which performs the actual
c     interpolation

c     IMPORTANT NOTE: the X data values in array X must be in ascending
c     order or the interpolation will fail

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NMAX=600)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)

c     if YP1>1.0E+30 use natural spline, otherwise estimate Y2 at the
c     first point

      IF (YP1.GT..99D30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF

c     store intermediate values of terms in the expansion series

      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE

c     if YPN>1.0E+30 use natural spline, otherwise estimate Y2 at the
c     last point point

      IF (YPN.GT..99D30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)

c     compute the Y2 from the 2nd order expansion series

      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE

      RETURN
      END

c..............................................................................

      SUBROUTINE OSPLINT(XA,YA,Y2A,N,X,Y)

c     SPLINT use: given an 1D array of XA data, an array of YA data, and
c     an array of the 2nd derivatives Y2A, all of length N, this routine
c     performs cubic spline interpolation, returning the interpolated
c     value Y at the user input value X.  The Y2A are computed in
c     routine SPLINE, which is called once before calling SPLINT.

c     IMPORTANT NOTE: the X data values in array X must be in ascending
c     order or the interpolation will fail

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XA(N),YA(N),Y2A(N)

      KLO=1
      KHI=N

c     determine the indices of array XA that bracket the input X value

1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF

c     determine the finite difference along the X dimension

      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) STOP 'Bad XA input in routine SPLINE.'

c     interpolate

      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.

      RETURN
      END
