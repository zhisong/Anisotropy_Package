*COMDECK COMVER
      CHARACTER  VERSION*(*),   DD*(*)
      PARAMETER (VERSION = '2', DD = '24 Oct 1996')
*COMDECK COMMAX
      PARAMETER (LANZ=9, MANZ=LANZ,MDIF=1, LMAX=128)
      PARAMETER (LVANZ=11, MVANZ=LVANZ, LVMAX=LMAX)
      PARAMETER (NGMAX=401, MXNINT=NGMAX-1, NDEQ=4*MXNINT)
      PARAMETER (NPSIMAX=201, NCHIMAX=257)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX, NP4=4*NPSIMAX)
      PARAMETER (NVPSIMX=201, NVCHIMX=257)
      PARAMETER (NVPNVC=NVPSIMX*NVCHIMX,NVP4=4*NVPSIMX)
*COMDECK COMPAR
      PARAMETER (NGL=7, NBG=NGL*MANZ, NZMA=NBG, NB3=3*NBG)
c      PARAMETER (KILWOR=15000000, NDIM1=1500/NBG*NBG)
c      PARAMETER (KILWOR=30000000, NDIM1=1000/NBG*NBG)
      PARAMETER (KILWOR=30000000,NDIM1=NZMA)
      PARAMETER (KPRGR=604999+11*NDEQ+NGMAX)
      PARAMETER (KPMAX=KILWOR-KPRGR, KPMEX=KPMAX-8*NBG*NBG)
*IF CRAY
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-4*NDIM1)
      PARAMETER (NREST1D=NREST1)
*ELSE
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-3*NDIM1-NDIM1/2)
      PARAMETER (NREST1D=KPMEX-4*NDIM1**2-4*NDIM1)
*ENDIF
*COMDECK COMPCON
      REAL       PI, ZERO, ONE
      COMPLEX    ZEROC, ONEC, CHALF, CTWO
      PARAMETER (NMAX=201)
      PARAMETER (PI=3.141592653589793)
      PARAMETER (ZERO=0.E0, ONE=1.E0)
      PARAMETER (ZEROC=(0.E0,0.E0), ONEC=(1.E0,0.E0))
      PARAMETER (CHALF=(.5E0,0.E0), CTWO=(2.E0,0.E0))
*COMDECK COMPIO
*IF IPP
      PARAMETER (NIN=5, NIN2=2, NOUT=6, NOUTI=8, NOUT2=1, NOUT3=10)
      PARAMETER (NOUTP=11, NOUTV=20, NOUTE=21, NOUTVB=22, NOUTB=23)
C *ELSEIF JET
C       PARAMETER (NIN=5, NIN2=2, NOUT=6, NOUTI=8, NOUT2=1, NOUT3=10)
C       PARAMETER (NOUTP=11, NOUTV=21, NOUTE=22, NOUTVB=23, NOUTB=24)
*ELSE
      PARAMETER (NIN=10, NIN2=11, NOUT=20, NOUTI=21, NOUT2=21, NOUT3=23)
      PARAMETER (NOUTP=24, NOUTV=25, NOUTE=26, NOUTVB = 27, NOUTB=28)
*ENDIF
      PARAMETER (NMAP=12)
      PARAMETER (ND3=15, ND4=16, ND5=17, ND6=18)
C-----------------------------------------------------------------------
*COMDECK CORE1
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AMAT(NZMA,NZMA), BMAT(NZMA,NZMA),
     R         WIG(NDIM1*NGMAX), WRG(NDIM1*NGMAX),
     R         SPLOT(NDIM1*NGMAX),
     R         XV(NDIM1*NGMAX), YV(NDIM1*NGMAX),
C     R         WR(NDIM1), WI(NDIM1),
C     R         EVMAG(1,NDIM1),
C     R         HCOR(NREST1),
C     I         INDEX(NDIM1)
     C         WRI(NDIM1), VRI(1,1),
     I         NIP
 
      COMPLEX  ZMA, AMAT, BMAT, WRI, VRI
      REAL     WIG, WRG, XV, YV, SPLOT
      INTEGER  NIP
C      REAL     WR, WI, EVMAG, HCOR
C      INTEGER  INDEX
C-----------------------------------------------------------------------
*COMDECK COMGEW
      COMMON / COMGEW  / GEWI(4)
      REAL               GEWI
c-----------------------------------------------------------------------
*COMDECK COMMOD
      COMMON / COMMOD  / MODE
      INTEGER            MODE
C-----------------------------------------------------------------------
*COMDECK COMDIM
      COMMON / COMDIM  / NDIM
      INTEGER            NDIM
C-----------------------------------------------------------------------
*COMDECK COMLAB
      COMMON / COMLAB   / LABEL,    EQNAME
      CHARACTER          LABEL*34, EQNAME*10
C-----------------------------------------------------------------------
*COMDECK COMIT
      COMMON / COMIT /
     C         EWSHIFT, EW, EWTEST,
     R         EPS,
     I         IT, ITER
C
      COMPLEX  EWSHIFT, EW, EWTEST
      REAL     EPS
      INTEGER  IT, ITER
C-----------------------------------------------------------------------
*COMDECK COMPLOT
      COMMON / COMPLOT /
     R         YMIN, YMAX,YRMIN,YRMAX,
     I         NPLOT
 
      REAL     YMIN, YMAX,YRMIN,YRMAX
      INTEGER  NPLOT
C-----------------------------------------------------------------------
*COMDECK COMGRID
      COMMON / COMGRID / SGRID(NGMAX), NG, NGINT
      REAL               SGRID
      INTEGER            NG, NGINT
C-----------------------------------------------------------------------
*COMDECK COMWEL
      COMMON / COMWEL  / RFOUR(MANZ), VFOUR(MVANZ),
     >                   MSTART(NGMAX), ZNKWEL, NTOR
      REAL               RFOUR, VFOUR, ZNKWEL, MSTART
      INTEGER            NTOR
C-----------------------------------------------------------------------
*COMDECK COMGEO
      COMMON / COMGEO  / ASPECT
      REAL               ASPECT
C-----------------------------------------------------------------------
*COMDECK COMEQUI
      COMMON / COMEQUI /
     R         GAMMA, ETA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN,
     R         DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW,
     R         VEL3, VSURF, VSURF1, VSURF2, VSURF3, VSURF4,
     R         DSCALE, DFLATS, GAMMAPER, GAMMAPAR,
     I         IDPOW, IEQ, IAS, ISLOW, IGAP
C
      REAL     GAMMA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN
      REAL     DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW, DSCALE, DFLATS
      REAL     GAMMAPER, GAMMAPAR
      INTEGER  IDPOW, IEQ, IAS, ISLOW, IGAP, IQUA
      COMPLEX  ETA
C-----------------------------------------------------------------------
*COMDECK COMEQV
      COMMON / COMEQV /
     R         SGI(NDEQ), Q(NDEQ), DQ(NDEQ),
     R         ETAV(NDEQ), DETA(NDEQ), 
     R         ZT0(NDEQ), ZDT0(NDEQ),ZDDT0(NDEQ),
     R         FLOW3(NDEQ), DFLOW3(NDEQ)
C
      REAL     SGI, Q, DQ, ZT0, ZDT0, ZDDT0
      REAL     FLOW3,DFLOW3
      COMPLEX  ETAV,DETA
C-----------------------------------------------------------------------
*COMDECK COMEQV2D
      COMMON /COMEQV2D/
     R         T(NPNC), PPAR(NPNC), PPER(NPNC), RHO(NPNC),
     R         DT(NPNC), DRHO(NPNC)

      REAL     T, PPAR, PPER, RHO
C-----------------------------------------------------------------------
*COMDECK COMGEM
      COMMON / COMGEM / GEM11(NPNC), GEM12(NPNC), GEM33(NPNC)
      REAL              GEM11, GEM12, GEM33
C-----------------------------------------------------------------------
*COMDECK COMIOD
      COMMON / COMIOD /
     R         CPSURF, CS(NPSIMAX), QS(NPSIMAX), DQS(NPSIMAX), 
     R         CURJ(NPSIMAX), CHI(NCHIMAX), FTS(NPSIMAX), DFTS(NPSIMAX),
     R         FLOW3I(NDEQ), DFLOW3I(NDEQ),
     R         OMEGAS(NPSIMAX),OMEGA2(NPSIMAX),
     I         NPSI, NCHI,P0(NDEQ),RBPHI(NDEQ),
     L         NLTORE
C
      REAL     CPSURF, CS, QS, DQS, CURJ, CHI, FTS, DFTS
      REAL     FLOW3I, DFLOW3I
      INTEGER  NPSI, NCHI
      LOGICAL  NLTORE
C-----------------------------------------------------------------------
*COMDECK COMMEW
      COMMON / COMMEW /
     C         VSHIFT(100),
     R         DRS, DIS,
     I         NRS, NIS
C
      COMPLEX  VSHIFT
      REAL     DRS, DIS
      INTEGER  NRS, NIS
c-----------------------------------------------------------------------
*COMDECK COMDIAG
      COMMON / COMDIAG / NDIAGFK,IBVAC
      INTEGER            NDIAGFK,IBVAC
C-----------------------------------------------------------------------
*COMDECK COMSPL
      COMMON / COMSPL /
     R         Q1(NPSIMAX), Q2(NPSIMAX), Q3(NPSIMAX), Q4(NPSIMAX),
     R         C1(NPSIMAX), C2(NPSIMAX), C3(NPSIMAX), C4(NPSIMAX),
     R         P1(NPSIMAX), P2(NPSIMAX), P3(NPSIMAX), P4(NPSIMAX),
     R         RBP1(NPSIMAX), RBP2(NPSIMAX), RBP3(NPSIMAX),
     R         RBP4(NPSIMAX)
C
      REAL     Q1, Q2, Q3, Q4, C1, C2, C3, C4,
     >         P1, P2, P3, P4, RBP1, RBP2, RBP3, RBP4
C-----------------------------------------------------------------------
*COMDECK COMFFT
      COMMON / COMFFT /
C============= INDIRECT FFT MATRIX COMPONENTS  =============
     R       RFF        (NP4,MANZ+11), IFF        (NP4,MANZ+11),
     R       RDFDS      (NP4,MANZ+11), IDFDS      (NP4,MANZ+11),
     R       RGPGTOF    (NP4,MANZ+11), IGPGTOF    (NP4,MANZ+11),
     R       RGP2OF     (NP4,MANZ+11), IGP2OF     (NP4,MANZ+11),
     R       RDGP2OF    (NP4,MANZ+11), IDGP2OF    (NP4,MANZ+11),
     R       RDFGP2OF2  (NP4,MANZ+11), IDFGP2OF2  (NP4,MANZ+11),
     R       RGPGT2OGP2F(NP4,MANZ+11), IGPGT2OGP2F(NP4,MANZ+11),
     R       RFOGP2R2   (NP4,MANZ+11), IFOGP2R2   (NP4,MANZ+11),
     R       RR2OF      (NP4,MANZ+11), IR2OF      (NP4,MANZ+11),
     R       RDR2OF     (NP4,MANZ+11), IDR2OF     (NP4,MANZ+11),
     R       RFDFDTOR2  (NP4,MANZ+11), IFDFDTOR2  (NP4,MANZ+11),
     R       RDGP2OR2   (NP4,MANZ+11), IDGP2OR2   (NP4,MANZ+11),
     R       RGP2OR2    (NP4,MANZ+11), IGP2OR2    (NP4,MANZ+11),
     R       RGP2DFOFR2 (NP4,MANZ+11), IGP2DFOFR2 (NP4,MANZ+11),
     R       RGPGTOR2   (NP4,MANZ+11), IGPGTOR2   (NP4,MANZ+11),
     R       RFDFOR2    (NP4,MANZ+11), IFDFOR2    (NP4,MANZ+11),
     R       RDXDTFOR2  (NP4,MANZ+11), IDXDTFOR2  (NP4,MANZ+11),
     R       RDETF2     (NP4,MANZ+11), IDETF2     (NP4,MANZ+11),
     R       RDETGP2    (NP4,MANZ+11), IDETGP2    (NP4,MANZ+11),
     R       RDETGPGT   (NP4,MANZ+11), IDETGPGT   (NP4,MANZ+11),
     R       RR2        (NP4,MANZ+11), IR2        (NP4,MANZ+11),
     R       RDR2       (NP4,MANZ+11), IDR2       (NP4,MANZ+11),
     R       RO1MDET    (NP4,MANZ+11), IO1MDET    (NP4,MANZ+11),
C============== DIRECT FFT MATRIX COEFFICIENTS ==============
C----------------------- BMAT, RB(I,J) IB(I,J) -----------------
     R       RB11(NP4,MANZ+11),RB12(NP4,MANZ+11),RB13(NP4,MANZ+11),
     R       RB22(NP4,MANZ+11),RB23(NP4,MANZ+11),RB33(NP4,MANZ+11),
     R       RB44(NP4,MANZ+11),RB11_2(NP4,MANZ+11),RB11_3(NP4,MANZ+11),
     R       IB11(NP4,MANZ+11),IB12(NP4,MANZ+11),IB13(NP4,MANZ+11),
     R       IB22(NP4,MANZ+11),IB23(NP4,MANZ+11),IB33(NP4,MANZ+11),
     R       IB44(NP4,MANZ+11),IB11_2(NP4,MANZ+11),IB11_3(NP4,MANZ+11),
C----------------------- AMAT, RA(I,J) IA(I,J) ----------------
     R       RA41_1     (NP4,MANZ+11), IA41_1     (NP4,MANZ+11),
     R       RA41_2     (NP4,MANZ+11), IA41_2     (NP4,MANZ+11),
     R       RA41_3     (NP4,MANZ+11), IA41_3     (NP4,MANZ+11),
     R       RA41_4     (NP4,MANZ+11), IA41_4     (NP4,MANZ+11),
     R       RA42_1     (NP4,MANZ+11), IA42_1     (NP4,MANZ+11),
     R       RA42_2     (NP4,MANZ+11), IA42_2     (NP4,MANZ+11),
     R       RA42_3     (NP4,MANZ+11), IA42_3     (NP4,MANZ+11),
     R       RA42_4     (NP4,MANZ+11), IA42_4     (NP4,MANZ+11),
     R       RA43_1     (NP4,MANZ+11), IA43_1     (NP4,MANZ+11),
     R       RA43_2     (NP4,MANZ+11), IA43_2     (NP4,MANZ+11),
     R       RA43_3     (NP4,MANZ+11), IA43_3     (NP4,MANZ+11),
     R       RA43_4     (NP4,MANZ+11), IA43_4     (NP4,MANZ+11),
     R       RA4P1      (NP4,MANZ+11), IA4P1      (NP4,MANZ+11),
     R       RA71_1     (NP4,MANZ+11), IA71_1     (NP4,MANZ+11),
     R       RA71_2     (NP4,MANZ+11), IA71_2     (NP4,MANZ+11),
     R       RA71_3     (NP4,MANZ+11), IA71_3     (NP4,MANZ+11),
     R       RA71_4     (NP4,MANZ+11), IA71_4     (NP4,MANZ+11),
     R       RA72_1     (NP4,MANZ+11), IA72_1     (NP4,MANZ+11),
     R       RA72_2     (NP4,MANZ+11), IA72_2     (NP4,MANZ+11),
     R       RA72_3     (NP4,MANZ+11), IA72_3     (NP4,MANZ+11),
     R       RA72_4     (NP4,MANZ+11), IA72_4     (NP4,MANZ+11),
     R       RA73_1     (NP4,MANZ+11), IA73_1     (NP4,MANZ+11),
     R       RA73_2     (NP4,MANZ+11), IA73_2     (NP4,MANZ+11),
     R       RA73_3     (NP4,MANZ+11), IA73_3     (NP4,MANZ+11),
     R       RA73_4     (NP4,MANZ+11), IA73_4     (NP4,MANZ+11),
     R       RA7P1      (NP4,MANZ+11), IA7P1      (NP4,MANZ+11),
     R       RA15_1     (NP4,MANZ+11), IA15_1     (NP4,MANZ+11),
     R       RA15_2     (NP4,MANZ+11), IA15_2     (NP4,MANZ+11),
     R       RA15_3     (NP4,MANZ+11), IA15_3     (NP4,MANZ+11),
     R       RA15_4     (NP4,MANZ+11), IA15_4     (NP4,MANZ+11),
     R       RA15_5     (NP4,MANZ+11), IA15_5     (NP4,MANZ+11),
     R       RA16_1     (NP4,MANZ+11), IA16_1     (NP4,MANZ+11),
     R       RA16_2     (NP4,MANZ+11), IA16_2     (NP4,MANZ+11),
     R       RA25_1     (NP4,MANZ+11), IA25_1     (NP4,MANZ+11),
     R       RA25_2     (NP4,MANZ+11), IA25_2     (NP4,MANZ+11),
     R       RA25_3     (NP4,MANZ+11), IA25_3     (NP4,MANZ+11),
     R       RA25_4     (NP4,MANZ+11), IA25_4     (NP4,MANZ+11),
     R       RA26_1     (NP4,MANZ+11), IA26_1     (NP4,MANZ+11),
     R       RA26_2     (NP4,MANZ+11), IA26_2     (NP4,MANZ+11),
     R       RA35_1     (NP4,MANZ+11), IA35_1     (NP4,MANZ+11),
     R       RA35_2     (NP4,MANZ+11), IA35_2     (NP4,MANZ+11),
     R       RA35_3     (NP4,MANZ+11), IA35_3     (NP4,MANZ+11),
     R       RA35_4     (NP4,MANZ+11), IA35_4     (NP4,MANZ+11),
     R       RA35_5     (NP4,MANZ+11), IA35_5     (NP4,MANZ+11),
     R       RA36_1     (NP4,MANZ+11), IA36_1     (NP4,MANZ+11),
     R       RA36_2     (NP4,MANZ+11), IA36_2     (NP4,MANZ+11),
     R       RA1PD1     (NP4,MANZ+11), IA1PD1     (NP4,MANZ+11),
     R       RA1PD2     (NP4,MANZ+11), IA1PD2     (NP4,MANZ+11),
     R       RA1PD3     (NP4,MANZ+11), IA1PD3     (NP4,MANZ+11),
     R       RA2PD1     (NP4,MANZ+11), IA2PD1     (NP4,MANZ+11),
     R       RA2PD2     (NP4,MANZ+11), IA2PD2     (NP4,MANZ+11),
     R       RA3PD      (NP4,MANZ+11), IA3PD      (NP4,MANZ+11),
C---------------------------------------------------------------
     I       NP1, N2P1, N3P1
      
      INTEGER  NP1, N2P1, N3P1

      REAL     RFF,RDFDS,RGPGTOF,RGP2OF,RDGP2OF,RDFGP2OF2,
     >         RGPGT2OGP2F,RFOGP2R2,RR2OF,RDR2OF,RFDFDTOR2,
     >         RDGP2OR2,RGP2OR2,RGP2DFOFR2,RGPGTOR2,RFDFOR2,
     >         RDXDTFOR2,RDETF2,RDETGP2,RDETGPGT,RR2,RDR2,
     >         RO1MDET

      REAL     IFF,IDFDS,IGPGTOF,IGP2OF,IDGP2OF,IDFGP2OF2,
     >         IGPGT2OGP2F,IFOGP2R2,IR2OF,IDR2OF,IFDFDTOR2,
     >         IDGP2OR2,IGP2OR2,IGP2DFOFR2,IGPGTOR2,IFDFOR2,
     >         IDXDTFOR2,IDETF2,IDETGP2,IDETGPGT,IR2,IDR2,
     >         IO1MDET

      REAL     RB11,RB12,RB13,RB22,RB23,RB33,RB44,RB11_2,RB11_3
      REAL     IB11,IB12,IB13,IB22,IB23,IB33,IB44,IB11_2,IB11_3
      REAL     RA41_1,RA41_2,RA41_3,RA41_4,RA42_1,RA42_2,RA42_3,
     >         RA42_4,RA43_1,RA43_2,RA43_3,RA43_4,RA4P1,RA71_1,
     >         RA71_2,RA71_3,RA71_4,RA72_1,RA72_2,RA72_3,RA72_4,
     >         RA73_1,RA73_2,RA73_3,RA73_4,RA7P1,RA15_1,RA15_2,
     >         RA15_3,RA15_4,RA15_5,RA16_1,RA16_2,RA25_1,RA25_2,
     >         RA25_3,RA25_4,RA26_1,RA26_2,RA35_1,RA35_2,RA35_3,
     >         RA35_4,RA35_5,RA36_1,RA36_2,RA1PD1,RA1PD2,RA1PD3,
     >         RA2PD1,RA2PD2,RA3PD
      REAL     IA41_1,IA41_2,IA41_3,IA41_4,IA42_1,IA42_2,IA42_3,
     >         IA42_4,IA43_1,IA43_2,IA43_3,IA43_4,IA4P1,IA71_1,
     >         IA71_2,IA71_3,IA71_4,IA72_1,IA72_2,IA72_3,IA72_4,
     >         IA73_1,IA73_2,IA73_3,IA73_4,IA7P1,IA15_1,IA15_2,
     >         IA15_3,IA15_4,IA15_5,IA16_1,IA16_2,IA25_1,IA25_2,
     >         IA25_3,IA25_4,IA26_1,IA26_2,IA35_1,IA35_2,IA35_3,
     >         IA35_4,IA35_5,IA36_1,IA36_2,IA1PD1,IA1PD2,IA1PD3,
     >         IA2PD1,IA2PD2,IA3PD
C-----------------------------------------------------------------------
*COMDECK COMESH
      COMMON / COMESH  / R0, RA, PALAC, PS0AC, PSIGAC
      REAL               R0, RA, PALAC, PS0AC, PSIGAC
C-----------------------------------------------------------------------
*COMDECK COMBND
      COMMON / COMBND  /
     R         ASPI, RADIUS, VX(NVCHIMX), VY(NVCHIMX), VC(NVCHIMX)
      REAL     ASPI, RADIUS, VX, VY, VC
C-----------------------------------------------------------------------
*COMDECK COMESH2
      COMMON / COMESH2 / RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT,
     >                   SBEGIN,SEND,IMESHAC
      REAL               RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT
      INTEGER            IMESHAC
C---------------------------------------------------------------------
*DECK CSMIS_AD
      PROGRAM CSMIS_AD
C
************************************************************************
************************************************************************
**                                                                    **
**  M A S T E R F I L E  :  MISHKA                                    **
**  ------------------------------                                    **
**                                                                    **
**  AUTHORS :         G. HUYSMANS, A. MIKHAILOVSKII,                  **
**                    W. KERNER, S. SHARAPOV                          **
**                                                                    **
**  VERSION :     2                                                   **
**           - seven variables resistive MHD version                  **
**                                                                    **
**           WITH ANISOTORPY CGL (7 VARS VERSION)                     **
**                    Z.S. QU, JUN 2014                               **
*CALL COMVER
**                                                                    **
************************************************************************
************************************************************************
**                                                                    **
**  INPUT :                                                           **
**  -----                                                             **
**                                                                    **
**  EQUILIBRIUM : READ FROM DISK (UNIT NMAP)                          **
**                                                                    **
**  NAMELIST NEWRUN :  ON UNIT NIN (DEFINED IN COMPIO = 5)            **
**                                                                    **
**    MODE     - CONTROL PARAMETER;                                   **
**                VALUES:                                             **
**                0 - TERMINATION OF EXECUTION                        **
**                1 - QR ALGORITHM                                    **
**                                                                    **
**    EQNAME   - NAME OF THE EQUILIBRIUM                              **
**    NLTORE   - TOROIDAL EQUILIBRIUM (.T. OR .F.)                    **
**    NG       - NUMBER OF GRID POINTS                                **
**    RFOUR(1) - LOWEST POLOIDAL MODE NUMBER                          **
**    NTOR     - TOROIDAL MODE NUMBER                                 **
**    ASPECT   - ASPECT RATIO (ONLY IF NLTORE=.F.)                    **
**    SIG1     - SIGMA OF FIRST MESHACC. POINT                        **
**    SIG2     - SIGMA OF SECOND MESHACC. POINT                       **
**    XR1      - POSITION OF FIRST MESHACC. POINT                     **
**    XR2      - POSITION OF SECOND MESHACC. POINT                    **
**    DSURF    - PARAMETER FOR DENSITY PROFILE :                      **
**    NDIAGFK  - PRINT SWITCH FOR FOURIER COMPONENTS OF THE METRIC    **
**                                                                    **
**    YMIN     - LOWER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    YMAX     - UPPER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    IAS      - =0 SYMMETRIC EQUILIBRIUM                             **
**               =1 ASYMMETRIC EQUILIBRIUM                            **
**    ISLOW    - =0 IF ONLY THE ALFVEN PART IS COMPUTED               **
**               =1 ALL PARTS (ALFVEN AND SLOW) ARE COMPUTED          **
**    IGAP     - =0 IF DENSITY PROFILE FROM MAPPING IS USED           **
**               =1 DENSITY PROFILE IS CHANGED TO HELP OPEN THE GAP   **
**                  CAUTION: FORCE BALANCE MIGHT NO BE SATISFIED!     **
**    DSCALE   - IF IGAP==1, SCALE THE DENSITY PROFILE BY             **
**               RHONEW = (1-DSCALE) * RHO + DSCALE                   **
**    DFLATS   - IF IGAP==1, SET DENSITY PROFILE OF S>DFLATS EQUAL TO **
**               DENSITY OF S=DFLATS                                  **
**    EPS      - CONTROL THE DISTANCE TO SINGULARITY                  **
**                                                                    **
**                                                                    **
**  OUTPUT :                                                          **
**  ------                                                            **
**                                                                    **
**    WRITTEN ON UNIT NOUT  (DEFINED IN COMPIO = 6)                   **
**            ON UNIT NOUTI (AT IPP, DEFINED IN COMPIO = 8)           **
**            ON UNIT NOUTP (TEXT FOR FIRST PLOT,                     **
**                           DEFINED IN COMPIO = 11)                  **
************************************************************************
************************************************************************
**                                                                    **
**   MODULAR STRUCTURE :                                               **
**  -----------------                                                 **
**    CASTOR - PRESET                                                 **
**             TESTS                                                   **
**             EQUIL                                                  **
**                (MODULE SPECIFYING THE EQUILIBRIUM)                 **
**             VACUUM                                                 **
**                (MODULE COMPUTING VACUUM PERTURBATION)              **
**             MAT1-5                                                 **
**                (MODULE COMPUTING THE MATRICES A AND B)             **
**             SOLV1-5                                                **
**                (MODULES FOR THE DIFFERENT EIGENVALUE SOLVERS)      **
**             DIAG1-5                                                **
**                (MODULE FOR THE DIAGNOSTICS)                        **
**                                                                    **
**  EXTERNAL SUBROUTINES :                                            **
**  --------------------                                              **
**    PPPLIB  : BEGPLT, LBLTOP, LPLOT, DLCH, NFRAME, FINPLT           **
**    BLAS    :  CXCOPY  ,  CXDOTC  ,  CXSCAL  ,  CXAXPY  ,  ICXAMAX  **
**              =C(Z)COPY, =C(Z)DOTC, =C(Z)SCAL, =C(Z)AXPY, =IC(Z)AMAX**
**               CXDOTU  ,  SGSCAL  ,  SGCOPY  ,  CX(SG)SCAL          **
**              =C(Z)DOTU, =S(D)SCAL, =S(D)COPY, =C(Z)S(D)SCAL        **
**    LINPACK : CPOCO, CPOSL, S(D)GTSL                                **
**    EISPACK : CBAL, CORTH, COMQR                                    **
**    CRAY    : RANSET, RCFFT2, ORDERS                                **
**    HGOLIB  : RFT2 (NOT AT IPP)                                     **
**                                                                    **
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMMOD
*CALL COMDIM
*CALL COMLAB
*CALL COMIT
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMEQV
*CALL COMEQV2D
*CALL COMGEM
*CALL COMIOD
*CALL COMMEW
*CALL COMSPL
*CALL COMFFT
*CALL COMESH
*CALL COMESH2
*CALL CORE1
*CALL COMBND
*CALL COMGEW
*CALL COMPIO
*CALL COMDIAG
C
      REAL DUMMY(3)
      CHARACTER*11  TXTPL(5)
      COMPLEX EWOUT
C
      DATA TXTPL /'QR         ','STEUERWALD ','OUT-OF-CORE',
     >            'IN-CORE-OOC','LANCZOS    '/
C
      NAMELIST / NEWRUN / MODE, EQNAME, NLTORE, NG,
     >                    RFOUR, NTOR, EPS,
     >                    ASPECT, SIG1, SIG2, XR1, XR2,
     >                    SBEGIN, SEND, ITER,
     >                    NPLOT, IAS,YMIN, YMAX, NDIAGFK, ISLOW,
     >                    IGAP, DSCALE, DFLATS, YRMIN, YRMAX,
     >                    GAMMAPER, GAMMAPAR
C
CC
      IF(LANZ.GT.MANZ) STOP '**ERROR: LANZ > MANZ'
      CALL PRESET
      WRITE(*,*) GAMMAPAR,GAMMAPER
C
      READ(NIN,NEWRUN)
      WRITE(EQNAME,'(A10)') ' '
      LABEL(1:3) = VERSION
C
C     OPENING PLOT FILE :
C     =================
*IF KUL
      IPLOT = 4
      CALL BEGPLT(IPLOT)
*ELSE
      CALL BEGPLT('CASPLOT')
*ENDIF
C
      RFOUR1 = RFOUR(1)
      DO 20 JJ=1,MANZ
         RFOUR(JJ) = RFOUR1 + FLOAT((JJ-1))
   20 CONTINUE
C
c$$$      EWSHIFT = VSHIFT(1)
c$$$      SIGMA   = VSHIFT(1)
c$$$      DO 40 I=0,NRS-1
c$$$         SR = REAL(EWSHIFT) + DRS*I
c$$$         DO 30 J=0,NIS-1
c$$$            SI = AIMAG(EWSHIFT) + DIS*J
c$$$            VSHIFT(I*NIS+J+1) = CMPLX(SR,SI)
c$$$   30    CONTINUE
c$$$   40 CONTINUE
C
c$$$      REWIND NOUTP
c$$$      WRITE(NOUT,41) VERSION,EQNAME
c$$$      WRITE(NOUTP,41) VERSION,EQNAME
c$$$      IF(MODE.GT.10) THEN
c$$$         WRITE(NOUT,42) NRTEST,TXTPL(MODSOL)
c$$$         WRITE(NOUTP,42) NRTEST,TXTPL(MODSOL)
c$$$         IF(MODSOL.GE.2.AND.MODSOL.LE.4) THEN
c$$$            WRITE(NOUT,43) EWTEST
c$$$            WRITE(NOUTP,43) EWTEST
c$$$         ENDIF
c$$$      ELSE
c$$$         WRITE(NOUT,44) TXTPL(MODSOL)
c$$$         WRITE(NOUTP,44) TXTPL(MODSOL)
c$$$      ENDIF
c$$$      WRITE(NOUT,45)  LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
c$$$     >                DSURF, IDPOW, RWALL, EPS
c$$$      WRITE(NOUTP,45) LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
c$$$     >                DSURF, IDPOW, RWALL, EPS
c$$$      WRITE(NOUT,61)  ZTE,ZTI
c$$$      WRITE(NOUTP,61) ZTE,ZTI
c$$$      WRITE(NOUT,62)  ZMU,GAMMA
c$$$      WRITE(NOUTP,62) ZMU,GAMMA
c$$$      WRITE(NOUT,46)  (RFOUR(II),II=1,MANZ)
c$$$      WRITE(NOUTP,46) (RFOUR(II),II=1,MANZ)
c$$$      WRITE(NOUT,47)  SIG1, SIG2, XR1, XR2
c$$$      IF(SIG1.LE.99.) THEN
c$$$         WRITE(NOUTP,47) SIG1, SIG2, XR1, XR2
c$$$      ENDIF
c$$$      WRITE(NOUT,48)  NVPSI, NGV, SIGV
c$$$      IF(RWALL.GT.1.) THEN
c$$$         WRITE(NOUTP,48) NVPSI, NGV, SIGV
c$$$      ENDIF
c$$$      WRITE(NOUT,49)  (VSHIFT(II),II=1,NRS*NIS)
c$$$      WRITE(NOUTP,49) (VSHIFT(II),II=1,NRS*NIS)
c$$$      IF(MODSOL.EQ.1) WRITE(NOUT,50) NPLOT,
c$$$     >                (XMINQR(J),YMINQR(J),XMAXQR(J),YMAXQR(J),
c$$$     >                J=1,NPLOT)
c$$$C
c$$$C      CALL WRTEXT(NOUTP)
c$$$C
      NDIM   = NBG
      NGINT  = NG - 1
      ZNKWEL = FLOAT(NTOR)
      IF(.NOT.NLTORE) ZNKWEL = ZNKWEL / ASPECT
C
      WRITE(NOUT,53) NDIM
      WRITE(NOUT,55) NGINT
      WRITE(NOUT,57) NZMA
      WRITE(NOUT,59) ZNKWEL
C------------------------------------------------------------------------
C READ EQUILIBRIUM  FROM DISK
C------------------------------------------------------------------------
      CALL EQUIL
c$$$     CALL BUSSAC
C------------------------------------------------------------------------
C SET POLOIDAL MODE NUMBERS AS FUNCTION OF RADIUS
C------------------------------------------------------------------------
      DO IG=1,NG
c$$$        QI = SPWERT(NPSI,SGRID(IG),Q1,Q2,Q3,Q4,CS,DUMMY)
c$$$		 IF (IFAST .EQ. 1) THEN
c$$$          MSTART(IG) = 1.+ FLOAT(INT(-ZNKWEL * QI - FLOAT((MANZ-1)/2)))
c$$$        ELSE
           MSTART(IG) = RFOUR(1)
c$$$        ENDIF
      ENDDO
      WRITE(*,'(A,2f6.0)') ' MSTART(1),MSTART(NG):',MSTART(1),MSTART(NG)

C------------------------------------------------------------------------
C VACUUM RESPONSE 
C------------------------------------------------------------------------
c      IF(RWALL.GT.1.) CALL VACUUM
C------------------------------------------------------------------------
C SOLVERS 
C------------------------------------------------------------------------
      REWIND(NOUTE)
C
C-----------------------------------------------------------------------
C QR ALGORITHM
C-----------------------------------------------------------------------
      WRITE(NOUT,101)
C
      DO NI = 1, NG
C
         NIP = NI
         WRITE(NOUT,*)'I = ', NIP, 'S = ', SGI(NIP)
         CALL MAT1
         CALL SOLV1
C         CALL DIAG1
      ENDDO


      CALL QRPLOT(SPLOT, WRG,WIG,NDIMP,XV,YV)
 1000 CALL FINPLT
C
      STOP
C
   41 FORMAT(1X,34('*')/1X,'***',28X,'***'/
     >       1X,'***  MISHKA1 ,  VERSION ',A3,'    ***'/
     >       1X,'***  EQUILIBRIUM : ',A10,'  ***'/
     >       1X,'***',28X,'***'/1X,34('*')/)
   42 FORMAT(' TEST CASE ',I2,' : ',A20)
   43 FORMAT(' EIGENVALUE SHOULD BE : ',1P,2E15.5,0P/)
   44 FORMAT(' ',A20,/)
   45 FORMAT(' INPUT DATA :',//,
     >       '    LANZ    = ',I5,        12X,               //
     >       ' NEWRUN :',/,
     >       '    NG      = ',I5,        12X,'ASPECT  = ',1P,E12.4,0P,/
     >       '    ETA     = ',1P,2E12.4,0P,5X,'Q0ZYL   = ',1P,E12.4,0P,/
     >       '    NTOR    = ',I5,        12X,'NLTORE  = ',L7         ,/
     >       '    DSURF   = ',1P,E12.4,0P,5X,'IDPOW   = ',I5         ,/
     >       '    RWALL   = ',1P,E12.4,0P,5X,'EPS     = ',1P,E12.4,0P )
   46 FORMAT('    M       = ',F7.2/(14X,F7.2))
   47 FORMAT('    SIG1    = ',1P,E12.4,0P,5X,'SIG2    = ',1P,E12.4,0P,/
     >       '    XR1     = ',1P,E12.4,0P,5X,'XR2     = ',1P,E12.4,0P )
   48 FORMAT('    NVPSI   = ',I5,        12X,'NGV     = ',I5         ,/
     >       '    SIGV    = ',1P,E12.4,0P,5X )
   49 FORMAT('    VSHIFT  = ',1P,2E12.4,0P/(14X,1P,E12.4,0P))
   50 FORMAT('    NPLOT   = ',I5,' PLOTS IN RANGE :'    /
     >           (4X,4E12.4))
   51 FORMAT(/,' NEWLAN :',/,
     >       '    ISTART  = ',I3,        14X,'ISTOP   = ',I3/
     >       '    KMAX    = ',I3,        14X,'MXLOOP  = ',I3/
     >       '    ISHIFT  = ',I3,        14X,'NUS     = ',I3/
     >       '    XLIML   = ',1P,E12.4,0P,5X,'XLIMR   = ',1P,E12.4,0P,/
     >       '    YLIMB   = ',1P,E12.4,0P,5X,'YLIMT   = ',1P,E12.4,0P,/
     >       '    IHOLE   = ',L7)
   52 FORMAT('    XHOLEL  = ',1P,E12.4,0P,5X,'XHOLER  = ',1P,E12.4,0P,/
     >       '    YHOLEB  = ',1P,E12.4,0P,5X,'YHOLET  = ',1P,E12.4,0P)
   53 FORMAT(/5X,'DIMENSION OF THE A-MATRIX :     NDIM = ',I8)
   55 FORMAT(5X,'NUMBER OF FINITE ELEMENTS :    NGINT = ',I8)
   57 FORMAT(5X,'DIMENSION OF ZMA SUBBLOCK :     NZMA = ',I4/)
   59 FORMAT(/' ZNKWEL =',F7.3)
   61 FORMAT('    TAUE    = ',1P,E12.4,5X,'TAUI    = ',E12.4)
   62 FORMAT('    ZMU     = ',1P,E12.4,5x,'GAMMA   = ',E12.4)
  101 FORMAT(///1X,80('*')/1X,'*',33X,'QR-ALGORITHM',
     >       33X,'*'/1X,80('*'))
  102 FORMAT(///1X,50('*')/1X,'***',44X,'***'/
     >       1X,'***',20X,'STOP',20X,'***'/
     >       1X,'***',10X,'NDIM=',I5,' > NDIM1=',I5,10X,'***'/
     >       1X,'***',44X,'***')
  103 FORMAT(' ***',44X,'***'/
     >       ' ***',5X,'FOR:',34X,'***'/
     >       ' ***',5X,'KILWOR=',I9,23X,'***'/
     >       ' ***',5X,'NGMAX=',I5,28X,'***'/
     >       ' ***',5X,'MANZ=',I3,31X,'***'/
     >       ' ***',5X,'IS MAXIMUM NDIM1=',I6,' POSSIBLE',6X,'***'/
     >       ' ***',5X,'I.E. : NG=',I5,19X,'***'/' ***',44X,'***')
  104 FORMAT(1X,50('*'))
  201 FORMAT(//1X,80('*')/1X,'*',25X,'VECTOR ITERATION (STEUERWALD)',
     >       25X,'*'/1X,80('*')/
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,0P)
  202 FORMAT(///' NDIM=',I5,' > NDIM2=',I5)
  301 FORMAT(///' BLOCKS TOO LARGE - NCV < 1 ')
  302 FORMAT(//1X,80('*')/1X,'*',26X,'VECTOR ITERATION (SCHWARZ)',
     >       27X,'*'/1X,80('*')//
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,0P,/)
  401 FORMAT(//1X,80('*')/1X,'*',26X,'VECTOR ITERATION INCORE-OOC',
     >       25X,'*'/1X,80('*')//
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,/)
  402 FORMAT(///' BLOCKS TOO LARGE - NCV < NG')
  501 FORMAT(///1X,80('*')/1X,'*',29X,'LANCZOS ',
     >       32X,'*'/1X,80('*'))
  502 FORMAT(///' NDIM=',I5,' > NDIM5=',I5)
      END
************************************************************************
*DECK PRESET
      SUBROUTINE PRESET
C-----------------------------------------------------------------------
C INITIALIZE ALL NAMELIST VARIABLES
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMMEW
*CALL COMIT
*CALL COMIOD
*CALL COMESH2
*CALL COMGEW
*CALL COMDIAG
C-----------------------------------------------------------------------
C PHYSICAL VARIABLES 
C-----------------------------------------------------------------------
      RFOUR(1)  =  0.
      DO 20 I= 2,MANZ
   20 RFOUR(I)  =  0.
      NLTORE    = .TRUE.
      NTOR      = -1
      GAMMA     =  5. / 3.
      GAMMAPER  =  2.
      GAMMAPAR  =  3.
      Q0ZYL     =  0.3
      ASPECT    =  1.
      DSURF     =  0.
      DSURF1    =  0.
      DSURF2    =  0.
      DSURF3    =  0.
      VEL3      =  0.
      VSURF     =  0.
      VSURF1    =  0.
      VSURF2    =  0.
      VSURF3    =  0.
      VSURF4    =  0.
      ALPHIN    =  1.
      ETA       =  (0.,0.)
      ZMU       =  0.
      ZTI       =  0.
      ZTE       =  0.
      IEQ       =  1
      IAS       =  0
      IDPOW     =  1
      SIG1      = 9999.
      SIG2      = 9999.
      XR1       = 99999.
      XR2       = 99999.
 25   CONTINUE
      RMIN      = 1.
      NVPSI     = 42
      NGV       = 51
      SIGV      = 9999.
      IVAC      = 1
      ISLOW     = 1
      IGAP      = 0
      DSCALE    = 0
      DFLATS    = 1.

      SBEGIN    = 0.01
      SEND      = 1.0
C--------------------------------------------------------------------------
C VARIABLES FOR DIAGNOSTICS 
C--------------------------------------------------------------------------
      NDIAGFK   = 0
      IBVAC     = 0
      YMIN      = 0.
      YMAX      = 2.
      YRMIN     = -0.01
      YRMAX     = 0.01
C--------------------------------------------------------------------------
C NUMERICAL VARIABLES 
C--------------------------------------------------------------------------
      NG        =  11
      ITER      =  50
      EPS       = 3.E-7
C
      GEWI(1)   = .17392742256872693
      GEWI(2)   = .17392742256872693
      GEWI(3)   = .32607257743127307
      GEWI(4)   = .32607257743127307
C-------------------------------------------------------------------------
C VARIABLES FOR LANCZOS 
C-------------------------------------------------------------------------
c$$$      SVSEED    = 7892713
c$$$C     ISEED     = 123456789
c$$$      MXINIT    = 5
c$$$      SAVTEV    = 1
c$$$      RELTOL    = 1.E-8
c$$$C
c$$$      ISHIFT    = 0
c$$$      ISTART    = 0
c$$$      ISTOP     = 1
c$$$      IHOLE     = .FALSE.
c$$$      KMAX      = 51
c$$$      MXLOOP    = 10
c$$$      NUS       =  0
c$$$      XLIML     = 0.
c$$$      XLIMR     = 1.30
c$$$      YLIMB     = 0.
c$$$      YLIMT     = 10.
c$$$      XHOLEL    =  0.0
c$$$      XHOLER    =  0.0
c$$$      YHOLEB    =  0.0
c$$$      YHOLET    =  0.0
      RETURN
      END
C***********************************************************************
*DECK EQUIL
      SUBROUTINE EQUIL
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE EQUIL  (SPECIFICATION OF THE EQUILIBRIUM)        **
**    --------------------                                            **
**                                                                    **
**    STRUCTURE :                                                     **
**                 EQUIL                                              **
**                   IODSK                                            **
**                   GRID                                             **
**                   MESHAC                                           **
**                     FGAUS                                          **
**                   EQUILV                                           **
**                   FKEQ                                             **
**                     FFTRAN                                         **
**                     SPLFK                                          **
**                     SPLINE                                         **
**                     SPWERT                                         **
**                 x EQVAC                                            **
**                 x VFKEQ                                            **
**                     FFTRAN                                         **
**                     SPLFK                                          **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPIO
*CALL COMGRID
*CALL COMESH2
*CALL COMIOD
C-------------------------------------------- READ EQUILIBRIUM FROM DISK 
      CALL IODSK
C------------------------------------------------- GRID (S - COORDINATES) 
      CALL GRID(SBEGIN,SEND,XWALL)
C----------------------------------------------------- MESH ACCUMULATION 
      RS0 = SGRID(1)
      RSA = SGRID(NG)
      BGF  = 0.3
      FACT = 1.
      IMESHAC = 1
      IF(SIG1.GT.99.) IMESHAC=0
C
      IF(IMESHAC.NE.0) THEN
         WRITE(NOUT,*)
         WRITE(NOUT,*) ' MESH ACCUMULATION AT XR1, XR2 : ',XR1,XR2
         WRITE(NOUT,*)
         CALL MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >               SGRID,NG,NG-1)
      ENDIF
C--------------------------------------- MESH ACCUMULATION IN THE VACUUM 
c$$$      RS0 = SVGRID(1)
c$$$      RSA = SVGRID(NGV)
c$$$      BGF = 0.3
c$$$      XR1 = 1.E-12
c$$$      XR2 = 999999.
c$$$      SIG1 = SIGV
c$$$      SIG2 = 999999.
c$$$      FACT = 1.
c$$$      IMESHAC = 1
c$$$      IF(SIGV.GT.99.) IMESHAC = 0
c$$$C
c$$$      IF(IMESHAC.NE.0) CALL MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
c$$$     >                             SVGRID,NGV,NGV-1)
C
C-------------------------------------------- DEFINE PLASMA COEFFICIENTS
      CALL EQUILV
      CALL FKEQ
C------------------------------------------------ DEFINE GEOMETRY VACUUM 
C
      RETURN
C
    1 FORMAT('1',61('*'),/' *',24X,'GRID POINTS',24X,'*',/1X,61('*'))
    2 FORMAT(10F8.4)
    3 FORMAT(' ',61('*'),
     >       /' *',20X,'VACUUM GRID POINTS',21X,'*',
     >       /' ',61('*'))
      END
************************************************************************
*DECK IODSK
      SUBROUTINE IODSK
C-----------------------------------------------------------------------
C     PERFORMS INPUT FROM DISK
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMGEM
*CALL COMEQUI
*CALL COMEQV2D
*CALL COMIOD
*CALL COMSPL
*CALL COMBND
*CALL COMWEL
*CALL COMDIAG
*CALL COMPCON
C
      REAL     SCALEQ, RBPHI02, RHOI0, PPARI0, PPERI0, RBPHII0, 
     >         WURZEL, DQEC, RAXIS, DFLOW0, DFLOWE
      REAL     PLOTS(100),PLOTQ(101),DUMMY(3)
      REAL     G22AV(NPSIMAX),B0AV(NPSIMAX)
      INTEGER  JS0
C
      REWIND(NMAP)
C------------------------------------- READ MAPPED EQUILIBRIUM FROM DISK
C
      READ(NMAP,*) JS0,NCHI,CPSURF,RADIUS,RAXIS,
     >             (CS(JS),JS=1,JS0+1),(CHI(JC),JC=1,NCHI),
     >             (QS(JS),JS=1,JS0+1),
     >             (GEM11(J),J=NCHI+1,(JS0+1)*NCHI),
     >             (GEM12(J),J=NCHI+1,(JS0+1)*NCHI)
C
C      WRITE(*,*) (GEM12(J),J=NCHI+1,2*NCHI)
      IF (JS0+1.GT.NPSIMAX) THEN
         WRITE(*,*) 'MAPPING JS0 SIZE EXCEEDS NPSIMAX', NPSIMAX
         WRITE(*,*) 'CURRENT JS0 = ', JS0
         STOP
      ENDIF
      IF (IAS.EQ.0) THEN
         IF ((NCHI*2-2).GT.NCHIMAX) THEN
            WRITE(*,*) 'MAPPING NCHI SIZE EXCEEDS NCHIMAX', NCHIMAX/2+1
            WRITE(*,*) 'CURRENT NCHI = ', NCHI
         ENDIF
      ELSE
         IF (NCHI.GT.NCHIMAX) THEN
            WRITE(*,*) 'MAPPING NCHI SIZE EXCEEDS NCHIMAX', NCHIMAX
            WRITE(*,*) 'CURRENT NCHI = ', NCHI
         ENDIF
      ENDIF
      NVCHI = NCHI 
      NPSI = JS0 + 1
      NG = NPSI*NCHI
C
       DO 10 J=1,NG
         GEM33(J) = 1.0
   10 CONTINUE
C------------------------------------------------------------------------
C FOR TOROIDAL GEOMETRY ADDITIONAL INPUT : GEM33(J) = R**2
C------------------------------------------------------------------------
      IF(NLTORE) READ(NMAP,*) (GEM33(J),J=NCHI+1,NG)
      READ(NMAP,*) (RHO  (J),J=NCHI+1,(JS0+1)*NCHI), RHOI0,
     >             (PPAR (J),J=NCHI+1,(JS0+1)*NCHI), PPARI0,
     >             (PPER (J),J=NCHI+1,(JS0+1)*NCHI), PPERI0,
     >             (T    (J),J=NCHI+1,(JS0+1)*NCHI), RBPHII0,
     >             (FLOW3I(J),J=1,JS0+1), DFLOW0, DFLOWE
C
C------------------------------------------ READ ADDITIONAL BOUNDARY DATA 
      IF(RWALL.GT.1.) THEN
         READ(NMAP,*) (VX(JS),JS=1,NCHI)
         READ(NMAP,*) (VY(JS),JS=1,NCHI)
         READ(NMAP,*) ASPI
      ENDIF
C
      DO 20 JC = 1,NCHI
         GEM11(JC) = 0.0
         RHO (JC) = RHOI0
         PPAR(JC) = PPARI0
         PPER(JC) = PPERI0
         T   (JC) = RBPHII0
   20 CONTINUE
      WRITE(*,*) 'PRESSURES AT CENTER'
      WRITE(*,*) 'PPAR = ', PPARI0
      WRITE(*,*) 'PPER = ', PPERI0
      IF(NLTORE) THEN
         DO 30 JC=1,NCHI
            GEM33(JC) = RAXIS**2
   30    CONTINUE
      ENDIF
C
      DO 40 JC=1,NCHI
         CALL SGCOPY(NPSI-1,GEM12(NCHI+JC),NCHI,C1,1)
         CALL SPLINE(NPSI-1,CS(2),C1,0.0,0.0,3,Q1,Q2,Q3,Q4)
         GEM12(JC) = SPWERT(NPSI-1,0.0,Q1,Q2,Q3,Q4,CS(2),DUMMY)
   40 CONTINUE

c$$$      DO I = 1, 16
c$$$         VX(I) = FLOAT(I)
c$$$      ENDDO
c$$$      CALL EXTENDIAS(4,4,VX,-1.)
c$$$      WRITE(*,'(6E10.2)')(VX(I),I=1,24)
c$$$      STOP
      CALL SGSCAL(NPSI*NCHI,1.,GEM12,1)
         
      IF (IAS.LT.1) THEN
         CALL EXTENDIAS(NPSI, NCHI, GEM11, 1.)
         CALL EXTENDIAS(NPSI, NCHI, GEM12,-1.)
         CALL EXTENDIAS(NPSI, NCHI, GEM33, 1.)
         CALL EXTENDIAS(NPSI, NCHI, RHO  , 1.)
         CALL EXTENDIAS(NPSI, NCHI, PPAR , 1.)
         CALL EXTENDIAS(NPSI, NCHI, PPER , 1.)
         CALL EXTENDIAS(NPSI, NCHI, T    , 1.)
         CALL EXTENDIAS(   1, NCHI, VX   , 1.)
         CALL EXTENDIAS(   1, NCHI, VY   ,-1.)
         DO I = 1, NCHI-2
            CHI(I+NCHI) = CHI(I+1) + PI
         ENDDO
         NCHI = NCHI*2 - 2
c$$$         WRITE(*,'(I3,E12.4)')(I,GEM33(I),I=1,NCHI)
         NG = NPSI*NCHI
         IAS = 1
      ENDIF
c$$$      DD = 1.1
c$$$      DO I = 1, NPSI
c$$$         DO J = 1, NCHI
c$$$            NO = (I-1) * NCHI + J
c$$$            CHG = 64./3.*(-1.+DD) * (CS(I)**2-0.5)**3
c$$$     >           +16./3.*(1.-DD) * (CS(I)**2-0.5) + 1.
c$$$            RHO(NO) = RHO(NO) * CHG
c$$$         ENDDO
c$$$      ENDDO
     
C----- GET FT = F(1-DET) AS A FLUX FUNCTION
      DO I = 1, NPSI
         NI = (I-1) * NCHI + 1
         DET = (PPAR(NI) - PPER(NI)) / (GEM11(NI)+T(NI)**2)*GEM33(NI)
         FTS(I) = T(NI) * (1 - DET)
      ENDDO
         
      WRITE(*,*) NPSI, NCHI

      WRITE(NOUT,52) CPSURF
      WRITE(NOUT,53) (QS(JJ),JJ=1,JS0+1)
C------------------------------------------------------------- SPLINES
      DQ1 = (QS(NPSI)-QS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      DQ0 = 0
      DRBPHI0 = 0
      DRBPHIE = (FTS(NPSI)-FTS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,1,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,FTS,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
C
      CALL SGCOPY(NPSI,Q2,1,DQS,1)
      CALL SGCOPY(NPSI,RBP2,1,DFTS,1)
      RETURN
C
   51 FORMAT(//' AFTER Q ON AXIS SCALING : SCALE FACTOR =',1P,E12.4,0P)
   52 FORMAT(/' CPSURF = ',1P,E12.4,0P)
   53 FORMAT(/' QS'/(1X,1P,6E12.4,0P))
c$$$   54 FORMAT(/' P0'/(1X,1P,6E12.4,0P))
c$$$   55 FORMAT(/' RBPHI'/(1X,1P,6E12.4,0P))
c$$$   56 FORMAT(/' DJ0,DJE',1P,2E12.4,0P/' CURJ'/(1X,1P,5E12.4,0P))
      END
**********************************************************************
*DECK DERIVS
      SUBROUTINE DERIVS(ZS, ARRIN, DARR, NPSI, NCHI)
C---------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE S DERIVATE USING SPLINE INTERPOLATION
C ZS    : THE S VALUE OF EACH POINTS (NPSI)
C ARRIN : THE INPUT ARRAY (NCHI*NPSI)
C DARR  : THE RESULTING S DERIVATIVE (NCHI*NPSI)
C NPSI  : NUMBER OF RADIAL POINTS
C NCHI  : NUMBER OF POLODIAL POINTS
C---------------------------------------------------------------------
*CALL COMMAX
      REAL      ZS(*), ARRIN(*), DARR(*)
      REAL      TMP(NGMAX),DD1(NGMAX),DD2(NGMAX),DD3(NGMAX),DD4(NGMAX)
      REAL      ABLTG(4), ADATA

      DO I = 1, NCHI
         CALL SGCOPY(NPSI, ARRIN(I), NCHI, TMP, 1)
         CALL SPLINE(NPSI, ZS, TMP, 0.0, 0.0, 3, DD1, DD2, DD3, DD4)
         DO J = 1, NPSI
            ADATA = SPWERT(NPSI, ZS(J), DD1, DD2, DD3, DD4, ZS, ABLTG)
            DARR((J-1)*NCHI + I) = ABLTG(1)
         ENDDO
      ENDDO
      RETURN
      END
************************************************************************
*DECK EXTENDIAS
      SUBROUTINE EXTENDIAS(NR, NCHI, FDATA, SGN)
C---------------------------------------------------------------------
C IF THE INPUT IS UP-DOWN SYMMETRICAL AND ONLY THE UPPER PART IS PROVIDED, 
C THIS SUBROUTINE WILL EXTEND THE INPUT ARRAY TO THE LOWER PART
C SGN = 1. IF LOWER PART HAS THE SAME SIGN AS UPPER PART, 
C     =-1. IF SIGN IS OPPOSITE
C---------------------------------------------------------------------
*CALL COMMAX
      REAL    FDATA(*), TDATA(NPNC), SGN
      INTEGER NR, NCHI
      NCHID = NCHI - 2
      NCHIT = 2*NCHI - 2
      DO J = 1, NR
         I = NR - J + 1
         NOF = (I-1)*NCHI + 1
         NOB = (I-1)*NCHI + 2
         NO1 = (I-1)*NCHIT + 1
         NO2 = (I-1)*NCHIT + NCHI + 1
         CALL SGCOPY(NCHI ,FDATA(NOF), 1,TDATA(NO1), 1)
         CALL SGCOPY(NCHID,FDATA(NOB),-1,TDATA(NO2), 1)
         CALL SGSCAL(NCHID,SGN,TDATA(NO2),1)
      ENDDO
      CALL SGCOPY(NCHIT*NR, TDATA, 1, FDATA, 1)
      RETURN
      END
************************************************************************
*DECK DERIV
      SUBROUTINE DERIV(ARRIN,DARR,NCHI,IAS)
C---------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE THETA DERIVATE USING FFT.
C ARRIN : THE INPUT ARRAY
C DARR  : THE RESULTING THETA DERIVATIVE 
C NCHI  : THE NUMBER OF POLOIDAL POINTS
C IAS   : 0 FOR UP/DOWN SYMMETRIC EQUILIBRIA, 1 FOR ASYMMETRIC EQUIL.
C---------------------------------------------------------------------
*CALL COMMAX 
      REAL       ARRIN(*),DARR(*),FF(2*NCHIMAX+2),DF(2*NCHIMAX+2)
      INTEGER    INDEX(2*NCHIMAX)

      PI = 2.* ASIN(1.)

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
C        WRITE(*,*)J,ARRIN(J),DARR(J)
   60 CONTINUE
      
      END
************************************************************************
*DECK GRID
      SUBROUTINE GRID(SBEGIN,SEND,XWALL)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
*CALL COMPIO
*CALL COMDIAG
C
      WRITE(NOUT,'(A,2f10.4)')' SBEGIN,SEND : ',SBEGIN,SEND

      DELS  = (SEND-SBEGIN) / FLOAT(NG - 1)
      DO N = 1, NG
         SGRID(N)  = SBEGIN+(N - 1) * DELS
      ENDDO
C
c$$$      DELSV = XWALL/(NGV - 1)
c$$$      DO N = 1, NGV
c$$$        SVGRID(N) = FLOAT(N - 1) * DELSV
c$$$      ENDDO
C
      RETURN
      END     
************************************************************************
C*DECK GRID
C      SUBROUTINE GRID(XWALL)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
C*CALL COMMAX
C*CALL COMPARV
C*CALL COMGRID
C*CALL COMVAC
C*CALL COMVGRD
C
C      DELS  = XWALL / (NG - 1)
C      DO 10 N = 1, NG
C         SGRID(N)  = (N - 1) * DELS
C   10 CONTINUE
C
C      DELSV = XWALL/(NGV - 1)
C      DO 20 N = 1, NGV
C        SVGRID(N) = (N - 1) * DELSV
C   20 CONTINUE
C
C      RETURN
C      END
************************************************************************
*DECK MESHAC
      SUBROUTINE MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >                  SGRID,NG,NGINT)
C-----------------------------------------------------------------------
C MESH ACCUMULATION IN XR1 AND XR2
C-----------------------------------------------------------------------
*CALL COMPIO
C
      REAL SGRID(*)
C
C     CONDITIONS
C
      IF(BGF .EQ.0.) GO TO 60
      IF(XR1 .EQ.0.) GO TO 60
      IF(XR2 .EQ.0.) GO TO 60
      IF(SIG1.EQ.0.) GO TO 60
      IF(SIG2.EQ.0.) GO TO 60
      IF(FACT.EQ.0.) GO TO 60
C------------------------------------------- EVALUATION OF NORM
      JINT = 100*NGINT+1
      ZS   = RS0
      ZSUM = 0.0
      ZDS  = (RSA - RS0) / FLOAT(JINT-1)
      DO 10 J=1,JINT
         ZF   = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
         ZSUM = ZSUM + ZF * ZDS
         ZS   = ZS + ZDS
   10 CONTINUE
      ZNORM   = (RSA-RS0) / (ZSUM)
C
      J1INT = 100
      JINT  = J1INT * NGINT
      ZFD   = (RSA - RS0) / FLOAT(NGINT)
      ZS    = RS0
      ZSUM  = 0.0
      ZF    = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
      ZF    = ZF * ZNORM
      ZDS0  = (RSA - RS0) * ZF / FLOAT(JINT)
      I = 2
   20 CONTINUE
      ZI    = FLOAT(I-1) * ZFD 
      ZDS   = ZDS0 / ZF
      ZS    = ZS + ZDS
      ZSUM1 = ZSUM
      ZF    = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
      ZF    = ZF * ZNORM
      ZSUM  = ZSUM + ZF * ZDS
      IF(ZI.GT.ZSUM) GO TO 20
      ZWL   = (ZI-ZSUM1)/(ZSUM - ZSUM1)
      SGRID(I) = ZS - ZDS * (1.0 - ZWL)
      IF(SGRID(I).LT.SGRID(I-1)) GOTO 40
      I = I + 1
      IF(I.GT.NGINT) GO TO 30
      GO TO 20
   30 CONTINUE
      SGRID(1) = RS0
      SGRID(NGINT+1) = RSA
      SGRID(NGINT) = 0.5 * (SGRID(NGINT-1) + SGRID(NGINT+1))
      IF(SGRID(NGINT-1).LT.RSA) GO TO 50
      WRITE(NOUT,31)
   40 WRITE(NOUT,41)
      STOP
   50 CONTINUE
   60 CONTINUE
C
      RETURN
C
   31 FORMAT('0',' ERR. IN S.R. MESHAC2 : SGRID(NGINT) GT. RSA ')
   41 FORMAT('0',' ERR. IN S.R. MESHAC2 : SGRID(I) .GT. SGRID(I+1) ')
   51 FORMAT('1',61('*'),
     >       /' *',20X,'MESHAC GRID POINTS',21X,'*',
     >       /' ',61('*'))
   53 FORMAT(15F8.4)
      END
************************************************************************
*DECK FGAUS
      FUNCTION FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
C-----------------------------------------------------------------------
C     BGF + (1 - BGF) * (GAUSS1 + FACT * GAUSS2) / FACT
C-----------------------------------------------------------------------
      ZNORM1 = 0.39894 / SIG1
      ZNORM2 = 0.39894 / SIG2
      ZEX1   = -0.5 * (ZS - XR1)**2 / SIG1**2
      ZEX2   = -0.5 * (ZS - XR2)**2 / SIG2**2
      F1     = ZNORM1 * EXP(ZEX1)
      F2     = ZNORM2 * EXP(ZEX2)
      FGAUS  = BGF + (1.0 - BGF) * (F1 + FACT * F2) / FACT
      RETURN
      END
************************************************************************
*DECK EQUILV
      SUBROUTINE EQUILV
C-----------------------------------------------------------------------
C CALCULATE EQUILIBRIUM QUANTITIES ON GAUSSIAN POINTS
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMIOD
*CALL COMSPL
*CALL COMDIAG
      REAL ZS(4), ABLTG(3), ZA, ZB, ZC, ZDIF,
     >     OMEGA(4*NGMAX), RLARM(4*NGMAX)
C
      DO 20  J = 1 , NG
            SGI(J)  = SGRID(J)
            Q(J)    = SPWERT(NPSI,SGRID(J),Q1,Q2,Q3,Q4,CS,ABLTG)
            DQ(J)   = ABLTG(1)
            ZT0(J)  = SPWERT(NPSI,SGRID(J),RBP1,RBP2,RBP3,RBP4,CS,ABLTG)
            ZDT0(J) = ABLTG(1)
            ZDDT0(J)= ABLTG(2)
C
   10    CONTINUE
   20 CONTINUE       

c      WRITE(NOUT,21)
c      DO 30 J=1,4*NGINT
c        WRITE(NOUT,22) SGI(J),Q(J),DQ(J),T(J),DT(J),ETAV(J),DETA(J),
c     >               RHO(J),DRHO(J),ZT0(J),ZDT0(J),FLOW3(J)
c   30 CONTINUE   
C
      DMNQ1 = 1e12
      DMNQ2 = 1e12      
      IQ1 = 0
      IQ2 = 0
      QMIN = 1e12
      QMAX = -1e12
      DO J=1,4*NGINT
        IF (ABS(Q(J)-1.).LT.DMNQ1) THEN
		   DMNQ1 = ABS(Q(J)-1.)
		   IQ1 = J
		 ENDIF
        IF (ABS(Q(J)-2.).LT.DMNQ2) THEN
		   DMNQ2 = ABS(Q(J)-2.)
		   IQ2 = J
		 ENDIF
        IF (QMIN.GT.Q(J)) QMIN=Q(J)
        IF (QMAX.LT.Q(J)) QMAX=Q(J)
      ENDDO
      IF ((QMIN.LE.1.).AND.(QMAX.GE.1.)) THEN
        WRITE(20,32) SGI(IQ1),Q(IQ1),DQ(IQ1)
      ENDIF
      IF ((QMIN.LE.2.).AND.(QMAX.GE.2.)) THEN
        WRITE(20,33) SGI(IQ2),Q(IQ2),DQ(IQ2)
      ENDIF
C
      RETURN   
 21   FORMAT(///7X,'S',11X,'Q',10X,'DQ',11X,'T',10X,'DT',10X,'ETA',
     >       8X,'DETA',9X,'RHO',8X,'DRHO',9X,'T0',10X,'DT0'/1X,132('-'))
   22 FORMAT(1X,1P,11E12.4)
   31 FORMAT(/' Q AT BOUNDARY :',E12.4)
   32 FORMAT(//' S = ',F6.3,'  Q = ',F6.3,'  DQ = ',E12.4,
     >       '  OM*(Q=1) = ',E12.4)
   33 FORMAT(' S = ',F6.3,'  Q = ',F6.3,'  DQ = ',E12.4,
     >       '  OM*(Q=2) = ',E12.4)
      END

*************************************************************************
*DECK FKEQ
      SUBROUTINE FKEQ
C-----------------------------------------------------------------------
C
C     COMPUTATION OF THE SPLINE COEFFICIENTS OF THE FOURIER COEFFICIENTS
C
C     DIMENSIONS:
C
C     INDEX,FWT(2*(NCHI-1)),
C     WORK(3*NCHI-1), ZFK(NCHI,NPSI)
C     HV,GEM11,GEM12,GEM33 (NCHI*NPSI)
C     ALLE FOURIERKOEFFIZIENTEN  (4*NPSI,L)
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMGEM
*CALL COMIOD
*CALL COMEQUI
*CALL COMEQV2D
*CALL COMFFT
*CALL COMDIAG
 
      INTEGER  INDEX(2*(NCHIMAX-1))
      REAL     HV (NPNC), HV1(NPNC), HV2(NPNC), HV3(NPNC), HV4(NPNC)
      REAL     HV5(NPNC), HV6(NPNC),            HV8(NPNC), HV9(NPNC)
      REAL     HVA(NPNC), HVB(NPNC)
      REAL     FWT(2*(NCHIMAX-1))
      REAL     H1(NPSIMAX), H2(NPSIMAX), H3(NPSIMAX), H4(NPSIMAX)
      REAL     DUMMY(3)
      REAL     DFDS(NPNC), DFDT(NPNC), DSGEM11(NPNC)
      REAL     DTGEM33(NPNC), DSGEM33(NPNC)
      REAL     DTGEM12(NPNC)
      REAL     G_11(NPNC), G_12(NPNC), G_22(NPNC)
      REAL     DTG_11(NPNC), DTG_12(NPNC), DTG_22(NPNC),
     >         DSG_11(NPNC), DSG_12(NPNC), DSG_22(NPNC)
      REAL     B02(NPNC), DTB02(NPNC), DSB02(NPNC)
      REAL     DET(NPNC), DTDET(NPNC), DSDET(NPNC)
      REAL     DPPERDT(NPNC),DPPARDT(NPNC), DPPERDS(NPNC), DPPARDS(NPNC)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
      REAL     K11, K21, K22, K23, K31, K32, K33,JAC
*IF CRAY
      COMPLEX  WORK(3*NCHIMAX)
*ELSE
      REAL     WORK(6*NCHIMAX)
*ENDIF
C
      INTEGER  NGES
      REAL     ABLTG(3)
C
      NGES = NPSI*NCHI
      NP1  = NPSI+1
      N2P1 = 2*NPSI+1
      N3P1 = 3*NPSI+1
C
      DO 10 I=1,NCHI
         INDEX(I) = I
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 I=NCHI+1,2*NCHI-2
            INDEX(I) = 2*NCHI-I
 20      CONTINUE
      ENDIF
      IF (ISLOW.GE.1) THEN
         ISLOW = 1
      ELSE
         ISLOW = 0
      ENDIF
      ZSLOW = FLOAT(ISLOW)
      IF (IGAP.GE.1) THEN
         WRITE(*,*) 'DENSITY MODIFIED'
         WRITE(*,*) 'FLATENING FROM S=', DFLATS
         WRITE(*,*) 'PEDESTAL', DSCALE*100, '%'
         PSIFLAT = INT(DFLATS * NPSI)
         DO I = (PSIFLAT-1)*NCHI+1, NPSI*NCHI
            RHO(I) = RHO(I-NCHI)
         ENDDO
         DO I = 1, NPSI*NCHI
            RHO(I) = (1-DSCALE)*RHO(I) + DSCALE
         ENDDO
      ENDIF
C-------------------------- INITIALIZATION OF THE SINE AND COSINE TABLES
      IF (IAS.EQ.0) THEN
         N = 2*(NCHI-1)
      ELSE
         N = NCHI
      ENDIF
C-------------------------- CALCULATE G_11 G_12 G_22 |B0|^2
      DO I=1 , NPSI
         SPS2 = 2 * CS(I) * CPSURF
         DO J = 1, NCHI 
            NO = (I-1)*NCHI + J
            G_11(NO) = SPS2**2 / GEM11(NO) *
     >           (1 + QS(I)**2*GEM33(NO)/T(NO)**2 * GEM12(NO)**2)
            G_12(NO) = - SPS2*QS(I)**2*GEM33(NO)/T(NO)**2 * GEM12(NO)
            G_22(NO) = QS(I)**2 * GEM33(NO) / T(NO)**2 * GEM11(NO)
            B02 (NO) = (T(NO)**2 + GEM11(NO)) / GEM33(NO)
            DET (NO) = (PPAR(NO) - PPER(NO))/B02(NO)
         ENDDO
      ENDDO
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,G_11(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         G_11(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
C-------------------------- CALCULATE D/D(CHI) -----------------------
      DO I=1,NPSI
        CALL DERIV(GEM33((I-1)*NCHI+1),DTGEM33((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(GEM12((I-1)*NCHI+1),DTGEM12((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(G_11 ((I-1)*NCHI+1),DTG_11 ((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(G_12 ((I-1)*NCHI+1),DTG_12 ((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(G_22 ((I-1)*NCHI+1),DTG_22 ((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(PPAR ((I-1)*NCHI+1),DPPARDT((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(PPER ((I-1)*NCHI+1),DPPERDT((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(B02  ((I-1)*NCHI+1),DTB02  ((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(T    ((I-1)*NCHI+1),DFDT   ((I-1)*NCHI+1),NCHI,IAS)
      ENDDO
C------------------------- CALCULATE D/DS ----------------------------
      CALL DERIVS(CS, T   , DFDS  , NPSI, NCHI)
      CALL DERIVS(CS, G_11, DSG_11, NPSI, NCHI)
      CALL DERIVS(CS, G_12, DSG_12, NPSI, NCHI)
      CALL DERIVS(CS, G_22, DSG_22, NPSI, NCHI)
      CALL DERIVS(CS, GEM33, DSGEM33, NPSI, NCHI)
      CALL DERIVS(CS, GEM11, DSGEM11, NPSI, NCHI)
      CALL DERIVS(CS, PPER, DPPERDS, NPSI, NCHI)
      CALL DERIVS(CS, PPAR, DPPARDS, NPSI, NCHI)
      CALL DERIVS(CS, B02, DSB02, NPSI, NCHI)

      DO I=1,NGES
         DSDET(I) = (DPPARDS(I) - DPPERDS(I)) / B02(I) 
     >          - (PPAR(I) - PPER(I)) * DSB02(I) / B02(I)**2
         DTDET(I) = (DPPARDT(I) - DPPERDT(I)) / B02(I) 
     >          - (PPAR(I) - PPER(I)) * DTB02(I) / B02(I)**2
      ENDDO

*IF CRAY
         CALL RCFFT2(1,IDUMMY,N,DUMMY,WORK,DUMMY)
*ELSE
         DO 30 I=1,N/2
            WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
 30      CONTINUE
*ENDIF
C-----------------------------------------------------------------------
C FOURIER ANALYSIS AND SPLINE FOR EVERY FOURIER COEFFICIENT
C-----------------------------------------------------------------------
C                      B(I,J) -> RBIJ, IBIJ
C-----------------------------------------------------------------------
C     B(1,1) = B11/SPS2/Q + B11_2*SPS2*Q + B11_3*Q/SPS2
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = RHO(NO)*SPS2**2*GEM33(NO)/GEM11(NO)/T(NO)
     >           /(1-DET(NO))
            HV1(NO) =-RHO(NO)*GEM33(NO)**2*GEM12(NO)**2/T(NO)**3
     >           /(T(NO)**2+GEM11(NO))/(1-DET(NO))
            HV2(NO) = RHO(NO)*SPS2**2*GEM33(NO)/GEM11(NO)/T(NO)
     >           *(GEM33(NO)*GEM12(NO)**2/T(NO)**2)/(1-DET(NO))
         ENDDO
      ENDDO
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV2(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV2(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB11,IB11)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB11_2,IB11_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB11_3,IB11_3)
C
      NO = 20*NCHI+1
      WRITE(*,*) DET(NO)
C     B(1,2) = B12*(-i) * Q, B(2,1) = B12 * i * Q     
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) =-RHO(NO)*GEM33(NO)**2*GEM12(NO)/T(NO)
     >           / (T(NO)**2 + GEM11(NO))/(1-DET(NO))
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB12,IB12)
C
C     B(1,3) = B13 * (i) * Q
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = RHO(NO)*GEM33(NO)/T(NO)**2 * GEM12(NO)/(1-DET(NO))
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB13,IB13)
C
C     B(2,2) = B22*Q
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = RHO(NO)*GEM33(NO)**2*GEM11(NO)/T(NO)
     >           / (T(NO)**2 + GEM11(NO)) / SPS2/(1-DET(NO))
         ENDDO
      ENDDO
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB22,IB22)
C
C     B(2,3) = B23 * Q
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = RHO(NO)*GEM33(NO)/T(NO)**2*GEM11(NO)/(1-DET(NO))
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB23,IB23)
C
C     B(3,3) * f*Q
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = (GEM33(NO)/T(NO))* B02(NO) * RHO(NO)/(1-DET(NO))
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB33,IB33)
C
C     B(4,4)
      DO I = 1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            HV(NO) = QS(I)*GEM33(NO)/T(NO)
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB44,IB44)
C
C-----------------------------------------------------------------------
C     A(I,J)  --> RAIJ , IAIJ    A(I',J) --> RAIPJ, IAIPJ  
C     A(I,J') --> RAIJP, IAIJP
C-----------------------------------------------------------------------
C
C     A(1,PD), A(2,PD), A(3,PD)
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            B0   = SQRT(B02(NO))
            DSB0 = DSB02(NO) / B0 / 2.
            DTB0 = DTB02(NO) / B0 / 2.
C-------A(1,PD) = A1PD1 + A1PD2 / SPS2 + i(m+nq)A1PD3
            HV(NO) = (-2.*GEM12(NO) * DTB0 / B0**3 + DTGEM12(NO)/B0**2
     >           -GEM12(NO) * DFDT(NO) / B02(NO) / T(NO)) / FTS(I)
            HV1(NO)= (GEM11(NO)/B02(NO)/QS(I)*DQS(I) + DSGEM33(NO)
     >           +GEM33(NO) * DSB0 / B0 - GEM33(NO) * DFDS(NO) / T(NO))
     >           / FTS(I)
            HV2(NO)= GEM12(NO) / B02(NO) / FTS(I)
C-------A(2,PD) = iA2PD1 / SPS2 + (m+nq)A2PD2 / SPS2
            HV3(NO) = (GEM11(NO)*DTB0 - T(NO)**2*DTB0 
     >           + T(NO)*B0*DFDT(NO)) / B0**3 / FTS(I)
            HV4(NO) = GEM11(NO) / B02(NO) / FTS(I)
C-------A(3,PD) = (m+nq) * 1/(1-DET) + i*A3PD
            HV5(NO) = (1/B0*DTB0)/(1-DET(NO))
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA1PD1,IA1PD1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA1PD2,IA1PD2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA1PD3,IA1PD3)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA2PD1,IA2PD1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA2PD2,IA2PD2)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA3PD,IA3PD)

C------A(1,5),A(1,6) ANISOTROPY TERMS
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            B0   = SQRT(B02(NO))
            DSB0 = DSB02(NO) / B0 / 2.
            DTB0 = DTB02(NO) / B0 / 2.
C
            BETA1 = -(2. * GEM11(NO)/B02(NO) /QS(I) * DQS(I)
     >           + 2. * GEM33(NO) * DSB0/B0 + DSGEM33(NO)
     >           - 2. * GEM33(NO) * DFDS(NO)/ T(NO)
     >           + GEM33(NO) * DFTS(I) / FTS(I)
     >           - 2. * GEM12(NO)/B02(NO) * DFDT(NO) / T(NO) * SPS2)
            HH1 = - T(NO)**2/ GEM33(NO)
            HH2 = GEM11(NO) / GEM33(NO)
            HH4 = GEM12(NO) / GEM33(NO)

C-----A(1,5) = M*A15_1/fQ + N*A15_2/f + N*A15_3 + I(M_+NQ)M * A15_4/Q + I(M_+NQ)N * A15_5
            HV(NO) = (DET(NO)*BETA1 + DSDET(NO)*GEM33(NO))*HH1/FTS(I)
            HV1(NO) =(DET(NO)*BETA1 + DSDET(NO)*GEM33(NO))*HH2/FTS(I)
            HV2(NO) = 2.*DTDET(NO)*GEM12(NO) / FTS(I)
            HV3(NO) = - 2. * DET(NO) * GEM12(NO) / B02(NO) *HH1/FTS(I)
            HV4(NO) = - 2. * DET(NO) * GEM12(NO) / B02(NO) *HH2/FTS(I)
C-----A(1,6) = (-A15_2/f - A15_3 - I(M_+NQ) * A15_5)*DQ/Q**2 + I(M+NQ) * A16_1/Qf + (M_+NQ)(M+NQ) * A16_2 * f/Q
            HV5(NO) = HH4 * BETA1 * SPS2 * DET(NO) / FTS(I)
     >         + T(NO)**2/QS(I)**2/GEM33(NO)*DTDET(NO)*G_11(NO)/FTS(I)
            HV6(NO) =   2. * DET(NO) *GEM12(NO)/B02(NO)*HH4/FTS(I)
C-----A(1,6') = (-A15_1 + A15_2)/fQ + A15_3/Q + I(M_+NQ) * (-A15_4 + A15_5)/Q
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA15_1,IA15_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA15_2,IA15_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA15_3,IA15_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA15_4,IA15_4)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA15_5,IA15_5)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA16_1,IA16_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV6,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA16_2,IA16_2)

C------A(2,5),A(2,6) ANISOTROPY TERMS
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            B0   = SQRT(B02(NO))
            DSB0 = DSB02(NO) / B0 / 2.
            DTB0 = DTB02(NO) / B0 / 2.

            BETA2= 2. * T(NO)*DFDT(NO)/B02(NO) - 2.*GEM33(NO)*DTB0/B0 
     >           - DTGEM33(NO)

            HH1 = - T(NO)**2/ GEM33(NO)
            HH2 = GEM11(NO) / GEM33(NO)
            HH4 = GEM12(NO) / GEM33(NO)

C-----A(2,5) = IM*A25_1/Qf + IN*A25_2/f + (M_+NQ)M * A25_3/Qf + (M_+NQ)N * A25_4/f + SOME TERMS~M_
            HV(NO) = (DET(NO)*BETA2 + DTDET(NO)*GEM33(NO))*HH1 /FTS(I)
            HV1(NO) =(DET(NO)*BETA2 - DTDET(NO)*GEM33(NO))*HH2 /FTS(I)
            HV2(NO) = -2.* DET(NO) * GEM11(NO) / B02(NO) * HH1 /FTS(I)
            HV3(NO) = -2.* DET(NO) * GEM11(NO) / B02(NO) * HH2 /FTS(I)
C-----A(2,6) = (-IA25_2/f - (M_+NQ) * A25_4)*DQ/Q**2 + (M+NQ) * A26_1 /Qf + I(M_+NQ)(M+NQ) * A26_2/Q + SOME TERMS~M_
            HV4(NO) = -HH4 * BETA2 * SPS2 * DET(NO) / FTS(I)
     >           - GEM11(NO) * DSDET(NO) / FTS(I)
            HV5(NO) = - 2. * DET(NO) *GEM11(NO)/B02(NO)*HH4/FTS(I)
C-----A(2,6') = I(-A25_1 + A25_2)/fQ + (M_+NQ) * (-A25_3 + A25_4)/fQ + SOME TERMS~M_
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA25_1,IA25_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA25_2,IA25_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA25_3,IA25_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA25_4,IA25_4)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA26_1,IA26_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA26_2,IA26_2)

C------A(3,5),A(3,6) ANISOTROPY TERMS
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            B0   = SQRT(B02(NO))
            DSB0 = DSB02(NO) / B0 / 2.
            DTB0 = DTB02(NO) / B0 / 2.
C
            BETA3 = -(GEM33(NO)* DTDET(NO)/(1-DET(NO))**2 
     >           + 2.*GEM33(NO)*DTB0/B0)
            HH1 = - T(NO)**2 / GEM33(NO)
            HH2 = GEM11(NO) / GEM33(NO)
            HH4 = GEM12(NO) / GEM33(NO)

C-----A(3,5) = iM*A35_1/Q+ iN*A35_2 + iN*A35_3 + (M_+NQ)M * A35_4/Q + (M_NQ)N * A35_5
            HV(NO) = DET(NO)*BETA3 / GEM33(NO) * HH1 / (1-DET(NO))
            HV1(NO) =DET(NO)*BETA3 / GEM33(NO) * HH2 / (1-DET(NO))
            HV2(NO) = - B02(NO) * DTDET(NO) / (1-DET(NO))
            HV3(NO) = - DET(NO) * HH1 / (1-DET(NO))
            HV4(NO) = - DET(NO) * HH2 / (1-DET(NO))
C-----A(3,6) = (-iA35_2 - iA35_3 - (M_+NQ) * A35_5)*DQ/Q**2 + (M+NQ) * A36_1/Q + I(M_+NQ)(M+NQ) * A36_2 * f/Q
            HV5(NO) =-HH4*BETA3*SPS2*DET(NO)/(1-DET(NO))/GEM33(NO)
     >           - B02(NO) * DSDET(NO)/(1-DET(NO))
            HV6(NO) = - DET(NO) * HH4/(1-DET(NO))
C-----A(3,6') = I(-A35_1 + A35_2)/Q +iA35_3/Q + (M_+NQ) * (-A35_4 + A35_5)/Q
         ENDDO
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA35_1,IA35_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA35_2,IA35_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA35_3,IA35_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA35_4,IA35_4)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA35_5,IA35_5)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA36_1,IA36_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV6,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA36_2,IA36_2)

C-----PRESSURE TERMS
C
C     A(4,1) A(4,2) A(4,3), A(4,4)
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            G11 = GEM11(NO) / SPS2**2
            G12 = GEM12(NO) / SPS2
            G22 = (T(NO)**2/QS(I)**2/GEM33(NO)+GEM12(NO)**2) / GEM11(NO)
            GB33= 1. / GEM33(NO)
            JAC = SPS2 * QS(I) * GEM33(NO) / T(NO)
C---  CALCULATE CHRISTOFFEL SYMBOLS            
            C121 = .5 * G11 * DTG_11(NO) + .5 * G12 * DSG_22(NO)
            C221 = .5 * G12 * DTG_11(NO) + .5 * G22 * DSG_22(NO)
            C122 = .5 * G11 * (2*DTG_12(NO) - DSG_22(NO)) 
     >            +.5 * G12 * DTG_22(NO)
            C222 = .5 * G12 * (2*DTG_12(NO) - DSG_22(NO))
     >            +.5 * G22 * DTG_22(NO)
            C331 = .5 * GB33 * DSGEM33(NO)
            C332 = .5 * GB33 * DTGEM33(NO)
            C133 =-.5 * G11 * DSGEM33(NO) -.5 * G12 * DTGEM33(NO)
            C233 =-.5 * G12 * DSGEM33(NO) -.5 * G22 * DTGEM33(NO)
C--- EQUILIBRIUM FIELD
            B2  = T(NO) / QS(I) / GEM33(NO)
            B3  = T(NO) / GEM33(NO)
            B_1 = G_12(NO) * B2
            B_2 = G_22(NO) * B2
            B_3 = T(NO)
C--- S1, S2, S3
            S1 = B2*B_1 * C121 + B2*B_2 * C221 + B3*B_3 * C331
            S2 = B2*B_1 * C122 + B2*B_2 * C222 + B3*B_3 * C332
            S3 = B2*B_3 * C332 + B3*B_1 * C133 + B3*B_2 * C233
C--- RELATIONSHIP BETWEEN v_I AND V^I, K^I_J = KIJ
            K11 = 1 / SPS2 / QS(I)
            K21 = - T(NO)**2*G_12(NO) / 
     >                             (SPS2*QS(I)**3*GEM33(NO)**2*B02(NO))
            K22 = - T(NO)**2          / 
     >                             (SPS2*QS(I)   *GEM33(NO)   *B02(NO))
            K32 =   T(NO)**2*G_22(NO) / 
     >                             (SPS2*QS(I)**2*GEM33(NO)**2*B02(NO))
            K23 = - T(NO) / QS(I) / GEM33(NO)
            K33 = - T(NO) /         GEM33(NO)
            K31 = QS(I) * K21
C
            IF (ABS(G_12(NO)).GT.1.E-16) THEN
               DTK21 = (2.*DFDT(NO)/T(NO) + DTG_12(NO)/G_12(NO) 
     >              -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K21
            ELSE
               DTK21 = (2.*DFDT(NO)/T(NO)  
     >              -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K21
            ENDIF
            DTK31 = QS(I) * DTK21
            DTK22 = (2.*DFDT(NO)/T(NO) 
     >             -  DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K22
            DTK32 = (2.*DFDT(NO)/T(NO) + DTG_22(NO)/G_22(NO) 
     >             -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K32
            DTK23 = (  DFDT(NO)/T(NO) - DTGEM33(NO)/GEM33(NO))   * K23
            DTK33 = QS(I) * DTK23
C            
            DET0J =  -(GAMMAPER-1.) * PPER(NO) / B02(NO) * JAC * ZSLOW
C
C     A(4,1) = HV  + if(_m+nq)HV1 + in HV2 + im HV3
            HV (NO)= GAMMAPER * DPPERDT(NO)*JAC*K21 *ZSLOW
     >              -      DPPERDT(NO)*JAC*K21 *ZSLOW
     >              -      DPPERDS(NO)*GEM33(NO)/T(NO)
     >              + (PPAR(NO)-PPER(NO))*DSB02(NO)/B02(NO)
     >               * GEM33(NO)/T(NO)*(1.-ZSLOW)
     >              -GAMMAPER * PPER(NO) *GEM33(NO)/T(NO)
     >                    *(DSGEM33(NO)/GEM33(NO)-DFDS(NO)/T(NO))*ZSLOW
     >              -DET0J * (B2*B_2*DTK21 + B2*B_3*DTK31
     >                           +S1*K11 + S2*K21 + S3*K31)
            HV1(NO)=-GAMMAPER *PPER(NO) * JAC*K21*ZSLOW/SPS2
            HV2(NO)=-DET0J *(B3*B_1*K11 + B3*B_2*K21 + B3*B_3*K31)
     >           
            HV3(NO)=-DET0J *(B2*B_1*K11 + B2*B_2*K21 + B2*B_3*K31)
     >           
C
C     A(4,2) =iHV4 + _m HV5 + n HV6
            HV4(NO)= GAMMAPER * DPPERDT(NO)*JAC*K22 *ZSLOW
     >              -DPPERDT(NO) * JAC*K22 *ZSLOW
     >              -DET0J * (B2*B_2*DTK22 + B2*B_3*DTK32
     >                           +S2*K22 + S3*K32)
            HV5(NO)= GAMMAPER* PPER(NO) * JAC * K22 * ZSLOW
            HV6(NO)= GAMMAPER* PPER(NO) * JAC * K32 * ZSLOW
C
C     A(4,3) =(iHV8 + _m HV9 + n HVA + m HVB)*f
            HV8(NO)= (GAMMAPER* DPPERDT(NO)*JAC*K23*ZSLOW
     >              -DPPERDT(NO)*JAC*K23*ZSLOW
     >              -DET0J * (B2*B_2*DTK23 + B2*B_3*DTK33
     >                           +S2*K23 + S3*K33))*ZSLOW/SPS2
            HV9(NO)= GAMMAPER * PPER(NO)  * JAC*K23*ZSLOW/SPS2
            HVA(NO)= (GAMMAPER * PPER(NO) * JAC*K33*ZSLOW
     >              +DET0J * (B3*B_2*K23 + B3*B_3*K33))/SPS2
            HVB(NO)= DET0J * (B2*B_2*K23 + B2*B_3*K33)/SPS2
         ENDDO
      ENDDO
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV (NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV (J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV1(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV1(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV2(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV2(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV3(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV3(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV4(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV4(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV5(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV5(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV6(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV6(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)

         CALL SGCOPY(NPSI-1,HV8(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV8(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV9(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV9(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HVA(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HVA(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HVB(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HVB(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA41_1,IA41_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA41_2,IA41_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA41_3,IA41_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA41_4,IA41_4)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA42_1,IA42_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA42_2,IA42_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV6,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA42_3,IA42_3)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV8,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA43_1,IA43_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV9,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA43_2,IA43_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HVA,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA43_3,IA43_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HVB,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA43_4,IA43_4)

C     A(4,1')
      DO I=1,NGES
         HV(I) = -GAMMAPER * PPER(I) * GEM33(I) / T(I) * ZSLOW
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA4P1,IA4P1)
C
C---------------------------------------------------
C     A(7,1) A(7,2) A(7,3)
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
            G11 = GEM11(NO) / SPS2**2
            G12 = GEM12(NO) / SPS2
            G22 = (T(NO)**2/QS(I)**2/GEM33(NO)+GEM12(NO)**2) / GEM11(NO)
            GB33= 1. / GEM33(NO)
            JAC = SPS2 * QS(I) * GEM33(NO) / T(NO)
C---  CALCULATE CHRISTOFFEL SYMBOLS            
            C121 = .5 * G11 * DTG_11(NO) + .5 * G12 * DSG_22(NO)
            C221 = .5 * G12 * DTG_11(NO) + .5 * G22 * DSG_22(NO)
            C122 = .5 * G11 * (2*DTG_12(NO) - DSG_22(NO)) 
     >            +.5 * G12 * DTG_22(NO)
            C222 = .5 * G12 * (2*DTG_12(NO) - DSG_22(NO))
     >            +.5 * G22 * DTG_22(NO)
            C331 = .5 * GB33 * DSGEM33(NO)
            C332 = .5 * GB33 * DTGEM33(NO)
            C133 =-.5 * G11 * DSGEM33(NO) -.5 * G12 * DTGEM33(NO)
            C233 =-.5 * G12 * DSGEM33(NO) -.5 * G22 * DTGEM33(NO)
C--- EQUILIBRIUM FIELD
            B2  = T(NO) / QS(I) / GEM33(NO)
            B3  = T(NO) / GEM33(NO)
            B_1 = G_12(NO) * B2
            B_2 = G_22(NO) * B2
            B_3 = T(NO)
C--- S1, S2, S3
            S1 = B2*B_1 * C121 + B2*B_2 * C221 + B3*B_3 * C331
            S2 = B2*B_1 * C122 + B2*B_2 * C222 + B3*B_3 * C332
            S3 = B2*B_3 * C332 + B3*B_1 * C133 + B3*B_2 * C233
C--- RELATIONSHIP BETWEEN v_I AND V^I, K^I_J = KIJ
            K11 = 1 / SPS2 / QS(I)
            K21 = - T(NO)**2*G_12(NO) / 
     >                             (SPS2*QS(I)**3*GEM33(NO)**2*B02(NO))
            K22 = - T(NO)**2          / 
     >                             (SPS2*QS(I)   *GEM33(NO)   *B02(NO))
            K32 =   T(NO)**2*G_22(NO) / 
     >                             (SPS2*QS(I)**2*GEM33(NO)**2*B02(NO))
            K23 = - T(NO) / QS(I) / GEM33(NO)
            K33 = - T(NO) /         GEM33(NO)
            K31 = QS(I) * K21
C
            IF (ABS(G_12(NO)).GT.1.E-16) THEN
               DTK21 = (2.*DFDT(NO)/T(NO) + DTG_12(NO)/G_12(NO) 
     >              -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K21
            ELSE
               DTK21 = (2.*DFDT(NO)/T(NO)  
     >              -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K21
            ENDIF
            DTK31 = QS(I) * DTK21
            DTK22 = (2.*DFDT(NO)/T(NO) 
     >             -  DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K22
            DTK32 = (2.*DFDT(NO)/T(NO) + DTG_22(NO)/G_22(NO) 
     >             -2.*DTGEM33(NO)/GEM33(NO) - DTB02(NO)/B02(NO)) * K32
            DTK23 = (  DFDT(NO)/T(NO) - DTGEM33(NO)/GEM33(NO))   * K23
            DTK33 = QS(I) * DTK23
C            
            DET0J = (GAMMAPAR - 1.) * PPAR(NO) / B02(NO) * JAC * ZSLOW
            GAMMAPAR2 = 1.
C
C     A(7,1) = HV  + if(_m+nq)HV1 + in HV2 + im HV3
            HV (NO)= GAMMAPAR2* DPPARDT(NO)*JAC*K21 *ZSLOW
     >              -      DPPARDT(NO)*JAC*K21 *ZSLOW
     >              -      DPPARDS(NO)*GEM33(NO)/T(NO)
     >              + (PPAR(NO)-PPER(NO))*DSB02(NO)/B02(NO)
     >               * GEM33(NO)/T(NO)*(1.-ZSLOW)/2.
     >              -GAMMAPAR2* PPAR(NO) *GEM33(NO)/T(NO)
     >                    *(DSGEM33(NO)/GEM33(NO)-DFDS(NO)/T(NO))*ZSLOW
     >              -DET0J * (B2*B_2*DTK21 + B2*B_3*DTK31
     >                           +S1*K11 + S2*K21 + S3*K31)
            HV1(NO)=-GAMMAPAR2*PPAR(NO) * JAC*K21*ZSLOW/SPS2
            HV2(NO)=-DET0J *(B3*B_1*K11 + B3*B_2*K21 + B3*B_3*K31)
     >           
            HV3(NO)=-DET0J *(B2*B_1*K11 + B2*B_2*K21 + B2*B_3*K31)
     >           
C
C     A(7,2) =iHV4 + _m HV5 + n HV6
            HV4(NO)= GAMMAPAR2* DPPARDT(NO)*JAC*K22 *ZSLOW
     >              -DPPARDT(NO) * JAC*K22 *ZSLOW
     >              -DET0J * (B2*B_2*DTK22 + B2*B_3*DTK32
     >                           +S2*K22 + S3*K32)
            HV5(NO)= GAMMAPAR2*PPAR(NO) * JAC * K22 * ZSLOW
            HV6(NO)= GAMMAPAR2*PPAR(NO) * JAC * K32 * ZSLOW
C
C     A(7,3) =(iHV8 + _m HV9 + n HVA + m HVB)*f
            HV8(NO)= (GAMMAPAR2*DPPARDT(NO)*JAC*K23*ZSLOW
     >              -DPPARDT(NO)*JAC*K23*ZSLOW
     >              -DET0J * (B2*B_2*DTK23 + B2*B_3*DTK33
     >                           +S2*K23 + S3*K33))*ZSLOW/SPS2
            HV9(NO)= GAMMAPAR2* PPAR(NO)  * JAC*K23*ZSLOW/SPS2
            HVA(NO)= (GAMMAPAR2* PPAR(NO) * JAC*K33*ZSLOW
     >              +DET0J * (B3*B_2*K23 + B3*B_3*K33))/SPS2
            HVB(NO)= DET0J * (B2*B_2*K23 + B2*B_3*K33)/SPS2
         ENDDO
      ENDDO
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV (NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV (J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV1(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV1(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV2(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV2(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV3(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV3(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV4(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV4(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV5(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV5(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV6(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV6(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)

         CALL SGCOPY(NPSI-1,HV8(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV8(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HV9(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV9(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HVA(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HVA(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
         CALL SGCOPY(NPSI-1,HVB(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HVB(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV ,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA71_1,IA71_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV1,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA71_2,IA71_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV2,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA71_3,IA71_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV3,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA71_4,IA71_4)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV4,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA72_1,IA72_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV5,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA72_2,IA72_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV6,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA72_3,IA72_3)

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV8,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA73_1,IA73_1)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV9,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA73_2,IA73_2)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HVA,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA73_3,IA73_3)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HVB,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA73_4,IA73_4)

C     A(7,1')
      DO I=1,NGES
         HV(I) = -GAMMAPAR2* PPAR(I) * GEM33(I) / T(I) * ZSLOW
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RA7P1,IA7P1)

C-----------------------------------------------------------------------
C                      RBPHI  -->  RFF, IFF
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,T,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFF,IFF)
C-----------------------------------------------------------------------
C                      DF/DS  -->  RDFDS, IDFDS
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,DFDS,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDFDS,IDFDS)
C-----------------------------------------------------------------------
C                      GRAD.PSI**2/F --> RGP2OF, IGP2OF
C-----------------------------------------------------------------------
      DO 45 I=1,NGES
         HV(I) = GEM11(I) / T(I)
   45 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGP2OF,IGP2OF)
C-----------------------------------------------------------------------
C                   GRAD.PSI*GRAD.THETA/F  -->  RGPGTOF, IGPGTOF
C-----------------------------------------------------------------------
      DO 55 I=1,NGES
         HV(I) = GEM12(I) / T(I)
   55 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGTOF,IGPGTOF)
C-----------------------------------------------------------------------
C                   D(GRAD.PSI**2)/DS /F -> RDGP2OF, IDGP2OF
C-----------------------------------------------------------------------
      DO 50 I=1,NGES
         HV(I) = DSGEM11(I)/T(I)
   50 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDGP2OF,IDGP2OF)

C-----------------------------------------------------------------------
C         DF/DS GRAD.PSI**2 / F**2 -> RDFGP2OF2, IDFGP2OF2
C-----------------------------------------------------------------------
      DO 75 I=1,NGES
         HV(I) = DFDS(I) * GEM11(I) / T(I)**2
   75 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >           RDFGP2OF2,IDFGP2OF2)
C-----------------------------------------------------------------------
C        S**2*(GRAD.PSI*GRAD.THETA)**2 /GRAD.PSI**2 /F -> RGPGT2OGP2F, IGPGT2OGP2F
C-----------------------------------------------------------------------
      DO 130 I=1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM12(I)**2 / GEM11(I) / T(I)
  130 CONTINUE
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >     RGPGT2OGP2F,IGPGT2OGP2F)
C-----------------------------------------------------------------------
C        S**2*F/GRAD.PSI**2/R**2 -->  RFOGP2R2, IFOGP2R2
C-----------------------------------------------------------------------
      DO 135 I=1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * T(I) / GEM11(I) / GEM33(I)
  135 CONTINUE
      DO J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFOGP2R2,IFOGP2R2)
C-----------------------------------------------------------------------
C         R^2 / F   --> RR2OF, IR2OF
C-----------------------------------------------------------------------
      DO 140 I=1,NGES
         HV(I) = GEM33(I) / T(I)
 140  CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2OF,IR2OF)
C-----------------------------------------------------------------------
C         D(R^2/F)/DS   --> RDR2OF, IDR2OF
C-----------------------------------------------------------------------
      DO 145 I=1,NGES
         HV(I) = DSGEM33(I)/T(I) - GEM33(I) * DFDS(I) /T(I)**2
  145 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDR2OF,IDR2OF)

C-----------------------------------------------------------------------
C         F(DF/DTHETA)/R2   --> FDFDTOR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = T(I) * DFDT(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFDFDTOR2,
     >     IFDFDTOR2)

C-----------------------------------------------------------------------
C         D(GRAD.PSI^2)/DS / R2   --> DGP2OR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DSGEM11(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDGP2OR2,IDGP2OR2)

C-----------------------------------------------------------------------
C         (GRAD.PSI^2) / R2   --> GP2OR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM11(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGP2OR2,IGP2OR2)

C-----------------------------------------------------------------------
C         (GRAD.PSI^2) * DF/DS / F * R2   --> GP2DFOFR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM11(I) * DFDS(I) / T(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >     RGP2DFOFR2,IGP2DFOFR2)

C-----------------------------------------------------------------------
C         (GRAD.PSI.GRAD.THETA) / R2   --> GPGTOR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM12(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGTOR2,IGPGTOR2)

C-----------------------------------------------------------------------
C         F * DF/DS / R2   --> FDFOR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = T(I) * DFDS(I) / GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFDFOR2, IFDFOR2)
C
C-----------------------------------------------------------------------
C         F/R^2 * D/DT(GPGT/F)   --> DXDTFOR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = (DTGEM12(I)*T(I) - GEM12(I)*DFDT(I))/GEM33(I)/T(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >     RDXDTFOR2,IDXDTFOR2)
C
C-----------------------------------------------------------------------
C         DET*F^2 --> DETF2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DET(I) * T(I)**2
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDETF2, IDETF2)
C
C-----------------------------------------------------------------------
C         DET*|GRAD.PSI|^2 --> DETGP2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DET(I) * GEM11(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDETGP2, IDETGP2)
C
C-----------------------------------------------------------------------
C         DET*(GRAD.PSI*GRAD.THETA) --> DETGPGT
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DET(I) * GEM12(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDETGPGT,IDETGPGT)
C
C-----------------------------------------------------------------------
C         R**2 --> R2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2,IR2)
C
C-----------------------------------------------------------------------
C         DS(R**2) --> DR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DSGEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDR2,IDR2)
C-----------------------------------------------------------------------
C         O1MDET --> 1/(1-DET)
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = 1/(1-DET(I))
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RO1MDET,IO1MDET)
      RETURN
 42   FORMAT(4E15.6)
      END
************************************************************************
*DECK FFTRAN
      SUBROUTINE FFTRAN(L,NCHIMAX,NPSI,NCHI,FW,WORK,FWT,ZFK,INDEX,IS)
C-----------------------------------------------------------------------
C     FOURIER ANALYSIS
C-----------------------------------------------------------------------
*CALL COMPIO
*CALL COMPCON
*CALL COMEQUI
 
      INTEGER  L, IS, N, NCHI, NPSI, INDEX(*)
      REAL     FW(NCHI,*), FWT(*), WORK(*), ONETON
      COMPLEX  ZFK(NCHIMAX,*)
C
      IF (IAS.EQ.0) THEN
          N=2*(NCHI-1)
      ELSE
         N=NCHI
      ENDIF
      ONETON = 1./(2.*N)
C
      DO 100 J=1,NPSI
*IF CRAY
         CALL GATHER(N,FWT,FW(1,J),INDEX)
*ELSE
         DO 10 IG = 1,N
            FWT(IG) = FW(INDEX(IG),J)
   10    CONTINUE
*ENDIF
         IF((IAS.EQ.0).AND.(IS.EQ.0))
     >        CALL SGSCAL(NCHI-2,-1.,FWT(NCHI+1),1)
*IF CRAY
         CALL RCFFT2(0,1,N,FWT,WORK,ZFK(1,J))
*ELSE
         CALL RFT2  (FWT,N,1)
         DO 30 I = 1,L
   30        ZFK(I,J) = CMPLX(FWT(2*I-1),-FWT(2*I)) / FLOAT(N)
*ENDIF
*IF CRAY
         CALL CSSCAL(L,ONETON,ZFK(1,J),1)
*ENDIF
         DO 40 I=2,L
            ZFK(I,J)=(N/(PI*(I-1)))**2*0.5*(1.-WORK((N+2)*2+I))*ZFK(I,J)
   40    CONTINUE
c         IF(NDIAGFK.NE.0) WRITE(NOUT,41) J,(ZFK(II,J),II=1,L)
  100 CONTINUE
C
      RETURN
C
   41 FORMAT(' J=',I2,2X,5(1X,1P,2E12.4,0P)/(7X,5(1X,1P,2E12.4,0P)))
      END
************************************************************************
*DECK SPLFK
      SUBROUTINE SPLFK(NCHI,NPSI,NCHIMAX,NP4,L,CS,HV,ZFK,RV,AIV)
C-----------------------------------------------------------------------
C L SPLINES ON FOURIER COEFF(1:NPSI)
C-----------------------------------------------------------------------
      INTEGER  NPSI, NCHI, L, NP1, N2P1, N3P1, NP4, NCHIMAX
      REAL     HV(*), CS(*), RV(NP4,*), AIV(NP4,*)
      COMPLEX  ZFK(NCHIMAX,*)
C
      NP1  = NPSI+1
      N2P1 = 2*NPSI+1
      N3P1 = 3*NPSI+1
      DO 30 J = 1 , L
C
         DO 10 JH=1,NPSI
            HV(JH)      = REAL (ZFK(J,JH))
   10    CONTINUE
         DO 20 JH=1,NPSI
            HV(NPSI+JH) = AIMAG(ZFK(J,JH))
   20    CONTINUE
C
         CALL SPLINE(NPSI,CS,HV,0.0,0.0,3,RV(1,J),RV(NP1,J),RV(N2P1,J),
     >               RV(N3P1,J))
         CALL SPLINE(NPSI,CS,HV(NPSI+1),0.0,0.0,3,AIV(1,J),AIV(NP1,J),
     >               AIV(N2P1,J),AIV(N3P1,J))
C
   30 CONTINUE
C
      RETURN
      END
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
*CALL COMPIO
*CALL COMPCON
C
      INTEGER  N, TYP
      REAL     X(N), Y(N), ALFA, BETA, A(N), B(N), C(N), D(N)
      INTEGER  I, IERR
      REAL     H(NMAX)
C
      IF((TYP.LT.0).OR.(TYP.GT.3)) THEN
         WRITE(NOUT,*) 'FEHLER IN ROUTINE SPLINE: FALSCHER TYP'
         STOP
      ENDIF
C
      IF((N.LT.3).OR.(N.GT.NMAX)) THEN
         WRITE(NOUT,*) 'FEHLER IN ROUTINE  SPLINE: N < 3 ODER N > NMAX'
         STOP
      ENDIF
C
C
C     BERECHNE DIFFERENZ AUFEINENDERFOLGENDER X-WERTE UND
C     UNTERSUCHE MONOTONIE
C
      DO 10 I = 1, N-1
         H(I) = X(I+1)- X(I)
         IF(H(I).LE.0.0) THEN
            WRITE(NOUT,*) 'MONOTONIEFEHLER IN SPLINE: X(I-1) >= X(I)'
            STOP
         ENDIF
   10 CONTINUE
C
C     AUFSTELLEN DES GLEICHUNGSSYSTEMS
C
      DO 20 I = 1, N-2
         A(I) = 3.0 * ((Y(I+2)-Y(I+1)) / H(I+1) - (Y(I+1)-Y(I)) / H(I))
         B(I) = H(I)
         C(I) = H(I+1)
         D(I) = 2.0 * (H(I) + H(I+1))
   20 CONTINUE
C
C     BERUECKSICHTIGEN DER RANDBEDINGUNGEN
C
C     NOT-A-KNOT
C
      IF(TYP.EQ.0) THEN
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
      IF(TYP.EQ.1) THEN
         A(1)   = A(1) - 1.5 * ((Y(2)-Y(1)) / H(1) - ALFA)
         A(N-2) = A(N-2) - 1.5 * (BETA - (Y(N)-Y(N-1)) / H(N-1))
         D(1)   = D(1) - 0.5 * H(1)
         D(N-2) = D(N-2) - 0.5 * H(N-1)
      ENDIF
C
C     2. ABLEITUNG VORGEGEBEN
C
      IF(TYP.EQ.2) THEN
         A(1)   = A(1) - 0.5 * ALFA * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)
      ENDIF
C
C     3. ABLEITUNG VORGEGEBEN
C
      IF(TYP.EQ.3 ) THEN
         A(1)   = A(1) + 0.5 * ALFA * H(1) * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)* H(N-1)
         D(1)   = D(1) + H(1)
         D(N-2) = D(N-2) + H(N-1)
      ENDIF
C
C     BERECHNUNG DER KOEFFIZIENTEN
C
*IF CRAY,JET
      CALL SGTSL(N-2,B,D,C,A,IERR)
*ELSE
      CALL SGTSL(N-2,B,D,C,A,IERR)
*ENDIF
      IF(IERR.NE.0) THEN
         WRITE(NOUT,21)
         STOP
      ENDIF
C
C     UEBERSCHREIBEN DES LOESUNGSVEKTORS
C
      CALL SGCOPY(N-2,A,1,C(2),1)
C
C     IN ABHAENGIGKEIT VON DEN RANDBEDINGUNGEN WIRD DER 1. UND
C     DER LETZTE WERT VON C KORRIGIERT
C
      IF(TYP.EQ.0) THEN
         C(1) = C(2) + H(1) * (C(2)-C(3)) / H(2)
         C(N) = C(N-1) + H(N-1) * (C(N-1)-C(N-2)) / H(N-2)
      ENDIF
C
      IF(TYP.EQ.1) THEN
         C(1) = 1.5*((Y(2)-Y(1)) / H(1) - ALFA) / H(1) - 0.5 * C(2)
         C(N) = -1.5*((Y(N)-Y(N-1)) / H(N-1)-BETA) / H(N-1)-0.5*C(N-1)
      ENDIF
C
      IF(TYP.EQ.2) THEN
         C(1) = 0.5 * ALFA
         C(N) = 0.5 * BETA
      ENDIF
C
      IF(TYP.EQ.3) THEN
         C(1) = C(2) - 0.5 * ALFA * H(1)
         C(N) = C(N-1) + 0.5 * BETA * H(N-1)
      ENDIF
C
      CALL SGCOPY(N,Y,1,A,1)
C
      DO 30 I = 1, N-1
         B(I) = (A(I+1)-A(I)) / H(I) - H(I) * (C(I+1)+2.0 * C(I)) / 3.0
         D(I) = (C(I+1)-C(I)) / (3.0 * H(I))
   30 CONTINUE
C
      B(N) = (3.0 * D(N-1) * H(N-1) + 2.0 * C(N-1)) * H(N-1) + B(N-1)
C
      RETURN
C
   21 FORMAT(1X,'ERROR IN SGTSL: MATRIX SINGULAR')
      END
************************************************************************
*DECK SPWERT
      REAL FUNCTION SPWERT(N,XWERT,A,B,C,D,X,ABLTG)
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
      INTEGER  N
      REAL     XWERT, A(*), B(*), C(*), D(*), X(N), ABLTG(3)
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
C******************************************************************
*DECK QINT     
      subroutine qint(s,p,N,pint)
C******************************************************************
C integrates a array of non-equidistant points using quadratic 
C interpolation
C******************************************************************
      real s(*),p(*),pint(*)
      
      pint(1) = 0.
      is=1
      ie=2
      i =1
      sumj = 0.
      do j=0,2
        j1=j+i
        j2=mod(j+1,3)+i
        j3=mod(j+2,3)+i
        sumj = sumj + p(j1)/((s(j1)-s(j2))*(s(j1)-s(j3))) *
     >           (   (s(ie)**3-s(is)**3)/3. 
     >              - (s(j2)+s(j3))*(s(ie)**2-s(is)**2)/2.
     >              + s(j2)*s(j3)*(s(ie)-s(is))  )
      enddo
      pint(2)=sumj
      do i=1,N-2 
        sumj=0.
        is=i
        ie=i+2
        do j=0,2
          j1=j+i
          j2=mod(j+1,3)+i
          j3=mod(j+2,3)+i
          sumj = sumj + p(j1)/((s(j1)-s(j2))*(s(j1)-s(j3))) *
     >             (   (s(ie)**3-s(is)**3)/3. 
     >                - (s(j2)+s(j3))*(s(ie)**2-s(is)**2)/2.
     >                + s(j2)*s(j3)*(s(ie)-s(is))  )
        enddo
        pint(i+2)= pint(i)+sumj
      enddo
      return
      end   

************************************************************************
*DECK MAT1
      SUBROUTINE MAT1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE MAT  (COMPUTATION OF MATRICES AMAT AND BMAT)     **
**    ------------------                                              **
**                                                                    **
**    STRUCTURE :                                                     **
**                                                                    **
**    MAT1       X MAT2       X MAT3       X MAT4       X MAT5        **
**      CONBMAT      (RANSET)     (RANSET)     (RANSET)     (CXSCAL)  **
**      X CUBFCT     STVAL        STVAL        (STVAL)      CONBMAT   **
**      X QUAFCT     CONAMAT      CONAMAT      CONAMAT        <--     **
**        SPWERT       <--          <--          <--        (SGSCAL)  **
**        FKUBL                   (CXCOPY)     CONBMAT      CONAMAT   **
**      CONAMAT                   CONBMAT        <--          <--     **
**      X CUBFCT                    <--                               **
**      X QUAFCT                  (CXDOTU)                            **
**      X DCUBF                   CGESLP                              **
**      X DQUAF                   CGEFAP                              **
**        SPWERT                     ICMAXP                           **
**        FKUBL                                                       **
**      X ADDBOUND                                                     **
**          FBOUND                                                    **
**                                                                    **
************************************************************************
************************************************************************
C
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR QR
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL CORE1
*CALL COMGRID
C
      DO 10 J=1,NDIM1
          DO 10 I=1,NDIM1
             AMAT(I,J) = (0.0,0.0)
             BMAT(I,J) = (0.0,0.0)
   10 CONTINUE
C
      CALL CONBMAT(NIP,NZMA,BMAT)
      CALL CONAMAT(NIP,NZMA,AMAT)
C
C
      RETURN
      END

C***********************************************************************
*DECK CONBMAT
      SUBROUTINE CONBMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX BMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMEQV2D
*CALL COMWEL
*CALL COMGEW
*CALL COMIOD
*CALL COMFFT
*CALL COMIT
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDQQ(4),INDCC(4),INDCQ(2),INDQC(1)
      REAL     SL,SU,ZDIF,ZA,ZB,ZC,ZSR,ZQ,ZT,QOT,TOQ,
     >         DZQ,DZT,ZRHO,DZRHO,T0,DT0,FKDUMMY,
     >         ZS,HC(4),HQ(4),DUMMY(3),SMZ,SMS
      COMPLEX  B11_K(MANZ+11),B12_K(MANZ+11),B22_K(MANZ+11),
     >         B13_K(MANZ+11),B23_K(MANZ+11),B33_K(MANZ+11),
     >         R2OF_K(MANZ+11),B11_2K(MANZ+11),B11_3K(MANZ+11),
     >         B11, B12, B33, B13, B23, B22,
     >         R2OF,B11_2,B11_3,
     >         ZMA(NZMA*NZMA),FACT(7)
C
C--------------------------------------------------------------------
C B(i,j) has the index value (j-1)*ngl + i
C--------------------------------------------------------------------

      DATA INDQQ / 9, 16, 17, 33 /
      DATA INDCC / 1, 25, 41, 49 /
      DATA INDCQ / 8, 15         /
      DATA INDQC / 2             /
      
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      NBG = NZMA
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
C
         ZSR     = SGI(NI)
         SPS2    = ZSR * CPSURF * 2
         ZQ      = Q(NI)

         FKDUMMY =  1.0
         CW      = CWW
C
c$$$      EPS = 1.E-7                                                           
      DO 11 I = 1 , 4                                                         
         HC(I)= ALOG(EPS)                                                     
         HQ(I)= 1./EPS                                                        
   11 CONTINUE
C
C
      DO  K = 1 , MANZ+5
C     ------------------------
C
        B11_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB11(1,K),RB11(NP1,K),
     >                      RB11(N2P1,K),RB11(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB11(1,K),IB11(NP1,K),
     >                      IB11(N2P1,K),IB11(N3P1,K),CS,DUMMY))
        B11_2K(K) =CMPLX(SPWERT(NPSI,ZSR,RB11_2(1,K),RB11_2(NP1,K),
     >                      RB11_2(N2P1,K),RB11_2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB11_2(1,K),IB11_2(NP1,K),
     >                      IB11_2(N2P1,K),IB11_2(N3P1,K),CS,DUMMY))
        B11_3K(K) =CMPLX(SPWERT(NPSI,ZSR,RB11_3(1,K),RB11_3(NP1,K),
     >                      RB11_3(N2P1,K),RB11_3(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB11_3(1,K),IB11_3(NP1,K),
     >                      IB11_3(N2P1,K),IB11_3(N3P1,K),CS,DUMMY))
        B12_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB12(1,K),RB12(NP1,K),
     >                      RB12(N2P1,K),RB12(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB12(1,K),IB12(NP1,K),
     >                      IB12(N2P1,K),IB12(N3P1,K),CS,DUMMY))
        B13_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB13(1,K),RB13(NP1,K),
     >                      RB13(N2P1,K),RB13(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB13(1,K),IB13(NP1,K),
     >                      IB13(N2P1,K),IB13(N3P1,K),CS,DUMMY))
        B22_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB22(1,K),RB22(NP1,K),
     >                      RB22(N2P1,K),RB22(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB22(1,K),IB22(NP1,K),
     >                      IB22(N2P1,K),IB22(N3P1,K),CS,DUMMY))
        B23_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB23(1,K),RB23(NP1,K),
     >                      RB23(N2P1,K),RB23(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB23(1,K),IB23(NP1,K),
     >                      IB23(N2P1,K),IB23(N3P1,K),CS,DUMMY))
        B33_K(K) = CMPLX(SPWERT(NPSI,ZSR,RB33(1,K),RB33(NP1,K),
     >                      RB33(N2P1,K),RB33(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB33(1,K),IB33(NP1,K),
     >                      IB33(N2P1,K),IB33(N3P1,K),CS,DUMMY))
        R2OF_K(K) = CMPLX(SPWERT(NPSI,ZSR,RR2OF(1,K),RR2OF(NP1,K),
     >                      RR2OF(N2P1,K),RR2OF(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2OF(1,K),IR2OF(NP1,K),
     >                      IR2OF(N2P1,K),IR2OF(N3P1,K),CS,DUMMY))
      ENDDO
C

C
      DO 100  MS = 1 , MANZ 
C     ------------------------------
C
      SMS = MSTART(NI)   + FLOAT(MS-1) * MDIF

      DO 100  MZ = 1 , MANZ
C     ------------------------------
C
      SMZ = MSTART(NI)   + FLOAT(MZ-1) * MDIF
            
      
      FKDUMMY = 0.
c      if (smz .ne. sms) goto 100
      IF (SMZ .EQ. SMS) FKDUMMY = 1. 
      KI = INT(ABS(INT(SMS) - INT(SMZ)))+ 1
      IF ( (SMZ - SMS) .LE. 0) THEN
         B11     = B11_K(KI)
         B11_2   = B11_2K(KI)
         B11_3   = B11_3K(KI)
         B12     = B12_K(KI)
         B13     = B13_K(KI)
         B22     = B22_K(KI)
         B23     = B23_K(KI)
         B33     = B33_K(KI)
         R2OF    = R2OF_K(KI)
      ELSE
         B11     = CONJG(B11_K(KI))
         B11_2   = CONJG(B11_2K(KI))
         B11_3   = CONJG(B11_3K(KI))
         B12     = CONJG(B12_K(KI))
         B13     = CONJG(B13_K(KI))
         B22     = CONJG(B22_K(KI))
         B23     = CONJG(B23_K(KI))
         B33     = CONJG(B33_K(KI))
         R2OF    = CONJG(R2OF_K(KI))
      ENDIF


C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(2,3), B(3,3), B(5,5)
C
      FACT(1) =  B22*ZQ
      FACT(2) =  B23*ZQ
      FACT(3) =  B33*ZQ*SPS2
      FACT(4) =  FKDUMMY
C
      CALL FKUBL(MZ,MS,MANZ,4,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(4,4), B(6,6), B(7,7)
C
      FACT(1) = B11 / SPS2 / ZQ + B11_3 /SPS2 * ZQ + B11_2 * SPS2 * ZQ 
      FACT(2) = ZQ * R2OF
      FACT(3) = FKDUMMY
      FACT(4) = ZQ * R2OF
C
      CALL FKUBL(MZ,MS,MANZ,4,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2), B(1,3)
C
      FACT(1)  = -(0.,1.) * B12 * ZQ
      FACT(2)  =  (0.,1.) * B13 * ZQ * SPS2
C
      CALL FKUBL(MZ,MS,MANZ,2,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1)
C
      FACT(1)  = (0.,1.) * B12 * ZQ
       CALL FKUBL(MZ,MS,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
      IF(MS.EQ.MZ)  GOTO 100
C
  100 CONTINUE
C     --------
C
      FKDUMMY =  0.0
C
  200 CONTINUE
C     --------
C
      RETURN
      END
************************************************************************
*DECK CONAMAT
      SUBROUTINE CONAMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX AMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
*CALL COMWEL
*CALL COMGEW
*CALL COMEQUI
*CALL COMEQV
*CALL COMEQV2D
*CALL COMIOD
*CALL COMFFT
*CALL COMIT
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDCC(6),INDCQ(5),INDQC(6),INDQQ(3),IDCDC(1),
     >         IDCQ(1),IQDC(2),IDCC(2),ICDC(3)
      REAL     SL,SU,ZDIF,ZSR,ZRHO,DZRHO,ZQ,DZQ,
     >         ZFT,DZFT,T0,DT0,DDT0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         ZBIG, HC(4),HQ(4),DHC(4),DHQ(4),DUMMY(3),
     >         SMS,SMZ,MSNQ,MZNQ,FKDUMMY
      COMPLEX  FF_K(MANZ+11),DFDS_K(MANZ+11),GPGTOF_K(MANZ+11),
     >         GP2OF_K(MANZ+11),DGP2OF_K(MANZ+11),
     >         DFGP2OF2_K(MANZ+11),GPGT2OGP2F_K(MANZ+11),
     >         FOGP2R2_K(MANZ+11),R2OF_K(MANZ+11),DR2OF_K(MANZ+11),
     >         FDFDTOR2_K(MANZ+11),DGP2OR2_K(MANZ+11),
     >         GP2OR2_K(MANZ+11),GP2DFOFR2_K(MANZ+11),
     >         GPGTOR2_K(MANZ+11),FDFOR2_K(MANZ+11),
     >         DXDTFOR2_K(MANZ+11),DETF2_K(MANZ+11),
     >         DETGP2_K(MANZ+11),DETGPGT_K(MANZ+11),R2_K(MANZ+11),
     >         DR2_K(MANZ+11),O1MDET_K(MANZ+11)

      COMPLEX  FF,DFDS,GPGTOF,GP2OF,DGP2OF,DFGP2OF2,GPGT2OGP2F,
     >         FOGP2R2,R2OF,DR2OF,FDFDTOR2,DGP2OR2,GP2OR2,
     >         GP2DFOFR2,GPGTOR2,FDFOR2,DXDTFOR2,DETF2,DETGP2,
     >         DETGPGT,R2,DR2,O1MDET,

     >         ZMA(NZMA*NZMA),FACT(14)
      COMPLEX  A41_1_K(MANZ+11),A41_2_K(MANZ+11),A41_3_K(MANZ+11),
     >         A41_4_K(MANZ+11),A42_1_K(MANZ+11),A42_2_K(MANZ+11),
     >         A42_3_K(MANZ+11),A42_4_K(MANZ+11),A43_1_K(MANZ+11),
     >         A43_2_K(MANZ+11),A43_3_K(MANZ+11),A43_4_K(MANZ+11),
     >         A4P1_K(MANZ+11),A71_1_K(MANZ+11),A71_2_K(MANZ+11),
     >         A71_3_K(MANZ+11),A71_4_K(MANZ+11),A72_1_K(MANZ+11),
     >         A72_2_K(MANZ+11),A72_3_K(MANZ+11),A72_4_K(MANZ+11),
     >         A73_1_K(MANZ+11),A73_2_K(MANZ+11),A73_3_K(MANZ+11),
     >         A73_4_K(MANZ+11),A7P1_K(MANZ+11),A15_1_K(MANZ+11),
     >         A15_2_K(MANZ+11),A15_3_K(MANZ+11),A15_4_K(MANZ+11),
     >         A15_5_K(MANZ+11),A16_1_K(MANZ+11),A16_2_K(MANZ+11),
     >         A25_1_K(MANZ+11),A25_2_K(MANZ+11),A25_3_K(MANZ+11),
     >         A25_4_K(MANZ+11),A26_1_K(MANZ+11),A26_2_K(MANZ+11),
     >         A35_1_K(MANZ+11),A35_2_K(MANZ+11),A35_3_K(MANZ+11),
     >         A35_4_K(MANZ+11),A35_5_K(MANZ+11),A36_1_K(MANZ+11),
     >         A36_2_K(MANZ+11),A1PD1_K(MANZ+11),A1PD2_K(MANZ+11),
     >         A1PD3_K(MANZ+11),A2PD1_K(MANZ+11),A2PD2_K(MANZ+11),
     >         A3PD_K(MANZ+11)


      COMPLEX  A41_1,A41_2,A41_3,A41_4,A42_1,A42_2,A42_3,A42_4,
     >         A43_1,A43_2,A43_3,A43_4,A4P1,A71_1,A71_2,A71_3,
     >         A71_4,A72_1,A72_2,A72_3,A72_4,A73_1,A73_2,A73_3,
     >         A73_4,A7P1,A15_1,A15_2,A15_3,A15_4,A15_5,A16_1,
     >         A16_2,A25_1,A25_2,A25_3,A25_4,A26_1,A26_2,A35_1,
     >         A35_2,A35_3,A35_4,A35_5,A36_1,A36_2,A1PD1,A1PD2,
     >         A1PD3,A2PD1,A2PD2,A3PD

C--------------------------------------------------------------------
C A(i,j) has the index value (j-1)*ngl + i
C--------------------------------------------------------------------
C
          DATA INDQQ / 12, 30, 31            /
          DATA INDCC /  4,  6,  7, 22, 36, 43/
          DATA INDCQ / 11, 14, 18, 21, 29    /
          DATA INDQC / 23, 24, 37, 38, 44, 45/
          DATA IDCC  / 22, 36                /
          DATA ICDC  /  4,  7, 36            /
          DATA IQDC  / 37,  38               /
          DATA IDCQ  / 29                    /
          DATA IDCDC / 36                    /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      ZBIG = 1.E+20
      NBG  = NZMA
C
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C--------------------------------------------------------------------      
      
C     ------------
C
      ZSR     = SGI(NI)
      ZQ      = Q(NI)
      DZQ     = DQ(NI)
      ZFT     = ZT0(NI)
      DZFT    = ZDT0(NI)
      
      SPS2    = 2.*ZSR*CPSURF
      DSPS    = 2.*CPSURF
      
      FKDUMMY =  1.0
C
c$$$      EPS = 1.E-7                                                           
      DO 11 I = 1 , 4                                                         
         HC(I)= ALOG(EPS)                                                     
         DHC(I)= 1./EPS                                                       
         HQ(I)= 1./EPS                                                        
         DHQ(I)= -1./EPS**2                                                   
   11 CONTINUE  
C
      DO K = 1, MANZ+11
C     --------------------
C
C
         FF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RFF(1,K),RFF(NP1,K),
     >   RFF(N2P1,K),RFF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IFF(1,K),IFF(NP1,K),
     >   IFF(N2P1,K),IFF(N3P1,K),CS,DUMMY))

         DFDS_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDFDS(1,K),RDFDS(NP1,K),
     >   RDFDS(N2P1,K),RDFDS(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDFDS(1,K),IDFDS(NP1,K),
     >   IDFDS(N2P1,K),IDFDS(N3P1,K),CS,DUMMY))

         GPGTOF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGPGTOF(1,K),RGPGTOF(NP1,K),
     >   RGPGTOF(N2P1,K),RGPGTOF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGPGTOF(1,K),IGPGTOF(NP1,K),
     >   IGPGTOF(N2P1,K),IGPGTOF(N3P1,K),CS,DUMMY))

         GP2OF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGP2OF(1,K),RGP2OF(NP1,K),
     >   RGP2OF(N2P1,K),RGP2OF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGP2OF(1,K),IGP2OF(NP1,K),
     >   IGP2OF(N2P1,K),IGP2OF(N3P1,K),CS,DUMMY))

         DGP2OF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDGP2OF(1,K),RDGP2OF(NP1,K),
     >   RDGP2OF(N2P1,K),RDGP2OF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDGP2OF(1,K),IDGP2OF(NP1,K),
     >   IDGP2OF(N2P1,K),IDGP2OF(N3P1,K),CS,DUMMY))

         DFGP2OF2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDFGP2OF2(1,K),RDFGP2OF2(NP1,K),
     >   RDFGP2OF2(N2P1,K),RDFGP2OF2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDFGP2OF2(1,K),IDFGP2OF2(NP1,K),
     >   IDFGP2OF2(N2P1,K),IDFGP2OF2(N3P1,K),CS,DUMMY))

         GPGT2OGP2F_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGPGT2OGP2F(1,K),RGPGT2OGP2F(NP1,K),
     >   RGPGT2OGP2F(N2P1,K),RGPGT2OGP2F(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGPGT2OGP2F(1,K),IGPGT2OGP2F(NP1,K),
     >   IGPGT2OGP2F(N2P1,K),IGPGT2OGP2F(N3P1,K),CS,DUMMY))

         FOGP2R2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RFOGP2R2(1,K),RFOGP2R2(NP1,K),
     >   RFOGP2R2(N2P1,K),RFOGP2R2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IFOGP2R2(1,K),IFOGP2R2(NP1,K),
     >   IFOGP2R2(N2P1,K),IFOGP2R2(N3P1,K),CS,DUMMY))

         R2OF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RR2OF(1,K),RR2OF(NP1,K),
     >   RR2OF(N2P1,K),RR2OF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IR2OF(1,K),IR2OF(NP1,K),
     >   IR2OF(N2P1,K),IR2OF(N3P1,K),CS,DUMMY))

         DR2OF_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDR2OF(1,K),RDR2OF(NP1,K),
     >   RDR2OF(N2P1,K),RDR2OF(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDR2OF(1,K),IDR2OF(NP1,K),
     >   IDR2OF(N2P1,K),IDR2OF(N3P1,K),CS,DUMMY))

         FDFDTOR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RFDFDTOR2(1,K),RFDFDTOR2(NP1,K),
     >   RFDFDTOR2(N2P1,K),RFDFDTOR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IFDFDTOR2(1,K),IFDFDTOR2(NP1,K),
     >   IFDFDTOR2(N2P1,K),IFDFDTOR2(N3P1,K),CS,DUMMY))

         DGP2OR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDGP2OR2(1,K),RDGP2OR2(NP1,K),
     >   RDGP2OR2(N2P1,K),RDGP2OR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDGP2OR2(1,K),IDGP2OR2(NP1,K),
     >   IDGP2OR2(N2P1,K),IDGP2OR2(N3P1,K),CS,DUMMY))

         GP2OR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGP2OR2(1,K),RGP2OR2(NP1,K),
     >   RGP2OR2(N2P1,K),RGP2OR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGP2OR2(1,K),IGP2OR2(NP1,K),
     >   IGP2OR2(N2P1,K),IGP2OR2(N3P1,K),CS,DUMMY))

         GP2DFOFR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGP2DFOFR2(1,K),RGP2DFOFR2(NP1,K),
     >   RGP2DFOFR2(N2P1,K),RGP2DFOFR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGP2DFOFR2(1,K),IGP2DFOFR2(NP1,K),
     >   IGP2DFOFR2(N2P1,K),IGP2DFOFR2(N3P1,K),CS,DUMMY))

         GPGTOR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RGPGTOR2(1,K),RGPGTOR2(NP1,K),
     >   RGPGTOR2(N2P1,K),RGPGTOR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IGPGTOR2(1,K),IGPGTOR2(NP1,K),
     >   IGPGTOR2(N2P1,K),IGPGTOR2(N3P1,K),CS,DUMMY))

         FDFOR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RFDFOR2(1,K),RFDFOR2(NP1,K),
     >   RFDFOR2(N2P1,K),RFDFOR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IFDFOR2(1,K),IFDFOR2(NP1,K),
     >   IFDFOR2(N2P1,K),IFDFOR2(N3P1,K),CS,DUMMY))

         DXDTFOR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDXDTFOR2(1,K),RDXDTFOR2(NP1,K),
     >   RDXDTFOR2(N2P1,K),RDXDTFOR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDXDTFOR2(1,K),IDXDTFOR2(NP1,K),
     >   IDXDTFOR2(N2P1,K),IDXDTFOR2(N3P1,K),CS,DUMMY))

         DETF2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDETF2(1,K),RDETF2(NP1,K),
     >   RDETF2(N2P1,K),RDETF2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDETF2(1,K),IDETF2(NP1,K),
     >   IDETF2(N2P1,K),IDETF2(N3P1,K),CS,DUMMY))

         DETGP2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDETGP2(1,K),RDETGP2(NP1,K),
     >   RDETGP2(N2P1,K),RDETGP2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDETGP2(1,K),IDETGP2(NP1,K),
     >   IDETGP2(N2P1,K),IDETGP2(N3P1,K),CS,DUMMY))

         DETGPGT_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDETGPGT(1,K),RDETGPGT(NP1,K),
     >   RDETGPGT(N2P1,K),RDETGPGT(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDETGPGT(1,K),IDETGPGT(NP1,K),
     >   IDETGPGT(N2P1,K),IDETGPGT(N3P1,K),CS,DUMMY))
                  R2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RR2(1,K),RR2(NP1,K),
     >   RR2(N2P1,K),RR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IR2(1,K),IR2(NP1,K),
     >   IR2(N2P1,K),IR2(N3P1,K),CS,DUMMY))

         DR2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RDR2(1,K),RDR2(NP1,K),
     >   RDR2(N2P1,K),RDR2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IDR2(1,K),IDR2(NP1,K),
     >   IDR2(N2P1,K),IDR2(N3P1,K),CS,DUMMY))

         O1MDET_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RO1MDET(1,K),RO1MDET(NP1,K),
     >   RO1MDET(N2P1,K),RO1MDET(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IO1MDET(1,K),IO1MDET(NP1,K),
     >   IO1MDET(N2P1,K),IO1MDET(N3P1,K),CS,DUMMY))
C
         A41_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA41_1(1,K),RA41_1(NP1,K),
     >   RA41_1(N2P1,K),RA41_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA41_1(1,K),IA41_1(NP1,K),
     >   IA41_1(N2P1,K),IA41_1(N3P1,K),CS,DUMMY))

         A41_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA41_2(1,K),RA41_2(NP1,K),
     >   RA41_2(N2P1,K),RA41_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA41_2(1,K),IA41_2(NP1,K),
     >   IA41_2(N2P1,K),IA41_2(N3P1,K),CS,DUMMY))

         A41_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA41_3(1,K),RA41_3(NP1,K),
     >   RA41_3(N2P1,K),RA41_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA41_3(1,K),IA41_3(NP1,K),
     >   IA41_3(N2P1,K),IA41_3(N3P1,K),CS,DUMMY))

         A41_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA41_4(1,K),RA41_4(NP1,K),
     >   RA41_4(N2P1,K),RA41_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA41_4(1,K),IA41_4(NP1,K),
     >   IA41_4(N2P1,K),IA41_4(N3P1,K),CS,DUMMY))

         A42_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA42_1(1,K),RA42_1(NP1,K),
     >   RA42_1(N2P1,K),RA42_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA42_1(1,K),IA42_1(NP1,K),
     >   IA42_1(N2P1,K),IA42_1(N3P1,K),CS,DUMMY))

         A42_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA42_2(1,K),RA42_2(NP1,K),
     >   RA42_2(N2P1,K),RA42_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA42_2(1,K),IA42_2(NP1,K),
     >   IA42_2(N2P1,K),IA42_2(N3P1,K),CS,DUMMY))

         A42_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA42_3(1,K),RA42_3(NP1,K),
     >   RA42_3(N2P1,K),RA42_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA42_3(1,K),IA42_3(NP1,K),
     >   IA42_3(N2P1,K),IA42_3(N3P1,K),CS,DUMMY))

         A42_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA42_4(1,K),RA42_4(NP1,K),
     >   RA42_4(N2P1,K),RA42_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA42_4(1,K),IA42_4(NP1,K),
     >   IA42_4(N2P1,K),IA42_4(N3P1,K),CS,DUMMY))

         A43_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA43_1(1,K),RA43_1(NP1,K),
     >   RA43_1(N2P1,K),RA43_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA43_1(1,K),IA43_1(NP1,K),
     >   IA43_1(N2P1,K),IA43_1(N3P1,K),CS,DUMMY))

         A43_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA43_2(1,K),RA43_2(NP1,K),
     >   RA43_2(N2P1,K),RA43_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA43_2(1,K),IA43_2(NP1,K),
     >   IA43_2(N2P1,K),IA43_2(N3P1,K),CS,DUMMY))

         A43_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA43_3(1,K),RA43_3(NP1,K),
     >   RA43_3(N2P1,K),RA43_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA43_3(1,K),IA43_3(NP1,K),
     >   IA43_3(N2P1,K),IA43_3(N3P1,K),CS,DUMMY))

         A43_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA43_4(1,K),RA43_4(NP1,K),
     >   RA43_4(N2P1,K),RA43_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA43_4(1,K),IA43_4(NP1,K),
     >   IA43_4(N2P1,K),IA43_4(N3P1,K),CS,DUMMY))

         A4P1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA4P1(1,K),RA4P1(NP1,K),
     >   RA4P1(N2P1,K),RA4P1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA4P1(1,K),IA4P1(NP1,K),
     >   IA4P1(N2P1,K),IA4P1(N3P1,K),CS,DUMMY))

         A71_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA71_1(1,K),RA71_1(NP1,K),
     >   RA71_1(N2P1,K),RA71_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA71_1(1,K),IA71_1(NP1,K),
     >   IA71_1(N2P1,K),IA71_1(N3P1,K),CS,DUMMY))

         A71_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA71_2(1,K),RA71_2(NP1,K),
     >   RA71_2(N2P1,K),RA71_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA71_2(1,K),IA71_2(NP1,K),
     >   IA71_2(N2P1,K),IA71_2(N3P1,K),CS,DUMMY))

         A71_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA71_3(1,K),RA71_3(NP1,K),
     >   RA71_3(N2P1,K),RA71_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA71_3(1,K),IA71_3(NP1,K),
     >   IA71_3(N2P1,K),IA71_3(N3P1,K),CS,DUMMY))

         A71_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA71_4(1,K),RA71_4(NP1,K),
     >   RA71_4(N2P1,K),RA71_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA71_4(1,K),IA71_4(NP1,K),
     >   IA71_4(N2P1,K),IA71_4(N3P1,K),CS,DUMMY))

         A72_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA72_1(1,K),RA72_1(NP1,K),
     >   RA72_1(N2P1,K),RA72_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA72_1(1,K),IA72_1(NP1,K),
     >   IA72_1(N2P1,K),IA72_1(N3P1,K),CS,DUMMY))

         A72_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA72_2(1,K),RA72_2(NP1,K),
     >   RA72_2(N2P1,K),RA72_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA72_2(1,K),IA72_2(NP1,K),
     >   IA72_2(N2P1,K),IA72_2(N3P1,K),CS,DUMMY))

         A72_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA72_3(1,K),RA72_3(NP1,K),
     >   RA72_3(N2P1,K),RA72_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA72_3(1,K),IA72_3(NP1,K),
     >   IA72_3(N2P1,K),IA72_3(N3P1,K),CS,DUMMY))

         A72_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA72_4(1,K),RA72_4(NP1,K),
     >   RA72_4(N2P1,K),RA72_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA72_4(1,K),IA72_4(NP1,K),
     >   IA72_4(N2P1,K),IA72_4(N3P1,K),CS,DUMMY))

         A73_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA73_1(1,K),RA73_1(NP1,K),
     >   RA73_1(N2P1,K),RA73_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA73_1(1,K),IA73_1(NP1,K),
     >   IA73_1(N2P1,K),IA73_1(N3P1,K),CS,DUMMY))

         A73_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA73_2(1,K),RA73_2(NP1,K),
     >   RA73_2(N2P1,K),RA73_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA73_2(1,K),IA73_2(NP1,K),
     >   IA73_2(N2P1,K),IA73_2(N3P1,K),CS,DUMMY))

         A73_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA73_3(1,K),RA73_3(NP1,K),
     >   RA73_3(N2P1,K),RA73_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA73_3(1,K),IA73_3(NP1,K),
     >   IA73_3(N2P1,K),IA73_3(N3P1,K),CS,DUMMY))

         A73_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA73_4(1,K),RA73_4(NP1,K),
     >   RA73_4(N2P1,K),RA73_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA73_4(1,K),IA73_4(NP1,K),
     >   IA73_4(N2P1,K),IA73_4(N3P1,K),CS,DUMMY))

         A7P1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA7P1(1,K),RA7P1(NP1,K),
     >   RA7P1(N2P1,K),RA7P1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA7P1(1,K),IA7P1(NP1,K),
     >   IA7P1(N2P1,K),IA7P1(N3P1,K),CS,DUMMY))

         A15_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA15_1(1,K),RA15_1(NP1,K),
     >   RA15_1(N2P1,K),RA15_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA15_1(1,K),IA15_1(NP1,K),
     >   IA15_1(N2P1,K),IA15_1(N3P1,K),CS,DUMMY))

         A15_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA15_2(1,K),RA15_2(NP1,K),
     >   RA15_2(N2P1,K),RA15_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA15_2(1,K),IA15_2(NP1,K),
     >   IA15_2(N2P1,K),IA15_2(N3P1,K),CS,DUMMY))

         A15_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA15_3(1,K),RA15_3(NP1,K),
     >   RA15_3(N2P1,K),RA15_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA15_3(1,K),IA15_3(NP1,K),
     >   IA15_3(N2P1,K),IA15_3(N3P1,K),CS,DUMMY))

         A15_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA15_4(1,K),RA15_4(NP1,K),
     >   RA15_4(N2P1,K),RA15_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA15_4(1,K),IA15_4(NP1,K),
     >   IA15_4(N2P1,K),IA15_4(N3P1,K),CS,DUMMY))

         A15_5_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA15_5(1,K),RA15_5(NP1,K),
     >   RA15_5(N2P1,K),RA15_5(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA15_5(1,K),IA15_5(NP1,K),
     >   IA15_5(N2P1,K),IA15_5(N3P1,K),CS,DUMMY))

         A16_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA16_1(1,K),RA16_1(NP1,K),
     >   RA16_1(N2P1,K),RA16_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA16_1(1,K),IA16_1(NP1,K),
     >   IA16_1(N2P1,K),IA16_1(N3P1,K),CS,DUMMY))

         A16_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA16_2(1,K),RA16_2(NP1,K),
     >   RA16_2(N2P1,K),RA16_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA16_2(1,K),IA16_2(NP1,K),
     >   IA16_2(N2P1,K),IA16_2(N3P1,K),CS,DUMMY))

         A25_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA25_1(1,K),RA25_1(NP1,K),
     >   RA25_1(N2P1,K),RA25_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA25_1(1,K),IA25_1(NP1,K),
     >   IA25_1(N2P1,K),IA25_1(N3P1,K),CS,DUMMY))

         A25_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA25_2(1,K),RA25_2(NP1,K),
     >   RA25_2(N2P1,K),RA25_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA25_2(1,K),IA25_2(NP1,K),
     >   IA25_2(N2P1,K),IA25_2(N3P1,K),CS,DUMMY))

         A25_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA25_3(1,K),RA25_3(NP1,K),
     >   RA25_3(N2P1,K),RA25_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA25_3(1,K),IA25_3(NP1,K),
     >   IA25_3(N2P1,K),IA25_3(N3P1,K),CS,DUMMY))

         A25_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA25_4(1,K),RA25_4(NP1,K),
     >   RA25_4(N2P1,K),RA25_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA25_4(1,K),IA25_4(NP1,K),
     >   IA25_4(N2P1,K),IA25_4(N3P1,K),CS,DUMMY))

         A26_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA26_1(1,K),RA26_1(NP1,K),
     >   RA26_1(N2P1,K),RA26_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA26_1(1,K),IA26_1(NP1,K),
     >   IA26_1(N2P1,K),IA26_1(N3P1,K),CS,DUMMY))

         A26_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA26_2(1,K),RA26_2(NP1,K),
     >   RA26_2(N2P1,K),RA26_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA26_2(1,K),IA26_2(NP1,K),
     >   IA26_2(N2P1,K),IA26_2(N3P1,K),CS,DUMMY))

         A35_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA35_1(1,K),RA35_1(NP1,K),
     >   RA35_1(N2P1,K),RA35_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA35_1(1,K),IA35_1(NP1,K),
     >   IA35_1(N2P1,K),IA35_1(N3P1,K),CS,DUMMY))

         A35_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA35_2(1,K),RA35_2(NP1,K),
     >   RA35_2(N2P1,K),RA35_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA35_2(1,K),IA35_2(NP1,K),
     >   IA35_2(N2P1,K),IA35_2(N3P1,K),CS,DUMMY))

         A35_3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA35_3(1,K),RA35_3(NP1,K),
     >   RA35_3(N2P1,K),RA35_3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA35_3(1,K),IA35_3(NP1,K),
     >   IA35_3(N2P1,K),IA35_3(N3P1,K),CS,DUMMY))

         A35_4_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA35_4(1,K),RA35_4(NP1,K),
     >   RA35_4(N2P1,K),RA35_4(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA35_4(1,K),IA35_4(NP1,K),
     >   IA35_4(N2P1,K),IA35_4(N3P1,K),CS,DUMMY))

         A35_5_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA35_5(1,K),RA35_5(NP1,K),
     >   RA35_5(N2P1,K),RA35_5(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA35_5(1,K),IA35_5(NP1,K),
     >   IA35_5(N2P1,K),IA35_5(N3P1,K),CS,DUMMY))

         A36_1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA36_1(1,K),RA36_1(NP1,K),
     >   RA36_1(N2P1,K),RA36_1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA36_1(1,K),IA36_1(NP1,K),
     >   IA36_1(N2P1,K),IA36_1(N3P1,K),CS,DUMMY))

         A36_2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA36_2(1,K),RA36_2(NP1,K),
     >   RA36_2(N2P1,K),RA36_2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA36_2(1,K),IA36_2(NP1,K),
     >   IA36_2(N2P1,K),IA36_2(N3P1,K),CS,DUMMY))

         A1PD1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA1PD1(1,K),RA1PD1(NP1,K),
     >   RA1PD1(N2P1,K),RA1PD1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA1PD1(1,K),IA1PD1(NP1,K),
     >   IA1PD1(N2P1,K),IA1PD1(N3P1,K),CS,DUMMY))

         A1PD2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA1PD2(1,K),RA1PD2(NP1,K),
     >   RA1PD2(N2P1,K),RA1PD2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA1PD2(1,K),IA1PD2(NP1,K),
     >   IA1PD2(N2P1,K),IA1PD2(N3P1,K),CS,DUMMY))

         A1PD3_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA1PD3(1,K),RA1PD3(NP1,K),
     >   RA1PD3(N2P1,K),RA1PD3(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA1PD3(1,K),IA1PD3(NP1,K),
     >   IA1PD3(N2P1,K),IA1PD3(N3P1,K),CS,DUMMY))

         A2PD1_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA2PD1(1,K),RA2PD1(NP1,K),
     >   RA2PD1(N2P1,K),RA2PD1(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA2PD1(1,K),IA2PD1(NP1,K),
     >   IA2PD1(N2P1,K),IA2PD1(N3P1,K),CS,DUMMY))

         A2PD2_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA2PD2(1,K),RA2PD2(NP1,K),
     >   RA2PD2(N2P1,K),RA2PD2(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA2PD2(1,K),IA2PD2(NP1,K),
     >   IA2PD2(N2P1,K),IA2PD2(N3P1,K),CS,DUMMY))

         A3PD_K(K) = CMPLX(SPWERT(NPSI,ZSR,
     >   RA3PD(1,K),RA3PD(NP1,K),
     >   RA3PD(N2P1,K),RA3PD(N3P1,K),CS,DUMMY),
     >   SPWERT(NPSI,ZSR,
     >   IA3PD(1,K),IA3PD(NP1,K),
     >   IA3PD(N2P1,K),IA3PD(N3P1,K),CS,DUMMY))

      ENDDO

      DO 100  MS = 1 , MANZ
     
      SMS = MSTART(NI)   + FLOAT(MS-1) * MDIF

      MSNQ = SMS + ZNKWEL*ZQ 

      DO 100  MZ = 1,MANZ

      SMZ = MSTART(NI)   + FLOAT(MZ-1) * MDIF

      MZNQ = SMZ + ZNKWEL*ZQ 
            
      FKDUMMY = 0.
      
      
c      if (smz .ne. sms) goto 100
      IF (SMZ .EQ. SMS) FKDUMMY = 1. 
      KI = ABS(SMS - SMZ) + 1
      IF (( SMZ - SMS ) .LE. 0) THEN
         FF        = FF_K(KI)
         DFDS      = DFDS_K(KI)
         GPGTOF    = GPGTOF_K(KI)
         GP2OF     = GP2OF_K(KI)
         DGP2OF    = DGP2OF_K(KI)
         DFGP2OF2  = DFGP2OF2_K(KI)
         GPGT2OGP2F= GPGT2OGP2F_K(KI)
         FOGP2R2   = FOGP2R2_K(KI)
         R2OF      = R2OF_K(KI)
         DR2OF     = DR2OF_K(KI)
         FDFDTOR2  = FDFDTOR2_K(KI)
         DGP2OR2   = DGP2OR2_K(KI)
         GP2OR2    = GP2OR2_K(KI)
         GP2DFOFR2 = GP2DFOFR2_K(KI)
         GPGTOR2   = GPGTOR2_K(KI)
         FDFOR2    = FDFOR2_K(KI)
         DXDTFOR2  = DXDTFOR2_K(KI)
         DETF2     = DETF2_K(KI)
         DETGP2    = DETGP2_K(KI)
         DETGPGT   = DETGPGT_K(KI)
         R2        = R2_K(KI)
         DR2       = DR2_K(KI)
         O1MDET    = O1MDET_K(KI)
C     
         A41_1     = A41_1_K(KI)
         A41_2     = A41_2_K(KI)
         A41_3     = A41_3_K(KI)
         A41_4     = A41_4_K(KI)
         A42_1     = A42_1_K(KI)
         A42_2     = A42_2_K(KI)
         A42_3     = A42_3_K(KI)
         A42_4     = A42_4_K(KI)
         A43_1     = A43_1_K(KI)
         A43_2     = A43_2_K(KI)
         A43_3     = A43_3_K(KI)
         A43_4     = A43_4_K(KI)
         A4P1      = A4P1_K(KI)
         A71_1     = A71_1_K(KI)
         A71_2     = A71_2_K(KI)
         A71_3     = A71_3_K(KI)
         A71_4     = A71_4_K(KI)
         A72_1     = A72_1_K(KI)
         A72_2     = A72_2_K(KI)
         A72_3     = A72_3_K(KI)
         A72_4     = A72_4_K(KI)
         A73_1     = A73_1_K(KI)
         A73_2     = A73_2_K(KI)
         A73_3     = A73_3_K(KI)
         A73_4     = A73_4_K(KI)
         A7P1      = A7P1_K(KI)
         A15_1     = A15_1_K(KI)
         A15_2     = A15_2_K(KI)
         A15_3     = A15_3_K(KI)
         A15_4     = A15_4_K(KI)
         A15_5     = A15_5_K(KI)
         A16_1     = A16_1_K(KI)
         A16_2     = A16_2_K(KI)
         A25_1     = A25_1_K(KI)
         A25_2     = A25_2_K(KI)
         A25_3     = A25_3_K(KI)
         A25_4     = A25_4_K(KI)
         A26_1     = A26_1_K(KI)
         A26_2     = A26_2_K(KI)
         A35_1     = A35_1_K(KI)
         A35_2     = A35_2_K(KI)
         A35_3     = A35_3_K(KI)
         A35_4     = A35_4_K(KI)
         A35_5     = A35_5_K(KI)
         A36_1     = A36_1_K(KI)
         A36_2     = A36_2_K(KI)
         A1PD1     = A1PD1_K(KI)
         A1PD2     = A1PD2_K(KI)
         A1PD3     = A1PD3_K(KI)
         A2PD1     = A2PD1_K(KI)
         A2PD2     = A2PD2_K(KI)
         A3PD      = A3PD_K(KI)
      ELSE
         FF        = CONJG(FF_K(KI))
         DFDS      = CONJG(DFDS_K(KI))
         GPGTOF    = CONJG(GPGTOF_K(KI))
         GP2OF     = CONJG(GP2OF_K(KI))
         DGP2OF    = CONJG(DGP2OF_K(KI))
         DFGP2OF2  = CONJG(DFGP2OF2_K(KI))
         GPGT2OGP2F= CONJG(GPGT2OGP2F_K(KI))
         FOGP2R2   = CONJG(FOGP2R2_K(KI))
         R2OF      = CONJG(R2OF_K(KI))
         DR2OF     = CONJG(DR2OF_K(KI))
         FDFDTOR2  = CONJG(FDFDTOR2_K(KI))
         DGP2OR2   = CONJG(DGP2OR2_K(KI))
         GP2OR2    = CONJG(GP2OR2_K(KI))
         GP2DFOFR2 = CONJG(GP2DFOFR2_K(KI))
         GPGTOR2   = CONJG(GPGTOR2_K(KI))
         FDFOR2    = CONJG(FDFOR2_K(KI))
         DXDTFOR2  = CONJG(DXDTFOR2_K(KI))
         DETF2     = CONJG(DETF2_K(KI))
         DETGP2    = CONJG(DETGP2_K(KI))
         DETGPGT   = CONJG(DETGPGT_K(KI))
         R2        = CONJG(R2_K(KI))
         DR2       = CONJG(DR2_K(KI))
         O1MDET    = CONJG(O1MDET_K(KI))
C     
         A41_1     = CONJG(A41_1_K(KI))
         A41_2     = CONJG(A41_2_K(KI))
         A41_3     = CONJG(A41_3_K(KI))
         A41_4     = CONJG(A41_4_K(KI))
         A42_1     = CONJG(A42_1_K(KI))
         A42_2     = CONJG(A42_2_K(KI))
         A42_3     = CONJG(A42_3_K(KI))
         A42_4     = CONJG(A42_4_K(KI))
         A43_1     = CONJG(A43_1_K(KI))
         A43_2     = CONJG(A43_2_K(KI))
         A43_3     = CONJG(A43_3_K(KI))
         A43_4     = CONJG(A43_4_K(KI))
         A4P1      = CONJG(A4P1_K(KI))
         A71_1     = CONJG(A71_1_K(KI))
         A71_2     = CONJG(A71_2_K(KI))
         A71_3     = CONJG(A71_3_K(KI))
         A71_4     = CONJG(A71_4_K(KI))
         A72_1     = CONJG(A72_1_K(KI))
         A72_2     = CONJG(A72_2_K(KI))
         A72_3     = CONJG(A72_3_K(KI))
         A72_4     = CONJG(A72_4_K(KI))
         A73_1     = CONJG(A73_1_K(KI))
         A73_2     = CONJG(A73_2_K(KI))
         A73_3     = CONJG(A73_3_K(KI))
         A73_4     = CONJG(A73_4_K(KI))
         A7P1      = CONJG(A7P1_K(KI))
         A15_1     = CONJG(A15_1_K(KI))
         A15_2     = CONJG(A15_2_K(KI))
         A15_3     = CONJG(A15_3_K(KI))
         A15_4     = CONJG(A15_4_K(KI))
         A15_5     = CONJG(A15_5_K(KI))
         A16_1     = CONJG(A16_1_K(KI))
         A16_2     = CONJG(A16_2_K(KI))
         A25_1     = CONJG(A25_1_K(KI))
         A25_2     = CONJG(A25_2_K(KI))
         A25_3     = CONJG(A25_3_K(KI))
         A25_4     = CONJG(A25_4_K(KI))
         A26_1     = CONJG(A26_1_K(KI))
         A26_2     = CONJG(A26_2_K(KI))
         A35_1     = CONJG(A35_1_K(KI))
         A35_2     = CONJG(A35_2_K(KI))
         A35_3     = CONJG(A35_3_K(KI))
         A35_4     = CONJG(A35_4_K(KI))
         A35_5     = CONJG(A35_5_K(KI))
         A36_1     = CONJG(A36_1_K(KI))
         A36_2     = CONJG(A36_2_K(KI))
         A1PD1     = CONJG(A1PD1_K(KI))
         A1PD2     = CONJG(A1PD2_K(KI))
         A1PD3     = CONJG(A1PD3_K(KI))
         A2PD1     = CONJG(A2PD1_K(KI))
         A2PD2     = CONJG(A2PD2_K(KI))
         A3PD      = CONJG(A3PD_K(KI))
      ENDIF


C      DO 100  MS = 1 , MANZ - KF + 1
C     ------------------------------
C
C      MZ = MS + KF - 1
C
C  
C      SMZ  = RFOUR(MZ)
C      SMS  = RFOUR(MS)
C      MZNQ = SMZ+ZNKWEL*ZQ
C      MSNQ = SMS+ZNKWEL*ZQ
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(4,1),  A(6,1),  A(7,1),  A(1,4), A(1,6), A(1,7)
C     pperv1,   a2v1,   pparv1,  v1pper,  v1a2,  v1ppar   
C
C
      FACT(1) = A41_1 + (0.,1.)*(SMZ+ZNKWEL*ZQ)*A41_2*SPS2
     >             +(0.,1.)*ZNKWEL*A41_3 + (0.,1.)*SMS*A41_4
      FACT(2) = -1. * FKDUMMY
      FACT(3) = A71_1 + (0.,1.)*(SMZ+ZNKWEL*ZQ)*A71_2*SPS2
     >             +(0.,1.)*ZNKWEL*A71_3 + (0.,1.)*SMS*A71_4
      FACT(4) = (DR2/ZFT-R2*DZFT/ZFT**2) / SPS2
     >         -(A1PD1+A1PD2/SPS2+(0.,1.)*MSNQ*A1PD3)
      FACT(5) = SPS2/ZQ*(SMZ+ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ) * 
     >              (GPGT2OGP2F/ZSR**2 + FOGP2R2/ZSR**2/ZQ**2)
     >             +2*(0.,1.)*(SMZ-SMS)/ZQ**2*DZQ * GPGTOF
     >             +DZQ/SPS2/ZQ**3*(2*DZQ*GP2OF-ZQ*DFGP2OF2)
     >             +DZQ/SPS2/ZQ**2*DGP2OF
     >           +(-A15_2/SPS2 - A15_3 - (0.,1.)*MZNQ*A15_5)*DZQ/ZQ**2    ANI
     >           +(0.,1.)*MSNQ*A16_1/ZQ/SPS2 + MSNQ*MZNQ*A16_2*SPS2/ZQ    ANI
      FACT(6) = (A1PD1+A1PD2/SPS2+(0.,1.)*MSNQ*A1PD3)
C
      CALL FKUBL(MZ,MS,MANZ,6 ,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(4,2),  A(7,2),  A(4,3),  A(7,3),  A(1,5)
C     pperv2,  pparv2,  pperv3,  pparv3,   v1a1
C
      FACT(1) = ((0.,1.)*A42_1 + SMZ*A42_2 + ZNKWEL*A42_3)
      FACT(2) = (0.,1.)*A72_1 + SMZ*A72_2 + ZNKWEL*A72_3
      FACT(3) = ((0.,1.)*A43_1 + SMZ*A43_2 + ZNKWEL*A43_3 + SMS*A43_4)
     >     *SPS2
      FACT(4) = ((0.,1.)*A73_1 + SMZ*A73_2 + ZNKWEL*A73_3 + SMS*A73_4)
     >     *SPS2
      FACT(5) = SMS/SPS2/ZQ*DFDS - ZNKWEL/SPS2*DGP2OF
     >             -(0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)*ZNKWEL * GPGTOF
     >             +ZNKWEL/SPS2/ZQ*(ZQ*DFGP2OF2 - 2*DZQ*GP2OF)
     >             +SMS*A15_1/SPS2/ZQ + ZNKWEL*(A15_2/SPS2 + A15_3)         ANI
     >             +(0.,1.)*MZNQ*(SMS*A15_4/ZQ+ZNKWEL*A15_5)                ANI
      
C
      CALL FKUBL(MZ,MS,MANZ,5,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,4),  A(3,4),  A(2,6), A(3,6), A(2,7), A(3,7)
C     v2pper,  v3pper,   v2a2,   v3a2,  v2ppar, v3ppar
C
      FACT(1) = SMS/SPS2 * R2/ZFT
     >          -((0.,1.)*A2PD1 / SPS2 + MSNQ * A2PD2/SPS2)
      FACT(2) =  - (0.,1.) * A3PD
      FACT(3) = (0.,1.)*(SMZ-SMS-ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             +(SMS+2*ZNKWEL*ZQ)*DZQ/SPS2/ZQ**2*GP2OF
     >             -(SMS+ZNKWEL*ZQ)/SPS2/ZQ*DFGP2OF2
     >             +(SMS+ZNKWEL*ZQ)/SPS2/ZQ*DGP2OF
     >             +(-(0.,1.)*A25_2/SPS2 - MZNQ*A25_4)*DZQ/ZQ**2            ANI
     >             +MSNQ*A26_1/ZQ/SPS2 + (0.,1.)*MSNQ*MZNQ*A26_2/ZQ         ANI
     >             -SMZ*DETGP2/ZFT/SPS2/ZQ**2*DZQ                           ANI
     >             +(0.,1.)*SMZ*MSNQ*DETGPGT/ZFT/ZQ                         ANI
      FACT(4) = (SMS+ZNKWEL*ZQ)*(DGP2OR2/ZQ + GP2OR2*DZQ/ZQ**2
     >             - GP2DFOFR2 / ZQ + FDFOR2 / ZQ
     >             +DXDTFOR2 * SPS2/ZQ )
     >             -(0.,1.)*FDFDTOR2 * DZQ/ZQ**2
     >             +(-(0.,1.)*(A35_2 + A35_3)-MZNQ*A35_5)*DZQ/ZQ**2         ANI
     >             +MSNQ*A36_1/ZQ +(0.,1.)*MSNQ*MZNQ*A36_2*SPS2/ZQ          ANI
      FACT(5) = ((0.,1.)*A2PD1 / SPS2 + MSNQ * A2PD2/SPS2)
      FACT(6) = O1MDET * MSNQ + (0.,1.) * A3PD
      CALL FKUBL(MZ,MS,MANZ,6 ,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(5,2),  A(2,5),  A(3,5)
C      a1v2,    v2a1,    v3a1
C
      FACT(1) = FKDUMMY
      FACT(2) =-1./SPS2/ZQ*(SMS*SMZ*FF+ZNKWEL**2*ZQ**2*GP2OF)
     >             -(SMZ-SMS)*SMS/SPS2/ZQ*FF
     >             +(0.,1.)*(SMS*A25_1/ZQ/SPS2 + ZNKWEL*(A25_2/SPS2))       ANI
     >             +MZNQ*(SMS*A25_3/ZQ+ZNKWEL*A25_4)/SPS2                   ANI
     >             -SMZ*SMS*DETF2/SPS2/ZQ/ZFT                               ANI
     >             +SMZ*ZNKWEL*DETGP2/SPS2/ZFT                              ANI
      FACT(3) = (0.,1.) * FDFDTOR2 / ZQ
     >             +(0.,1.)*(SMS*A35_1/ZQ+ZNKWEL*(A35_2+ A35_3))            ANI
     >             +MZNQ*(SMS*A35_4/ZQ+ZNKWEL*A35_5)                        ANI
      CALL FKUBL(MZ,MS,MANZ,3,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',6')
C     dv1dA2 
C
      FACT(1) =  1./SPS2/ZQ*(FF+GP2OF)
     >          +(DETF2 + DETGP2)/SPS2/ZQ/ZFT                             ANI
      CALL FKUBL(MZ,MS,MANZ,1,IDCDC,NBG,NZMA,ZMA,FACT,1.,DHC,DHC)

C      
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',5)
C     dv1A1   
C
      FACT(1) =-SMS/SPS2/ZQ*FF + ZNKWEL/SPS2*GP2OF
     >     -SMS*DETF2/ZFT/ZQ/SPS2 + ZNKWEL*DETGP2/ZFT/SPS2                ANI
C
      CALL FKUBL(MZ,MS,MANZ,1,IDCQ,NBG,NZMA,ZMA,FACT,1.,DHC,HQ)
C

C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,6')  A(3,6')
C     v2dA2,   V3DA2
C
      FACT(1) = 1./SPS2/ZQ*(SMZ*FF - ZNKWEL*ZQ*GP2OF)
     >             +(SMZ-SMS)/SPS2/ZQ*FF
     >             +(0.,1.)*(-A25_1 + A25_2)/SPS2/ZQ                       ANI
     >             +MZNQ * (-A25_3 + A25_4)/SPS2/ZQ                        ANI
     >             +SMZ * (DETF2+DETGP2)/ZQ/SPS2/ZFT                       ANI
      FACT(2) =    (0.,1.)*(-A35_1 + A35_2)/ZQ +(0.,1.)* A35_3/ZQ          ANI
     >             +MZNQ * (-A35_4 + A35_5)/ZQ                             ANI
C
      CALL FKUBL(MZ,MS,MANZ,2,IQDC,NBG,NZMA,ZMA,FACT,1.,HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',4), A(1',6)
C     dv1pPER, dv1a2
C
      FACT(1) = R2 / ZFT / SPS2
      FACT(2) = (0.,1.)*(SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             -DZQ/SPS2/ZQ**2*GP2OF
     >          -DETGP2/SPS2/ZQ**2*DZQ                                     ANI
     >          +(0.,1.) * MSNQ * DETGPGT / ZQ /ZFT                        ANI
C
      CALL FKUBL(MZ,MS,MANZ,2,IDCC,NBG,NZMA,ZMA,FACT,1.,DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(4,1'), A(7,1'), A(1,6'),  
C     pperdv1, ppardv1,  v1dA2,         
C
      FACT(1) = A4P1
      FACT(2) = A7P1
      FACT(3) =-(0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             -1./SPS2/ZQ*DGP2OF - 1./SPS2/ZQ*DFDS
     >             -1./SPS2/ZQ**2*(2.*DZQ*GP2OF - ZQ*DFGP2OF2)
     >             +(-A15_1 + A15_2)/SPS2/ZQ + A15_3/ZQ                    ANI
     >             +(0.,1.) * MZNQ * (-A15_4 + A15_5)/ZQ                   ANI
C
      CALL FKUBL(MZ,MS,MANZ,3,ICDC,NBG,NZMA,ZMA,FACT,1.,HC,DHC)
C 
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',3)
C     dA1A1
C
C      FACT(1:4,1) = 1
C      
C      CALL FKUBL(MZ,MS,MANZ,1,IDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,HQ)
C
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',4')
C     dA1dA2
C
C      FACT(1:4,1) = 1
C      
C      CALL FKUBL(MZ,MS,MANZ,1,IDQDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,DHC)
C
      IF(MS.EQ.MZ) GOTO 100
C     --------------------
C
C
  100 CONTINUE
C     --------
C
      FKDUMMY =  0.0
C
  200 CONTINUE
C     --------
C
      RETURN
      END
************************************************************************       
*DECK FKUBL
      SUBROUTINE FKUBL(MZ,MS,L,IANZ,INDHG,NBG,NZMA,ZMA,FACT,GEW,H1,H2)
C-----------------------------------------------------------------------
C
C     BESETZT DIE ZUSAMMENGEHOERENDEN UNTERBLOECKE U(MZ,MS)
C     DER FOUR.KOEFF. MZ,MS DER UNTERBLOECKE A(IZ,IS).
C                (IZ=MOD(INDHG(I)-1,NGL)+1)
C                (IS=   (INDHG(I)-1)/NGL+1)
C     (D.H. DEN UNTERBLOCK U(MZ,MS) VON A(IZ,IS) IM HAUPTDIAGONAL-BLOCK,
C     IM OBEREN UND UNTEREN NEBENDIAGONALBLOCK UND DEN ERSTEN TEIL
C     DES FOLGENDEN HAUPTDIAGONAL-BLOCKS VON AMAT(SR. CONAMAT).)
C          Z M A ( J )  =  Z M A ( J ) + H 1 * F A C T * H 2
C
C
C     AMAT:
C
C     =========
C     = Z |   =
C     =-- M --=----
C     =   | A =   |
C     =========--------
C         |   |   |   |
C         -------------
C           .   .   .
C
C               .   .   .
C
C                   .   .   .
C                   -------------
C                   |   |   |   |
C                   -------------
C                       |   |   |
C                       ---------
C
C      ZMA:
C
C     --------------------------------------------------------------
C     | A(1,1)  A(1,2)  . . . A(1,8) | A(1,1)  A(1,2) . . . A(1,8) |
C     | A(2,1)  A(2,2)  . . .   .    | A(2,1)    .    . . .    .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     | A(8,1)   .      . . . A(8,8) | A(8,1)    .    . . . A(8,8) |
C     --------------------------------------------------------------
C     | A(1,1)  A(1,2)  . . . A(1,8) | A(1,1)  A(1,2) . . . A(1,8) |
C     | A(2,1)  A(2,2)  . . .   .    | A(2,1)    .    . . .    .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     | A(8,1)   .      . . . A(8,8) | A(8,1)    .    . . . A(8,8) |
C     --------------------------------------------------------------
C
C
C     JEDES A(IZ,IS) BESTEHT AUS EINEM BLOCK DER DIMENSION 2*L
C     IN FOLGENDER FORM:
C     MZ,MS IST DIE NUMERIERUNG DER STOERUNGEN.DER JEWEILIGE FOURIER-
C     KOEFFIZIENT FUER MZ,MS IST: MS-MZ
C
C            MS=1       MS=2      .  .  .  MS=L
C          |--------------------------------------|
C     MZ=1 | U11 U12 | U11 U12 |          |       |
C      "   | U21 U22 | U21 U22 |          |       |
C          |---------|---------|----------|-------|
C     MZ=2 | U11 U12 |                    |       |
C      "   | U21 U22 |                    |       |
C          |---------|--------------------|-------|
C      .   |         |                    |       |
C          |         |                    |       |
C      .   |         |                    |       |
C          |         |                    |       |
C      .   |         |                    |       |
C          |         |                    |       |
C          |------------------------------|-------|
C     MZ=L | U11 U12 |                    |       |
C      "   | U21 U22 |                    |       |
C          |--------------------------------------|
C
C-----------------------------------------------------------------------
C
C
*CALL COMIT
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND,INDO,INDU,INDN,MZ,MS,IANZ,L
      COMPLEX  ZMA(*), FACT(*)
      REAL     H1(*), H2(*), GEW
C
C@    SIZE OF EACH SUB BLOCK                                                                       
        NGL=NBG/L                                                             
C
C@    SCALING "FACT"
c$$$      EPS = 1.E-8                                                                             
      DO 5 J=1,IANZ                                                           
        FACT(J)=FACT(J)*EPS                                                   
    5 CONTINUE                                                                
C                                                                             
      DO 10 I=1,IANZ                                                          
        IND    = ((INDHG(I)-1)/NGL  * L + (MS-1)  ) * NZMA
     >           + MOD(INDHG(I)-1,NGL)  * L + (MZ - 1)  + 1
c$$$         IND = (MS-1)*NGL*NZMA + (INDHG(I)-1)/NGL*NZMA
c$$$     >           + (MZ-1)*NGL + MOD(INDHG(I)-1,NGL) + 1
                 IZ=MOD(INDHG(I)-1,NGL)+1                                     
                 IS=   (INDHG(I)-1)/NGL+1  
C                                                                             
        ZMA(IND)          = ZMA(IND) + H1(1) * FACT(I) * H2(1)                
C                                                                             
   10 CONTINUE  
C
      RETURN
      END

************************************************************************
*DECK CUBFCT
      SUBROUTINE CUBFCT(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H(1)= 3.*Q1**2 - 2.*Q1**3
        H(2)= 3.*Q2**2 - 2.*Q2**3
        H(3)= (S-SU)*Q1**2
        H(4)= (S-SL)*Q2**2
      RETURN
      END
************************************************************************
*DECK QUAFCT
      SUBROUTINE QUAFCT(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
c        DS2=(SU-SL)**2
c        SM=(SU+SL)/2.
c        H(1)= 4.*(S-SL)*(SU-S)/DS2
c        H(2)= 0.0
c        H(3)= 2.*(S-SM)*(S-SL)/DS2
c        H(4)= 2.*(S-SM)*(S-SU)/DS2


        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H(1)= 3.*Q1**2 - 2.*Q1**3
        H(2)= 3.*Q2**2 - 2.*Q2**3
        H(3)= (S-SU)*Q1**2
        H(4)= (S-SL)*Q2**2

      RETURN
      END
************************************************************************
*DECK DCUBF
      SUBROUTINE DCUBF(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
        DS= SU-SL
        H(1)= 6.*(S-SL)/DS**2-6.*(S-SL)**2/DS**3
        H(2)= -6.*(SU-S)/DS**2+6.*(SU-S)**2/DS**3
        H(3)= ((S-SU)*2.*(S-SL)+(S-SL)**2)/DS**2
        H(4)= ((S-SL)*2.*(S-SU)+(S-SU)**2)/DS**2
      RETURN
      END
************************************************************************
*DECK DQUAF
      SUBROUTINE DQUAF(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
c        DS2=(SU-SL)**2
c        H(1)= 4.*(-2.*S+SU+SL)/DS2
c        H(2)= 0.0
c        H(3)= (4.*S-SU-3.*SL)/DS2
c        H(4)= (4.*S-SL-3.*SU)/DS2

        DS= SU-SL
        H(1)= 6.*(S-SL)/DS**2-6.*(S-SL)**2/DS**3
        H(2)= -6.*(SU-S)/DS**2+6.*(SU-S)**2/DS**3
        H(3)= ((S-SU)*2.*(S-SL)+(S-SL)**2)/DS**2
        H(4)= ((S-SL)*2.*(S-SU)+(S-SU)**2)/DS**2

      RETURN
      END

************************************************************************
*DECK SOLV1
      SUBROUTINE SOLV1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULES SOLV1 - SOLV5  (EIGENVALUE SOLVERS)             **
**    -----------------------------                                   **
**                                                                    **
************************************************************************
************************************************************************
**                                                                    **
**    SOLV1 : QR-SOLVER (COMPLEX)                                     **
**    CHANGED TO ALL LAPACK ROUTINES (GTA 13-9-2001)                  **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPIO
*CALL COMDIM
*CALL CORE1
*CALL COMEQV
C
      INTEGER  LWORK,IPIV(NDIM1)
      REAL     RWORK(2*NDIM1)
      COMPLEX  WORK(2*NDIM1), ALPHA(NDIM1), BETA(NDIM1), VR(NDIM1)
C
C
C ... QR - ALGORITHM ...
C
      LWORK = 2*NDIM1
c$$$      do i=1,ndim
c$$$         WRITE(NOUT,'(12E16.4)')(REAL(BMAT(I,J)),J=1,NDIM)
c$$$      enddo
      CALL ZPOTRF('U',NDIM,BMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,3)
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
C      WRITE(*,*)'ZPOTRF'
      CALL ZPOTRS('U',NDIM,NDIM,BMAT,NDIM1,AMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
C      WRITE(*,*)'ZPOTRS'
      
      CALL ZGEEV('N','N', NDIM, AMAT, NDIM1, WRI, VL, NDIM1,
     >            VRI, NDIM1, WORK, LWORK, RWORK, INFO)
C
c$$$      CALL ZGGEV('N','N',NDIM,AMAT,NDIM1,BMAT,NDIM1,ALPHA,BETA,VL,NDIM1,
c$$$     >           VR,NDIM1,WORK,LWORK,RWORK,INFO)
C      WRITE(*,*)'ZGEEV'
      DO 40 I=1,NDIM
         IND=(NIP-1)*NBG+I
         SPLOT(IND)=SGI(NIP)
         WRG(IND)=REAL(WRI(I))
         WIG(IND)=AIMAG(WRI(I))
c$$$         WIG(IND)=ABS(AIMAG(ALPHA(I)/BETA(I)))
   40 CONTINUE
      WRITE(NOUT,43) (I,WRI(I),I=1,NDIM)
c$$$      WRITE(NOUTI,43)(I,WRI(I),I=1,NDIM)
C
      RETURN
C
    3 FORMAT(///5X,'MATRIX BMAT1 NOT POSITIVE DEFINIT')
    4 FORMAT(' ZPOTRF : INFO = ',I4)
    5 FORMAT(' ZGEEV  : INFO = ',I4)
   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')
      END
************************************************************************
*DECK QRPLOT                                                                 
      SUBROUTINE QRPLOT(SPL, WR,WI,NDIM,X,Y)                                       
C           
*CALL COMMAX    
*CALL COMPAR     
*CALL COMPLOT  
*CALL COMEQV
*CALL COMEQV2D
*CALL COMGRID                                                                  
*CALL COMEQUI
*CALL COMIOD
      INTEGER  NDIM                                                           
      REAL  WR(*), WI(*), SPL(*)
      REAL  X(*), Y(*)                                                        
      REAL  GTAE(NDEQ), GEAE(NDEQ), GAM(NDEQ), SMOL(NDEQ)
      INTEGER L,LOR                                                           
      CHARACTER*10 XNAME,YNAME                                                
C
      WRITE(*,*)'QRPLOT'
      CALL LBLBOT('CSMIS-A : QR-PLOT',28)                          
      XNAME = 'S'                                                             
      YNAME = 'IM(LAMBDA)'                                                    
C                                                                             
      L = 1               
      XMIN = 0.
      XMAX = 1.
      DO  10  I = 1,NZMA*NG
         IF ((WI(I) .GE. YMIN)  .AND. (WI(I) .LE. YMAX ))  THEN                
               Y(L) = WI(I)                                                   
               X(L) = SPL(I)                                                   
               L    = L + 1 
               WRITE(25,'(2E16.8)') SPL(I), WI(I)
c               WRITE(25,'(3E16.8)') SPL(I), WR(I), WI(I)
         ENDIF 
                                                                       
   10 CONTINUE                                                                
      L = L - 1                                                               
C      WRITE(*,1000)L,XMIN,XMAX,YMIN,YMAX
C      IF (L .LE. 1) RETURN                                                    
C                      
  
C      WRITE(*,*) ' PLOT SPECTRUM'
      CALL NFRAME(1,1,1,XMIN,XMAX,YMIN,YMAX,
     &           'CONTINUOUS SPECTRUM IMAGINARY',19,XNAME,1,YNAME,10)                                           
      CALL LPLOT(1,1,1201,X,Y,-L,1,'CONTINUOUS SPECTRUM',19,                   
     &           XNAME,1,YNAME,10)                                            
C                                                                             
C      WRITE(*,*) ' PLOT Q-profile'     
      CALL LINCOL(1)                                                
      CALL LPLOT(1,1,1,SGI,Q,-NG,1,' ',1,' ',1,' ',1)                         
C                                                                             
c$$$C      WRITE(*,*) ' PLOT DENSITY'  
c$$$      CALL LINCOL(2)                                                   
c$$$      CALL LPLOT(1,1,1,SGI,RHO,-NG,1,' ',1,' ',1,' ',1)
C                                                                             
      DO 20 I = 1,NPSI
         RHOAVG = (RHO((I-1)*NCHI+1) + RHO((I-1)*NCHI+NCHI/2+1))/2
         GTAE(I) = 1./(2.*QS(I)*SQRT(RHOAVG))
         GEAE(I) = 1./(QS(I)*SQRT(RHOAVG))
c$$$	 GAM(I)  = SQRT( GAMMA *  ZT0(I) / (RHO(I)) * (2. + 1./Q(I)) )
c$$$         SMOL(I) = SQRT( 2. * GAMMA *  ZT0(I) / RHO(I) + 1. / Q(I)**2)
c$$$C	 WRITE(*,'(i5,f8.4,6e12.4)') 
c$$$C     >           I,SGI(I),Q(I),RHO(I),ZT0(I),GTAE(I),GAM(I),SMOL(I)
 20   CONTINUE
C
      CALL LINCOL(3)
      CALL LPLOT(1,1,1,CS,GTAE,-NPSI,1,' ',1,' ',1,' ',1)
c$$$      CALL LINCOL(1)
c$$$      CALL LPLOT(1,1,1,SGI,GAM,-NG,1,' ',1,' ',1,' ',1)
      CALL LINCOL(2)
c$$$C 
c$$$      DO 30 I = 1,NG
c$$$         
c$$$ 30   CONTINUE
c$$$C
      CALL LPLOT(1,1,1,CS,GEAE,-NPSI,1,' ',1,' ',1,' ',1)
c$$$C                                                                           C  
      CALL LBLBOT('CSMIS-A : QR-PLOT',28)                          
      XNAME = 'S'                                                             
      YNAME = 'RE(LAMBDA)'                                                    
C                                                                             
      L = 1               
      XMIN = 0.
      XMAX = 1.
      DO  40  I = 1,NZMA*NG
         IF ((WR(I) .GE. YRMIN)  .AND. (WR(I) .LE. YRMAX )
     >        .AND.(WI(I) .GE. YMIN)  .AND. (WI(I) .LE. YMAX ))THEN               
               Y(L) = WR(I)                                                   
               X(L) = SPL(I)                                                   
               L    = L + 1 
         ENDIF 
 40   CONTINUE                                                                
      L = L - 1                                                               
C      WRITE(*,1000)L,XMIN,XMAX,YMIN,YMAX
C      IF (L .LE. 1) RETURN                                                    
C                      
  
C      WRITE(*,*) ' PLOT SPECTRUM'
      CALL NFRAME(1,1,1,XMIN,XMAX,YRMIN,YRMAX,
     &           'CONTINUOUS SPECTRUM',19,XNAME,1,YNAME,10)
      CALL LPLOT(1,1,1201,X,Y,-L,1,'CONTINUOUS SPECTRUM REAL',19,   
     &           XNAME,1,YNAME,10) 
      RETURN                                                                  
 1000 FORMAT(1X,I5,' VALUES IN WINDOW ',1P,2E12.4,2X,                          
     >       2E12.4,' PLOTTED')                                             
      END 
************************************************************************
C***********************************************************************
*DECK SGCOPY
      SUBROUTINE SGCOPY(N,V1,INC1,V2,INC2)
C-----------------------------------------------------------------------
C     COPY(SINGLE)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      REAL     V1(*), V2(*)
C
*IF CRAY
      CALL SCOPY(N,V1,INC1,V2,INC2)
*ELSE
      CALL DCOPY(N,V1,INC1,V2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK SGSCAL
      SUBROUTINE SGSCAL(N,FAC,V,INC)
C-----------------------------------------------------------------------
C     FACTOR*VEKTOR(SINGLE)
C-----------------------------------------------------------------------
      INTEGER  N, INC
      REAL     V(*), FAC
C
*IF CRAY
      CALL SSCAL(N,FAC,V,INC)
*ELSE
      CALL DSCAL(N,FAC,V,INC)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXDOTU
      COMPLEX FUNCTION CXDOTU(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CV1*CV2(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      COMPLEX  CDOTU
      CXDOTU = CDOTU(N,CV1,INC1,CV2,INC2)
*ELSE
      COMPLEX  ZDOTU
      CXDOTU = ZDOTU(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXDOTC
      COMPLEX FUNCTION CXDOTC(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CONJG(CV1)*CV2(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      COMPLEX  CDOTC
      CXDOTC =  CDOTC(N,CV1,INC1,CV2,INC2)
*ELSE
      COMPLEX  ZDOTC
      CXDOTC =  ZDOTC(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXCOPY
      SUBROUTINE CXCOPY(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     COPY(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      CALL CCOPY(N,CV1,INC1,CV2,INC2)
*ELSE
      CALL ZCOPY(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXSCAL
      SUBROUTINE CXSCAL(N,CFAC,CV,INC)
C-----------------------------------------------------------------------
C     SCALE ARRAY(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC
      COMPLEX  CV(*), CFAC
C
*IF CRAY
      CALL CSCAL(N,CFAC,CV,INC)
*ELSE
      CALL ZSCAL(N,CFAC,CV,INC)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXAXPY
      SUBROUTINE CXAXPY(N,CFAC,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CFAC*CV1+CV2 (COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*), CFAC
C
*IF CRAY
      CALL CAXPY(N,CFAC,CV1,INC1,CV2,INC2)
*ELSE
      CALL ZAXPY(N,CFAC,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK ICXAMAX
      INTEGER FUNCTION ICXAMAX(N,CV,INC)
C-----------------------------------------------------------------------
C     INDEX OF ELEMENT WITH MAXIMUM ABSOLUTE VALUE
C-----------------------------------------------------------------------
      COMPLEX  CV(*)
      INTEGER  N, INC
*IF CRAY
      INTEGER  ICAMAX
      ICXAMAX = ICAMAX(N,CV,INC)
*ELSE
      INTEGER  IZAMAX
      ICXAMAX = IZAMAX(N,CV,INC)
*ENDIF
      RETURN
      END
      SUBROUTINE RFT2(DATA,NR,KR)                                       
C                                                                       
C     ******************************************************************
C     * REAL FOURIER TRANSFORM.                                        *
C     * INPUT:  NR REAL COEFFICIENTS                                   *
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
      DIMENSION DATA(*)                                               
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,-(KR+KR))                
      CALL RTRAN2(DATA,NR,KR,1)                               
      RETURN                                                   
      END                                                        
      SUBROUTINE FFT2 (DATAR,DATAI,N,INC)                               
C                                                                       
C     ******************************************************************
C     * FFT2 FORTRAN VERSION CLAIR NIELSON MAY 75.                     *
C     ******************************************************************
C                                                                 
      DIMENSION DATAR(*), DATAI(*)                                 
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
      THETA=REAL(KTRAN)*3.1415926535898                          
   30 IF(IP1.GE.IP3) GOTO 60                                        
      IP2=IP1+IP1                                                 
      SINTH=SIN(.5*THETA)                                        
      WSTPR=-2.*SINTH*SINTH                                       
      WSTPI=SIN(THETA)                                             
      WR=1.                                                      
      WI=0.                                                         
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
      THETA=.5*THETA                                                  
      GOTO 30                                                         
   60 RETURN                                                         
      END                                                               
      SUBROUTINE RTRAN2(DATA,NR,KR,KTRAN)                              
C                                                                       
C     ******************************************************************
C     * INTERFACE BETWEEN RFT2, RFI2, AND FFT2.                        *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS.                                          *
C     * LASL ROUTINE MAY 75, CALLED FROM RFT2 AND RFI2.                *
C     ******************************************************************
C                                                                
      DIMENSION DATA(*)                                         
      KS=2*KR                                               
      N=NR/2                                                   
      NMAX=N*KS+2                                                 
      KMAX=NMAX/2                                                 
      THETA=1.5707963267949/N                                         
      DC=2.*SIN(THETA)**2                                            
      DS=SIN(2.*THETA)                                              
      WS=0.                                                         
      IF(KTRAN.LE.0) THEN                                        
         WC=-1.0                                                 
         DS=-DS                                                     
      ELSE                                                            
         WC=1.0                                                        
         DATA(NMAX-1)=DATA(1)                                          
         DATA(NMAX-1+KR)=DATA(KR+1)                                  
      ENDIF                                                          
      DO 10 K=1,KMAX,KS                                            
         NK=NMAX-K                                                 
         SUMR=.5*(DATA(K)+DATA(NK))                                  
         DIFR=.5*(DATA(K)-DATA(NK))                                   
         SUMI=.5*(DATA(K+KR)+DATA(NK+KR))                               
         DIFI=.5*(DATA(K+KR)-DATA(NK+KR))                            
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

      SUBROUTINE RFI2(DATA,NR,KR)                                       
C                                                                       
C     ******************************************************************
C     * INVERSE OF RFT2.                                               *
C     * WHEN USING RFI2 IT IS NECESSARY TO HAVE VANISHING IMAGINARY    *
C     * PARTS OF THE FIRST AND LAST ELEMENT OF THE INPUT VECTOR:       *
C     *   DATA(1+KR)=DATA(1+(NR+1)*KR)=0.                              *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS.                                          *
C     * LASL ROUTINE MAY 75, CALLING RTRAN2 AND FFT2.                  *
C     ******************************************************************
C                                                                       
      DIMENSION DATA(*)                                                 
      CALL RTRAN2(DATA,NR,KR,-1)                                        
      MR=NR*KR                                                          
      FNI=2./NR                                                         
      DO 10 I=1,MR,KR                                                   
   10 DATA(I)=FNI*DATA(I)                                               
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,(KR+KR))                        
      RETURN                                                            
      END                                                               


*DECK SGTSL                                                             CAS02750
*** FROM NETLIB, TUE AUG 28 08:28:34 EDT 1990 ***                               
C                                                                               
      SUBROUTINE SGTSL(N,C,D,E,B,INFO)                                          
      INTEGER N,INFO                                                            
      REAL C(1),D(1),E(1),B(1)                                                  
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
C     FORTRAN ABS                                                               
C                                                                               
C     INTERNAL VARIABLES                                                        
C                                                                               
      INTEGER K,KB,KP1,NM1,NM2                                                  
      REAL T                                                                    
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
               IF (ABS(C(KP1)) .LT. ABS(C(K))) GO TO 10                         
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
