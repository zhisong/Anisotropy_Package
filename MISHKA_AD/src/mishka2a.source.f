*COMDECK COMVER
      CHARACTER  VERSION*(*),   DD*(*)
      PARAMETER (VERSION = '2', DD = '24 Oct 1996')
*COMDECK COMMAX
      PARAMETER (LANZ=7,MANZ=LANZ, MDIF=1, LMAX=128)
      PARAMETER (LVANZ=5, MVANZ=LANZ, LVMAX=LMAX)
      PARAMETER (NGMAX=501, MXNINT=NGMAX-1, NDEQ=4*MXNINT)
      PARAMETER (NPSIMAX=201, NCHIMAX=257)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX, NP4=4*NPSIMAX)
      PARAMETER (NVPSIMX=201, NVCHIMX=257)
      PARAMETER (NVPNVC=NVPSIMX*NVCHIMX,NVP4=4*NVPSIMX)
*COMDECK COMPAR
      PARAMETER (NGL=7, NBG=2*NGL*MANZ, NZMA=2*NBG, NB3=3*NBG)
c      PARAMETER (KILWOR=15000000, NDIM1=1500/NBG*NBG)
      PARAMETER (KILWOR=100000000, NDIM1=3000/NBG*NBG)
      PARAMETER (KPRGR=604999+11*NDEQ+NGMAX)
      PARAMETER (KPMAX=KILWOR-KPRGR, KPMEX=KPMAX-8*NBG*NBG)
*IF CRAY
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-4*NDIM1)
      PARAMETER (NREST1D=NREST1)
*ELSE
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-3*NDIM1-NDIM1/2)
      PARAMETER (NREST1D=KPMEX-4*NDIM1**2-4*NDIM1)
*ENDIF
*COMDECK COMPAR2
      PARAMETER (ML=2*NBG-1)
      PARAMETER (MU=ML)
      PARAMETER (LDA=3*(2*NBG-1)+1,LDB=LDA-ML)
      PARAMETER (NDIM2=(KPMEX/(2*LDA+9))/NBG*NBG)
*IF CRAY
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-9*NDIM2)
*ELSE
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-8*NDIM2-NDIM2/2)
*ENDIF
*COMDECK COMPAR3
      PARAMETER (NCV=(KPMEX-2*NBG*(2*NGMAX+NB3+6))/(4*NBG*NB3+12*NBG))
      PARAMETER (NDIM3=NBG*NB3*NCV*2)
      PARAMETER (NDIM3H=NDIM3/2)
*IF CRAY
      PARAMETER (N3=KPMEX-2*NBG*(2*NCV*(NB3+3)+2*NGMAX+NB3+6)-2*NBG*NCV)
*ELSE
      PARAMETER (N3=KPMEX-2*NBG*(2*NCV*(NB3+3)+2*NGMAX+NB3+6)-NBG*NCV)
*ENDIF
      PARAMETER (NREST3=N3)
*COMDECK COMPAR4
      PARAMETER (NCVIC=(KPMEX-NB3*(4+2*NBG))/(2*NBG*NB3+9*NBG))
      PARAMETER (NDIM4=NBG*NB3*NCVIC)
*IF CRAY
      PARAMETER (NREST4=KPMEX-9*NBG*NCVIC-2*NB3*(NBG*NCVIC+NBG+2))
*ELSE
      PARAMETER (NSP4=8*NBG*NCVIC+2*NB3*(NBG*NCVIC+NBG+2)+NBG*NCVIC/2)
      PARAMETER (NREST4=KPMEX-NSP4)
*ENDIF
*COMDECK COMP234
      PARAMETER (NCVD=(KPMEX-NBG*(12+6*NBG)-3*NGL*NGL)/(4*MANZ+2*NBG+2))
      PARAMETER (NPDIM=2*NCVD-1)
      PARAMETER (ND=2*NBG*NCVD+2*NB3*(NBG+2)+3*NGL**2+NPDIM*(2*MANZ+1))
      PARAMETER (NR234=KPMEX-ND)
*COMDECK COMPAR5
      PARAMETER (LDAA=3*(2*NBG-1)+1)
      PARAMETER (MXAB=51, MXLP=12)
      PARAMETER (NDIM5=(KPMEX/(3*LDAA+30))/NBG*NBG)
      PARAMETER (MLLZ=2*NBG-1)
      PARAMETER (MULZ=MLLZ)
      PARAMETER (LDAL=LDAA-MULZ)
      PARAMETER (LDBL=MULZ+1)
*IF CRAY
      PARAMETER (NREST5=KPMEX-NDIM5*(2*LDAA+16+LDAL+LDBL+6+7))
*ELSE
      PARAMETER (NREST5=KPMEX-NDIM5*(2*LDAA+16+LDAL+LDBL+6)-7*NDIM5/2)
*ENDIF
*COMDECK COMPARV
      PARAMETER (NGLV=1, NBGV=2*NGLV*MVANZ, NZMAV=2*NBGV, NB3V=3*NBGV)
      PARAMETER (NGVMAX=101, NPDIMV=2*NGVMAX)
      PARAMETER (MLV=2*NBGV-1)
      PARAMETER (MUV=MLV)
      PARAMETER (LDAV=3*(2*NBGV-1)+1)
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
*COMDECK COMPARP
      PARAMETER (IF1 =0)
      PARAMETER (IXC1=2)
      PARAMETER (IXC2=7)
      PARAMETER (IXC3=8)
      PARAMETER (IXQ1=1)
      PARAMETER (IXQ2=3)
      PARAMETER (IXQ3=4)
      PARAMETER (IXQ4=5)
      PARAMETER (IXQ5=6)
*COMDECK COMPCON
      REAL       PI, ZERO, ONE
      COMPLEX    ZEROC, ONEC, CHALF, CTWO
      PARAMETER (NMAX=201)
      PARAMETER (PI=3.141592653589793)
      PARAMETER (ZERO=0.E0, ONE=1.E0)
      PARAMETER (ZEROC=(0.E0,0.E0), ONEC=(1.E0,0.E0))
      PARAMETER (CHALF=(.5E0,0.E0), CTWO=(2.E0,0.E0))
C-----------------------------------------------------------------------
*COMDECK CORE
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     R         AP(KPMEX)
C
      COMPLEX  ZMA
      REAL     AP
C-----------------------------------------------------------------------
*COMDECK CORE1
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AMAT(NDIM1,NDIM1), BMAT(NDIM1,NDIM1),
C     R         WR(NDIM1), WI(NDIM1),
C     R         EVMAG(1,NDIM1),
C     R         HCOR(NREST1),
C     I         INDEX(NDIM1)
     C         WRI(NDIM1), VRI(1,1)
 
      COMPLEX  ZMA, AMAT, BMAT, WRI, VRI
C      REAL     WR, WI, EVMAG, HCOR
C      INTEGER  INDEX
C-----------------------------------------------------------------------
*COMDECK CORE2
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         X0(NDIM2), X1(NDIM2), Y0(NDIM2), Y1(NDIM2),
     C         AMAT(LDA,NDIM2), BMAT(LDB,NDIM2),
     R         HCOR(NREST2),
     I         IPVT(NDIM2)
 
      COMPLEX  ZMA, X0, X1, Y0, Y1, AMAT, BMAT
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE3
      COMMON / CORE /
     C         ZMA(NZMA,NZMA), EV(NBG,NGMAX,2),
     C         APR(NBG,NB3,NCV,2), X(NBG,NCV,2,3),
     C         BUFF(NBG,NB3), HVX(NBG,3), HVX2(NBG,3),
     R         HCOR(NREST3),
     I         IPVT(NBG,NCV,2)
 
      COMPLEX  ZMA, EV, APR, X, BUFF, HVX, HVX2
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE4
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         EV(NBG,NCVIC,2),
     C         X(NBG,NCVIC,2), APR(NBG,NB3,NCVIC),
     C         BUFF(NBG,NB3), HVX(NB3), HVX2(NB3),
     R         HCOR(NREST4),
     I         IPVT(NBG,NCVIC)
 
      COMPLEX  ZMA, EV, X, APR, BUFF, HVX, HVX2
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE5
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AA(LDAA,NDIM5),
     C         VS(NDIM5), WS(NDIM5),
     C         V1(NDIM5), V2(NDIM5), W1(NDIM5), W2(NDIM5),
     C         CONEV(NDIM5), CONEVB(NDIM5),
     R         AORIG(LDAL,NDIM5), BB(LDBL,NDIM5),
     R         ERREV(NDIM5), ERREVB(NDIM5),
     R         GR(NDIM5), GC(NDIM5), G(NDIM5), GG(NDIM5),
     R         HCOR(NREST5),
     I         IPVT(NDIM5), MP(NDIM5), MP2(NDIM5),
     I         MULEV(NDIM5), MULEVB(NDIM5), ISIGMA(NDIM5), ILOOP(NDIM5)
 
      COMPLEX  ZMA, AA, VS, WS, V1, V2, W1, W2, CONEV, CONEVB
      REAL     AORIG, BB, ERREV, ERREVB, GR, GC, G, GG, HCOR
      INTEGER  IPVT, MP, MP2, MULEV, MULEVB, ISIGMA, ILOOP
C-----------------------------------------------------------------------
*COMDECK CORE1D
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AMAT(NDIM1,NDIM1), BMAT(NDIM1,NDIM1),
     R         WR(NDIM1), WI(NDIM1),
     R         X(NDIM1), Y(NDIM1),
     R         HCOR(NREST1D)
C
      COMPLEX  ZMA, AMAT, BMAT
      REAL     WR, WI, X, Y, HCOR
C-----------------------------------------------------------------------
*COMDECK CORE234D
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         EV(NBG,NCVD),
     C         BUFF(NBG,NB3), HVX(NB3), HVX2(NB3),
     C         XTAX(NGL*NGL),
     R         YP(NPDIM,2,MANZ), XP(NPDIM), PROZ(NGL*NGL),
     R         HCOR(NR234)
C
      COMPLEX  ZMA, EV, BUFF, HVX, HVX2, XTAX
      REAL     YP, XP, PROZ, HCOR
C-----------------------------------------------------------------------
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
*COMDECK COMDIM5
      COMMON / COMDIM5 / MUL, MLL, NCONV
      INTEGER            MUL, MLL, NCONV
C-----------------------------------------------------------------------
*COMDECK COMIT
      COMMON / COMIT /
     C         EWSHIFT, EW, EWTEST,
     R         EPS,
     I         IT, ITER, IQUA
C
      COMPLEX  EWSHIFT, EW, EWTEST
      REAL     EPS
      INTEGER  IT, ITER, IQUA
C-----------------------------------------------------------------------
*COMDECK COMINT
      COMMON / COMINT /
     I         NREC,
     I         CPLG, CPLGA, CPLGX, CPLGXA, CPLGXS,
     I         LREAD, LWRIT
C
      INTEGER  NREC, CPLG, CPLGA, CPLGX, CPLGXA, CPLGXS, LREAD, LWRIT
C-----------------------------------------------------------------------
*COMDECK COMBX
      COMMON / COMBX   / NSHIFT
      INTEGER            NSHIFT
C-----------------------------------------------------------------------
*COMDECK COMDIAG
      COMMON / COMDIAG / NDIAGFK,IBVAC
      INTEGER            NDIAGFK,IBVAC
C-----------------------------------------------------------------------
*COMDECK COMPLOT
      COMMON / COMPLOT /
     R         XMINQR(5), XMAXQR(5), YMINQR(5), YMAXQR(5),
     I         NPLOT
 
      REAL     XMINQR, XMAXQR, YMINQR, YMAXQR
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
*COMDECK COMGEW
      COMMON / COMGEW  / GEWI(4)
      REAL               GEWI
C-----------------------------------------------------------------------
*COMDECK COMGEO
      COMMON / COMGEO  / ASPECT
      REAL               ASPECT
C-----------------------------------------------------------------------
*COMDECK COMEQUI
      COMMON / COMEQUI /
     R         GAMMA, ETA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN,
     R         DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW,
     R         GAMMAPAR, GAMMAPER, DSCALE, DFLATS,
     R         VEL3, VSURF, VSURF1, VSURF2, VSURF3, VSURF4,
     I         IDPOW, IEQ, IAS, ISLOW, IGAP
C
      REAL     GAMMA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN
      REAL     DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW, DSCALE, DFLATS
      INTEGER  IDPOW, IEQ, IAS, ISLOW, IGAP
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
     R         CURJ(NPSIMAX), CHI(NCHIMAX), FTS(NPSIMAX),DFTS(NPSIMAX),
     R         FLOW3I(NDEQ), DFLOW3I(NDEQ),
     R         OMEGAS(NPSIMAX),OMEGA2(NPSIMAX),
     I         NPSI, NCHI,P0(NDEQ),RBPHI(NDEQ),
     L         NLTORE
C
      REAL     CPSURF, CS, QS, DQS, CURJ, CHI,FTS,DFTS
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
     R       RGPGT(NP4,MANZ+11),ROOR2(NP4,MANZ+11),
     R       IGPGT(NP4,MANZ+11),IOOR2(NP4,MANZ+11),
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
      
      
C
      REAL     RGPGT,ROOR2,IGPGT,IOOR2
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
*COMDECK COMVGRD
      COMMON / COMVGRD /
     R         SVGRID(NGVMAX), CSV(NGVMAX), SVGI(4*(NGVMAX-1)),
     R         SIGV, NGV
      REAL     SVGRID, CSV, SVGI, SIGV
C-----------------------------------------------------------------------
*COMDECK COMVAC
      COMMON / COMVAC /
     R         RWALL,           FD,              RMIN,
     R         TC(NVCHIMX),     RW(NVCHIMX),     RP(NVCHIMX),
     R         FTC(2*NVCHIMX),  FRW(2*NVCHIMX),  FRP(2*NVCHIMX),
     R         DTC(NVCHIMX),    DRW(NVCHIMX),    DRP(NVCHIMX),
     R         VJ(NVPNVC),      VG11(NVPNVC),    VG12(NVPNVC),
     R         VG22(NVPNVC),    VG33(NVPNVC),    FW(NVCHIMX),
     I         NVPSI, NVCHI, NFW, IVAC
C
      REAL     RWALL, FD, RMIN, TC, RW, RP, FTC, FRW, FRP, DTC,
     >         DRW, DRP, VJ, VG11, VG12, VG22, VG33, FW
      INTEGER  NVPSI, NVCHI, NFW, IVAC
C-----------------------------------------------------------------------
*COMDECK COMJET                                                         
      COMMON / COMJET /                                                 
     R         RZV(2,73),THTV(73),
     R         RV1(73),RV2(73),RV3(73),RV4(73),                         
     R         RCNTR,ZCNTR,
     I         NV                                                   
C                                                                       
      REAL     RZV,THTV,RV1,RV2,RV3,RV4,RCNTR,ZCNTR
      INTEGER  NV 
C-----------------------------------------------------------------------
*COMDECK COMVFT
      COMMON / COMVFT /
     R         VR11(NVP4,LVANZ), VR12(NVP4,LVANZ), VR22(NVP4,LVANZ),
     R         VR33(NVP4,LVANZ), VI11(NVP4,LVANZ), VI12(NVP4,LVANZ),
     R         VI22(NVP4,LVANZ), VI33(NVP4,LVANZ),
     I         NVP1,NV2P1,NV3P1
C
      REAL     VR11, VR12, VR22, VR33, VI11, VI12, VI22, VI33
      INTEGER  NVP1,NV2P1,NV3P1
C-----------------------------------------------------------------------
*COMDECK COMBND
      COMMON / COMBND  /
     R         ASPI, RADIUS, VX(NVCHIMX), VY(NVCHIMX), VC(NVCHIMX)
      REAL     ASPI, RADIUS, VX, VY, VC
C-----------------------------------------------------------------------
*COMDECK COMVRSP
      COMMON / COMVRSP / B3B1(MANZ,MANZ)
      COMPLEX            B3B1
C-----------------------------------------------------------------------
*COMDECK COMVMAT
      COMMON / CORE /
     C         VMAT(LDAV,NBGV*NGVMAX), B(NBGV*NGVMAX), Q(NBGV*NGVMAX)
C
      COMPLEX  VMAT, B, Q
C-----------------------------------------------------------------------
*COMDECK COMBDIA
      COMMON / COMBDIA /
     R         RPSOR2(NVP4,LVANZ), IPSOR2(NVP4,LVANZ),
     R         RPTOR2(NVP4,LVANZ), IPTOR2(NVP4,LVANZ)
C
      REAL     RPSOR2, IPSOR2, RPTOR2, IPTOR2
C-----------------------------------------------------------------------
*COMDECK COMESH2
      COMMON / COMESH2 / RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT,
     >                   SBEGIN,SEND,IMESHAC
      REAL               RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT
      INTEGER            IMESHAC
C---------------------------------------------------------------------
*COMDECK COMSTVR
      COMMON / COMSTVR / SX, SY
*IF CRAY
      REAL               SX, SY
*ELSE
      REAL               SX(NGMAX*NBG), SY(NGMAX*NBG)
*ENDIF
C-----------------------------------------------------------------------
*COMDECK ISEED
      COMMON ISEED
C-----------------------------------------------------------------------
*COMDECK SHIFT
      COMMON / SHIFT   / SIGMA
      COMPLEX            SIGMA
C-----------------------------------------------------------------------
*COMDECK MACHT
      COMMON / MACHT   / MACHEP
      REAL               MACHEP
C-----------------------------------------------------------------------
*COMDECK LANTOL
      COMMON / LANTOL  / LTOL
      REAL               LTOL
C-----------------------------------------------------------------------
*COMDECK LANCZOS
      COMMON / LANCZOS /
     C         OWS(100),
     R         XLIML, XLIMR, YLIMB, YLIMT,
     R         XHOLEL, XHOLER, YHOLEB, YHOLET,
     I         NUS, ISHIFT, ISTART, ISTOP, KMAX, MXLOOP,
     L         IHOLE
C
      COMPLEX  OWS
      REAL     XLIML, XLIMR, YLIMB, YLIMT,
     >         XHOLEL, XHOLER, YHOLEB, YHOLET
      INTEGER  NUS, ISHIFT, ISTART, ISTOP, KMAX, MXLOOP
      LOGICAL  IHOLE
C-----------------------------------------------------------------------
*COMDECK CONLAN
      COMMON / CONLAN  /
     R         RELTOL,
     I         SVSEED, SAVTEV, MXINIT
C
      REAL     RELTOL
      INTEGER  SVSEED, SAVTEV, MXINIT
C-----------------------------------------------------------------------
*DECK MISHKA
      PROGRAM MISHKA
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
**                VALUES:                                              **
**                0 - TERMINATION OF EXECUTION                        **
**                1 - QR ALGORITHM                                    **
**                2 - VECTOR ITERATION, IN-CORE (STEUERWALD)           **
**                3 - VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)         **
**                4 - VECTOR ITERATION, IC VERSION OF OOC SOLVER      **
**                5 - LANCZOS ALGORITHM                               **
**               11 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 1  **
**               12 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 2  **
**               13 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 3  **
**               14 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 4  **
**               15 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 5  **
**                                                                    **
**    EQNAME   - NAME OF THE EQUILIBRIUM                              **
**    NLTORE   - TOROIDAL EQUILIBRIUM (.T. OR .F.)                    **
**    NG       - NUMBER OF GRID POINTS                                **
**    RFOUR(1) - LOWEST POLOIDAL MODE NUMBER                          **
**    NTOR     - TOROIDAL MODE NUMBER                                 **
**    ASPECT   - ASPECT RATIO (ONLY IF NLTORE=.F.)                    **
**    Q0ZYL    - Q ON AXIS                                            **
**    SIG1     - SIGMA OF FIRST MESHACC. POINT                        **
**    SIG2     - SIGMA OF SECOND MESHACC. POINT                       **
**    XR1      - POSITION OF FIRST MESHACC. POINT                     **
**    XR2      - POSITION OF SECOND MESHACC. POINT                    **
**    RWALL    - POSITION OF THE WALL                                 **
**    NVPSI    - NUMBER OF RADIAL POINTS IN VACUUM METRIC             **
**    NGV      - NUMBER OF RADIAL POINTS IN VAC. POTENTIAL SOL.       **
**    SIGV     - SIGMA OF MESHACC. IN VACUUM (XR = 0.)                **
**    DSURF    - PARAMETER FOR DENSITY PROFILE :                      **
** x  IDPOW    - PARAMETER FOR DENSITY PROFILE :                      **
**               RHO = ( 1 - (1 - DSURF) * S**2 )**IDPOW              **
**    NDIAGFK  - PRINT SWITCH FOR FOURIER COMPONENTS OF THE METRIC    **
**                                                                    **
**    VSHIFT(I)- I-TH ESTIMATE OF EIGENVALUE FOR VECTOR ITERATION     **
**               (I.LE.100); IF AUTOMATIC :                           **
**    NRS      - NUMBER OF AUTOMATIC REAL      SHIFTS                 **
**    NIS      - NUMBER OF AUTOMATIC IMAGINARY SHIFTS                 **
**    DRS      - WIDTH  OF AUTOMATIC REAL      SHIFTS                 **
**    DIS      - WIDTH  OF AUTOMATIC IMAGINARY SHIFTS                 **
**                                                                    **
**    EPS      - RELATIVE ACCURACY OF EIGENVALUE (VECTOR ITERATION)   **
**    NPLOT    - PLOT SWITCH FOR SOLVERS 1 - 4,                       **
**               NUMBER OF PLOTS FOR QR-SOLVER                        **
**    XMINQR(I)- LOWER LIMIT X-AXIS FOR I-TH QR-PLOT                  **
**    XMAXQR(I)- UPPER LIMIT X-AXIS FOR I-TH QR-PLOT                  **
**    YMINQR(I)- LOWER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    YMAXQR(I)- UPPER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    DSURF1   - JET-EQUIL.                                           **
**    ALPHIN   - JET-EQUIL.                                           **
**    IEQ      - =1                                                   **
**               =2 --> JET-EQUIL.                                    **
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
**    IQUA     - =0 DO NOT USE TWO SETS OF FINITE ELEMENTS            **
**               =1 USE TWO SETS OF FINITE ELEMETNS                   **
**                                                                    **
**  NAMELIST NEWLAN :  ON UNIT NIN (DEFINED IN COMPIO = 5)            **
**                                                                    **
**    ISTART  - IF 0 : START WITH SHIFT (EWSHIFT) AND COMPUTATION     **
**                     OF THE T-MATRIX                                **
**              ELSE : READ T-MATRIX FROM DISK                        **
**    ISTOP   - IF < 0 : WRITE T-MATRIX ON DISK + COMPUTATION         **
**              ELSE   : WRITE T-MATRIX ON DISK AND STOP              **
**    KMAX    - DIMENSION OF THE T-MATRIX                             **
**    MXLOOP  - MAXIMUM NUMBER OF SHIFTS                              **
**    ISHIFT  - IF 1 : MAKE SHIFTS FOR EWSHIFT AND OWS(1:NUS)         **
**              ELSE : MAKE SHIFTS FOR EWSHIFT AND REGION DETERINED   **
**                     BY XLIML,XLIMR,YLIMB,YLIMT                     **
**    FOR ISHIFT.EQ.1 :                                               **
**    NUS     - NUMBER OF GIVEN SHIFTS IN ADDITION TO EWSHIFT         **
**    OWS     - VECTOR FOR EIGENVALUE SHIFTS                          **
**    FOR ISHIFT.NE.1 :                                               **
**    XLIML   - MINIMUM REAL PART OF INVESTIGATED REGION              **
**    XLIMR   - MAXIMUM REAL PART OF INVESTIGATED REGION              **
**    YLIMB   - MINIMUM IMAGINARY PART OF INVESTIGATED REGION         **
**    YLIMT   - MAXIMUM IMAGINARY PART OF INVESTIGATED REGION         **
**    IHOLE   - .TRUE. : THERE IS A REGION XHOLEL,XHOLER,YHOLEB,      **
**                       YHOLET, PART OF THE REGION XLIML,XLIMR,      **
**                       YLIMB,YLIMT, WITHIN WHICH NO EIGENVALUES     **
**                       ARE TO BE SEARCHED FOR                       **
**    XHOLEL  - MINIMUM REAL PART                                     **
**    XHOLER  - MAXIMUM REAL PART                                     **
**    YHOLEB  - MINIMUM IMAGINARY PART                                **
**    YHOLET  - MAXIMUM IMAGINARY PART                                **
**                                                                    **
**                                                                    **
**  AND FOR SOLVER 5 : READ ON UNIT NIN2 (DEFINED IN COMPIO = 2)      **
**                                                                    **
**                                                                    **
**  OUTPUT :                                                          **
**  ------                                                            **
**                                                                    **
**    WRITTEN ON UNIT NOUT  (DEFINED IN COMPIO = 6)                   **
**            ON UNIT NOUTI (AT IPP, DEFINED IN COMPIO = 8)           **
**            ON UNIT NOUTP (TEXT FOR FIRST PLOT,                     **
**                           DEFINED IN COMPIO = 11)                  **
**            AND FOR SOLVER 3 :                                      **
**            ON UNIT ND3 (DEFINED IN COMPIO = 15)                    **
**            ON UNIT ND4 (DEFINED IN COMPIO = 16)                    **
**            ON UNIT ND5 (DEFINED IN COMPIO = 17)                    **
**            ON UNIT ND6 (DEFINED IN COMPIO = 18)                    **
**            AND FOR SOLVER 5 :                                      **
**            ON UNIT NOUT2 (DEFINED IN COMPIO = 1)                   **
**            ON UNIT NOUT3 (DEFINED IN COMPIO = 10)                  **
**                                                                    **
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
*CALL COMPAR2
*CALL COMPAR3
*CALL COMPAR4
*CALL COMPAR5
*CALL COMPARP
*CALL COMP234
*CALL COMPARV
*CALL COMPIO
*CALL COMMOD
*CALL COMDIM
*CALL COMLAB
*CALL COMDIM5
*CALL COMIT
*CALL COMINT
*CALL COMBX
*CALL COMDIAG
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
*CALL SHIFT
*CALL LANCZOS
*CALL CONLAN
*CALL CORE
*CALL COMVAC
*CALL COMVGRD
*CALL COMVFT
*CALL COMBND
C
      REAL DUMMY(3)
      CHARACTER*11  TXTPL(5)
      COMPLEX EWOUT
C
      DATA TXTPL /'QR         ','STEUERWALD ','OUT-OF-CORE',
     >            'IN-CORE-OOC','LANCZOS    '/
C
      NAMELIST / NEWRUN / MODE, EQNAME, NLTORE, NG, CWW,IFAST,
     >                    RFOUR, VFOUR, NTOR, ETA, ZMU, ZTE, ZTI,
     >                    ASPECT, Q0ZYL, SIG1, SIG2, XR1, XR2,
     >                    RWALL, SBEGIN, SEND,
     >                    NVPSI, NGV, SIGV, DSURF, IDPOW, NDIAGFK,
     >                    VSHIFT, NRS, NIS, DRS, DIS, EPS,ITER,
     >                    NPLOT, XMINQR, YMINQR, XMAXQR, YMAXQR,
     >                    DSURF1,ALPHIN,IEQ,IAS,IBVAC,GAMMA,
     >                    VSURF, VSURF1, VSURF2, VSURF3, VSURF4, VEL3,
     >                    DSURF2, DSURF3, IVAC, FW, NFW, RMIN, ZCNTR,
     >                    ISLOW, GAMMAPER, GAMMAPAR, IGAP, DSCALE,IQUA,
     >                    DFLATS
C
      NAMELIST / NEWLAN / ISTART, ISTOP, KMAX, MXLOOP,
     >                    ISHIFT, NUS, OWS,
     >                    XLIML, XLIMR, YLIMB, YLIMT,
     >                    IHOLE, XHOLEL, XHOLER, YHOLEB, YHOLET
CC
      IF(LANZ.GT.MANZ) STOP '**ERROR: LANZ > MANZ'
      CALL PRESET
C
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
      IBEGIN = 0
   10 READ(NIN,NEWRUN)
      IF(IBEGIN.NE.0) WRITE(NOUT,'(''1'')')
      IBEGIN = 1
      IF(MODE.EQ.0) GOTO 1000
      IF(NG.GT.NGMAX) STOP 'NG > NGMAX'
c$$$      MODSOL=MOD(MODE,10)
      MODSOL = MODE
      NRTEST=MODE/10
      IF(MODSOL.LT.1.OR.MODSOL.GT.5) THEN
         WRITE(NOUT,*) 'WRONG MODE'
         GOTO 1000
      ENDIF
      IF(MODSOL.EQ.5) THEN
         IF(KMAX.GT.MXAB) STOP 'MXAB'
         IF(MXLOOP.GT.MXLP) STOP 'MXLP'
      ENDIF
 
c$$$      IF(MODE.GT.10) CALL TESTS
      READ(NIN,NEWLAN)
C
      RFOUR1 = RFOUR(1)
      DO 20 JJ=1,MANZ
         RFOUR(JJ) = RFOUR1 + FLOAT((JJ-1))
   20 CONTINUE
C
      EWSHIFT = VSHIFT(1)
      SIGMA   = VSHIFT(1)
      DO 40 I=0,NRS-1
         SR = REAL(EWSHIFT) + DRS*I
         DO 30 J=0,NIS-1
            SI = AIMAG(EWSHIFT) + DIS*J
            VSHIFT(I*NIS+J+1) = CMPLX(SR,SI)
   30    CONTINUE
   40 CONTINUE
C
      REWIND NOUTP
      WRITE(NOUT,41) VERSION,EQNAME
      WRITE(NOUTP,41) VERSION,EQNAME
      IF(MODE.GT.10) THEN
         WRITE(NOUT,42) NRTEST,TXTPL(MODSOL)
         WRITE(NOUTP,42) NRTEST,TXTPL(MODSOL)
         IF(MODSOL.GE.2.AND.MODSOL.LE.4) THEN
            WRITE(NOUT,43) EWTEST
            WRITE(NOUTP,43) EWTEST
         ENDIF
      ELSE
         WRITE(NOUT,44) TXTPL(MODSOL)
         WRITE(NOUTP,44) TXTPL(MODSOL)
      ENDIF
      WRITE(NOUT,45)  LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
     >                DSURF, IDPOW, RWALL, EPS
      WRITE(NOUTP,45) LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
     >                DSURF, IDPOW, RWALL, EPS
      WRITE(NOUT,61)  ZTE,ZTI
      WRITE(NOUTP,61) ZTE,ZTI
      WRITE(NOUT,62)  ZMU,GAMMA
      WRITE(NOUTP,62) ZMU,GAMMA
      WRITE(NOUT,46)  (RFOUR(II),II=1,MANZ)
      WRITE(NOUTP,46) (RFOUR(II),II=1,MANZ)
      WRITE(NOUT,47)  SIG1, SIG2, XR1, XR2
      IF(SIG1.LE.99.) THEN
         WRITE(NOUTP,47) SIG1, SIG2, XR1, XR2
      ENDIF
      WRITE(NOUT,48)  NVPSI, NGV, SIGV
      IF(RWALL.GT.1.) THEN
         WRITE(NOUTP,48) NVPSI, NGV, SIGV
      ENDIF
      WRITE(NOUT,49)  (VSHIFT(II),II=1,NRS*NIS)
      WRITE(NOUTP,49) (VSHIFT(II),II=1,NRS*NIS)
      IF(MODSOL.EQ.1) WRITE(NOUT,50) NPLOT,
     >                (XMINQR(J),YMINQR(J),XMAXQR(J),YMAXQR(J),
     >                J=1,NPLOT)
C
      CALL WRTEXT(NOUTP)
C
      NDIM   = NG * NBG
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
      CALL BUSSAC
C------------------------------------------------------------------------
C SET POLOIDAL MODE NUMBERS AS FUNCTION OF RADIUS
C------------------------------------------------------------------------
      DO IG=1,NG
        QI = SPWERT(NPSI,SGRID(IG),Q1,Q2,Q3,Q4,CS,DUMMY)
		 IF (IFAST .EQ. 1) THEN
          MSTART(IG) = 1.+ FLOAT(INT(-ZNKWEL * QI - FLOAT((MANZ-1)/2)))
        ELSE
		   MSTART(IG) = RFOUR(1)
		 ENDIF
      ENDDO
      IF (IFAST .EQ. 1) VFOUR(1) = MSTART(NG) - FLOAT((MVANZ - MANZ)/2)
      WRITE(*,'(A,i5)') ' IFAST : ',IFAST
      WRITE(*,'(A,2f6.0)') ' MSTART(1),MSTART(NG):',MSTART(1),MSTART(NG)

C------------------------------------------------------------------------
C VACUUM RESPONSE 
C------------------------------------------------------------------------
      IF(RWALL.GT.1.) CALL VACUUM
C------------------------------------------------------------------------
C SOLVERS 
C------------------------------------------------------------------------
      NREC = (NG+NCV-1)/NCV
      REWIND(NOUTE)
C
      GOTO ( 100 , 200 , 300 , 400 , 500 ) MODSOL
C
  100 CONTINUE
C-----------------------------------------------------------------------
C QR ALGORITHM
C-----------------------------------------------------------------------
      WRITE(NOUT,101)
C
      IF(NDIM.GT.NDIM1) THEN
         WRITE(NOUT,102) NDIM,NDIM1
*IF CRAY
         MNDIM1 = ((-4.+SQRT(16.+16.*KPMEX))/8.)/NBG*NBG
*ELSE
         MNDIM1 = ((-3.5+SQRT(3.5**2+16.*KPMEX))/8.)/NBG*NBG
*ENDIF
         NGT = MNDIM1/NBG
         IF(MNDIM1.NE.NDIM1) WRITE(NOUT,103) KILWOR,NGMAX,MANZ,MNDIM1,
     >                                    NGT
         WRITE(NOUT,104)
         CALL FINPLT
         STOP
      ENDIF
C
      CALL MAT1
      CALL SOLV1
      CALL DIAG1
      GOTO 10
  200 CONTINUE
C-------------------------------------------------------------------------
C INVERSE VECTOR ITERATION, IN-CORE (STEUERWALD)
C-------------------------------------------------------------------------
      WRITE(NOUT,201) EWSHIFT
C
      IF(NDIM.GT.NDIM2) THEN
         WRITE(NOUT,202) NDIM,NDIM2
         STOP
      ENDIF
C
      CALL MAT2
      CALL SOLV2
      CALL DIAG234
      GOTO 10
  300 CONTINUE
C-------------------------------------------------------------------------
C     INVERSE VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)
C-------------------------------------------------------------------------
      IF(NCV.LT.1) THEN
         WRITE(NOUT,301)
         STOP
      ENDIF
C
      DO 310  JJ = 1, NRS*NIS
C
         NSHIFT = JJ
C
         CALL MAT3
C
         EWSHIFT = VSHIFT(JJ)
         WRITE(NOUT,302) EWSHIFT
         CALL SOLV3
         VSHIFT(JJ) = EW
C
         IF(NPLOT.NE.0) CALL DIAG234
C
         REWIND ND3
         REWIND ND4
         REWIND ND5
         REWIND ND6
C
  310 CONTINUE
      GOTO 10
  400 CONTINUE
C-----------------------------------------------------------------------
C INVERSE VECTOR ITERATION, IN-CORE VERSION OF OUT-OF-CORE SOLVER
C-----------------------------------------------------------------------
      WRITE(NOUT,401) EWSHIFT
C
      IF(NCVIC.LT.NG) THEN
         WRITE(NOUT,402)
         STOP
      ENDIF
C
      CALL MAT4
      CALL SOLV4
      CALL DIAG234
      GOTO 10
  500 CONTINUE
C------------------------------------------------------------------------
C LANCZOS ALGORITHM
C------------------------------------------------------------------------
      WRITE(NOUT,501)
C
      IF(NDIM.GT.NDIM5) THEN
         WRITE(NOUT,502) NDIM,NDIM5
         STOP
      ENDIF
C
      CALL MAT5
      CALL SOLV5
      IF(ISHIFT.NE.1) CALL DIAG5
      GOTO 10
C------------------------------------------------------------------------
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
*CALL COMPARV
*CALL COMDIAG
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMGEW
*CALL COMMEW
*CALL COMIT
*CALL COMINT
*CALL LANCZOS
*CALL CONLAN
*CALL COMIOD
*CALL COMVGRD
*CALL COMVAC
*CALL COMESH2
*CALL ISEED
C-----------------------------------------------------------------------
C VARIABLES FOR VSHIFT
C-----------------------------------------------------------------------
      NRS       =  1
      NIS       =  1
      DRS       =  1.0
      DIS       =  1.0
      VSHIFT(1) = (0.,33.)
      DO 10 I=2,100
   10 VSHIFT(I) = (0.,0.)
C----------------------------------------------------------------------
C VARIABLES FOR PLOTTING 
C----------------------------------------------------------------------
      NPLOT     =    3
      XMINQR(1) =   -1.
      XMINQR(2) =  -10.
      XMINQR(3) =   -1.
      XMINQR(4) =  -10.
      XMINQR(5) =  -10.
      XMAXQR(1) =    1.
      XMAXQR(2) =   10.
      XMAXQR(3) =    1.
      XMAXQR(4) =   10.
      XMAXQR(5) =   10.
      YMINQR(1) =   -0.1
      YMINQR(2) =    0.
      YMINQR(3) =  1.4
      YMINQR(4) =    0.
      YMINQR(5) =    0.
      YMAXQR(1) =    2.
      YMAXQR(2) =  2000.
      YMAXQR(3) =  1.6
      YMAXQR(4) =   10.
      YMAXQR(5) =   10.
C-----------------------------------------------------------------------
C PHYSICAL VARIABLES 
C-----------------------------------------------------------------------
      RFOUR(1)  =  0.
      DO 20 I= 2,MANZ
   20 RFOUR(I)  =  0.
      NLTORE    = .TRUE.
      NTOR      = -1
      GAMMA     =  5. / 3.
      GAMMAPAR  =  3.
      GAMMAPER  =  2.
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
      RWALL     = -1.
      FW(1)     = 10.
      NFW       = NVCHIMX
      DO 25 I = 1, NFW
         FW(I)  = 0.
 25   CONTINUE
      RMIN      = 1.
      NVPSI     = 42
      NGV       = 51
      SIGV      = 9999.
      IVAC      = 1
      ISLOW     = 1
      IGAP      = 0
      IQUA      = 1
      DSCALE    = 0
      DFLATS    = 1.

      SBEGIN    = 0.0
      SEND      = 1.0
C--------------------------------------------------------------------------
C VARIABLES FOR DIAGNOSTICS 
C--------------------------------------------------------------------------
      NDIAGFK   = 0
      IBVAC     = 0
C--------------------------------------------------------------------------
C NUMERICAL VARIABLES 
C--------------------------------------------------------------------------
      NG        =  11
      ITER      =  50
      EPS       = 1.E-6
C
      GEWI(1)   = .17392742256872693
      GEWI(2)   = .17392742256872693
      GEWI(3)   = .32607257743127307
      GEWI(4)   = .32607257743127307
C-------------------------------------------------------------------------
C VARIABLES FOR LANCZOS 
C-------------------------------------------------------------------------
      SVSEED    = 7892713
C     ISEED     = 123456789
      MXINIT    = 5
      SAVTEV    = 1
      RELTOL    = 1.E-8
C
      ISHIFT    = 0
      ISTART    = 0
      ISTOP     = 1
      IHOLE     = .FALSE.
      KMAX      = 51
      MXLOOP    = 10
      NUS       =  0
      XLIML     = 0.
      XLIMR     = 1.30
      YLIMB     = 0.
      YLIMT     = 10.
      XHOLEL    =  0.0
      XHOLER    =  0.0
      YHOLEB    =  0.0
      YHOLET    =  0.0
      DO 30 I=1,100
   30 OWS(I)    = (0.,0.)
C
      RETURN
      END
************************************************************************
*DECK TESTS
      SUBROUTINE TESTS
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMMOD
*CALL COMDIAG
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMGEW
*CALL COMMEW
*CALL COMIT
*CALL COMINT
*CALL LANCZOS
*CALL CONLAN
*CALL COMIOD
*CALL ISEED
C
         WRITE(NOUT,*) 'THIS TEST CASE DOES NOT EXIST'
         STOP
C
      RETURN
      END
************************************************************************
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
**                   EQVAC                                            **
**                   VFKEQ                                            **
**                     FFTRAN                                         **
**                     SPLFK                                          **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPARV
*CALL COMPIO
*CALL COMGRID
*CALL COMESH2
*CALL COMIOD
*CALL COMVGRD
*CALL COMVAC
C-------------------------------------------- READ EQUILIBRIUM FROM DISK 
      CALL IODSK
C------------------------------------------------- GRID (S - COORDINATES) 
      XWALL=1.0
      IF ((RWALL.GT.0.).AND.(RWALL.LT.1.)) XWALL = RWALL
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
      RS0 = SVGRID(1)
      RSA = SVGRID(NGV)
      BGF = 0.3
      XR1 = 1.E-12
      XR2 = 999999.
      SIG1 = SIGV
      SIG2 = 999999.
      FACT = 1.
      IMESHAC = 1
      IF(SIGV.GT.99.) IMESHAC = 0
C
      IF(IMESHAC.NE.0) CALL MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >                             SVGRID,NGV,NGV-1)
C
C-------------------------------------------- DEFINE PLASMA COEFFICIENTS
      CALL EQUILV
      CALL FKEQ
C      STOP 'TEST'
C------------------------------------------------ DEFINE GEOMETRY VACUUM 
      IF(RWALL.GT.1.) THEN
         CALL EQVAC
         CALL VFKEQ
      ENDIF
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
*CALL COMVAC
*CALL COMWEL
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

c$$$      DO I = 1, NPSI
c$$$         AVG = 0
c$$$         DO J = 1, NCHI
c$$$            NPOS = (I-1) * NCHI + J
c$$$            AVG = AVG + T(NPOS)
c$$$         ENDDO
c$$$         AVG = AVG / FLOAT(NCHI)
c$$$         DO J = 1, NCHI
c$$$            NPOS = (I-1) * NCHI + J
c$$$            T(NPOS) = AVG
c$$$         ENDDO
c$$$      ENDDO

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
C
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
c$$$      DD = 1.2
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

      WRITE(NOUT,52) CPSURF
      WRITE(NOUT,53) (QS(JJ),JJ=1,JS0+1)
c$$$      WRITE(*,*) 'RHO0, PPAR0, PPER0, RBPHI0'
c$$$      WRITE(*,*) RHOI0, PPARI0, PPERI0, RBPHII0,ASPI
c$$$      WRITE(NOUT,54) (P0(JJ),JJ=1,NPSI)
c$$$      WRITE(NOUT,55) (RBPHI(JJ),JJ=1,NPSI)
c$$$      WRITE(NOUT,56) DJ0,DJE,(CURJ(JJ),JJ=1,JS0+1)
C
C------------------------------------------------------------- SPLINES
      DQ1 = (QS(NPSI)-QS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      DQ0 = 0
      DRBPHI0 = 0
      DRBPHIE = (FTS(NPSI)-FTS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,1,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,FTS,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.002
        PLOTQ(I) = SPWERT(NPSI,PLOTS(I),Q1,Q2,Q3,Q4,CS,DUMMY)
      ENDDO
      CALL LPLOT6(3,3,PLOTS,PLOTQ,100,'splined q-profile')
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.01
        PLOTQ(I) = SPWERT(NPSI,PLOTS(I),Q1,Q2,Q3,Q4,CS,DUMMY)
      ENDDO
      CALL LPLOT6(2,3,PLOTS,PLOTQ,100,'splined q-profile')
c$$$      CALL SPLINE(NPSI,CS,CURJ,DJ0,DJE,1,C1,C2,C3,C4)
c$$$      CALL SPLINE(NPSI,CS,P0,DP0,DPE,1,P1,P2,P3,P4)
c$$$      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
C
      CALL SGCOPY(NPSI,Q2,1,DQS,1)
      CALL SGCOPY(NPSI,RBP2,1,DFTS,1)
      RETURN
c---------------------------------- omega* calculation
c$$$c------------ flux surface everages of some quantities
c$$$      AREA = 0.
c$$$      DO 200 I=2,NPSI
c$$$        AG22 = 0.
c$$$        AB0  = 0.
c$$$        ADL  = 0.
c$$$        RMA  = -1.
c$$$        RMI  = +2.
c$$$        SPS2 = 2. * CPSURF * CS(I)
c$$$        SPSM = 2. * CPSURF * CS(I-1)
c$$$        DO 210 J=1,NCHI
c$$$          INDEX = (I-1)*NCHI + J
c$$$          INDM  = (I-2)*NCHI + J
c$$$          FACT=1.
c$$$          IF ((IAS.EQ.0).AND.((J.EQ.1).OR.(J.EQ.NCHI))) FACT=0.5
c$$$c----------------------------------- carefull : this is a 2D jacobian
c$$$          ZJACM= SPSM*QS(I-1)*SQRT(GEM33(INDM))/RBPHI(I-1) 
c$$$          ZJAC = SPS2*QS(I)*SQRT(GEM33(INDEX))/RBPHI(I)
c$$$          G22L = QS(I)**2 * GEM11(INDEX)*GEM33(INDEX)/RBPHI(I)**2
c$$$          AG22 = AG22 + FACT * SQRT(G22L*GEM11(INDEX)) 
c$$$          ADL  = ADL  + FACT * SQRT(G22L)
c$$$          AREA = AREA + FACT * (ZJAC+ZJACM)*(CS(I)-CS(I-1))/2.
c$$$          AB0  = AB0 + SQRT((RBPHI(I)**2+GEM11(INDEX))/GEM33(INDEX))
c$$$     >         * SQRT(G22L) * FACT
c$$$          RM = SQRT(GEM33(INDEX))
c$$$          IF (RM.GT.RMA) RMA = RM
c$$$          IF (RM.LT.RMI) RMI = RM
c$$$  210   CONTINUE
c$$$        G22AV(I) = AG22 / ADL 
c$$$        B0AV(I) = AB0 / ADL
c$$$        RAV = (RMA-RMI)/(RMA+RMI) 
c$$$        RAV2= 2.*AREA / ADL
c$$$        SPS2 = 2. * CPSURF * CS(I)
c$$$        OMEGAS(I) = - CWW/(SPS2 * RAV * B0AV(I)) *
c$$$     >            G22AV(I) * P2(I) * ABS(ZNKWEL) * QS(I)
c$$$        OMEGA2(I) = - CWW/(SPS2 * RAV2 * B0AV(I)) *
c$$$     >            G22AV(I) * P2(I) * ABS(ZNKWEL) * QS(I)
c$$$        OMOLD = - CWW/(ASPI**2 * CS(I) *RBPHI(I)) * P2(I)
c$$$     >        * ZNKWEL * QS(I) 
c$$$c        WRITE(20,201) I,CS(I),QS(I),OMEGAS(I),OMOLD,B0AV(I),
c$$$c     >                G22AV(I)*ASPI/(2*CPSURF*CS(I)),
c$$$c     >                RAV/(ASPI*CS(I)),
c$$$c     >                RAV2/(ASPI*CS(I)),
c$$$c     >                AREA*3.14159/(NCHI-1)/ASPI**2,
c$$$c     >                ADL*3.14159/(NCHI-1)/ASPI
c$$$  200 CONTINUE
c$$$  201 FORMAT(I3,2F7.3,10F9.5)
c$$$      CALL SPLINE(NPSI,CS,OMEGA2,0.,0.,2,OM1,OM2,OM3,OM4)
c$$$      RETURN
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
*CALL COMPARV
*CALL COMGRID
*CALL COMVAC
*CALL COMVGRD
*CALL COMPIO
C
      WRITE(NOUT,'(A,2f10.4)')' SBEGIN,SEND : ',SBEGIN,SEND
      WRITE(NOUT,'(A,f10.4)') ' XWALL : ',XWALL

      DELS  = (SEND-SBEGIN) / FLOAT(NG - 1)
      DO N = 1, NG
         SGRID(N)  = SBEGIN+(N - 1) * DELS
      ENDDO
C
      DELSV = XWALL/(NGV - 1)
      DO N = 1, NGV
        SVGRID(N) = FLOAT(N - 1) * DELSV
      ENDDO
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
      REAL ZS(4), ABLTG(3), ZA, ZB, ZC, ZDIF,
     >     OMEGA(4*NGMAX), RLARM(4*NGMAX)
C
      DO 20  NI = 1 , NGINT
         SL = SGRID(NI)
         SU = SGRID(NI+1)
C-------------------------------------------------- GAUSSIAN POINTS
         ZDIF  = SU - SL
         ZA    = .5 * ( SU + SL )
         ZB    = .43056815579702629 * ZDIF
         ZC    = .16999052179242813 * ZDIF
         ZS(1) = ZA + ZB
         ZS(2) = ZA - ZB
         ZS(3) = ZA + ZC
         ZS(4) = ZA - ZC
C
         DO 10 I=1,4
            J       = (NI-1)*4+I
            SGI(J)  = ZS(I)
            Q(J)    = SPWERT(NPSI,ZS(I),Q1,Q2,Q3,Q4,CS,ABLTG)
            DQ(J)   = ABLTG(1)
            ZT0(J)  = SPWERT(NPSI,SGI(J),RBP1,RBP2,RBP3,RBP4,CS,ABLTG)
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
   
      QWALL   = SPWERT(NPSI,SGRID(NI),Q1,Q2,Q3,Q4,CS,ABLTG)
      WRITE(NOUT,31) QWALL
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
   21 FORMAT(///7X,'S',11X,'Q',10X,'DQ',11X,'T',10X,'DT',10X,'ETA',
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
      REAL     G_11(NPNC)
      REAL     B02(NPNC), DTB02(NPNC), DSB02(NPNC)
      REAL     DET(NPNC), DTDET(NPNC), DSDET(NPNC)
      REAL     DPPERDT(NPNC),DPPARDT(NPNC), DPPERDS(NPNC), DPPARDS(NPNC)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
      REAL     K11, K21, K22, K23, K31, K32, K33,JAC
      REAL     PPERAVG(NPNC), PPARAVG(NPNC), GEM33AVG(NPNC)
      REAL     DSPPERAVG(NPNC), DSPPARAVG(NPNC)
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
         IF (PSIFLAT .LE. 1) THEN
            DO I = 1+NCHI, NPSI*NCHI
               RHO(I) = RHO(I-NCHI)
            ENDDO
         ELSE
            DO I = INT(PSIFLAT-1)*NCHI+1, NPSI*NCHI
               RHO(I) = RHO(I-NCHI)
            ENDDO
         ENDIF
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
      DO I = 1, NPSI
         PPER2 = 0
         PPAR1 = 0
         RBPHI1= 0
         R21   = 0
         DO J = 1, NCHI
            NO = (I-1)*NCHI + J
            PPER1 = PPER1 + PPER(NO)
            PPAR1 = PPAR1 + PPAR(NO)
            RBPHI1= RBPHI1 + T(NO)
            R21   = R21 + GEM33(NO)
         ENDDO
         PPER1 = PPER1 / FLOAT(NCHI)
         PPAR1 = PPAR1 / FLOAT(NCHI)
         RBPHI1 = RBPHI1 / FLOAT(NCHI)
         R21   = R21   / FLOAT(NCHI)
         DO J = 1, NCHI
            NO = (I-1)* NCHI + J
            PPERAVG(NO) = (PPER1+PPAR1)/2.
            PPARAVG(NO) = (PPAR1+PPER1)/2.
            GEM33AVG(NO) = R21
c$$$            T(NO) = RBPHI1
         ENDDO
      ENDDO
      DO I=1 , NPSI
         SPS2 = 2 * CS(I) * CPSURF
         DO J = 1, NCHI 
            NO = (I-1)*NCHI + J
            G_11(NO) = SPS2**2 / GEM11(NO) *
     >           (1 + QS(I)**2*GEM33(NO)/T(NO)**2 * GEM12(NO)**2)
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
        CALL DERIV(PPAR ((I-1)*NCHI+1),DPPARDT((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(PPER ((I-1)*NCHI+1),DPPERDT((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(B02  ((I-1)*NCHI+1),DTB02  ((I-1)*NCHI+1),NCHI,IAS)
        CALL DERIV(T    ((I-1)*NCHI+1),DFDT   ((I-1)*NCHI+1),NCHI,IAS)
      ENDDO
C------------------------- CALCULATE D/DS ----------------------------
      CALL DERIVS(CS, T   , DFDS  , NPSI, NCHI)
      CALL DERIVS(CS, GEM33, DSGEM33, NPSI, NCHI)
      CALL DERIVS(CS, GEM11, DSGEM11, NPSI, NCHI)
      CALL DERIVS(CS, PPER, DPPERDS, NPSI, NCHI)
      CALL DERIVS(CS, PPAR, DPPARDS, NPSI, NCHI)
      CALL DERIVS(CS, B02, DSB02, NPSI, NCHI)
      CALL DERIVS(CS, PPERAVG, DSPPERAVG, NPSI, NCHI)
      CALL DERIVS(CS, PPARAVG, DSPPARAVG, NPSI, NCHI)

      DO I=1,NGES
         DSDET(I) = ((DPPARDS(I) - DPPERDS(I)) / B02(I) 
     >          - (PPAR(I) - PPER(I)) * DSB02(I) / B02(I)**2)
         DTDET(I) = ((DPPARDT(I) - DPPERDT(I)) / B02(I) 
     >          - (PPAR(I) - PPER(I)) * DTB02(I) / B02(I)**2)
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
            BETA3 = -( DTDET(NO)/(1-DET(NO)) 
     >           + 2.*DTB0/B0)
            HH1 = - T(NO)**2 / GEM33(NO)
            HH2 = GEM11(NO) / GEM33(NO)
            HH4 = GEM12(NO) / GEM33(NO)

C-----A(3,5) = iM*A35_1/Q+ iN*A35_2 + iN*A35_3 + (M_+NQ)M * A35_4/Q + (M_NQ)N * A35_5
            HV(NO) = DET(NO)*BETA3  * HH1 / (1-DET(NO))
            HV1(NO) =DET(NO)*BETA3  * HH2 / (1-DET(NO))
            HV2(NO) = - B02(NO) * DTDET(NO) / (1-DET(NO))
            HV3(NO) = - DET(NO) * HH1 / (1-DET(NO))
            HV4(NO) = - DET(NO) * HH2 / (1-DET(NO))
C-----A(3,6) = (-iA35_2 - iA35_3 - (M_+NQ) * A35_5)*DQ/Q**2 + (M+NQ) * A36_1/Q + I(M_+NQ)(M+NQ) * A36_2 * f/Q
            HV5(NO) =-HH4*BETA3*SPS2*DET(NO)/(1-DET(NO))
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
C
            EE1 = -(GEM11(NO)/T(NO)/B02(NO))
            EE2 = -(DSGEM33(NO)/T(NO)-DFDS(NO)*GEM33(NO)/T(NO)**2)
     >            -GEM33(NO)/T(NO)*DSB02(NO)/B02(NO)/2.
            EE3 = SPS2/B02(NO)*(DTGEM12(NO)/T(NO)
     >             - GEM12(NO)*DFDT(NO)/T(NO)**2
     >            - GEM12(NO)/T(NO)/B02(NO)*DTB02(NO)/2.)

C     A(4,1) = HV  + if(_m+nq)HV1 + HV2*DQ/DS + HV3*Q
            HV (NO)= (GAMMAPER * DPPERDT(NO) - DPPERDT(NO)) * ZSLOW
     >              * SPS2 * GEM12(NO) / T(NO) / B02(NO)
     >              -      DPPERDS(NO)*GEM33(NO)/T(NO)
     >              -GAMMAPER * PPER(NO) *GEM33(NO)/T(NO)
     >                    *(DSGEM33(NO)/GEM33(NO)-DFDS(NO)/T(NO))*ZSLOW
            IF ((PPAR(NO).NE.0.D0).AND.(ZSLOW.EQ.0.D0)) THEN
               HV(NO) = HV(NO) + GEM33(NO)/T(NO)*(1.-ZSLOW)/2.
     >            *2.*(PPER(NO)-PPER(NO)**2/PPAR(NO))*DSB02(NO)/B02(NO)
            ENDIF
            HV1(NO)=-GAMMAPER *PPER(NO)*GEM12(NO) /T(NO) /B02(NO)*ZSLOW
            HV2(NO)=(1.-GAMMAPER)*PPER(NO)*EE1      *ZSLOW     
            HV3(NO)=(1.-GAMMAPER)*PPER(NO)*(EE2+EE3)*ZSLOW

            HV (NO) = - GEM33(NO)/T(NO) * (DSPPERAVG(NO)
     >        + (GEM33AVG(NO) - GEM33(NO))/GEM33AVG(NO)/2.
     >           * 2.*(DSPPERAVG(NO) 
     >           - 2.*DSPPERAVG(NO)*PPERAVG(NO)/PPARAVG(NO)
     >           + DSPPARAVG(NO)*PPERAVG(NO)**2/PPARAVG(NO)**2)*0)
C
C     A(4,2) =iHV4 + _m HV5 + nq HV6
            HV4(NO)= -(GAMMAPER * DPPERDT(NO) -DPPERDT(NO))
     >           * T(NO) / B02(NO) *ZSLOW
     >           -(- DFDT(NO) / B02(NO) + T(NO)/B02(NO)**2*DTB02(NO)/2.)
     >           *(1-GAMMAPER)*PPER(NO) * ZSLOW
            HV5(NO)= -GAMMAPER* PPER(NO) *T(NO)/B02(NO) * ZSLOW
            HV6(NO)= -HV5(NO) * GEM11(NO) / T(NO)**2
C
C     A(4,3) =(iHV8 + (_m+nq) HV9 + (m+nq) HVA)*f
            HV8(NO)= -(GAMMAPER* DPPERDT(NO) - DPPERDT(NO))*ZSLOW
     >              +(1.-GAMMAPER)*PPER(NO)*DTB02(NO)/B02(NO)/2.
     >              * ZSLOW
            HV9(NO)= -GAMMAPER * PPER(NO) *ZSLOW
            HVA(NO)= -(1-GAMMAPER) * PPER(NO) * ZSLOW
         ENDDO
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
      GAMMAPAR2 = 1.
      DO I=1, NPSI
         SPS2 = 2. * CS(I) * CPSURF
         DO J = 1,NCHI
            NO = (I-1)*NCHI + J
C
            EE1 = -(GEM11(NO)/T(NO)/B02(NO))
            EE2 = -(DSGEM33(NO)/T(NO)-DFDS(NO)*GEM33(NO)/T(NO)**2)
     >            -GEM33(NO)/T(NO)*DSB02(NO)/B02(NO)/2.
            EE3 = SPS2/B02(NO)*(DTGEM12(NO)/T(NO)
     >             - GEM12(NO)*DFDT(NO)/T(NO)**2
     >            - GEM12(NO)/T(NO)/B02(NO)*DTB02(NO)/2.)

C     A(7,1) = HV  + if(_m+nq)HV1 + HV2*DQ/DS + HV3*Q
            HV (NO)= (GAMMAPAR2* DPPARDT(NO) - DPPARDT(NO)) * ZSLOW
     >              * SPS2 * GEM12(NO) / T(NO) / B02(NO)
     >              -      DPPARDS(NO)*GEM33(NO)/T(NO)
     >              + (PPAR(NO)-PPER(NO))*DSB02(NO)/B02(NO)
     >               * GEM33(NO)/T(NO)*(1.-ZSLOW)/2.
     >              -GAMMAPAR2* PPAR(NO) *GEM33(NO)/T(NO)
     >                    *(DSGEM33(NO)/GEM33(NO)-DFDS(NO)/T(NO))*ZSLOW
            HV1(NO)=-GAMMAPAR2*PPAR(NO) * GEM12(NO) /T(NO)/B02(NO)*ZSLOW
            HV2(NO)=-(1.-GAMMAPAR)*PPAR(NO)*EE1      *ZSLOW     
            HV3(NO)=-(1.-GAMMAPAR)*PPAR(NO)*(EE2+EE3)*ZSLOW
            HV(NO)= - GEM33(NO)/T(NO) * (DSPPARAVG(NO)
     >        + (GEM33AVG(NO) - GEM33(NO))/GEM33AVG(NO)/2.
     >           *(DSPPARAVG(NO) - DSPPERAVG(NO))*0)
c$$$            WRITE(*,*) HVSSS, HV(NO)+ GEM33(NO)/T(NO) * DSPPARAVG(NO),
c$$$     >HV(NO)+ GEM33(NO)/T(NO) * DSPPARAVG(NO) - HVSSS, J
C
C     A(7,2) =iHV4 + _m HV5 + nq HV6
            HV4(NO)= -(GAMMAPAR1* DPPARDT(NO) -DPPARDT(NO))
     >           * T(NO) / B02(NO) *ZSLOW
     >           -(- DFDT(NO) / B02(NO) + T(NO)/B02(NO)**2*DTB02(NO)/2.)
     >           *(GAMMAPAR-1.)*PPAR(NO) * ZSLOW
            HV5(NO)= -GAMMAPAR1*PPAR(NO) *T(NO)/B02(NO) * ZSLOW
            HV6(NO)= -HV5(NO) * GEM11(NO) / T(NO)**2
C
C     A(7,3) =(iHV8 + (_m+nq) HV9 + (m+nq) HVA)*f
            HV8(NO)= -(GAMMAPAR1*DPPARDT(NO) - DPPARDT(NO))*ZSLOW
     >              -(1.-GAMMAPAR)*PPAR(NO)*DTB02(NO)/B02(NO)/2.
     >              * ZSLOW
            HV9(NO)= -GAMMAPAR1* PPAR(NO) *ZSLOW
            HVA(NO)= -(GAMMAPAR-1.) * PPAR(NO) * ZSLOW
         ENDDO
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
C
C-----------------------------------------------------------------------
C                        1/R**2  -->  ROOR2,IOOR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = 1./GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,ROOR2,IOOR2)
C
C-----------------------------------------------------------------------
C        GRAD.PSI * GRAD.THETA  -->  RGPGT,IGPGT
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM12,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGT,IGPGT)
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
************************************************************************
*DECK BUSSAC
      SUBROUTINE BUSSAC
C***********************************************************************
C calculates the value of the bussac poloidal beta for the q=1 and
C q=2 surface. Result should be identical to HELENA output.
C***********************************************************************
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMGEM
*CALL COMIOD
*CALL COMEQUI
*CALL COMDIAG
*CALL COMFFT
*CALL COMBND
*CALL COMEQV2D
      INTEGER  INDEX(2*(NCHIMAX-1)),NGES
      REAL     HV(NPNC), FWT(2*(NCHIMAX-1))
      REAL     RAV(NPSIMAX),R2AV(NPSIMAX),R3AV(NPSIMAX)
      REAL     WORK(6*NCHIMAX)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
      REAL     ZJ0(NPSIMAX),ZPARPAR(NPSIMAX),ZPARPER(NPSIMAX),
     >         ZAR(NPSIMAX),XL(NPSIMAX),ZPARC(NPSIMAX),
     >         BPBUSPAR(NPSIMAX), BPBUSPER(NPSIMAX),BPBUSC(NPSIMAX),
     >         DPPAR0(NPSIMAX), DPPER0(NPSIMAX),
     >         PPARAVG(NPSIMAX), PPERAVG(NPSIMAX),CAVG(NPSIMAX),
     >         DRBPHI(NPSIMAX), RGSAV(NPSIMAX),
     >         P1(NPSIMAX),P2(NPSIMAX),P3(NPSIMAX),P4(NPSIMAX),
     >         RB1(NPSIMAX),RB2(NPSIMAX),RB3(NPSIMAX),RB4(NPSIMAX),
     >         PARINTPAR(NPSIMAX), PARINTPER(NPSIMAX),ARINT(NPSIMAX),
     >         PARINTC(NPSIMAX)
C
      NGES = NPSI*NCHI
C
      DO I=1,NCHI
         INDEX(I) = I
      ENDDO
      IF (IAS.EQ.0) THEN
         DO I=NCHI+1,2*NCHI-2
            INDEX(I) = 2*NCHI-I
         ENDDO
      ENDIF
      IF (IAS.EQ.0) THEN
         N = 2*(NCHI-1)
      ELSE
         N = NCHI
      ENDIF
      DO I=1,N/2
         WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
      ENDDO
C
C-----------------first calculate the poloidal average of R,R**2,R**3
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM33,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        R2AV(I) = REAL(ZFK(1,I))
      ENDDO   
      DO I=1,NGES
         HV(I) = GEM33(I)**0.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        RAV(I) = REAL(ZFK(1,I))
      ENDDO 
      DO I=1,NGES
         HV(I) = GEM33(I)**1.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        R3AV(I) = REAL(ZFK(1,I))
      ENDDO 
      DO I=1,NGES
         HV(I) = (GEM11(I)/GEM33(I))**0.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        RGSAV(I) = REAL(ZFK(1,I))
      ENDDO
C
C     FLUX SURFACE AVERAGE OF PPAR
      DO I = 1, NPSI
         SUMP = 0.
         DO J = 1, NCHI
            NO = (I-1) * NCHI + J
            SUMP = SUMP + PPAR(NO)
         ENDDO
         SUMP = SUMP / FLOAT(NCHI)
         PPARAVG(I) = SUMP
         P0(I) = SUMP
      ENDDO
      DP1 = (P0(NPSI)-P0(NPSI-1))/(CS(NPSI)-CS(NPSI-1)) 
      CALL SPLINE(NPSI,CS,P0,0.,DP1,1,P1,P2,P3,P4)
      DO I=2,NPSI
        DPPAR0(I) = P2(I)/(2*CPSURF*CS(I))
      ENDDO
C
C     FLUX SURFACE AVERAGE OF PPER
      DO I = 1, NPSI
         SUMP = 0.
         DO J = 1, NCHI
            NO = (I-1) * NCHI + J
            SUMP = SUMP + PPER(NO)
         ENDDO
         SUMP = SUMP / FLOAT(NCHI)
         PPERAVG(I) = SUMP
         P0(I) = SUMP
      ENDDO
      DP1 = (P0(NPSI)-P0(NPSI-1))/(CS(NPSI)-CS(NPSI-1)) 
      CALL SPLINE(NPSI,CS,P0,0.,DP1,1,P1,P2,P3,P4)
      DO I=2,NPSI
        DPPER0(I) = P2(I)/(2*CPSURF*CS(I))
      ENDDO
C
C     FLUX SURFACE AVERAGE OF RBPHI
      DO I = 1, NPSI
         SUMRB = 0.
         DO J = 1, NCHI
            NO = (I-1) * NCHI + J
            SUMRB = SUMRB + T(NO)
         ENDDO
         SUMRB = SUMRB / FLOAT(NCHI)
         RBPHI(I) = SUMRB
      ENDDO
      DRB1 = (RBPHI(NPSI)-RBPHI(NPSI-1))/(CS(NPSI)-CS(NPSI-1)) 
      CALL SPLINE(NPSI,CS,RBPHI,0.,DRB1,1,RB1,RB2,RB3,RB4)
      DO I=2,NPSI
        DRBPHI(I) = RB2(I)/(2*CPSURF*CS(I))
      ENDDO
      DO I=2,NPSI
        SPS2 = 2.*CPSURF*CS(I)
        CAVG(I)     = -2. * PPERAVG(I)**2 / PPARAVG(I)
        PARINTPAR(I)=PPARAVG(I)*R2AV(I)*SPS2*QS(I)/RBPHI(I)* 2.*PI
        PARINTPER(I)=PPERAVG(I)*R2AV(I)*SPS2*QS(I)/RBPHI(I)* 2.*PI
        PARINTC(I)  =   CAVG(I)*R2AV(I)*SPS2*QS(I)/RBPHI(I)* 2.*PI
        ARINT(I)= SPS2*QS(I)/RBPHI(I) * R2AV(I)            * 2.*PI
      ENDDO
      PARINTPAR(1) = 0.  
      PARINTPER(1) = 0.
      PARINTC  (1) = 0.
      ARINT(1)  = 0. 
      CALL QINT(CS,PARINTPAR,NPSI,ZPARPAR)
      CALL QINT(CS,PARINTPER,NPSI,ZPARPER)
      CALL QINT(CS,PARINTC,NPSI,ZPARC)
      CALL QINT(CS,ARINT,NPSI,ZAR)
      DO I=2,NPSI
        BPBUSPAR(I) =-2.*(PPARAVG(I)-ZPARPAR(I)/ZAR(I))
     >        / RGSAV(I)**2
        BPBUSPER(I) =-2.*(PPERAVG(I)-ZPARPER(I)/ZAR(I))
     >        / RGSAV(I)**2
        BPBUSC  (I) =-2.*(   CAVG(I)-  ZPARC(I)/ZAR(I))
     >        / RGSAV(I)**2
      ENDDO 
C      WRITE(20,3) (I,RAV(I),R2AV(I),RGSAV(I),ARINT(I),I=1,NPSI)
    3 FORMAT(I4,3F10.5,E12.4)
C      WRITE(20,4) (I,XL(I),ZAR(I),ZPAR(I),P0(I),I=1,NPSI)
C      WRITE(20,*)
C      WRITE(20,4) (I,ZJAR(I),BPBUS(I),P0(I),DP0(I),I=1,NPSI)
    4 FORMAT(I3,4e14.6)
      
      IQ1 = 0
      IQ2 = 0
      DO I=1,NPSI
        IF (QS(I).LT.1.) IQ1=I
        IF (QS(I).LT.2.) IQ2=I
      ENDDO
      IF ((IQ1.GT.0).AND.(IQ1.LT.NPSI)) THEN
         XR1 = CS(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (CS(IQ1+1)-CS(IQ1))
         BB1A= BPBUSPAR(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (BPBUSPAR(IQ1+1)-BPBUSPAR(IQ1))
         BB1E= BPBUSPER(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (BPBUSPER(IQ1+1)-BPBUSPER(IQ1))
         BB1C= BPBUSC  (IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (BPBUSC  (IQ1+1)-BPBUSC  (IQ1))
      ENDIF
      IF ((IQ2.GT.0).AND.(IQ2.LT.NPSI)) THEN
         XR2 = CS(IQ2) + (2.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (CS(IQ2+1)-CS(IQ2))
         BB2A= BPBUSPAR(IQ2) + (1.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (BPBUSPAR(IQ2+1)-BPBUSPAR(IQ2))
         BB2E= BPBUSPER(IQ2) + (1.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (BPBUSPER(IQ2+1)-BPBUSPER(IQ2))
         BB2C= BPBUSC  (IQ2) + (1.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (BPBUSC  (IQ2+1)-BPBUSC  (IQ2))
      ENDIF
      IF (IQ1.LT.NPSI) THEN
        WRITE(20,6) XR1
        WRITE(20,8) BB1A, BB1E, (BB1A+BB1E)/2., (BB1A+BB1E+BB1C)/2.
      ENDIF
      IF (IQ2.LT.NPSI) THEN
        WRITE(20,7) XR2
        WRITE(20,8) BB2A, BB2E, (BB2A+BB2E)/2., (BB2A+BB2E+BB2C)/2.
      ENDIF
    6 FORMAT(' BUSSAC BETA AT Q=1 : S=',f6.3)
    7 FORMAT(' BUSSAC BETA AT Q=2 : S=',f6.3)
    8 FORMAT(' BETAPAR(B) = ',e12.4, ' BETAPER(B) = ',e12.4,
     >     ' BETAMHD(B) = ',e12.4,' BETAPA(B) = ',e12.4)
      END
********************************************************************
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
*DECK EQVAC
      SUBROUTINE EQVAC
C-----------------------------------------------------------------------
C     CALCULATES THE GEOMETRIC QUANTITIES OF THE COORDINATE SYSTEM IN
C     THE VACUUM.
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMPIO
*CALL COMDIAG
*CALL COMEQUI
*CALL COMVAC
*CALL COMJET
*CALL COMVGRD
*CALL COMBND
C
      REAL     ZS(4),RPP(NVCHIMX),ZPP(NVCHIMX),
     >         RWP(NVCHIMX),ZWP(NVCHIMX),DUMMY(3)
C
      PI = 2.*ASIN(1.)
      RGEO = RADIUS / ASPI
C
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'EPS      : ',ASPI
      WRITE(NOUT,*) 'RADIUS   : ',RADIUS
      WRITE(NOUT,*) 'RGEO     : ',RGEO
C
C     IMPLEMENTED JET GEOMETRY JUST FOR IAS = 1 !!!                     
C                                                                       
      IF (IVAC.EQ.4) CALL JETGEO                                        
C                                                                       
      DO 5 J=1,NVCHI
         RP(J) = RADIUS * SQRT(VX(J)**2 + VY(J)**2)
         TC(J) = ATAN2(VY(J),VX(J))
         IF (IAS.EQ.0) THEN
            VC(J) = PI * REAL(J-1)/REAL(NVCHI-1)
         ELSE
            IF((J.GT.1).AND.(TC(J).LT.TC(J-1)))
     >           TC(J) = TC(J) + 2.*PI
            VC(J) = 2. * PI * REAL(J-1)/REAL(NVCHI)
         ENDIF
                                                                        
         TCPOS = TC(J)                                                  
         IF (TCPOS.LT.0.) TCPOS = TCPOS + 2.*PI                         
         IF (TCPOS.GE.2.*PI) TCPOS = TCPOS - 2.*PI                      
                                                                        
         IF (IVAC.EQ.1) THEN
            RW(J) = RWALL * RP(J)
         ELSEIF (IVAC.EQ.4) THEN                                        
            RW(J) = SPWERT(NV,TCPOS,RV1,RV2,RV3,RV4,THTV,DUMMY)         
         ELSE
            RW(J) = RADIUS*RBOUND(FW,NFW,TC(J))/RMIN
         ENDIF
    5 CONTINUE
      DO 7 J = 1, NVCHI
         RPP(J) = RP(J) * COS(TC(J))
         ZPP(J) = RP(J) * SIN(TC(J))
         RWP(J) = RW(J) * COS(TC(J))
         ZWP(J) = RW(J) * SIN(TC(J))
 7    CONTINUE
      IF (IAS.EQ.0) THEN
         CALL NFRAME(1,1,1,-0.5,0.5,0.,1.5,
     >        'GEOMETRY',8,'R',1,'Z',1)
      ELSE
         CALL NFRAME(1,1,1,-0.5,0.5,-1.,1.,
     >        'GEOMETRY',8,'R',1,'Z',1)
      ENDIF
      CALL LPLOT(1,1,1,RPP,ZPP,-NVCHI,1,' ',1,' ',1,' ',1)
      CALL LPLOT(1,1,1,RWP,ZWP,-NVCHI,1,' ',1,' ',1,' ',1)
         

C
C ... FOURIER TRANSFORM RW AND RP IN THE ANGLE CHI!! ...
C
      DO 10 J=1,NVCHI
         FRW(J) = RW(J)
         FRP(J) = RP(J)
         FTC(J) = TC(J) - VC(J)
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 J=1,NVCHI-2
            FRW(NVCHI+J) =   FRW(NVCHI-J)
            FRP(NVCHI+J) =   FRP(NVCHI-J)
            FTC(NVCHI+J) = - FTC(NVCHI-J)
 20      CONTINUE
C
         CALL RFT2(FRW,2*(NVCHI-1),1)
         CALL RFT2(FRP,2*(NVCHI-1),1)
         CALL RFT2(FTC,2*(NVCHI-1),1)
C
C ... KEEP ONLY COSINE OR SINE PART ...
C
         DO 30 M=1,NVCHI-1
            FRW(M) = FRW(2*(M-1)+1) / REAL(NVCHI-1)
            FRP(M) = FRP(2*(M-1)+1) / REAL(NVCHI-1)
            FTC(M) = FTC(2*(M-1)+2) / REAL(NVCHI-1)
 30      CONTINUE
      ELSE
         CALL RFT2(FRW,NVCHI,1)
         CALL RFT2(FRP,NVCHI,1)
         CALL RFT2(FTC,NVCHI,1)
      ENDIF
C
C ... DERIVATIVES OF RW, RP AND TC ...
C
      DO 50 J=1,NVCHI
         ANGLE = VC(J)
         SUMW = 0.
         SUMP = 0.
         IF (IAS.EQ.0) THEN
            SUMT = 1.
         ELSE
            SUMT = 0.
         ENDIF
         SUMC = 0.
         DSUMW = 0.
         DSUMP = 0.
         DSUMT = 0.
         IF (IAS.EQ.0) THEN
            DO 40 M=2,NVCHI-1
               SUMW = SUMW - FRW(M) * (M-1) * SIN((M-1)*ANGLE)
               SUMP = SUMP - FRP(M) * (M-1) * SIN((M-1)*ANGLE)
               SUMT = SUMT - FTC(M) * (M-1) * COS((M-1)*ANGLE)
 40         CONTINUE
            DRW(J) = SUMW
            DRP(J) = SUMP
            DTC(J) = SUMT
         ELSE
            DO 45 M=2,NVCHI/2
               SUMW = SUMW - FRW(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FRW(2*M)   * (M-1) * COS((M-1)*ANGLE)
               SUMP = SUMP - FRP(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FRP(2*M)   * (M-1) * COS((M-1)*ANGLE)
               SUMT = SUMT - FTC(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FTC(2*M)   * (M-1) * COS((M-1)*ANGLE)
 45         CONTINUE
            DRW(J) = 2. * SUMW / REAL(NVCHI)
            DRP(J) = 2. * SUMP / REAL(NVCHI)
            DTC(J) = 2. * SUMT / REAL(NVCHI) + 1.
         ENDIF
   50 CONTINUE
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
      IF (IBVAC.NE.0) WRITE(NOUTVB,*) NVPSI, NVCHI, ASPI, RADIUS
C
C ... GEOMETRIC QUANTITIES ...
C
      DO 70 I=1,NVPSI
         DO 60 J=1,NVCHI
            INDEX = (I-1)*NVCHI + J
            S = REAL(I-1)/REAL(NVPSI-1)
            CSV(I) = S
             RAD = S * (RW(J) - RP(J)) + RP(J)
            BIGR = RGEO + RAD * COS(TC(J))
            DSDR = 1./(RW(J) - RP(J))
             DCDT = 1./DTC(J)
            DSDC = -DRP(J)/(RW(J)-RP(J)) - (RAD-RP(J))/(RW(J)-RP(J))**2
     >                                                 *(DRW(J)-DRP(J))
            DSDT = DSDC * DCDT
C
            VJ(INDEX) = RAD * BIGR / (DSDR * DCDT)
            VG11(INDEX) = DSDR**2 + (DSDT/RAD)**2
            VG22(INDEX) = (DCDT / RAD)**2
            VG12(INDEX) = (DSDT * DCDT) / RAD**2
            VG33(INDEX) = 1. / BIGR**2
C
C     CALCULATE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
            IF (IBVAC.NE.0) THEN
               X = RAD*COS(TC(J))/RADIUS
               Y = RAD*SIN(TC(J))/RADIUS
               DSDX = RADIUS*(DSDR*COS(TC(J))-DSDT*SIN(TC(J))/RAD)
               DSDY = RADIUS*(DSDR*SIN(TC(J))+DSDT*COS(TC(J))/RAD)
               DCDX = -RADIUS*SIN(TC(J))*DCDT/RAD
               DCDY = RADIUS*COS(TC(J))*DCDT/RAD
C
C     WRITE DATA:
C
               WRITE(NOUTVB,*) S, VC(J), X, Y
               WRITE(NOUTVB,*) DSDX, DSDY, DCDX, DCDY
            ENDIF
C ......................................................................
C           IF(I.EQ.1) THEN
C              XJAC = BIGR/SQRT(VG11(INDEX)*VG22(INDEX)-VG12(INDEX)**2)
C              WRITE(NOUTV,*) VJ(INDEX),XJAC,VJ(INDEX)-XJAC
C           ENDIF
C ......................................................................
C
   60    CONTINUE
   70 CONTINUE
C
      DO 90 NI = 1, NGV-1
         SL = SVGRID(NI)
         SU = SVGRID(NI+1)
C
C ...... BERECHNUNG DER STUETZSTELLEN ...
C
         ZDIF = SU - SL
         ZA   = .5 * (SU + SL)
         ZB   = .43056815579702629 * ZDIF
         ZC   = .16999052179242813 * ZDIF
         ZS(1) = ZA + ZB
         ZS(2) = ZA - ZB
         ZS(3) = ZA + ZC
         ZS(4) = ZA - ZC
         DO 80 I=1,4
            J = (NI-1)*4+I
            SVGI(J) = ZS(I)
   80    CONTINUE
   90 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK JETGEO                                                            
      SUBROUTINE JETGEO                                                 
C-----------------------------------------------------------------------
C THE DESCRIPTION OF R AND Z COORDINATES OF THE JET VESSEL              
C-----------------------------------------------------------------------
*CALL COMMAX                                                            
*CALL COMPARV                                                           
*CALL COMVAC                                                            
*CALL COMBND                                                            
*CALL COMJET                                                            
*CALL COMEQUI                                                           
                                                                        
      REAL RZV2(2,37),RV(73)
                                                                        
      DATA   ((RZV2(J,I),J=1,2),I=1,37) /                               
     >          4.350,    0.000,                                        
     >          4.346,    0.118,                                        
     >          4.336,    0.236,                                        
     >          4.319,    0.353,                                        
     >          4.294,    0.471,                                        
     >          4.263,    0.589,                                        
     >          4.224,    0.707,                                        
     >          4.177,    0.824,                                        
     >          4.123,    0.942,                                        
     >          4.060,    1.060,                                        
     >          3.988,    1.178,                                        
     >          3.907,    1.295,                                        
     >          3.815,    1.412,                                        
     >          3.712,    1.527,                                        
     >          3.597,    1.640,                                        
     >          3.469,    1.750,                                        
     >          3.327,    1.853,                                        
     >          3.170,    1.945,                                        
     >          3.000,    2.022,                                        
     >          2.819,    2.074,                                        
     >          2.631,    2.092,                                        
     >          2.446,    2.067,                                        
     >          2.275,    1.993,                                        
     >          2.127,    1.873,                                        
     >          2.006,    1.721,                                        
     >          1.913,    1.552,                                        
     >          1.843,    1.379,                                        
     >          1.791,    1.210,                                        
     >          1.751,    1.048,                                        
     >          1.721,    0.895,                                        
     >          1.699,    0.751,                                        
     >          1.682,    0.615,                                        
     >          1.670,    0.484,                                        
     >          1.661,    0.359,                                        
     >          1.655,    0.237,                                        
     >          1.651,    0.118,                                        
     >          1.650,    0.000 /                                       
      DATA   NV2 / 37 /                                          
                                                                        
      PI = 2.*ASIN(1.)                                                  
                                                                        
      DO 10 J=1,NV2                                                     
         RZV(1,J) = RZV2(1,J)                                           
         RZV(2,J) = RZV2(2,J)                                           
         RZV(1,2*NV2-J) = RZV2(1,J)                                     
         RZV(2,2*NV2-J) = -RZV2(2,J)                                    
   10 CONTINUE                                                          

      NV = 2*NV2-1                                                      
                                                                        
      RCNTR = RMIN/ASPI                                                 
      RMAG  = RMIN/RADIUS       
      RMIN = 0.90     
      ZCNTR = 0.32
      
      WRITE(20,11) RCNTR,ZCNTR
   11 FORMAT(' PLASMA CENTRE, R, Z : ',2f9.3)                                     
                                                                        
      DO 100 J=1,NV                                                     
        THTV(J) = ATAN2(RZV(2,J)-ZCNTR,RZV(1,J)-RCNTR)                        
        IF (J.GT.1) THEN                                                
           IF (THTV(J).LT.THTV(J-1)) THTV(J) = THTV(J) + 2.*PI          
        ENDIF                                                           
        RV(J) = SQRT((RZV(1,J)-RCNTR)**2
     >                +(RZV(2,J)-ZCNTR)**2) / RMAG            
  100 CONTINUE                                                          
                                                                        
      CALL SPLINE(NV,THTV,RV,0.,0.,2,RV1,RV2,RV3,RV4)                   
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*DECK RBOUND
      REAL FUNCTION RBOUND(FR,NFR,THETA)
C-----------------------------------------------------------------------
C     CALCULATE RADIUS AS A FUNCTION OF THETA
C-----------------------------------------------------------------------

*CALL COMEQUI
      REAL FR(*), THETA
      INTEGER NFR

      R = FR(1)
      IF (IAS.EQ.0) THEN
         DO 10 M=2, NFR
            R = R + FR(M)*COS(FLOAT(M-1)*THETA)
 10      CONTINUE
      ELSE
         DO 20 M=2, NFR/2
            R = R + FR(M*2-1)*COS(FLOAT(M-1)*THETA)
     >            + FR(M*2)*SIN(FLOAT(M-1)*THETA)
 20      CONTINUE
      ENDIF
      RBOUND = R
      END
************************************************************************
*DECK VFKEQ
      SUBROUTINE VFKEQ
C-----------------------------------------------------------------------
C     SPLINE COEFFICIENTS OF THE  FOURIER COEFFICIENTS FOR THE VACUUM
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMPARV
*CALL COMEQUI
*CALL COMVAC
*CALL COMVFT
*CALL COMVGRD
C
      INTEGER  INDEX(2*(NVCHIMX-1))
      REAL     HV(NVPNVC), FWT(2*(NVCHIMX-1))
      REAL     H1(NVPSIMX), H2(NVPSIMX), H3(NVPSIMX), H4(NVPSIMX)
      REAL     DUMMY(3)
      COMPLEX  ZFK(NVCHIMX,NVPSIMX)
*IF CRAY
      COMPLEX  WORK(3*NVCHIMX)
*ELSE
      REAL     WORK(6*NVCHIMX)
*ENDIF
C
      INTEGER  L, NVGES
      REAL     ABLTG(3)
C
      NVGES = NVPSI*NVCHI
      NVP1  = NVPSI+1
      NV2P1 = 2*NVPSI+1
      NV3P1 = 3*NVPSI+1
C
      NPR = 1
C
      DO 10 I=1,NVCHI
         INDEX(I) = I
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 I=NVCHI+1,2*NVCHI-2
            INDEX(I) = 2*NVCHI-I
 20      CONTINUE
      ENDIF
C
C     INITIALIZATION OF THE SINE AND COSINE TABLES
C     --------------------------------------------
C
      IF (IAS.EQ.0) THEN
         N = 2*(NVCHI-1)
      ELSE
         N = NVCHI
      ENDIF
*IF CRAY
      CALL RCFFT2(1,IDUMMY,N,DUMMY,WORK,DUMMY)
*ELSE
      DO 30 I=1,N/2
         WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
   30 CONTINUE
*ENDIF
C
C     FOURIER ANALYSIS AND SPLINE FOR EVERY FOURIER COEFFICIENT
C     ---------------------------------------------------------
C
C        JAC * G11  -->  VR11, VI11
C
      DO 50 I=1,NVGES
         HV(I)= VJ(I) * VG11(I)
   50 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR11,VI11)
C
C        JAC * G12 --->  VR12,VI12
C
      DO 70 I=1,NVGES
         HV(I) = VJ(I) * VG12(I)
   70 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR12,VI12)
C
C        JAC * G22 -->  VR22,VI22
C
      DO 90 I=1,NVGES
         HV(I) = VJ(I) * VG22(I)
   90 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR22,VI22)
C
C        JAC * G33 --->  VR33,VI33
C
      DO 110 I=1,NVGES
         HV(I) = VJ(I) * VG33(I)
  110 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR33,VI33)
      RETURN
C
      END

************************************************************************
*DECK VACUUM
      SUBROUTINE VACUUM
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE VACUUM  (VACUUM RESPONSE TO UNIT PERTURBATION)   **
**    ---------------------                                           **
**                                                                    **
**    STRUCTURE :                                                     **
**                 VACUUM                                             **
**                   CONVMAT                                          **
**                     CUBFCT                                         **
**                     DCUBF                                           **
**                     SPWERT                                         **
**                     FKUBL                                          **
**                   CGBFA                                            **
**                   CGBSL                                            **
**                                                                    **
************************************************************************
C
*CALL COMMAX
*CALL COMPARV
*CALL COMPIO
*CALL COMDIAG
*CALL COMWEL
*CALL COMVGRD
*CALL COMGRID
*CALL COMVRSP
*CALL COMVMAT
*CALL COMBND
C
      COMPLEX  ZMA(NZMAV*NZMAV)
      COMPLEX  ZB1, ZB2, ZB3
      INTEGER  IPVT(NBGV*NGVMAX)
C
      WRITE(NOUT,1)

      NDIMV = NGV * NBGV
C
      VFOUR1 = VFOUR(1)
      DO 50 M=1,LVMAX
C         VFOUR(M) = REAL(- LVMAX/2 + M - 1)
          VFOUR(M) = VFOUR1 + FLOAT(M) - 1.
   50 CONTINUE
C      
      DO M=1,MANZ
        RFOUR(M) = MSTART(NG) + FLOAT(M-1)
      ENDDO
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
      IF (IBVAC.NE.0) THEN
         WRITE(NOUTVB,*) NGV, MVANZ
         DO 51, M = 1, MVANZ
            WRITE(NOUTVB,*) VFOUR(M)
 51      CONTINUE
         WRITE(NOUTVB,*) MANZ
      ENDIF
C
C
      DO 70 I=1,LDAV
         DO 60 J=1,NBGV*NGVMAX
            VMAT(I,J) = (0.,0.)
   60    CONTINUE
   70 CONTINUE
      DO 120 NI=1,NGV-1
         CALL CONVMAT(NI,NZMAV,ZMA)
         NB = (NI-1)*NZMAV/2
         DO 90 L=1,NZMAV
            JZ = NB + L - 1
            IZ = 2*NZMAV
            DO 80 K=L,NZMAV
               JZ = JZ + 1
               IZ = IZ - 1
               VMAT(IZ,JZ) = VMAT(IZ,JZ) + ZMA(L+(K-1)*NZMAV)
   80       CONTINUE
   90    CONTINUE
         DO 110 L=2,NZMAV
            IZ = 2*NZMAV + L - 1
            JZ = NB
            DO 100 K=1,L-1
               IZ = IZ - 1
               JZ = JZ + 1
               VMAT(IZ,JZ) = VMAT(IZ,JZ) + ZMA(L+(K-1)*NZMAV)
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
C
      CALL CGBFA(VMAT,LDAV,NDIMV,MLV,MUV,IPVT,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,*) 'ERROR IN VACUUM --> CGBFA INFO : ',INFO
         STOP
      ENDIF
C
      DO 170 MF=1,MANZ
C
         MOFF = INT(RFOUR(MF) - VFOUR(1)) - MF + 1
         ZMPERT = RFOUR(MF)
C
         DO 130 I=1,NBGV * NGVMAX
            B(I) = (0.,0.)
  130    CONTINUE
C
C ...... FILL RIGHT HAND SIDE ...
C
         B(2*(MF+MOFF-1)+1) = (1.,0.)
C
         CALL CGBSL(VMAT,LDAV,NDIMV,MLV,MUV,IPVT,B,0,Q)
C
         DO 140 MRESP = 1,MANZ
            ZMFRES = RFOUR(MRESP)
            ZB1 = - B(2*(MRESP+MOFF-1)+2)
            ZB2 = - (0.,1.) * ZMFRES * B(2*(MRESP+MOFF-1)+1)
            ZB3 = - (0.,1.) * ZNKWEL * B(2*(MRESP+MOFF-1)+1)
            B3B1(MF,MRESP) = ZB3
            WRITE(NOUT,131) INT(ZMPERT),INT(ZMFRES),ZB1,ZB2,ZB3
  140    CONTINUE
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
         IF (IBVAC.NE.0) THEN
            DO 160 MRESP = 1,MVANZ
               DO 150 I=1,NGV
                  WRITE(NOUTVB,*) SVGRID(I),
     >                 REAL(B(2*(MRESP-1)+2*(I-1)*MVANZ+1)),
     >                 AIMAG(B(2*(MRESP-1)+2*(I-1)*MVANZ+1))
 150           CONTINUE
 160        CONTINUE
            DO 165 MRESP = 1,MVANZ
               DO 155 I=1,NGV
                  WRITE(NOUTVB,*) SVGRID(I),
     >                 REAL(B(2*(MRESP-1)+2*(I-1)*MVANZ+2)),
     >                 AIMAG(B(2*(MRESP-1)+2*(I-1)*MVANZ+2))
 155           CONTINUE
 165        CONTINUE
         ENDIF
  170 CONTINUE
      RETURN
C
C  21 FORMAT(1X,2I3,2E12.4)
  131 FORMAT(1X,2i3,6E12.4)
    1 FORMAT(1X,/,' VACUUM PERTURBATION MATRIX : ',/)
      END
************************************************************************
*DECK CONVMAT
      SUBROUTINE CONVMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMWEL
*CALL COMGEW
*CALL COMVAC
*CALL COMVFT
*CALL COMVGRD
C
      INTEGER  NI, NBG, NZMA,  MS, MZ, I, K, INDCC(1)
      REAL     SL, SU, ZDIF, ZSR, MSNQ, MZNQ, FKDUMMY,
     >         ZBIG, HC(4), DHC(4), DUMMY(3) 
      COMPLEX  G11, G12, G22, G33, ZMA(*), FACT(4,1)
C
      DATA INDCC / 1 /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      DO 10  I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
      ZBIG = 1.E+22
      NBG = NZMA/2
C
C ... INTEGRATION IM INTERVALL  SVGRID(N) - SVGRID(N+1) ...
C
      SL = SVGRID(NI)
      SU = SVGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = (SU - SL)
C
      DO 200  I = 1 , 4
C
         ZSR     = SVGI((NI-1)*4+I)
         FKDUMMY = 1.0
C
      CALL CUBFCT(ZSR,SL,SU,HC)
      CALL DCUBF (ZSR,SL,SU,DHC)
C
      DO 200   KF = 1 , MVANZ
C
        K = (KF-1) + 1
C
        G11 = CMPLX(SPWERT(NVPSI,ZSR,VR11(1,K),VR11(NVP1,K),
     >                     VR11(NV2P1,K),VR11(NV3P1,K),CSV,DUMMY),
     >              SPWERT(NVPSI,ZSR,VI11(1,K),VI11(NVP1,K),
     >                     VI11(NV2P1,K),VI11(NV3P1,K),CSV,DUMMY))
C
        G12 = - CMPLX(SPWERT(NVPSI,ZSR,VR12(1,K),VR12(NVP1,K),
     >                     VR12(NV2P1,K),VR12(NV3P1,K),CSV,DUMMY),
     >              SPWERT(NVPSI,ZSR,VI12(1,K),VI12(NVP1,K),
     >                     VI12(NV2P1,K),VI12(NV3P1,K),CSV,DUMMY))
C
        G22 = CMPLX(SPWERT(NVPSI,ZSR,VR22(1,K),VR22(NVP1,K),
     >                      VR22(NV2P1,K),VR22(NV3P1,K),CSV,DUMMY),
     >                 SPWERT(NVPSI,ZSR,VI22(1,K),VI22(NVP1,K),
     >                      VI22(NV2P1,K),VI22(NV3P1,K),CSV,DUMMY))
C
        G33 = CMPLX(SPWERT(NVPSI,ZSR,VR33(1,K),VR33(NVP1,K),
     >                      VR33(NV2P1,K),VR33(NV3P1,K),CSV,DUMMY),
     >                 SPWERT(NVPSI,ZSR,VI33(1,K),VI33(NVP1,K),
     >                      VI33(NV2P1,K),VI33(NV3P1,K),CSV,DUMMY))
C
C
      DO 100  MS = 1 , MVANZ - KF + 1
C
      MZ = MS + KF - 1
C
C
      SMZ = VFOUR(MZ)
      SMS = VFOUR(MS)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     ----------------------
C
         FACT(1:4,1) =   SMS*SMZ * G22 + ZNKWEL**2 * G33
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     ------------------------
C
         FACT(1:4,1) =   G11
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = + (0.,1.) * SMS * G12
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = - (0.,1.) * SMZ * G12
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
      IF(MS.EQ.MZ) GOTO 100
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN  P(1,1)
C     ----------------------
C
         FACT(1:4,1) =   SMS*SMZ * CONJG(G22) + ZNKWEL**2 * CONJG(G33)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG   P(1,10
C     ------------------------
C
         FACT(1:4,1) = CONJG(G11)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = + (0.,1.) * SMZ * CONJG(G12)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = - (0.,1.) * SMS * CONJG(G12)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
  100 CONTINUE
C
         FKDUMMY = 0.0
C
  200 CONTINUE
C
      DO 210  I = 1,NZMA* NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
C
      RETURN
      END
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
**    MAT1         MAT2         MAT3         MAT4         MAT5        **
**      CONBMAT      (RANSET)     (RANSET)     (RANSET)     (CXSCAL)  **
**        CUBFCT     STVAL        STVAL        (STVAL)      CONBMAT   **
**        QUAFCT     CONAMAT      CONAMAT      CONAMAT        <--     **
**        SPWERT       <--          <--          <--        (SGSCAL)  **
**        FKUBL                   (CXCOPY)     CONBMAT      CONAMAT   **
**      CONAMAT                   CONBMAT        <--          <--     **
**        CUBFCT                    <--                               **
**        QUAFCT                  (CXDOTU)                            **
**        DCUBF                   CGESLP                              **
**        DQUAF                   CGEFAP                              **
**        SPWERT                     ICMAXP                           **
**        FKUBL                                                       **
**        ADDBOUND                                                     **
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
      CALL CONBMAT(1,NZMA,ZMA)
C
      DO 20 J=1,NZMA
          DO 20 I=1,NZMA
             BMAT(I,J) = ZMA(I,J)
   20 CONTINUE
C
      CALL CONAMAT(1,NZMA,ZMA)
C
      DO 30 J=1,NZMA
         DO 30 I=1,NZMA
            AMAT(I,J) = ZMA(I,J)
   30 CONTINUE
C
      DO 60 NI=2,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         IA = (NI-1)*NBG
C
         DO 40 J=1,NZMA
            DO 40 I=1,NZMA
               BMAT(IA+I,IA+J) = BMAT(IA+I,IA+J)+ZMA(I,J)
   40    CONTINUE
C
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 50 J=1,NZMA
            DO 50 I=1,NZMA
               AMAT(IA+I,IA+J) = AMAT(IA+I,IA+J) + ZMA(I,J)
   50    CONTINUE
   60 CONTINUE
C
      RETURN
      END
C***********************************************************************
*DECK MAT2
      SUBROUTINE MAT2
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION (IN-CORE)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR2
*CALL CORE2
*CALL COMGRID
*CALL COMDIM
*CALL ISEED
C
      COMPLEX  CNUL
C
      DATA CNUL /(0.0,0.0)/
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
      DO 10 II=1,NDIM2
         X0(II) = (0.0,0.0)
         X1(II) = (0.0,0.0)
         Y0(II) = (0.0,0.0)
         Y1(II) = (0.0,0.0)
   10 CONTINUE
      CALL STVAL(NDIM,X0)
      CALL STVAL(NDIM,Y0)
C
      DO 20 I=1,LDA
      DO 20 J=1,NDIM
         AMAT(I,J) = CNUL
   20 CONTINUE
C
      DO 50 NI=1,NGINT
         CALL CONAMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 30 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 30 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AMAT(IZ,JZ) = AMAT(IZ,JZ)+ZMA(L,K)
   30    CONTINUE
         DO 40 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 40 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AMAT(IZ,JZ) = AMAT(IZ,JZ)+ZMA(L,K)
   40    CONTINUE
   50 CONTINUE
C
      RETURN
      END
C***********************************************************************
*DECK MAT3
      SUBROUTINE MAT3
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION (OUT-OF-CORE)
C     VERS: 11.9.92 (CRAY-YMP)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR3
*CALL COMPIO
*CALL CORE3
*CALL COMINT
*CALL COMIT
*CALL COMGRID
C
      COMPLEX  CXDOTU, SUM, ZZ
C
      REWIND ND3
      REWIND ND4
      REWIND ND6
C
      NBG1 = NBG+1
      NBG2 = 2*NBG+1
      NREC = (NG+NCV-1)/NCV
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
      DO 10 I=1,NG
      DO 10 K=1,NBG
         EV(K,I,1) = (0.0,0.0)
         EV(K,I,2) = (0.0,0.0)
   10 CONTINUE
C
      CALL STVAL(NBG*NG,EV(1,1,1))
      CALL STVAL(NBG*NG,EV(1,1,2))
C
C
      DO 20 J=1,NB3
      DO 20 I=1,NBG
         BUFF(I,J) = (0.0,0.0)
   20 CONTINUE
C
      DO 30 I=1,NBG
         HVX(I,1) = (0.0,0.0)
         HVX(I,2) = (0.0,0.0)
   30 CONTINUE
C
      IZAS = 0
      LWRIT = 0
      CPLG = 1
      IND3 = 1
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 200 NI=1,NG
C     --------------
      JZAS = IZAS
      IND3A = IND3
      IZAS = MOD(NI-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3 = CPLGA
         IF(NI.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND3,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND3/MAT3'
               WAIT(ND4,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND4/MAT3'
               WAIT(ND6,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND6/MAT3'
            ENDIF
            WRITE(ND3,ID=LWRIT) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            WRITE(ND4,ID=LWRIT) X(1,1,1,CPLG)...X(NBG,NCV,2,CPLG)
            WRITE(ND6,ID=LWRIT) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            WRITE(ND3) (((APR(II,IJ,IK,CPLG),
     >                 II=1,NBG),IJ=1,NB3),IK=1,NCV)
            WRITE(ND4) (((X(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NCV),IK=1,2)
            WRITE(ND6) ((IPVT(II,IK,CPLG),II=1,NBG),IK=1,NCV)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND3).GE.0.0) STOP 'ND3/MAT3'
               IF(UNIT(ND4).GE.0.0) STOP 'ND4/MAT3'
               IF(UNIT(ND6).GE.0.0) STOP 'ND6/MAT3'
            ENDIF
            BUFFER OUT(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER OUT(ND4,0) (X(1,1,1,CPLG),X(NBG,NCV,2,CPLG))
             BUFFER OUT(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
         ENDIF
      ENDIF
C
      IF(NI.LT.NG) THEN
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 40 L=1,NZMA
            M = NBG + L
            DO 40 K=1,NBG
               BUFF(K,M) = BUFF(K,M) + ZMA(K,L)
   40    CONTINUE
      ENDIF
C
      CALL CXCOPY(NBG*NB3,BUFF(1,1),1,APR(1,1,IZAS,CPLGA),1)
      CALL CXCOPY(NBG,HVX(1,1),1,X(1,IZAS,1,CPLGA),1)
      CALL CXCOPY(NBG,HVX(1,2),1,X(1,IZAS,2,CPLGA),1)
      IF(NI.EQ.NG) GOTO 110
C
      DO 50 J=1,NB3
      DO 50 I=1,NBG
         BUFF(I,J) = (0.0,0.0)
   50 CONTINUE
      DO 60 L=1,NZMA
         CALL CXCOPY(NBG,ZMA(NBG+1,L),1,BUFF(1,L),1)
   60 CONTINUE
C
C ... A-EWSHIFT*B  UND B*X ...
C
C
      CALL CONBMAT(NI,NZMA,ZMA)
C
      DO 70 J=NBG+1,NB3
         M = J-NBG
         DO 70 I=1,NBG
            APR(I,J,IZAS,CPLGA) = APR(I,J,IZAS,CPLGA)-EWSHIFT*ZMA(I,M)
   70 CONTINUE
C
      DO 80 J=1,NZMA
      DO 80 I=1,NBG
         M = I+NBG
         BUFF(I,J) = BUFF(I,J)-EWSHIFT*ZMA(M,J)
   80 CONTINUE
C
      DO 90 K=1,NBG
         X(K,IZAS,1,CPLGA) = X(K,IZAS,1,CPLGA)
     >                      + CXDOTU(NZMA,ZMA(K,1),NZMA,EV(1,NI,1),1)
         HVX(K,1) = CXDOTU(NZMA,ZMA(NBG+K,1),NZMA,EV(1,NI,1),1)
         X(K,IZAS,2,CPLGA) = X(K,IZAS,2,CPLGA)
     >                      + CXDOTU(NZMA,ZMA(K,1),NZMA,EV(1,NI,2),1)
         HVX(K,2) = CXDOTU(NZMA,ZMA(NBG+K,1),NZMA,EV(1,NI,2),1)
   90 CONTINUE
C
C
C ... A' = L*U ...
C ... L-U-ZERLEGUNG VON APR ...
C
  100 IF(NI.EQ.1) GOTO 150
C
  110 CALL CGESLP(APR(1,NBG1,JZAS,IND3A),NBG,NBG,APR(1,1,IZAS,IND3),
     >            IPVT(1,JZAS,IND3A),HVX2,1)
C
      DO 140 K2=1,NBG
         K2A = 2*NBG+K2
         DO 120 KUS=1,NBG
            HVX2(KUS,1) = APR(IPVT(KUS,JZAS,IND3A),K2A,JZAS,IND3A)
  120    CONTINUE
         DO 130 K1=1,NBG
            SUM = CXDOTU(NBG,APR(K1,1,IZAS,IND3),NBG,HVX2(1,1),1)
            ZZ = APR(K1,NBG+K2,IZAS,IND3)-SUM
            APR(K1,NBG+K2,IZAS,IND3) = ZZ
  130    CONTINUE
  140 CONTINUE
C
  150 CALL CGEFAP(APR(1,NBG1,IZAS,IND3),NBG,NBG,IPVT(1,IZAS,IND3),IER)
      IF(NI.EQ.NG) GOTO 200
      CALL CGESLP(APR(1,NBG1,IZAS,IND3),NBG,NBG,APR(1,NBG2,IZAS,IND3),
     >            IPVT(1,IZAS,IND3),HVX2,0)
C
C
  200 CONTINUE
C     --------
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.GT.1)
         WAIT(ND3,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND3/MAT3'
         WAIT(ND4,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND4/MAT3'
         WAIT(ND6,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND6/MAT3'
      ENDIF
      WRITE(ND3,ID=LWRIT) APR(1,1,1,CPLGA)...APR(NBG,NB3,NCV,CPLGA)
      WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGA)...X(NBG,NCV,2,CPLGA)
      WRITE(ND6,ID=LWRIT) IPVT(1,1,CPLGA)...IPVT(NBG,NCV,CPLGA)
*ELSEIF IBM
      WRITE(ND3) (((APR(II,IJ,IK,CPLGA),II=1,NBG),IJ=1,NB3),IK=1,NCV)
      WRITE(ND4) (((X(II,IJ,IK,CPLGA),II=1,NBG),IJ=1,NCV),IK=1,2)
      WRITE(ND6) ((IPVT(II,IK,CPLGA),II=1,NBG),IK=1,NCV)
*ENDIF
*IF CRAY
      IF(LWRIT.GT.1) THEN
         IF(UNIT(ND3).GE.0.0) STOP 'ND3/MAT3'
         IF(UNIT(ND4).GE.0.0) STOP 'ND4/MAT3'
         IF(UNIT(ND6).GE.0.0) STOP 'ND6/MAT3'
      ENDIF
      BUFFER OUT(ND3,0) (APR(1,1,1,CPLGA),APR(NBG,NB3,NCV,CPLGA))
      BUFFER OUT(ND4,0) (X(1,1,1,CPLGA),X(NBG,NCV,2,CPLGA))
      BUFFER OUT(ND6,0) (IPVT(1,1,CPLGA),IPVT(NBG,NCV,CPLGA))
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK MAT4
      SUBROUTINE MAT4
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION
C     (IN-CORE VERSION OF THE OUT-OF-CORE SOLVER)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR4
*CALL CORE4
*CALL COMIT
*CALL COMDIAG
*CALL COMGRID
*CALL ISEED
C
      COMPLEX   CXDOTU
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
      DO 10 I = 1 , NCVIC
      DO 10 K = 1 , NBG
         EV(K,I,1) = (0.0,0.0)
         EV(K,I,2) = (0.0,0.0)
   10 CONTINUE
C
      CALL STVAL(NBG*NG,EV(1,1,1))
      CALL STVAL(NBG*NG,EV(1,1,2))
C
      DO 20 NI = 1 , NCVIC
      DO 20 J  = 1 , NB3
      DO 20 I  = 1 , NBG
         APR(I,J,NI) = (0.0,0.0)
   20 CONTINUE
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 100  NI = 1 , NGINT
C
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 30  L = 1 , NZMA
            M = NBG + L
            DO 30  K = 1 , NBG
               APR(K,M,NI) = APR(K,M,NI) + ZMA(K,L)
   30    CONTINUE
C
         NIP1 = NI + 1
C
         DO 40  L = 1     , NZMA
         DO 40  K = NBG+1 , NZMA
            M = K - NBG
            APR(M,L,NIP1) = APR(M,L,NIP1) + ZMA(K,L)
   40    CONTINUE
  100 CONTINUE
C
C ... A-EWSHIFT*B  UND B*X ...
C
      NBG2 = 2*NBG
C
      DO 110 K = 1 , NBG
         X(K,1,1) = (0.0,0.0)
         X(K,1,2) = (0.0,0.0)
  110 CONTINUE
C
      CALL CONBMAT(1,NZMA,ZMA)
C
      DO 120 J = NBG+1,NB3
         M = J-NBG
         DO 120 I = 1, NBG
            APR(I,J,1) = APR(I,J,1) - EWSHIFT * ZMA(I,M)
  120 CONTINUE
C
      DO 200  NI = 2 , NG
C
         DO 130 K = 1 , NBG
            X(K,NI-1,1) = X(K,NI-1,1)
     >                    +CXDOTU(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI,1)   =  CXDOTU(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI-1,2) = X(K,NI-1,2)
     >                    +CXDOTU(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,2),1)
            X(K,NI,2)   =  CXDOTU(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,2),1)
  130    CONTINUE
C
         DO 140 J = 1 , NZMA
         DO 140 I = 1 , NBG
            M = I + NBG
            APR(I,J,NI) = APR(I,J,NI) - EWSHIFT*ZMA(M,J)
  140    CONTINUE
C
         IF(NI.LT.NG) THEN
            CALL CONBMAT(NI,NZMA,ZMA)
            DO 150  J = NBG+1 , NB3
               M = J - NBG
               DO 150  I = 1 , NBG
                  APR(I,J,NI) = APR(I,J,NI) - EWSHIFT * ZMA(I,M)
  150       CONTINUE
         ENDIF
C
  200 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK STVAL
      SUBROUTINE STVAL(N,X)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
 
*CALL COMMAX
*CALL COMPAR
*CALL ISEED
*CALL COMSTVR
 
      INTEGER  N, I, ISEED
      COMPLEX  X(*)
 
*IF IBM
C
C     TEST HORUS
C
      RSEED= 1.0
      CALL DURAND(RSEED,N,SX)
      CALL DURAND(RSEED,N,SY)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      DO 10 I=1,N
*IF CRAY
         SX = RANF( )
         SY = RANF( )
         X(I) = CMPLX(SX,SY)
*ELSE
         X(I) = CMPLX(SX(I),SY(I))
*ENDIF
   10 CONTINUE
 
      RETURN
      END
************************************************************************
*DECK DURAND
      SUBROUTINE DURAND(RSEED,NR,AR)
      USE IFPORT
      REAL AR(*)
      DO I=1,NR
         AR(I) = RAND()
      ENDDO
      RETURN
      END
************************************************************************
*DECK MAT5
      SUBROUTINE MAT5
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRICES FOR LANCZOS SOLVER
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL CORE5
*CALL COMGRID
*CALL COMDIM
*CALL COMDIM5
C
      COMPLEX  CNUL
 
      DATA CNUL /(0.0,0.0)/
 
      MUL = MULZ
      MLL = MLLZ
C
      CALL CXSCAL(LDAA*NDIM,CNUL,AA,1)
C
      DO 100 NI=1,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 10 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 10 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
   10    CONTINUE
         DO 20 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 20 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
   20    CONTINUE
  100 CONTINUE
C
      CALL SGSCAL(LDBL*NDIM5,0.0,BB,1)
C
      DO 120 J=1,NDIM
         DO 110 K=1,LDBL
            BB(K,J) = REAL(AA(K+MLL,J))
  110    CONTINUE
  120 CONTINUE
C
      CALL CXSCAL(LDAA*NDIM,CNUL,AA,1)
C
      DO 200 NI=1,NGINT
         CALL CONAMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 130 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 130 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
  130    CONTINUE
         DO 140 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 140 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
  140    CONTINUE
  200 CONTINUE
 
      CALL SGSCAL(LDAL*NDIM5,0.0,AORIG,1)
C
      DO 220 J=1,NDIM
         DO 210 K=1,LDAL
            AORIG(K,J) = REAL(AA(K+MLL,J))
  210    CONTINUE
  220 CONTINUE
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
*CALL COMVAC
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDQQ(4),INDCC(4),INDCQ(2),INDQC(1)
      REAL     SL,SU,ZDIF,ZA,ZB,ZC,ZSR,ZQ,ZT,QOT,TOQ,
     >         DZQ,DZT,ZRHO,DZRHO,T0,DT0,FKDUMMY(4),
     >         ZS(4),HC(4),HQ(4),DUMMY(3),SMZ(4),SMS(4)
      COMPLEX  B11_K(MANZ+11),B12_K(MANZ+11),B22_K(MANZ+11),
     >         B13_K(MANZ+11),B23_K(MANZ+11),B33_K(MANZ+11),
     >         B11_2K(MANZ+11), B11_3K(MANZ+11),
     >         R2OF_K(MANZ+11),
     >         B11(4), B12(4), B33(4), B13(4), B23(4), B22(4),
     >         R2OF(4), B11_2(4), B11_3(4),
     >         ZMA(NZMA*NZMA),FACT(4,7)
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
      NBG = NZMA/2
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
C ... INTEGRATION IM INTERVALL  SGRID(N) - SGRID(N+1) ...
C
      SL = SGRID(NI)
      SU = SGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = SU - SL
C
      DO 200  I = 1 , 4
C     -----------------
         ZSR     = SGI((NI-1)*4+I)
         ZQ      = Q((NI-1)*4+I)
         SPS2    = 2. * ZSR * CPSURF

         FKDUMMY =  1.0
         CW      = CWW
C
         CALL CUBFCT(ZSR,SL,SU,HC)
         CALL QUAFCT(ZSR,SL,SU,HQ)
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
      SMS(1) = MSTART(NI)   + FLOAT(MS-1) * MDIF
      SMS(2) = MSTART(NI+1) + FLOAT(MS-1) * MDIF
      SMS(3) = MSTART(NI)   + FLOAT(MS-1) * MDIF
      SMS(4) = MSTART(NI+1) + FLOAT(MS-1) * MDIF

      DO 100  MZ = 1 , MANZ
C     ------------------------------
C
      SMZ(1) = MSTART(NI)   + FLOAT(MZ-1) * MDIF
      SMZ(2) = MSTART(NI)   + FLOAT(MZ-1) * MDIF
      SMZ(3) = MSTART(NI+1) + FLOAT(MZ-1) * MDIF
      SMZ(4) = MSTART(NI+1) + FLOAT(MZ-1) * MDIF
            
      DO I4=1,4
      
        FKDUMMY(I4) = 0.
        IF (SMZ(I4) .EQ. SMS(I4)) FKDUMMY(I4) = 1. 
        KI = INT(ABS(INT(SMS(I4)) - INT(SMZ(I4))))+ 1
        IF ( (SMZ(I4) - SMS(I4)) .LE. 0) THEN
           B11(I4)     = B11_K(KI)
           B11_2(I4)   = B11_2K(KI)
           B11_3(I4)   = B11_3K(KI)
           B12(I4)     = B12_K(KI)
           B13(I4)     = B13_K(KI)
           B22(I4)     = B22_K(KI)
           B23(I4)     = B23_K(KI)
           B33(I4)     = B33_K(KI)
           R2OF(I4)    = R2OF_K(KI)
        ELSE
           B11(I4)     = CONJG(B11_K(KI))
           B11_2(I4)   = CONJG(B11_2K(KI))
           B11_3(I4)   = CONJG(B11_3K(KI))
           B12(I4)     = CONJG(B12_K(KI))
           B13(I4)     = CONJG(B13_K(KI))
           B22(I4)     = CONJG(B22_K(KI))
           B23(I4)     = CONJG(B23_K(KI))
           B33(I4)     = CONJG(B33_K(KI))
           R2OF(I4)    = CONJG(R2OF_K(KI))
        ENDIF
      ENDDO


C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(2,3), B(3,3), B(5,5)
C
      FACT(1:4,1) =  B22*ZQ
      FACT(1:4,2) =  B23*ZQ / SPS2
      FACT(1:4,3) =  B33*ZQ
      FACT(1:4,4) =  FKDUMMY 
C
      CALL FKUBL(MZ,MS,MANZ,4,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(4,4), B(6,6), B(7,7)
C
      FACT(1:4,1) = B11 / SPS2 / ZQ + B11_3 /SPS2 * ZQ + B11_2 * SPS2*ZQ 
      FACT(1:4,2) = ZQ * R2OF
      FACT(1:4,3) = FKDUMMY
      FACT(1:4,4) = ZQ * R2OF
C
      CALL FKUBL(MZ,MS,MANZ,4,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2), B(1,3)
C
      FACT(1:4,1)  = -(0.,1.) * B12 * ZQ
      FACT(1:4,2)  =  (0.,1.) * B13 * ZQ
C
      CALL FKUBL(MZ,MS,MANZ,2,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1)
C
      FACT(1:4,1)  = (0.,1.) * B12 * ZQ
       CALL FKUBL(MZ,MS,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
      IF(MS.EQ.MZ)  GOTO 100
C     ----------------------
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
      DO 210 I = 1,NZMA*NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
c$$$      IF (NI.EQ.25) THEN
c$$$         WRITE(*,*) zsr, ZMA(1)
c$$$         STOP
c$$$      ENDIF
C
C
C ... REGULARITAETSBEDINGUNG FUER S = 0 ...
C
      IF(NI.EQ.1) THEN
        DO 230 I=1,2*MANZ,2
        DO 220 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  220   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  230   CONTINUE
        DO 232 I=2*MANZ+1,10*MANZ,2
        DO 234 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  234   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  232   CONTINUE
        DO 236 I=10*MANZ+1,14*MANZ,2
        DO 238 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  238   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  236   CONTINUE
      ENDIF
C
C ... RANDBEDINGUNG FUER  S = 1 ...
C
      IF(RWALL.LE.1.AND.NI.EQ.NGINT) THEN
C
C V1 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 250 I=NBG+1,NBG+2*MANZ,2
            DO 240 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  240       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
  250    CONTINUE
C
C A2, A3  AT PLASMA BOUNDARY SET TO ZERO
C 
         DO 270 I=NBG+10*MANZ+1,NBG+12*MANZ,2
            DO 260 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  260       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
  270    CONTINUE
      ENDIF
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
*CALL COMVAC
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDCC(6),INDCQ(5),INDQC(6),INDQQ(3),IDCDC(1),
     >         IDCQ(1),IQDC(2),IDCC(2),ICDC(3)
      REAL     SL,SU,ZDIF,ZSR,ZRHO,DZRHO,ZQ,DZQ,
     >         ZT,DZT,T0,DT0,DDT0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         ZBIG, HC(4),HQ(4),DHC(4),DHQ(4),DUMMY(3),
     >         SMS(4),SMZ(4),MSNQ(4),MZNQ(4),FKDUMMY(4)
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

      COMPLEX  FF(4),DFDS(4),GPGTOF(4),GP2OF(4),DGP2OF(4),
     >         DFGP2OF2(4),GPGT2OGP2F(4),FOGP2R2(4),R2OF(4),
     >         DR2OF(4),FDFDTOR2(4),DGP2OR2(4),GP2OR2(4),
     >         GP2DFOFR2(4),GPGTOR2(4),FDFOR2(4),DXDTFOR2(4),
     >         DETF2(4),DETGP2(4),DETGPGT(4),R2(4),DR2(4),
     >         O1MDET(4),
     >         ZETA,DZETA,ZETA1,
     >         ZMA(NZMA*NZMA),FACT(4,14)

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

      COMPLEX  A41_1(4),A41_2(4),A41_3(4),A41_4(4),A42_1(4),
     >         A42_2(4),A42_3(4),A42_4(4),A43_1(4),A43_2(4),
     >         A43_3(4),A43_4(4),A4P1(4),A71_1(4),A71_2(4),
     >         A71_3(4),A71_4(4),A72_1(4),A72_2(4),A72_3(4),
     >         A72_4(4),A73_1(4),A73_2(4),A73_3(4),A73_4(4),
     >         A7P1(4),A15_1(4),A15_2(4),A15_3(4),A15_4(4),
     >         A15_5(4),A16_1(4),A16_2(4),A25_1(4),A25_2(4),
     >         A25_3(4),A25_4(4),A26_1(4),A26_2(4),A35_1(4),
     >         A35_2(4),A35_3(4),A35_4(4),A35_5(4),A36_1(4),
     >         A36_2(4),A1PD1(4),A1PD2(4),A1PD3(4),A2PD1(4),
     >         A2PD2(4),A3PD(4)
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
      NBG  = NZMA/2
C
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
C ... INTEGRATION IM INTERVALL  SGRID(N) - SGRID(N+1)...
C
      SL = SGRID(NI)
      SU = SGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = SU - SL
C
C----------------------------------- switch eta on/off in A1 equation
      ETA1 = ETA
C--------------------------------------------------------------------      
      
      DO 200 I=1,4
C     ------------
C
      ZSR     = SGI((NI-1)*4+I)
      ZQ      = Q((NI-1)*4+I)
      DZQ     = DQ((NI-1)*4+I)
      ZFT     = ZT0((NI-1)*4+I)
      DZFT    = ZDT0((NI-1)*4+I)
      
      SPS2    = 2.*ZSR*CPSURF
      DSPS    = 2.*CPSURF
      
      FKDUMMY =  1.0
C
      CALL CUBFCT(ZSR,SL,SU,HC)
      CALL QUAFCT(ZSR,SL,SU,HQ)
      CALL DCUBF (ZSR,SL,SU,DHC)
      CALL DQUAF (ZSR,SL,SU,DHQ)
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
     
      SMS(1) = MSTART(NI)   + FLOAT(MS-1) * MDIF
      SMS(2) = MSTART(NI+1) + FLOAT(MS-1) * MDIF
      SMS(3) = MSTART(NI)   + FLOAT(MS-1) * MDIF
      SMS(4) = MSTART(NI+1) + FLOAT(MS-1) * MDIF

      MSNQ(1:4) = SMS(1:4) + ZNKWEL*ZQ

      DO 100  MZ = 1,MANZ

      SMZ(1) = MSTART(NI)   + FLOAT(MZ-1) * MDIF
      SMZ(2) = MSTART(NI)   + FLOAT(MZ-1) * MDIF
      SMZ(3) = MSTART(NI+1) + FLOAT(MZ-1) * MDIF
      SMZ(4) = MSTART(NI+1) + FLOAT(MZ-1) * MDIF

      MZNQ(1:4) = SMZ(1:4) + ZNKWEL*ZQ
            
      DO I4=1,4
      
        FKDUMMY(I4) = 0.

        IF (SMZ(I4) .EQ. SMS(I4)) FKDUMMY(I4) = 1. 
        KI = ABS(SMS(I4) - SMZ(I4)) + 1
        IF ( (SMZ(I4) - SMS(I4) ) .LE. 0) THEN
           FF        (I4)= FF_K(KI)
           DFDS      (I4)= DFDS_K(KI)
           GPGTOF    (I4)= GPGTOF_K(KI)
           GP2OF     (I4)= GP2OF_K(KI)
           DGP2OF    (I4)= DGP2OF_K(KI)
           DFGP2OF2  (I4)= DFGP2OF2_K(KI)
           GPGT2OGP2F(I4)= GPGT2OGP2F_K(KI)
           FOGP2R2   (I4)= FOGP2R2_K(KI)
           R2OF      (I4)= R2OF_K(KI)
           DR2OF     (I4)= DR2OF_K(KI)
           FDFDTOR2  (I4)= FDFDTOR2_K(KI)
           DGP2OR2   (I4)= DGP2OR2_K(KI)
           GP2OR2    (I4)= GP2OR2_K(KI)
           GP2DFOFR2 (I4)= GP2DFOFR2_K(KI)
           GPGTOR2   (I4)= GPGTOR2_K(KI)
           FDFOR2    (I4)= FDFOR2_K(KI)
           DXDTFOR2  (I4)= DXDTFOR2_K(KI)
           DETF2     (I4)= DETF2_K(KI)
           DETGP2    (I4)= DETGP2_K(KI)
           DETGPGT   (I4)= DETGPGT_K(KI)
           R2        (I4)= R2_K(KI)
           DR2       (I4)= DR2_K(KI)
           O1MDET    (I4)= O1MDET_K(KI)
C
           A41_1     (I4)= A41_1_K(KI)
           A41_2     (I4)= A41_2_K(KI)
           A41_3     (I4)= A41_3_K(KI)
           A41_4     (I4)= A41_4_K(KI)
           A42_1     (I4)= A42_1_K(KI)
           A42_2     (I4)= A42_2_K(KI)
           A42_3     (I4)= A42_3_K(KI)
           A42_4     (I4)= A42_4_K(KI)
           A43_1     (I4)= A43_1_K(KI)
           A43_2     (I4)= A43_2_K(KI)
           A43_3     (I4)= A43_3_K(KI)
           A43_4     (I4)= A43_4_K(KI)
           A4P1      (I4)= A4P1_K(KI)
           A71_1     (I4)= A71_1_K(KI)
           A71_2     (I4)= A71_2_K(KI)
           A71_3     (I4)= A71_3_K(KI)
           A71_4     (I4)= A71_4_K(KI)
           A72_1     (I4)= A72_1_K(KI)
           A72_2     (I4)= A72_2_K(KI)
           A72_3     (I4)= A72_3_K(KI)
           A72_4     (I4)= A72_4_K(KI)
           A73_1     (I4)= A73_1_K(KI)
           A73_2     (I4)= A73_2_K(KI)
           A73_3     (I4)= A73_3_K(KI)
           A73_4     (I4)= A73_4_K(KI)
           A7P1      (I4)= A7P1_K(KI)
           A15_1     (I4)= A15_1_K(KI)
           A15_2     (I4)= A15_2_K(KI)
           A15_3     (I4)= A15_3_K(KI)
           A15_4     (I4)= A15_4_K(KI)
           A15_5     (I4)= A15_5_K(KI)
           A16_1     (I4)= A16_1_K(KI)
           A16_2     (I4)= A16_2_K(KI)
           A25_1     (I4)= A25_1_K(KI)
           A25_2     (I4)= A25_2_K(KI)
           A25_3     (I4)= A25_3_K(KI)
           A25_4     (I4)= A25_4_K(KI)
           A26_1     (I4)= A26_1_K(KI)
           A26_2     (I4)= A26_2_K(KI)
           A35_1     (I4)= A35_1_K(KI)
           A35_2     (I4)= A35_2_K(KI)
           A35_3     (I4)= A35_3_K(KI)
           A35_4     (I4)= A35_4_K(KI)
           A35_5     (I4)= A35_5_K(KI)
           A36_1     (I4)= A36_1_K(KI)
           A36_2     (I4)= A36_2_K(KI)
           A1PD1     (I4)= A1PD1_K(KI)
           A1PD2     (I4)= A1PD2_K(KI)
           A1PD3     (I4)= A1PD3_K(KI)
           A2PD1     (I4)= A2PD1_K(KI)
           A2PD2     (I4)= A2PD2_K(KI)
           A3PD      (I4)= A3PD_K(KI)
        ELSE
           FF        (I4)= CONJG(FF_K(KI))
           DFDS      (I4)= CONJG(DFDS_K(KI))
           GPGTOF    (I4)= CONJG(GPGTOF_K(KI))
           GP2OF     (I4)= CONJG(GP2OF_K(KI))
           DGP2OF    (I4)= CONJG(DGP2OF_K(KI))
           DFGP2OF2  (I4)= CONJG(DFGP2OF2_K(KI))
           GPGT2OGP2F(I4)= CONJG(GPGT2OGP2F_K(KI))
           FOGP2R2   (I4)= CONJG(FOGP2R2_K(KI))
           R2OF      (I4)= CONJG(R2OF_K(KI))
           DR2OF     (I4)= CONJG(DR2OF_K(KI))
           FDFDTOR2  (I4)= CONJG(FDFDTOR2_K(KI))
           DGP2OR2   (I4)= CONJG(DGP2OR2_K(KI))
           GP2OR2    (I4)= CONJG(GP2OR2_K(KI))
           GP2DFOFR2 (I4)= CONJG(GP2DFOFR2_K(KI))
           GPGTOR2   (I4)= CONJG(GPGTOR2_K(KI))
           FDFOR2    (I4)= CONJG(FDFOR2_K(KI))
           DXDTFOR2  (I4)= CONJG(DXDTFOR2_K(KI))
           DETF2     (I4)= CONJG(DETF2_K(KI))
           DETGP2    (I4)= CONJG(DETGP2_K(KI))
           DETGPGT   (I4)= CONJG(DETGPGT_K(KI))
           R2        (I4)= CONJG(R2_K(KI))
           DR2       (I4)= CONJG(DR2_K(KI))
           O1MDET    (I4)= CONJG(O1MDET_K(KI))
C
           A41_1     (I4)= CONJG(A41_1_K(KI))
           A41_2     (I4)= CONJG(A41_2_K(KI))
           A41_3     (I4)= CONJG(A41_3_K(KI))
           A41_4     (I4)= CONJG(A41_4_K(KI))
           A42_1     (I4)= CONJG(A42_1_K(KI))
           A42_2     (I4)= CONJG(A42_2_K(KI))
           A42_3     (I4)= CONJG(A42_3_K(KI))
           A42_4     (I4)= CONJG(A42_4_K(KI))
           A43_1     (I4)= CONJG(A43_1_K(KI))
           A43_2     (I4)= CONJG(A43_2_K(KI))
           A43_3     (I4)= CONJG(A43_3_K(KI))
           A43_4     (I4)= CONJG(A43_4_K(KI))
           A4P1      (I4)= CONJG(A4P1_K(KI))
           A71_1     (I4)= CONJG(A71_1_K(KI))
           A71_2     (I4)= CONJG(A71_2_K(KI))
           A71_3     (I4)= CONJG(A71_3_K(KI))
           A71_4     (I4)= CONJG(A71_4_K(KI))
           A72_1     (I4)= CONJG(A72_1_K(KI))
           A72_2     (I4)= CONJG(A72_2_K(KI))
           A72_3     (I4)= CONJG(A72_3_K(KI))
           A72_4     (I4)= CONJG(A72_4_K(KI))
           A73_1     (I4)= CONJG(A73_1_K(KI))
           A73_2     (I4)= CONJG(A73_2_K(KI))
           A73_3     (I4)= CONJG(A73_3_K(KI))
           A73_4     (I4)= CONJG(A73_4_K(KI))
           A7P1      (I4)= CONJG(A7P1_K(KI))
           A15_1     (I4)= CONJG(A15_1_K(KI))
           A15_2     (I4)= CONJG(A15_2_K(KI))
           A15_3     (I4)= CONJG(A15_3_K(KI))
           A15_4     (I4)= CONJG(A15_4_K(KI))
           A15_5     (I4)= CONJG(A15_5_K(KI))
           A16_1     (I4)= CONJG(A16_1_K(KI))
           A16_2     (I4)= CONJG(A16_2_K(KI))
           A25_1     (I4)= CONJG(A25_1_K(KI))
           A25_2     (I4)= CONJG(A25_2_K(KI))
           A25_3     (I4)= CONJG(A25_3_K(KI))
           A25_4     (I4)= CONJG(A25_4_K(KI))
           A26_1     (I4)= CONJG(A26_1_K(KI))
           A26_2     (I4)= CONJG(A26_2_K(KI))
           A35_1     (I4)= CONJG(A35_1_K(KI))
           A35_2     (I4)= CONJG(A35_2_K(KI))
           A35_3     (I4)= CONJG(A35_3_K(KI))
           A35_4     (I4)= CONJG(A35_4_K(KI))
           A35_5     (I4)= CONJG(A35_5_K(KI))
           A36_1     (I4)= CONJG(A36_1_K(KI))
           A36_2     (I4)= CONJG(A36_2_K(KI))
           A1PD1     (I4)= CONJG(A1PD1_K(KI))
           A1PD2     (I4)= CONJG(A1PD2_K(KI))
           A1PD3     (I4)= CONJG(A1PD3_K(KI))
           A2PD1     (I4)= CONJG(A2PD1_K(KI))
           A2PD2     (I4)= CONJG(A2PD2_K(KI))
           A3PD      (I4)= CONJG(A3PD_K(KI))
       	 ENDIF
      ENDDO


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
      FACT(1:4,1) = A41_1 + (0.,1.)*MZNQ*A41_2*SPS2
     >             +DZQ * A41_3 + ZQ * A41_4
      FACT(1:4,2) = -FKDUMMY
      FACT(1:4,3) = A71_1 + (0.,1.)*MZNQ*A71_2*SPS2
     >             +DZQ * A71_3 + ZQ * A71_4
      FACT(1:4,4) = (DR2/ZFT-R2*DZFT/ZFT**2) / SPS2
     >         -(A1PD1+A1PD2/SPS2+(0.,1.)*MSNQ*A1PD3)
      FACT(1:4,5) = SPS2/ZQ*(SMZ+ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ) * 
     >              (GPGT2OGP2F/ZSR**2 + FOGP2R2/ZSR**2/ZQ**2)
     >             +2*(0.,1.)*(SMZ-SMS)/ZQ**2*DZQ * GPGTOF
     >             +DZQ/SPS2/ZQ**3*(2*DZQ*GP2OF-ZQ*DFGP2OF2)
     >             +DZQ/SPS2/ZQ**2*DGP2OF
     >             +(-A15_2/SPS2 - A15_3 - (0.,1.)*MZNQ*A15_5)*DZQ/ZQ**2    ANI
     >             +(0.,1.)*MSNQ*A16_1/ZQ/SPS2 + MSNQ*MZNQ*A16_2*SPS2/ZQ    ANI
      FACT(1:4,6) = (A1PD1+A1PD2/SPS2+(0.,1.)*MSNQ*A1PD3)
C
      CALL FKUBL(MZ,MS,MANZ,6 ,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(4,2),  A(7,2),  A(4,3),  A(7,3),  A(1,5)
C     pperv2,  pparv2,  pperv3,  pparv3,   v1a1
C
      FACT(1:4,1) = (0.,1.)*A42_1 + SMZ*A42_2 + ZNKWEL*A42_3*ZQ
      FACT(1:4,2) = (0.,1.)*A72_1 + SMZ*A72_2 + ZNKWEL*A72_3*ZQ
      FACT(1:4,3) = ((0.,1.)*A43_1 + MZNQ*A43_2+ MSNQ*A43_3)
C     >     *SPS2
      FACT(1:4,4) = ((0.,1.)*A73_1 + MZNQ*A73_2+ MSNQ*A73_3)
C     >     *SPS2
      FACT(1:4,5) = SMS/SPS2/ZQ*DFDS - ZNKWEL/SPS2*DGP2OF
     >             -(0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)*ZNKWEL * GPGTOF
     >             +ZNKWEL/SPS2/ZQ*(ZQ*DFGP2OF2 - 2*DZQ*GP2OF)
     >             +SMS*A15_1/SPS2/ZQ + ZNKWEL*(A15_2/SPS2 + A15_3)         ANI
     >             +(0.,1.)*MZNQ*(SMS*A15_4/ZQ+ZNKWEL*A15_5)                ANI
C
      CALL FKUBL(MZ,MS,MANZ,5,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,4),  A(3,4),  A(2,6), A(3,6), A(2,7), A(3,7)
C     v2pper,  v3pper,   v2a2,   v3a2,  v2ppar, v3ppar
C
      FACT(1:4,1) = SMS/SPS2 * R2/ZFT
     >          -((0.,1.)*A2PD1 / SPS2 + MSNQ * A2PD2/SPS2)
      FACT(1:4,2) =  - (0.,1.) * A3PD
      FACT(1:4,3) =(0.,1.)*(SMZ-SMS-ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             +(SMS+2*ZNKWEL*ZQ)*DZQ/SPS2/ZQ**2*GP2OF
     >             -(SMS+ZNKWEL*ZQ)/SPS2/ZQ*DFGP2OF2
     >             +(SMS+ZNKWEL*ZQ)/SPS2/ZQ*DGP2OF
     >             +(-(0.,1.)*A25_2/SPS2 - MZNQ*A25_4)*DZQ/ZQ**2            ANI
     >             +MSNQ*A26_1/ZQ/SPS2 + (0.,1.)*MSNQ*MZNQ*A26_2/ZQ         ANI
     >             -SMZ*DETGP2/ZFT/SPS2/ZQ**2*DZQ                           ANI
     >             +(0.,1.)*SMZ*MSNQ*DETGPGT/ZFT/ZQ                         ANI
      FACT(1:4,4) = ((SMS+ZNKWEL*ZQ)*(DGP2OR2/ZQ + GP2OR2*DZQ/ZQ**2
     >             - GP2DFOFR2 / ZQ + FDFOR2 / ZQ
     >             +DXDTFOR2 * SPS2/ZQ )
     >             -(0.,1.)*FDFDTOR2 * DZQ/ZQ**2)
     >             +(-(0.,1.)*(A35_2 + A35_3)-MZNQ*A35_5)*DZQ/ZQ**2         ANI
     >             +MSNQ*A36_1/ZQ +(0.,1.)*MSNQ*MZNQ*A36_2*SPS2/ZQ          ANI
      FACT(1:4,5) = ((0.,1.)*A2PD1/SPS2 + MSNQ * A2PD2/SPS2)
      FACT(1:4,6) = (O1MDET * MSNQ + (0.,1.) * A3PD)
C     
      CALL FKUBL(MZ,MS,MANZ,6 ,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(5,2),  A(2,5),  A(3,5)
C      a1v2,    v2a1,    v3a1
C
      FACT(1:4,1) = FKDUMMY
      FACT(1:4,2) =-1./SPS2/ZQ*(SMS*SMZ*FF+ZNKWEL**2*ZQ**2*GP2OF)
     >             -(SMZ-SMS)*SMS/SPS2/ZQ*FF
     >             +(0.,1.)*(SMS*A25_1/ZQ/SPS2 + ZNKWEL*(A25_2/SPS2))       ANI
     >             +MZNQ*(SMS*A25_3/ZQ+ZNKWEL*A25_4)/SPS2                   ANI
     >             -SMZ*SMS*DETF2/SPS2/ZQ/ZFT                               ANI
     >             +SMZ*ZNKWEL*DETGP2/SPS2/ZFT                              ANI
      FACT(1:4,3) = (0.,1.) * FDFDTOR2 / ZQ
     >             +(0.,1.)*(SMS*A35_1/ZQ+ZNKWEL*(A35_2+ A35_3))            ANI
     >             +MZNQ*(SMS*A35_4/ZQ+ZNKWEL*A35_5)                        ANI
      CALL FKUBL(MZ,MS,MANZ,3,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',6')
C     dv1dA2 
C
      FACT(1:4,1) = 1./SPS2/ZQ*(FF+GP2OF)
     >          +(DETF2 + DETGP2)/SPS2/ZQ/ZFT                             ANI
      CALL FKUBL(MZ,MS,MANZ,1,IDCDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C      
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',5)
C     dv1A1   
C
      FACT(1:4,1) =-SMS/SPS2/ZQ*FF + ZNKWEL/SPS2*GP2OF
     >     -SMS*DETF2/ZFT/ZQ/SPS2 + ZNKWEL*DETGP2/ZFT/SPS2                ANI
C
      CALL FKUBL(MZ,MS,MANZ,1,IDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,6')  A(3,6')
C     v2dA2,   V3DA2
C
      FACT(1:4,1) = 1./SPS2/ZQ*(SMZ*FF - ZNKWEL*ZQ*GP2OF)
     >             +(SMZ-SMS)/SPS2/ZQ*FF
     >             +(0.,1.)*(-A25_1 + A25_2)/SPS2/ZQ                       ANI
     >             +MZNQ * (-A25_3 + A25_4)/SPS2/ZQ                        ANI
     >             +SMZ * (DETF2+DETGP2)/ZQ/SPS2/ZFT                       ANI
      FACT(1:4,2) =(0.,1.)*(-A35_1 + A35_2)/ZQ +(0.,1.)* A35_3/ZQ          ANI
     >             +MZNQ * (-A35_4 + A35_5)/ZQ                             ANI
      CALL FKUBL(MZ,MS,MANZ,2,IQDC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C      A(1',4), A(1',6)
C        dv1p,   dv1a2
C
      FACT(1:4,1) = R2 / ZFT / SPS2
      FACT(1:4,2) =  (0.,1.)*(SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             -DZQ/SPS2/ZQ**2*GP2OF
     >          -DETGP2/SPS2/ZQ**2*DZQ                                     ANI
     >          +(0.,1.) * MSNQ * DETGPGT / ZQ /ZFT                        ANI
C
      CALL FKUBL(MZ,MS,MANZ,2,IDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(4,1'), A(7,1'), A(1,6'),  
C     pperdv1, ppardv1,  v1dA2,     
c
      FACT(1:4,1) = A4P1
      FACT(1:4,2) = A7P1
      FACT(1:4,3) =-(0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)/ZQ*GPGTOF
     >             -1./SPS2/ZQ*DGP2OF - 1./SPS2/ZQ*DFDS
     >             -1./SPS2/ZQ**2*(2.*DZQ*GP2OF - ZQ*DFGP2OF2)
     >             +(-A15_1 + A15_2)/SPS2/ZQ + A15_3/ZQ                    ANI
     >             +(0.,1.) * MZNQ * (-A15_4 + A15_5)/ZQ                   ANI
C
      CALL FKUBL(MZ,MS,MANZ,3,ICDC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
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
      DO 210 I = 1,NZMA*NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
c    
C
C ... REGULARITAETSBEDINGUNG FUER S = 0 ...
C
      IF(NI.EQ.1) THEN
        DO 230 I=1,2*MANZ,2
        DO 220 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  220   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  230   CONTINUE
        DO 232 I=2*MANZ+1,10*MANZ,2
        DO 234 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  234   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  232   CONTINUE
        DO 236 I=10*MANZ+1,14*MANZ,2
        DO 238 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  238   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  236   CONTINUE
      ENDIF
C
C ... RANDBEDINGUNG FUER  S = 1 ...
C
      IF (NI.EQ.NGINT) THEN
        CALL ADDBND(ZMA)
      ENDIF  
      IF(RWALL.GT.1.AND.NI.EQ.NGINT) THEN
ccc         CALL ADDBND(ZMA)
      ELSEIF(NI.EQ.NGINT) THEN
C
C V1 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 250 I=NBG+1,NBG+2*MANZ,2
            DO 240 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  240       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
  250    CONTINUE
C
C A2, A3 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 270 I=NBG+10*MANZ+1,NBG+12*MANZ,2
            DO 260 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  260       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
  270    CONTINUE
        ENDIF
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
C modified to allow different factors coming from left and right element
C nodes (i.e. to allow different harmonics on the nodes)
C   fact(1,:) MS from LEFT,  MZ from LEFT
C   fact(2,:) MS from RIGHT, MZ from LEFT
C   fact(3,:) MS from LEFT,  MZ from RIGHT
C   fact(4,:) MS from RIGHT, MZ from RIGHT
C Guido Huysmans (9-2-2004) 
C-----------------------------------------------------------------------
C
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND,INDO,INDU,INDN,MZ,MS,IANZ,L
      COMPLEX  ZMA(*), FACT(4,*)
      REAL     H1(*), H2(*), GEW
C
      NGL=NBG/(2*L)
C
      DO 5 J=1,IANZ
         FACT(1:4,J)=FACT(1:4,J)*GEW
    5 CONTINUE
C
      DO 10 I=1,IANZ
         IND    = ((INDHG(I)-1)/NGL * 2 * L + (MS-1) * 2 ) * NZMA
     >           + MOD(INDHG(I)-1,NGL) * 2 * L + (MZ - 1) * 2 + 1
         INDO   = IND+NBG*NZMA
         INDU   = IND+NBG
         INDN   = INDO+NBG
         IZ = MOD(INDHG(I)-1,NGL)+1
         IS =   (INDHG(I)-1)/NGL+1
C
         ZMA(IND)         = ZMA(IND)         + H1(2) * FACT(1,I) * H2(2)
         ZMA(IND+NZMA)    = ZMA(IND+NZMA)    + H1(2) * FACT(1,I) * H2(4)
         ZMA(IND+1)       = ZMA(IND+1)       + H1(4) * FACT(1,I) * H2(2)
         ZMA(IND+1+NZMA)  = ZMA(IND+1+NZMA)  + H1(4) * FACT(1,I) * H2(4)
C
         ZMA(INDO)        = ZMA(INDO)        + H1(2) * FACT(2,I) * H2(1)
         ZMA(INDO+NZMA)   = ZMA(INDO+NZMA)   + H1(2) * FACT(2,I) * H2(3)
         ZMA(INDO+1)      = ZMA(INDO+1)      + H1(4) * FACT(2,I) * H2(1)
         ZMA(INDO+1+NZMA) = ZMA(INDO+1+NZMA) + H1(4) * FACT(2,I) * H2(3)
C
         ZMA(INDU)        = ZMA(INDU)        + H1(1) * FACT(3,I) * H2(2)
         ZMA(INDU+NZMA)   = ZMA(INDU+NZMA)   + H1(1) * FACT(3,I) * H2(4)
         ZMA(INDU+1)      = ZMA(INDU+1)      + H1(3) * FACT(3,I) * H2(2)
         ZMA(INDU+1+NZMA) = ZMA(INDU+1+NZMA) + H1(3) * FACT(3,I) * H2(4)
C
         ZMA(INDN)        = ZMA(INDN)        + H1(1) * FACT(4,I) * H2(1)
         ZMA(INDN+NZMA)   = ZMA(INDN+NZMA)   + H1(1) * FACT(4,I) * H2(3)
         ZMA(INDN+1)      = ZMA(INDN+1)      + H1(3) * FACT(4,I) * H2(1)
         ZMA(INDN+1+NZMA) = ZMA(INDN+1+NZMA) + H1(3) * FACT(4,I) * H2(3)
C
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ADDBND
      SUBROUTINE ADDBND(ZMA)
C-----------------------------------------------------------------------
C     ADDS THE BOUNDARY CONTRIBUTIONS TO THE ZMA MATRIX OF THE LAST
C     INTERVAL IN TERMS OF THE PLASMA VELOCITY AND VACUUM FIELD
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMWEL
*CALL COMGRID
*CALL COMEQUI
*CALL COMIOD
*CALL COMSPL
*CALL COMFFT
*CALL COMEQV
*CALL COMVRSP
*CALL COMVAC
C
      COMPLEX  ZMA(*),FACT(4),R2,GPGT,ZETA
      REAL     ZSR,ZRHO,ZQ,MSNQ,MZNQ,FKDUMMY,
     >         ZT,T0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         DUMMY(3)
      INTEGER  NI,MS,MZ,I,K,
     >         INDBM(1),INDA1(1),INDA2(1)
C
      DATA INDBM /  1 /
      DATA INDA1 / 17 /
      DATA INDA2 / 24 /
C
      ZSR  = 1.
      ZQ   = Q1(NPSI)
      ZT   = RBP1(NPSI)
      ZETA = ETA
      T0   = P1(NPSI)
C
      QOT  = ZQ/ZT
      TOQ  = ZT/ZQ
      SPS2 = 2.*CPSURF
C
C-----------------------------------------------------------------------
C           A(3,3), A(3,4')
C-----------------------------------------------------------------------
C
      DO 200  KF = 1, LANZ
        K = (KF-1) * MDIF + 1
        GPGT = CMPLX(SPWERT(NPSI,ZSR,RGPGT(1,K),RGPGT(NP1,K),
     >                        RGPGT(N2P1,K),RGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGT(1,K),IGPGT(NP1,K),
     >                        IGPGT(N2P1,K),IGPGT(N3P1,K),CS,DUMMY))
        DO 100  MS = 1 , MANZ - KF + 1
          MZ = MS + KF - 1
          SMZ  = RFOUR(MZ)
          SMS  = RFOUR(MS)
c------------------------------------------ A(3,3)          
          FACT(1)= (0.,1.)*ZETA*SMS*GPGT/SPS2
          CALL FBOUND(MZ,MS,MANZ,1,INDA1,NBG,NZMA,ZMA,FACT,1)
c----------------------------------------------------------------------          
c idrv=1 in case of derivative of cubic OR quadratic FEM at boundary   
c----------------------------------------------------------------------       
c------------------------------------------ A(3,4')          
          FACT(1)= -(0.,1.)*ZETA*GPGT/SPS2
          CALL FBOUND(MZ,MS,MANZ,1,INDA2,NBG,NZMA,ZMA,FACT,1)        
c    
          IF(MS.EQ.MZ) GOTO 100
c
c------------------------------------------ A(3,3)          
          FACT(1)= (0.,1.)*ZETA*SMZ*CONJG(GPGT)/SPS2
          CALL FBOUND(MS,MZ,MANZ,1,INDA1,NBG,NZMA,ZMA,FACT,0)
c------------------------------------------ A(3,4')          
          FACT(1)= -(0.,1.)*ZETA*CONJG(GPGT)/SPS2
          CALL FBOUND(MS,MZ,MANZ,1,INDA2,NBG,NZMA,ZMA,FACT,1)        
  100   CONTINUE
  200 CONTINUE    
C
C-----------------------------------------------------------------------
C           A(1,1)      MOMENTUM EQ.
C-----------------------------------------------------------------------
      IF (RWALL .GT. 1.) THEN
        DO 20 MS = 1, MANZ
           SMS = MSTART(NG) + FLOAT(MS-1) 
           DO 10 MZ = 1, MANZ
              SMZ = MSTART(NG) + FLOAT(MZ-1) 
             FACT(1)=-(0.,1.)*(SMZ+ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ)/ZNKWEL
     >              /ZQ**2  * B3B1(MS,MZ) 
             CALL FBOUND(MZ,MS,MANZ,1,INDBM,NBG,NZMA,ZMA,FACT,0)
   10      CONTINUE
   20   CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*DECK FBOUND
      SUBROUTINE FBOUND(MZ,MS,L,IANZ,INDHG,NBG,NZMA,ZMA,FACT,IDRV)
C-----------------------------------------------------------------------
C     ADDS THE BOUNDARY TERMS TO THE ZMA MATRIX
C          DESCRIPTION SEE FKUBL
C          IDRV = 0 NO DERIVATIVES IN THE SECOND VARIABLE
C          IDRV = 1 ADDS TO THE DERIVATIVE OF THE SECOND VARIABLE
C-----------------------------------------------------------------------
C
      COMPLEX  ZMA(*), FACT(*)
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND, INDO, INDU, INDN,
     >         MZ, MS, IANZ, L
C
      NGL = NBG/(2*L)
C
      DO 10 I=1,IANZ
         IND  = ((INDHG(I)-1)/NGL * 2 * L + (MS-1) * 2 ) * NZMA
     >          + MOD(INDHG(I)-1,NGL) * 2 * L + (MZ - 1) * 2 + 1
         INDO = IND+NBG*NZMA
         INDN = INDO+NBG
C
         IF(IDRV.EQ.0) THEN
            ZMA(INDN)      = ZMA(INDN) + FACT(I)
         ELSE
            ZMA(INDN+NZMA) = ZMA(INDN+NZMA) + FACT(I)
         ENDIF
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
*CALL COMIT
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
      IF (IQUA.GE.1) THEN
        DS2=(SU-SL)**2
        SM=(SU+SL)/2.
        H(1)= 4.*(S-SL)*(SU-S)/DS2
        H(2)= 0.0
        H(3)= 2.*(S-SM)*(S-SL)/DS2
        H(4)= 2.*(S-SM)*(S-SU)/DS2
      ELSE
        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H(1)= 3.*Q1**2 - 2.*Q1**3
        H(2)= 3.*Q2**2 - 2.*Q2**3
        H(3)= (S-SU)*Q1**2
        H(4)= (S-SL)*Q2**2
      ENDIF
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
*CALL COMIT
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
      IF (IQUA.GE.1) THEN
        DS2=(SU-SL)**2
        H(1)= 4.*(-2.*S+SU+SL)/DS2
        H(2)= 0.0
        H(3)= (4.*S-SU-3.*SL)/DS2
        H(4)= (4.*S-SL-3.*SU)/DS2
      ELSE
        DS= SU-SL
        H(1)= 6.*(S-SL)/DS**2-6.*(S-SL)**2/DS**3
        H(2)= -6.*(SU-S)/DS**2+6.*(SU-S)**2/DS**3
        H(3)= ((S-SU)*2.*(S-SL)+(S-SL)**2)/DS**2
        H(4)= ((S-SL)*2.*(S-SU)+(S-SU)**2)/DS**2
      ENDIF
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
C
      INTEGER  LWORK,IPIV(NDIM1)
      REAL     RWORK(2*NDIM1)
      COMPLEX  WORK(2*NDIM1)
C
C
C ... QR - ALGORITHM ...
C
      CALL ZPOTRF('U',NDIM,BMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,3)
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
 
      CALL ZPOTRS('U',NDIM,NDIM,BMAT,NDIM1,AMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
      
      LWORK = 2*NDIM1
      CALL ZGEEV('N','N', NDIM, AMAT, NDIM1, WRI, VL, NDIM1,
     >            VRI, NDIM1, WORK, LWORK, RWORK, INFO)
C

      WRITE(NOUT,43) (I,WRI(I),I=1,NDIM)
      WRITE(NOUTI,43)(I,WRI(I),I=1,NDIM)
C
      RETURN
C
    3 FORMAT(///5X,'MATRIX BMAT1 NOT POSITIVE DEFINIT')
    4 FORMAT(' ZPOTRF : INFO = ',I4)
    5 FORMAT(' ZGEEV  : INFO = ',I4)
   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')
      END
************************************************************************
*DECK SOLV2
      SUBROUTINE SOLV2
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV2 : INVERSE VECTOR ITERATION, IN-CORE (WITHOUT BMAT)        **
**            VERSION B,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                      **
**                 SOLV2                                              **
**                   MATVER                                           **
**                     CONBMAT                                        **
**                   CSHIFT                                           **
**                     CONBMAT                                        **
**                   CGBFA                                            **
**                     (IZAMAX)                                      **
**                     (ZSCAL)                                       **
**                     (ZAXPY)                                       **
**                   (ZSCAL)                                         **
**                   CGBSL                                            **
**                     (ZAXPY)                                       **
**                     (ZCOPY)                                       **
**                     (ZDOTC)                                       **
**                   (ZDOTC)                                         **
**                   (ZCOPY)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR2
*CALL COMPIO
*CALL CORE2
*CALL COMIT
*CALL COMGRID
*CALL COMDIM
*CALL COMEQUI
C
      COMPLEX  EWALT, EWNEU
      COMPLEX  ZDOTC, PRD1, PRD2, CONE, CZERO
C
      DATA INC /1/
      DATA QQ /1.E-4/

      CONE  = (1.0,0.0)
      CZERO = (0.0,0.0)
C
C ... R(0) = B * X(0) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)

      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,X0,1,CZERO,X1,1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,Y0,1,CZERO,Y1,1)


C      CALL CSHIFT(AMAT,BMAT,LDA,NDIM,EWSHIFT,ZMA,NZMA)
      
      DO J=1,NDIM                                                       
        DO I=1,LDB                                                      
          AMAT(NZMA+I-1,J) = AMAT(NZMA+I-1,J) - EWSHIFT * BMAT(I,J)     
        ENDDO                                                           
      ENDDO   

C      AMAT(NZMA:LDA,1:NDIM) = AMAT(NZMA:LDA,1:NDIM) 
C     >                       - EWSHIFT * BMAT(1:LDB,1:NDIM)
C
C ... ABSCHAETZEN START-EIGENWERT ...
C
      EWALT = QQ * EWSHIFT
C
C
C ... ZERLEGEN AMAT : AMAT = L * R ...
C
C      CALL CGBFA(AMAT,LDA,NDIM,ML,MU,IPVT,INFO)
C-----------------------------------------  use LAPACK ROUTINE
      CALL ZGBTRF(NDIM,NDIM,ML,MU,AMAT,LDA,IPVT,INFO)
C
      IF(INFO.NE.0) THEN
         WRITE(NOUT,1) INFO
         STOP
      ENDIF
C
C
      IT = 0
C
C ... ITERATIONEN ...
C
   10 CONTINUE
C
      IT = IT + 1
C
C ... R(I) = DELTA(LAMBDA(I-1)) * R(I-1) ...
C
      CALL ZSCAL(NDIM,EWALT,X1,INC)
      CALL ZSCAL(NDIM,CONJG(EWALT),Y1,INC)
C
C ... LOESUNG GL.SYST  ( A - LMBDA * B ) * X(I) = X(I-1) ...
C
C
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,X1,0,X0)
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,Y1,1,Y0)
C---------------------------------------------- use LAPACK routine
      CALL XGBTRS('N',NDIM,ML,MU,1,AMAT,LDA,IPVT,X1,NDIM2,INFO,X0)
      CALL XGBTRS('C',NDIM,ML,MU,1,AMAT,LDA,IPVT,Y1,NDIM2,INFO,Y0)
C
C ... PRD1 = HERM(Y) * L * R * X ...
C
      PRD1 = ZDOTC(NDIM,Y0,INC,X0,INC)
C
C ... KOPIEREN ITERIERTE WERTE ...
C
      CALL ZCOPY(NDIM,X1,INC,X0,INC)
      CALL ZCOPY(NDIM,Y1,INC,Y0,INC)
C
C ... R(I) = B * X(I) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,X0,1,CZERO,X1,1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,Y0,1,CZERO,Y1,1)

C
C ... PRD2 = HERM(Y) * B * X ...
C
      PRD2 = ZDOTC(NDIM,Y0,INC,X1,INC)
C
C
      EWNEU = PRD1/PRD2
C
      EW = EWSHIFT + EWNEU
C
C ... RELATIVE AENDERUNG DES EIGENWERTES ...
C
      DE = ABS(CABS(EWNEU/EWALT)-1.0)
      WRITE(NOUT,11) IT,EW,DE
      WRITE(*,*)'ITERATION:',IT,'EIGENVALUE:',EW,'CHANGE:',DE      

      IF(DE.LE.EPS ) GOTO 30
      IF(IT.GE.ITER ) GOTO 20
C
      EWALT = EWNEU
      DEALT = DE
C
      GOTO 10
   20 WRITE(NOUT,21) IT
C
   30 CONTINUE
C     WRITE(NOUT,31) (X0(JJJ),JJJ=1,NDIM)
C
      RETURN
C
    1 FORMAT(' INFO =',I4,' ON DECOMPOSITION')
   11 FORMAT(1X,' IT : ',I2,' EIGENVALUE : ',1P,2E12.4,
     >       '   REL. CHANGE : ',E12.4,0P)
   21 FORMAT(' STOPPED AFTER ',I4,' ITERATIONS')
   31 FORMAT(' EIGENVECTOR:'/(5(1X,1P,2E12.4,0P)))
      END
************************************************************************
C*DECK SOLV1
C      SUBROUTINE SOLV1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULES SOLV1 - SOLV5  (EIGENVALUE SOLVERS)             **
**    -----------------------------                                   **
**                                                                    **
************************************************************************
************************************************************************
**                                                                     **
**    SOLV1 : QR-SOLVER (COMPLEX)                                     **
**            VERSION C,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV1                                              **
**                   (CPOCO)                                          **
**                   (CPOSL)                                          **
**                   (CBAL)                                           **
**                   (CORTH)                                          **
**                   (COMQR)                                          **
**                   (ORDERS)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
C*CALL COMMAX
C*CALL COMPAR
C*CALL COMPIO
C*CALL COMDIM
C*CALL CORE1
C
C      INTEGER  IWORK(257)
C      INTEGER INTGER(NDIM1)
C      CHARACTER*1 UPLO
C      REAL     AMATR(NDIM1,NDIM1), AMATI(NDIM1,NDIM1), DUMMY(NDIM1)
C      COMPLEX  HVEC(NDIM1), WORK(2*NDIM1),RWORK(NDIM1)
C
C      EQUIVALENCE (BMAT(1,1), AMATR(1,1))
C      EQUIVALENCE (BMAT(1,NDIM1/2+1), AMATI(1,1))
C      EQUIVALENCE (HVEC(1), WR(1))
C      EQUIVALENCE (HVEC(NDIM1/2+1), WI(1))
C      EQUIVALENCE (INTGER(1),RWORK(1))
C
C ... QR - ALGORITHM ...
C
C*IF CRAY
C      TIME1 = SECOND()
C*ELSE
C      TIME1 = X05BAF()
C*ENDIF
C
C      CALL CPOCO(BMAT,NDIM1,NDIM,RCOND,HVEC,INFO)
C 
C ... LINPACK ZPOCO REPLACED TO LAPACK ZPOTRF (ALSO USABLE WITH NAG)
C ... FIRST CALCULATE 1-NORM (REQUIRED FOR CONDITION NUMBER)
C     
C      UPLO = 'U'
C      ANORM = F06UCF('1',UPLO,NDIM,BMAT,NDIM1,RWORK)
C      INFO = 0
C      CALL ZPOTRF(UPLO,NDIM,BMAT,NDIM1,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,3)
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C
C ... ESTIMATE CONDITION NUMBER WITH LAPACK ZPOCON
C
C      CALL ZPOCON(UPLO,NDIM,BMAT,NDIM1,ANORM,RCOND,WORK,RWORK,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C      WRITE(NOUT,1) RCOND
C 
C      CALL ZPOTRS(UPLO,NDIM,NDIM,BMAT,NDIM1,AMAT,NDIM1,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C 
C     DO 10 K=1,NDIM
C        CALL CPOSL(BMAT,NDIM1,NDIM,AMAT(1,K))
C  10 CONTINUE
C  
C      DO 20 J=1,NDIM
C      DO 20 I=1,NDIM
C         AMATR(I,J)=REAL(AMAT(I,J))
C   20 CONTINUE
C      DO 30 J=1,NDIM
C      DO 30 I=1,NDIM
C         AMATI(I,J)=AIMAG(AMAT(I,J))
C   30 CONTINUE
C     CALL CBAL(NDIM1,NDIM,AMATR,AMATI,IS1,IS2,WI)
C     CALL CORTH(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI)
C     CALL COMQR(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI,IERR)
C      CALL F02AJF(AMATR,NDIM1,AMATI,NDIM1,NDIM,WR,WI,INTGER,IERR)
C
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,3)
C         STOP
C      ENDIF
C
C*IF CRAY
C      TIME2 = SECOND()
C      WRITE(NOUT,11) TIME2-TIME1
C*ELSE
C      TIME2 = X05BAF()
C      TIMDIF = (TIME2-TIME1)*1.0E-06
C      WRITE(NOUT,11) TIMDIF
C*ENDIF
C
C
C      IF(IERR.NE.0) THEN
C         WRITE(NOUT,31) IERR
C         STOP
C      ENDIF
C
C      DO 40 I=1,NDIM
C         EVMAG(1,I) = CABS(CMPLX(WR(I),WI(I)))
C   40 CONTINUE
C
C*IF CRAY
C      CALL ORDERS(2,IWORK,EVMAG,INDEX,NDIM,1,8,1)
C*ELSE
C      CALL DSORTX(EVMAG,+1,NDIM,INDEX)
C      CALL DSVRGP(NDIM,EVMAG,DUMMY,INDEX)
C*ENDIF
C
C      WRITE(NOUT,41)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
C*IF IBM
C      WRITE(NOUTI,43)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
C*ENDIF
C
C      RETURN
C
C    1 FORMAT('   KONDITION = ',1P,E16.6,0P)
C    3 FORMAT(///5X,'MATRIX BMAT1 NICHT POSITIV DEFINIT')
C    4 FORMAT(' INFO = ',I4)
C   11 FORMAT('   TIME = ',1P,E16.6,0P)
C   31 FORMAT(//' HQR/HQZ :  IERR=',I5)
C   41 FORMAT(1X,I3,'.-EIGENWERT:',1P,2E16.6,0P)
C   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')
C      END

************************************************************************
C*DECK SOLV2
C      SUBROUTINE SOLV2
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV2 : INVERSE VECTOR ITERATION, IN-CORE (WITHOUT BMAT)        **
**            VERSION B,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                      **
**                 SOLV2                                              **
**                   MATVER                                           **
**                     CONBMAT                                        **
**                   CSHIFT                                           **
**                     CONBMAT                                        **
**                   CGBFA                                            **
**                     (ICXAMAX)                                      **
**                     (CXSCAL)                                       **
**                     (CXAXPY)                                       **
**                   (CXSCAL)                                         **
**                   CGBSL                                            **
**                     (CXAXPY)                                       **
**                     (CXCOPY)                                       **
**                     (CXDOTC)                                       **
**                   (CXDOTC)                                         **
**                   (CXCOPY)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
C*CALL COMMAX
C*CALL COMPAR
C*CALL COMPAR2
C*CALL COMPIO
C*CALL CORE2
C*CALL COMIT
C*CALL COMGRID
C*CALL COMDIM
C*CALL COMEQUI
C
C      COMPLEX  EWALT, EWNEU
C      COMPLEX  CXDOTC, PRD1, PRD2
C
C      DATA INC /1/
C      DATA QQ /1.E-4/
C
C ... R(0) = B * X(0) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C      CALL CSHIFT(AMAT,LDA,NDIM,EWSHIFT,ZMA,NZMA)
C
C ... ABSCHAETZEN START-EIGENWERT ...
C
C      EWALT = QQ * EWSHIFT
C
C
C ... ZERLEGEN AMAT : AMAT = L * R ...
C
C      CALL CGBFA(AMAT,LDA,NDIM,ML,MU,IPVT,INFO)
C
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,1) INFO
C         STOP
C      ENDIF
C
C
C      IT = 0
C
C ... ITERATIONEN ...
C
C   10 CONTINUE
C
C      IT = IT + 1
C
C ... R(I) = DELTA(LAMBDA(I-1)) * R(I-1) ...
C
C      CALL CXSCAL(NDIM,EWALT,X1,INC)
C      CALL CXSCAL(NDIM,CONJG(EWALT),Y1,INC)
C
C ... LOESUNG GL.SYST  ( A - LMBDA * B ) * X(I) = X(I-1) ...
C
C
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,X1,0,X0)
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,Y1,1,Y0)
C
C ... PRD1 = HERM(Y) * L * R * X ...
C
C      PRD1 = CXDOTC(NDIM,Y0,INC,X0,INC)
C
C ... KOPIEREN ITERIERTE WERTE ...
C
C      CALL CXCOPY(NDIM,X1,INC,X0,INC)
C      CALL CXCOPY(NDIM,Y1,INC,Y0,INC)
C
C ... R(I) = B * X(I) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C
C ... PRD2 = HERM(Y) * B * X ...
C
C      PRD2 = CXDOTC(NDIM,Y0,INC,X1,INC)
C      WRITE(NOUT,11) PRD1,PRD2
C
C
C      EWNEU = PRD1/PRD2
C
C      WRITE(NOUT,13) IT
C      EW = EWSHIFT + EWNEU
C      WRITE(NOUT,15) EW
C
C ... RELATIVE AENDERUNG DES EIGENWERTES ...
C
C      DE = ABS(CABS(EWNEU/EWALT)-1.0)
C      WRITE(NOUT,17) EWNEU,DE
C      IF(DE.LE.EPS ) GOTO 30
C      IF(IT.GE.ITER ) GOTO 20
C
C      EWALT = EWNEU
C      DEALT = DE
C
C      GOTO 10
C   20 WRITE(NOUT,21) IT
C
C   30 CONTINUE
C     WRITE(NOUT,31) (X0(JJJ),JJJ=1,NDIM)
C
C      RETURN
C
C    1 FORMAT(' INFO =',I4,' ON DECOMPOSITION')
C   11 FORMAT(///' PRD1 = HERM(Y) * L * R * X  PRD2 = HERM(Y) * B * X',
C     >       1P,2E12.4,2X,2E12.4,0P)
C   13 FORMAT(' ITERATION =',I5)
C   15 FORMAT(' EIGENWERT =',1P,2E14.5)
C   17 FORMAT(' DELT(EW) =',1P,E21.11,E20.11,/' DE =',E20.11,0P)
C   21 FORMAT(' NACH',I4,' ITERATIONEN ABGEBROCHEN')
C   31 FORMAT(' EIGENVEKTOR:'/(5(1X,1P,2E12.4,0P)))
C      END
************************************************************************
*DECK XGBTRS      
      SUBROUTINE XGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,
     $                   INFO, Q )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         AB( LDAB, * ), B( LDB, * ), Q(LDB,*)
*     ..
*
*  Purpose
*  =======
*
*  ZGBTRS solves a system of linear equations
*     A * X = B,  A**T * X = B,  or  A**H * X = B
*  with a general band matrix A using the LU factorization computed
*  by ZGBTRF.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations.
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Conjugate transpose)
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of subdiagonals within the band of A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals within the band of A.  KU >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AB      (input) COMPLEX*16 array, dimension (LDAB,N)
*          Details of the LU factorization of the band matrix A, as
*          computed by ZGBTRF.  U is stored as an upper triangular band
*          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
*          the multipliers used during the factorization are stored in
*          rows KL+KU+2 to 2*KL+KU+1.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices; for 1 <= i <= N, row i of the matrix was
*          interchanged with row IPIV(i).
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LNOTI, NOTRAN
      INTEGER            I, J, KD, L, LM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEMV, ZGERU, ZLACGV, ZSWAP, ZTBSV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.( 2*KL+KU+1 ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGBTRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      KD = KU + KL + 1
      LNOTI = KL.GT.0
*
      IF( NOTRAN ) THEN
*
*        Solve  A*X = B.
*
*        Solve L*X = B, overwriting B with X.
*
*        L is represented as a product of permutations and unit lower
*        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
*        where each transformation L(i) is a rank-one modification of
*        the identity matrix.
*
         IF( LNOTI ) THEN
            DO 10 J = 1, N - 1
               LM = MIN( KL, N-J )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
               CALL ZGERU( LM, NRHS, -ONE, AB( KD+1, J ), 1, B( J, 1 ),
     $                     LDB, B( J+1, 1 ), LDB )
   10       CONTINUE
         END IF
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
         DO 20 I = 1, NRHS
*
*           Solve U*X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'No transpose', 'Non-unit', N, KL+KU,
     $                  AB, LDAB, B( 1, I ), 1 )
   20    CONTINUE
*
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
*
*        Solve A**T * X = B.
*
         DO 30 I = 1, NRHS
*
*           Solve U**T * X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'Transpose', 'Non-unit', N, KL+KU, AB,
     $                  LDAB, B( 1, I ), 1 )
   30    CONTINUE
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
*        Solve L**T * X = B, overwriting B with X.
*
         IF( LNOTI ) THEN
            DO 40 J = N - 1, 1, -1
               LM = MIN( KL, N-J )
               CALL ZGEMV( 'Transpose', LM, NRHS, -ONE, B( J+1, 1 ),
     $                     LDB, AB( KD+1, J ), 1, ONE, B( J, 1 ), LDB )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
   40       CONTINUE
         END IF
*
      ELSE
*
*        Solve A**H * X = B.
*
         DO 50 I = 1, NRHS
*
*           Solve U**H * X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'Conjugate transpose', 'Non-unit', N,
     $                  KL+KU, AB, LDAB, B( 1, I ), 1 )
   50    CONTINUE
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
*        Solve L**H * X = B, overwriting B with X.
*
         IF( LNOTI ) THEN
            DO 60 J = N - 1, 1, -1
               LM = MIN( KL, N-J )
               CALL ZLACGV( NRHS, B( J, 1 ), LDB )
               CALL ZGEMV( 'Conjugate transpose', LM, NRHS, -ONE,
     $                     B( J+1, 1 ), LDB, AB( KD+1, J ), 1, ONE,
     $                     B( J, 1 ), LDB )
               CALL ZLACGV( NRHS, B( J, 1 ), LDB )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
   60       CONTINUE
         END IF
      END IF
      RETURN
*
*     End of ZGBTRS
*
      END
      
************************************************************************
*DECK MATVER
      SUBROUTINE MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C-----------------------------------------------------------------------
C     REDUCED MEMORY STEUERWALD SOLVER ROUTINES
C-----------------------------------------------------------------------
C
      INTEGER NZMA,I,J,K,NBG,L,NGINT
      COMPLEX ZMA(NZMA,NZMA),CTE
      COMPLEX X0(*),X1(*),Y0(*),Y1(*)
C
      DO 10 I=1,NDIM
         X1(I) = (0.0,0.0)
         Y1(I) = (0.0,0.0)
   10 CONTINUE
C
      NBG = NZMA/2
C
      DO 60 NI=1,NGINT
C
         CALL CONBMAT(NI,NZMA,ZMA)
C
C ... PERFORM MULTIPLICATION ...
C
         DO 30 J=1,NZMA
            CTE = X0((NI-1)*NBG+J)
            DO 20 I=1,NZMA
               X1((NI-1)*NBG+I)=X1((NI-1)*NBG+I)+ZMA(I,J)*CTE
   20       CONTINUE
   30    CONTINUE
         DO 50 J=1,NZMA
            CTE = X0((NI-1)*NBG+J)
            DO 40 I=1,NZMA
               Y1((NI-1)*NBG+I)=Y1((NI-1)*NBG+I)+ZMA(I,J)*CTE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CSHIFT
      SUBROUTINE CSHIFT(A,LDA,N,EW,ZMA,NZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
C
      INTEGER  LDA, N, I, K, NBG, NZMA
      COMPLEX  A(LDA,*), EW, ZMA(NZMA,*)
C
      NBG = NZMA/2
C
      DO 30 NI=1,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 10 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 10 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               A(IZ,JZ) = A(IZ,JZ)-ZMA(L,K)*EW
   10    CONTINUE
         DO 20 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 20 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               A(IZ,JZ) = A(IZ,JZ)-ZMA(L,K)*EW
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGBFA
      SUBROUTINE CGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
C-----------------------------------------------------------------------
C
C     CGBFA FACTORS A COMPLEX BAND MATRIX BY ELIMINATION.
C
C     CGBFA IS USUALLY CALLED BY CGBCO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C
C     ON ENTRY
C
C        ABD     COMPLEX(LDA, N)
C                CONTAINS THE MATRIX IN BAND STORAGE.  THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  ABD  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  ABD .
C                SEE THE COMMENTS BELOW FOR DETAILS.
C
C          SPEICHERUNG DER MATRIX ABD:
C
C                   1      2  .  . .  .  MU      MU+1 . . . . .  N
C                 ____________________________________________________
C             1  |            FREI
C             2  |            FREI
C             .  |
C            ML  |            FREI
C                 ____________________________________________________
C          ML+1  |   -    - .   .   .    -    A(1,MU+1). . . .A(N-MU,N)
C          ML+2  |   -    - .   .   . A(1,MU) A(2,MU+1). . . A(N-MU+1,N)
C            .   |
C            .   |
C         ML+MU  |   -   A(1,2) A(2,3) . . . . . . . . . . . . A(N-1,N)
C      HAUPTDIAG | A(1,1) A(2,2) . . . . . . . . . . . . . . .  A(N,N)
C        ML+MU+2 | A(2,1) A(3,2) . . . . . . . . . . A(N,N-1)    -
C        ML+MU+3 | A(3,1) A(4,2) . . . . . . A(N,N-2)    -       -
C           .    |
C           .    |
C     2*ML+MU+1  | A(ML+1,1) . . . .  A(N,N-ML) - . . . . . . .  -
C                 ____________________________________________________
C
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C                LDA MUST BE .GE. 2*ML + MU + 1 .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C                0 .LE. ML .LT. N .
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C                0 .LE. MU .LT. N .
C                MORE EFFICIENT IF  ML .LE. MU .
C     ON RETURN
C
C        ABD     AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                      CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT SGBSL WILL DIVIDE BY ZERO IF
C                     CALLED.  USE  RCOND  IN SGBCO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     BAND STORAGE
C
C           IF  A  IS A BAND MATRIX, THE FOLLOWING PROGRAM SEGMENT
C           WILL SET UP THE INPUT.
C
C                   ML = (BAND WIDTH BELOW THE DIAGONAL)
C                   MU = (BAND WIDTH ABOVE THE DIAGONAL)
C                   M = ML + MU + 1
C                   DO 20 J=1,N
C                      I1 = MAX0(1, J-MU)
C                      I2 = MIN0(N, J+ML)
C                      DO 10 I=I1,I2
C                         K = I - J + M
C                         ABD(K,J) = A(I,J)
C                10    CONTINUE
C                20 CONTINUE
C
C           THIS USES ROWS  ML+1  THROUGH  2*ML+MU+1  OF  ABD .
C           IN ADDITION, THE FIRST  ML  ROWS IN  ABD  ARE USED FOR
C           ELEMENTS GENERATED DURING THE TRIANGULARIZATION.
C           THE TOTAL NUMBER OF ROWS NEEDED IN  ABD  IS  2*ML+MU+1 .
C           THE  ML+MU BY ML+MU  UPPER LEFT TRIANGLE AND THE
C           ML BY ML  LOWER RIGHT TRIANGLE ARE NOT REFERENCED.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CXAXPY=C(Z)AXPY,CXSCAL=C(Z)SCAL,ICXAMAX=IC(Z)AMAX
C     FORTRAN MAX0,MIN0,ABS,AIMAG,REAL
C-----------------------------------------------------------------------
C
      INTEGER  I, ICXAMAX, I0, J, JU, JZ, J0, J1, K, KP1, L, LM,M,MM,NM1
      INTEGER  LDA, N, ML, MU, IPVT(*), INFO
      REAL     CABS1
      COMPLEX  ABD(LDA,*)
      COMPLEX  T, ZDUM
C
      CABS1(ZDUM) = ABS(REAL(ZDUM))+ABS(AIMAG(ZDUM))
      M = ML + MU + 1
      INFO = 0
C
C ... ZERO INITIAL FILL-IN COLUMNS ...
C
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF(J1.LT.J0) GOTO 30
         DO 20 JZ=J0,J1
            I0 = M + 1 - JZ
            DO 10 I=I0,ML
               ABD(I,JZ) = (0.0E0,0.0E0)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      JZ = J1
      JU = 0
C
C ... GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING ...
C
      NM1 = N - 1
      IF(NM1.LT.1) GOTO 130
C
      DO 120 K=1,NM1
         KP1 = K + 1
C
C ... ZERO NEXT FILL-IN COLUMN ...
C
         JZ = JZ + 1
         IF(JZ.GT.N) GOTO 50
         IF(ML.LT.1) GOTO 50
            DO 40 I=1,ML
               ABD(I,JZ) = (0.0E0,0.0E0)
   40       CONTINUE
   50    CONTINUE
C
C ... FIND L = PIVOT INDEX ...
C
         LM = MIN0(ML,N-K)
         L = ICXAMAX(LM+1,ABD(M,K),1) + M - 1
         IPVT(K) = L + K - M
C
C ... ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED ...
C
         IF(CABS1(ABD(L,K)).EQ.0.0E0) GOTO 100
C
C ... INTERCHANGE IF NECESSARY ...
C
         IF(L.EQ.M) GOTO 60
            T = ABD(L,K)
            ABD(L,K) = ABD(M,K)
            ABD(M,K) = T
   60    CONTINUE
C
C ... COMPUTE MULTIPLIERS ...
C
         T = -(1.0E0,0.0E0)/ABD(M,K)
         CALL CXSCAL(LM,T,ABD(M+1,K),1)
C
C ... ROW ELIMINATION WITH COLUMN INDEXING ...
C
         JU = MIN0(MAX0(JU,MU+IPVT(K)),N)
         MM = M
         IF(JU.LT.KP1) GOTO 90
            DO 80 J=KP1,JU
               L = L - 1
               MM = MM - 1
               T = ABD(L,J)
               IF(L.EQ.MM) GOTO 70
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
   70          CONTINUE
               CALL CXAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   80       CONTINUE
   90    CONTINUE
         GOTO 110
  100    CONTINUE
         INFO = K
  110    CONTINUE
  120 CONTINUE
C
  130 CONTINUE
      IPVT(N) = N
      IF(CABS1(ABD(M,N)).EQ.0.0E0) INFO = N
C
      RETURN
      END
************************************************************************
*DECK CGBSL
      SUBROUTINE CGBSL(ABD,LDA,N,ML,MU,IPVT,B,JOB,Q)
C-----------------------------------------------------------------------
C     ======================================================
C     =    CGBSL SOLVES THE COMPLEX BAND SYSTEM            =
C     =    A * X = B  OR  TRANS(A) * X = B                 =
C     =    USING THE FACTORS COMPUTED BY CGBCO OR CGBFA.   =
C     ======================================================
C
C     ON ENTRY
C
C        ABD     COMPLEX(LDA, N)
C                THE OUTPUT FROM CGBCO OR CGBFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM SGBCO OR SGBFA.
C
C        B       COMPLEX(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  A*X = B ,
C                = NONZERO   TO SOLVE  HERM(A)*X = B , WHERE
C                         HERM(A)  IS THE CONJUGATE TRANSPOSE.
C                             OF MATRIX A.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF CGBCO HAS SET RCOND .GT. 0.0
C        OR CGBFA HAS SET INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL CGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
C           IF(RCOND IS TOO SMALL) GOTO ...
C           DO 10 J=1,P
C              CALL CGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
C        10 CONTINUE
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CXAXPY=C(Z)AXPY,CXDOTC=C(Z)DOTC
C     FORTRAN MIN0,CONJG
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      INTEGER  K, KB, L, LA, LB, LM, M, NM1
      COMPLEX  ABD(LDA,*), B(*), Q(*)
      COMPLEX  CXDOTC, T
C
      M = MU + ML + 1
      NM1 = N - 1
C
      IF(JOB.NE.0) GOTO 60
C
C ... JOB = 0 , SOLVE  A * X = B ...
C ... FIRST SOLVE L*Y = B ...
C
      IF(ML.EQ.0) GOTO 30
      IF(NM1.LT.1) GOTO 30
         DO 20 K=1,NM1
            LM = MIN0(ML,N-K)
            L = IPVT(K)
            T = B(L)
            IF(L.EQ.K) GOTO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL CXAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20    CONTINUE
   30 CONTINUE
      CALL CXCOPY(N,B,1,Q,1)
C
C ... NOW SOLVE  U*X = Y ...
C
      DO 50 KB=1,N
         K = N + 1 - KB
         T = -B(K)/ABD(M,K)
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         B(K) = -T
         DO 40 KL=1,LM
            B(LB)=B(LB)+T*ABD(LA,K)
            LA=LA+1
            LB=LB+1
   40    CONTINUE
   50 CONTINUE
C
      GOTO 110
C
   60 CONTINUE
C
C ... JOB = NONZERO, SOLVE  HERM(A) * X = B ...
C ... FIRST SOLVE  HERM(U)*Y = B ...
C
      DO 70 K=1,N
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = CXDOTC(LM,ABD(LA,K),1,B(LB),1)
         B(K) = (B(K) - T)/CONJG(ABD(M,K))
   70 CONTINUE
      CALL CXCOPY(N,B,1,Q,1)
C
C ... NOW SOLVE HERM(L)*X = Y ...
C
      IF(ML.EQ.0) GOTO 100
      IF(NM1.LT.1) GOTO 100
         DO 90 KB=1,NM1
            K = N - KB
            LM = MIN0(ML,N-K)
            B(K) = B(K) + CXDOTC(LM,ABD(M+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF(L.EQ.K) GOTO 80
               T = B(L)
               B(L) = B(K)
               B(K) = T
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
C
  110 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SOLV3
      SUBROUTINE SOLV3
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV3 : INVERSE VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)         **
**            VERSION G, 6.5.92                                       **
**                 (FEHLER AUS VERS.F (SR.RITER) FUER N*NCV=NG        **
**                  BESEITIGT)                                        **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV3                                              **
**                   VIT                                              **
**                     (CXSCAL)                                       **
**                     CGES2P                                         **
**                   VITER                                            **
**                     (CXSCAL)                                       **
**                     CGES2P                                         **
**                   (SKIPR)                                          **
**                   RITER                                            **
**                     CGES2P                                         **
**                     (CXCOPY)                                       **
**                     CONBMAT                                        **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR3
*CALL COMPIO
*CALL CORE3
*CALL COMIT
*CALL COMINT
*CALL COMGRID
*CALL COMEQUI
C
      COMPLEX  QP, YR, DLI, DLIM1
C
      DATA QQ /1.E-4/
C
      DLIM1 = QQ*EWSHIFT
C
*IF KUL
      WAIT(ND4,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND4/SOLV3'
      WAIT(ND3,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND3/SOLV3'
      WAIT(ND6,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND6/SOLV3'
*ENDIF
*IF CRAY
      IF(UNIT(ND4).GE.0.0) STOP 'ND4/SOLV3'
      IF(UNIT(ND3).GE.0.0) STOP 'ND3/SOLV3'
      IF(UNIT(ND6).GE.0.0) STOP 'ND6/SOLV3'
*ENDIF
      REWIND ND3
      REWIND ND4
      REWIND ND6
C
C ... LOESUNG DES GLEICHUNGSSYSTEMS ...
C
      IT = 1
C
      CALL VIT(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIM1)
C     --------
      GOTO 30
C
   10 IT = IT + 1
C
      IF(IT.GT.ITER) GOTO 40
      DLIM1 = DLI
C
      CALL VITER(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIM1)
C     ----------
C
   30 CALL RITER(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,IPVT,ZMA,BUFF,X,EV)
C     ----------
C
      DLI = QP/YR
C     WRITE(NOUT,31) IT,DLI,DLIM1,QP,YR
C
      EW = EWSHIFT+DLI
      WRITE(NOUT,33) IT,EW
      IF(ABS(CABS(DLI/DLIM1)-1.0).LE.EPS ) GOTO 50
      GOTO 10
   40 CONTINUE
      WRITE(NOUT,41) IT-1
   50 CONTINUE
*IF CRAY
      WRITE(NOUTI,51) IT,EW
*ENDIF
*IF KUL
      WAIT(ND4,ID=NREC,COND=I1)
      IF(I1.NE.1) STOP ' ND4/SOLV3'
*ENDIF
      RETURN
C
   31 FORMAT(///1X,I2,'-TE ITERATION, DELTA LAMBDA (I)=',1P,2E12.4,
     >       ' DLIM1=',2E12.4/18X,'QP=',2E12.4/18X,'YR=',2E12.4)
   33 FORMAT(///1X,I3,' ITERATIONEN, EIGENWERT:',1P,2E14.6)
   41 FORMAT(' NACH',I4,' ITERATIONEN ABGEBROCHEN')
   51 FORMAT(1X,I3,' ITERATIONEN, EIGENWERT:',1P,2E14.6)
      END
************************************************************************
*DECK VIT
      SUBROUTINE VIT (NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      LREAD = 1
      LWRIT = 0
*IF KUL
      READ(ND3,ID=LREAD) APR(1,1,1,1)...APR(NBG,NB3,NCV,1)
      READ(ND4,ID=LREAD) X(1,1,1,1)...X(NBG,NCV,2,1)
      READ(ND6,ID=LREAD) IPVT(1,1,1)...IPVT(NBG,NCV,1)
*ELSEIF IBM
      READ(ND3) (((APR(II,IJ,IK,1),II=1,NBG),IJ=1,NB3),IK=1,NCV)
      READ(ND4) (((X(II,IJ,IK,1),II=1,NBG),IJ=1,NCV),IK=1,2)
      READ(ND6) ((IPVT(II,IJ,1),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
      BUFFER IN(ND3,0) (APR(1,1,1,1),APR(NBG,NB3,NCV,1))
      BUFFER IN(ND4,0) (X(1,1,1,1),X(NBG,NCV,2,1))
      BUFFER IN(ND6,0) (IPVT(1,1,1),IPVT(NBG,NCV,1))
*ENDIF
      CPLG = 1
      CPLGX = 1
      CPLGXA = 3
      CPLGXS = 2
C
      QP = (0.0,0.0)
C
      DO 50 I=1,NG
C     ------------
C
      IZAS = MOD(I-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3XA = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = IND3XA
*IF KUL
         WAIT(ND3,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND3 '
         WAIT(ND6,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND6 '
         WAIT(ND4,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND4 '
*ENDIF
*IF CRAY
         IF(UNIT(ND3).GE.0.0) STOP ' VIT/ND3 '
         IF(UNIT(ND6).GE.0.0) STOP ' VIT/ND6 '
         IF(UNIT(ND4).GE.0.0) STOP ' VIT/ND4 '
*ENDIF
         IF(LREAD.LT.NREC) THEN
            LREAD = LREAD+1
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
            READ(ND4,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
            READ(ND4) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
            BUFFER IN(ND4,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
         IF(I.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND5,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP ' VIT/ND5 '
            ENDIF
            WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
            WRITE(ND5) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
            ENDIF
            BUFFER OUT(ND5,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
         ENDIF
      ENDIF
C
      CALL CXSCAL(NBG,DLIALT,X(1,IZAS,1,CPLGXA),1)
      CALL CXSCAL(NBG,CONJG(DLIALT),X(1,IZAS,2,CPLGXA),1)
C
      IF(I.EQ.1) GOTO 20
      DO 10 K1=1,NBG
         SUM = CXDOTU(NBG,APR(K1,1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(K1,IZAS,2,CPLGXA) = X(K1,IZAS,2,CPLGXA)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),0)
C
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1+2*NBG,IZAS,CPLGA),1,
     >               X(1,IZAS,2,CPLGXA),1)
         HVX(K1,1) = SUM
   30 CONTINUE
C
C ... Q(*)*P ...
C
      SUM = (0.0,0.0)
      DO 40 J=1,NBG
         JPV = IPVT(J,IZAS,CPLGA)
         SUM=SUM+CONJG(X(JPV,IZAS,2,CPLGXA))*X(J,IZAS,1,CPLGXA)
   40 CONTINUE
      QP = QP+SUM
      CALL CXCOPY (NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
C
   50 CONTINUE
C     --------
C
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND5,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND5 '
      ENDIF
      WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND5) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
      ENDIF
      BUFFER OUT(ND5,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND4
C
      RETURN
      END
************************************************************************
*DECK VITER
      SUBROUTINE VITER(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      LREAD = 1
      LWRIT = 0
      CPLG = CPLGA
      IND3XA = CPLGXS
      CPLGXS = CPLGX
      CPLGX = CPLGXA
      CPLGXA = IND3XA
C
      QP = (0.0,0.0)
*IF KUL
      WAIT(ND4,ID=NREC,COND=I1)
      IF(I1.NE.1) STOP ' VIT/ND4 '
*ENDIF
*IF CRAY
      IF(UNIT(ND4).GE.0.0) STOP 'VIT/ND4 '
*ENDIF
C
      DO 50 I=1,NG
C     ------------
C
      IZAS = MOD(I-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3XA = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = IND3XA
*IF KUL
         IF(LREAD.NE.1) THEN
            WAIT(ND3,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND3 '
            WAIT(ND6,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND6 '
            WAIT(ND4,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND4 '
         ENDIF
*ENDIF
*IF CRAY
         IF(LREAD.NE.1) THEN
            IF(UNIT(ND3).GE.0.0) STOP 'VIT/ND3 '
            IF(UNIT(ND6).GE.0.0) STOP 'VIT/ND6 '
            IF(UNIT(ND4).GE.0.0) STOP 'VIT/ND4 '
         ENDIF
*ENDIF
         IF(LREAD.LT.NREC) THEN
            LREAD = LREAD+1
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
            BACKSPACE ND4
            BACKSPACE ND4
*IF KUL
            READ(ND4,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND4) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND4,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
         IF(I.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND5,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP ' VIT/ND5 '
            ENDIF
            WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
            WRITE(ND5) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
            ENDIF
            BUFFER OUT (ND5,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
         ENDIF
      ENDIF
C
      CALL CXSCAL(NBG,DLIALT,X(1,IZAS,1,CPLGXA),1)
      CALL CXSCAL(NBG,CONJG(DLIALT),X(1,IZAS,2,CPLGXA),1)
C
      IF(I.EQ.1) GOTO 20
      DO 10 K1=1,NBG
         SUM = CXDOTU(NBG,APR(K1,1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(K1,IZAS,2,CPLGXA) = X(K1,IZAS,2,CPLGXA)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),0)
C
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1+2*NBG,IZAS,CPLGA),1,
     >               X(1,IZAS,2,CPLGXA),1)
        HVX(K1,1) = SUM
   30 CONTINUE
C
C ... Q(*)*P ...
C
      SUM = (0.0,0.0)
      DO 40 J=1,NBG
         JPV = IPVT(J,IZAS,CPLGA)
         SUM = SUM+CONJG(X(JPV,IZAS,2,CPLGXA))*X(J,IZAS,1,CPLGXA)
   40 CONTINUE
      QP = QP+SUM
      CALL CXCOPY(NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
C
   50 CONTINUE
C     --------
C
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND5,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND5 '
      ENDIF
      WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND5) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
      ENDIF
      BUFFER OUT(ND5,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND4
C
      RETURN
      END
************************************************************************
*DECK RITER
      SUBROUTINE RITER(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,IPVT,ZMA,BUFF,
     >                 X,EV)
C-----------------------------------------------------------------------
C     VERSION : 18.9.92
C        BMAT WIRD JEDESMAL NEU BERECHNET
C     SEIT 6.5.92:
C        FEHLER FUER ALLE N*NCV=NG BESEITIGT
C     SEIT 9.9.91:
C     1. BMAT MUSS NICHT MEHR SYMMETRISCH SEIN
C     2. EIGENVEKTOR FUER PLOT WIRD NICHT MEHR WEGGESSCHRIEBEN,SONDERN
C        IN EV GESPEICHERT
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  BUFF(NBG,*),ZMA(2*NBG,*), EV(*)
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*),HVX(NBG,*),HVX2(NBG,*)
      COMPLEX  SUM, SUMY, CXDOTU, CXDOTC, YR
C
      NBG1 = NBG+1
      NZMA=2*NBG
      LWRIT = 0
      CPLG = CPLGA
      IND3XA = CPLGXS
      CPLGXS = CPLGX
      CPLGX = CPLGXA
      CPLGXA = IND3XA
C
      YR = (0.0,0.0)
      DO 10 K=1,NBG
         HVX(K,2)  = (0.0,0.0)
         HVX2(K,2) = (0.0,0.0)
   10 CONTINUE
         DO 20 J=1,NB3
         DO 20 L=1,NBG
            BUFF(L,J) = (0.0,0.0)
   20    CONTINUE
C
C
      DO 130 NI=NG,1,-1
C     ----------------
C
      IZAS = MOD(NI-1,NCV)+1
      IF(IZAS.EQ.NCV.OR.NI.EQ.NG) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         JHILF = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = JHILF
         IND3X = CPLGXA
*IF KUL
         IF(LREAD.NE.NREC) THEN
            WAIT(ND3,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND3 '
            WAIT(ND6,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND6 '
         ENDIF
         WAIT(ND5,ID=LREAD,COND=I1)
         IF(I1.NE.1)  STOP ' RIT/ND5 '
*ENDIF
*IF CRAY
         IF(LREAD.NE.NREC) THEN
            IF(UNIT(ND3).GE.0.0) STOP 'RIT/ND3 '
            IF(UNIT(ND6).GE.0.0) STOP 'RIT/ND6 '
         ENDIF
         IF(UNIT(ND5).GE.0.0) STOP 'RIT/ND5 '
*ENDIF
         IF(LREAD.GT.1) THEN
            LREAD = LREAD-1
            BACKSPACE ND3
            BACKSPACE ND3
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
*ENDIF
            BACKSPACE ND6
            BACKSPACE ND6
*IF KUL
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
            BACKSPACE ND5
            BACKSPACE ND5
*IF KUL
            READ(ND5,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND5) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND5,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
      ENDIF
C
      IF(NI.EQ.NG) GOTO 40
      DO 30 K1=1,NBG
         IPVK1 = IPVT(K1,IZAS,CPLGA)
         SUM = CXDOTU(NBG,APR(IPVK1,2*NBG+1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(IPVK1,IZAS,2,CPLGXA) = X(IPVK1,IZAS,2,CPLGXA)-HVX(K1,1)
   30 CONTINUE
   40 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),1)
      DO 50 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1,IZAS,CPLGA),1,X(1,IZAS,2,CPLGXA),1)
         HVX(K1,1) = SUM
   50 CONTINUE
      CALL CXCOPY(NBG,HVX(1,2),1,HVX(1,3),1)
      CALL CXCOPY(NBG,HVX2(1,2),1,HVX2(1,3),1)
      CALL CXCOPY(NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
      CALL CXCOPY(NBG,X(1,IZAS,2,CPLGXA),1,HVX2(1,2),1)
C
      INDEX = (NI-1)*NBG+1
      CALL CXCOPY(NBG,HVX(1,2),1,EV(INDEX),1)
C
C ... B*X ...
C
      IF(NI.EQ.NG) GOTO 100
      DO 60 L=1,NBG
         SUM = CXDOTU(NBG,BUFF(L,1),NBG,HVX(1,2),1)
         X(L,JZAS,1,IND3XA) = X(L,JZAS,1,IND3XA)+SUM
   60 CONTINUE
      YR = YR+CXDOTC(NBG,X(1,JZAS,2,IND3XA),1,X(1,JZAS,1,IND3XA),1)
      DO 70 L=1,NBG
         SUMY = CXDOTU(NBG,BUFF(L,1),NBG,HVX2(1,2),1)
         X(L,JZAS,2,IND3XA) = HVX2(L,1)+SUMY
   70 CONTINUE
      DO 80 J=1,NB3
      DO 80 L=1,NBG
          BUFF(L,J) = (0.0,0.0)
   80 CONTINUE
      DO 90 L=1,NZMA
         CALL CXCOPY(NBG,ZMA(1,L),1,BUFF(1,NBG+L),1)
   90 CONTINUE
  100 IF(NI.GT.1) THEN
         CALL CONBMAT(NI-1,NZMA,ZMA)
         DO 110 J=1,NZMA
         DO 110 L=1,NBG
            BUFF(L,J) = BUFF(L,J)+ZMA(NBG+L,J)
  110    CONTINUE
      ENDIF
      IF(IZAS.EQ.NCV.AND.NI.LT.NG) THEN
         LWRIT = LWRIT+1
*IF KUL
         IF(LWRIT.NE.1) THEN
            WAIT(ND4,ID=LWRIT-1,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND4 '
         ENDIF
         WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
         WRITE(ND4) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
         IF(LWRIT.NE.1) THEN
            IF(UNIT(ND4).GE.0.0) STOP 'RIT/ND4 '
         ENDIF
         BUFFER OUT (ND4,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
      ENDIF
      DO 120 L=1,NBG
         SUM  = CXDOTU(NBG,BUFF(L,NBG1),NBG,HVX(1,2),1)
     >          +CXDOTU(NBG,BUFF(L,2*NBG+1),NBG,HVX(1,3),1)
         SUMY = CXDOTU(NBG,BUFF(L,NBG1),NBG,HVX2(1,2),1)
     >          +CXDOTU(NBG,BUFF(L,2*NBG+1),NBG,HVX2(1,3),1)
         X(L,IZAS,1,CPLGXA) = SUM
         HVX2(L,1) = SUMY
  120 CONTINUE
C
      JZAS = IZAS
      IND3XA = IND3X
      INDA = CPLGA
C
  130 CONTINUE
C     --------
C
      YR = YR+CXDOTC(NBG,X(1,IZAS,2,CPLGXA),1,X(1,IZAS,1,CPLGXA),1)
      CALL CXCOPY(NBG,HVX2(1,1),1,X(1,IZAS,2,CPLGXA),1)
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND4,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' RIT/ND4 '
      ENDIF
      WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND4) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND4).GE.0.0) STOP 'RIT/ND4 '
      ENDIF
      BUFFER OUT(ND4,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND5
C
      RETURN
      END
************************************************************************
*DECK SOLV4
      SUBROUTINE SOLV4
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV4 : INVERSE VECTOR ITERATION,                               **
**            IN-CORE VERSION OF THE OUT-OF-CORE SOLVER               **
**            VERSION C,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV4                                              **
**                   ASLUIC                                           **
**                     CGESLP                                         **
**                       (CXDOTU)                                     **
**                     CGEFAP                                         **
**                       ICMAXP                                       **
**                   VITERIC                                          **
**                     (CXSCAL)                                       **
**                     (CXDOTU)                                       **
**                     CGES2P                                         **
**                       (CXCOPY)                                     **
**                     (CXDOTC)                                       **
**                     (CXCOPY)                                       **
**                   RITERIC                                          **
**                     (CXDOTU)                                       **
**                     CGES2P                                         **
**                       (CXCOPY)                                     **
**                     (CXDOTC)                                       **
**                     (CXCOPY)                                       **
**                     CONBMAT                                        **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR4
*CALL COMPIO
*CALL CORE4
*CALL COMIT
*CALL COMGRID
*CALL COMEQUI
C
      COMPLEX  QP, YR, DLI, DLIM1, EWT
C
      DATA QQ /1.E-4/
C
      DLI = QQ*EWSHIFT
C
C ... L-U-ZERLEGUNG VON APR ...
C
      CALL ASLUIC(NBG,NB3,NCVIC,NG,APR,IPVT,HVX)
C     -----------
C
C ... LOESUNG DES GLEICHUNGSSYSTEMS ...
C
      IT = 0
C
   10 IT = IT + 1
      IF(IT.GT.ITER) GOTO 20
      DLIM1 = DLI
C
      CALL VITERIC(NBG,NB3,NCVIC,NG,HVX,HVX2,QP,APR,IPVT,X,DLIM1)
C     ------------
C
      CALL RITERIC(NBG,NB3,NCVIC,NG,HVX,HVX2,YR,APR,IPVT,X,EV,ZMA,NZMA)
C     ------------
C
      DLI = QP/YR
      EW = EWSHIFT+DLI
      WRITE(NOUT,11) IT,EW,CABS(DLI/DLIM1)-1.
      WRITE(*,11) IT,EW,CABS(DLI/DLIM1)-1.
      IF(ABS(CABS(DLI/DLIM1)-1.0).LE.EPS ) GOTO 30
      GOTO 10
C
   20 WRITE(NOUT,21) IT-1
C
   30 CONTINUE
C
      RETURN
C
   11 FORMAT(1X,' IT : ',I2,' EIGENVALUE : ',1P,2E12.4,
     >       '   REL. CHANGE : ',E12.4)
   21 FORMAT(' STOPPED AFTER ',I4,' ITERATIONS')
      END
************************************************************************
*DECK ASLUIC
      SUBROUTINE ASLUIC(NBG,NB3,NCV,NG,APR,IPVT,HVX)
C-----------------------------------------------------------------------
C     L-U-ZERLEGUNG VON APR
C-----------------------------------------------------------------------
C
      INTEGER  IPVT(NBG,NCV)
      COMPLEX  SUM, ZZ, CXDOTU
      COMPLEX  APR(NBG,NB3,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      NBG2 = 2*NBG+1
C
      DO 50 I=1,NG
C
C ... A' = L*U ...
C ... L-U-ZERLEGUNG VON APR ...
C
         IF(I.EQ.1) GOTO 40
C
         CALL CGESLP(APR(1,NBG1,I-1),NBG,NBG,
     >               APR(1,1,I),IPVT(1,I-1),HVX,1)
C
         DO 30 K2=1,NBG
            K2A = 2*NBG+K2
            DO 10 KUS=1,NBG
               HVX(KUS,1) = APR(IPVT(KUS,I-1),K2A,I-1)
   10       CONTINUE
            DO 20 K1=1,NBG
               SUM = CXDOTU(NBG,APR(K1,1,I),NBG,HVX(1,1),1)
               ZZ = APR(K1,NBG+K2,I)-SUM
               APR(K1,NBG+K2,I) = ZZ
   20       CONTINUE
   30    CONTINUE
C
   40    CALL CGEFAP(APR(1,NBG1,I),NBG,NBG,IPVT(1,I),IER)
         IF(I.EQ.NG) GOTO 50
         CALL CGESLP(APR(1,NBG1,I),NBG,NBG,
     >               APR(1,NBG2,I),IPVT(1,I),HVX,0)
C
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK VITERIC
      SUBROUTINE VITERIC(NBG,NB3,NCV,NG,HVX,HVX2,QP,APR,
     >                   IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  IPVT(NBG,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,*), X(NBG,NCV,*), HVX(NBG,*), HVX2(NBG,*)
C
      NBG1 = NBG+1
      QP = (0.0,0.0)
C
      DO 50 I=1,NG
C
         CALL CXSCAL(NBG,DLIALT,X(1,I,1),1)
         CALL CXSCAL(NBG,CONJG(DLIALT),X(1,I,2),1)
C
         IF(I.EQ.1) GOTO 20
         DO 10 K1=1,NBG
            SUM = CXDOTU(NBG,APR(K1,1,I),NBG,HVX(1,2),1)
            X(K1,I,1) = X(K1,I,1)-SUM
            X(K1,I,2) = X(K1,I,2)-HVX(K1,1)
   10    CONTINUE
   20    CALL CGES2P(APR(1,NBG1,I),NBG,NBG,X(1,I,1),
     >               X(1,I,2),IPVT(1,I),HVX(1,3),0)
C
         DO 30 K1=1,NBG
            SUM = CXDOTC(NBG,APR(1,K1+2*NBG,I),1,X(1,I,2),1)
            HVX(K1,1) = SUM
   30    CONTINUE
C
C ... Q(*)*P ...
C
         SUM = (0.0,0.0)
         DO 40 J=1,NBG
            JPV = IPVT(J,I)
            SUM = SUM+CONJG(X(JPV,I,2))*X(J,I,1)
   40    CONTINUE
         QP = QP+SUM
         CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,2),1)
C
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK RITERIC
      SUBROUTINE RITERIC(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,
     >                   IPVT,X,EV,ZMA,NZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMINT
C
      INTEGER  IND1, IPVT(NBG,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, YR
      COMPLEX  EV(NBG,*)
      COMPLEX  ZMA(NZMA,*)
      COMPLEX  APR(NBG,NB3,*), X(NBG,NCV,*), HVX(NBG,*), HVX2(NBG,*)
C
      N1 = NBG+1
      NGINT = NG - 1
C
      YR=(0.0,0.0)
C
      DO 40 I=NG,1,-1
C     -----------------
C
      IF(I.EQ.NG) GOTO 20
      DO 10 K1=1,NBG
         IPVK1 = IPVT(K1,I)
         SUM = CXDOTU(NBG,APR(IPVK1,2*NBG+1,I),NBG,HVX(1,2),1)
         X(K1,I,1) = X(K1,I,1)-SUM
         X(IPVK1,I,2) = X(IPVK1,I,2)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,N1,I),NBG,NBG,X(1,I,1),
     >            X(1,I,2),IPVT(1,I),HVX(1,2),1)
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1,I),1,X(1,I,2),1)
         HVX(K1,1) = SUM
   30 CONTINUE
      CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,2),1)
      CALL CXCOPY(NBG,X(1,I,1),1,EV(1,I),1)
C
   40 CONTINUE
C     --------
C
      CALL CXCOPY(NBG,X(1,NG,1),1,HVX(1,1),1)
      CALL CXCOPY(NBG,X(1,NG,2),1,HVX(1,2),1)
C
      CALL CONBMAT(NGINT,NZMA,ZMA)
C
      DO 50 K=1,NBG
         X(K,NG,1) = CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
         X(K,NG,2) = CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   50 CONTINUE
C
      DO 90 I=NGINT,1,-1
C     ----------------------
C
      DO 60 K=1,NBG
         X(K,I+1,1) =X(K,I+1,1)+CXDOTU(NBG,ZMA(NBG+K,1),NZMA,X(1,I,1),1)
         X(K,I+1,2) =X(K,I+1,2)+CXDOTU(NBG,ZMA(NBG+K,1),NZMA,X(1,I,2),1)
   60 CONTINUE
C
      YR = YR+CXDOTC(NBG,HVX(1,2),1,X(1,I+1,1),1)
C
      CALL CXCOPY(NBG,HVX(1,1),1,HVX2(1,1),1)
      CALL CXCOPY(NBG,HVX(1,2),1,HVX2(1,2),1)
      CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,1),1)
      CALL CXCOPY(NBG,X(1,I,2),1,HVX(1,2),1)
C
      DO 70 K=1,NBG
         X(K,I,1) = CXDOTU(NBG,ZMA(K,1),NZMA,HVX(1,1),1)
     >            + CXDOTU(NBG,ZMA(K,N1),NZMA,HVX2(1,1),1)
         X(K,I,2) = CXDOTU(NBG,ZMA(K,1),NZMA,HVX(1,2),1)
     >            + CXDOTU(NBG,ZMA(K,N1),NZMA,HVX2(1,2),1)
   70 CONTINUE
C
      IF(I.GT.1) THEN
C
         CALL CONBMAT(I-1,NZMA,ZMA)
C
         DO 80 K=1,NBG
            X(K,I,1) = X(K,I,1) +
     >                 CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
            X(K,I,2) = X(K,I,2) +
     >                 CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   80    CONTINUE
C
      ENDIF
C
   90 CONTINUE
C     --------
C
      YR = YR+CXDOTC(NBG,HVX(1,2),1,X(1,1,1),1)
C
      RETURN
      END
************************************************************************
*DECK CGESLP
      SUBROUTINE CGESLP(A,LDA,N,B,IPVT,HVX,JOB)
C-----------------------------------------------------------------------
C
C     CGESL SOLVES THE COMPLEX SYSTEM
C     L * X = B  OR  CTRANS(U) * X = B  (A=L*U)
C     USING THE FACTORS COMPUTED BY CGECO OR CGEFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CGECO OR CGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        B       COMPLEX(LDA,N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        IPVT    INTEGER(N)
C                PIVOT VECTOR
C
C        JOB     INTEGER
C                = 0         TO SOLVE  L*X = B ,
C                = NONZERO   TO SOLVE  X*U = B
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF CGECO HAS SET RCOND .GT. 0.0
C        OR CGEFA HAS SET INFO .EQ. 0 .
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, JOB
      INTEGER  IPVT(*)
      INTEGER  K, KB, NM1
      COMPLEX  A(LDA,*), B(LDA,*), HVX(*)
      COMPLEX  CXDOTU, T
C
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 60
C
C ... JOB = 0 , SOLVE  L * X = B ...
C
      IF(NM1.LT.1) GOTO 100
      DO 50 J=1,N
         DO 10 II=1,N
            HVX(II) = B(IPVT(II),J)
   10    CONTINUE
         DO 30 K=1,NM1
            T = -HVX(K)
            DO 20 I=1,N-K
               KPI = K+I
               HVX(KPI) = HVX(KPI)+T*A(IPVT(KPI),K)
   20       CONTINUE
   30    CONTINUE
         DO 40 II=1,N
            B(IPVT(II),J) = HVX(II)
   40    CONTINUE
   50 CONTINUE
C
      GOTO 100
   60 CONTINUE
C
C ... JOB = NONZERO, SOLVE  X * U = B ...
C
      DO 90 J=1,N
         B(J,1) = B(J,1)/A(IPVT(1),1)
         DO 80 K=2,N
            DO 70 I=1,K-1
              HVX(I) = A(IPVT(I),K)
   70       CONTINUE
            T = CXDOTU(K-1,HVX,1,B(J,1),LDA)
            B(J,K) = (B(J,K)-T)/A(IPVT(K),K)
   80    CONTINUE
   90 CONTINUE
C
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGEFAP
      SUBROUTINE CGEFAP(A,LDA,N,IPVT,INFO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, INFO, INC, IPVT(*)
      INTEGER  ICMAXP, I, J, K, KP1, L
      REAL     CABS1
      COMPLEX  A(LDA,*)
      COMPLEX  T, ZDUM
C
      DATA INC/1/
C
      CABS1(ZDUM) = ABS(REAL(ZDUM))+ABS(AIMAG(ZDUM))
      INFO = 0
      DO 10 I=1,N
         IPVT(I) = I
   10 CONTINUE
C
      DO 50 K=1,N
         KP1 = K+1
         L = ICMAXP(N-K+1,A(1,K),IPVT(K),INC)+K-1
         IF(L.NE.K) THEN
            I = IPVT(K)
            IPVT(K) = IPVT(L)
            IPVT(L) = I
         ENDIF
         L  = IPVT(K)
         IF(CABS1(A(L,K)).EQ.0.0E0) THEN
            INFO = K
            GOTO 50
         ENDIF
         T = (1.0,0.0)/A(L,K)
         DO 20 J=KP1,N
            A(IPVT(J),K) = T*A(IPVT(J),K)
   20    CONTINUE
         DO 40 J=KP1,N
            T = A(L,J)
            DO 30 I=KP1,N
               A(IPVT(I),J) = A(IPVT(I),J)-T*A(IPVT(I),K)
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ICMAXP
      INTEGER FUNCTION ICMAXP(L,Q,IP,INC)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  IP(*)
      COMPLEX  Q(*)
C
      ICMAXP = 1
      IPVM = IP(ICMAXP)
      RMAX = ABS(REAL(Q(IPVM)))+ABS(AIMAG(Q(IPVM)))
      IF(L.LE.1) RETURN
C
      DO 10 I=INC+1,L,INC
         IF(ABS(REAL(Q(IP(I))))+ABS(AIMAG(Q(IP(I)))).LE.RMAX) GOTO 10
         ICMAXP = I
         IPVM = IP(I)
         RMAX = ABS(REAL(Q(IPVM)))+ABS(AIMAG(Q(IPVM)))
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGES2P
      SUBROUTINE CGES2P(A,LDA,N,R,S,IPVT,HVX,JOB)
C-----------------------------------------------------------------------
C
C     CGESL2 SOLVES THE COMPLEX SYSTEM
C     L * P = R AND U(*) * Q = S  OR  U * X = R AND L(*) * Y = S
C     USING THE FACTORS COMPUTED BY CGECO OR CGEFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CGECO OR CGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        R,S     COMPLEX(LDA)
C                THE RIGHT HAND SIDE VECTOR.
C
C        IPVT    INTEGER(LDA)
C                PIVOT VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  L*P = R , U(*)*Q = S
C                = NONZERO   TO SOLVE  U*X = R , L(*)*Y = S
C
C     ON RETURN
C
C        R,S     THE SOLUTION VECTOR  P,Q OR X,Y
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, JOB, IPVT(*)
      INTEGER  K, KB, NM1
      COMPLEX  A(LDA,*), R(*), S(*), HVX(*)
      COMPLEX  T
C
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 50
C     ---------------------
C
C ... JOB = 0 , SOLVE  L * P = R  AND  U(*) * Q = S ...
C
      IF(NM1.LT.1) GOTO 100
      DO 10 I=1,N
         HVX(I) = R(IPVT(I))
   10 CONTINUE
      DO 20 K=1,NM1
         T = -HVX(K)
         DO 20 I=1,N-K
            KPI = K+I
            HVX(KPI) = HVX(KPI)+T*A(IPVT(KPI),K)
   20 CONTINUE
      CALL CXCOPY(N,HVX,1,R,1)
C
      HVX(IPVT(1)) = S(1)
      IF(A(IPVT(1),1).NE.(0.,0.)) HVX(IPVT(1))=S(1)/CONJG(A(IPVT(1),1))
      DO 40 K=2,N
         IPK = IPVT(K)
         IF(A(IPK,K).EQ.(0.0,0.0)) GOTO 40
         T = (0.0,0.0)
         DO 30 I=1,K-1
            IPI = IPVT(I)
            T = T+CONJG(A(IPI,K))*HVX(IPI)
   30    CONTINUE
         HVX(IPK) = (S(K)-T)/CONJG(A(IPK,K))
   40 CONTINUE
      CALL CXCOPY(N,HVX,1,S,1)
C
      GOTO 100
   50 CONTINUE
C     --------
C
C ... JOB = NONZERO, SOLVE  U * X = R  AND  L(*) * Y = S ...
C
      IF(A(IPVT(N),N).NE.(0.0,0.0)) R(N) = R(N)/A(IPVT(N),N)
      DO 70 K=NM1,1,-1
         IPK = IPVT(K)
         IF(A(IPK,K).EQ.(0.0,0.0)) GOTO 70
         T = (0.0,0.0)
         DO 60 I=K+1,N
            T = T+A(IPK,I)*R(I)
   60    CONTINUE
         R(K) = (R(K)-T)/A(IPK,K)
   70 CONTINUE
C
      DO 90 K=N-1,1,-1
         IPK = IPVT(K)
         T = (0.0,0.0)
         DO 80 I=K+1,N
            IPI = IPVT(I)
            T = T+CONJG(A(IPI,K))*S(IPI)
   80    CONTINUE
         S(IPK) = S(IPK)-T
   90 CONTINUE
C
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SOLV5
      SUBROUTINE SOLV5
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV5 : LANCZOS SOLVER BY JANE CULLUM                           **
**            MODIFIED BY J. STEUERWALD                               **
**            MODIFIED BY ELISABETH SCHWARZ                           **
**            VERSION C, 16.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV5                                              **
**                   (RANSET)                                         **
**                   ABSHIF                                           **
**                     CGBFA                                          **
**                   LANCZS                                           **
**                     BMATV                                          **
**                     ABSOLV                                         **
**                       CGBSLL                                       **
**                   TNORM                                            **
**                   COMPEV                                           **
**                     CMTQL1                                         **
**                     SINVER                                         **
**                   LUMP                                             **
**                   COMGAP                                           **
**                   ISOEV                                            **
**                   INVERR                                           **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL COMPIO
*CALL COMPCON
*CALL CORE5
*CALL COMDIM
*CALL MACHT
*CALL SHIFT
*CALL LANTOL
*CALL COMDIM5
*CALL LANCZOS
*CALL CONLAN
*CALL ISEED
C
      INTEGER  SVSOLD
C
      REAL     ERRTOL, CLSTOL, CR, GTEMP
      REAL     TEMPX, TEMPY
      REAL     SEED, TEMP4, TEMP5, TEMP6, TEMP7
      REAL     BTOL, GAPTOL, EPSM, TSCALE
      REAL     SCALE1, SCALE2, SPUTOL, MULTOL, MSPUTO
      REAL     TEMP, TKMAX, EVMAX, BKMIN, T0, TEMP2
C
      COMPLEX  ALPHA(MXAB), BETA(MXAB+1)
      COMPLEX  SSIGMA(MXLP)
      COMPLEX  BETAM, Z, LAMBDS, LAMBDA, RESIDX, RESIDY
      COMPLEX   HVVS, HVWS, HCONEV
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
*IF CRAY
      MACHEP = SMACH(1)*.3136E-1
*ELSE
      MACHEP = 0.5*X02AJF(0.0)*.3136E-1
*ENDIF
      EPSM = 2.0E0*MACHEP
      SCALE1 = 5.0E2
      SCALE2 = 5.0E0
      BTOL = 0.E0
      GAPTOL = 1.0E-7
      CLSTOL = 1.E-4
      ERRTOL = 1.E-4
      LTOL = 1.E6
      LST = 5
      MOLD = 0
      MOLD1 = 1
      NLOOP = 0
      NCONV = 0
C
      IF(SAVTEV.LT.0.AND.ISTART.EQ.0) THEN
         WRITE(NOUT,1) SAVTEV,ISTART
         STOP
      ENDIF
      LS = NUS
      LC = 0
C
      IF(ISHIFT.NE.1) THEN
         AMY   = AMAX1(ZERO,YLIMB)
         DX    = (AMIN1(ZERO,XLIMR) - XLIML)/4.0
         DY    = (YLIMT - AMY)/4.0
         OWS(1) = CMPLX(XLIML+2.*DX,AMY+2.*DY)
         OWS(2) = CMPLX(XLIML+DX,AMY+DY)
         OWS(3) = CMPLX(XLIML+3.*DX,AMY+3.*DY)
         OWS(4) = CMPLX(XLIML+DX,AMY+3.*DY)
         OWS(5) = CMPLX(XLIML+3.*DX,AMY+DY)
         LS = 5
         LC = 0
         WRITE(NOUT,3) (OWS(L), L=1,LS)
         DO 20 J=1,LS
            IF(OWS(J).EQ.SIGMA) THEN
               IF(J.LT.LS) THEN
                  DO 10 K=J+1,LS
                     OWS(K-1) = OWS(K)
   10             CONTINUE
               ENDIF
               LS = LS -1
            ENDIF
   20    CONTINUE
      ELSE
         LS = NUS
         WRITE(NOUT,21) (J,OWS(J),J=1,NUS)
      ENDIF
C
      LSO = LS
C
      IF(ISTART.EQ.0) GOTO 100
C
      READ(NIN2,23) MOLD,NOLD,SVSOLD
      IF(KMAX.LT.MOLD) KMAX = MOLD
      KMAX1 = KMAX + 1
      ITEMP = (NOLD-NDIM)**2+(SVSEED-SVSOLD)**2
C
      IF(ITEMP.NE.0) THEN
         WRITE(NOUT,25) NOLD,NDIM,SVSEED,SVSOLD
         STOP
      ENDIF
      MOLD1 = MOLD+1
      READ(NIN2,27) (ALPHA(J),J=1,MOLD)
      READ(NIN2,27) (BETA(J),J=1,MOLD1)
      IF(KMAX.EQ.MOLD) GOTO 110
C
C----------------------------------------------------------------------
C
  100 CALL ABSHIF(SIGMA,AA,BB,AORIG,IPVT,LDAA,LDAL,LDBL,NDIM)
C     -----------
      CALL LANCZS(V1,VS,V2,W1,WS,W2,ALPHA,BETA,GR,GC,
     >            KMAX,MOLD1,AA,BB,IPVT,LDAA,LDBL,NDIM)
C     -----------
      IF(GR(1).EQ.-1.E3) THEN
         LC = LC+1
         IF(LC.GT.LS) THEN
            WRITE(NOUT,101) NLOOP
            GOTO 600
         ELSE
            GOTO 510
         ENDIF
      ENDIF
      NLOOP = NLOOP + 1
       LSO = LS
      SSIGMA(NLOOP) = SIGMA
      KMAX1 = KMAX + 1
      IF(ISTOP.LE.0) THEN
         WRITE(NOUT2,103) KMAX,NDIM,SVSEED,SIGMA
         WRITE(NOUT2,105) (ALPHA(I), I=1,KMAX)
         WRITE(NOUT2,105) (BETA(I), I=1,KMAX1)
         IF(ISTOP.EQ.0) THEN
            WRITE(NOUT,107)
            IF(KMAX.NE.MOLD) WRITE(NOUT2,109)
            STOP
         ENDIF
      ENDIF
  110 BKMIN = BTOL
C     WRITE(NOUT,111)
      CALL TNORM(ALPHA,BETA,BKMIN,TKMAX,KMAX,IB)
C     ----------
      TSCALE = TKMAX
      MEV = KMAX
      MP1 = MEV + 1
      BETAM = BETA(MP1)
      IF(IB.LT.0) THEN
         T0 = BTOL
         CALL TNORM(ALPHA,BETA,T0,TSCALE,MEV,IBMEV)
C        ----------
         TEMP = T0/TKMAX
         IBMEV = IABS(IBMEV)
         IF(TEMP.LT.BTOL) THEN
            TEMP = CABS(BETA(IBMEV))
            WRITE(NOUT,113) MEV,IBMEV,TEMP
            STOP
         ENDIF
      ENDIF
  120 CONTINUE
      MULTOL = 500.E0 * FLOAT(MEV+1000) * EPSM
      SPUTOL =  MULTOL
      KT = 1
      V1(KT) = CMPLX(TSCALE,ZERO)
      CALL COMPEV(ALPHA,BETA,V1,V2,VS,GR,GC,
     >            MULTOL,SPUTOL,EPSM,MP,MP2,MEV,NDIS,SAVTEV)
C     -----------
      IF(NDIS.EQ.0) THEN
         WRITE(NOUT,121)
         STOP
      ENDIF
      LOOP = NDIS
      MSPUTO = 500.E0*SPUTOL
      CALL LUMP(VS,V1,GR,RELTOL,MSPUTO,SCALE2,MP,MP2,LOOP)
C     ---------
      IF(LOOP.LT.0) STOP
      IF(NDIS.NE.LOOP) WRITE(NOUT,123) NDIS,LOOP,MEV
      NDIS = LOOP
      ITAG = 1
      CALL COMGAP(VS,GR,GG,MP,MP2,NDIS,ITAG)
C     -----------
       BETA(MP1) = BETAM
      CALL ISOEV(VS,GR,GG,GAPTOL,SPUTOL,SCALE1,MP,NDIS,NNG,NISO)
C     ----------
      WRITE(NOUT,125) NNG,NISO,NDIS
      IF(NISO.NE.0) THEN
         IT = MXINIT
         CALL INVERR(ALPHA,BETA,V1,V2,VS,EPSM,GR,GC,G,GG,
     >               MP,MP2,MEV,NDIS,NISO,IT)
C        -----------
         DO 130 I=1,NISO
  130       WS(I) = ONE/V2(I)+SIGMA
      ELSE
         WRITE(NOUT,131)
      ENDIF
      T0 = CABS(BETAM)
C     WRITE(NOUT,133)
C     WRITE(NOUT,135) (J,VS(J),G(J),GR(J),J=1,NISO)
      DO 140 I=1,NDIS
         WS(I) = (ONE/VS(I)) + SIGMA
  140 CONTINUE
      DO 150 I=1,NDIS
         GC(I) = ZERO
  150 CONTINUE
      L=0
      IC = 0
      DO 160 I=1,NDIS
         IF(MP(I).EQ.0) THEN
            IC = IC + 1
         ELSE
            IF(MP(I).EQ.1) THEN
               L = L+1
               GC(I) = ABS(G(L))/T0
            ENDIF
         ENDIF
  160 CONTINUE
      DO 190 K=2,NDIS
         HVGC = GC(K)
         IHV = K
         DO 170 J=K-1,1,-1
            IF(HVGC.LT.GC(J)) IHV = J
  170    CONTINUE
         IF(IHV.NE.K) THEN
            HVVS = VS(K)
            HVWS = WS(K)
            IHVMP = MP(K)
            DO 180 I=K,IHV+1,-1
               GC(I) = GC(I-1)
               VS(I) = VS(I-1)
               WS(I) = WS(I-1)
               MP(I) = MP(I-1)
  180       CONTINUE
            GC(IHV) = HVGC
            VS(IHV) = HVVS
            WS(IHV) = HVWS
            MP(IHV) = IHVMP
         ENDIF
  190 CONTINUE
      L = 0
      DO 200 J=1,NDIS
         IF(MP(J).NE.0) THEN
            L = L+1
            CONEVB(L) = WS(J)
            MULEVB(L) = MP(J)
            ERREVB(L) = GC(J)
            MP2(L) = J
         ENDIF
  200 CONTINUE
      NEVG = L
      WRITE(NOUT,201) NEVG,NLOOP,MEV
      WRITE(NOUT,203) SIGMA, T0
C     WRITE(NOUT,205)
C     WRITE(NOUT,207)
C     WRITE(NOUT,209) (J,CONEVB(J),MULEVB(J),ERREVB(J),J=1,NEVG)
C
      DO 210 I=1,NEVG
         IF(ABS(ERREVB(I)).GT.ERRTOL) GOTO 220
  210 CONTINUE
  220 NCONVB = I-1
C
      IF(NCONVB.LE.1) GOTO 250
C
      DO  240 K=1,NCONVB-1
         IF(MULEVB(K).EQ.0) GOTO 240
         K1 = K+1
         DO 230 J=K1,NCONVB
            IF((MULEVB(J).EQ.0).OR.
     >      (CABS(CONEVB(K) - CONEVB(J))/CABS(CONEVB(K)).GT.CLSTOL))
     >         GOTO 230
            IF(ERREVB(J).GT.ERREVB(K)) THEN
               MULEVB(J) = 0
               MP(MP2(J)) = 0
            ELSE
               MULEVB(K) = 0
               MP(MP2(K)) = 0
            ENDIF
  230    CONTINUE
  240 CONTINUE
C
  250 WRITE(NOUT,251) NCONVB,NLOOP,MEV
      IF(NCONVB.EQ.0) GOTO 300
C     WRITE(NOUT,253)
C     WRITE(NOUT,207)
C     WRITE(NOUT,209) (J,CONEVB(J),MULEVB(J),ERREVB(J),J=1,NCONVB)
      IF(NLOOP.EQ.1.OR.NCONV.EQ.0) GOTO 280
      DO 270 K=1,NCONVB
         IF(MULEVB(K).EQ.0) GOTO 270
         ICLOSE = 1
         LAMBDS = CONEVB(K)
         IF(NCONV.NE.1) THEN
            DO 260 J=2,NCONV
               IF(MULEV(J).EQ.0) GOTO 260
               IF(CABS(LAMBDS-CONEV(J)).LT.CABS(LAMBDS-CONEV(ICLOSE)))
     >                              ICLOSE = J
  260       CONTINUE
         ENDIF
         IF(CABS(LAMBDS-CONEV(ICLOSE))/CABS(LAMBDS).GT.CLSTOL)
     >      GOTO 270
         IF(ERREV(ICLOSE).GT.ERREVB(K)) THEN
            CONEV(ICLOSE) = CONEVB(K)
            ERREV(ICLOSE) = ERREVB(K)
            MULEV(ICLOSE) = MULEVB(K)
            ISIGMA(ICLOSE) = NLOOP
         ENDIF
         MULEVB(K) = 0
         MP(MP2(K)) = 0
  270 CONTINUE
  280 L = NCONV
      ICOUNT = 0
      DO 290 I=1,NCONVB
         IF(MULEVB(I).NE.0) THEN
            L = L+1
            ICOUNT = ICOUNT + 1
            CONEV(L) = CONEVB(I)
            MULEV(L) = MULEVB(I)
            ERREV(L) = ERREVB(I)
            ISIGMA(L) = NLOOP
            ILOOP(L) = NLOOP
         ENDIF
  290 CONTINUE
      NCONV = L
      BETA(MP1) = BETAM
C
       IF(ISHIFT.EQ.1) GOTO 500
C
  300 IF(NCONVB.EQ.NEVG) WRITE(NOUT,301) NLOOP
      ITEMP =0
      LS = 0
      DO 320 I=NDIS,1,-1
         IF(MP(I).EQ.0) GOTO 320
         IF(GC(I).GT.ERRTOL) THEN
            AIT  = AIMAG(WS(I))
            AAIT = ABS(AIT)
            RT   = REAL(WS(I))
            IF(AAIT.LT.ABS(YLIMB).OR.AAIT.GT.YLIMT) GOTO 320
            IF(RT.LT.XLIML.OR.RT.GT.XLIMR) GOTO 320
            IF(IHOLE) THEN
               IF(AIT.GE.YHOLEB.AND.AIT.LE.YHOLET) GOTO 320
               IF(RT.GE.XHOLEL.AND.RT.LE.XHOLER) GOTO 320
            ENDIF
            IF(LS.NE.0) THEN
               DO 310 K=1,LS
                  IF(CABS(WS(I)-OWS(K))/CABS(WS(I)).LT.1.E-2) GOTO 320
  310          CONTINUE
            ENDIF
            LS = LS + 1
            OWS(LS) = WS(I)
         ENDIF
  320 CONTINUE
C
      IF(LS.LT.LST) THEN
         IF(LC.EQ.LSO) THEN
            WRITE(NOUT,321) NLOOP
            GOTO 600
         ENDIF
         LC1 = LC + 1
         DO 330 K=LC1,LSO
            LS = LS + 1
            OWS(LS) = OWS(K)
  330    CONTINUE
      ENDIF
      WRITE(NOUT,331) (OWS(K),K=1,LS)
      LC = 0
C----------------------------------------------------------------------
  500 LC = LC + 1
      IF(LC.GT.LS) GOTO 600
  510 SIGMA = OWS(LC)
      DO 520 II=1,NLOOP
         IF(CABS(SIGMA - SSIGMA(II))/CABS(SIGMA).LE.1.E-2) GOTO 500
  520 CONTINUE
      SIGMA = CMPLX(REAL(SIGMA),ABS(AIMAG(SIGMA)))
      WRITE(NOUT,521) NLOOP,SIGMA
      IF(NLOOP.LE.MXLOOP) GOTO 100
      WRITE(NOUT,523) NLOOP,MXLOOP
C----------------------------------------------------------------------
  600 CONTINUE
      DO 610 I=1,NCONV
         GR(I) = CABS(CONEV(I))
  610 CONTINUE
      DO 640 K=2,NCONV
         HVGR = GR(K)
         IHV = K
         DO 620 J=K-1,1,-1
            IF(HVGR.LT.GR(J)) IHV = J
  620    CONTINUE
         IF(IHV.NE.K) THEN
            HCONEV = CONEV(K)
            HERREV = ERREV(K)
            IHSIGM = ISIGMA(K)
            IHLOOP = ILOOP(K)
            IHMULV = MULEV(K)
            DO 630 I=K,IHV+1,-1
               GR(I) = GR(I-1)
               CONEV(I) = CONEV(I-1)
               ERREV(I) = ERREV(I-1)
               ISIGMA(I) = ISIGMA(I-1)
               ILOOP(I) = ILOOP(I-1)
               MULEV(I) = MULEV(I-1)
  630       CONTINUE
            GR(IHV) = HVGR
            CONEV(IHV) = HCONEV
            ERREV(IHV) = HERREV
            ISIGMA(IHV) = IHSIGM
            ILOOP(IHV) = IHLOOP
            MULEV(IHV) = IHMULV
         ENDIF
  640 CONTINUE
C
      IF(NLOOP.GT.MXLOOP) NLOOP = NLOOP - 1
C
      ITAG = 0
      CALL COMGAP(CONEV,GR,GG,MULEV,MP2,NCONV,ITAG)
C     -----------
      WRITE(NOUT,641) NCONV,NDIM,MEV,SVSEED
      WRITE(NOUT,642) SIGMA,ERRTOL
      WRITE(NOUT,643)
      WRITE(NOUT,644) (I,CONEV(I),MULEV(I),ERREV(I),GG(I),ISIGMA(I),
     >              ILOOP(I),I=1,NCONV)
      WRITE(NOUT,645) NDIM
      WRITE(NOUT,646) (J,SSIGMA(J),J=1,NLOOP)
C
      WRITE(NOUT,647)
      IF(ISTOP.EQ.0) THEN
         WRITE(NOUT,107)
         IF(KMAX.NE.MOLD) WRITE(NOUT2,109)
      ELSE
         WRITE(NOUT,648)
      ENDIF
      RETURN
C
    1 FORMAT(2I6,' = SAVTEV,ISTART'/' WHEN SAVTEV = -1, WE MUST',
     >       ' HAVE ISTART = 1'/)
    3 FORMAT(/' SHIFTS SPECIFIED INTIALLY ='/1P,2(2E20.12))
   21 FORMAT(/' USER SUPPLIED SHIFTS TO BE CONSIDERED'
     >       / (I6,1P,2E15.6,0P))
   23 FORMAT(2I6,I12)
   25 FORMAT(' PROGRAM TERMINATES'/    ' READ FROM FILE 2',
     >       ' CORRESPONDS TO DIFFERENT MATRIX THAN MATRIX SPECIFIED'/
     >       ' NOLD, NDIM =', 2I10/' SVSEED AND SVSOLD =', 2I14/)
   27 FORMAT(4Z20)
  101 FORMAT(/' PROGRAM TRIES TO GET NEW SHIFT BUT NO MORE SHIFTS',
     >       'ARE AVAILABLE'/ ' PROGRAM TERMINATES FOR USER TO SUPPLY',
     >       ' SHIFT'/' NLOOP =',I6/)
  103 FORMAT(2I6,I12,1P,2E9.2,' KMAX,NDIM,SVSEED,SIGMA')
  105 FORMAT(4Z20)
  107 FORMAT(/' T-MATRICES (ALPHA AND BETA) ARE NOW AVAILABLE,',
     >       ' TERMINATE')
  109 FORMAT(/' ABOVE ARE THE FOLLOWING VECTORS '/
     >       '  ALPHA(I), I = 1,KMAX'/
     >       '  BETA(I), I = 1,KMAX+1'/
     >       ' FINAL LANCZOS VECTORS OF ORDER N V1,VS,V2,W1,WS,W2'/
     >       ' ALPHA BETA ARE IN HEX FORMAT 4Z20 '/
     >       ' LANCZOS VECTORS ARE IN HEX FORMAT 4Z20 '/
     >       ' ----- END OF FILE 1 NEW ALPHA, BETA HISTORY---------'///)
  111 FORMAT(/' T-MATRICES (ALPHA AND BETA) ARE NOW AVAILABLE'/)
  113 FORMAT(/' PROGRAM TERMINATES BECAUSE MEV REQUESTED = ',I6,
     >       ' IS .GT.',I6/' AT WHICH AN ABNORMALLY SMALL BETA = ' ,
     >       1P,E13.4,' OCCURRED'/)
  121 FORMAT(/' INTERVALS SPECIFIED FOR BISECT DID NOT CONTAIN',
     >       ' ANY T-EIGENVALUES'/' PROGRAM TERMINATES')
  123 FORMAT(' AFTER LUMP NDIS, LOOP, MEV =',3I6)
  125 FORMAT(/I6,' GOOD T-EIGENVALUES HAVE BEEN COMPUTED'/
     >       I6,' OF THESE ARE ISOLATED'/
     >       I6,' = NUMBER OF DISTINCT T-EIGENVALUES COMPUTED'/)
  131 FORMAT(/' ALL COMPUTED GOOD T-EIGENVALUES ARE T-MULTIPLE'/
     >       ' THEREFORE THESE EIGENVALUES ARE ASSUMED TO HAVE',
     >       ' CONVERGED')
  133 FORMAT(/' COMPARE WITH ESTIMATES WRITTEN FROM INVERR'/)
  135 FORMAT(I10,1P,4E14.3)
  201 FORMAT(' NEVG, NLOOP, MEV, EQUAL,EVALS GEN'/ (2I4, I6))
  203 FORMAT(1P,2E20.12, E13.4,' SIGMA, BETAM')
  205 FORMAT(' RUNNING LIST OF GOODEV OBTAINED FOR EACH SHIFT USED')
  207 FORMAT(' J, CONEVB(J), MULEVB(J), ERREVB(J) EQUAL ')
  209 FORMAT(I4,1P, 2E20.12,0P,I4,1P,E13.4,0P)
  251 FORMAT(' NCONVB,NLOOP, MEV EQUAL,EVALS GEN'/ (2I4,I6))
  253 FORMAT(' RUNNING LIST OF EIGENVALUES THAT GOTO KERINV')
  301 FORMAT(' ALL EIGENVALUE APPROXIMATIONS OBTAINED WITH',
     >       ' CURRENT SHIFT'/' HAVE CONVERGED WEAKLY'/' THEREFORE',
     >       ' PROGRAM WILL TRY TO USE SHIFTS FROM PREVIOUS LIST'/,
     >       ' NLOOP=',I5)
  321 FORMAT(' TRIED TO PICK EIGENVALUE IN BOX AS POSSIBLE',
     >       ' NEW SHIFT'/  ' BUT WAS UNSUCCESSFUL AND THERE WERE NO',
     >       ' POSSIBLE SHIFTS LEFT FROM THE PREVIOUS ITERATION.',
     >       ' PROCEDURE TERMINATES FOR USER TO SUPPLY SHIFTS'/,
     >       ' NLOOP=',I5)
  331 FORMAT(' POSSIBLE SHIFTS CHOSEN EQUAL'/3(1P,2E13.5,0P))
  521 FORMAT(' ON',I4,' LOOP, NEW SHIFT CHOSEN EQUALS',1P,2E20.12)
  523 FORMAT(' NLOOP =',I6,' EXCEEDS MXLOOP =',I6,
     >       'PROGRAM TERMINATES')
  641 FORMAT(' NCONV, NDIM, MEV, SVSEED'/ 3I6,I12)
  642 FORMAT( 1P,2E20.12,E13.4, ' SIGMA AND ERRTOL')
  643 FORMAT(' OVERALL LIST OF CONVERGED GOODEV OBTAINED FROM ALL',
     >       ' SIGMAS'/
     >       ' EVNO',7X,'R(GOODEV)',8X,'I(GOODEV)',2X,'MUL',
     >       1X,'ERR',3X,'AMGP',3X,'ISIG',1X,'FSIG')
  644 FORMAT(I4,1P,2E20.12,0P,I4,1P,E13.4,E11.2,0P,I4,I4)
  645 FORMAT(/' KERNER   NDIM =',I8)
  646 FORMAT(/' SHIFTS USED' / (I6, 1P,2E20.12,0P))
  647 FORMAT(' LANCZOS EIGENVALUE COMPUTATION COMPLETE, NOW COMPUTE',
     >       ' EIGENVECTORS'/)
  648 FORMAT(/' ABOVE ARE COMPUTED GOOD T-EIGENVALUES'/
     >       ' NG = NUMBER OF GOOD T-EIGENVALUES COMPUTED'/
     >       ' NDIS = NUMBER OF COMPUTED DISTINCT EIGENVALUES OF',
     >       ' T(1,MEV)'/
     >       ' NDIM = ORDER OF A,  MATNO = MATRIX IDENT'/
     >       ' MULTOL = T-MULTIPLICITY TOLERANCE FOR T-EIGENVALUES'/
     >       ' SPUTOL = SPURIOUS TOLERANCE FOR T-EIGENVALUES'/
     >       ' MULT IS THE T-MULTIPLICITY OF GOOD T-EIGENVALUE'/
     >       ' MULT = -1 MEANS SPURIOUS T-EIGENVALUE TOO CLOSE'/
     >       ' DO NOT COMPUTE ERROR ESTIMATES FOR SUCH T-EIGENVALUES'/
     >       ' AMINGAP= MINIMAL GAP BETWEEN THE COMPUTED A-EIGENVALUES'/
     >       ' TMINGAP= MINIMAL GAP W.R.T.  DISTINCT EIGENVALUES IN',
     >       '  T(1,MEV)'/
     >       ' ----- END OF FILE 3 GOOD T-EIGENVALUES--------------'///)
      END
************************************************************************
*DECK ABSHIF
      SUBROUTINE ABSHIF(EW,AAMAT,BMAT,AORIG,IPVT,LDAA,LDAL,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMDIM5
*CALL COMPCON
C
      INTEGER  NDIM, LDAA, LDAL, LDBL, LDB1, MUN1, MUU, J, KF, KL
      INTEGER  KA, L, INFO, KS, KB, IPVT(*)
      REAL     BMAT(LDBL,*), AORIG(LDAL,*)
      REAL     SR, SI
      COMPLEX  AAMAT(LDAA,*)
      COMPLEX  EW
C
      LDB1 = LDBL+1
      MUN1 = MUL + NDIM + 1
      MUU = 2*(MUL+1)
      SR = REAL(EW)
      SI = AIMAG(EW)
      CALL CXSCAL(LDAA*NDIM,ZEROC,AAMAT,1)
      DO 20 J=1,NDIM
         KF = MAX0(1,LDB1-J)
         KL = MIN0(LDAL,MUN1-J)
         DO 10 KA=KF,KL
            KS = KA + MLL
            IF(KA.LE.LDBL) THEN
               KB = KA
               L = J
            ELSE
               KB = MUU - KA
               L = J + KA - LDBL
            ENDIF
            AAMAT(KS,J)=CMPLX((AORIG(KA,J)-SR*BMAT(KB,L)),
     >                  -SI*BMAT(KB,L))
   10    CONTINUE
   20 CONTINUE
      CALL CGBFA(AAMAT,LDAA,NDIM,MLL,MUL,IPVT,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,21) INFO
         STOP
      ENDIF
      RETURN
C
   21 FORMAT(' PROGRAM TERMINATES BECAUSE ON RETURN FROM',
     >       ' FACTORIZATION INFO =',I7)
      END
************************************************************************
*DECK LANCZS
      SUBROUTINE LANCZS(V1,VS,V2,W1,WS,W2,ALPHA,BETA,GR,GC,KMAX,
     >                  MOLD1,AA,BB,IPVT,LDAA,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL LANTOL
*CALL ISEED
*CALL COMPCON
C
      INTEGER  IPVT(*), LDAA, LDBL,NDIM,ISEED,KMAX,MOLD1,K,IVEC,ISOLV,IN
      REAL     GR(*), GC(*)
      REAL     BB(LDBL,*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*), W1(*),W2(*),WS(*)
      COMPLEX  SUMC, TEMPC, SUMCT, ALFA, BATA, CXDOTU
      COMPLEX  AA(LDAA,*)
C
      IF(MOLD1.GT.1) GOTO 60
C
      BETA(1) = ZEROC
*IF CRAY
      DO 10 K=1,NDIM
         GR(K) = RANF( )
   10 CONTINUE
      DO 20 K=1,NDIM
         GC(K) = RANF( )
   20 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED= 1.0
      CALL DURAND(RSEED,NDIM,GR)
      CALL DURAND(RSEED,NDIM,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(NDIM,GR)
C      CALL DRNUN(NDIM,GC)
*ENDIF
      DO 30 K=1,NDIM
         V2(K) = CMPLX(GR(K),GC(K))
   30 CONTINUE
      CALL BMATV(V2,VS,BB,LDBL,NDIM)
      SUMC= ONE/CSQRT(CXDOTU(NDIM,V2,1,VS,1))
      DO 40 K=1,NDIM
         VS(K) = SUMC*VS(K)
         V2(K) = SUMC*V2(K)
   40 CONTINUE
      DO 50 K=1,NDIM
         V1(K) = ZEROC
         W1(K) = V1(K)
         W2(K) = V2(K)
         WS(K) = VS(K)
   50 CONTINUE
   60 CONTINUE
C
      DO 140 IVEC=MOLD1,KMAX
C     ------------------------
C
         SUMC = BETA(IVEC)
         DO 70 K=1,NDIM
            GR(K) = REAL(VS(K))
            GC(K) = AIMAG(VS(K))
   70    CONTINUE
         JOB = 0
         CALL ABSOLV(VS,V1,SUMC,JOB,AA,IPVT,LDAA,NDIM)
         DO 80 K=1,NDIM
            VS(K) = CMPLX(GR(K),GC(K))
   80    CONTINUE
         DO 90 K=1,NDIM
            GR(K) = REAL(WS(K))
            GC(K) = AIMAG(WS(K))
   90    CONTINUE
         JOB = 1
         CALL ABSOLV(WS,W1,SUMC,JOB,AA,IPVT,LDAA,NDIM)
         DO 100 K=1,NDIM
            WS(K) = CMPLX(GR(K),GC(K))
  100    CONTINUE
         SUMC = .5E0*(CXDOTU(NDIM,WS,1,V1,1) + CXDOTU(NDIM,VS,1,W1,1))
         ALPHA(IVEC) = SUMC
         ALFA = SUMC
         DO 110 K=1,NDIM
            W1(K) = W1(K) - SUMC*W2(K)
            V1(K) = V1(K) - SUMC*V2(K)
  110    CONTINUE
         DO 120 K=1,NDIM
            TEMPC = V1(K)
            V1(K) = V2(K)
            V2(K) = TEMPC
            TEMPC = W1(K)
            W1(K) = W2(K)
            W2(K) = TEMPC
  120    CONTINUE
         CALL BMATV(V2,VS,BB,LDBL,NDIM)
         CALL BMATV(W2,WS,BB,LDBL,NDIM)
         IN = IVEC+1
         SUMC = CSQRT(CXDOTU(NDIM,VS,1,W2,1))
         SUMCT = CSQRT(CXDOTU(NDIM,WS,1,V2,1))
         BATA = .5E0*(SUMC + SUMCT)
         IF(CABS(ALFA).GT.LTOL.OR.CABS(BATA).GT.LTOL) THEN
            GR(1) = -1.E3
            WRITE(NOUT,121) ALFA,BATA
            RETURN
         ENDIF
         BETA(IN) = BATA
         SUMC = ONE/BATA
         DO 130 K=1,NDIM
            V2(K) = SUMC*V2(K)
            VS(K) = SUMC*VS(K)
            W2(K) = SUMC*W2(K)
            WS(K) = SUMC*WS(K)
  130    CONTINUE
C
  140 CONTINUE
C     --------
C
C     WRITE(NOUT,141)
C     WRITE(NOUT,143) (ALPHA(K),K=1,KMAX)
C     WRITE(NOUT,145)
C     WRITE(NOUT,143) (BETA(K),K=2,KMAX)
      RETURN
C
  121 FORMAT(/' EITHER ALPHA OR BETA EXCEEDED LTOL.  ALPHA,BETA ='/
     >       1P,2(E20.8))
  141 FORMAT(' DIAGONALE 1:KMAX'/)
  143 FORMAT(1P,2(E17.7,E16.7))
  145 FORMAT(/' SUBDIAGONALE 2:KMAX'/)
      END
************************************************************************
*DECK BMATV
      SUBROUTINE BMATV(W,U,BB,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMDIM5
C
      INTEGER  NDIM, LDBL, J, LDB1, KF, K
      REAL     BB(LDBL,*)
      COMPLEX  U(*), W(*)
C
      DO 10 J=1,NDIM
         U(J) = BB(LDBL,J)*W(J)
   10 CONTINUE
      LDB1 = LDBL+1
      DO 30 J=2,NDIM
         KF = MAX0(1,LDB1-J)
         DO 20 K=KF,MUL
            I = J+K-LDBL
            U(I) = U(I) + BB(K,J)*W(J)
            U(J) = U(J) + BB(K,J)*W(I)
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ABSOLV
      SUBROUTINE ABSOLV(W,U,SUMC,JOB,AA,IPVT,LDAA,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMDIM5
C
      INTEGER  IPVT(*), NDIM, LDAA, JOB, K
      COMPLEX  AA(LDAA,*), U(*), W(*), SUMC
C
      CALL CGBSLL(AA,LDAA,NDIM,MLL,MUL,IPVT,W,JOB)
      DO 10 K=1,NDIM
         U(K) = W(K) - SUMC*U(K)
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGBSLL
      SUBROUTINE CGBSLL(ABD,LDA,N,ML,MU,IPVT,B,JOB)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      INTEGER  K, KB, L, LA, LB, LM, M, NM1
      COMPLEX  ABD(LDA,*), B(*)
      COMPLEX  CXDOTU, T
C
      M = MU + ML + 1
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 50
C     ---------------------
C
C ... JOB = 0 , SOLVE  A * X = B ...
C ... FIRST SOLVE L*Y = B ...
C
      IF(ML.EQ.0) GOTO 30
      IF(NM1.LT.1) GOTO 30
         DO 20 K=1,NM1
            LM = MIN0(ML,N-K)
            L = IPVT(K)
            T = B(L)
            IF(L.EQ.K) GOTO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL CXAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20    CONTINUE
   30 CONTINUE
C
C ... NOW SOLVE  U*X = Y ...
C
      DO 40 KB=1,N
         K = N + 1 - KB
         B(K) = B(K)/ABD(M,K)
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = -B(K)
         CALL CXAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   40 CONTINUE
      GOTO 100
C
   50 CONTINUE
C     --------
C
C ... JOB = NONZERO, SOLVE  HERM(A) * X = B ...
C ... FIRST SOLVE  HERM(U)*Y = B ...
C
      DO 60 K=1,N
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = CXDOTU(LM,ABD(LA,K),1,B(LB),1)
          B(K) = (B(K) - T)/ABD(M,K)
   60 CONTINUE
C
C ... NOW SOLVE HERM(L)*X = Y ...
C
      IF(ML.EQ.0) GOTO 90
      IF(NM1.LT.1) GOTO 90
         DO 80 KB=1,NM1
            K = N - KB
            LM = MIN0(ML,N-K)
            B(K) = B(K) + CXDOTU(LM,ABD(M+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF(L.EQ.K) GOTO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK TNORM
      SUBROUTINE TNORM(ALPHA,BETA,BTOL,AMAX,MEV,IB)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
      INTEGER  IB, I, MEV
      REAL     AMAX, BMIN, BMAX, BSIZE, BTOL, ABATA, AALFA
      COMPLEX  ALPHA(*), BETA(*)
C
      IB = 2
      BMIN = CABS(BETA(2))
      BMAX = BMIN
      AMAX = CABS(ALPHA(1))
C
      DO 10 I=2,MEV
         AMAX = AMAX1(AMAX,CABS(ALPHA(I)))
         BMAX = AMAX1(BMAX,CABS(BETA(I)))
         IF(CABS(BETA(I)).LT.BMIN) THEN
            IB = I
            BMIN = CABS(BETA(I))
         ENDIF
   10 CONTINUE
      AMAX = AMAX1(BMAX,AMAX)
      BSIZE = BMIN/AMAX
      IF(BSIZE.LT.BTOL) THEN
         IB = -IB
         WRITE(NOUT,11) MEV
      ENDIF
      BTOL = BMIN
C     WRITE(NOUT,13) IB
C     WRITE(NOUT,15) MEV,BMIN,AMAX,BSIZE
      RETURN
C
   11 FORMAT(/' BETA TEST INDICATES POSSIBLE LOSS OF LOCAL',
     >       ' ORTHOGONALITY OVER 1ST',I6,' LANCZOS VECTORS'/)
   13 FORMAT(/' MINIMUM BETA RATIO OCCURS AT',I6,' TH BETA' )
   15 FORMAT(/1X,'TSIZE',6X,'MIN BETA',5X,'TKMAX',6X,'MIN RATIO'/
     >       I6,1P,E14.3,E10.3,E15.3 )
      END
************************************************************************
*DECK COMPEV
      SUBROUTINE COMPEV(ALPHA,BETA,V1,V2,VS,EVMAG,GC,MULTOL,SPUTOL,
     >                  EPS,MP,T2FLAG,MEV,NDIS,SAVTEV)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
C
      INTEGER  MP(*), T2FLAG(*), SAVTEV, MEV1, J, IERR, K, KK, KP1
      INTEGER  NDIS, INDEX, I, JP1, IMIN
      REAL     EVMAG(*), GC(*)
      REAL     TEMP, TOL, DELMIN, EPS
      REAL     MULTOL, SPUTOL
      COMPLEX  ALPHA(*), BETA(*), VS(*), V1(*), V2(*), EVAL,CTEMP,TSCALE
C
      IF(SAVTEV.LE.0) THEN
         WRITE(NOUT,1) SAVTEV
         STOP
      ENDIF
      TSCALE = V1(1)
      MEV1 = MEV - 1
C     WRITE(NOUT,3) TSCALE
      CALL CXCOPY(MEV,ALPHA,1,VS,1)
      CALL CXCOPY(MEV,BETA,1,V1,1)
      V1(1) = TSCALE
C     WRITE(NOUT,5) MEV
      CALL CMTQL1(MEV,VS,V1,IERR)
      IF(IERR.NE.0) THEN
         WRITE(NOUT,7)
         STOP
      ENDIF
      DO 10 J=1,MEV
         EVMAG(J) = CABS(VS(J))
   10 CONTINUE
C
C ... SORTIERT VS UND EVMAG  EVMAG(1) < EVMAG(2) ...
C
      DO 30 K=2,MEV
         DO 20 KK=K-1,1,-1
            KP1 = KK+1
            IF(EVMAG(KP1).GE.EVMAG(KK)) GOTO 30
            EVAL = VS(KK)
            VS(KK) = VS(KP1)
            VS(KP1) = EVAL
            TEMP = EVMAG(KK)
            EVMAG(KK) = EVMAG(KP1)
            EVMAG(KP1) = TEMP
   20    CONTINUE
   30 CONTINUE
      IF(SAVTEV.GE.1) THEN
         WRITE(NOUT3,31)
         WRITE(NOUT3,33) MEV
         WRITE(NOUT3,35)
         WRITE(NOUT3,37) (VS(J),J=1,MEV)
      ENDIF
C
      MULTOL = MULTOL*EVMAG(MEV)
      SPUTOL = SPUTOL*EVMAG(MEV)
      TOL = 1000.0E0*SPUTOL
C     WRITE(NOUT,39) MULTOL,SPUTOL
      NDIS = 0
      DO 40 I=1,MEV
         T2FLAG(I) = 0
   40 CONTINUE
      DO 70 J=1,MEV
         IF(T2FLAG(J).EQ.1) GOTO 70
         CTEMP = VS(J)
         NDIS = NDIS + 1
         INDEX = 1
         T2FLAG(J) = 1
         IF(I.LT.MEV) THEN
            DO 50 I=J+1,MEV
               IF(T2FLAG(I).EQ.1) GOTO 50
               IF(EVMAG(I)-EVMAG(J).GT.MULTOL) GOTO 60
               IF(CABS(VS(J)-VS(I)).LE.MULTOL) THEN
                  INDEX = INDEX + 1
                  CTEMP = CTEMP + VS(I)
                  T2FLAG(I) = 1
               ENDIF
   50       CONTINUE
         ENDIF
   60    VS(NDIS) = CTEMP/FLOAT(INDEX)
         MP(NDIS) = INDEX
   70 CONTINUE
      IF(SAVTEV.GE.1) THEN
         IF(SAVTEV.EQ.1000) THEN
            WRITE(NOUT,71) MEV
            CALL CXCOPY(MEV1,ALPHA(2),1,V2,1)
            CALL CXCOPY(MEV1,BETA(2),1,V1,1)
            V1(1) = TSCALE
            CALL CMTQL1(MEV1,V2,V1,IERR)
            WRITE(NOUT,73) IERR
            IF(IERR.NE.0) STOP
         ELSE
            CALL SINVER(ALPHA,BETA,V1,V2,VS,EPS,EVMAG,GC,SPUTOL,
     >                  MP,T2FLAG,MEV,NDIS,NISO)
            DO 80 J=1,NDIS
               EVMAG(J) = CABS(VS(J))
   80       CONTINUE
            RETURN
         ENDIF
      ENDIF
C
      DO 90 J=1,MEV1
         EVMAG(J) = CABS(V2(J))
   90 CONTINUE
      DO 110 K=2,MEV1
         KM1 = K-1
         DO 100 L=1,KM1
            KK = K-L
            KP1 = KK+1
            IF(EVMAG(KP1).GE.EVMAG(KK)) GOTO 110
            EVAL = V2(KK)
            V2(KK) = V2(KP1)
            V2(KP1) = EVAL
            TEMP = EVMAG(KK)
            EVMAG(KK) = EVMAG(KP1)
            EVMAG(KP1) = TEMP
  100    CONTINUE
  110 CONTINUE
      DO 120 J=1,MEV1
         EVMAG(J) = CABS(V2(J))
  120 CONTINUE
      IF(SAVTEV.GE.1) THEN
         WRITE(NOUT3,121) (V2(J),J=1,MEV1)
      ENDIF
      DO 130 I=1,MEV1
         T2FLAG(I) = 0
  130 CONTINUE
C
      DO 180 J=1,MEV1
         DELMIN = 2.E0*CABS(VS(MEV))
         IMIN = 0
         DO 140 I=J,1,-1
            K = I
            IF(I.GT.NDIS) K = NDIS
            TEMP = CABS(VS(K))
            IF(CABS(VS(K)).LT.EVMAG(J)-SPUTOL) GOTO 150
            IF(MP(K).NE.0.AND.CABS(VS(K)-V2(J)).LT.DELMIN) THEN
               DELMIN = CABS(VS(K)-V2(J))
               IMIN = K
            ENDIF
  140    CONTINUE
  150    IF(J.LT.NDIS) THEN
            DO 160 I=J+1,NDIS
               IF(CABS(VS(I)).GT.EVMAG(J)+SPUTOL) GOTO 170
               IF(MP(I).NE.0.AND.CABS(VS(I)-V2(J)).LT.DELMIN) THEN
                  DELMIN = CABS(VS(I)-V2(J))
                  IMIN = I
               ENDIF
  160       CONTINUE
         ENDIF
  170    IF(IMIN.GT.0.AND.DELMIN.LE.SPUTOL.AND.MP(IMIN).LE.1) MP(IMIN)=0
C
  180 CONTINUE
C
      DO 190 J=1,NDIS
         EVMAG(J) = CABS(VS(J))
  190 CONTINUE
      RETURN
C
    1 FORMAT(' VALUE OF SAVTEV ',I5)
    3 FORMAT(/' IN COMPEV TSCALE EQUALS',1P,2E20.12/)
    5 FORMAT(/' COMPUTE EIGENVALUES OF T(1,',I4,') USING CMTQL1'/)
    7 FORMAT(' ON RETURN FROM CMTQL1 ERROR FLAG WAS NOT ZERO'/)
   31 FORMAT(' T-T2EVAL')
   33 FORMAT(I6,' = ORDER OF T-MATRIX, T-EIGVALS =')
   35 FORMAT(' T(1,MEV) EVAL')
   37 FORMAT(1P,4E20.12)
   39 FORMAT(/' TOLERANCES USED IN T-MULTIPLICITY AND SPURIOUS',
     >       ' TESTS =',1P,2E10.3/)
   71 FORMAT(/' COMPUTE T(2,',I4,') EIGENVALUES'/)
   73 FORMAT(' T2-HAT EIGENVALUES VIA CMTQL1'/' IERR = ',I6/)
   75 FORMAT(' DISTINCT T-EVAL ON RETURN FROM SINVER'
     >       /(I6,1P,2E20.12,0P))
  121 FORMAT(' T(2,MEV) EVALS'/(1P,4E20.12))
      END
************************************************************************
*DECK CMTQL1
      SUBROUTINE CMTQL1(N,D,E,IERR)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL MACHT
*CALL COMPCON
C
      COMPLEX  D(*), E(*), B, C , F, G, P, R, S, W
      COMPLEX  TEMPC, T0C
      REAL     EPS, EPSTWO, TEMP, TEMP0, T0, T1, T2
      INTEGER  I, J, L, M, N, MML, IERR, K
C
      EPS = 100.E0*MACHEP
      EPSTWO = SQRT(2.0E0)*MACHEP
      IERR = 0
C
      IF(N.EQ.1) RETURN
C
      DO 10 I=2,N
          E(I-1) = E(I)
   10 CONTINUE
      E(N) = ZEROC
C
      DO 60 L=1,N-1
         J = 0
   20    DO 30 M=L,N-1
            TEMP = ABS(REAL(D(M))) + ABS(AIMAG(D(M)))
     >             + ABS(REAL(D(M+1))) + ABS(AIMAG(D(M+1)))
            T2   = ABS(REAL(E(M))) + ABS(AIMAG(E(M)))
            IF(T2.LE.TEMP*EPSTWO) GOTO 40
   30    CONTINUE
   40    P = D(L)
         IF(M.EQ.L) GOTO 60
         IF(J.EQ.100) THEN
            IERR = L
            RETURN
         ENDIF
         J = J+1
         G = (D(L+1) - P)*CHALF
         TEMP = ABS(REAL(G))+ABS(AIMAG(G))
         T1 = ABS(REAL(E(L)))+ABS(AIMAG(E(L)))
         IF(TEMP.LE.T1) THEN
            W = G/E(L)
            R = CSQRT(ONEC + W*W)
            T0 = REAL(W)*REAL(R) + AIMAG(W)*AIMAG(R)
            T0C = ONEC
            IF(T0.LT.ZERO) T0C = -ONEC
            G = D(M) - P + E(L)/(W + T0C*R)
         ELSE
            W = E(L)/G
            R = CSQRT(ONEC + W*W)
            T0C = ONEC
            IF(REAL(R).LT.ZERO) T0C = -ONEC
            G = D(M) - P + W*E(L)/(ONEC + T0C*R)
         ENDIF
         S = ONEC
         C = -ONEC
         P = ZEROC
C
         DO 50 I=M-1,L,-1
            F =  S*E(I)
            B = -C*E(I)
            T0 = ABS(REAL(G)) + ABS(AIMAG(G))
            T1 = ABS(REAL(F)) + ABS(AIMAG(F))
            IF(T1.LE.T0) THEN
               W = F/G
               T2 = T1/T0
               TEMP = T2*T2 + ONE
               R = ONEC + W*W
               TEMP0 = ABS(REAL(R)) + ABS(AIMAG(R))
               R = CSQRT(R)
               E(I+1) = G*R
               C = ONEC/R
               S = W*C
            ELSE
               W = G/F
               T2 = T0/T1
               TEMP = ONE + T2*T2
               R = ONEC + W*W
               TEMP0 = ABS(REAL(R)) + ABS(AIMAG(R))
               R = CSQRT(R)
               E(I+1) = F*R
               S = ONEC/R
               C = W*S
            ENDIF
            IF(TEMP0.LE.EPS*TEMP) THEN
               IERR = -L
               RETURN
            ENDIF
            G = D(I+1)-P
            R = (D(I)-G)*S+CTWO*C*B
            P = S*R
            D(I+1) = G+P
            G = B-C*R
   50    CONTINUE
C
         D(L) = D(L)-P
         E(L) = G
         E(M) = ZEROC
         GOTO 20
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SINVER
      SUBROUTINE SINVER(ALPHA,BETA,V1,V2,VS,EPS,GR,GC,SPUTOL,MP,
     >                  INTERC,MEV,NDIS,NISO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPCON
*CALL ISEED
C
      INTEGER  MP(*), INTERC(*), ISEED, NISO, JEV, NG, MEV, MP1, MM1
      INTEGER  MEV1, I, II, ISPUR, NGOOD, NDIS
      REAL     XU, NORM, TSUM, GSUM, SPUTOL
      REAL     EPS, EPS3, EPS4, GR(*), GC(*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*)
      COMPLEX  U, Z, X1, RATIO, BETAM, TEMP, SUMC, CXDOTU
C
C     WRITE(NOUT,1)
      NISO = 0
      DO 10 JEV=1,NDIS
         IF(MP(JEV).LE.1) NISO = NISO + 1
   10 CONTINUE
      IF(NISO.EQ.0) THEN
         WRITE(NOUT,11)
         RETURN
      ENDIF
C
      NG = 0
      NISO = 0
      MP1 = MEV+1
      MM1 = MEV-2
      MEV1 = MEV - 1
      BETAM = BETA(MP1)
      BETA(MP1) = ZEROC
      TSUM = CABS(ALPHA(2))
      DO 20 I=3,MEV
         TSUM = TSUM + CABS(ALPHA(I)) + CABS(BETA(I))
   20 CONTINUE
      EPS3 = EPS*TSUM
      EPS4 = FLOAT(MEV1)*EPS3
*IF CRAY
      DO 30 I=1,MEV1
         GR(I) = RANF( )
   30 CONTINUE
      DO 40 I=1,MEV1
         GC(I) = RANF( )
   40 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED = 1.0
      CALL DURAND(RSEED,MEV1,GR)
      CALL DURAND(RSEED,MEV1,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      GSUM = ZERO
      DO 50 I=1,MEV1
         GSUM = GSUM + ABS(GR(I)) + ABS(GC(I))
   50 CONTINUE
      GSUM = EPS4/GSUM
      DO 60 I=1,MEV1
         GR(I) = GSUM*GR(I)
         GC(I) = GSUM*GC(I)
   60 CONTINUE
C
      DO 130 JEV=1,NDIS
C     -------------------
C
         IF(MP(JEV).EQ.0) GOTO 130
         NG = NG + 1
         IF(MP(JEV).NE.1) GOTO 130
         NISO = NISO + 1
         X1 = VS(JEV)
         DO 70 I=1,MEV1
            INTERC(I) = 0
            V2(I) = CMPLX(GR(I),GC(I))
   70    CONTINUE
         U = ALPHA(2)-X1
         Z = BETA(3)
         DO 80 I=3,MEV
            IF(CABS(BETA(I)).LE.CABS(U)) THEN
               V1(I-2) = Z/U
               V2(I-2) = V2(I-2)/U
               V2(I-1) = V2(I-1)-BETA(I)*V2(I-2)
               RATIO = BETA(I)/U
               U = ALPHA(I)-X1-Z*RATIO
               Z = BETA(I+1)
            ELSE
               RATIO = U/BETA(I)
               INTERC(I-1) = 1
               V1(I-2) = ALPHA(I)-X1
               U = Z-RATIO*V1(I-2)
               Z = -RATIO*BETA(I+1)
               TEMP = V2(I-2)
               V2(I-2) = V2(I-1)
               V2(I-1) = TEMP-RATIO*V2(I-1)
            ENDIF
   80    CONTINUE
         IF(CABS(U).EQ.ZERO) U = CMPLX(EPS3,EPS3)
         V2(MEV1) = V2(MEV1)/U
         DO 90 I=MM1,1,-1
            IF(INTERC(I+1).NE.1) THEN
               V2(I) = V2(I)-V1(I)*V2(I+1)
            ELSE
               V2(I) = (V2(I)-V1(I)*V2(I+1)-BETA(I+3)*V2(I+2))/BETA(I+2)
            ENDIF
   90    CONTINUE
         NORM = CABS(V2(MEV1))
         DO 100 I=MM1,1,-1
            NORM = NORM + CABS(V2(I))
  100    CONTINUE
         SUMC = CXDOTU(MEV1,V2,1,V2,1)
         SUMC = ONEC/CSQRT(SUMC)
         DO 110 II=1,MEV1
            V2(II) = SUMC*V2(II)
  110    CONTINUE
         V1(1) = ALPHA(2)*V2(1) + BETA(3)*V2(2)
         V1(MEV1) = BETA(MEV)*V2(MEV1-1) + ALPHA(MEV)*V2(MEV1)
         DO 120 I=2,MM1
            V1(I) = BETA(I+1)*V2(I-1) + ALPHA(I+1)*V2(I) +
     >              BETA(I+2)*V2(I+1)
  120    CONTINUE
         SUMC = CXDOTU(MEV1,V1,1,V2,1)
         NORM = CABS(SUMC - X1)
         IF(NORM.LE.SPUTOL) MP(JEV) = 0
C
  130 CONTINUE
C     --------
C
      ISPUR = 0
      DO 140 J=1,NDIS
         IF(MP(J).EQ.0) ISPUR = ISPUR + 1
  140 CONTINUE
      NGOOD = NDIS - ISPUR
C     WRITE(NOUT,141) ISPUR,NGOOD,NDIS
C
      BETA(MP1) = BETAM
      RETURN
C
    1 FORMAT(' DETERMINE WHICH EIGENVALUES ARE SPURIOUS'/)
   11 FORMAT(/' THERE ARE NO ISOLATED T-EIGENVALUES SO SPURIOUS',
     >       ' TEST IS NOT REQUIRED')
  141 FORMAT(I5,' EIGENVALUES WERE LABELLED SPURIOUS, LEAVING',I5,
     >       ' T-EIGENVALUES'/' OUT OF',I6,' DISTINCT ONES.'/' SOME OF',
     >       ' THESE MAY GET LUMPED BY SUBROUTINE LUMP'/)
      END
************************************************************************
*DECK LUMP
      SUBROUTINE LUMP(VC,V1,VA,RELTOL,SPUTOL,SCALE2,LINDEX,TFLAG,LOOP)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPCON
C
      INTEGER  LINDEX(*), TFLAG(*), K, LOOP, NLOOP, J
      INTEGER  ICOUNT, JN, INDSUM, ISPUR, IN
      REAL     VA(*), RELTOL, SPUTOL, SCALE2
      REAL      THOLD, TH1, TH2, DGAP
      COMPLEX  VC(*), V1(*), SUMC
C
      TH2 = SCALE2*SPUTOL
      DO 10 K=1,LOOP
         TFLAG(K) = 0
   10 CONTINUE
      NLOOP = 0
C
      DO 40 J=1,LOOP
C
         IF(TFLAG(J).EQ.1) GOTO 40
         NLOOP = NLOOP + 1
         TFLAG(J) = 1
         V1(1) = VC(J)
         ICOUNT = 1
         JN = LINDEX(J)
         TH1 = RELTOL*VA(J)
         THOLD = AMAX1(TH1,TH2)
         IF(JN.NE.0) THEN
            INDSUM = JN
            ISPUR = 0
            SUMC = FLOAT(JN)*VC(J)
         ELSE
            INDSUM = 1
            ISPUR = 1
            SUMC = ZEROC
         ENDIF
         IF(J.NE.LOOP) THEN
             DO 20 I=J+1,LOOP
               IF(TFLAG(I).EQ.1) GOTO 20
               IF(VA(I)-VA(J).GE.THOLD) GOTO 30
               IF(CABS(VC(I)-VC(J)).GE.THOLD) GOTO 20
               ICOUNT = ICOUNT + 1
               TFLAG(I) = 1
               V1(ICOUNT) = VC(I)
               IF(LINDEX(I).EQ.0) THEN
                  ISPUR = ISPUR + 1
                  INDSUM = INDSUM + 1
               ELSE
                 INDSUM = INDSUM + LINDEX(I)
                 SUMC = SUMC + FLOAT(LINDEX(I))*VC(I)
               ENDIF
   20       CONTINUE
         ENDIF
   30    IF(ICOUNT.EQ.1) INDSUM = JN
         IDIF = INDSUM - ISPUR
         IF(ICOUNT.EQ.1.OR.(ICOUNT.GT.1.AND.IDIF.EQ.0)) THEN
            VC(NLOOP) = VC(J)
            VA(NLOOP) = VA(J)
         ELSE
            SUMC = SUMC/FLOAT(IDIF)
            VC(NLOOP) = SUMC
            VA(NLOOP) = CABS(SUMC)
         ENDIF
         LINDEX(NLOOP) = INDSUM
C
   40 CONTINUE
C
      LOOP = NLOOP
C
      RETURN
      END
************************************************************************
*DECK COMGAP
      SUBROUTINE COMGAP(VC,VA,GG,MP,IND,M,ITAG)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  MP(*), IND(*), M, INDEX, ITAG
      REAL     VA(*), GG(*), T1
      COMPLEX  VC(*)
C
      DO 50 K=1,M
C
         INDEX = 0
         T1 = 2.*VA(M)
C
         IF(K.EQ.1) GOTO 20
         DO 10 J=K-1,1,-1
            IF(VA(K)-VA(J).GT.T1) GOTO 20
            IF(T1.GT.CABS(VC(K)-VC(J))) THEN
               T1 = CABS(VC(K)-VC(J))
               INDEX = J
            ENDIF
   10    CONTINUE
C
   20    IF(K.EQ.M) GOTO 40
         DO 30 J=K+1,M
            IF(VA(J)-VA(K).GT.T1) GOTO 40
            IF(T1.GT.CABS(VC(K)-VC(J))) THEN
               T1=CABS(VC(K)-VC(J))
               INDEX = J
            ENDIF
   30    CONTINUE
C
   40    IND(K) = INDEX
         GG(K) = T1
C
   50 CONTINUE
C
      IF(ITAG.EQ.0) RETURN
C
      DO 60 K=1,M
         IF(MP(IND(K)).EQ.0) GG(K) = -GG(K)
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ISOEV
      SUBROUTINE ISOEV(VS,GR,GG,GAPTOL,SPUTOL,SCALE1,MP,NDIS,NG,NISO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  MP(*), NISO, NG, J, NDIS, I
      REAL     GR(*), SPUTOL, GAPTOL, SCALE1, TEMP, TOL, DGAP
      REAL     GG(*)
      COMPLEX  VS(*)
C
      DGAP = SCALE1*SPUTOL
      NISO = 0
      NG = 0
      DO 40 J=1,NDIS
         IF(MP(J).EQ.0) GOTO 40
         NG = NG+1
         IF(MP(J).NE.1) GOTO 40
         TOL = AMAX1(DGAP,GAPTOL*GR(J))
         NISO = NISO + 1
         IF(ABS(GG(J)).GT.TOL) GOTO 40
         IF(J.GT.1) THEN
            DO 10 I=J-1,1,-1
               IF(GR(J)-GR(I).GT.TOL) GOTO 20
               IF(MP(I).EQ.0) THEN
                  TEMP = CABS(VS(J)-VS(I))
                  IF(TEMP.LE.TOL) THEN
                     MP(J) = -MP(J)
                     NISO = NISO-1
                     GOTO 40
                  ENDIF
               ENDIF
   10       CONTINUE
         ENDIF
   20    IF(J.LT.NDIS) THEN
            DO 30 I=J+1,NDIS
               IF(GR(I)-GR(J).GT.TOL) GOTO 40
               IF(MP(I).EQ.0) THEN
                  TEMP = CABS(VS(J)-VS(I))
                  IF(TEMP.LE.TOL) THEN
                        MP(J) = -MP(J)
                        NISO = NISO-1
                        GOTO 40
                   ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
   40 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK INVERR
      SUBROUTINE INVERR(ALPHA,BETA,V1,V2,VS,EPS,GR,GC,G,GG,MP,INTERC,
     >                   MEV,NDIS,NISO,IT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPCON
*CALL ISEED
C
      INTEGER  MP(*), INTERC(*)
      INTEGER  SEED, MEV, NDIS, NISO, IT
      INTEGER  NG, ITER, MP1, MM1, I, II, ISO
      REAL     EST, XU, NORM, TSUM, GSUM
      REAL     EPS, EPS3, EPS4
      REAL     G(*), GG(*), GR(*), GC(*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*)
      COMPLEX  U, Z, X1, RATIO, BETAM, TEMP, SUM
      COMPLEX  CXDOTC
C
      NG = 0
      NISO = 0
      ITER = IT
      MP1 = MEV+1
      MM1 = MEV-1
      BETAM = BETA(MP1)
      BETA(MP1) = ZEROC
      TSUM = CABS(ALPHA(1))
C
      DO 10 I=2,MEV
         TSUM = TSUM + CABS(ALPHA(I)) + CABS(BETA(I))
   10 CONTINUE
C
      EPS3 = EPS*TSUM
      EPS4 = FLOAT(MEV)*EPS3
*IF CRAY
      DO 20 I=1,MEV
         GR(I) = RANF( )
   20 CONTINUE
      DO 30 I=1,MEV
         GC(I) = RANF( )
   30 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED = 1.0
      CALL DURAND(RSEED,MEV,GR)
      CALL DURAND(RSEED,MEV,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      GSUM = ZERO
      DO 40 I=1,MEV
         GSUM = GSUM + ABS(GR(I)) + ABS(GC(I))
   40 CONTINUE
      GSUM = EPS4/GSUM
      DO 50 I=1,MEV
         GR(I) = GSUM*GR(I)
         GC(I) = GSUM*GC(I)
   50 CONTINUE
C
      DO 140 JEV=1,NDIS
C     -------------------
C
         IF(MP(JEV).EQ.0) GOTO 140
         NG = NG + 1
         IF(MP(JEV).NE.1) GOTO 140
         IT = 1
         NISO = NISO + 1
         X1 = VS(JEV)
         DO 60 I=1,MEV
            INTERC(I) = 0
            V2(I) = CMPLX(GR(I),GC(I))
   60    CONTINUE
   70    U = ALPHA(1)-X1
         Z = BETA(2)
         DO 80 I=2,MEV
            IF(CABS(BETA(I)).LE.CABS(U)) THEN
               V1(I-1) = Z/U
               V2(I-1) = V2(I-1)/U
               V2(I) = V2(I)-BETA(I)*V2(I-1)
               RATIO = BETA(I)/U
               U = ALPHA(I)-X1-Z*RATIO
               Z = BETA(I+1)
            ELSE
               RATIO = U/BETA(I)
               INTERC(I) = 1
               V1(I-1) = ALPHA(I)-X1
               U = Z-RATIO*V1(I-1)
               Z = -RATIO*BETA(I+1)
               TEMP = V2(I-1)
               V2(I-1) = V2(I)
               V2(I) = TEMP-RATIO*V2(I)
            ENDIF
   80    CONTINUE
         IF(CABS(U).EQ.ZERO) U = CMPLX(EPS3,EPS3)
         V2(MEV) = V2(MEV)/U
         DO 90 I=MM1,1,-1
            IF(INTERC(I+1).NE.1) THEN
               V2(I) = V2(I)-V1(I)*V2(I+1)
            ELSE
               V2(I)=(V2(I)-V1(I)*V2(I+1)-BETA(I+2)*V2(I+2))/BETA(I+1)
            ENDIF
   90    CONTINUE
         NORM = CABS(V2(MEV))
         DO 100 I=MM1,1,-1
            NORM = NORM + CABS(V2(I))
  100    CONTINUE
         IF(NORM.GE.ONE) GOTO 120
         IT = IT+1
         IF(IT.GT.ITER) GOTO 120
         XU = EPS4/NORM
         DO 110 I=1,MEV
            V2(I) = V2(I)*XU
  110    CONTINUE
         GOTO 70
  120    CONTINUE
         SUM = CXDOTC(MEV,V2,1,V2,1)
         SUM = ONE/SQRT(SUM)
         DO 130 II=1,MEV
            V2(II) = SUM*V2(II)
  130    CONTINUE
         EST = CABS(BETAM)*CABS(V2(MEV))
         GSUM = CABS(BETAM)
         IF(IT.GT.ITER) EST = -EST
         G(NISO) = EST
C
  140 CONTINUE
C     --------
C
      ISO = 0
      DO 150 J=1,NDIS
         IF(MP(J).EQ.1) THEN
            ISO = ISO+1
            GR(ISO) = GG(J)
            V2(ISO) = VS(J)
         ENDIF
  150 CONTINUE
      IF(NISO.EQ.0) WRITE(NOUT,151)
      BETA(MP1) = BETAM
      RETURN
C
  151 FORMAT(/' THERE ARE NO ISOLATED T-EIGENVALUES SO NO ERROR',
     >       ' ESTIMATES WERE COMPUTED')
      END
************************************************************************
*DECK DIAG1
      SUBROUTINE DIAG1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE DIAG  (DIAGNOSTICS)                              **
**    -------------------                                             **
**                                                                    **
**    STRUCTURE :                                                     **
**                                                                    **
**        DIAG1              DIAG234              DIAG5               **
**          QRPLOT             XINP                 (LBLTOP)          **
**            (LBLTOP)         EIGVFK               (NFRAME)          **
**            (LPLOT)            KOMPFK             (LPLOT)           **
**                                 CUBFCT                              **
**                               PPPEVP                               **
**                                 (NFRAME)                           **
**                                 (LPLOT)                            **
**                                 (DLCH)                             **
**                             DIAGXTAX                               **
**                               CONAMAT                              **
**                               (CXDOTU)                             **
**                                                                    **
************************************************************************
************************************************************************
C
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR QR
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMDIM
*CALL CORE1D
C
C     PLOT THE EIGENVALUES
C
      CALL QRPLOT(WR,WI,NDIM,X,Y)
C
      RETURN
      END
************************************************************************
*DECK QRPLOT
      SUBROUTINE QRPLOT(WR,WI,NDIM,X,Y)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPLOT
*CALL COMLAB
C
      INTEGER  NDIM
      REAL     WR(*), WI(*), XMIN, XMAX, YMIN, YMAX
      REAL     X(*), Y(*)
      INTEGER  L,LOR
      CHARACTER*10 XNAME, YNAME
      CHARACTER*24 TOP
C
      WRITE(NOUTE) LABEL, EQNAME
      WRITE(NOUTE) NDIM
      WRITE(NOUTE) (WR(I),I=1,NDIM)
      WRITE(NOUTE) (WI(I),I=1,NDIM)
 
      WRITE(TOP,'(''QR-PLOT (VERSION '',A3,'') : '')') LABEL(1:3)
      LABEL=TOP//EQNAME
      WRITE(NOUT,*) LABEL
      CALL LBLTOP(LABEL,34)
      CALL LBLBOT(' ',1)
C
      XNAME = 'RE(LAMBDA)'
      YNAME = 'IM(LAMBDA)'
C
C     TOTAL SPECTRUM
C
      XMIN = XMINQR(1)
      XMAX = XMAXQR(1)
      YMIN = YMINQR(1)
      YMAX = YMAXQR(1)
C
      L = 1
      DO 10 I = 1,NDIM
         IF((WI(I).GE.YMIN).AND.(WI(I).LE.YMAX)
     >   .AND.(WR(I).GE.XMIN).AND.(WR(I).LE.XMAX)) THEN
            Y(L) = WI(I)
            X(L) = WR(I)
            L    = L + 1
         ENDIF
   10 CONTINUE
      L = L - 1
      IF(L.LE.1) RETURN
      WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
C
      CALL LPLOT(2,1,1201,X,Y,L,1,'TOTAL SPECTRUM',14,XNAME,10,YNAME,10)
C
C     SPECTRUM
C
      IF(NPLOT.GT.1) THEN
C
         DO 30 IP=2,NPLOT
            XMIN = XMINQR(IP)
            XMAX = XMAXQR(IP)
            YMIN = YMINQR(IP)
            YMAX = YMAXQR(IP)
            LOR  = 3-MOD(IP,2)
            L = 1
            DO 20 I = 1,NDIM
               IF((WI(I).GE.YMIN).AND.(WI(I).LE.YMAX)
     >         .AND.(WR(I).GE.XMIN).AND.(WR(I).LE.XMAX)) THEN
                  Y(L) = WI(I)
                  X(L) = WR(I)
                  L    = L + 1
               ENDIF
   20       CONTINUE
            L = L - 1
            IF(L.LE.1) RETURN
C
            CALL LPLOT
     >        (LOR,1,1201,X,Y,L,1,'(PART)SPECTRUM',12,XNAME,10,YNAME,10)
            WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
   30    CONTINUE
C
      ENDIF
C
      RETURN
C
   11 FORMAT(1X,I5,' WERTE IM RAHMEN ',1P,2E12.4,2X,
     >       2E12.4,' GEPLOTTET')
      END
************************************************************************
*DECK DIAG234
      SUBROUTINE DIAG234
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR INVERSE VECTOR ITERATION
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMP234
*CALL COMPIO
*CALL COMDIAG
*CALL CORE234D
*CALL COMGRID
*CALL COMMOD
*CALL COMWEL
*CALL COMIOD
*CALL COMSPL
*CALL COMFFT
*CALL COMBDIA
*CALL COMVRSP
*CALL COMVAC
*CALL COMIT
C
      COMPLEX  CSUM, CSUM1, CSUM2, CSUM3
      COMPLEX  A1(MANZ), A2(MANZ), DA2(MANZ), A3(MANZ), DA3(MANZ),
     >         JB1(MANZ), JB2(MANZ), JB3(MANZ), JB1V(MANZ), B3V(MANZ),
     >         BDB(MANZ), BDBV(MANZ), RHO1(MANZ), T1(MANZ),
     >         B3L(MANZ)
      COMPLEX  OOR2, PSOR2, PTOR2
      REAL     DUMMY(3),ABLTG(3)
C
      EXTERNAL CONAMAT, CONBMAT
C
C     DO 123 J = 1,NCVD
C     DO 123 I = 1,NBG
C        WRITE(NOUTE) EV(I,J)
C 123 CONTINUE
C
c-----------------------------------------------------------------------
c write eigenvector for use in seperate plot programs
c-----------------------------------------------------------------------
      write(22,11) ew
      write(22,12) ng,manz
      write(22,11) (rfour(m),m=1,manz)
      write(22,11) (sgrid(i),i=1,ng)
      do 10 i=1,ng
         do j = 1,nbg
            if (abs(ev(j,i)).le.1e-80) then
               ev(j,i) = 0
            endif
         enddo
        write(22,11) (real(ev(j,i)),j=1,nbg)
        write(22,11) (aimag(ev(j,i)),j=1,nbg)
   10 continue

   11 format(4e16.8)
   12 format(2i8)


      IF(RWALL.GT.1.) THEN
C     --------------------
C

      SPS2 = 2. * CPSURF
      ZQ   = Q1(NPSI)
      T2OQ = RBP1(NPSI)**2 / ZQ
      TOQ  = RBP1(NPSI) / ZQ

      Qtemp   = SPWERT(NPSI,1.,Q1,Q2,Q3,Q4,CS,ABLTG)
      DQ1     = ABLTG(1)
      Ptemp   = SPWERT(NPSI,1.,P1,P2,P3,P4,CS,ABLTG)
      DP1     = ABLTG(1)
C
C ... VECTOR POTENTIAL AT THE BOUNDARY ...
C
      DO 700 M=1,MANZ
         A1(M)   =  EV(2*MANZ+2+2*(M-1),NG)
         A2(M)   =  EV(1+2*(M-1),NG)
         DA2(M)  =  EV(2+2*(M-1),NG)
  700 CONTINUE
C
C ... MAGNETIC FIELD AT THE BOUNDARY ...
C
C      WRITE(NOUT,*)
C      WRITE(NOUT,*) 'PLASMA MAGNETIC FIELD AT BOUNDARY, JB1, B3L : '
C      WRITE(NOUT,*)
      DO 710 M=1,MANZ
         SMS = RFOUR(M)
         JB1(M) = -(0.,1.)*(SMS+ZNKWEL*ZQ)/ZQ * A2(M)
         JB2(M) = ZNKWEL*A1(M) + DA2(M)/ZQ - DQ1 * A2(M)/ZQ**2
         JB3(M) = DA2(M) - SMS*A1(M)
         B3L(M) = JB3(M) * TOQ / SPS2
         JB1V(M) = JB1(M)
         CSUM1 = 0.
         CSUM2 = 0.
         CSUM3 = 0.
C         WRITE(NOUT,701) M,JB1(M),B3L(M)
  710 CONTINUE
C
C      WRITE(NOUT,*)
C      WRITE(NOUT,*) ' VACUUM MAGNETIC FIELD AT BOUNDARY, JB1V, B3LV : '
C      WRITE(NOUT,*)
      DO 730 M=1,MANZ
         B3V(M) = (0.,0.)
         DO 720 L=1,MANZ
            B3V(M) = B3V(M) + B3B1(L,M) * JB1V(L)
  720    CONTINUE
C         WRITE(NOUT,721) M,JB1V(M),B3V(M)
  730 CONTINUE
C
      DO 750 M=1,MANZ
         CSUM = (0.,0.)
         DO 740 L=1,MANZ
            SMS = RFOUR(M)
            SML = RFOUR(L)
            K = ABS(INT(SMS)-INT(SML)) + 1
C
            OOR2 = CMPLX(SPWERT(NPSI,1.,ROOR2(1,K),ROOR2(NP1,K),
     >                           ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                   SPWERT(NPSI,1.,IOOR2(1,K),IOOR2(NP1,K),
     >                           IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
            PSOR2 = CMPLX(SPWERT(NPSI,1.,RPSOR2(1,K),RPSOR2(NP1,K),
     >                          RPSOR2(N2P1,K),RPSOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IPSOR2(1,K),IPSOR2(NP1,K),
     >                          IPSOR2(N2P1,K),IPSOR2(N3P1,K),CS,DUMMY))
C
            PTOR2 = CMPLX(SPWERT(NPSI,1.,RPTOR2(1,K),RPTOR2(NP1,K),
     >                          RPTOR2(N2P1,K),RPTOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IPTOR2(1,K),IPTOR2(NP1,K),
     >                          IPTOR2(N2P1,K),IPTOR2(N3P1,K),CS,DUMMY))
C
            IF (INT(SMS).LT.INT(SML)) THEN
               OOR2  = CONJG(OOR2)
               PSOR2 = CONJG(PSOR2)
               PTOR2 = CONJG(PTOR2)
            ENDIF
C
            CSUM = CSUM - PTOR2 * JB1(L)
     >                  + PSOR2 * JB2(L)  / SPS2
     >                  + OOR2  * JB3(L)  * T2OQ / SPS2
  740    CONTINUE
         BDB(M) = CSUM
  750 CONTINUE
C
      DO 770 M=1,MANZ
         CSUM = (0.,0.)
         DO 760 L=1,MANZ
            SMS = RFOUR(M)
            SML = RFOUR(L)
            K = ABS(INT(SMS)-INT(SML)) + 1
C
            OOR2 = CMPLX(SPWERT(NPSI,1.,ROOR2(1,K),ROOR2(NP1,K),
     >                            ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IOOR2(1,K),IOOR2(NP1,K),
     >                           IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
            IF(INT(SMS).LT.INT(SML)) OOR2  = CONJG(OOR2)
            CSUM = CSUM + OOR2 *B3V(L)*TOQ*(SML+ZNKWEL*ZQ)/ZNKWEL
C
  760    CONTINUE
         BDBV(M) = CSUM
  770 CONTINUE
C
      WRITE(NOUT,*)
      WRITE(NOUT,*) ' *************************************************'
      WRITE(NOUT,*) ' *            CHECK BOUNDARY CONDITIONS          *'
      WRITE(NOUT,*) ' *************************************************'
      DO 780 M=1,MANZ
         WRITE(NOUT,771) INT(RFOUR(M)),BDB(M)+DP1*A2(M)/(SPS2*ZQ)
         WRITE(NOUT,772) BDBV(M)
  780 CONTINUE
      ENDIF
C     -----
C
      CALL EIGVFK(NG,NPDIM,EV,XP,YP,NBG)
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION
C
      IF (IBVAC.NE.0) THEN
         DO 790 M = 1, MANZ
            WRITE(NOUTB,*) REAL(JB1(M)),AIMAG(JB1(M))
 790     CONTINUE
      ENDIF
C
C     WRITE(NOUT,1)
C     CALL DIAGXTAX(NBG,NGL,3*NBG,NZMA,EV,ZMA,
C    >              BUFF,XTAX,PROZ,CSUM1,CONAMAT)
C     WRITE(NOUT,2)
C     CALL DIAGXTAX(NBG,NGL,3*NBG,NZMA,EV,ZMA,
C    >              BUFF,XTAX,PROZ,CSUM2,CONBMAT)
C     CSUM=CSUM1/CSUM2
C     WRITE(NOUT,3) CSUM1,CSUM2,CSUM
CC    CALL DIAGNOS(NG,EV)
      RETURN
C
  701 FORMAT(1X,I3,6E12.4)
  721 FORMAT(1X,I3,4E12.4)
  771 FORMAT(' M = ',I3,'   p1+B.b(plasma)  = ',2E16.8)
  772 FORMAT('           B.b(vacuum)     = ',2E16.8)
    1 FORMAT(///' XT*A*X :')
    2 FORMAT(///' XT*B*X :')
    3 FORMAT(/////' CSUM1,CSUM2',1P,4E12.4,0P/' DIAGNOSTIK:'/
     >       ' (XT*A*X)/(XT*B*X):',1P,2E12.4,0P)
      END
************************************************************************
*DECK EIGVFK
      SUBROUTINE EIGVFK(NG,NPDIM,X0,X,Y,NBG)
C-----------------------------------------------------------------------
C     PLOTS OF THE EIGENVECTOR FOR A NUMBER OF RFOUR(I)'S
C     VERSION FOR PPPLIB
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMPARP
*CALL COMLAB
*CALL COMWEL
*CALL COMIT
C
      REAL     X(*), Y(NPDIM,2,MANZ)
      COMPLEX  X0(*)
      INTEGER  NG, NPDIM, NBG, IEIG, IK(7)
      CHARACTER*3 KOMP(7)
      DATA IK  / 1, 2, 3, 4, 5, 6, 7/
      DATA KOMP/'V1 ','V2 ','V3 ','PER','A1 ','A2 ','PAR'/
C
      EXTERNAL QUAFCT, CUBFCT
C
      NP = 2 * NG - 1
      WRITE(NOUTE) LABEL(1:3), EQNAME
C
      DO 30 IEIG=1,7
         DO 10 IM=1,MANZ
         DO 10 I=1,NPDIM
            Y(I,1,IM) = 0.0
            Y(I,2,IM) = 0.0
   10    CONTINUE
C
         IF ((IEIG.EQ.1).OR.(IEIG.EQ.4)
     >       .OR.(IEIG.EQ.6).OR.(IEIG.EQ.7)    ) THEN
           CALL KOMPFK(IK(IEIG),NBG,NPDIM,1.,IF1,X0,X,Y,CUBFCT)
         ELSE
           CALL KOMPFK(IK(IEIG),NBG,NPDIM,1.,IF1,X0,X,Y,QUAFCT)
         ENDIF  
         CALL PPPEVP(X,Y,KOMP(IK(IEIG)),NP,NPDIM,EW,IEIG)
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK KOMPFK
      SUBROUTINE KOMPFK(MKP,NBG,NPDIM,FAC,IFAC,X0,X,Y,CUBFCT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
C
      INTEGER  MKP, NBG, NPDIM, IFAC
      REAL     X(*), Y(NPDIM,2,MANZ), H(4), FAC
      COMPLEX  X0(*)
C
C
      DO 20 IM = 1, MANZ
C
         K1   = 2*MANZ*(MKP-1)+(IM*2-1)
         K2   = K1+1
C
C ...... FUNKTIONSWERTE DER EIGENFUNKTION ...
C
         ZSR  = SGRID(1)
         FACT = FAC
         IF(IFAC.NE.0) FACT = FACT*ZSR**IFAC
         NP   = 1
C
         X(NP) = ZSR
C
         SL   = SGRID(1)
         SU   = SGRID(2)
C
C ...... BEITRAG DES KOEFFIZIENTEN A(1) ...
C
         CALL CUBFCT(ZSR,SL,SU,H)
         ZA   = H(2)
         ZB   = H(4)
C
         Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                   + REAL(X0(K2)) * ZB)
         Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                   + AIMAG(X0(K2)) * ZB)
C
C ...... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
         ZA   = H(1)
         ZB   = H(3)
C
         Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                   + REAL(X0(K2+NBG))*ZB)
         Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                   + AIMAG(X0(K2+NBG))*ZB)
C
C ...... SCHLEIFE UEBER DIE INTERVALLE ...
C
         DO 10  N = 1, NGINT
C
            SL    = SGRID(N)
            SU    = SGRID(N+1)
C
C ......... INTERVALL MITTE ...
C
            ZSR   = (SU + SL) / 2.
            FACT  = FAC*ZSR**IFAC
            NP    = NP + 1
C
            X(NP) = ZSR
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N)  UND  B(N) ...
C
            CALL CUBFCT(ZSR,SL,SU,H)
C
            ZA    = H(2)
            ZB    = H(4)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                       + REAL(X0(K2)) * ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                      + AIMAG(X0(K2)) * ZB)
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
            ZA    = H(1)
            ZB    = H(3)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                      + REAL(X0(K2+NBG))*ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                      + AIMAG(X0(K2+NBG))*ZB)
C
C ......... INTERVALL ENDE ...
C
            ZSR   = SU
            FACT  = FAC*ZSR**IFAC
            NP    = NP + 1
C
            X(NP) = ZSR
C
C ......... BEITRAG DES KOEFFIZIENTEN  B(N+1) ...
C
            CALL CUBFCT(ZSR,SL,SU,H)
            ZA    = H(2)
            ZB    = H(4)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                      + REAL(X0(K2)) * ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                      + AIMAG(X0(K2)) * ZB)
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
            ZA    = H(1)
            ZB    = H(3)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                      + REAL(X0(K2+NBG))*ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                      + AIMAG(X0(K2+NBG))*ZB)
C
            K1     = K1 + NBG
            K2     = K2 + NBG
C
   10    CONTINUE
C
   20 CONTINUE
 
      RETURN
      END
************************************************************************
*DECK PPPEVP
      SUBROUTINE PPPEVP(X,Y,YNAME,NPOINT,NPDIM,EW,IPN)
C-----------------------------------------------------------------------
C PLOTS THE REAL AND IMAGINARY PART OF ONE EIGENVECTOR
C USING PPPLIB, JANUAR 1990              (E.SCHWARZ)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMWEL
*CALL COMLAB
C
      REAL     X(*), Y(NPDIM,2,*)
      INTEGER  IOP(7),IOP7(7), IP(7), NPOINT, IPN
      COMPLEX  EW
      CHARACTER*7   TITLE
      CHARACTER*(*) YNAME 
      CHARACTER*30  YNAMER,YNAMEI
      CHARACTER*39  EIGENW
      CHARACTER*52  EWEQ
C
*IF IPP
C     DATA IOP7 /1121,871,151,111,141,121,131/
      DATA IOP7 /1111,2061,461,421,451,431,441/
*ELSE
C     DATA IOP7 /190971,190641,190981,190991,191001,191101,191201/
      DATA IOP7 /190981,190991,191001,191011,191021,191031,191041/
*ENDIF
C
      XMIN = X(1)
      XMAX = X(NPOINT)
      MZ = MIN0(MANZ,7)
C
      WRITE(YNAMER,'(''RE'',A5,22X)') YNAME
      WRITE(YNAMEI,'(''IM'',A5,22X)') YNAME
C
      MMP  = (MANZ+1)/2
      MZP  = (MZ+1)/2
      MLOR = MZ/2
      IOP(MZP) = IOP7(4)
      IP(MZP)  = MMP
      DO 10 IM=1,MLOR
         IOP(MZP+IM) = IOP7(4+IM)
         IOP(MZP-IM) = IOP7(4-IM)
         IP(MZP+IM)  = MMP+IM
         IP(MZP-IM)  = MMP-IM
   10 CONTINUE
C
C ... REALTEIL ...
C
      IOMU = IPN + 1
      
      IF (IPN.GT.5) IOMU = IPN - 2

      ILR = 2
C
      YMIN = 0.0
      YMAX = 0.0
      DO 20 I  = 1,MZ
      DO 20 J  = 1,NPOINT
         IM = IP(I)
         IF (Y(J,1,IM).GT.YMAX) YMAX = Y(J,1,IM)
         IF (Y(J,1,IM).LT.YMIN) YMIN = Y(J,1,IM)
   20 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C PLOT THE EIGENFUNKTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMER,30,'S',1,' ',1)
      DO 30 I = 1,MZ
c---------------------- determine number of zeros in eigenfunction
        nzero = 0
        DO 35 j=2,npoint-1
          if (y(j,1,im)*y(j+1,1,im) .LT. 0) nzero = nzero + 1
   35   CONTINUE
c        write(20,*) ' harmonic : ',im,' number of zeros :  ',nzero
         IM = IP(I)
         CALL LPLOT(ILR,IOMU,IOP(I),X,Y(1,1,IM),-NPOINT,1,YNAMER,30,
     >              'S',1,' ',1)
   30 CONTINUE
C
      IF(IPN.EQ.1) THEN
         IY = 0
         DO 40 I=1,MZ
            IM = IP(I)
            M  = RFOUR(IM)
            WRITE(TITLE,'(''M='',I2,3X)') M
            IXA = 85+(I-1)*11*12
*IF IPP
            IPC = IOP(I)/10
*ELSE
            IPC = (IOP(I)-190001)/10
*ENDIF
            CALL DLCH(IXA,IY,' ',IPC,2)
            CALL DLCH(IXA+12,IY,' ',IPC,2)
            CALL DLCH(IXA+24,IY,' ',IPC,2)
            CALL DLCH(IXA+48,IY,TITLE,7,2)
   40    CONTINUE
         WRITE(EIGENW,'('' EIGENVALUE:'',1P,2E13.5)')EW
         EWEQ = EQNAME//EIGENW
         CALL DLCH(112,779,EWEQ,52,2)
      ENDIF
C----------------------------------------------------------------------
C IMAGINARY PART
C----------------------------------------------------------------------
      ILR = 3
C
      YMIN = 0.0
      YMAX = 0.0
      DO 50 I = 1,MZ
      DO 50 J = 1,NPOINT
         IM = IP(I)
         IF (Y(J,2,IM).GT.YMAX) YMAX = Y(J,2,IM)
         IF (Y(J,2,IM).LT.YMIN) YMIN = Y(J,2,IM)
   50 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C  PLOT THE EIGENFUNCTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMEI,30,'S',1,' ',1)
      DO 60 I = 1,MZ
         IM = IP(I)
         CALL LPLOT(ILR,IOMU,IOP(I),X,Y(1,2,IM),-NPOINT,1,YNAMEI,30,
     >              'S',1,' ',1)
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK DIAGXTAX
      SUBROUTINE DIAGXTAX (NBG,NGL,NB3,NZMA,X,ZMA,
     >                     BUFF,XTAX,PROZ,CSUM,CONAMAT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMMAX
*CALL COMGRID
C
      REAL     PROZ(NGL,NGL)
      COMPLEX  ZMA(NZMA,NZMA),BUFF(NBG,3*NBG)
      COMPLEX  X(*),XTAX(NGL,NGL)
      COMPLEX  CXDOTU,CSUM1,CSUM2,CSUM3,CSUM
C
      DO 10 I=1,NB3
      DO 10 J=1,NBG
         BUFF(J,I) = (0.,0.)
   10 CONTINUE
C
      DO 20 J=1,NGL
      DO 20 I=1,NGL
         XTAX(I,J)=(0.0,0.0)
   20 CONTINUE
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 80  NI = 1,NGINT
C     -------------------
C
      CALL CONAMAT(NI,NZMA,ZMA)
C
      DO 30 K = 1, NBG
      DO 30 L = 1, NZMA
         J = NBG + L
         BUFF(K,J) = BUFF(K,J) + ZMA(K,L)
   30 CONTINUE
C
      IXM1 = MAX0((NI-2)*NBG,0)
      IX   = (NI-1)*NBG
      IXP1 = NI*NBG
C
      DO 50 KK=1,NGL
      DO 50 JJ=1,NGL
         JA = (JJ-1)*MANZ*2 + 1
         KA = (KK-1)*MANZ*2 + 1
         JE = JJ*MANZ*2
         KE = KK*MANZ*2
         KANZ = KE-KA+1
C
         DO 40 J=JA,JE
C
            CSUM1 = CXDOTU(KANZ,BUFF(J,KA),      NBG,X(IXM1+KA),1)
            CSUM2 = CXDOTU(KANZ,BUFF(J,NBG+KA),  NBG,X(IX+KA),  1)
            CSUM3 = CXDOTU(KANZ,BUFF(J,2*NBG+KA),NBG,X(IXP1+KA),1)
C
            XTAX(JJ,KK) = XTAX(JJ,KK) + X(IX+J)*(CSUM1+CSUM2+CSUM3)
C
   40    CONTINUE
   50 CONTINUE
C
      DO 60  K = 1, NBG
      DO 60  L = 1, NB3
         BUFF(K,L) = (0.,0.)
   60 CONTINUE
C
      DO 70  K = NBG+1, NZMA
      DO 70  L = 1, NZMA
         J = K - NBG
         BUFF(J,L) = ZMA(K,L)
   70 CONTINUE
C
   80 CONTINUE
C     --------
C
      IXM1 = (NG-2)*NBG
      IX   = (NG-1)*NBG
C
      DO 100 KK=1,NGL
      DO 100 JJ=1,NGL
         JA = (JJ-1)*MANZ*2 + 1
         KA = (KK-1)*MANZ*2 + 1
         JE = JJ*MANZ*2
         KE = KK*MANZ*2
         KANZ = KE-KA+1
C
         DO 90 J=JA,JE
C
            CSUM1 = CXDOTU(KANZ,BUFF(J,KA),    NBG,X(IXM1+KA),1)
            CSUM2 = CXDOTU(KANZ,BUFF(J,NBG+KA),NBG,X(IX+KA),  1)
C
            XTAX(JJ,KK) = XTAX(JJ,KK) + X(IX+J)*(CSUM1+CSUM2)
C
   90    CONTINUE
  100 CONTINUE
C
C     ANTEILE DER GLEICHUNGEN IN PROZENT:
C
      CSUM = (0.0,0.0)
      SUM  = 0.0
      DO 110 J=1,NGL
      DO 110 I=1,NGL
         CSUM = CSUM+XTAX(I,J)
         SUM  = SUM+CABS(XTAX(I,J))
  110 CONTINUE
      DO 120 J=1,NGL
      DO 120 I=1,NGL
         PROZ(I,J) = 100.*CABS(XTAX(I,J))/SUM
  120 CONTINUE
      WRITE(NOUT,121) ((I,J,XTAX(I,J),PROZ(I,J),J=1,NGL),I=1,NGL)
      RETURN
C
  121 FORMAT(///(' (',2I3,')=',1P,2E12.4,' : ',E12.4))
      END
************************************************************************
*DECK DIAG5
      SUBROUTINE DIAG5
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR LANCZOS
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL COMPIO
*CALL CORE5
*CALL COMDIM5
*CALL LANCZOS
*CALL COMLAB
 
      INTEGER      L
      REAL         XMIN, XMAX, YMIN, YMAX
      REAL         X(NDIM5), Y(NDIM5)
      CHARACTER*10 XNAME, YNAME
      CHARACTER*24 TOPN
 
      EQUIVALENCE (GR(1), X(1))
      EQUIVALENCE (GC(1), Y(1))
 
      WRITE(NOUTE) LABEL, EQNAME
      WRITE(NOUTE) NCONV
      WRITE(NOUTE) XLIML, XLIMR, YLIMB, YLIMT
      WRITE(NOUTE) (CONEV(J),J=1,NCONV)
 
      WRITE(TOPN,'(''LANCZOS (VERSION '',A3,'') : '')') LABEL(1:3)
      LABEL = TOPN//EQNAME
      CALL LBLTOP(LABEL,34)
      CALL LBLBOT(' ',1)
 
      XNAME = 'RE(LAMBDA)'
      YNAME = 'IM(LAMBDA)'
 
C ... SPECTRUM ...
 
      XMIN = XLIML
      XMAX = XLIMR
      YMIN = YLIMB
      YMAX = YLIMT
      WRITE(NOUT,*)' SR.DIAG5 XMIN,XMAX,YMIN,YMAX ',XMIN,XMAX,YMIN,YMAX
 
      L = 1
      DO 10 I=1,NCONV
         IF((AIMAG(CONEV(I)).GE.YMIN).AND.(AIMAG(CONEV(I)).LE.YMAX).AND.
     >   (REAL(CONEV(I)).GE.XMIN).AND.(REAL(CONEV(I)).LE.XMAX)) THEN
            Y(L) = AIMAG(CONEV(I))
            X(L) = REAL(CONEV(I))
            L    = L + 1
         ENDIF
   10 CONTINUE
      L = L - 1
      WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
      IF(L.LT.1) RETURN
      WRITE(NOUT,12) (X(I),Y(I),I=1,L)
      REWIND NOUTP
      WRITE(NOUTP,12) (X(I),Y(I),I=1,L)
      CALL WRTEXT(-NOUTP)
 
      CALL NFRAME(11,1,1,XMIN,XMAX,YMIN,YMAX,'SPECTRUM',8,XNAME,10,
     >            YNAME,10)
      CALL LPLOT(11,1,1201,X,Y,-L,1,' ',1,' ',1,' ',1)
 
      RETURN
 
   11 FORMAT(1X,I5,' WERTE IM RAHMEN ',1P,2E12.4,2X,2E12.4,' GEPLOTTET')
   12 FORMAT(////////'  EIGENWERTE IM GEGEBENEN RAHMEN:'
     >       //(1X,1P,2E16.8,0P))
      END
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
      COMPLEX  CCDOTU
      CXDOTU = CCDOTU(N,CV1,INC1,CV2,INC2)
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
      COMPLEX  CCDOTC
      CXDOTC =  CCDOTC(N,CV1,INC1,CV2,INC2)
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

      REAL FUNCTION X02AJF(AJUNK)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C
C     RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
C     RETURNS  B**(1-P)  OTHERWISE
C
C     .. Executable Statements ..
      X02AJF =     1.1102230246252d-16    
      RETURN
      END
