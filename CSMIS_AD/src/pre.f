      PROGRAM PRE                                                               
CY   >            (SOURCE,FORTRAN,TAPE10=SOURCE,TAPE40=FORTRAN)                 
C                                                                               
************************************************************************        
*                                                                      *        
*   REVISE                                                 VERSION 3B  *        
*                          ***************                 28/11/91    *        
*     1B) : PRECOMPILE     *  S  -->  F  *                             *        
*           ===            ***************                             *        
*                                                                      *        
*   WRITE ACTIVE SOURCE LINES ON FORTRAN AND INSERT *COMDECK LINES     *        
*                                                                      *        
*   INPUT FILE,  UNIT=IOS  : SOURCE  (S)                               *        
*   OUTPUT FILE, UNIT=IOF  : FORTRAN (F)                               *        
*                                                                      *        
*   PARAMETER  : LMAX  - MAXIMUM NUMBER OF LINES IN SOURCE             *        
*              : NCMAX - MAXIMUM NUMBER OF LINES IN *COMDECK'S         *        
*              : NDMAX - MAXIMUM NUMBER OF *COMDECKS'S                 *        
*                                                                      *        
************************************************************************        
C                                                                               
      PARAMETER (LMAX  = 15000)                                                 
      PARAMETER (NCMAX = 1000)                                                   
      PARAMETER (NDMAX = 200)                                                   
      PARAMETER (IMAX = 10, JMAX = 9)                                           
      PARAMETER (IOS = 5, IOF = 6)                                            
C                                                                               
      CHARACTER*72 LINE                                                         
      CHARACTER*1  A(IMAX),AA(IMAX)                                             
      CHARACTER*8  B(JMAX),BB(JMAX)                                             
      DIMENSION IA(IMAX),IB(JMAX)                                               
C                                                                               
      COMMON /CAB/ AA,BB                                                        
      COMMON /CLB/ LB(JMAX)                                                     
C                                                                               
      CHARACTER*72 COMLINE(NCMAX)                                               
      CHARACTER*8  DNAME(NDMAX)                                                 
      DIMENSION LC(NDMAX),LD(NDMAX)                                             
C                                                                               
C     * SINGLE LINE ACTIVATION WITH $                                           
C     * (LETTERS Z - N TO BE USED AS A NEGATION OF A - M):                      
      DATA ( A(I),I=1,5)    /'A','F','I','J','K'/                               
      DATA (IA(I),I=1,5)    / 0 , 1 , 0 , 0 , 0 /                               
      DATA ( A(I),I=6,IMAX) /'Z','U','R','Q','P'/                               
      DATA (IA(I),I=6,IMAX) / 1 , 0 , 1 , 1 , 1 /                               
C                                                                               
C     * BLOCK LINE ACTIVATION WITH *IF/*ELSEIF/*ELSE/*ENDIF:                    
      DATA ( B(J),J=1,6)    /'FOM','IPP','JET','KUL','MFE','STA'/               
      DATA (IB(J),J=1,6)    /  0  ,  0  ,  1  ,  0  ,  0  ,  0  /               
      DATA ( B(J),J=7,JMAX) /'CRAY','IBM','SUN'/                                
      DATA (IB(J),J=7,JMAX) /   0  ,   1 ,  0  /                                
C                                                                               
      DO 10 I=1,IMAX                                                            
	 IF(IA(I).EQ.1) AA(I)=A(I)                                              
	 IF(IA(I).NE.1) AA(I)='-'                                               
   10 CONTINUE                                                                  
      DO 20 J=1,JMAX                                                            
	 IF(IB(J).EQ.1) BB(J)=B(J)                                              
	 IF(IB(J).NE.1) BB(J)='--------'                                        
	 LB(J)=INDEX(B(J)//' ',' ')-1                                           
   20 CONTINUE                                                                  
C                                                                               
CY    OPEN(UNIT=IOS,FILE='SOURCE' ,ERR=101)                                     
CY    OPEN(UNIT=IOF,FILE='FORTRAN',ERR=102)                                     
      REWIND(IOS)                                                               
C                                                                               
      ISW=1                                                                     
      ID=0                                                                      
      L=0                                                                       
C                                                                               
C     * CHECK FORMAT INPUT FILE.                                                
      READ(IOS,'(A)', END=103) LINE                                             
      I=1                                                                       
      IF(LINE(1:8).NE.'*COMDECK'.AND.LINE(1:5).NE.'*DECK') GOTO 103             
      GOTO 40                                                                   
C                                                                               
   30 CONTINUE                                                                  
C                                                                               
C        * READ SOURCE AND ACTIVATE BRANCHES.                                   
C                                                                               
	 READ(IOS,'(A)',END=105) LINE                                           
	 I=I+1                                                                  
   40    CALL ACT(LINE,IWRITE)                                                  
	 IF(IWRITE.NE.1) GOTO 30                                                
C                                                                               
	 IF(ISW.EQ.1) THEN                                                      
C                                                                               
C           * STORE *COMDECKS IN THE ARRAY COMLINE.                             
C                                                                               
	    IF(LINE(1:8).EQ.'*COMDECK') THEN                                    
	       ID=ID+1                                                          
	       LC(ID)=L+1                                                       
	       LD(ID)=0                                                         
	       K=INDEX(LINE(10:18),' ')-1                                       
	       DNAME(ID)=LINE(10:9+K)                                           
	    ELSEIF(LINE(1:5).EQ.'*DECK') THEN                                   
	       ISW=2                                                            
	       GOTO 30                                                          
	    ELSE                                                                
	       LD(ID)=LD(ID)+1                                                  
	       L=L+1                                                            
	       WRITE(COMLINE(L),'(A)') LINE(1:72)                               
	    ENDIF                                                               
C                                                                               
	 ELSE                                                                   
C                                                                               
C        * WRITE ACTIVATED LINES AND INSERT *COMDECK'S.                         
C                                                                               
	    IF(LINE(1:5).EQ.'*DECK') THEN                                       
C           ELSEIF(LINE(1:1).EQ.'C') THEN                                       
	    ELSEIF(LINE(1:5).EQ.'*CALL') THEN                                   
	       DO 50 J=1,NDMAX                                                  
		  K=INDEX(LINE(7:15),' ')-1                                     
		  IF(LINE(7:6+K).EQ.DNAME(J)) THEN                              
		     ND=J                                                       
		     GOTO 60                                                    
		  ENDIF                                                         
   50          CONTINUE                                                         
	       GOTO 104                                                         
   60          WRITE(IOF,'(A)') (COMLINE(LC(ND)+N-1),N=1,LD(ND))                
	    ELSE                                                                
	       WRITE(IOF,'(A)') LINE(1:72)                                      
	    ENDIF                                                               
C                                                                               
	 ENDIF                                                                  
C                                                                               
      IF(I.LT.LMAX) GOTO 30                                                     
C                                                                               
CY101 STOP 'PRE **ERROR OPENING SOURCE FILE'                                    
CY102 STOP 'PRE **ERROR OPENING FORTRAN FILE'                                   
  103 STOP 'PRE **ERROR: NO *COMDECKS OR *DECKS'                                
  104 STOP 'PRE **ERROR: MISSING *COMDECK'                                      
  105 STOP 'PRE'                                                                
      END                                                                       
C                                                                               
C ======================================================================        
C                                                                               
      SUBROUTINE ACT(LINE,IWRITE)                                               
C                                                                               
C     ******************************************************************        
C     * ACTIVATE LINES OF SOURCE FILE.                                 *        
C     ******************************************************************        
C                                                                               
      PARAMETER (IMAX = 10, JMAX = 9)                                           
C                                                                               
      CHARACTER*(*) LINE                                                        
      CHARACTER*1   AA(IMAX)                                                    
      CHARACTER*8   BB(JMAX)                                                    
C                                                                               
      COMMON /CAB/ AA,BB                                                        
      COMMON /CLB/ LB(JMAX)                                                     
C                                                                               
      SAVE IACT                                                                 
C                                                                               
      DATA IACT / 1 /                                                           
C                                                                               
      IWRITE=0                                                                  
C                                                                               
      IF(LINE(1:1).EQ.'$') THEN                                                 
C                                                                               
C        * ACTIVATE AND WRITE SINGLE LINES.                                     
C                                                                               
	 DO 10 I=1,IMAX                                                         
	    IF(LINE(2:2).EQ.AA(I)) THEN                                         
	       LINE(1:2)=' '                                                    
	       IWRITE=1                                                         
	       GOTO 40                                                          
	    ENDIF                                                               
   10    CONTINUE                                                               
C                                                                               
      ELSEIF(LINE(1:2).EQ.'*I'.OR.LINE(1:2).EQ.'*E') THEN                       
C                                                                               
C        * ACTIVATE BLOCK LINES.                                                
C                                                                               
	 IF(LINE(1:4).EQ.'*IF ') THEN                                           
	    IACT=0                                                              
	    K=INDEX(LINE(5:72),' ')-1                                           
	    DO 20 J=1,JMAX                                                      
	       KK=INDEX(LINE(5:4+K),BB(J)(1:LB(J)))                             
	       IF(KK.NE.0) THEN                                                 
		  IACT=1                                                        
		  GOTO 40                                                       
	       ENDIF                                                            
   20       CONTINUE                                                            
	 ELSEIF(LINE(1:8).EQ.'*ELSEIF ') THEN                                   
	    IF(IACT.EQ.1) THEN                                                  
	       IACT=0                                                           
	       GOTO 40                                                          
	    ELSE                                                                
	       K=INDEX(LINE(9:72),' ')-1                                        
	       DO 30 J=1,JMAX                                                   
		  KK=INDEX(LINE(9:8+K),BB(J)(1:LB(J)))                          
		  IF(KK.NE.0) THEN                                              
		     IACT=1                                                     
		     GOTO 40                                                    
		  ENDIF                                                         
   30          CONTINUE                                                         
	    ENDIF                                                               
	 ELSEIF(LINE(1:5).EQ.'*ELSE') THEN                                      
	    IF(IACT.EQ.0) THEN                                                  
	       IACT=1                                                           
	    ELSE                                                                
	       IACT=0                                                           
	    ENDIF                                                               
	 ELSEIF(LINE(1:6).EQ.'*ENDIF') THEN                                     
	    IACT=1                                                              
	 ENDIF                                                                  
C                                                                               
      ELSEIF(IACT.EQ.1) THEN                                                    
C                                                                               
C        * WRITE NORMAL AND ACTIVATED BLOCK LINES.                              
C                                                                               
	 IWRITE=1                                                               
C                                                                               
      ENDIF                                                                     
C                                                                               
   40 RETURN                                                                    
C                                                                               
      END                                                                       
