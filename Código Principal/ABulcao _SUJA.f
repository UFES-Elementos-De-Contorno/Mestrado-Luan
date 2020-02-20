c************************************************
*     SUBROTINA QUE RESOLVE O PROBLEMA DE AUTOVALOR GENERALIZADO
C     [A]{X}=LAMBDA[B]{X}
C
C     SETEMBRO DE 1998 - ANDRE BULCAO
      SUBROUTINE AUTOV(A,B,N,NUM,NUM_2,PRINTE,N_NOS,CONT_UPRES)
C
      IMPLICIT NONE
      INTEGER I,J,K,N,PRINTE,IMP,CONT,NDIF,NUM(2000),N_NOS,NUM_2(2000)
     1,CONT_UPRES,VALOR
	integer CONT_OPT
      REAL*8 A(2000,2000),B(2000,2000),Y1(2000),AUT_VAL(2000)
     1,C(2000,2000),TERMO(2000),WR(2000),WI(2000),SOMA,wra(2000)
     1,MAUT_VET(2000,2000),B1(2000,2000),wr_pos(1)                    
     1,A1(2000,2000),C1(2000,2000),X(2000),Y(2000),XI(2000),YI(2000)
     1,XINIC(2000),YA(2000),PERCENT
     1,ATRAS,FRENTE,CHUTE,MAIOR
      COMMON /P_CONT/ X,Y
      COMMON /PIC/ XI,YI
*      
*
      IMP=6
c
*
*
C     FORMACAO DA MATRIZ C DO PROBLEMA REDUZIDO
C     [C]{X}=LAMBDA{X}
C     [B][C]=[A]
c     chamada da subrotina
      DO I=1,N
        DO J=1,N
          B1(I,J)=B(I,J)
          C(I,J)=A(I,J)
        ENDDO
      ENDDO
      call gaussj(B1,N,N,C,N,N)


*      DO J=1,N                   
*        WRITE(*,*) ' RESOLUCAO DO SISTEMA NUMERO ',J,' FALTAM- ',(N-J)
*       FORMACAO DO VETOR DE TERMOS INDEPENDENTE
*        DO I=1,N
*          TERMO(I)=A(I,J)
*        ENDDO
*       CHAMADA DA ROTINA DE RESOLCAO DO SISTEMA
*        CALL SLNPD(B,TERMO,N,PRINTE)
*       ALOCA A RESPOSTA DO SISTEMA NA MATRIZ C        
*        DO I=1,N
*          C(I,J)=TERMO(I)
*        ENDDO                                               
*      ENDDO                                         
*     
403     FORMAT(15(F15.3))
*     
*     ARMAZENAR A MATRIZ [C] ORIGINAL
      DO I=1,N
        DO J=1,N
          C1(I,J)=C(I,J)
        ENDDO
      ENDDO
      write(*,*) 'sistema reduzido montado'
*        
*     CHAMADA DA ROTINA QUE BALANCA A MATRIZ [C]
*     11.5 NUMERICAL RECIPES P. 476
      CALL balanc(C1,N)       
      write(*,*) 'matriz balanceada'
*
*
*     CHAMADA DA ROTINA DE REDUCAO DE HESSEMBERG [C1]
      CALL elmhes(C1,n)                                     
      write(*,*) 'matriz reduzida de Hessemberg'
*
*
*     CHAMADA DA ROTINA DO CALCULO DOS AUTOVALORES - QR 
      CALL hqr(C1,n,wr,wi)                                  
      write(*,*) 'autovalores calculados'
*                     
*     COMANDO DE IMPRESSAO DOS AUTOVALORES
*
c		write(*,*) 'wr(',I,')  = ',wr(i)
c		write(*,*) 'wr_pos(',CONT_OPT,')  = ',wr_pos(CONT_OPT)
c		write(*,*) 'wr(',j,')  = ',wr(j)
c		write(*,*) 'CONT_OPT  = ',CONT_OPT
c		write(*,*) 'wr_pos(',CONT_OPT,')  = ',wr_pos(CONT_OPT)
c		write(*,*) 'wr_pos(',j,')  = ',wr_pos(j)
c		write(*,*) 'wr_pos(',i,')  = ',wr_pos(i)
c		write(*,*) 'wra(',I,')  = ',wra(i)


      WRITE(IMP,*)'  '
      WRITE(IMP,*)'VALORES DE WR_POS ORIGINAIS ************************'

	CONT_OPT=0
	DO j=1,N
		IF (wr(j).GT.0) THEN
			CONT_OPT=CONT_OPT+1
			wr_pos(CONT_OPT)=wr_pos(CONT_OPT)+wr(j)
		END IF
  		write(imp,554)J,wr_pos(J)
	ENDDO

      WRITE(IMP,*)'  '
      WRITE(IMP,*)'VALORES DE WRA  ************************************'
      WRITE(IMP,*)'WRA ORIGINAL POSSUIA ',N,
	*' NUM., REDUZIDO PARA ',CONT_OPT,' NUM.'
	DO I=1,CONT_OPT
 		wra(i)=((1./wr_pos(i))**(0.5))
  		write(imp,554)i,wra(i)
554		format(3X,I4,f10.5)
	ENDDO

c      write(*,*) 'ja passou daqui 1'


      write(*,3301)
      write(IMP,3301)
3301  FORMAT(///,10X,' - AUTOVALORES - ',/,10X,'REAL',5X,'IMAGINARIO')
      DO I=1,N
        WRITE(*,*) I,WR(I),WI(I)
        WRITE(IMP,*) I,WR(I),WI(I)
      ENDDO

c      write(*,*) 'ja passou daqui'

*      pause ' '
*
C..........................................................
C     CALCULO DOS AUTOVETORES POR ITERACAO INVERSA
C
C     DETERMINACAO DO VALOR A SER CHUTADO PARA O AUTOVALOR
*     ORDENACAO DOS AUTOVALORES E RETIRADA DOS AUTOVALORES REPETIDOS
      WRITE(*,*) 'INICIO ITERACAO INVERSA' 
      CALL ORD(WR,WI,N,AUT_VAL,NDIF)

*     COMANDO DE IMPRESSAO DOS AUTOVALORES
      WRITE(IMP,3302)
      WRITE(*,3302)
3302  FORMAT(///,10X,' - AUTOVALORES DIFERENTES E REAIS e positivos - '
     1,/,10X,'REAL')
      DO I=1,NDIF
        WRITE(*,*) I,AUT_VAL(I)
        WRITE(IMP,*) I,AUT_VAL(I)
      ENDDO
	


*      pause ' '
*
      PERCENT=0.05
      IF(NDIF.GT.15)THEN
      valor=15    
      ELSE
      VALOR=NDIF
      ENDIF
      
*     CALCULO DOS PRIMEIROS 15 AUTOVETORES      
      DO K=1,VALOR

          IF (K.EQ.N) THEN
            ATRAS=abs(AUT_VAL(K)-AUT_VAL(K-1))
            CHUTE=AUT_VAL(K)+((-PERCENT)*ATRAS)
            GOTO 123
          ENDIF
          IF (K.EQ.1) THEN
            FRENTE=abs(AUT_VAL(K)-AUT_VAL(K+1))
            CHUTE=AUT_VAL(K)+((+PERCENT)*FRENTE)
            GOTO 123          
          ENDIF
          FRENTE=abs(AUT_VAL(K)-AUT_VAL(K+1))
          ATRAS=abs(AUT_VAL(K-1)-AUT_VAL(K))
          IF(FRENTE.GT.ATRAS)THEN
            CHUTE=AUT_VAL(K)+((-PERCENT)*ATRAS)
          ELSE
            CHUTE=AUT_VAL(K)+((+PERCENT)*FRENTE)
          ENDIF
123       CONTINUE 

*         WRITE(*,*)   'CHUTE DO AUTOVALOR=',CHUTE

*         CALCULO DA MATRIZ  [A1]=[C]-CHUTE[I]
          DO I=1,N

            DO J=1,N  
              IF (I.NE.J) THEN
                A1(I,J)=C(I,J) 
              ELSE
                IF(I.EQ.J)THEN 
                  A1(I,I)=C(I,I)-CHUTE
                ENDIF
              ENDIF
            ENDDO
          ENDDO

*         CRIACAO DO VETOR INICIAL
          DO I=1,N
            XINIC(I)=I
          ENDDO      
*
*         INICIO DA ITERACOES
          CONT=0
666       CONTINUE
          CONT=CONT+1

*         ARMAZENA XINIC EM YA
          DO I=1,N
            YA(I)=XINIC(I)
          ENDDO
	    write(*,*) 'ja passou daqui 4'
		
		
		
c		write(*,*) 'a(k,k) = ',a(100,100)
c		write(*,*) 'a(k,k) = ',a(100,100)
		write(*,*) 'N = ',N
		write(*,*) 'PRINTE = ',PRINTE

c          DO i=1,10
c			DO j=1,10  					   .
c				write(*,*) 'A1(',i,j,') = ',A1(i,j)
c			ENDDO 
c          ENDDO 

				        
*         RESOLUCAO DO SITEMA
          CALL SLNPD(A1,YA,N,PRINTE)


*         NORMALIZACAO DO VETOR YA
          SOMA=0.
          DO J=1,N
	      write(*,*) 'ja passou daqui 5'
            IF(ABS(YA(J)).GT.SOMA) THEN
              SOMA=ABS(YA(J))
            ENDIF          
          ENDDO 
          DO J=1,N
	      write(*,*) 'ja passou daqui 6'
            YA(J)=YA(J)/SOMA
          ENDDO
     
*         COMPARACAO ENTRE XINIC E YA 
*         SE - OS DOIS FOREM IGUAIS O PROCESSO ATINGIU A CONVERGENCIA
*         SENAO - SUBSTITUIR XINIC POR YA E FAZER NOVA ITERACAO
          MAIOR=0.
          DO J=1,N
*            WRITE(*,*) 'YA',YA(J),' XINIC',XINIC(J)
            IF(ABS(ABS(YA(J))-ABS(XINIC(J))).GT.MAIOR)THEN
              MAIOR=ABS(ABS(YA(J))-ABS(XINIC(J)))
            ENDIF
*            PAUSE '  '
          ENDDO                

          WRITE(*,23) AUT_VAL(K),k,ndif
23        FORMAT(////,' AUTOVALOR =',D16.8,2(5x,i3))          
          WRITE(*,*)   'CONTADOR=',CONT,'  MAIOR=',MAIOR 
          DO J=1,N
            XINIC(J)=YA(J)
            MAUT_VET(J,K)=YA(J)
          ENDDO 
*
          IF((MAIOR.GT.1E-11).AND.(CONT.LE.200)) GOTO 666
*
*          WRITE(IMP,*) 'CONTADOR=',CONT,'  MAIOR=',MAIOR           
*          WRITE(IMP,*) 'CALCULO DO AUTOVETOR - AUTOVALOR=',K,AUT_VAL(K)                   
*          DO I=1,N
*            WRITE(IMP,*) MAUT_VET(I,K) 
*          ENDDO
*        ENDIF
      ENDDO

 
c..........................................................
c     VERIFICACAO DOS AUTOVETORES PARA O PROBLEMA GENERALIZADO
*          
      DO K=1,VALOR    
*       PASSA O AUTOVETOR PARA XINIC
        DO I=1,N
          XINIC(I)=MAUT_VET(I,K)
        ENDDO        
        SOMA=1.
        call multi(n,A,XINIC,YA,SOMA) 
        SOMA=AUT_VAL(K)
        call multi(n,B,XINIC,Y1,SOMA)
c       verificacao da igualdade entre yA e y1
        WRITE(IMP,*) '   '
        WRITE(IMP,*) ' ITERACAO INVERSA  '
        WRITE(IMP,*) 'VERIFICACAO  AUTOVETOR -  AUTOVALOR',K,AUT_VAL(K)
*        WRITE(IMP,12)
*12    format(1X,'POSICAO',8X,'[A]{X}',8X,'LAMBDA[B]{X}',8X,'DIFERENCA')
        MAIOR=0.
        DO J=1,N
          IF(MAIOR.LT.(ABS((YA(J)-Y1(J)))))THEN
            MAIOR=(ABS((YA(J)-Y1(J))))
          ENDIF
*          WRITE(IMP,13) J,YA(J),Y1(J),(ABS((YA(J)-Y1(J))))
*13        FORMAT(3X,I3,3(3X,D16.8))
        ENDDO
        WRITE(IMP,*) 'MAIOR DIFERENCA NA RESPOSTA=',MAIOR
      ENDDO
*
*     IMPRESSAO DOS RESULTADOS
*     IMPRESSAO DOS AUTOVALORES
      WRITE(IMP,3332)
3332  FORMAT(///,70('#'),//,10X,' - AUTOVALORES DIFERENTES E REAIS - ',/
     1,10X,'REAL',10X,'FREQUENCIA')   
      DO I=1,VALOR                            
        IF(AUT_VAL(I).GT.0)THEN
          WRITE(IMP,*) I,AUT_VAL(I),SQRT(AUT_VAL(I))
        ELSE
          WRITE(IMP,*) I,AUT_VAL(I)                 
        ENDIF
      ENDDO
      WRITE(IMP,333)
333   FORMAT(///,10X,'AUTOVETORES',//)      
      DO K=1,VALOR
        IF(AUT_VAL(I).GT.0)THEN
          WRITE(IMP,258) K,SQRT(AUT_VAL(K))              
258       FORMAT(//,3X,'AUTOVETOR =',I3,3X,'FREQUENCIA=',F10.6,/
     1    ,10X,'NO',10X,'DESLOCAMENTO',/)          
        ELSE
          WRITE(IMP,268) K,AUT_VAL(K)              
268       FORMAT(//,3X,'AUTOVETOR =',I3,3X,'AUTOVALOR=',F10.6,/
     1    ,10X,'NO',10X,'DESLOCAMENTO',/)          
        ENDIF
          DO I=1,N
            J=NUM(I)     
            IF (J.LE.N_NOS) THEN
*             NO DO CONTORNO
              WRITE(IMP,248) J,X(J),Y(J),MAUT_VET(I,K) 
248           FORMAT(I3,3X,2(F10.4,3X),D16.10)
            ELSE
*             PONTOS INTERNOS CONSTITUTIVOS
              WRITE(IMP,248) J,XI(J-N_NOS),YI(J-N_NOS),MAUT_VET(I,K) 
            ENDIF
          ENDDO
*         IMPRIMIR OS NOS COM DESLOCAMENTO PRESCRITOS
          DO I=1,CONT_UPRES
            J=NUM_2(I)
            WRITE(IMP,248) J,X(J),Y(J),(0.) 
          ENDDO
          
                    
      ENDDO
*              
      STOP
      END
	
     










c	***********************************************************************************************************************************************************
      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL*8 a(2000,2000),b(2000,2000)
      PARAMETER (NMAX=2000)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL*8 big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj 1'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
c	  write(*,*) 'icol = ',icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj 2'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END      



     







c************************************************************************************************************************************************
      SUBROUTINE balanc(a,n)
      IMPLICIT NONE
      INTEGER n
      REAL*8 a(2000,2000),RADIX,SQRDX
      PARAMETER (RADIX=2.,SQRDX=RADIX**2)
      INTEGER i,j,last
      REAL*8 c,f,g,r,s
      WRITE(*,*) 'DENTRO DA ROTINA DE BALANCO'
1     continue
        last=1
        do 14 i=1,n
          c=0.
          r=0.
          do 11 j=1,n
            if(j.ne.i)then
              c=c+abs(a(j,i))
              r=r+abs(a(i,j))
            endif
11        continue
          if(c.ne.0..and.r.ne.0.)then
            g=r/RADIX
            f=1.
            s=c+r
2           if(c.lt.g)then
              f=f*RADIX
              c=c*SQRDX
            goto 2
            endif
            g=r*RADIX
3           if(c.gt.g)then
              f=f/RADIX
              c=c/SQRDX
            goto 3
            endif
            if((c+r)/f.lt.0.95*s)then
              last=0
              g=1./f
              do 12 j=1,n
                a(i,j)=a(i,j)*g
12            continue
              do 13 j=1,n
                a(j,i)=a(j,i)*f
13            continue
            endif
          endif
14      continue
      if(last.eq.0)goto 1
      return
      END












c************************************************************************************************************************************************
*     SUBROTINA DE REDUCAO DE HESSENBERG - NUMERICAL RECIPES 11.5 P 476      
      SUBROUTINE elmhes(a,n)
      IMPLICIT NONE
      INTEGER n
      REAL*8 a(2000,2000)
      INTEGER i,j,m
      REAL*8 x,y
      do 17 m=2,n-1
        x=0.
        i=m
        do 11 j=m,n
          if(abs(a(j,m-1)).gt.abs(x))then
            x=a(j,m-1)
            i=j
          endif
11      continue
        if(i.ne.m)then
          do 12 j=m-1,n
            y=a(i,j)
            a(i,j)=a(m,j)
            a(m,j)=y
12        continue
          do 13 j=1,n
            y=a(j,i)
            a(j,i)=a(j,m)
            a(j,m)=y
13        continue
        endif
        if(x.ne.0.)then
          do 16 i=m+1,n
            y=a(i,m-1)
            if(y.ne.0.)then
              y=y/x
              a(i,m-1)=y
              do 14 j=m,n
                a(i,j)=a(i,j)-y*a(m,j)
14            continue
              do 15 j=1,n
                a(j,m)=a(j,m)+y*a(j,i)
15            continue
            endif
16        continue
        endif
17    continue
      return
      END





         

c************************************************************************************************************************************************
*     SUBROTINA DE FATORACAO QR - NUMERICAL RECIPES P.484      
      SUBROUTINE hqr(C,n,wr,wi)
      IMPLICIT NONE
      INTEGER n
      REAL*8 a(2000,2000),wi(2000),wr(2000),C(2000,2000)
      INTEGER i,its,j,k,l,m,nn
      REAL*8 anorm,p,q,r,s,t,u,v,w,x,y,z
*     ARMAZENA A MATRIZ C ORIGINAL
      DO I=1,N
        DO J=1,N
          A(I,J)=C(I,J)
        ENDDO
      ENDDO
*
      anorm=0.
      do 12 i=1,n
        do 11 j=max(i-1,1),n
          anorm=anorm+abs(a(i,j))
11      continue
12    continue
      nn=n
      t=0.
1     if(nn.ge.1)then
        its=0
2       do 13 l=nn,2,-1
          s=abs(a(l-1,l-1))+abs(a(l,l))
          if(s.eq.0.)s=anorm
          if(abs(a(l,l-1))+s.eq.s)goto 3
13      continue
        l=1
3       x=a(nn,nn)
        if(l.eq.nn)then
          wr(nn)=x+t
          wi(nn)=0.
          nn=nn-1
        else
          y=a(nn-1,nn-1)
          w=a(nn,nn-1)*a(nn-1,nn)
          if(l.eq.nn-1)then
            p=0.5*(y-x)
            q=p**2+w
            z=sqrt(abs(q))
            x=x+t
            if(q.ge.0.)then
              z=p+sign(z,p)
              wr(nn)=x+z
              wr(nn-1)=wr(nn)
              if(z.ne.0.)wr(nn)=x-w/z
              wi(nn)=0.
              wi(nn-1)=0.
            else
              wr(nn)=x+p
              wr(nn-1)=wr(nn)
              wi(nn)=z
              wi(nn-1)=-z
            endif
            nn=nn-2
          else
            if(its.eq.100)pause 'too many iterations in hqr'
            if(its.eq.10.or.its.eq.20)then
              t=t+x
              do 14 i=1,nn
                a(i,i)=a(i,i)-x
14            continue
              s=abs(a(nn,nn-1))+abs(a(nn-1,nn-2))
              x=0.75*s
              y=x
              w=-0.4375*s**2
            endif
            its=its+1
            do 15 m=nn-2,l,-1
              z=a(m,m)
              r=x-z
              s=y-z
              p=(r*s-w)/a(m+1,m)+a(m,m+1)
              q=a(m+1,m+1)-z-r-s
              r=a(m+2,m+1)
              s=abs(p)+abs(q)+abs(r)
              p=p/s
              q=q/s
              r=r/s
              if(m.eq.l)goto 4
              u=abs(a(m,m-1))*(abs(q)+abs(r))
              v=abs(p)*(abs(a(m-1,m-1))+abs(z)+abs(a(m+1,m+1)))
              if(u+v.eq.v)goto 4
15          continue
4           do 16 i=m+2,nn
              a(i,i-2)=0.
              if (i.ne.m+2) a(i,i-3)=0.
16          continue
            do 19 k=m,nn-1
              if(k.ne.m)then
                p=a(k,k-1)
                q=a(k+1,k-1)
                r=0.
                if(k.ne.nn-1)r=a(k+2,k-1)
                x=abs(p)+abs(q)+abs(r)
                if(x.ne.0.)then
                  p=p/x
                  q=q/x
                  r=r/x
                endif
              endif
              s=sign(sqrt(p**2+q**2+r**2),p)
              if(s.ne.0.)then
                if(k.eq.m)then
                  if(l.ne.m)a(k,k-1)=-a(k,k-1)
                else
                  a(k,k-1)=-s*x
                endif
                p=p+s
                x=p/s
                y=q/s
                z=r/s
                q=q/p
                r=r/p
                do 17 j=k,nn
                  p=a(k,j)+q*a(k+1,j)
                  if(k.ne.nn-1)then
                    p=p+r*a(k+2,j)
                    a(k+2,j)=a(k+2,j)-p*z
                  endif
                  a(k+1,j)=a(k+1,j)-p*y
                  a(k,j)=a(k,j)-p*x
17              continue
                do 18 i=l,min(nn,k+3)
                  p=x*a(i,k)+y*a(i,k+1)
                  if(k.ne.nn-1)then
                    p=p+z*a(i,k+2)
                    a(i,k+2)=a(i,k+2)-p*r
                  endif
                  a(i,k+1)=a(i,k+1)-p*q
                  a(i,k)=a(i,k)-p
18              continue
              endif
19          continue
            goto 2
          endif
        endif
      goto 1
      endif
      return
      END












C************************************************************************************************************************************************
C     SUBROTINA QUE FAZ A ORDENACAO DOS AUTOVALORES EM ORDEM CRESCENTE
      SUBROUTINE ORD(WR,WI,N,AUT_VAL,CONT)
      IMPLICIT NONE
      REAL*8 WR(2000),WI(2000),CH,MENOR,AUT_VAL(2000)
      INTEGER I,J,N,P_M,CONT
C
      DO J=1,N
        MENOR=WR(J)
        DO I=J,N
*       VARIACAO AO LONGO DOS AUTOVALORES            
          IF(WR(I).LE.MENOR)THEN
            P_M=I
            MENOR=WR(I)
          ENDIF
        ENDDO
*       TROCA DE POSICAO            
        CH=WR(J)
        WR(J)=MENOR
        WR(P_M)=CH 
        CH=WI(J)
        WI(J)=WI(P_M)
        WI(P_M)=CH
      ENDDO
*     RETIRADA DOS AUTOVALORE REPETIDOS
      CONT=0
      DO J=1,N
        IF (WI(J).EQ.0) THEN
        IF (WR(J).GE.0) THEN
          IF (CONT.EQ.0) THEN
            CONT=CONT+1
            AUT_VAL(CONT)=WR(J)
            GOTO 3
          ENDIF
            IF (AUT_VAL(CONT).NE.WR(J)) THEN
              CONT=CONT+1
              AUT_VAL(CONT)=WR(J)
            ENDIF
3         CONTINUE
        ENDIF
        ENDIF
      ENDDO 
      RETURN
      END









C************************************************************************************************************************************************
	subroutine slnpd(a,b,n,nx) !(cc,dfi,nt,nx) (A1,YA,N,PRINTE)
      
      implicit none
	integer n1,n,k,k1,j,l,nx,i
	real*8 c,a(nx,nx),b(nx),d

	write(*,*) 'ja passou daqui 4.1'
	write(*,*) 'a(k,k) = ',a(10,10)

      n1=n-1
	write(*,*) 'n = ',n
	write(*,*) 'n1 = ',n1

      do 100 k=1,n1
		k1=k+1
		c=a(k,k)
	
		if (dabs(c)-0.000001)1,1,3
  1		do 7 j=k1,n
			if (dabs(a(j,k))-0.000001)7,7,5
  5			do 6 l=k,n
				c=a(k,l)
				a(k,l)=a(j,l)
  6				a(j,l)=c
				c=b(k)
				b(k)=b(j)
				b(j)=c
				c=a(k,k)
				go to 3
  7		continue
  8		write(6,2) k
  2		format('**** singularity in row',i5)
		d=0.
		go to 300
      
  3		c=a(k,k)
		write(*,*) 'dabs(c)3 = ',dabs(c)-0.000001
		
		
		do 4 j=k1,n
  4		a(k,j)=a(k,j)/c
		
		b(k)=b(k)/c
		
		do 10 i=k1,n
			c=a(i,k)
			do 9 j=k1,n
				a(i,j)=a(i,j)-c*a(k,j)
  9			continue
			b(i)=b(i)-c*b(k)
 10		continue	

100   continue

	write(*,*) 'ja passou daqui 4.2'

      if (dabs(a(n,n))-0.000001)8,8,101
		b(n)=b(n)/a(n,n)
101	continue

      do 200 l=1,n1
		k=n-l
		k1=k+1
		do 201 j=k1,n	
			b(k)=b(k)-a(k,j)*b(j)
201		continue 
200	continue

      d=1.
      do 250 i=1,n
250		d=d*a(i,i)
300   return
      end









C************************************************
      subroutine multi(n,a,b,c,LAMBDA)
c
      IMPLICIT NONE
      integer n,i,j
      REAL*8 soma,a(2000,2000),b(2000),c(2000),LAMBDA
c      
      do 2 i=1,n
        soma=0.
        do j=1,n
           soma=soma+A(i,j)*b(j)
        enddo
        c(i)=soma*lambda
c          write(*,*) 'c',i,'=',c(i)
2     continue
      return
      end
