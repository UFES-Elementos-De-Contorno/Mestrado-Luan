
n1      program main
c
c	programa com elementos lineares para interpolação do problema de HELMHOTZ
c	 versão com calculo dA MAtriz através de uso direto de funções radiais	MECIC
c     com introdução de termo regularizador, evitando duas numens de pontos

	implicit none
      common ne,np,l,lec,imp,npi,nt
	integer nx,lec,imp
	parameter (nx=2000)
 	INTEGER CONT_UPRES,CONT_QPRES,CONT_INTF,NUM(2000),NUM_2(2000)
	integer nop(nx,3),kode(nx),locc(nx),idup(nx),ne,np,l,npi,nt,printe
 	real*8 x(nx),y(nx),g(nx,nx),fi(nx),dfi(nx),xxc(nx)
	real*8 yyc(nx),sol(nx),h(nx,nx),dsolx(nx),dsoly(nx)
	real*8 a(nx,nx),b(nx,nx),cb(nx),xx(nx),yy(nx),d,dist
	real*8 tempo,calcula_tempo
	character sai*20,tempo1*10,tempo2*10
	lec=5
	imp=6
	printe=1
      call input(xxc,yyc,x,y,kode,fi,nop,locc,Idup,nx,sai)
c	write(imp,*)'vou entrar na fmat'
c
c		CALCULO DO TEMPO DE PROCESSAMENTO
c
      call date_and_time(TIME=tempo1)

      call fmat(x,y,g,h,fi,dfi,kode,nop,Idup,a,b,cb,xx,yy,xxc,yyc,nx)

      nt=np+npi 

      call inter(fi,dfi,kode,xxc,yyc,x,y,sol,nop,dsolx,dsoly,locc,a,b,
     *cb,xx,yy,nx)

c
c		CALCULO DO TEMPO DE PROCESSAMENTO
c
      call date_and_time(TIME=tempo2)      
      tempo=calcula_tempo(tempo1,tempo2)

      call output (x,y,fi,dfi,xxc,yyc,sol,dsolx,dsoly,nx,sai,tempo)
      stop
      end

	real*8 function calcula_tempo(tempo1,tempo2)
	implicit none
	real*8 temp1(4),temp2(4)
	character tempo1*10,tempo2*10,aux*2,aux2*3

	aux=tempo1(1:2)
	read(aux,*) temp1(1)
	aux=tempo1(3:4)
	read(aux,*) temp1(2)
	aux=tempo1(5:6)
	read(aux,*) temp1(3)
	aux2=tempo1(8:10)
	read(aux2,*) temp1(4)
	aux=tempo2(1:2)
	read(aux,*) temp2(1)
	aux=tempo2(3:4)
	read(aux,*) temp2(2)
	aux=tempo2(5:6)
	read(aux,*) temp2(3)
	aux2=tempo2(8:10)
	read(aux2,*) temp2(4)
	if (temp1(1).gt.temp2(1)) then
	  temp2(1)=temp2(1)+24
	end if
	temp1(1)=temp1(1)*3600+temp1(2)*60+temp1(3)+temp1(4)/1000
	temp2(1)=temp2(1)*3600+temp2(2)*60+temp2(3)+temp2(4)/1000
	calcula_tempo=temp2(1)-temp1(1)
	end

      subroutine input(xxc,yyc,x,y,kode,fi,nop,locc,Idup,nx,sai)

	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer nx,lec,imp,ne,np,npi,l,nt,i,locc(nx),kode(nx),j
	integer idup(nx),nop(nx,3),k
	real*8 xxc(nx),yyc(nx),x(nx),y(nx),fi(nx),ra,teta,pe,xs(nx,nx)
	character title*18,arqent*20,arqout*20,sai*20


c	open(unit=1,file='entrada.txt',status='old')
c	read(1,7) ARQENT
c	read(1,7) ARQOUT
c	read(1,7) sai
c	close(unit=1,status='delete')
c7	format (a20)

	write(*,'(A\)')' INFORME NOME DO ARQUIVO DE ENTRADA-->'
      read(*,'(A20)')ARQENT
      open(LEC,FILE=ARQENT)
      write(*,'(A\)')' INFORME NOME DO ARQUIVO DE SAIDA --->'
      read(*,'(A20)')ARQOUT
      open(IMP,FILE=ARQOUT)
c

      write(imp,100)
100   format(' ',120('*'))

      read(lec,150) title
150   format(18a4)
      write(imp,250) title
250   format(25x,18a4)

	read(lec,200) ne
	read(lec,200) np
	read(lec,200) npi
      read(lec,200) l
200   format(i5)
      nt=np+npi
      write(imp,300) ne,np,npi,l
300   format(//'data'//2x,'number of boundary elements=',
     1i3//,2x,'number of functional nodes=',i3//,2x,
     2'number of internal points where the function is calculated='
     3,i4,//,2x,'number of recursive points located on the boundary='
	4,i3,//)

      write(*,*) ne,np,npi,l
c
 	if (l.eq.0)go to 1
 	write(imp,350)
350   format (//2x,'coordinates of the auxiliary internal points' 
     1,//4x,'point',10x,'x',18x,'y')

      do 1 i=1,l
      read(lec,400) xxc(i),yyc(i)
400   format(2f12.4)
	write(imp,450) i,xxc(i),yyc(i)
450   format(5x,i3,5x,e14.7,5x,e14.7)
  1   CONTINUE

      write(imp,500)
500   format (//2x,'coordinates of the extreme points of the boundary el
     1ements',//4x,'point',10x,'x',18x,'y')
	nt=np+npi
      do 750 i=1,nt
      read(lec,600)i, x(i),y(i)
600   format(i4,2f12.6)
	write(imp,700) i,x(i),y(i)
c	write(imp,700) x(i),y(i)
c	write(imp,700) i,(x(i)-0.02),y(i)
c700   format(5x,i4,2(5x,e14.7))
700   format(i4,2f12.6)
c700   format(2f12.6)

750	continue
c	do 751 i=1,12
c	do 751 j=1,12 
c	xs(i,j)=3.1415926*((i*i)+(j*j/0.0625))**0.5
c	xs(i,j)=3.1415926*((i*i)+(j*j))**0.5

c	write(imp,*)i,j,xs(i,j)
751   continue
c	pe=4.
c	k=0
1212  continue
c      do 50 i=1,nt
c	do 50 k=1,nt
c	if(i.eq.k) go to 752
c	ra=(x(i)-x(k))**2+(y(i)-y(k))**2
c	if(ra.ne.0)go to 752
c	write(imp,*)'i=',i,'k=',k
752   continue
50    continue	 
c	teta=2*3.1415926*pe/360
c	if(teta.ge.1.04)go to 1213
c	ra=50.
c	do 751 i=1,49
c	k=k+1
c	ra=ra+1.0
c	xs(k)=ra*cos(teta)
c	ys(k)=ra*sin(teta)
c	write(imp,601)k+268, xs(k),ys(k)
c	write(imp,*)'pe=',pe
601   format(i4,52f12.4)
c751   continue
c      pe=pe+2.5
c	go to 1212
1213  continue
c      write(imp,*)k 
      write(imp,800)
800   format(//2x,'boundary conditions'//5x,'node',6x,'code',
     15x,'prescribed value')
      do 20 i=1,np
      read(lec,900) kode(i),Idup(i),fi(i)
900   format(I5,i6,f12.4)
 20   write(imp,950) i,kode(i),Idup(i),fi(i)
950   format (5x,i3,8x,I1,8x,I3,8x,e14.7)

	write(imp,880)
880   format(//,2x,'incidence of geometric nodes'//,5x,' first node',
     16x,'second node',/)
      do 261 i=1,ne
      read(lec,920) nop(i,1),nop(i,2)
920   format(i5,i5)
	write(imp,960) i,nop(i,1),nop(i,2)
960   format(5x,i3,8x,i3,5x,i3)
261   continue

      return
      end

	subroutine slnpd(a,b,n,nx)
      
      implicit none
	integer n1,n,k,k1,j,l,nx,i
	real*8 c,a(nx,nx),b(nx),d

      n1=n-1
      do 100 k=1,n1
      k1=k+1
      c=a(k,k)
      if (dabs(c)-0.000001)1,1,3
  1   do 7 j=k1,n
      if (dabs(a(j,k))-0.000001)7,7,5
  5   do 6 l=k,n
      c=a(k,l)
      a(k,l)=a(j,l)
  6   a(j,l)=c
      c=b(k)
      b(k)=b(j)
      b(j)=c
      c=a(k,k)
      go to 3
  7   continue
  8   write(6,2) k
  2   format('**** singularity in row',i5)
      d=0.
      go to 300
      
  3   c=a(k,k)
      do 4 j=k1,n
  4   a(k,j)=a(k,j)/c
      b(k)=b(k)/c
      do 10 i=k1,n
      c=a(i,k)
      do 9 j=k1,n
  9   a(i,j)=a(i,j)-c*a(k,j)
 10   b(i)=b(i)-c*b(k)
100   continue

      if (dabs(a(n,n))-0.000001)8,8,101
  
101   b(n)=b(n)/a(n,n)

      do 200 l=1,n1
      k=n-l
      k1=k+1
      do 200 j=k1,n
200   b(k)=b(k)-a(k,j)*b(j)

      d=1.
      do 250 i=1,n
250   d=d*a(i,i)
300   return
      end
    

      subroutine fmat(x,y,g,h,fi,dfi,kode,nop,Idup,a,b,cb,xx,yy,xxc,yyc,
	*nx)
      implicit none
	common ne,np,l,lec,imp,npi,nt
	INTEGER CONT_UPRES,CONT_QPRES,CONT_INTF,NUM(2000),NUM_2(2000)
 	integer nx,nop(nx,3),idup(nx),l1,l3,i,ne,l2,l4,np,imp
	integer npi,nt,j,ng,l,lec,kode(nx),con1,con2,k,n1,n2,printe
	integer IUU,JUU,IQQ,JQQ,IUQ,JUQ,IQU,JQU,KA,KB
	real*8 xd(nx),x(nx),yd(nx),y(nx),xx(nx),yy(nx)
	real*8 g(nx,nx),h(nx,nx),h1,h2,g1,g2,dx1,dx2
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,ta,ra,c(nx,nx),fdo(nx),ge
	real*8 p(nx,nx),cd(nx),d,r,tok(nx,nx),alfa(nx,nx),teta
	real*8 gg,oneta(nx),ch,dfi(nx),cb(nx),fi(nx),q(nx,nx),hg1
	real*8 a(nx,nx),b(nx,nx),cc(nx,nx),dc(nx),ag,k1,k2,pi,hgk(nx,nx)
	real*8 slamb(nx),ulamb(nx,nx),ci(nx,nx),xxc(nx),yyc(nx),dist,ala
	real*8 ff(nx,nx),ggm(nx,nx),HUU(NX,NX),HQQ(NX,NX),HQU(NX,NX)
	REAL*8 HUQ(NX,NX),CCUQ(NX,NX),CCQU(NX,NX),CCUU(NX,NX),CCQQ(NX,NX)
	REAL*8 GUQ(NX,NX),GQU(NX,NX),GUU(NX,NX),GQQ(NX,NX),CCBAR(NX,NX)
	REAL*8 DOMA(NX,NX),DOMB(NX,NX),DOMC(NX,NX),DOMD(NX,NX),HBAR(NX,NX)
	REAL*8 TAK(NX,NX),eigv(nx)
	parameter (pi=3.141592)
c

	printe=1

c
c	 rotina para afastar os pontos nodais duplos da quina das arestas
c
	dist=0.05 !porcentagem do afastamento do elemento da aresta
	kode(0)=1
	do 156 i=1,ne
	l1=nop(i,1)
	l3=idup(l1)
	if (l3.eq.0) go to 153
	l2=nop(i,2)
	x(l1)=x(l1)+dist*(x(l2)-x(l1))
	y(l1)=y(l1)+dist*(y(l2)-y(l1))
c	ala=kode(l1)+kode(l3)
c	if(ala.ne.0) go to 152
c152     continue
c      go to 154
153     continue
154     continue
      l2=nop(i,2)
	l4=idup(l2)
	if (l4.eq.0) go to 155
	x(l2)=x(l2)-dist*(x(l2)-x(l1))
	y(l2)=y(l2)-dist*(y(l2)-y(l1))
c	ala=kode(l2)+kode(l4)
c	if (ala.ne.0) go to 155
155     continue
156     continue

      WRITE(IMP,*)'NOVAS COORDENADAS DOS nos duplos'
	DO 17 i=1,NP
	WRITE(IMP,181)X(I),Y(I)
 181    FORMAT(2(3X,F8.4))
 17    CONTINUE
c
c      formação das matrizes H, G e do termo independente HGK tipo Galerkin
c
      do 10 i=1,Nt
      do 10 j=1,Nt
      g(i,j)=0
 10   h(i,j)=0
  	DO 12 I=1,NP
	DO 18 J=1,NE
	L1=NOP(J,1)
	L2=NOP(J,2)
  	IF(I.EQ.L1)GO TO 20
	IF(I.EQ.L2)GO TO 22
      call inte(x(i),y(i),x(L1),y(L1),x(L2),y(L2),h1,h2,g1,g2,dx1,
     *dx2,dy1,dy2,ex1,ex2,ey1,ey2) 
      h(i,L2)=h(i,L2)+h2
      h(i,L1)=h(i,L1)+h1
      g(i,L2)=g(i,L2)+g2
      g(i,L1)=g(i,L1)+g1

c     matriz hgk: é uma matriz diagonal gerada pela regularização
c     e formada pela soma dos componentes das linhas 
 
	call galerk1(x(i),y(i),x(L1),y(L1),x(L2),y(L2),hg1)  
	hgk(i,i)=hgk(i,i)+hg1
	GO TO 26
  20  continue
      call inlo(x(L1),y(L1),x(L2),y(L2),g1,g2)
      g(i,L1)=g(i,L1)+g1
	h(i,L1)=h(i,L1)+0.0
	g(i,L2)=g(i,L2)+g2
	h(i,L2)=h(i,L2)+0.0
	go to 26
  22  continue
      call inlo(x(L1),y(L1),x(L2),y(L2),g1,g2)
	g(i,L1)=g(i,L1)+g2
 	h(i,L1)=h(i,L1)+0.0
	g(i,L2)=g(i,L2)+g1
	h(i,L2)=h(i,L2)+0.0
  26  continue
 18   continue
 12   continue
c  
c	 construção dos termos da diagonal de h
c 
 	DO 29 I=1,NP
  	ta=0.0
	DO 28 J=1,NP
	ta=ta+h(i,j) 
 28   continue
      h(i,i)=-ta
 29   continue

c
c 	 montagem da matriz de interpolação de termos de domínio
c      
c	ng=np+l
	continue
      do 23 i=1,nt
	do 23 j=1,nt
 	ra=dsqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      if(ra.eq.0)go to 222
c	c(i,j)=(1.-ra/1.41)**3
c	c(i,j)=(1.-ra/2.13)**3-0*ra*ra*dlog(ra)
c	c(i,j)=(1.-ra/2.)**3-0*ra*ra*dlog(ra)
c      c(i,j)=(1.-ra)*(1.-ra)*dlog(ra)
	c(i,j)=ra*ra*dlog(ra)
c	c(i,j)=ra
	go to 23
c222	c(i,j)=(1.-ra/2.13)**3
c222	c(i,j)=1.
222	c(i,j)=0.
23    continue
c
c	write(imp,*)'matriz h'
c      do 5423 i=1,nt
c    	write(imp,5410)(h(i,j),j=1,nt)
5423   continue
c
c	 inversão da matriz de interpolação
c
	call minv(c,nt)
c
c	introdução de pontos internos constitutivos	- pontos fonte - em H e G
c
c
      IF (NPI.EQ.0)GO TO 132 
	do 55 i=np+1,nt
	do 55 j=1,ne
	l1=nop(j,1)
	l2=nop(j,2)
      call inte(x(i),y(i),x(L1),y(L1),x(L2),y(L2),h1,h2,g1,g2,
     *dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2)
c     matriz hgk: é uma matriz diagonal gerada pela regularização
c     e formada pela soma dos componentes das linhas 
      call galerk1(x(i),y(i),x(L1),y(L1),x(L2),y(L2),hg1)  
 	hgk(i,i)=hgk(i,i)+hg1
	h(i,L2)=h(i,L2)+h2
      h(i,L1)=h(i,L1)+h1
      g(i,L2)=g(i,L2)+g2
      g(i,L1)=g(i,L1)+g1
55    continue

c
c	 diagonais de H	e G
c
	do 58 i=np+1,nt
	h(i,i)=1.
	g(i,i)=0.
58    continue
132   continue
134   continue

c
c	 impressão de matrizes gerais
c
c     	write(imp,*)'matriz c invertida geral'
c      do 5420 i=1,nt
c    	write(imp,5410)(c(i,j),j=1,nt)
5410   format(10f9.4)
5420   continue
C
c	  write(imp,*)'matriz h geral'
c      do 5450 i=1,nt
c	  write(imp,5460)(h(i,j),j=1,nt)
5460   format(10f9.4)
5450   continue
c
c	 write(imp,*)'matriz hgk geral'
c      do 5455 i=1,nt
c	   write(imp,5460)(hgk(i,j),j=1,nt)
5455   continue
c
c	 varredura nos pontos de interpolação i até ne; os pontos csi neste "do" inicial 
c      vão até o número de pontos nodais np
c 
	do 24 i=1,nt
	do 24 j=1,nt
	ra=dsqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
	if(ra.eq.0)go to 25
      ulamb(i,j)=-dlog(ra)/(2*3.1415926)
	go to 244
25    ulamb(i,j)=0.
244   continue  
24    continue
c
c		 write(imp,*)'matriz ulamb'
c      do 5457 i=1,nt
c	   write(imp,5460)(ulamb(i,j),j=1,nt)
5457   continue

c
c	 geração das integrais de neta (controle i)
c
	DO 34 I=1,Nt
 	DO 34 J=1,NE
	L1=NOP(J,1)
	L2=NOP(J,2)
c	write(imp,*)'i=',i
c	write(imp,*)'j=',j
c	write(imp,*)'l1=',l1
c	write(imp,*)'l2=',l2
c  	IF(I.EQ.l1)GO TO 21
c	if(i.eq.l2)go to 21
      call inte1(x(i),y(i),x(L1),y(L1),x(L2),Y(L2),gg)
c	write(imp,*)'gg',gg
     	GO TO 36
  21  continue
      gg=0.
  36  CONTINUE
c
c	neta é função do ponto base, coincidente com os pontos fonte neste caso e do elemento NE (gama) de integração
c
	oneta(I)=oneta(i)+gg
34    continue

c
C     Dos produtos finais
C
      Do 35 j=1,nt
      Do 35 k=1,nt
      slamb(j)=slamb(j)+oneta(k)*c(k,j)
 35   continue

c 	 write(imp,*)'matriz slamb(j)'
c 	  write(imp,5460)(slamb(j),j=1,nt)
c

c
c	produto slamb pela matriz diagonal com termo singular
c
      do 42 i=1,nt
      do 42 j=1,nt
 	p(i,j)=slamb(j)*ulamb(j,i)
	a(i,i)=a(i,i)+p(i,j)
42    continue
c
c
c	 write(imp,*)'matriz p(i,j)'
c      do 5477 i=1,nt
c	  write(imp,5460)(p(i,j),j=1,nt)
5477   continue
c
c		 write(imp,*)'matriz a(i,j)'
c      do 5479 i=1,nt
c	  write(imp,5460)(a(i,j),j=1,nt)
5479   continue

c
c	 matriz cc 
c
      do 112 i=1,nt
	do 112 j=1,nt

	CC(i,j)=p(i,j)-a(i,j)+hgk(i,j)
c	CC(i,j)=p(i,j)
112   continue
c
c		 write(imp,*)'matriz final cc(i,j)'
c      do 5478 i=1,nt
c	  write(imp,5460)(cc(i,j),j=1,nt)
5478   continue
c	do 3105 i=1,nt
c	do 3105 j=1,nt
c	do 3105 k=1,nt
c 	h(i,j)=h(i,j)+h(k,i)*h(k,j)
c	g(i,j)=g(i,j)+h(k,i)*g(k,j)
c	cc(i,j)=cc(i,j)+h(k,i)*cc(k,j)
c3105  continue


115   continue


 	JUU=0 
	JQQ=0
	DO 422 J=1,Nt
	IUU=0
	IQU=0
	IUQ=0
	IQQ=0
	KA=KODE(J)
	IF(J.LE.NP)GO TO 367
	KA=1
367	IF(KA.EQ.1)GO TO 39
	JUU=JUU+1
	DO 38 I=1,Nt
	KB=KODE(I)
	IF(I.LE.NP)GO TO 377
 	KB=1
377 	IF(KB.EQ.1)GO TO 366
	IUU=IUU+1
	HUU(IUU,JUU)=H(I,J)
	CCUU(IUU,JUU)=CC(I,J)
	GUU(IUU,JUU)=G(I,J)
	GO TO 37
366	CONTINUE
	IQU=IQU+1
	HQU(IQU,JUU)=H(I,J)
	CCQU(IQU,JUU)=CC(I,J)
	GQU(IQU,JUU)=G(I,J)
37	CONTINUE
38    CONTINUE
	GO TO 413
39 	JQQ=JQQ+1
	DO 412 I=1,Nt
	KB=KODE(I)
	IF(I.LE.NP)GO TO 397
	KB=1
397	IF(KB.EQ.0)GO TO 40
	IQQ=IQQ+1
	HQQ(IQQ,JQQ)=H(I,J)
	CCQQ(IQQ,JQQ)=CC(I,J)
	GQQ(IQQ,JQQ)=G(I,J)
      GO TO 41 
40	CONTINUE
	IUQ=IUQ+1
	HUQ(IUQ,JQQ)=H(I,J)
	CCUQ(IUQ,JQQ)=CC(I,J)
	GUQ(IUQ,JQQ)=G(I,J)
41	CONTINUE
412   CONTINUE
413   CONTINUE
422   CONTINUE

C
C	  CRIAÇÃO DAS MATRIZES AUXILIARES DE AUTOVALOR
C
	CALL MINV(GUU,JUU)


	do 3102 i=1,JUU
	do 3102 j=1,JQQ
	do 3102 k=1,JUU
	doma(i,J)=doma(i,J)+GUU(i,K)*HUQ(K,J)
	domb(i,J)=domb(i,J)+GUU(i,K)*CCUQ(K,J)
3102  continue
	do 3103 i=1,JQQ
 	do 3103 j=1,JQQ
	do 3103 k=1,JUU
	domC(i,J)=domC(i,J)+GQU(i,K)*DOMA(K,J)
	domD(i,J)=domD(i,J)+GQU(i,K)*DOMB(K,J)
3103  continue
	do 3104 i=1,JQQ
 	do 3104 j=1,JQQ
	HBAR(I,J)=HQQ(I,J)-domC(I,J)
	CCBAR(I,J)=CCQQ(I,J)-DOMD(I,J)
3104  continue
	
c       CHAMADA DA ROTINA DE RESOLUCAO DO PROBLEMA DE AUTOVALOR GENERALIZADO
C     [A]{X}=LAMBDA[B]{X}
c

	CALL AUTOV(CCBAR,HBAR,JQQ,fi,dfi,1,nt,juu)
	
 

C	CALL AUTOV(TOK,TAK,JQQ,fi,dfi,1,nt,juu)
C	call jacobi(Nt,HBAR,ccbar,EIGV,fi)


      return
      end

      subroutine inte(xp,yp,x1,y1,x2,y2,h1,h2,g1,g2,dx1,dx2,dy1,dy2,
 	*ex1,ex2,ey1,ey2)  
      implicit none  
	common ne,np,l,lec,imp,npi,nt
	integer i,ne,np,l,lec,imp,npi,nt
	real*8 gi(20),ome(20),ax,x1,x2,bx,ay,y1,y2,by,comp,h1,h2
	real*8 g1,g2,dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2,pi2,xco(20)
	real*8 yco(20),ra,xp,yp,aux,h,g,cjac,gx,gy,hx,hy,sen,cos


	 GI(1)=0.993128599185094
       GI(2)=-GI(1)
       GI(3)=0.963971927277913
       GI(4)=-GI(3)
       GI(5)=0.912234428251325
       GI(6)=-GI(5)
       GI(7)=0.839116971822218
       GI(8)=-GI(7)
       GI(9)=0.746331906460150
       GI(10)=-GI(9)
       GI(11)=0.636053680726515
       GI(12)=-GI(11)
       GI(13)=0.510867001950827
       GI(14)=-GI(13)
       GI(15)=0.373706088715419
       GI(16)=-GI(15)
       GI(17)=0.227785851141645
       GI(18)=-GI(17)
       GI(19)=0.076526521133497
       GI(20)=-GI(19)
*
*      pesos de Gauss
*
       OME(1)=0.017614007139152
       OME(2)=OME(1)
       OME(3)=0.040601429800386
       OME(4)=OME(3)
       OME(5)=0.062672048334109
       OME(6)=OME(5)
       OME(7)=0.083276741576704
       OME(8)=OME(7)
       OME(9)=0.101930119817240
       OME(10)=OME(9)
       OME(11)=0.118194531961518
       OME(12)=OME(11)
       OME(13)=0.131688638449176
       OME(14)=OME(13)
       OME(15)=0.142096109318382
       OME(16)=OME(15)
       OME(17)=0.149172986472603
       OME(18)=OME(17)
       OME(19)=0.152753387130725
       OME(20)=OME(19)


      ax=(x2-x1)/2
      bx=(x2+x1)/2
      ay=(y2-y1)/2
      by=(y1+y2)/2
	comp=2*dsqrt(ax**2+ay**2)
	cjac=comp/2
      SEN=(X1-X2)/COMP
      COS=(Y2-Y1)/COMP
		     
      h1=0
      h2=0
      g1=0
      g2=0
	dx1=0
	dx2=0
	dy1=0
	dy2=0
	ex1=0
	ex2=0
	ey1=0
	ey2=0
	pi2=6.28318
      do 40 i=1,20
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen
      h=-aux*ome(i)*cjac/(pi2*ra**2)
      g=dlog(1/ra)*ome(i)*cjac/pi2
      gx=ome(I)*cjac*(XCO(i)-XP)/(pi2*ra**2)
	gy=ome(I)*cjac*(YCO(i)-YP)/(pi2*ra**2)
	hx=OME(I)*cjac*(2*AUX*(XP-XCO(i))+COS*ra*ra)/(pi2*ra**4)
	hy=OME(I)*cjac*(2*AUX*(YP-YCO(i))+SEN*ra*ra)/(pi2*ra**4)
	h1=h1-(gi(i)-1)*h/2
      h2=h2+(gi(i)+1)*h/2
      g1=g1-(gi(i)-1)*g/2
      g2=g2+(gi(i)+1)*g/2	
 	dx1=dx1-(gi(i)-1)*gx/2
      dx2=dx2+(gi(i)+1)*gx/2
      dy1=dy1-(gi(i)-1)*gy/2
      dy2=dy2+(gi(i)+1)*gy/2
      ex1=ex1-(gi(i)-1)*hx/2
      ex2=ex2+(gi(i)+1)*hx/2
      ey1=ey1-(gi(i)-1)*hy/2
 40   ey2=ey2+(gi(i)+1)*hy/2
      return
      end

      subroutine inlo(x1,y1,x2,y2,g1,g2)
	implicit none
	real*8 sep,g1,x1,x2,y1,y2,g2

      sep=dsqrt((x2-x1)**2+(y2-y1)**2)
	g1=sep*(1.5-dlog(sep))/(2*6.28318)
      g2=sep*(0.5-dlog(sep))/(2*6.28318)
      continue
      return
      end
c
c
c
      subroutine inte1(xp,yp,x1,y1,x2,y2,gg)
	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer imp,i,ne,np,l,lec,npi,nt
	real*8 gi(20),ome(20),gg,ax,x2,x1,bx,ay,y2,yp,aux,cos
	real*8 y1,by,comp,cjac,pi2,xco(20),yco(20),ra,xp,sen,rb   
 	real*8 elm,er,ccos 
	 GI(1)=0.993128599185094
       GI(2)=-GI(1)
       GI(3)=0.963971927277913
       GI(4)=-GI(3)
       GI(5)=0.912234428251325
       GI(6)=-GI(5)
       GI(7)=0.839116971822218
       GI(8)=-GI(7)
       GI(9)=0.746331906460150
       GI(10)=-GI(9)
       GI(11)=0.636053680726515
       GI(12)=-GI(11)
       GI(13)=0.510867001950827
       GI(14)=-GI(13)
       GI(15)=0.373706088715419
       GI(16)=-GI(15)
       GI(17)=0.227785851141645
       GI(18)=-GI(17)
       GI(19)=0.076526521133497
       GI(20)=-GI(19)
*
*      pesos de Gauss
*
       OME(1)=0.017614007139152
       OME(2)=OME(1)
       OME(3)=0.040601429800386
       OME(4)=OME(3)
       OME(5)=0.062672048334109
       OME(6)=OME(5)
       OME(7)=0.083276741576704
       OME(8)=OME(7)
       OME(9)=0.101930119817240
       OME(10)=OME(9)
       OME(11)=0.118194531961518
       OME(12)=OME(11)
       OME(13)=0.131688638449176
       OME(14)=OME(13)
       OME(15)=0.142096109318382
       OME(16)=OME(15)
       OME(17)=0.149172986472603
       OME(18)=OME(17)
       OME(19)=0.152753387130725
       OME(20)=OME(19)

	gg=0.
c
c	xp e yp aqui são as coordenadas do ponto de interpolação
c

	ax=(x2-x1)/2
      bx=(x2+x1)/2
      ay=(y2-y1)/2
      by=(y1+y2)/2
	comp=2*dsqrt(ax**2+ay**2)
	cjac=comp/2
      SEN=(X1-X2)/COMP
      COS=(Y2-Y1)/COMP
c      pi2=6.28318
      do 40 i=1,20
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
c
c	ra é a distância das coordenadas do ponto de integração ao ponto base
c
 	ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	if(ra.eq.0)go to 36
c	aux=((xp-xco(i))*cos+(yp-yco(i))*sen)/ra
	aux=((xco(i)-xp)*cos+(yco(i)-yp)*sen)/ra**2
	gg=gg+(ra**4)*(4*dlog(ra)-1.)*aux*ome(i)*cjac/16
c	gg=gg+(ra**3)*(3*dlog(ra)-1.)*aux*cjac/9
c      gg=gg+(ra**3)**aux*ome(i)*cjac/3
	go to 37
  36  continue
	gg=0.
  37  continue
  40  continue
      return
      end

 


c
c
c
      subroutine inlo1(x1,y1,x2,y2,gg)
	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer imp,ne,np,l,lec,npi,nt
	real*8 ax,x1,x2,ay,y1,y2,comp,aux,ra,gg,sen,cos

	ax=(x2-x1)/2
      ay=(y2-y1)/2
	comp=2*dsqrt(ax**2+ay**2)
	SEN=(X1-X2)/COMP
      COS=(Y2-Y1)/COMP
  	aux=(x2-x1)*cos+(y2-y1)*sen
      ra=dsqrt((x2-x1)**2+(y2-y1)**2)
      gg=aux/(2*6.28318)
  
       write(imp,*)'finalmente entrei na inlo1'
	write(imp,*)'gg=',gg
      return
      end
c
c
      subroutine galerk1(xp,yp,x1,y1,x2,y2,hg1)  
	common ne,np,l,lec,imp,npi,nt
	integer imp,ne,np,l,lec,npi,nt
	real*8 gi(20),ome(20),ax,x2,x1,bx,ay,y2,yp,aux,cos,hg1,hgar
	real*8 y1,by,comp,cjac,pi2,xco(20),yco(20),ra,xp,sen   
		 
	 GI(1)=0.993128599185094
       GI(2)=-GI(1)
       GI(3)=0.963971927277913
       GI(4)=-GI(3)
       GI(5)=0.912234428251325
       GI(6)=-GI(5)
       GI(7)=0.839116971822218
       GI(8)=-GI(7)
       GI(9)=0.746331906460150
       GI(10)=-GI(9)
       GI(11)=0.636053680726515
       GI(12)=-GI(11)
       GI(13)=0.510867001950827
       GI(14)=-GI(13)
       GI(15)=0.373706088715419
       GI(16)=-GI(15)
       GI(17)=0.227785851141645
       GI(18)=-GI(17)
       GI(19)=0.076526521133497
       GI(20)=-GI(19)
*
*      pesos de Gauss
*
       OME(1)=0.017614007139152
       OME(2)=OME(1)
       OME(3)=0.040601429800386
       OME(4)=OME(3)
       OME(5)=0.062672048334109
       OME(6)=OME(5)
       OME(7)=0.083276741576704
       OME(8)=OME(7)
       OME(9)=0.101930119817240
       OME(10)=OME(9)
       OME(11)=0.118194531961518
       OME(12)=OME(11)
       OME(13)=0.131688638449176
       OME(14)=OME(13)
       OME(15)=0.142096109318382
       OME(16)=OME(15)
       OME(17)=0.149172986472603
       OME(18)=OME(17)
       OME(19)=0.152753387130725
       OME(20)=OME(19)
c
c	 funciona como se o elemento fosse constante
c

      ax=(x2-x1)/2
      bx=(x2+x1)/2
      ay=(y2-y1)/2
      by=(y1+y2)/2
	comp=2*sqrt(ax**2+ay**2)
C
C	 cjac é o jacobiano, igual ao comprimento sobre 2
C
	cjac=comp/2
      SEN=(X1-X2)/COMP
      COS=(Y2-Y1)/COMP
	hg1=0
	pi2=6.28318
      do 1400 i=1,20
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen
      hgar=(0.5-dlog(ra))*ome(i)*aux*cjac/(2*pi2)
	hg1=hg1+hgar
1400   continue
      return
      end

c
c
c

      subroutine inter(fi,dfi,kode,xxc,yyc,x,y,sol,nop,dsolx,dsoly,locc,
     *a,b,cb,xx,yy,nx)
	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer nx,kode(nx),i,np,k,l,m,locc(nx),l1,l2,nop(nx,3)
	integer j,ne,nt,lec,imp,npi
	real*8 ch,fi(nx),dfi(nx),sol(nx),dsolx(nx),dsoly(nx)
	real*8 xxc(nx),yyc(nx),x(nx),y(nx),a1,a2,b1,b2,dx1,dx2
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,drumx,drumy,zog1,zog2
	real*8 hcsi(nx,nx),xx(nx),yy(nx),rex(nx,nx)
	real*8 doma(nx,nx),domb(nx,nx),gcsi(nx,nx),c1
	real*8 a(nx,nx),b(nx,nx),domt(nx),cb(nx)

      do 20 i=1,np
      if (kode(i)) 20,20,10
 10   ch=fi(i)
      fi(i)=dfi(i)
      dfi(i)=ch
 20   continue
      do 21 i=np+1,nt
	ch=fi(i)
      fi(i)=dfi(i)
      dfi(i)=ch
 21   continue
c      do 40 k=1,l
c      sol(k)=0
c	  dsolx(k)=0
c	  dsoly(k)=0
c	  m=locc(k)
c      do 30 j=1,ne
c        L1=NOP(J,1)
c	  L2=NOP(J,2)
c	  write(6,*)m,j
C	  write(6,*)'l1 e l2'
C	  write(6,*)L1,L2
c
c       evitando entrar na recurs
c	  if(m.ne.j)go to 23
c       go to 23
c	  write(6,*)'entrei na recurs'
c      call recurs(cx(k),cy(k),x(L1),y(L1),x(L2),y(L2),a1,a2,b1,b2,dx1,
c     *dx2,dy1,dy2,ex1,ex2,ey1,ey2,drumx,drumy)
c	zog1=fi(l1)
c	zog2=fi(l2)
c	  go to 26
c 23	  call inte(cx(k),cy(k),x(L1),y(L1),x(L2),y(L2),a1,a2,b1,b2,dx1,
c     *dx2,dy1,dy2,ex1,ex2,ey1,ey2)
c	hcsi(k,l1)=hcsi(k,l1)+a1
c	hcsi(k,l2)=hcsi(k,l2)+a2
c	gcsi(k,l1)=gcsi(k,l1)+B1
c	gcsi(k,l2)=gcsi(k,l2)+B2
c 26   continue 
c       sol(k)=sol(k)+dfi(L1)*b1+dfi(L2)*b2-fi(L1)*a1-fi(L2)*a2
c	 dsolx(k)=dsolx(k)+dfi(L1)*dx1+dfi(L2)*dx2-fi(L1)*ex1-fi(L2)*ex2
c	 dsoly(k)=dsoly(k)+dfi(L1)*dy1+dfi(L2)*dy2-fi(L1)*ey1-fi(L2)*ey2
c  30   CONTINUE
c
c	sol(k)=2.0*sol(k)
c	 conferir
c     dsoly(k)=dsoly(k)-drumy*sol(k)+(1./6)*drumy*(zog1+zog2)
c     dsolx(k)=dsolx(k)-drumx*sol(k)+(1./6)*drumx*(zog1+zog2)
c  	  dsolx(k)=2.0*dsolx(k)
c	  dsoly(k)=2.0*dsoly(k)
c 40   continue
c
c	coeficiente de influência para os valores recursivos
c
c	 do 185 i=1,l
c	do 185 j=1,np
c	call nard3(cx(i),cy(i),Xx(j),Yy(j),a1,c1)
c	rex(i,j)=0.5*a1
c185   continue
c      do 186 i=1,l
c	do 186 j=np+1,nt
c	call nard3(cx(i),cy(i),X(j),Y(j),a1,c1)
c	rex(i,j)=0.5*a1
c186	continue
c
c	 fazendo o produto dos h e g recursivos pelas a e b versus cb
c
c	do 31 i=1,l
c	do 31 j=1,np
c	do 31 k=1,nt
c	doma(i,k)=doma(i,k)+hcsi(i,j)*a(j,k)
c	domb(i,k)=domb(i,k)+gcsi(i,j)*b(j,k)
c  31  continue
c      do 32 i=1,l
c	do 32 j=1,nt
c	domt(i)=domt(i)+(domb(i,j)-doma(i,j)-rex(i,j))*cb(j)
c32    continue
c	do 34 k=1,l
c	sol(k)=2*(sol(k)-domt(k))
c34    continue	 
      return
      end
c
	 SUBROUTINE MINV(A,N)
	implicit none
	integer imp,k,n,j,i,ji,ki
	real*8 lo(2000),mo(2000),biga,a(2000,2000),hold

C     
C      INVERTE A MATRIZ ATRAVES DO METODO PADRAO DE GAUSS-JORDAN
C      O DETERMINANTE TAMBEM E CALCULADO. UM DETERMINANTE NULO INDICA QUE A 
C      MATRIZ E SINGULAR.
C
C      A - MATRIZ DE ENTRADA, DESTRUIDA NA OPERACAO E SUBSTITUIDA PELA
C          INVERSA RESULTANTE
C      N - ORDEM DA MATRIZ A
C      LO - VETOR DE TRABALHO
C      MO - VETOR DE TRABALHO
c     
      IMP=6
C
C
C      PROCURA DO MAIOR ELEMENTO
C
       DO 100 K=1,N
         LO(K)=K
         MO(K)=K
         BIGA=A(K,K)
         DO 10 J=K,N
         DO 10 I=K,N
           IF(dabs(BIGA).GE.dabs(A(I,J))) GO TO 10
           BIGA=A(I,J)
           LO(K)=I
           MO(K)=J
10       CONTINUE
C
C      MUDANCA DE LINHAS
C
         J=LO(K)
         IF(J.LE.K) GO TO 30
         DO 20 I=1,N
           HOLD=-A(K,I)
c           JI=KI-K+J
           A(K,I)=A(J,I)
           A(J,I)=HOLD
20       CONTINUE
C
C      MUDANCA DE COLUNAS
C
30       I=MO(K)
         IF(I.LE.K) GO TO 50
         DO 40 J=1,N
           HOLD=-A(J,K)
           A(J,K)=A(J,I)
           A(J,I)=HOLD
40       CONTINUE
C
C      DIVIDE COLUNA POR PIVOT NEGATIVO ( VALOR DOS ELEMENTOS DO PIVOT ESTAO
C      CONTABILIZADOS EM BIGA)
C
50       IF((dabs(BIGA)).GT.0)  GO TO 60
           WRITE (imp,160) K
           STOP
60       DO 70 I=1,N
           IF(I.EQ.K) GO TO 70
           A(I,K)=A(I,K)/(-BIGA)
70       CONTINUE
C
C      REDUCAO DA MATRIZ
C
         DO 80 I=1,N
           HOLD=A(I,K)
           DO 80 J=1,N
             IF(I.EQ.K) GO TO 80
             IF(J.EQ.K) GO TO 80
             A(I,J)=HOLD*A(K,J)+A(I,J)
80       CONTINUE
C
C      DIVIDE A LINHA PELO PIVOT
C
         DO 90 J=1,N
           IF(J.EQ.K) GO TO 90
           A(K,J)=A(K,J)/BIGA
90       CONTINUE
C
C      SUBSTITUI O PIVOT PELO RECIPROCO
C
         A(K,K)=1.0/BIGA
100    CONTINUE
C
C      MUDANCA FINAL DE LINHAS E COLUNAS
C
       K=N-1
110    CONTINUE
       I=LO(K)
       IF(I.LE.K) GO TO 130
       DO 120 J=1,N
         HOLD=A(J,K)
         A(J,K)=-A(J,I)
         A(J,I)=HOLD
120    CONTINUE
130    J=MO(K)
       IF(J.LE.K) GO TO 150
       DO 140 I=1,N
         HOLD=A(K,I)
         A(K,I)=-A(J,I)
         A(J,I)=HOLD
140    CONTINUE
150    K=K-1
       IF(K.GT.0)GO TO 110
       RETURN
160    FORMAT(/,20X,'DURANTE A OPERACAO DE INVERSAO DA MATRIZ',/,20X,
     1'DETERMINANTE NULO FOI DETECTADO.  O PROCESSAMENTO FOI',/,20X,
     1'INTERROMPIDO  O NUMERO DA LINHA OU COLUNA FOI :',I4,/,20x,
     1'dentro da rotina MINV - inversao de matriz')
       END

c	CALL AUTOV(CCBAR,HBAR,JQQ,fi,dfi,1,nt,juu)
c	JQQ dimensao da matriz total menos os prescritos
c	JUU = cont pres  quantidade de nos prescritosc
c	nt = jqq mais juu  

c************************************************
*     SUBROTINA QUE RESOLVE O PROBLEMA DE AUTOVALOR GENERALIZADO
C     [A]{X}=LAMBDA[B]{X}
C
C     SETEMBRO DE 1998 - ANDRE BULCAO
      SUBROUTINE AUTOV(A,B,N,NUM,NUM_2,PRINTE,N_NOS,CONT_UPRES)
C
      IMPLICIT NONE
      REAL*8 A(2000,2000),B(2000,2000),Y1(2000),AUT_VAL(2000)
     1,C(2000,2000),TERMO(2000),WR(2000),WI(2000),SOMA,wra(2000)
     1,MAUT_VET(2000,2000),B1(2000,2000)                    
     1,A1(2000,2000),C1(2000,2000),X(2000),Y(2000),XI(2000),YI(2000)
     1,XINIC(2000),YA(2000),PERCENT
     1,ATRAS,FRENTE,CHUTE,MAIOR
      INTEGER I,J,K,N,PRINTE,IMP,CONT,NDIF,NUM(2000),N_NOS,NUM_2(2000)
     1,CONT_UPRES,VALOR
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
		do i=1,40
 	wra(i)=(1./wr(i))**(1/2)
	

	write(imp,554)wra(i)
554   format(f10.5)
	enddo


      WRITE(*,3301)
      WRITE(IMP,3301)
3301   FORMAT(///,10X,' - AUTOVALORES - ',/,10X,'REAL',5X,'IMAGINARIO')
      DO I=1,N
        WRITE(*,*) I,WR(I),WI(I)
        WRITE(IMP,*) I,WR(I),WI(I)
      ENDDO

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
*          WRITE(*,*)   'CHUTE DO AUTOVALOR=',CHUTE
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
*         RESOLUCAO DO SITEMA        
          CALL SLNPD(A1,YA,N,PRINTE)
*         NORMALIZACAO DO VETOR YA
          SOMA=0.
          DO J=1,N
            IF(ABS(YA(J)).GT.SOMA) THEN
              SOMA=ABS(YA(J))
            ENDIF          
          ENDDO 
          DO J=1,N
            YA(J)=YA(J)/SOMA
          ENDDO
*     
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
C************************************************
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
c************************************************
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
c************************************************
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
c************************************************
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


c	***********************************************************
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
                pause 'singular matrix in gaussj'
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
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
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


C      
      SUBROUTINE JACOBI(N,A,B,EIGV,X)
C
C     EXTRACAO MODAL
      implicit none
	integer	iFPR, NSMAX,IOUT,I,J,K,N,NSWEEP,NR,JJ,KM1,L

      REAL*8 A(2000,2000),B(2000,2000),X(2000,2000),EIGV(2000)
	REAL*8 D(2000),BM(2000,2000),RM(2000,2000),XT(2000,2000)
	REAL*8 AB,CA,CG,EPTOLA,EPTOLB,CHECK,SQCH,D1,D2,DEN,XJ,XK,TOL,DIF
	REAL*8 EPS,RTOL,EPSA,EPSB,AUX,AKK,AJJ,JP1,JM1,KP1,KJ1,AJ,BJ,AK,BK


      IFPR=1
      NSMAX=10
      RTOL=1.0D-12
      IOUT=2
      DO 10 I=1,N
      IF(A(I,I).GT.0.0.AND. B(I,I).GT.0.0) GOTO 4
      WRITE(2,49)I,A(I,I),B(I,I)
  49  FORMAT(/,I5,F17.0,5X,F15.0)
      WRITE(IOUT,2020)
      STOP
  4   D(I)=A(I,I)/B(I,I)
  10  EIGV(I)=D(I)
      DO 30 I=1,N
      DO 20 J=1,N
  20  X(I,J)=0.0
  30  X(I,I)=1.0
      DO 25 I=1,N
      DO 25 J=1,N
      BM(I,J)=B(I,J)
  25  CONTINUE
      IF(N.EQ.1) THEN
                 EIGV(1)=SQRT(A(1,1)/B(1,1))
                 X(1,1)=1.0
                 GOTO 299
                 END IF 
C
      NSWEEP=0
      NR=N-1
  40  NSWEEP=NSWEEP+1
C
      EPS=(0.01**NSWEEP)**2
      DO 210 J=1,NR
      JJ=J+1
      DO 210 K=JJ,N
      EPTOLA=(A(J,K)*A(J,K))/(A(J,J)*A(K,K))
      EPTOLB=(B(J,K)*B(J,K))/(B(J,J)*B(K,K))
      IF((EPTOLA.LT.EPS).AND.(EPTOLB.LT.EPS)) GOTO 210
C
      AKK=A(K,K)*B(J,K) - B(K,K)*A(J,K)
      AJJ=A(J,J)*B(J,K) - B(J,J)*A(J,K)
      AB=A(J,J)*B(K,K) - A(K,K)*B(J,J)
      CHECK=(AB*AB+4.*AKK*AJJ)/4.
      IF(CHECK) 50,60,60
  50  WRITE(IOUT,2020)
      STOP
  60  SQCH=SQRT(CHECK)
      D1=AB/2.+SQCH
      D2=AB/2.-SQCH
      DEN=D1
      IF(ABS(D2).GT.ABS(D1)) DEN=D2
      IF(DEN) 80,70,80
  70  CA=0.
      CG=-A(J,K)/A(K,K)
      GOTO 90
  80  CA=AKK/DEN
      CG=-AJJ/DEN
C
  90  IF(N-2) 100,190,100
  100 JP1=J+1
      JM1=J-1
      KP1=K+1
      KM1=K-1
      IF(JM1-1) 130,110,110
  110 DO 120 I=1,JM1
      AJ=A(I,J)
      BJ=B(I,J)
      AK=A(I,K)
      BK=B(I,K)
      A(I,J)=AJ+CG*AK
      B(I,J)=BJ+CG*BK
      A(I,K)=AK+CA*AJ
  120 B(I,K)=BK+CA*BJ
  130 IF(KP1-N) 140,140,160
  140 DO 150 I=KP1,N
      AJ=A(J,I)
      BJ=B(J,I)
      AK=A(K,I)
      BK=B(K,I)
      A(J,I)=AJ+CG*AK
      B(J,I)=BJ+CG*BK
      A(K,I)=AK+CA*AJ
  150 B(K,I)=BK+CA*BJ
  160 IF(JP1-KM1) 170,170,190
  170 DO 180 I=JP1,KM1
      AJ=A(J,I)
      BJ=B(J,I)
      AK=A(I,K)
      BK=B(I,K)
      A(J,I)=AJ+CG*AK
      B(J,I)=BJ+CG*BK
      A(I,K)=AK+CA*AJ
  180 B(I,K)=BK+CA*BJ
  190 AK=A(K,K)
      BK=B(K,K)
      A(K,K)=AK+2.*CA*A(J,K)+CA*CA*A(J,J)
      B(K,K)=BK+2.*CA*B(J,K)+CA*CA*B(J,J)
      A(J,J)=A(J,J)+2.*CG*A(J,K)+CG*CG*AK
      B(J,J)=B(J,J)+2.*CG*B(J,K)+CG*CG*BK
      A(J,K)=0.
      B(J,K)=0.
C
      DO 200 I=1,N
      XJ=X(I,J)
      XK=X(I,K)
      X(I,J)=XJ+CG*XK
  200 X(I,K)=XK+CA*XJ
  210 CONTINUE
C
      DO 220 I=1,N
      IF(A(I,I).GT.0. .AND. B(I,I).GT.0.) GOTO 220
      WRITE(IOUT,2020)
      STOP
  220 EIGV(I)=A(I,I)/B(I,I)
C
  230 DO 240 I=1,N
      TOL=RTOL*D(I)
      DIF=ABS(EIGV(I) - D(I))
      IF(DIF.GT.TOL) GOTO 280
  240 CONTINUE
C
      EPS=RTOL**2
      DO 250 J=1,NR
      JJ=J+1
      DO 250 K=JJ,N
      EPSA=(A(J,K)*A(J,K))/(A(J,J)*A(K,K))
      EPSB=(B(J,K)*B(J,K))/(B(J,J)*B(K,K))
      IF(EPSA.LT.EPS.AND.EPSB.LT.EPS) GOTO 250
      GOTO 280
  250 CONTINUE
C
  255 DO 254 I=1,N
      DO 254 J=1,N
      A(J,I)=A(I,J)
  254 B(J,I)=B(I,J)
      I=1
 2200 DO 2100 J=I+1,N
      IF(EIGV(I).LT.EIGV(J))GOTO 2100
      AUX=EIGV(J)
      EIGV(J)=EIGV(I)
      EIGV(I)=AUX
      DO 2080 L=1,N
      D(L)=X(L,J)
      X(L,J)=X(L,I)
 2080 X(L,I)=D(L)
 2100 CONTINUE
      IF(I.EQ.N-1) GOTO 2300
      I=I+1
      GOTO 2200
 2300 DO 260 I=1,N
      DO 260 J=1,N
      XT(J,I)=X(I,J)
  260 CONTINUE
      AUX=0.0
      DO 262 I=1,N
      DO 262 J=1,N
      DO 261 L=1,N
  261 AUX=AUX+XT(I,L)*BM(L,J)
      RM(I,J)=AUX
  262 AUX=0.0
      DO 264 I=1,N
      DO 264 J=1,N
      DO 263 L=1,N
  263 AUX=RM(I,L)*X(L,J)+AUX
      XT(I,J)=AUX
  264 AUX=0.0
      DO 265 I=1,N
      EIGV(I)=SQRT(EIGV(I))
      DO 265 J=1,N
      X(J,I)=X(J,I)/SQRT(XT(I,I))
  265 CONTINUE
C
  299 DO 350 I=1,N
      WRITE(6,300) I,EIGV(I)
  300 FORMAT(/5X,'MODO= ',I3,2X,'FREQUENCIA= ',F12.4)
      DO 340 J=1,N
      WRITE(6,330) X(J,I)
  330 FORMAT(F12.4)
  340 CONTINUE
  350 CONTINUE
C
      RETURN
  280 DO 290 I=1,N
  290 D(I)=EIGV(I)
 2020 FORMAT(//,10X,'ERROR -INTERRUPTED SOLUTION!',/,10X,'NON POSITIVE
     +       DEFINITE MATRICES!')
      IF(NSWEEP.LT.NSMAX) GOTO 40
      GOTO 255
      END




      subroutine output(x,y,fi,dfi,xxc,yyc,sol,dsolx,dsoly,nx,sai,tempo)
	implicit none
      common ne,np,l,lec,imp,npi,nt
	common dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2
 	integer	i,nt,k,l,ne,np,lec,imp,npi,nx,ini,fim
	real*8 x(nx),y(nx),fi(nx),dfi(nx),xxc(nx),yyc(nx)
	real*8 sol(nx),dsolx(nx),dsoly(nx),ti(nx)
	real*8 total,sn,cs,ym(nx),xm(nx),dx1,dx2,ge,bi
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,aux,total2,tempo
	character sai*20

	write(imp,77) tempo
 77	format(/'Tempo de Processamento: ',f8.3,' segundos'/)
      write(imp,100)
100   format (' ',120('*'),//1x,'results'//2x,'boundary nodes'//16x,
     1'x',23x,'y',19x,'potential',10x,'potential derivative'/)
      do 10 i=1,nt
 10   write(imp,200) x(i),y(i),fi(i),dfi(i)
200   format (4(10x,e14.7))
c      write(imp,300)
c300   format (//2x,'recursive points',//11x,'x',18x,'y',14x,
c     1'potential',10x,'derivada x',15x,'derivada y',/)
c      do 20 k=1,l
c 20   write(imp,400) cx(k),cy(k),sol(k),dsolx(k),dsoly(k)
c400   format (5(5x,e14.7))
      write(imp,500)
500   format (' ',120('*'))
c
c	     APENDICE PARA CALCULO DO ERRO COMETIDO

	
      ini=2
	if (ne.eq.8) then
	ini=2
	fim=5
    	else if (ne.eq.32) then
	  ini=2
        fim=14
    	else if (ne.eq.80) then
	  ini=2
c	fim=21
	  fim=32
	 else if (ne.eq.120) then
	  ini=2
	  fim=47
	 else if (ne.eq.160) then
	  ini=2
	  fim=62
	 else
	 ini=2
	fim=77
	 end if
	aux=0
	total=0.0

c      ini=2
c	if (ne.eq.32) then
c	  ini=28
c        fim=33
c	else if (ne.eq.80) then
c	  ini=64
c	  fim=75
c	else if (ne.eq.120) then
c	  ini=94
c	  fim=110
c	else
c	  ini=124
c	  fim=145
c	end if
c	aux=0
c	total=0.0
      DO 611 i=ini,fim
c	     RESPOSTA ANALITICA exponencial
c	  ti=sinh(x(i))/cosh(1.0)
c	  ti=(1.0-x(i))
c	      RESPOSTA ANALITICA harmonica
	  ti=sin(1*x(i))/(1*cos(1*1.0))
	bi=1./cos(1.)
c	  ti=0.5-x(i)*2/3
c	     RESPOSTA ANALITICA SENOIDAL
c	  ti(i)=dsin(x(i))-x(i)*cos(1.0)
c       ti(i)=dcos(x(i))-cos(1.0)
c		      CALCULO DO ERRO
c	write(imp,*)i,x(i),ti(i)
	ge=abs((abs(ti(i))-abs(fi(i)))/bi)*100
c  	write(imp,*)
	total=total+ge
	aux=aux+1
	WRITE(imp,581)fi(i),ti(i),ge
581   format (2X,f12.8,2x,f12.8,2x,f12.8)
611   CONTINUE
	write(imp,*)'bi=',bi

      total=total/aux
	write(imp,*)total


      return
      end
