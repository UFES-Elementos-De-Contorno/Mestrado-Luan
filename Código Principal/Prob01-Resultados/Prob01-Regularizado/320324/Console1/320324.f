      program main
c
c	programa com elementos lineares para interpolação do problema de HELMHOTZ
c	 versão com calculo dA MAtriz através de uso direto de funções radiais	MECIC
c     com introdução de termo regularizador, evitando duas numens de pontos
C     RESPOSTA
		implicit none
      common ne,np,l,lec,imp,npi,nt
   	integer nx,lec,imp
	parameter (nx=1500)
	integer nop(nx,3),kode(nx),locc(nx),idup(nx),ne,np,l,npi,nt
	real*8 x(nx),y(nx),g(nx,nx),fi(nx),dfi(nx),xxc(nx)
	real*8 yyc(nx),sol(nx),h(nx,nx),dsolx(nx),dsoly(nx)
	real*8 a(nx,nx),b(nx,nx),cb(nx),xx(nx),yy(nx),d,omega
	real*8 tempo,calcula_tempo,fr
	character sai*20,tempo1*10,tempo2*10,arqent*20
	lec=5
	imp=6
      
      
!      write(*,'(A\)')' INFORME NOME DO ARQUIVO DE ENTRADA (arquivo.txt)
!     *==> '
!      read(*,'(A20)')ARQENT
      
      ARQENT = '320324.txt'
      write(*,*) 'ARQUIVO DE ENTRADA ===> ', ARQENT
      write(*,*) 'ARQUIVO DE SAIDA   ===> x.xxw',ARQENT
      write(*,*)
      open(LEC,FILE=ARQENT)
      
      write(*,'(A\)')' INFORME A FREQUENCIA (x.xx) ==> '
      read(*,'(F10.2)') omega
!      fr = 2.0
      
      
      call input(xxc,yyc,x,y,kode,fi,nop,locc,Idup,nx,sai,arqent,omega)
c	write(imp,*)'vou entrar na fmat'
c
c		CALCULO DO TEMPO DE PROCESSAMENTO
c
!      call date_and_time(TIME=tempo1)

      call fmat(x,y,g,h,fi,dfi,kode,nop,Idup,a,b,cb,xx,yy,xxc,yyc,nx,
     *omega)
c	write(imp,*)'ja sai da fmat'
      nt=np+npi 
c      call slnpd(h,dfi,d,np,nx)
c	write(imp,*)'ja sai da slnpd'
      call inter(fi,dfi,kode,xxc,yyc,x,y,sol,nop,dsolx,dsoly,locc,a,b,
     *cb,xx,yy,nx)
c	write(imp,*)'ja sai da inter'

      call output (x,y,fi,dfi,xxc,yyc,sol,dsolx,dsoly,nx,sai,tempo,
     *omega)
      stop
      end

	subroutine input(xxc,yyc,x,y,kode,fi,nop,locc,Idup,nx,sai,arqent,omega)

	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer nx,lec,imp,ne,np,npi,l,nt,i,locc(nx),kode(nx)
	integer idup(nx),nop(nx,3),j
	real*8 xxc(nx),yyc(nx),x(nx),y(nx),fi(nx),omega
	character title*18,arqent*20,arqout*30,sai*20,string2*10


c	open(unit=1,file='entrada.txt',status='old')
c	read(1,7) ARQENT
c	read(1,7) ARQOUT
c	read(1,7) sai
c	close(unit=1,status='delete')
c7	format (a20)

 !     write(*,'(A\)')' INFORME NOME DO ARQUIVO DE SAIDA --->'
 !     read(*,'(A20)')ARQOUT
      write (string2,'(F10.2)') omega
      ARQOUT = string2//"w"//ARQENT//'.txt'
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
! 	read(lec,201)omega
200   format(i5)
201   format(f5.2)
      nt=np+npi

      write(imp,300) ne,np,npi
300   format(//'data'//2x,'number of boundary elements=',
     1i3//,2x,'number of functional nodes=',i3//,2x,
     2'number of internal source points where the funct. is calculated='
     3,i3,//)

      write(*,*) ne,np,npi
c
       write(imp,500)
500   format (//2x,'coordinates of the source points of the boundary el
     1ements',//4x,'point',10x,'x',18x,'y')
	nt=np+npi
c      a varredura inclui pontos internos,  que são pontos fonte
      do 750 i=1,nt
      read(lec,600)j, x(i),y(i)
600   format(i3,2f12.4)
c      write(imp,900) kode(i),idup(i),dsin(3.1415*y(i))
	write(imp,700) i,x(i),y(i)
c	write(imp,700) x(i),y(i)
c700   format(2f12.5)
700   format(5x,i3,2(5x,e14.7))
750	continue

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



      !rotina de Eliminação de Gauss
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
     *nx,omega)
      implicit none
	common ne,np,l,lec,imp,npi,nt
	INTEGER CONT_UPRES,CONT_QPRES,CONT_INTF,NUM(1500),NUM_2(1500)
 	integer nx,nop(nx,3),idup(nx),l1,l3,i,ne,l2,l4,np,imp
	integer npi,nt,j,ng,l,lec,kode(nx),con1,con2,k,n1,n2,printe
	integer IUU,JUU,IQQ,JQQ,IUQ,JUQ,IQU,JQU,KA,KB
	real*8 xd(nx),x(nx),yd(nx),y(nx),xx(nx),yy(nx)
	real*8 g(nx,nx),h(nx,nx),h1,h2,g1,g2,dx1,dx2,omega
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,ta,ra,c(nx,nx),fdo(nx),ge
	real*8 p(nx,nx),cd(nx),d,r,tok(nx,nx),alfa(nx,nx),teta
	real*8 gg,oneta(nx),ch,dfi(nx),cb(nx),fi(nx),q(nx,nx),hg1
	real*8 a(nx,nx),b(nx,nx),cc(nx,nx),dc(nx),ag,k1,k2,pi,hgk(nx,nx)
	real*8 slamb(nx),ulamb(nx,nx),ci(nx,nx),xxc(nx),yyc(nx),dist,ala
	real*8 ff(nx,nx),ggm(nx,nx),HUU(NX,NX),HQQ(NX,NX),HQU(NX,NX)
	
	parameter (pi=3.141592)
c

      do 1 i=1,np
	xx(i)=x(i)
	yy(i)=y(i)
1     continue
            
	printe=1
      
!      omega=fr*fr
c
c	 rotina para afastar os pontos nodais duplos da quina das arestas
c
	dist=0.05 !porcentagem do afastamento do elemento da aresta
!	kode(0)=1
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
      call inte(x(i),y(i),xx(L1),yy(L1),xx(L2),yy(L2),h1,h2,g1,g2,dx1,
     *dx2,dy1,dy2,ex1,ex2,ey1,ey2) 
      h(i,L2)=h(i,L2)+h2
      h(i,L1)=h(i,L1)+h1
      g(i,L2)=g(i,L2)+g2
      g(i,L1)=g(i,L1)+g1

c     matriz hgk: é uma matriz diagonal gerada pela regularização
c     e formada pela soma dos componentes das linhas 
 
	call galerk1(x(i),y(i),xx(L1),yy(L1),xx(L2),yy(L2),hg1)  
	hgk(i,i)=hgk(i,i)+hg1
	GO TO 26
      
  20  continue
      call inlo(xx(L1),yy(L1),xx(L2),yy(L2),g1,g2)
      g(i,L1)=g(i,L1)+g1
	h(i,L1)=h(i,L1)+0.0
	g(i,L2)=g(i,L2)+g2
	h(i,L2)=h(i,L2)+0.0
	go to 26
  22  continue
      call inlo(xx(L1),yy(L1),xx(L2),yy(L2),g1,g2)
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
      c(i,j)=ra*ra*dlog(ra) ! função RADIAL = r²*ln(r)
c	c(i,j)=ra
	go to 23
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
      call inte(x(i),y(i),xx(L1),yy(L1),xx(L2),yy(L2),h1,h2,g1,g2,
     *dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2)
c     matriz hgk: é uma matriz diagonal gerada pela regularização
c     e formada pela soma dos componentes das linhas 
      call galerk1(x(i),y(i),xx(L1),yy(L1),xx(L2),yy(L2),hg1)  
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
      ulamb(i,j)=-dlog(ra)/(2*3.1415926) ! ulamb = -ln(r)/2pi = u*
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
  	IF(I.EQ.j)GO TO 21
      call inte1(x(i),y(i),xx(L1),yy(L1),xx(L2),yy(L2),gg)
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

5477   continue

5479   continue

c
c	 matriz cc 
c
      do 112 i=1,nt
	do 112 j=1,nt

	CC(i,j)=h(i,j)-(p(i,j)-a(i,j)+hgk(i,j))*omega*omega

112   continue
	


5478   continue

	do 152 j=1,np
      if (kode(j)) 140,140,150
140   continue
      do 151 i=1,np+npi
      ch=g(i,j)
      g(i,j)=-cc(i,j)
      cc(i,j)=-ch
151   continue
150   continue
152   continue


      do 161 i=1,nt
      dfi(i)=0.
      do 160 j=1,nt
      dfi(i)=dfi(i)+g(i,j)*fi(j)
160   continue
161   continue


      call slnpd(cc,dfi,nt,nx)

      return
      end

      subroutine inte(xp,yp,x1,y1,x2,y2,h1,h2,g1,g2,dx1,dx2,dy1,dy2,
     *ex1,ex2,ey1,ey2)  
      implicit none  
	common ne,np,l,lec,imp,npi,nt
	integer i,ne,np,l,lec,imp,npi,nt
	real*8 gi(40),ome(40),ax,x1,x2,bx,ay,y1,y2,by,comp,h1,h2
	real*8 g1,g2,dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2,pi2,xco(40)
	real*8 yco(40),ra,xp,yp,aux,h,g,cjac,gx,gy,hx,hy,sen,cos


	 GI(1)=0.998237709710559340
       GI(2)=-GI(1)
       GI(3)=0.990726238699457038
       GI(4)=-GI(3)
       GI(5)=0.977259949983774336
       GI(6)=-GI(5)
       GI(7)=0.957916819213791734
       GI(8)=-GI(7)
       GI(9)=0.932812808278676532 
       GI(10)=-GI(9)
       GI(11)=0.902098806968874330 
       GI(12)=-GI(11)
       GI(13)=0.865959503212259528
       GI(14)=-GI(13)
       GI(15)=0.824612230833311726
       GI(16)=-GI(15)
       GI(17)=0.778305651426519424
       GI(18)=-GI(17)
       GI(19)=0.727318255189927122 
       GI(20)=-GI(19)
	 GI(21)=0.671956684614179620
       GI(22)=-GI(21)
       GI(23)=0.612553889667980218
       GI(24)=-GI(23)
       GI(25)=0.549467125095128216
       GI(26)=-GI(25)
       GI(27)=0.483075801686178714
       GI(28)=-GI(27)
       GI(29)=0.413779204371605012
       GI(30)=-GI(29)
       GI(31)=0.341994090825758510 
       GI(32)=-GI(31)
       GI(33)=0.26815218500725378
       GI(34)=-GI(33)
       GI(35)=0.19269758070137116
       GI(36)=-GI(35)
       GI(37)=0.11608407067525524
       GI(38)=-GI(37)
       GI(39)=0.03877241750605082 
       GI(40)=-GI(39)

*
*      pesos de Gauss
*
       OME(1)=0.0045212770985332
       OME(2)=OME(1)
       OME(3)=0.0104982845311528 
       OME(4)=OME(3)
       OME(5)=0.0164210583819079 
       OME(6)=OME(5)
       OME(7)=0.0222458491941670
       OME(8)=OME(7)
       OME(9)=0.0279370069800234
       OME(10)=OME(9)
       OME(11)=0.0334601952825478 
       OME(12)=OME(11)
       OME(13)=0.0387821679744720
       OME(14)=OME(13)
       OME(15)=0.0438709081856733
       OME(16)=OME(15)
       OME(17)=0.0486958076350722
       OME(18)=OME(17)
       OME(19)=0.0532278469839368
       OME(20)=OME(19)
       OME(21)=0.0574397690993916 
       OME(22)=OME(21)
       OME(23)=0.0613062424929289
       OME(24)=OME(23)
       OME(25)=0.0648040134566010
       OME(26)=OME(25)
       OME(27)=0.0679120458152339
       OME(28)=OME(27)
       OME(29)=0.0706116473912868
       OME(30)=OME(29)
       OME(31)=0.0728865823958041 
       OME(32)=OME(31)
       OME(33)=0.0747231690579683
       OME(34)=OME(33)
       OME(35)=0.0761103619006262
       OME(36)=OME(35)
       OME(37)=0.0770398181642480 
       OME(38)=OME(37)
       OME(39)=0.0775059479784248
       OME(40)=OME(39)



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
      
      do 40 i=1,40
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen
      h=-aux*ome(i)*cjac/(pi2*ra**2)
      g=dlog(1/ra)*ome(i)*cjac/pi2
      
      gx=ome(I)*cjac*(XCO(i)-XP)/(pi2*ra**2)  !nao usa
	gy=ome(I)*cjac*(YCO(i)-YP)/(pi2*ra**2)  !nao usa
	hx=OME(I)*cjac*(2*AUX*(XP-XCO(i))+COS*ra*ra)/(pi2*ra**4)  !nao usa
	hy=OME(I)*cjac*(2*AUX*(YP-YCO(i))+SEN*ra*ra)/(pi2*ra**4)  !nao usa
      
	h1=h1-(gi(i)-1)*h/2
      h2=h2+(gi(i)+1)*h/2
      g1=g1-(gi(i)-1)*g/2
      g2=g2+(gi(i)+1)*g/2	
      
 	dx1=dx1-(gi(i)-1)*gx/2  !nao usa
      dx2=dx2+(gi(i)+1)*gx/2  !nao usa
      dy1=dy1-(gi(i)-1)*gy/2  !nao usa
      dy2=dy2+(gi(i)+1)*gy/2  !nao usa
      ex1=ex1-(gi(i)-1)*hx/2  !nao usa
      ex2=ex2+(gi(i)+1)*hx/2  !nao usa
      ey1=ey1-(gi(i)-1)*hy/2  !nao usa
 40   ey2=ey2+(gi(i)+1)*hy/2  !nao usa
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
	real*8 gi(40),ome(40),gg,ax,x2,x1,bx,ay,y2,yp,aux,cos
	real*8 y1,by,comp,cjac,pi2,xco(40),yco(40),ra,xp,sen   
 
	 GI(1)=0.998237709710559340
       GI(2)=-GI(1)
       GI(3)=0.990726238699457038
       GI(4)=-GI(3)
       GI(5)=0.977259949983774336
       GI(6)=-GI(5)
       GI(7)=0.957916819213791734
       GI(8)=-GI(7)
       GI(9)=0.932812808278676532 
       GI(10)=-GI(9)
       GI(11)=0.902098806968874330 
       GI(12)=-GI(11)
       GI(13)=0.865959503212259528
       GI(14)=-GI(13)
       GI(15)=0.824612230833311726
       GI(16)=-GI(15)
       GI(17)=0.778305651426519424
       GI(18)=-GI(17)
       GI(19)=0.727318255189927122 
       GI(20)=-GI(19)
	 GI(21)=0.671956684614179620
       GI(22)=-GI(21)
       GI(23)=0.612553889667980218
       GI(24)=-GI(23)
       GI(25)=0.549467125095128216
       GI(26)=-GI(25)
       GI(27)=0.483075801686178714
       GI(28)=-GI(27)
       GI(29)=0.413779204371605012
       GI(30)=-GI(29)
       GI(31)=0.341994090825758510 
       GI(32)=-GI(31)
       GI(33)=0.26815218500725378
       GI(34)=-GI(33)
       GI(35)=0.19269758070137116
       GI(36)=-GI(35)
       GI(37)=0.11608407067525524
       GI(38)=-GI(37)
       GI(39)=0.03877241750605082 
       GI(40)=-GI(39)

*
*      pesos de Gauss
*
       OME(1)=0.0045212770985332
       OME(2)=OME(1)
       OME(3)=0.0104982845311528 
       OME(4)=OME(3)
       OME(5)=0.0164210583819079 
       OME(6)=OME(5)
       OME(7)=0.0222458491941670
       OME(8)=OME(7)
       OME(9)=0.0279370069800234
       OME(10)=OME(9)
       OME(11)=0.0334601952825478 
       OME(12)=OME(11)
       OME(13)=0.0387821679744720
       OME(14)=OME(13)
       OME(15)=0.0438709081856733
       OME(16)=OME(15)
       OME(17)=0.0486958076350722
       OME(18)=OME(17)
       OME(19)=0.0532278469839368
       OME(20)=OME(19)
       OME(21)=0.0574397690993916 
       OME(22)=OME(21)
       OME(23)=0.0613062424929289
       OME(24)=OME(23)
       OME(25)=0.0648040134566010
       OME(26)=OME(25)
       OME(27)=0.0679120458152339
       OME(28)=OME(27)
       OME(29)=0.0706116473912868
       OME(30)=OME(29)
       OME(31)=0.0728865823958041 
       OME(32)=OME(31)
       OME(33)=0.0747231690579683
       OME(34)=OME(33)
       OME(35)=0.0761103619006262
       OME(36)=OME(35)
       OME(37)=0.0770398181642480 
       OME(38)=OME(37)
       OME(39)=0.0775059479784248
       OME(40)=OME(39)


	gg=0.
      ax=(x2-x1)/2
      bx=(x2+x1)/2
      ay=(y2-y1)/2
      by=(y1+y2)/2
	comp=2*dsqrt(ax**2+ay**2)
	cjac=comp/2
      SEN=(X1-X2)/COMP
      COS=(Y2-Y1)/COMP
      pi2=6.28318
      do 40 i=1,40
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen
c	gg=gg+(ra**2)*(4*dlog(ra)-1.)*aux*ome(i)*cjac/16+
c	*(dexp(-ra)*(2.-ra))*aux*ome(i)*cjac															
c	gg=gg+ra*(dlog(ra)+dexp(-ra))*aux*ome(i)*cjac
c     radial simples

c	gg=gg+ra*aux*ome(i)*cjac/3
c
c	função log
	gg=gg+(ra**2)*(4*dlog(ra)-1.)*aux*ome(i)*cjac/16  ! (r²/16)*[4*ln(r)-1]
c      gg=gg+ra*aux*ome(i)*cjac/3+0.5*aux*ome(i)*cjac
c	gg=gg+(dexp(-ra)*(2.-ra))*aux*ome(i)*cjac
c	write(imp,*)i,'valor de gg na inte1=',gg
  40   continue
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
	real*8 gi(40),ome(40),ax,x2,x1,bx,ay,y2,yp,aux,cos,hg1,hgar
	real*8 y1,by,comp,cjac,pi2,xco(40),yco(40),ra,xp,sen   
		 
	 GI(1)=0.998237709710559340
       GI(2)=-GI(1)
       GI(3)=0.990726238699457038
       GI(4)=-GI(3)
       GI(5)=0.977259949983774336
       GI(6)=-GI(5)
       GI(7)=0.957916819213791734
       GI(8)=-GI(7)
       GI(9)=0.932812808278676532 
       GI(10)=-GI(9)
       GI(11)=0.902098806968874330 
       GI(12)=-GI(11)
       GI(13)=0.865959503212259528
       GI(14)=-GI(13)
       GI(15)=0.824612230833311726
       GI(16)=-GI(15)
       GI(17)=0.778305651426519424
       GI(18)=-GI(17)
       GI(19)=0.727318255189927122 
       GI(20)=-GI(19)
	 GI(21)=0.671956684614179620
       GI(22)=-GI(21)
       GI(23)=0.612553889667980218
       GI(24)=-GI(23)
       GI(25)=0.549467125095128216
       GI(26)=-GI(25)
       GI(27)=0.483075801686178714
       GI(28)=-GI(27)
       GI(29)=0.413779204371605012
       GI(30)=-GI(29)
       GI(31)=0.341994090825758510 
       GI(32)=-GI(31)
       GI(33)=0.26815218500725378
       GI(34)=-GI(33)
       GI(35)=0.19269758070137116
       GI(36)=-GI(35)
       GI(37)=0.11608407067525524
       GI(38)=-GI(37)
       GI(39)=0.03877241750605082 
       GI(40)=-GI(39)

*
*      pesos de Gauss
*
       OME(1)=0.0045212770985332
       OME(2)=OME(1)
       OME(3)=0.0104982845311528 
       OME(4)=OME(3)
       OME(5)=0.0164210583819079 
       OME(6)=OME(5)
       OME(7)=0.0222458491941670
       OME(8)=OME(7)
       OME(9)=0.0279370069800234
       OME(10)=OME(9)
       OME(11)=0.0334601952825478 
       OME(12)=OME(11)
       OME(13)=0.0387821679744720
       OME(14)=OME(13)
       OME(15)=0.0438709081856733
       OME(16)=OME(15)
       OME(17)=0.0486958076350722
       OME(18)=OME(17)
       OME(19)=0.0532278469839368
       OME(20)=OME(19)
       OME(21)=0.0574397690993916 
       OME(22)=OME(21)
       OME(23)=0.0613062424929289
       OME(24)=OME(23)
       OME(25)=0.0648040134566010
       OME(26)=OME(25)
       OME(27)=0.0679120458152339
       OME(28)=OME(27)
       OME(29)=0.0706116473912868
       OME(30)=OME(29)
       OME(31)=0.0728865823958041 
       OME(32)=OME(31)
       OME(33)=0.0747231690579683
       OME(34)=OME(33)
       OME(35)=0.0761103619006262
       OME(36)=OME(35)
       OME(37)=0.0770398181642480 
       OME(38)=OME(37)
       OME(39)=0.0775059479784248
       OME(40)=OME(39)

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
      
      do 1400 i=1,40
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen
      hgar=(0.5-dlog(ra))*ome(i)*aux*cjac/(2*pi2) ! W=(G,i*ni) !mas está regularizando o termo que já está no contorno pelo teorema da divergencia ???
	hg1=hg1+hgar
1400   continue   ! onde está o calculo da matriz ggar
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

      return
      end
c
	 SUBROUTINE MINV(A,N)
	implicit none
	integer imp,k,n,j,i,ji,ki
	real*8 lo(1500),mo(1500),biga,a(1500,1500),hold

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






      subroutine output(x,y,fi,dfi,xxc,yyc,sol,dsolx,dsoly,nx,sai,
     *tempo,omega)
	implicit none
      common ne,np,l,lec,imp,npi,nt
	common dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2
 	integer	i,nt,k,l,ne,np,lec,imp,npi,nx,ini,fim
	real*8 x(nx),y(nx),fi(nx),dfi(nx),xxc(nx),yyc(nx)
	real*8 sol(nx),dsolx(nx),dsoly(nx),ti(nx),sbs,teta,tfo
	real*8 total,sn,cs,ym(nx),xm(nx),dx1,dx2,ge,ttb,fr,omega
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,aux,total2,tempo,parcial
      real*8 maiorAnalitico
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
	fim=122
       end if
	
      aux=0
	total=0.0
      maiorAnalitico=0
      
      fr = omega
      

      DO i=ini,fim
         ti(i)=sin(fr*x(i))/(fr*cos(fr*1.0))
         if(maiorAnalitico.LT.abs(ti(i))) then
             maiorAnalitico = abs(ti(i))
         endif  
      enddo
      
      
            DO 611 i=ini,fim
c	     RESPOSTA ANALITICA exponencial
c	  ti=sinh(x(i))/cosh(1.0)
c	  ti=(1.0-x(i))
c	      RESPOSTA ANALITICA harmonica
!	  ti(i)=sin(fr*x(i))/(fr*cos(fr*1.0))
c	  ti=0.5-x(i)*2/3
c	     RESPOSTA ANALITICA SENOIDAL
c	  ti(i)=dsin(x(i))-x(i)*cos(1.0)
c       ti(i)=dcos(x(i))-cos(1.0)
c         CALCULO DO ERRO
c	write(imp,*)i,x(i),ti(i)
   	ge=abs((abs(ti(i))-abs(fi(i)))/maiorAnalitico)*100
c  	write(imp,*)
 	total=total+ge
	aux=aux+1
	WRITE(imp,581)fi(i),ti(i),ge
581   format (2X,f12.8,2x,f12.8,2x,f12.8)
611   CONTINUE
       total=total/aux
	write(imp,*)total
      write(*,*) fr
       return
      end
      
      
!	DO 611 i=203,241
!
!c	      DO 611 i=ini,fim
!c	     RESPOSTA ANALITICA exponencial
!c	  ti=sinh(x(i))/cosh(1.0)
!c	  ti=(1.0-x(i))
!c	      RESPOSTA ANALITICA harmonica
!c	  ti(i)=sin(omega*x(i))/(omega*cos(omega*1.0))
!
!c      denominador da fração
!c
!	if(omega.gt.3)go to 618
!	ttb=(1.0-2.249997*((omega/3))**2+
!     *1.2656208*((omega/3)**4)
!     * -0.3163866*((omega/3)**6)+0.0444479*((omega/3)**8))
!c	write(imp,*)ttb
!	  go to 619
!618   continue
!	  teta=omega-0.78539816-0.04166397*(3./omega)
!     *-0.0003954*(3./omega)**2+0.0026257*(3./omega)**3
!     *-0.00054125*(3./omega)**4-0.0001356*(3./omega)**6
!      tfo=0.79788456-0.0000077*(3./omega)-.00552740*(3./omega)**2
!     *-.00009512*(3./omega)**3+0.00137237*(3./omega)**4
!     *-.00072805*(3/omega)**5 
!	ttb=(omega**(-0.5))*tfo*dcos(teta)
!c	write(imp,*)ttb
!619   continue
!c
!c	funcao no numerador
!c
!	sbs=omega*x(i)
!	if(sbs.gt.3)go to 615
!	ti(i)=(1.0-2.249997*((sbs/3))**2+1.2656208*((sbs/3)**4)
!     *-0.3163866*((sbs/3)**6)+0.0444479*((sbs/3)**8))
!      ti(i)=ti(i)/ttb
!	go to 616
!615   continue
!	teta=sbs-0.78539816-0.04166397*(3./sbs)-0.0003954*(3./sbs)**2
!     *+0.0026257*(3./sbs)**3-0.00054125*(3./sbs)**4-0.0001356*(3/sbs)**6
!	tfo=0.79788456-0.0000077*(3./sbs)-.00552740*(3./sbs)**2
!     *-.00009512*(3./sbs)**3+0.00137237*(3./sbs)**4-.00072805*(3/sbs)**5 
!	ti(i)=(sbs**(-0.5))*tfo*dcos(teta)/ttb
!616   continue
!c	  ti=0.5-x(i)*2/3
!c	     RESPOSTA ANALITICA SENOIDAL
!c	  ti(i)=dsin(x(i))-x(i)*cos(1.0)
!
!c       ti(i)=dcos(x(i))-cos(1.0)
!c         CALCULO DO ERRO
!c	write(imp,*)i,x(i),ti(i)
!c    	ge=abs((abs(ti(i))-abs(fi(i)))/abs(ti(i)))*100
!	ge=abs((abs(ti(i))-abs(fi(i)))/1.0)*100
!
!c  	write(imp,*)
! 	total=total+ge
!	aux=aux+1
!	WRITE(imp,581)fi(i),ti(i),ge
!581   format (2X,f12.8,2x,f12.8,2x,f12.8)
!611   CONTINUE
!       total=total/aux
!	write(imp,*)total
!       return
!      end
