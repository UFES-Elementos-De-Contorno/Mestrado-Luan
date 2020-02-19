      !IMPRESSÃO DO RESULTADO
      
      subroutine output(x,y,fi,dfi,nx,fr)
	implicit none
      common ne,np,l,lec,imp,npi,nt
      real*8 fr,maiorAnalitico
	common dx1,dx2,dy1,dy2,ex1,ex2,ey1,ey2
 	integer	i,nt,k,l,ne,np,lec,imp,npi,nx,ini,fim
	real*8 x(nx),y(nx),fi(nx),dfi(nx),xxc(nx),yyc(nx)
	real*8 sol(nx),dsolx(nx),dsoly(nx),ti(nx)
	real*8 total,sn,cs,ym(nx),xm(nx),dx1,dx2,ge,bi
	real*8 dy1,dy2,ex1,ex2,ey1,ey2,aux,total2,tempo
	character sai*20

c	 write(imp,77) tempo
c 77	format(/'Tempo de Processamento: ',f8.3,' segundos'/)
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

	
      
      ini=2                       !o que é feito aqui ???
	if (ne.eq.8) then           !o que é feito aqui ???
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
	fim=122   !ATÉ 320 elementos
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
