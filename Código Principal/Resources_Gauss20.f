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


C	 GI(1)=0.993128599185094
C       GI(2)=-GI(1)
C       GI(3)=0.963971927277913
C       GI(4)=-GI(3)
C       GI(5)=0.912234428251325
C       GI(6)=-GI(5)
C       GI(7)=0.839116971822218
C       GI(8)=-GI(7)
C       GI(9)=0.746331906460150
C       GI(10)=-GI(9)
C       GI(11)=0.636053680726515
C       GI(12)=-GI(11)
C       GI(13)=0.510867001950827
C       GI(14)=-GI(13)
C       GI(15)=0.373706088715419
C       GI(16)=-GI(15)
C       GI(17)=0.227785851141645
C       GI(18)=-GI(17)
C       GI(19)=0.076526521133497
C       GI(20)=-GI(19)
C
C      pesos de Gauss
C
C       OME(1)=0.017614007139152
C       OME(2)=OME(1)
C       OME(3)=0.040601429800386
C       OME(4)=OME(3)
C       OME(5)=0.062672048334109
C       OME(6)=OME(5)
C       OME(7)=0.083276741576704
C       OME(8)=OME(7)
C       OME(9)=0.101930119817240
C       OME(10)=OME(9)
C       OME(11)=0.118194531961518
C       OME(12)=OME(11)
C       OME(13)=0.131688638449176
C       OME(14)=OME(13)
C       OME(15)=0.142096109318382
C       OME(16)=OME(15)
C       OME(17)=0.149172986472603
C       OME(18)=OME(17)
C       OME(19)=0.152753387130725
C       OME(20)=OME(19)
C
C
      ax=(x2-x1)/2 !tamanho x da metade do elemento
      bx=(x2+x1)/2 !valor da coordenada x da metade do elemento
      ay=(y2-y1)/2 !tamanho y da metade do elemento
      by=(y1+y2)/2 !valor da coordenada y da metade do elemento
	comp=2*dsqrt(ax**2+ay**2) !comprimento do elemento
	cjac=comp/2 !metade do comprimento do elemento (comp. jacobiana)
      SEN=(X1-X2)/COMP !seno do elemento
      COS=(Y2-Y1)/COMP ! cosseno do elemento
		     
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
		xco(i)=ax*gi(i)+bx !valor da coordenada do pto central do elemento +- metade do comprimento ponderado pelo ponto de gauss
		yco(i)=ay*gi(i)+by
		ra=dsqrt((xco(i)-xp)**2+(yco(i)-yp)**2) ! raio = sqrt(ri²)
		aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen ! multiplicação ri*ni = [x(ksi)-x(j)]*n1+[y(ksi)-y(j)]*n2 = [x(ksi)-x(j)]*cos+[y(ksi)-y(j)]*sen
		h=-aux*ome(i)*cjac/(pi2*ra**2) ! calculo matriz h = (-1/2pi)*(1/r)*(dr/dn)*ni = (-1/2pi)*(1/r)*r,i*ni = (-1/2pi)*(1/r)*(ri/r)*ni = (-1/2pi)*(1/r²)*(ri*ni)
		g=dlog(1/ra)*ome(i)*cjac/pi2   ! calculo matriz g = ln(1/r)/2pi
		gx=ome(I)*cjac*(XCO(i)-XP)/(pi2*ra**2) !nao usado
		gy=ome(I)*cjac*(YCO(i)-YP)/(pi2*ra**2) !nao usado
		hx=OME(I)*cjac*(2*AUX*(XP-XCO(i))+COS*ra*ra)/(pi2*ra**4) !nao usado
		hy=OME(I)*cjac*(2*AUX*(YP-YCO(i))+SEN*ra*ra)/(pi2*ra**4) !nao usado
		h1=h1-(gi(i)-1)*h/2 !h com phi1     !VERIFICAR
		h2=h2+(gi(i)+1)*h/2 !h com phi2     !VERIFICAR
		g1=g1-(gi(i)-1)*g/2 !g com phi1     !VERIFICAR
		g2=g2+(gi(i)+1)*g/2 !g com phi2     !VERIFICAR - É POR CAUSA DO ELEMENTO SER LINEAR ?
 		dx1=dx1-(gi(i)-1)*gx/2 !nao usado
		dx2=dx2+(gi(i)+1)*gx/2 !nao usado
		dy1=dy1-(gi(i)-1)*gy/2 !nao usado
		dy2=dy2+(gi(i)+1)*gy/2 !nao usado
		ex1=ex1-(gi(i)-1)*hx/2 !nao usado
		ex2=ex2+(gi(i)+1)*hx/2 !nao usado
		ey1=ey1-(gi(i)-1)*hy/2 !nao usado
 40		ey2=ey2+(gi(i)+1)*hy/2 !nao usado
	return
      end

      subroutine inlo(x1,y1,x2,y2,g1,g2)
	implicit none
	real*8 sep,g1,x1,x2,y1,y2,g2

	sep=dsqrt((x2-x1)**2+(y2-y1)**2) !calculo do raio do elemento
	g1=sep*(1.5-dlog(sep))/(2*6.28318) !g com phi1     !VERIFICAR
      g2=sep*(0.5-dlog(sep))/(2*6.28318) !g com phi2     !VERIFICAR - É POR CAUSA DO ELEMENTO SER LINEAR ?
      continue
      return
      end
c
c

	subroutine galerk1(xp,yp,x1,y1,x2,y2,hg1,hg2,gg1,gg2)  
	common ne,np,l,lec,imp,npi,nt
	real*8 xco(40),yco(40),gi(40),ome(40)
	real*8 hg1,hg2,gg1,gg2,ax,x1,x2,bx,ay,y1,y2,by,comp
	real*8 ra,xp,yp,aux,h,g,cjac,gx,gy,hx,hy,sen,coss
	real*8 hgar,ggar

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


C	 GI(1)=0.993128599185094
C       GI(2)=-GI(1)
C       GI(3)=0.963971927277913
C       GI(4)=-GI(3)
C       GI(5)=0.912234428251325
C       GI(6)=-GI(5)
C       GI(7)=0.839116971822218
C       GI(8)=-GI(7)
C       GI(9)=0.746331906460150
C       GI(10)=-GI(9)
C       GI(11)=0.636053680726515
C       GI(12)=-GI(11)
C       GI(13)=0.510867001950827
C       GI(14)=-GI(13)
C       GI(15)=0.373706088715419
C       GI(16)=-GI(15)
C       GI(17)=0.227785851141645
C       GI(18)=-GI(17)
C       GI(19)=0.076526521133497
C       GI(20)=-GI(19)

C      pesos de Gauss

C       OME(1)=0.017614007139152
C       OME(2)=OME(1)
C       OME(3)=0.040601429800386
C       OME(4)=OME(3)
C       OME(5)=0.062672048334109
C       OME(6)=OME(5)
C       OME(7)=0.083276741576704
C       OME(8)=OME(7)
C       OME(9)=0.101930119817240
C       OME(10)=OME(9)
C       OME(11)=0.118194531961518
C       OME(12)=OME(11)
C       OME(13)=0.131688638449176
C       OME(14)=OME(13)
C       OME(15)=0.142096109318382
C       OME(16)=OME(15)
C       OME(17)=0.149172986472603
C       OME(18)=OME(17)
C       OME(19)=0.152753387130725
C       OME(20)=OME(19)

       
      ax=(x2-x1)/2
      bx=(x2+x1)/2
      ay=(y2-y1)/2
      by=(y1+y2)/2
	comp=2*sqrt(ax**2+ay**2)
	cjac=comp/2 ! cjac é o jacobiano, igual ao comprimento sobre 2
      SEN=(X1-X2)/COMP
      COSS=(Y2-Y1)/COMP
      
	hg1=0
	hg2=0
	gg1=0
	gg2=0
	pi2=6.28318
      
      do 1400 i=1,40
      xco(i)=ax*gi(i)+bx
      yco(i)=ay*gi(i)+by
      ra=sqrt((xco(i)-xp)**2+(yco(i)-yp)**2)
	aux=(xco(i)-xp)*coss+(yco(i)-yp)*sen
      ggar=ra*ra*(1.-dlog(ra))*ome(i)*cjac/(4*pi2) !G*=(r²/8pi)*(1-ln(r))
      hgar=(0.5-dlog(ra))*ome(i)*aux*cjac/(2*pi2)  !G,i*ni=(1/4pi)*[0.5-ln(r)]*aux
	hg1=hg1-(gi(i)-1)*hgar/2
      hg2=hg2+(gi(i)+1)*hgar/2
      gg1=gg1-(gi(i)-1)*ggar/2
      gg2=gg2+(gi(i)+1)*ggar/2
1400  continue
      return
      end
c
c
	subroutine galerk2(x1,y1,x2,y2,gg1,gg2)
c
c	determinação dos coeficientes da integração de G* DIRETA no próprio elemento, ponto csi nos extremos
c
	real*8 sep,gg1,gg2,x1,x2,y1,y2

      sep=sqrt((x2-x1)**2+(y2-y1)**2)
    	gg1=(sep**3)*(19.0-12*dlog(sep))/(1152*3.1415926) !encontrar origem dessa integral ???
      gg2=(sep**3)*(5.0-4*dlog(sep))/(128*3.1415926)
      continue
      return
      end


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


C	 GI(1)=0.993128599185094
C       GI(2)=-GI(1)
C       GI(3)=0.963971927277913
C       GI(4)=-GI(3)
C       GI(5)=0.912234428251325
C       GI(6)=-GI(5)
C       GI(7)=0.839116971822218
C       GI(8)=-GI(7)
C       GI(9)=0.746331906460150
C       GI(10)=-GI(9)
C       GI(11)=0.636053680726515
C       GI(12)=-GI(11)
C       GI(13)=0.510867001950827
C       GI(14)=-GI(13)
C       GI(15)=0.373706088715419
C       GI(16)=-GI(15)
C       GI(17)=0.227785851141645
C       GI(18)=-GI(17)
C       GI(19)=0.076526521133497
C       GI(20)=-GI(19)

C      pesos de Gauss

C       OME(1)=0.017614007139152
C       OME(2)=OME(1)
C       OME(3)=0.040601429800386
C       OME(4)=OME(3)
C       OME(5)=0.062672048334109
C       OME(6)=OME(5)
C       OME(7)=0.083276741576704
C       OME(8)=OME(7)
C       OME(9)=0.101930119817240
C       OME(10)=OME(9)
C       OME(11)=0.118194531961518
C       OME(12)=OME(11)
C       OME(13)=0.131688638449176
C       OME(14)=OME(13)
C       OME(15)=0.142096109318382
C       OME(16)=OME(15)
C       OME(17)=0.149172986472603
C       OME(18)=OME(17)
C       OME(19)=0.152753387130725
C       OME(20)=OME(19)

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
	aux=(xco(i)-xp)*cos+(yco(i)-yp)*sen !ri*ni
c	gg=gg+(ra**2)*(4*dlog(ra)-1.)*aux*ome(i)*cjac/16+
c	*(dexp(-ra)*(2.-ra))*aux*ome(i)*cjac															
c	gg=gg+ra*(dlog(ra)+dexp(-ra))*aux*ome(i)*cjac
c     radial simples

c	gg=gg+ra*aux*ome(i)*cjac/3
c
c	função log
	gg=gg+(ra**2)*(4*dlog(ra)-1.)*aux*ome(i)*cjac/16  !(r²/16)*[4*ln(r)-1]*(ri*ni)
c      gg=gg+ra*aux*ome(i)*cjac/3+0.5*aux*ome(i)*cjac
c	gg=gg+(dexp(-ra)*(2.-ra))*aux*ome(i)*cjac
c	write(imp,*)i,'valor de gg na inte1=',gg
  40   continue
      return
      end
c
c
c
 !     subroutine inlo1(x1,y1,x2,y2,gg)
	!implicit none
	!common ne,np,l,lec,imp,npi,nt
	!integer imp,ne,np,l,lec,npi,nt
	!real*8 ax,x1,x2,ay,y1,y2,comp,aux,ra,gg,sen,cos
 !
	!ax=(x2-x1)/2
 !     ay=(y2-y1)/2
	!comp=2*dsqrt(ax**2+ay**2)
	!SEN=(X1-X2)/COMP
 !     COS=(Y2-Y1)/COMP
 ! 	aux=(x2-x1)*cos+(y2-y1)*sen
 !     ra=dsqrt((x2-x1)**2+(y2-y1)**2)
 !     gg=aux/(2*6.28318)
 ! 
 !      write(imp,*)'finalmente entrei na inlo1'
	!write(imp,*)'gg=',gg
 !     return
 !     end
c
c
      subroutine inlo1(xp,yp,x1,y1,x2,y2,b1,b2)
	implicit none
	common ne,np,l,lec,imp,npi,nt
	integer imp,ne,np,l,lec,npi,nt
	real*8 x1,x2,y1,y2,xp,yp,sep
	real*8 b1,b2,wrop1,wrop2,auxil
      sep=sqrt((x2-x1)**2+(y2-y1)**2)
	
c	write(imp,*)'x1=',x1,'x2=',x2
c 	write(imp,*)'y1=',y1,'y2=',y2
c	write(imp,*)'xp=',xp,'yp=',yp


      wrop1=sqrt((xp-x1)**2+(yp-y1)**2)
   	wrop2=sqrt((xp-x2)**2+(yp-y2)**2)

c	write(imp,*)'wrop1=',wrop1
c	write(imp,*)'wrop2=',wrop2

	if(wrop1.gt.wrop2)go to 11
	auxil=wrop1
	wrop1=wrop2
	wrop2=auxil
	b1=-(wrop1/(sep*6.28318))*(wrop1*dlog(wrop1)    ! DE ONDE VEIO ESSA EQUAÇÃO DE INTEGRAÇÃO.
     *+wrop2*dlog(wrop2)-sep)-(1.0/(sep*2*6.28318))  
     **((wrop2**2)*dlog(wrop2)-wrop1**2*dlog(wrop1)   ! O QUE É b1 E b2 ?
     *+sep*sep/2-sep*wrop2)   
     	b2=-(wrop2/(sep*6.28318))*(wrop1*dlog(wrop1)
     *+wrop2*dlog(wrop2)-sep)+(1.0/(sep*2*6.28318))
     **((wrop2**2)*dlog(wrop2)-wrop1**2*dlog(wrop1)
     *+sep*sep/2-sep*wrop2)
11    continue	
 	b2=-(wrop1/(sep*6.28318))*(wrop1*dlog(wrop1)
     *+wrop2*dlog(wrop2)-sep)-(1.0/(sep*2*6.28318))
     **((wrop2**2)*dlog(wrop2)-wrop1**2*dlog(wrop1)
     *+sep*sep/2-sep*wrop2)
     	b1=-(wrop2/(sep*6.28318))*(wrop1*dlog(wrop1)
     *+wrop2*dlog(wrop2)-sep)+(1.0/(sep*2*6.28318))
     **((wrop2**2)*dlog(wrop2)-wrop1**2*dlog(wrop1)
     *+sep*sep/2-sep*wrop2)
  30  continue
	return
      end

      subroutine inter(fi,dfi,kode,nx)
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
      if (kode(i)) 20,20,10 ! fluxo (1)-10 e potencial (0)-20
 10   ch=fi(i) !é fluxo (dfi devolve o potencial calculado para o fi)
      fi(i)=dfi(i)
      dfi(i)=ch
 20   continue !é potencial (está correto os pontos do contorno) mas, dfi devolve o potencial calculado para os pontos internos 
      do 21 i=np+1,nt                                                                     !(pois q=0 pts internos)
	ch=fi(i)
      fi(i)=dfi(i)
      dfi(i)=ch
 21   continue
      
	do 22 i=1,nt    !IMPRESSÃO POTENCIAL
	write(imp,*)'fi=',fi(i)
22    continue
      return
      end
c
      
      
      !Rotina de inversão da matriz (F-¹) 
	SUBROUTINE MATRIZINVERSA(A,N) 
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
     1'dentro da rotina MATRIZINVERSA - inversao de matriz')
      END
  
      
