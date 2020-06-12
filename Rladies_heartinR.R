#-------------------------------------------------------------------------
# Que tal declarar para o seu amor utilização programação?
# Nós do R-Ladies Lavras organizamos esse script com varias formas de declarar
# para o seu amor nesse dia 12/06. 
# Use sua criatividade e feliz dia dos namorados
#-------------------------------------------------------------------------

# CORAÇÃO 1 - FORMULA DO AMOR ---------------------------------------------


core=function(nome="Meu amor")
{
  x=seq(-2,2,0.001)
  y1=sqrt(1-((abs(x)-1)^2))
  y2=-3*sqrt(1-(sqrt(abs(x))/sqrt(2)))
  plot(main="Enfim, achei uma fórmula pra explicar", ylab="RLADIES-LAVRAS",xlab="o que sinto por você...",sub=nome,c(x,x),c(y1,y2),lwd=3,pch=20,cex=6)
  grid()
  cores=paste(c("red","tomato"),sort(rep(1:4,2)),sep="")
  eq=c("( AM + BC ) * X = AM ( X + BOC ) - BCTE","AMX + BCX = AMX + AMBOC - BCTE","BCX = AMX - AMX + BC (AMO - TE)","BCX = BC ( AMO - TE )","X = ???")
  pos=c(-0.3,-0.6,-0.9,-1.2,-1.5)
  cont=0
  for (i in 1:30)
  {
    points(c(x,x),c(y1,y2),col=cores[sample(1:8,1)],lwd=sample(1:4,1),pch=20,cex=round(runif(1,1,4)))
    if (i%%5 == 0 & i <=25)
    {
      cont=cont+1
      text(0,pos[cont],eq[cont])
    }
  }
  text(1.2,-2.5,"AMO-TE",col="red",cex=3)
}
core()

#FONTE: www.profmsouza.blogspot.com

# Se quiser personalizar, basta colocar o nome de sua amada ou amado (entre aspas) como parâmetro da função core (e.g.: core("Cíntia"), escreveria o nome de minha amada... rs)
# Bom namoro pra vocês!
#----------------------------------------------------------------~~


# CORAÇÃO 2: Happy Valentine's Day ----------------------------------------


heart1 = function(name){
  t = seq(0,60,len=100)
  plot(c(-8,8),c(0,20),type='n',axes=FALSE,xlab='',ylab='')
  x = -.01*(-t^2+40*t+1200)*sin(pi*t/180)
  y = .01*(-t^2+40*t+1200)*cos(pi*t/180)
  lines(x,y, lwd=4)
  lines(-x,y, lwd=4)
  text(0,7,"Happy Valentine's Day",col='red',cex=2.5)
  text(0,5.5,name,col='red',cex=2.5)
}
heart1()

#FONTE: https://blog.revolutionanalytics.com/2013/02/make-a-valentines-heart-with-r.html

# CORAÇÃO 3 - HOT COLOR HEART ---------------------------------------------


dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l"))

with(dat, polygon(x,y, col="hotpink"))   

points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5)


#FONTE: https://stackoverflow.com/questions/8082429/plot-a-heart-in-r


# CORAÇÃO 4: CARTESIANO  --------------------------------------------------

library(ggplot2)
dat <- data.frame(x=seq(0, 2*pi, length.out=100))
cardioid <- function(x, a=1)a*(1-cos(x))
ggplot(dat, aes(x=x)) + stat_function(fun=cardioid) + coord_polar()

heart <- function(x)2-2*sin(x) + sin(x)*(sqrt(abs(cos(x))))/(sin(x)+1.4)
ggplot(dat, aes(x=x)) + stat_function(fun=heart) + coord_polar(start=-pi/2)

#FONTE: https://stackoverflow.com/questions/8082429/plot-a-heart-in-r
