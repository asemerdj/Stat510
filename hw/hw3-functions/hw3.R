q.formula=function(a,b,c){
  rad=b^2-(4*a*c)
  if(all(rad>=0)){
    rad=sqrt(rad)
  }else{rad=sqrt(as.complex(rad))}
  return(cbind(-b-rad,-b+rad)/(2*a))
}
q.formula(2,10,12)

##1a
area.f=function(r){
  area=pi*r^2
  return(round(area,digits=2))
}
area.f(c(2,4,6))

##1b
f.to.c=function(f){
  conversion=(5/9)*(f-32)
  return(round(conversion,digits = 2))
}
f.to.c(c(32,60,90,110,212))

##2
gomp=function(a,k,x0,x){
  y=a*exp(-exp(-k*(x-x0)))
  return(y)
}

x=seq(0,15,0.02)
g_a3=gomp(3,0.5,0,x)
g_a9=gomp(9,0.5,0,x)
g_a12=gomp(12,0.5,0,x)

plot(x,g_a3,ylab="",type="l",lty=1,ylim=c(0,15))
lines(x,g_a9,ylab="",type="l",lty=2,col=2)
lines(x,g_a12,ylab="",type = "l",lty=3,col=4)
legend(locator(1),lty=c(1,2,3),col=c(1,2,4),c("a=3","a=9","a=12"),cex=1.5)

##b
x=seq(0,15,0.02)
g_k0.3=gomp(9,0.3,0,x)
g_k0.7=gomp(9,0.7,0,x)
g_k1.1=gomp(9,1.1,0,x)

plot(x,g_k0.3,ylab="",type="l",lty=1,ylim=c(0,15))
lines(x,g_k0.7,ylab="",type="l",lty=2,col=2)
lines(x,g_k1.1,ylab="",type = "l",lty=3,col=4)
legend(locator(1),lty=c(1,2,3),col=c(1,2,4),c("k=0.3","k=0.7","k=1.1"),cex=1.5)

##c
x=seq(-10,20,0.02)
g_x0_0=gomp(7,0.5,0,x)
g_x0_4=gomp(7,0.5,4,x)
g_x0_8=gomp(7,0.5,8,x)

plot(x,g_x0_0,ylab="",type="l",lty=1,ylim=c(-10,20))
lines(x,g_x0_4,ylab="",type="l",lty=2,col=2)
lines(x,g_x0_8,ylab="",type = "l",lty=3,col=4)
legend(locator(1),lty=c(1,2,3),col=c(1,2,4),c("x0=0","x0=4","x0=8"),cex=1.5)

##d
vonb=function(a,k,t0,t){
  y=a*(1-exp(-k*(t-t0)))
  return(y)
}

t=seq(0,15,0.02)
a=c(3,9,12)
k=c(0,0,0)
t0=c(0,0,0)
nam1=paste0("vb_a.",a[1],"_k.",k[1],"_t0.",t0[1])
nam2=paste0("vb_a.",a[2],"_k.",k[2],"_t0.",t0[2])
nam3=paste0("vb_a.",a[3],"_k.",k[3],"_t0.",t0[3])

b=gomp(a[1],k[1],t0[1],x)
c=gomp(a[2],k[2],t0[2],x)
d=gomp(a[3],k[3],t0[3],x)

plot(x,b,ylab="",type="l",lty=1,ylim=c(0,15))
lines(x,c,ylab="",type="l",lty=2,col=2)
lines(x,d,ylab="",type = "l",lty=3,col=4)
legend(locator(1),lty=c(1,2,3),col=c(1,2,4),c(nam1,nam2,nam3),cex=1.5)
