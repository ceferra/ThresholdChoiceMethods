
cline<-function (x,pend,vinit)
{
  return (pend*x+vinit)
}

pdf("explots/simulatedcurves.pdf")
valphabeta<-c(0,1,2,4,8,16,Inf)

mcolors<-c("mistyrose2", "lightpink","violet","purple","mediumpurple3","mediumpurple4","black")
steps <-1000
alphabeta<-2

proptest<-0.4447
x<-c(1:steps)/steps
l1<-cline(x,2*proptest,0)
l2<-cline(x,-2*(1-proptest),2*(1-proptest))
leg.txt <-c()
for (ia in 1:length(valphabeta)) 
{
  
  alphabeta<-valphabeta[ia]
  km<-1000
  fv<-c(1:km)
  
  for (it in 1:km) 
  {
    creal<-it/km
    
    
    
    if (alphabeta<Inf)
      {
      v<-pbeta(1-proptest,alphabeta*creal + 1,alphabeta-alphabeta*creal + 1)
      fv[it]<-2*((1-proptest)*(1 -creal -v)+ v*creal )
    }
    
    #fv[it]<-2*((v)*creal*proptest+(1-v)*(1-proptest-(1-proptest)*creal))
    #fv[it]<-2*((1-proptest) + (creal* (proptest-1)) + (v*(proptest+creal-1)))
    #fv[it]<-2*(((proptest-1)*(creal-1) ) + (v*(proptest+creal-1)))
    #fv[it]<-2*(1-proptest) +2*creal*(proptest-1)  + 2*v*creal + 2*(proptest-1)*v
    #fv[it]<-2*((1-proptest) -creal*(1-proptest)  + v*creal -(1-proptest)*v)
    
    if (alphabeta==Inf)
    {
      if (creal<(1-proptest)) fv[it]<-cline(creal,2*proptest,0)
      else fv[it]<-cline(creal,-2*(1-proptest),2*(1-proptest))
    }
  }
 if (ia==1) plot(c(0,1),c(0,0),xlab= "cost",ylab="loss", ylim=c(0,1), col="white",cex.main=0.8)
  title("Cost Curves", cex.main = 1.4)#0.7
  
  # plot(fv, ylim=c(0,1), type="l", col=mcolors[ia])
   lines(c(1:km)/km,fv, ylim=c(0,1), type="l", col=mcolors[ia])
  
  t1<-paste(alphabeta,format(mean(fv),digits=4))
  leg.txt <- c(leg.txt,t1)
}


y.leg <- c(4.5, 3, 2.1, 1.4, .7)
cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)
#cols   <-c(2:(ncurv+1))
xoff<-0.35 #xoffset of the legend

j<-1
for (i in seq(cexv)) {
  legend(xoff, y.leg[i]+0.33, leg.txt, pch = "___", col = mcolors, cex = 2*cexv[i])
  j<-j+1
}

dev.off()
#plot(c(0,1),c(0,0), ylim=c(0,1), col="white",main=title,cex.main=0.8)
#plot(fv, ylim=c(0,1), type="l")

####alphabeta =2 
# mean (fv)
# 0.361046
##Integrant amb wofram
##v=integrate [CDF[BetaDistribution[(2*x + 1),(2-(2*x + 1)+2)],0.5553]] from 0 to 1= 0.567497
##Integrate[BetaRegularized[0.5553,2*x + 1,3-2*x ]x, {x,0,1}] = 0.217991

#print(2*(1-proptest) +(proptest-1) + 2 *0.217991+ 2*(proptest-1)*0.567497)
