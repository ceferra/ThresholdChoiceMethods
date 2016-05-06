
############ROC#############
#########RCC################## #sense empats
roc <- function(inp,pplot,newplot)
{
  POSITIVE1<-TRUE
  BALANCELINE<-FALSE
  SPLITPOINTS<-FALSE
  COLOURCURVE<-1
  CONVEXSKULL<-TRUE
  ORDERDECREASING<-FALSE
  
  if (POSITIVE1) inp<-1-inp
  inp<-inp[order(inp[,2],decreasing=ORDERDECREASING), ]
  #print(inp)
  
  n<-length(inp[,1])
  np<-sum(inp[,1])
  nn<-n-np
  #Ordenem
  
  
  #Busquem empats:
  i<-1
  e<-c(1:3)
  e[1]<-1
  
  for (i in 2:n)
  {
    
    if (inp[i-1,2]!=inp[i,2]) 
    {
      e[i]<-1
      i<-i+1    
    }
    else
    { #print(i)
      e[i]<-0
      i<-i+1
    }
    #	print(i)
  }
 # print(e)
  
  if (pplot && newplot==TRUE) plot(c(0,1),c(0,0), ylim=c(0,1),xlab= "F1",ylab= "F0", col="white")
  n<-length(inp[,1])
  nn<-sum(inp[,1])
  np<-n-np
  Area<-0
#  print(n)
#  print("pos")
#  print(np)
#  print("neg")
#  print(nn)
  #stepy<-2/n cost
  stepyp<-1/np
  stepyn<-1/nn
  #step<-1/(trunc(n/2)+1)
  #print(step)
  
  #### BUCLE PRINCIPAL
  
  c<-1
  xo<-0
  yo<-0
  xn<-0
  yn<-0
  ap<-1
  an<-1
  
  lndx<-0
  lntx<-0
  lnly<-0
  lnry<-0
  tk1<-0
  tk2<-0
  nnp<-0
  npp<-0
  area<-0
  pointsx<-c()
  pointsy<-c()
  
  i<-1
  pix<-0
  piy<-0
  pfx<-0
  pfy<-0
  #diagonals
  while(i<(n+1))
  {
    #  xa<-0
    # ya<-0
    # if (lnly>1) xa<-lnly-1
    # if (lndx>1) ya<-lndx-1
    if (i>np) pix<-pix+stepyn
    else  piy<-piy+stepyp
    # print("punts i")
    # print(pix)
    # print(piy)
    if (i<=nn) pfx<-pfx+stepyn
    else pfy<-pfy+stepyp
    # print("punts f")
    #print(pfx)
    #print(pfy)
    lines(c(pix,pfx),c(piy,pfy), lty=1, col= "grey")
    if (BALANCELINE && i==np)	if (pplot && newplot==TRUE ) 	lines( c(pix,pfx),c(piy,pfy),lty=1, col= "black")
    
    i<-i+1
  }
  
  i<-1 
  
  while(i<(n+1))
  {
    #positiu
    if(inp[i,1]==0)
    { yn<-yo+stepyp*ap
      npp<-npp+1}
    else 
    {#negatiu
      xn<-xo+stepyn*an
      nnp<-nnp+1
    }
    
    if (e[i]==1)
    {
      
      
      if(inp[i,1]==0) yn<-yo+stepyp*ap
      else xn<-xo+stepyn*an
      if (pplot && SPLITPOINTS) points(c(xo,xn),c(yo,yn),col="blue4") 
      if (pplot && newplot==TRUE ) 	lines(c(xo,xn),c(yo,yn), lty=1, col= COLOURCURVE)	
      pointsx[i]<-xo
      pointsy[i]<-yo
      pointsx[i+1]<-xn
      pointsy[i+1]<-yn
      yo<-yn
      xo<-xn
      
      
    }
    else
    {
      if(inp[i,1]==0) ap<-ap+1
      else an<-an+1
    }
    i<-i+1
    
  }
  
  
  if (CONVEXSKULL)
  {
    
    rpx<-c(pointsx,1)
    rpy<-c(pointsy,0)
    #print(rpx)
    #print(rpy)
    z<-sort(chull(rpx,rpy))
#    print(z[1:length(z)-1])
    punts<-z[1:length(z)-1]
    
    nump<-length(punts)	
    
    for(i in (nump-1):1)
    {
      lines(c(pointsx[punts[i]],pointsx[punts[i+1]]),c(pointsy[punts[i]],pointsy[punts[i+1]]),col=COLOURCURVE,lty=2,lwd=2)
    }
  }
  roc<-area
}



########plot a set of vectors as a curve
plot_curves_test7<-function(sd,o,to,rd,rsd,sq,rsq,title="PLOT",newplot=TRUE)
{
  if (newplot) plot(c(0,1),c(0,0),xlab= COSTRATIO,ylab= LOSS, ylim=c(0,1), col="white",main=title)
  vector<-sd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=2)
  }
  t1<-paste("scoredriven",sd$area)
  ##########
  vector<-o$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=3)
  }
  t2<-paste("optimal",o$area)
  
  ############
  vector<-to$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=4)
  }
  t3<-paste("trainoptimal",to$area)
  #########
  vector<-rd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=5)
  }
  t4 <-paste("ratedriven",rd$area)
  
  #########
  vector<-rsd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=6)
  }
  t5 <-paste("ratescoredriven",sq$area)
  
  
  #########
  vector<-sq$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=7)
  }
  t6 <-paste("squared",sq$area)
  
  
  #########
  vector<-rsq$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=8)
  }
  t7 <-paste("rootsquared",rsq$area)
  
  leg.txt <- c(t1,t2,t3,t4,t5,t6,t7)
  
  y.leg <- c(4.5, 3, 2.1, 1.4, .7)
  cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)
  cols   <-c(2,3,4,5,6,7,8)
  j<-1
  for (i in seq(cexv)) {
    legend(0.02, y.leg[i]+0.2, leg.txt, pch = "___", col = cols, cex = 2*cexv[i])
    j<-j+1
  }
}



########plot a set of vectors as a curve
plot_curves_test6<-function(sd,o,rd,rsd,sq,rsq,title="PLOT",newplot=TRUE)
{
  if (newplot) plot(c(0,1),c(0,0),xlab= COSTRATIO,ylab= LOSS, ylim=c(0,1), col="white",main=title)
  vector<-sd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=2)
  }
  t1<-paste("scoredriven",sd$area)
  ##########
  vector<-o$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=3)
  }
  t2<-paste("optimal",o$area)
  
  
  #########
  vector<-rd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=4)
  }
  t3 <-paste("ratedriven",rd$area)
  
  #########
  vector<-rsd$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=5)
  }
  t4 <-paste("ratescoredriven",sq$area)
  
  
  #########
  vector<-sq$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=6)
  }
  t5 <-paste("squared",sq$area)
  
  
  #########
  vector<-rsq$points
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=7)
  }
  t6 <-paste("rootsquared",rsq$area)
  
  leg.txt <- c(t1,t2,t3,t4,t5,t6)
  
  y.leg <- c(4.5, 3, 2.1, 1.4, .7)
  cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)
  cols   <-c(2,3,4,5,6,7)
  j<-1
  for (i in seq(cexv)) {
    legend(0.02, y.leg[i]+0.2, leg.txt, pch = "___", col = cols, cex = 2*cexv[i])
    j<-j+1
  }
}


########plot a set of vectors as a curve
plot_curves_testl<-function(curves,title="",ytit=LOSS,newplot=TRUE,points=TRUE)
{
  if (newplot) plot(c(0,1),c(0,0),xlab= COSTRATIO,ylab=ytit, ylim=c(0,1), col="white",main=title,cex.main=0.8)
  title(title)
  ncurv =length(curves)
  leg.txt<-c()
  for (j in 1:(ncurv))
  {
    sd<-curves[[j]]
    # print(i)
    if (points) vector<-sd$points
    else vector<-sd$thres
    #  print(vector)
    tam<-length(vector)
    for (i in 1:(tam))
    {
      lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]),col=j+1)
    }
    # t1<-paste(sd$title,sd$area)
    t1<-paste(sd$title,format(sd$area,digits=4))
    leg.txt <- c(leg.txt,t1)
  }
  #leg.txt <- c(t1,t2,t3,t4,t5,t6)
  # print(leg.txt)
  y.leg <- c(4.5, 3, 2.1, 1.4, .7)
  cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)
  cols   <-c(2:(ncurv+1))
  
  j<-1
  for (i in seq(cexv)) {
    legend(0.01, y.leg[i]+0.33, leg.txt, pch = "___", col = cols, cex = 2*cexv[i])
    j<-j+1
  }
}

########plot a vector as a curve
plot_curve<-function(vector,title="PLOT",newplot=TRUE)
{
  if (newplot) plot(c(0,1),c(0,0), ylim=c(0,1), col="white",main=title)
  
  tam<-length(vector)
  for (i in 1:(tam))
  {
    lines(c((i-1)/(tam-1),(i)/(tam-1)),c(vector[i],vector[i+1]))
  }
  
}
################

ROC<-function(res,title=""){
  # 1st column is class has 0 and 1 only
  # 2nd colum is their scores
  ord<-order(res[,2],decreasing=T)
  score<-res[ord,2]
  class<-res[ord,1]
  temp1<-unique(score)
  n2<-length(temp1)
  n<-length(class)
  class0<-which(class==0)
  class1<-which(class==1)
  n1<-length(class1)
  n0<-length(class0)
  Sen<-rep(0,(n2+1)) #Sensitivity
  Spe<-rep(1,(n2+1)) #Specificity
  for (i in 1:n2){
    tmp1<-which(score>=temp1[i])
    tmp2<-setdiff(1:n,tmp1)
    Sen[(i+1)]<-length(intersect(tmp1,class1))/n1
    Spe[(i+1)]<-length(intersect(tmp2,class0))/n0
  }
  out<-data.frame(Sen=Sen,Spe=Spe)
  temp<-out
  plot(1-temp$Spe,temp$Sen,ylab="Sensitivity",xlab="1-Specificity",type="l",lwd=2,lty=1,main=title)
  lines(c(0,1),c(0,1))
  
}

################
##prints curves for complete noise (average)
plot_av_curves<-function(trop,teop,sd,costlines){
  tam<-length(trop)
  ncl<-length(costlines)/2
  dtrop<-c(1:ncl)
  dteop<-c(1:ncl)
  dsd<-c(1:ncl)
  drd<-c(1:ncl)
  
  #compute weights
  for (i in 1:ncl){
    dtrop[i]<-sum(trop==i)
    dteop[i]<-sum(teop==i)
    
    dsd[i]<-sum(sd==i)
    
  }

  dtrop<-dtrop/tam
  dteop<-dteop/tam
  dsd<-dsd/tam
  drd<-rep(1/(ncl-1),ncl)
  drd[1]<-drd[1]/2
  drd[ncl]<-drd[ncl]/2
  
  #weigthed average
  ltrop<-c(0,0)
  lteop<-c(0,0)
  lsd<-c(0,0)
  lrd<-c(0,0)
  k<-1
  #for each cost line we just compute extremes
  for (i in 1:ncl)
  {
    
    ltrop[1]<-costlines[k]*dtrop[i]+ltrop[1]
    ltrop[2]<-costlines[k+1]*dtrop[i]+ltrop[2]
    lteop[1]<-costlines[k]*dteop[i]+lteop[1]
    lteop[2]<-costlines[k+1]*dteop[i]+lteop[2]
    lsd[1]<-costlines[k]*dsd[i]+lsd[1]
    lsd[2]<-costlines[k+1]*dsd[i]+lsd[2]
    lrd[1]<-costlines[k]*drd[i]+lrd[1]
    lrd[2]<-costlines[k+1]*drd[i]+lrd[2]
    k<-k+2

  }
#  print(c(lteop[1],lteop[2]))
#  print(c(ltrop[1],ltrop[2]))
 #  print(c(lsd[1],lsd[2]))
#  print(c(lrd[1],lrd[2]))
  
  lines(c(0,1),c(lsd[1],lsd[2]),col=2,lty=2,lwd=2)
  lines(c(0,1),c(lteop[1],lteop[2]),col=3,lty=2,lwd=2)
  lines(c(0,1),c(lrd[1],lrd[2]),col=4,lty=2,lwd=2)
  lines(c(0,1),c(ltrop[1],ltrop[2]),col=5,lty=2,lwd=2)
  
}


################
##prints a calibration map
calmap<-function(inp,nbins=10){
  #funciona per a probs entre 0 i 1
  
  # sort
  x <- t(inp)
  zord <- order(x[2,])
  #zordrev <- rev(zord)  #Decreasing order
  zordrev <-zord
  screv <- x[,zordrev]
  inp<-screv
  inp <- t(screv)
  
  probs<-inp[,2]
  classes<-inp[,1]
    

  
  tam<-length(probs)
  points<-array(0.0, dim=c(nbins,2))
  sbin<-trunc(tam/nbins)
  for (i in 1:(nbins-1)){
 #   print((((i-1)*sbin)+1))
 #    print(i*sbin)
    points[i,1]=sum(probs[(((i-1)*sbin)+1):(i*sbin)])/(sbin)
    points[i,2]=sum(classes[(((i-1)*sbin)+1):(i*sbin)])/(sbin)
}
  #last segmen is bigger ( xapusa )
  i<-nbins
  points[i,1]=sum(probs[(((i-1)*sbin)+1):tam])/(tam -((i-1)*sbin))
  points[i,2]=sum(classes[(((i-1)*sbin)+1):tam])/(tam -((i-1)*sbin))
  #print((tam -((i-1)*sbin)))
  plot(points[,1],points[,2],ylab="p_act",xlab="p_est",type="l",lwd=2,lty=1, main="Calibration Map")
}
  
