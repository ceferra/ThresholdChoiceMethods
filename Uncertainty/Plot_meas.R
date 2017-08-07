
CORRECTINGSD<-FALSE

library(dplyr)




tit<-"Results/new2017bresults_binomial_iter10_p0.5_n0"
#file<-paste(tit,".pdf",sep="")
file<-"explots/"
load(paste(tit,".Rdata",sep=""))

#datasets<-c("breast-w.arff","spect_test.arff","cylinder-bands.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
datasets<-list.files("../data/")
#datasets<c("credit-a.arff")
methods<-c("J48","J48Unp","Logist","NB","Stump","IBK10","IBK1","SVM","SVMp","PART","Prior","Random")

#methodssort<-sort(methods)
methodssort<-c(sort(methods[1:10]),"Prior","Random")


mcolores<-c('deepskyblue2', 'darkolivegreen3', 'yellow2','orange','red',"green","pink","brown","violetred","lightskyblue2","black","grey")
colores<-c(1:length(datasets))
#dades<-read.csv(file)


if (CORRECTINGSD==TRUE)
{
  dades[,"rated"]<-1/3+(dades[,"proptrain"]*(1-dades[,"proptrain"]) *(1-(2*dades[,"AUC"] )))
}

ylimi<-0.6
xlimi<-0.6
#dades<-subset(dades, Dataset==12) #filtering by dataset
#dades<-subset(dades, Dataset==2 & Iteration==1)

plotcal <- function(ny="trainoptimal",nx="scored",nz="cal",yt="Scored-Trainopt",xt="calloss",nd=0) {
  
  nmd="All"
  dadesn<-dades
  if (nd==-1)
  {
    kk<-group_by(dades, Dataset,Method)
    resum<-summarise(kk,count=n(),MSE = mean(MSE, na.rm = TRUE),cal = mean(cal, na.rm = TRUE),trainoptimal = mean(trainoptimal, na.rm = TRUE),scored = mean(scored, na.rm = TRUE),rated = mean(rated, na.rm = TRUE))
    dadesn<-data.frame(resum["Dataset"],resum["Method"],resum["MSE"],resum["cal"],resum["trainoptimal"],resum["scored"],resum["rated"])
    nmd<-"datasets"
    
  }
  
  if(nd>0)
  {
    dadesn<-subset(dades, Dataset==nd) #filtering by dataset
    nmd=datasets[nd]
  }
  
  for (i in (1:length(methodssort)))
  {
    #paste(nmd,file)
    ind<-which(methods==methodssort[i])
    datt<-subset(dadesn, Method==ind) #infinity (para poner en el caption)
    if (i==1) plot( datt[,nz],(datt[,nx]-datt[,ny]), main=bquote(gamma ==0  ), xlim=c(0,0.5), ylim=c(-0.25,0.25),xlab=xt, ylab=yt, pch=20, col=mcolores[ind],cex=0.3)
    else points(datt[,nz],(datt[,nx]-datt[,ny]), pch=20, col=mcolores[i],cex=0.3)
   # mx = mean(datt[,nx])
  #  lines(c(mx,mx),c(0,ylimi),col=colores[i])
  #  my = mean(datt[,ny])
  #  lines(c(0,xlimi),c(my,my),col=colores[i])
  }
  
  
  #lines(c(0,xlimi),c(0,ylimi),col="black")
  
  legend('topright', methods , pch=19, col=mcolores,cex=.75)
  
}



plotmeas <- function(ny="trainoptimal",nx="scored",yt="Atrainopt",xt="Ascored",nd=0, linesON=TRUE) {
  
  nmd=""
  dadesn<-dades
  if(nd>0)
  {
    dadesn<-subset(dades, Dataset==nd) #filtering by dataset
    nmd=datasets[nd]
  }
  
  for (i in (1:length(methodssort)))
  {
    ind<-which(methods==methodssort[i])
    
    datt<-subset(dadesn, Method==ind)
    #main=paste(nmd,file)
    if (i==1) plot(datt[,nx], datt[,ny], main=expression(gamma == 0), xlim=c(0,xlimi), ylim=c(0,ylimi),xlab=xt, ylab=yt, pch=20, col=mcolores[i],cex=.5)
    else points(datt[,nx], datt[,ny], pch=20, col=mcolores[i],cex=.5)
    mx = mean(datt[,nx])
    if (linesON) lines(c(mx,mx),c(0,ylimi),col=mcolores[i])
    my = mean(datt[,ny])
    if (linesON) lines(c(0,xlimi),c(my,my),col=mcolores[i])
  }
  
  
 lines(c(0,xlimi),c(0,ylimi),col="black")
  
  legend('topright', methodssort , pch=19, col=mcolores,cex=.75)
  
}

## plot a figure for each method, to vs sc, to vs rd, sc vs rd
plotmod <- function(ny="trainoptimal",nx="scored",yt="Atrainopt",xt="Ascored",nd=0) {
  
  legtdata<-datasets
  
  nmd=""
  dadesn<-dades
  if(nd>0)
  {
    dadesn<-subset(dades, Method==nd) #filtering by method
    nmd=methods[nd]
  }
  datt<-dadesn
  
  
  for (i in (1:length(datasets)))
  {
  
    dattn<-subset(datt, Dataset==i) #filtering by method
    if (i==1) 
      {
      plot(dattn[,nx], dattn[,ny], main=paste(nmd,file), xlim=c(0,xlimi), ylim=c(0,ylimi),xlab=xt, ylab=yt, pch=20, col=colores[i])
      #rect(min(dattn[,nx]),min(dattn[,ny]),max(dattn[,nx]),max(dattn[,ny]),border=colores[i])
      mx = mean(dattn[,nx])
      sx<-sd(dattn[,nx])
      lines(c(mx,mx),c(0,ylimi),col=colores[i])
      my = mean(dattn[,ny])
      sy<-sd(dattn[,ny])
      rect(mx-sx,my-sy,mx+sx,my+sy,border=colores[i])
      lines(c(0,xlimi),c(my,my),col=colores[i])
      legtdata[i]<-paste(legtdata[i],format(mx,digits=3),"·",format(sx,digits=3),"|",format(my,digits=3),"·",format(sy,digits=3))
    
    }
    else {
      points(dattn[,nx], dattn[,ny], pch=20, col=colores[i])
     # print(dattn)
     mx = mean(dattn[,nx])
     sx<-sd(dattn[,nx])
     lines(c(mx,mx),c(0,ylimi),col=colores[i])
     my = mean(dattn[,ny])
     sy<-sd(dattn[,ny])
     rect(mx-sx,my-sy,mx+sx,my+sy,border=colores[i])
     lines(c(0,xlimi),c(my,my),col=colores[i])
    legtdata[i]<-paste(legtdata[i],format(mx,digits=3),"·",format(sx,digits=3),"|",format(my,digits=3),"·",format(sy,digits=3))
    }
  }
  lines(c(0,xlimi),c(0,ylimi),col="black")
  
  
  legtdata<-datasets
  legend('topright', legtdata , pch=19, col=colores,cex=.75)
  
  
}

pdf(paste(file,"_MSE.pdf",sep=""))
plotcal(nd=0,nz="MSE",xt="MSE")### All points
plotcal(nd=-1,nz="MSE",xt="MSE")### one point per dataset and method
for (i in (1:length(datasets)))
{
  plotcal(nd=i,nz="MSE",xt="MSE")
}
dev.off()


pdf(paste(file,"_cal.pdf",sep=""))
plotcal(nd=0)### All points
#plotcal(nd=-1)### one point per dataset and method
for (i in (1:length(datasets)))
{
#plotcal(nd=i)
}
dev.off()

pdf(paste(file,"_methods.pdf",sep=""))
#plotmeas("scored","trainoptimal","Scored","Trainoptimal")
#plotmeas("ratedz","trainoptimal","Aratedz","Atrainopt")
#plotmeas("ratedz","scored","Aratedz","Ascored")
for (i in (1:length(datasets)))
{
 if (i==6) plotmeas("scored","trainoptimal","Scored","Trainoptimal",i,linesON=TRUE)
# # plotmeas("ratedz","trainoptimal","Aratedz","Atrainopt",i)
#  plotmeas("ratedz","scored","Aratedz","Ascored",i)
}
dev.off()
pdf(paste(file,"_datasets.pdf",sep=""))
for (i in (1:length(methods))) 
  {
  plotmod("scored","trainoptimal","Ascored","Atrainopt",i)
 # plotmod("ratedz","trainoptimal","Aratedz","Atrainopt",i)
#  plotmod("ratedz","scored","Aratedz","Ascored",i)
}
dev.off()
