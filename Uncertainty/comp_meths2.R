library(dplyr)

#### Para cada método muestra las 4 THCM dependiendo del ruido
#setwd("/home/cesar/Dropbox/Thresholds/Nou")

levelsn<-c("Inf","16","10","9","8","7","6","5","4","3","2","1","0")
levelsn<-c("Inf","16","8","4","2","1","0")

#levelsn<-c("Inf")

files<-paste("Results/new2017bresults_binomial_iter10_p0.5_n",levelsn,sep="")
datasets<-list.files("../data/")
nfiles<-length(files)

methods<-c("J48","J48Unp","logist","NB","Stump","IBK","IBK1","SVM","SVMp","PART","Major","Random")
thrmethods<-c("scoredriven","testoptimal","ratedriven","trainoptimal")
mcolores<-c(2, 3, 4,5)
colores<-c(1:length(datasets))

sel<-4 #method selected

ldades<-list()

for (i in 1:nfiles) 
  {
  load(paste(files[i],".Rdata",sep=""))
  dades<-subset(dades, Dataset==6) #filtering by dataset
  dades<-subset(dades, Method==sel) #filtering by dataset
  ldades[[i]]<-dades
}


dades<-data.frame(Methods=c(1:length(methods)))
dadesntr<-c(1:nfiles)
dadesnsd<-c(1:nfiles)
dadesnrd<-c(1:nfiles)
dadesnte<-c(1:nfiles)
#dadesnrd<-data.frame(Methods=c(1:length(methods)))
for (i in 1:nfiles)
{
  dat<-ldades[[i]]
  kk<-group_by(dat, Method)
  resum<-summarise(kk,count=n(),MSE = mean(MSE, na.rm = TRUE),cal = mean(cal, na.rm = TRUE),optimal = mean(optimal, na.rm = TRUE),trainoptimal = mean(trainoptimal, na.rm = TRUE),scored = mean(scored, na.rm = TRUE),rated = mean(rated, na.rm = TRUE))
  
   dadesntr[i]<-resum["trainoptimal"]
   dadesnte[i]<-resum["optimal"]
   dadesnrd[i]<-resum["rated"]
   dadesnsd[i]<-resum["scored"]
  #names(dadesn[i+1])<-levelsn[i]
}

tit<-paste("explots/curves_",methods[sel],".pdf",sep="")
pdf(tit)
dades<-data.frame(levels=levelsn,scored=unlist(dadesnsd),rated=unlist(dadesnrd),optimal=unlist(dadesnte),trainoptimal=unlist(dadesntr))

#plot(dades["scored"] ,ylim=c(0.0,0.4),xlab="Level of Certainty", ylab="loss",col=mcolores[1],xaxt="n")
#plot(dades["rated"])
plot(unlist(dades["scored"]) ,ylim=c(0.0,0.4),xlab="Level of Certainty", ylab="loss",col=mcolores[1],xaxt="n", main=methods[sel])
axis(1, at=1:nfiles, labels=levelsn)

points(unlist(dades["scored"]),col=mcolores[1])
lines(unlist(dades["scored"]),col=mcolores[1])

points(unlist(dades["optimal"]),col=mcolores[2])
lines(unlist(dades["optimal"]),col=mcolores[2])

points(unlist(dades["rated"]),col=mcolores[3])
lines(unlist(dades["rated"]),col=mcolores[3])




points(unlist(dades["trainoptimal"]),col=mcolores[4])
lines(unlist(dades["trainoptimal"]),col=mcolores[4])

legend('topleft', thrmethods[1:4] , pch=19, col=mcolores,cex=.75)
dev.off()
