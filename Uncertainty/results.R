library(xtable)
rm(list=ls()) #will remove ALL objects 

VARIANCE<-TRUE

tit<-"Results/new2017bresults_binomial_iter10_p0.5_nInf"
load(paste(tit,".Rdata",sep=""))
#tit<-"Results/table_binomial_iter10_p0.5_n0"
#title<-"new_1_mean"
title<-""

#datasets<-c("credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff","colic.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#nomclasses<-c("good","democrat","tested_negative","no-recurrence-events","yes","absent","DIE","won","b","+")
#nomcol<-c("class","Class","class","Class","surgical_lesion","class","Class","class","class","class")
#datasets<-c("credit-g.arff","vote.arff","diabetes.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","credit-a.arff")
#nomclasses<-c("good","democrat","tested_negative","absent","DIE","won","+")
#nomcol<-c("class","Class","class","class","Class","class","class")

methods<-c("J48","J48Unp","logist","NB","Stump","IBK","SMV","PART","Major","Random")
methods<-c("J48","J48Unp","logist","NB","Stump","IBK10","IBK1","SVM","SVMp","PART","Prior","Random")


#datasets<-c("breast-w.arff","spect_test.arff","cylinder-bands.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
selected<-2
datasets<-list.files("../data/")
#props<-c(1/4,3/4)#percentage for training

Numiter<-10
#pdf("plots.pdf")
numcol<-16

resultats<-dades
#resultats[,"trainoptimal"]<-resultats[,"trainoptimal"]-resultats[,"optimal"]
#resultats[,"scored"]<-resultats[,"scored"]-resultats[,"optimal"]
#resultats[,"rated"]<-resultats[,"rated"]-resultats[,"optimal"]

resu<- matrix (0,length(datasets)*length(methods),numcol-1)#iter,dataset,method,proportion,MSE,7methods
k<-1
#print("dataset,method,prop,MSE(inp),cscoredriven$area,coptimal$area,ctrainoptimal$area,cratedrive$area,cscoreratedrive$area,squared$area,rootsquared$area")
for (id in 1:length(datasets))
  for (im in 1:length(methods))   
  {
    filt<-subset(resultats,resultats[,2]==id&resultats[,3]==im)  
    
    #print(paste(datasets[id],methods[im],props[ip],ave(filt[1,5]),ave(filt[1,6]),ave(filt[1,7]),ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),ave(filt[1,12]),sep=","))
    resu[k,]<-c(id,im,ave(filt[,4])[1],ave(filt[,5])[1],ave(filt[,6])[1],ave(filt[,7])[1],ave(filt[,8])[1],ave(filt[,9])[1],ave(filt[,10])[1],ave(filt[,11])[1],ave(filt[,12])[1],ave(filt[,13])[1],ave(filt[,14])[1],ave(filt[,15])[1],ave(filt[,16])[1])
k<-k+1
  }
print("jj")


dres<-data.frame(dataset=datasets[resu[,1]],meth=methods[resu[,2]],AUC=resu[,3],MSE=resu[,4],cal=resu[,5],ref=resu[,6],opt=resu[,7],trainopt=resu[,8],scored=resu[,9],scoredz=resu[,10],rated=resu[,11],ratedz=resu[,12],scunif=resu[,13], ratunif=resu[,14],props=resu[,15])


ttable<-xtable(dres,digits=4)
sink("Results/tmp.tex")
print(ttable,size="\\tiny")
cat("\\end{document}")
sink()
tit1<-paste("Results/table_datasets",title,".tex",sep="")
com<-paste("cat Results/template.tex Results/tmp.tex >",tit1,sep="")
system(com)
com<-paste("pdflatex ",tit1,sep="")
system(com)



#agrupant props i métodes

resu<- matrix (0,length(datasets),numcol)#iter,dataset,method,proportion,MSE,7methods
k<-1
#print("dataset,method,prop,MSE(inp),cscoredriven$area,coptimal$area,ctrainoptimal$area,cratedrive$area,cscoreratedrive$area,squared$area,rootsquared$area")
for (id in 1:length(datasets))
{
  print(k)
  fres<-subset(resultats,resultats[,3]<11) #we remove random and majority
  filt<-subset(fres,fres[,2]==id)  
  #print(paste(datasets[id],methods[im],props[ip],ave(filt[1,5]),ave(filt[1,6]),ave(filt[1,7]),ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),ave(filt[1,12]),sep=","))
  
  resu[k,]<-c(id,ave(filt[,4])[1],ave(filt[,5])[1],ave(filt[,6])[1],ave(filt[,7])[1],ave(filt[,8])[1],ave(filt[,9])[1],ave(filt[,10])[1],ave(filt[,11])[1],ave(filt[,12])[1],ave(filt[,13])[1],ave(filt[,14])[1],ave(filt[,15])[1],ave(filt[,16])[1],sum(filt[,"trainoptimal"]>=filt[,"scored"])/length(filt[,"scored"]),sum(filt[,"trainoptimal"]>=filt[,"rated"])/length(filt[,"scored"]))
k<-k+1
}


#dres<-data.frame(dataset=datasets[resu[,1]],AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],trainopt=resu[,7],scored=resu[,8],scoredz=resu[,9],rated=resu[,10], ratedz=resu[,11],scunif=resu[,12],ratunif=resu[,13],props=resu[,14])
#dres<-data.frame(dataset=datasets[resu[,1]],AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],trainopt=resu[,7],scored=resu[,8],rated=resu[,10],tvss=resu[,15],tvcsr=resu[,16])
dres<-data.frame(AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],trainopt=resu[,7],scored=resu[,8],rated=resu[,10],scvtr=resu[,15],rdvtr=resu[,16])
ttable<-xtable(dres,digits=3)
sink("Results/tmp.tex")
print(ttable,size="\\footnotesize")
cat("\\end{document}")
sink()
tit1<-paste("Results/table3",title,".tex",sep="")
com<-paste("cat Results/template.tex Results/tmp.tex >",tit1,sep="")
system(com)
com<-paste("pdflatex ",tit1,sep="")
system(com)


#agrupant methods

resu<- matrix (0,length(methods),numcol)#iter,dataset,method,proportion,MSE,7methods, comptrainsco,compo train rated
if (VARIANCE== TRUE) resu<- matrix (0,length(methods),(numcol+4)) #iter,dataset,method,proportion,MSE,7methods, comptrainsco,compo train rated  4 variances
k<-1
#print("dataset,method,prop,MSE(inp),cscoredriven$area,coptimal$area,ctrainoptimal$area,cratedrive$area,cscoreratedrive$area,squared$area,rootsquared$area")
for (im in 1:length(methods))
{
  print(k)
  filt<-subset(resultats,resultats[,3]==im)  
  #print(paste(datasets[id],methods[im],props[ip],ave(filt[1,5]),ave(filt[1,6]),ave(filt[1,7]),ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),ave(filt[1,12]),sep=","))
  if (VARIANCE== FALSE)  resu[k,]<-c(im,ave(filt[,4])[1],ave(filt[,5])[1],ave(filt[,6])[1],ave(filt[,7])[1],ave(filt[,8])[1],ave(filt[,9])[1],ave(filt[,10])[1],ave(filt[,11])[1],ave(filt[,12])[1],ave(filt[,13])[1],ave(filt[,14])[1],ave(filt[,15])[1],ave(filt[,16])[1],sum(filt[,"trainoptimal"]>=filt[,"scored"])/length(filt[,"scored"]),sum(filt[,"trainoptimal"]>=filt[,"rated"])/length(filt[,"scored"]))
  if (VARIANCE== TRUE)  resu[k,]<-c(im,ave(filt[,4])[1],ave(filt[,5])[1],ave(filt[,6])[1],ave(filt[,7])[1],ave(filt[,8])[1],ave(filt[,9])[1],ave(filt[,10])[1],ave(filt[,11])[1],ave(filt[,12])[1],ave(filt[,13])[1],ave(filt[,14])[1],ave(filt[,15])[1],ave(filt[,16])[1],sum(filt[,"trainoptimal"]>=filt[,"scored"])/length(filt[,"scored"]),sum(filt[,"trainoptimal"]>=filt[,"rated"])/length(filt[,"scored"]),var(filt[,"optimal"]),var(filt[,"trainoptimal"]),var(filt[,"scored"]),var(filt[,"rated"]))
  
  k<-k+1
}

#dres<-data.frame(dataset=methods[resu[,1]],AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],trainopt=resu[,7],scored=resu[,8],scoredz=resu[,9],rated=resu[,10], ratedz=resu[,11],scunif=resu[,12],ratunif=resu[,13],props=resu[,14])
if (VARIANCE== FALSE) dres<-data.frame(dataset=methods[resu[,1]],AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],trainopt=resu[,7],scored=resu[,8],rated=resu[,10],scvtr=resu[,15],rdvtr=resu[,16])
if (VARIANCE== TRUE) dres<-data.frame(dataset=methods[resu[,1]],AUC=resu[,2],MSE=resu[,3],cal=resu[,4],ref=resu[,5],opt=resu[,6],varopt=resu[,17]*100,trainopt=resu[,7],vartrainopt=resu[,18]*100,scored=resu[,8],varscore=resu[,19]*100,rated=resu[,10],varrate=resu[,20]*100,scvtr=resu[,15],rdvtr=resu[,16])
ttable<-xtable(dres,digits=3)
sink("Results/tmp.tex")
print(ttable,size="\\footnotesize",include.rownames=FALSE)
cat("\\end{document}")
sink()
tit1<-paste("Results/tablemeth",title,".tex",sep="")
com<-paste("cat Results/template.tex Results/tmp.tex >",tit1,sep="")
system(com)
com<-paste("pdflatex ",tit1,sep="")
system(com)



titf<-paste("mv table_datasets.pdf ",tit,"_datasets.pdf",sep="")
system(titf)



titf<-paste("mv table3.pdf ",tit,"_summary.pdf",sep="")
system(titf)

titf<-paste("mv tablemeth.pdf ",tit,"_methods.pdf",sep="")
system(titf)


#write.csv(file=paste("Results/",tit,".csv",sep=""), x=resultats)
#write.csv(file="Results/tmp.csv", x=resultats)
#system(paste("cat Results/header.txt Results/tmp.csv>Results/",tit,".csv",sep=""))


#system("mv *pdf Results")
system("mv *log Results")
system("mv *aux Results")
