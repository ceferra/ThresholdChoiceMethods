########### per a vore un exemple
## Script to plot differnt levels of noise in the same plot. (valphabeta) # we select between RD and SD in lines 424-428
#setwd("/home/cesar/Dropbox/Thresholds/Nou")
library("RWeka")
library(caret)

PLOTS<-TRUE
source("options.R")
source("Threshold_methods.R")
source("cost_functions.R")
# La classe 0 es la minoritaria
# i la 1 la majoritaria


RDPLOT<-FALSE # To select the TCM to plot
SDPLOT<-TRUE
TRPLOT<-FALSE
TEPLOT<-FALSE

source("plots.R")
km<-RESOLUTION_EMPIRICAL_CURVES 
km<-200


#require("sampling")

args <- commandArgs(trailingOnly = TRUE)
###############################################
###############################################
############# OPERATING OPTIONS ###############
###############################################

set.seed(0)
valphabeta<-c(0,1,2,4,6,8,10,Inf)
valphabeta<-c(0,1,2,4,8,16,Inf)
#valphabeta<-c(0,1)
mcolors<-c("mistyrose2", "lightpink","violet","purple","mediumpurple3","mediumpurple4","black")

Numiter<-1
Numrep<-10
prop<-0.5#percentage for training


bfactor <- 2  # 1 is like Hand, and Drummond and Holte, but 2 makes more sense, since it places the maximum expected loss at 1 (and not 0.5 for a balanced dataset)
m<-1

##########
########
#######     MAIN
#####
########

#pdf("kk.pdf")
destdir<-"explots/"

#datasets<-c("spambase.arff","sick.arff","labor.arff","colic.arff","breast-w.arff","spect_test.arff","cylinder-bands.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")


#datasets<-c("spect_test.arff")

#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("spect_test.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff")
#datasets<-c("tic-tac-toe.arff")
#selected<-2
datasets<-list.files("../data/")
datasets<-c("tic-tac-toe.arff")
datasets<-c("monks1.arff")
datasets<-c("credit-a.arff")
#datasets<-c("spect_test.arff")
#datasets<-c("monks1.arff")

methods<-c("J48","J48Unp","logist","NB","Stump","IBK","IBK1","SMV","SMVp","PART","Majority","Random")
methods<-c("Majority")
#methods<-c("Random")
#methods<-c("J48Unp")

#pdf(paste(methods[1],"_",datasets[1],".pdf",sep=""))

#pdf("peterexample.pdf")
numcol<-16
nomscol<-c("iter","Dataset","Method","AUC","MSE","cal","ref","optimal","trainoptimal","scored","scoredz","rated","ratedz","scdun","rtun","proptrain")

resultats<- matrix (0,Numiter*length(datasets)*length(methods),numcol)#iter,dataset,method,proportion,MSE,7methods
listc<-list()

moda<-TRUE

k<-1
bal<-1 # 1 balanced, >1 classe majoritaria es fa més majoritaria al train
ktot<-Numiter*length(datasets)*length(methods)

#for (id in 1:length(datasets))
for (id in 1:length(datasets))
{
  selected <- id
  #selected<-2
  msel<-1
  #prop<-1/4
  datos <- read.arff(paste("../data/",datasets[selected],sep=""))
  
  # Asumimos que el estudio se hace en base al ultimo campo
  posParamEstudio<-length(datos[1,])
  nomParamEstudio<-names(datos)[posParamEstudio]
  
  
  
  # nomclasse<-names(datos)[length(names(datos))]
  tam<-length(datos[,1])
  # print(tam)
  #mnom<-nomclasses[id]
  
  
  #mnon ha de ser la minoritaria
  nomcl<-unique(datos[nomParamEstudio])
  if(sum(datos[,nomParamEstudio]==nomcl[[1]][1])>(tam/2)) mnom=nomcl[[1]][1]
  else mnom=nomcl[[1]][2]
  
  
  #mnom<-datos[1,posParamEstudio]
  #  print("ss")
  # print(mnom)
  for (iter in 1:Numiter)
  {
    #rank the dataset
    w<-datos[sample(nrow(datos)),]
    # Separamos por 50 train 50 test
    indiceStrat<-createDataPartition(w[,posParamEstudio], p = prop,list=FALSE)
    train<-w[indiceStrat,]
    test<-w[-indiceStrat,]
    if (methods[msel]=="Majority") test<-w # we do not train/test here
    if (methods[msel]=="Random") test<-w
    
    #  train<-w[resd[,2],]
    print("data")
    print(tam)
    #  print(propdata)
    print("train")
    print(length(train[,1]))
    print("test")
    print(length(test[,1]))
    proptrain<-sum(train[,nomParamEstudio]==mnom)/length(train[,posParamEstudio])
    proptrain<-1-proptrain
    print("proptain")
    print(proptrain)
    print("proptest")
    proptest<-sum(test[,nomParamEstudio]==mnom)/length(test[,posParamEstudio])
    proptest<-1-proptest
    print(proptest)
    
    ob<-paste(nomParamEstudio,"~.",sep="")
    for (im in 1:length(methods))
    {
      msel<-im
      print(paste(k,"/",ktot))
      print(paste(datasets[selected],prop,methods[msel],iter))
      
      #m <- J48(class~., data = train, control = Weka_control(U = TRUE))
      if (methods[msel]=="J48") model <- J48(ob, data = train)
      if (methods[msel]=="J48Unp") model <- J48(ob, data = train, control = Weka_control(U = TRUE,A=TRUE))
      if (methods[msel] =="logist" ) model <- Logistic(ob, data = train)
      
      if (methods[msel] == "NB")
      {
        NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
        model<- NB(ob, data = train)
        
      }
      
      if (methods[msel] == "Stump") model<- DecisionStump(ob, data = train)
      
      
      if (methods[msel] == "IBK") model<- IBk(ob, data = train,control = Weka_control(K=10))
      
      if (methods[msel] == "IBK1") model<- IBk(ob, data = train,control = Weka_control(K=1))
      
      if (methods[msel] == "SMV") model<- SMO(ob, data = train)
      
      if (methods[msel] == "SMVp") model<- SMO(ob, data = train,control = Weka_control(M=TRUE))
      if (methods[msel] == "PART") model<- PART(ob, data = train)
      
      
      
      probstrain<-matrix(0,length(train[,1]),1)
      
      if (methods[msel]=="Majority") probstrain<-rep((1-proptrain+0.001),length(train[,1]))## majority
      else if (methods[msel]=="Random") probstrain<-runif(length(train[,1]))##random
      else  
      {
        allprobstrain<-predict(model, newdata = train,type=c("probability"))
        probstrain<-allprobstrain[,mnom]
      }
      #print(probstrain)
      cltrain<-c()
      for (i in 1:length(train[,1]))
      {
        if(train[i,nomParamEstudio]==mnom)  {cltrain[i]<-1 }
        else cltrain[i]<-0
      }
      
      #inptrain<-cbind(cltrain,probstrain[,1])
      inptrain<-cbind(cltrain,probstrain)
      #sort prbs train (for the otrain optimal)
      x <- t(inptrain)
      zord <- order(x[2,])
      zordrev <- rev(zord)
      screv <- x[,zordrev]
      inptrain <- t(screv) #Decreasing order
      
      
      probs<-matrix(0,length(test[,1]),1)
      
      if (methods[msel]=="Majority") probs<-rep((1-proptrain+0.001),length(test[,1]))## majority
      else if (methods[msel]=="Random") probs<-runif(length(test[,1]))##random
      else
      {
        allprobs<-predict(model, newdata = test,type=c("probability"))
        probs<-allprobs[,mnom]
      }
      #print(probs)
      #### Classes
      cl<-c()
      for (i in 1:length(test[,1]))
      {
        if(test[i,posParamEstudio]==mnom)
        {cl[i]<-1 }
        else cl[i]<-0 
        # if (probs[i,1]==0)probs[i,1]<-0.0000001
      }
      
      #
      inp<-cbind(cl,probs)
      
      
      #        #PETER
      #sort test probs
      x <- t(inp)
      zord <- order(x[2,])
      zordrev <- rev(zord)
      screv <- x[,zordrev]
      inp <- t(screv) #Decreasing order
      
      #    inp <- cbind(c(1,1,1,0),c(0.2,0.2,0.2,0.2))
      #    #       inp <- cbind(c(1,1,0,1,1,0,0,1,1,0,0,0,0,0),c(0.9,0.9,0.9,0.9,0.9,0.8,0.8,0.4,0.4,0.2,0.14,0.14,0.14,0.14)) 
      #    inptrain<-inp
      #    #  inptrain<-cbind(c(1,1,0,0),c(0.85,0.65,0.45,0.25))
      #    proptest<-sum(inp[,1]==1)/length(inp[,1])
      #    proptest<-1-proptest
      #    prottrain<-proptest
      
      
      ###cost lines
      resul<-ExpLossC(inp,ProbThresFun) # Brier score (probabilistic threshold)
      dd<-LossCurveC(inp,ProbThresFun)
      #  cscoredriven<-list("points"=dd,"area"=resul)
      
   if(RDPLOT==TRUE)   pdf(paste(destdir,methods[im],"trainCLrd_",datasets[selected],".pdf",sep=""))
   if(SDPLOT==TRUE) pdf(paste(destdir,methods[im],"trainCLsd_",datasets[selected],".pdf",sep=""))
    if(TRPLOT==TRUE) pdf(paste(destdir,methods[im],"trainCLtr_",datasets[selected],".pdf",sep=""))
    if(TEPLOT==TRUE) pdf(paste(destdir,methods[im],"trainCLte_",datasets[selected],".pdf",sep=""))
      
      ##calculem linies de cost i area
      xrtrain<-MinCost(inptrain,title=methods[im])
      dev.off()
      
      if(RDPLOT==TRUE)   pdf(paste(destdir,methods[im],"testCLrd_",datasets[selected],".pdf",sep=""))
      if(SDPLOT==TRUE) pdf(paste(destdir,methods[im],"testCLsd_",datasets[selected],".pdf",sep=""))
      if(TRPLOT==TRUE) pdf(paste(destdir,methods[im],"testCLtr_",datasets[selected],".pdf",sep=""))
      if(TEPLOT==TRUE) pdf(paste(destdir,methods[im],"testCLte_",datasets[selected],".pdf",sep=""))
      ##calculem linies de cost i area
      #xr<-MinCost(inp,title=bquote(J48~gamma == .(alphabeta)))
      #xr<-MinCost(inp)
      xr<-CostLines(inp,title="")
      #print(xr$res)
      
      
      
      
      for (na in 1:length(valphabeta))    
      {
        alphabeta<-valphabeta[na]
        
        opta<-0
        trainopt<-0
        scoa<-0
        scda<-0
        vcreal<-c()
        vchat<-c()
        
        curvoptimal<-list()
        curvoptimal$points<-rep(0,km)
        curvoptimal$selth<-c()
        
        curvtrainoptimal<-list()
        curvtrainoptimal$points<-rep(0,km)
        curvtrainoptimal$selth<-c()
        
        
        curvscored<-list()
        curvscored$points<-rep(0,km)
        curvscored$selth<-c()
        
        
        curvrated<-list()
        curvrated$points<-rep(0,km)
        
        
        for (nr in 1:Numrep)    
        {
          print(nr)
          for (it in 1:km) 
          {
            creal<-it/km
            
            zreal<-creal*proptrain/(creal*proptrain+((1-creal)*(1-proptrain)))
            #  zreal<-creal*proptest/(creal*proptest+((1-creal)*(1-proptest)))
            # print(zreal)
            
            if (alphabeta<Inf) { 
              if (alphabeta==0)
              {
                chat<-runif(1,0,1)
                zhat<-chat*proptrain/(chat*proptrain+((1-chat)*(1-proptrain)))
                
              }
              else
              {
                ## Beta case
                alpha <- alphabeta*creal + 1
                beta <- alphabeta-alpha+2
                chat<-rbeta(1,alpha,beta)
                
                zhat<-chat*proptrain/(chat*proptrain+((1-chat)*(1-proptrain)))
                
                ### Binomial case
                #   nz<- rbinom(1,alphabeta,zreal)
                #    zhat <- (nz+(m*proptrain))/(alphabeta+m)
                #    chat<-zhat*(proptrain-1)/(2*proptrain*zhat-zhat-proptrain)
              }
            }
            else {
              chat<-creal
              zhat<-zreal
            }
            
            
            #print(chat)
            
            #zhat<- rbinom(1,alphabeta,zreal)/alphabeta
            #chat<-zhat*(proptrain-1)/(2*proptrain*zhat-zhat-proptrain)
            
            vcreal[it]<-creal
            vchat[it]<-chat
            
            
            ####test_optimal_test 
            thredsh<-ThresMin(inp,chat,xr$yf)
            
            
            #print(thredsh)
            coptimal<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
            #print(coptimal)
            opta<-opta+coptimal
            
            curvoptimal$title<-"optimal"
            curvoptimal$points[it]<-coptimal+curvoptimal$points[it]
            curvoptimal$selth[it]<-whichLC(xr$yf,coptimal,creal)
            
            #plot_curve(gg$points,tit)
            ####train_optimal_test 
            
            thredsh<-ThresMin(inptrain,chat,xrtrain$yf)
            
            # print(thredsh)
            #print("train_optimal_test ")
            ctrainoptimal<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
            #print(ctrainoptimal)
            trainopt<-trainopt+ctrainoptimal        
            curvtrainoptimal$title<-"trainoptimal"
            curvtrainoptimal$points[it]<-ctrainoptimal+curvtrainoptimal$points[it]
            curvtrainoptimal$selth[it]<-whichLC(xr$yf,ctrainoptimal,creal)
            
            #tit<-paste("train_optimal_test",gg$area)
            #plot_curve(gg$points,tit)
            
            ####Rate_driven
            # 
            
            allt<-RateDrivenThresFunCdisROCCOST_empat(inp,chat)
            #  print("chat")
            #  print(chat)
            #print(allt)
            thredsh1<-allt[1]
            thredsh2<-allt[2]
            pesos<-allt[3]
            # }
            #thredsh1<-1
            #thredsh2<-1
            # cratedrive<-comp_cost_single_rd(inp[,1],inp[,2],thredsh1,thredsh2,pesos,creal)
            cratedrive<-pesos*comp_cost_single(inp[,1],inp[,2],thredsh1,creal)+(1-pesos)*comp_cost_single(inp[,1],inp[,2],thredsh2,creal)
            # print(cratedrive)
            # cratedrive$title<-"ratedriven"
            scda<-scda+cratedrive
            
            curvrated$title<-"ratedriven"
            curvrated$points[it]<-cratedrive+ curvrated$points[it]
            
            
            ####score_driven_test
            
            thredsh<-ScoreDrivenThres(inp,chat)
            # print(thredsh)
            cscoredriven<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
            
            scoa<-scoa+  cscoredriven
            
            curvscored$title<-"scoredriven"
            curvscored$points[it]<-cscoredriven+ curvscored$points[it]
            curvscored$selth[it]<-whichLC(xr$yf,cscoredriven,creal)
            
            #    print("scored")
            #    print(thredsh)
            #  print(cscoredriven)
            
          }
          
        }
        #curves
        curvoptimal$area<-opta/(km*Numrep)
        curvtrainoptimal$area<-trainopt/(km*Numrep)
        curvscored$area<-scoa/(km*Numrep)
        curvrated$area<-scda/(km*Numrep)
        
        curvtrainoptimal$points<-curvtrainoptimal$points/Numrep
        curvoptimal$points<-curvoptimal$points/Numrep
        curvscored$points<-curvscored$points/Numrep
        curvrated$points<-curvrated$points/Numrep
        
        k<-k+1
        
        tit<-methods[im] 
        if (tit=="Majority") tit<-"Prior Model"
        
        # tit=""
        #plot_curves_testl(list(curvscored,curvoptimal,curvrated,curvtrainoptimal),title=tit,newplot=FALSE)
        #  plot_curves_testl(list(curvoptimal),newplot=FALSE)
        if(SDPLOT==TRUE) 
        {
          curvscored$title<-alphabeta  ### Aci si volem scoredriven
         listc[[na]]<-curvscored
         tit<-paste(tit, " - Score Driven")
        }
        
        if(RDPLOT==TRUE) 
        {
        curvrated$title<-alphabeta ### Aci si volem ratedriven
        listc[[na]]<-curvrated
        tit<-paste(tit, " - Rate Driven")
        }
        
        if(TRPLOT==TRUE) 
        {
          curvtrainoptimal$title<-alphabeta ### Aci si volem ratedriven
          listc[[na]]<-curvtrainoptimal
          tit<-paste(tit, " - Train Optimal")
        }
        
        if(TEPLOT==TRUE) 
        {
          curvoptimal$title<-alphabeta ### Aci si volem ratedriven
          listc[[na]]<-curvoptimal
          tit<-paste(tit, " - Test Optimal")
        }
        
        #print(asd)
        
      }
      if(RDPLOT==TRUE)  plot_av_curves(inp,curvscored$selth,curvscored$selth,curvscored$selth,xr$yf,PLOTRD=TRUE,PLOTSD=FALSE,PLOTTR=FALSE,PLOTTE=FALSE) #xapuseta per a no modificar funcio
      if(SDPLOT==TRUE) plot_av_curves(inp,curvscored$selth,curvscored$selth,curvscored$selth,xr$yf,PLOTRD=FALSE,PLOTSD=TRUE,PLOTTR=FALSE,PLOTTE=FALSE) #xapuseta per a no modificar funcio
      if(TRPLOT==TRUE) plot_av_curves(inp,curvscored$selth,curvscored$selth,curvscored$selth,xr$yf,PLOTRD=FALSE,PLOTSD=FALSE,PLOTTR=TRUE,PLOTTE=FALSE) #xapuseta per a no modificar funcio
      if(TEPLOT==TRUE) plot_av_curves(inp,curvscored$selth,curvscored$selth,curvscored$selth,xr$yf,PLOTRD=FALSE,PLOTSD=FALSE,PLOTTR=FALSE,PLOTTE=TRUE) #xapuseta per a no modificar funcio
      
      plot_curves_testl(listc,title=tit,newplot=FALSE,colors=mcolors)
      
      dev.off()
    }
  }
}

dades<-data.frame(resultats)
names(dades)<-nomscol

#save(dades,file=titfile)

print("dataset,method,MSE(inp),coptimal$area,ctrainoptimal$area,cscoredriven$area,cratedrive$area,RateDrivenThresFunCdisROCCOST$area,squared$area,rootsquared$area,scoreunif$area,rateunif$area")
for (id in 1:length(datasets))
  for (im in 1:length(methods))
    
  {
    filt<-subset(resultats,resultats[,2]==id&resultats[,3]==im)  
    
    #print(paste(datasets[id],methods[im],ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),sep=","))
    
  }

#dev.off()

