########### per a vore un exemple
# V2 21/7/14
####incorporamos nuevos modelos y la prop
### modificamos el metodo para hacer train /test
# V3  24/7 /14
# vamos a eliminar la chapuza de poner nombre a las clases.
#verificar todo el código
# v6 parametres command

library("RWeka")
library(caret)
library("Iso") 

PLOTS<-TRUE
source("options.R")
source("Threshold_methods.R")
source("cost_functions.R")
# La classe 0 es la minoritaria
# i la 1 la majoritaria



source("plots.R")
km<-RESOLUTION_EMPIRICAL_CURVES*10
#km<-100


#require("sampling")

args <- commandArgs(trailingOnly = TRUE)
###############################################
###############################################
############# OPERATING OPTIONS ###############
###############################################

set.seed(0)
alphabeta<-Inf
Numiter<-1

prop<-0.5#percentage for training

if (length(args)==3)
{
  
  if(args[1]=="Inf") alphabeta<-Inf
  else alphabeta<-as.integer(args[1])
  Numiter<-as.integer(args[2])
  prop<-as.double(args[3])#percentage for training
}

print(alphabeta)
print(Numiter)
print(prop)

bfactor <- 2  # 1 is like Hand, and Drummond and Holte, but 2 makes more sense, since it places the maximum expected loss at 1 (and not 0.5 for a balanced dataset)
m<-1

titfile<-paste("Results/newresults_binomial_iter",Numiter,"_p",prop,"_n",alphabeta,".Rdata")
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
datasets<-c("breast-w.arff")
datasets<-c("credit-a.arff")

methods<-c("J48","J48Unp","logist","NB","Stump","IBK10","IBK1","SVM","SVMp","PART","Majority","Random")
methods<-c("SVM")
#methods<-c("Random")

#pdf(paste(methods[1],"_",datasets[1],".pdf",sep=""))

#pdf("peterexample.pdf")
numcol<-16
nomscol<-c("iter","Dataset","Method","AUC","MSE","cal","ref","optimal","trainoptimal","scored","scoredz","rated","ratedz","scdun","rtun","proptrain")

resultats<- matrix (0,Numiter*length(datasets)*length(methods),numcol)#iter,dataset,method,proportion,MSE,7methods

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
    #test<-w
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
    #for (im in 1:length(methods))
    for (im in 1:1)
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
      
      
      if (methods[msel] == "IBK10") model<- IBk(ob, data = train,control = Weka_control(K=10))
      
      if (methods[msel] == "IBK1") model<- IBk(ob, data = train,control = Weka_control(K=1))
      
      if (methods[msel] == "SVM") model<- SMO(ob, data = train)
      
      if (methods[msel] == "SVMp") model<- SMO(ob, data = train,control = Weka_control(M=TRUE))
      if (methods[msel] == "PART") model<- PART(ob, data = train)
      
      
      
      probstrain<-matrix(0,length(train[,1]),1)
      
      if (methods[msel]=="Majority") probstrain<-rep((1-proptrain+0.001),length(train[,1]))## majority
      else if (methods[msel]=="Random") probstrain<-runif(length(train[,1]))##random
      else if (methods[msel]=="IBK1") ### IBK1 in Rweka computes probs in a strange way, we modify it
        ## we predict the  class,and then assign 1 if 
      {
        allprobstrain<-predict(model, newdata = train,type=c("probability"))
        probstrain<-unlist(lapply(allprobstrain[,mnom],function(x) rbinom(1, 1, x) ))
        
      }
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

      inptrain<-sort_inp(inptrain)
      
      
      probs<-matrix(0,length(test[,1]),1)
      
      if (methods[msel]=="Majority") probs<-rep((1-proptrain+0.001),length(test[,1]))## majority
      else if (methods[msel]=="Random") probs<-runif(length(test[,1]))##random
      else if (methods[msel]=="IBK1") ### IBK1 in Rweka computes probs in a strange way, we modify it
        ## we predict the  class,and then assign 1 if 
      {
        allprobs<-predict(model, newdata = test,type=c("probability"))
        probs<-unlist(lapply(allprobs[,mnom],function(x) rbinom(1, 1, x) ))
      }
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
      inp<-sort_inp(inp)
      #       #PETER
      #        minp<-read.csv("scores.txt")
      #        ninp<-minp
      #        ninp[,1]<-1-minp[,2]
      #        ninp[,2]<-minp[,1]
      #        inp<-ninp[order(ninp[,2],decreasing=TRUE),]
      #        proptest<-sum(inp[,1]==0)/length(inp[,1])
      #         proptrain<-proptest
      # #        #PETER
      #       ff<-rep(0.36,6)
      #       inp <- cbind(c(1,1,1,0,0,0),ff)
     #inp <- cbind(c(1,0,1,0,1,0,0,1),c(0.74,0.74,0.74,0.74,0.74,0.44,0.3,0.3))
     #inp <- cbind(c(1,0,1,1,0),c(0.65,0.65,0.65,0.45,0.45))
  #   inp <- cbind(c(1,1,1,1,1,0),c(0.8,0.8,0.8,0.8,0.8,0.8))
       #inp <- cbind(c(1,1,0,0,0,0,1,0,0,0,0,0),c(0.95,0.9,0.9,0.7,0.65,0.6,0.5,0.4,0.3,0.2,0.15,0.1)) 
      ###random, difn marks the numb of repetitions if difn is high few repetition
  #    set.seed(2)
#      n1<-14
     # difn<-2
     # inp<-cbind(sample(c(1,0),n1,replace=TRUE),sort(sample(runif(difn),n1,replace=TRUE),decreasing=TRUE))
      ###balanced
      #inp<-cbind(sample(c(rep(0,n1/2),rep(1,n1/2))),sort(sample(runif(difn),n1,replace=TRUE),decreasing=TRUE))
      ##exm orig
    #  inp <- cbind(c(1,1,0,0),c(0.75,0.5,0.35,0.15))
     # inptrain<-cbind(c(1,1,1,0),c(0.75,0.5,0.35,0.15))
      ##exm 1
#inp <- cbind(c(1,0,0,1,0,0),c (0.9,0.65,0.45,0.35,0.2,0.15)) ## Exemple 1 
#      inptrain <- cbind(c(1,0,1,1,0),c(0.9,0.65,0.45,0.35,0.2))## Exemple 1 
#      inp <- cbind(c(1,1,0),c (0.8,0.5,0.1))
    
  # inp<-inverse_inp(inp)  ### fa de espill....## Exemple 1 
  #  inptrain<-inverse_inp(inptrain)  ### fa de espill....## Exemple 1 
      #inp <- cbind(c(1,0,0),c (0.9,0.5,0.2))
     # inptrain<-inp
     # inp<-cbind(c(0, 0, 1, 0, 0, 0, 1, 0, 1, 0),c(0.85,0.8,0.65,0.55,0.45,0.2,0.15,0.13,0.1,0.05)) ## Example ML2012 en paper
    
      #inp<-cbind(c(0,1, 0, 1, 0, 0, 0, 1, 0, 0),c(0.85,0.8,0.65,0.55,0.45,0.2,0.15,0.13,0.1,0.05)) ## Example MLJ012 en format local (pegar la volta)
      #inp <-cbind(inp[,1],pava(inp[,1],decreasing=TRUE)) ### convexify!!!
      #inp <- cbind(c(1,1,1,0),c(0.2,0.2,0.2,0.2))
  #       inp <- cbind(c(1,1,0,1,1,0,0,1,1,0,0,0,0,0),c(0.9,0.9,0.9,0.9,0.9,0.8,0.8,0.4,0.4,0.2,0.14,0.14,0.14,0.14)) 
       #    inptrain<-inp
#  inptrain<-cbind(c(1,1,0,0),c(0.85,0.65,0.45,0.25))
           proptest<-sum(inp[,1]==1)/length(inp[,1])
             proptest<-1-proptest
             prottrain<-sum(inptrain[,1]==1)/length(inptrain[,1])
             prottrain<-1-proptrain
      #       ff<-rep(proptrain,6)
      #       inp <- cbind(c(1,1,1,0,0,0),ff)
      #        #PETER
             #sort inp
             inp<-sort_inp(inp)
             inptrain<-sort_inp(inptrain)
      
      ###cost lines
      resul<-ExpLossC(inp,ProbThresFun) # Brier score (probabilistic threshold)
      dd<-LossCurveC(inp,ProbThresFun)
      #  cscoredriven<-list("points"=dd,"area"=resul)
      
      pdf(paste(destdir,methods[im],"trainCL.pdf",sep=""))
      
      ##calculem linies de cost i area
      #xrtrain<-CostLines(inptrain,title=methods[im])
      xrtrain<-CostLines(inptrain,title="Train")
      dev.off()
      
      pdf(paste(destdir,methods[im],"testCL_",alphabeta,".pdf",sep=""))
      
      ##calculem linies de cost i area
      #xr<-MinCost(inp,title=bquote(J48~gamma == .(alphabeta)))
     xr<-CostLines(inp,title="")
      #print(xr$res)
      
      opta<-0
      trainopt<-0
      scoa<-0
      scoaz<-0
      scda<-0
      scdaz<-0
      ratua<-0
      scua<-0
      
      vcreal<-c()
      vchat<-c()
      
      curvoptimal<-list()
      curvoptimal$points<-c()
      curvoptimal$selth<-c()
      
      curvtrainoptimal<-list()
      curvtrainoptimal$points<-c()
      curvtrainoptimal$selth<-c()
      
      
      curvscored<-list()
      curvscored$points<-c()
      curvscored$selth<-c()
      
      curvscoredz<-list()
      curvscoredz$points<-c()
      
      curvrated<-list()
      curvrated$points<-c()
      
      curvratedz<-list()
      curvratedz$points<-c()
      
      
      curvscdun<-list()
      curvscdun$points<-c()
      
      curvrtun<-list()
      curvrtun$points<-c()
      
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
        
        curvoptimal$title<-"testoptimal"
        curvoptimal$points[it]<-coptimal
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
        curvtrainoptimal$points[it]<-ctrainoptimal
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
        curvrated$points[it]<-cratedrive
        
        
        
        ####Rate_driven_Z
        # 
        
        
        #allt<-RateDrivenThresFunCdisROCCOST_empat(inp,zreal)
        allt<-RateDrivenThresFunCdisROCCOST_empat(inp,zhat)
        
        #  print("zreal")
        # print(zreal)
        #print(allt)
        thredsh1<-allt[1]
        thredsh2<-allt[2]
        pesos<-allt[3]
        # }
        #thredsh1<-1
        #thredsh2<-1
        # cratedrive<-comp_cost_single_rd(inp[,1],inp[,2],thredsh1,thredsh2,pesos,creal)
        cratedrivez<-pesos*comp_cost_single(inp[,1],inp[,2],thredsh1,creal)+(1-pesos)*comp_cost_single(inp[,1],inp[,2],thredsh2,creal)
        # print(cratedrive)
        # cratedrive$title<-"ratedriven"
        scdaz<-scdaz+cratedrivez
        
        curvratedz$title<-"ratedrivenz"
        curvratedz$points[it]<-cratedrivez
        
        ####score_driven_test
        
        thredsh<-ScoreDrivenThres(inp,chat)
        # print(thredsh)
        cscoredriven<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
        
        scoa<-scoa+  cscoredriven
        
        curvscored$title<-"scoredriven"
        curvscored$points[it]<-cscoredriven
        curvscored$selth[it]<-whichLC(xr$yf,cscoredriven,creal)
        
        #    print("scored")
        #    print(thredsh)
        #  print(cscoredriven)
        
        
        ####score_driven_Z
        #print("zhat")
        #print(zhat)
        #thredsh<-ScoreDrivenThres(inp,zreal)
        thredsh<-ScoreDrivenThres(inp,zhat)
        
        cscoredrivenz<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
        
        scoaz<-scoaz+  cscoredrivenz
        
        curvscoredz$title<-"scoredrivenz"
        curvscoredz$points[it]<-cscoredrivenz
        # print("th")
        #  print(thredsh)
        #     print("scored")
        #      print(cscoredriven)
        
        
        ####scoreunif
        thredsh<- ScoreUnif(inp,chat)
        scoreu<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
        scua<-scua+scoreu
        curvscdun$title<-"scoreuniform"
        curvscdun$points[it]<-scoreu
        
        ####rateunif
        thredsh<-RateUnif(inp,chat)        
        rateu<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
        ratua<-ratua+rateu
        curvrtun$title<-"rateuniform"
        curvrtun$points[it]<-rateu
        
        
        
      }
      
      #curves
      curvoptimal$area<-opta/km
      curvtrainoptimal$area<-trainopt/km
      curvscored$area<-scoa/km
      curvscoredz$area<-scoaz/km
      curvrated$area<-scda/km
      curvratedz$area<-scdaz/km
      curvscdun$area<-scua/km
      curvrtun$area<-ratua/km
      
      
      resu<-MSEdecomp(inp)
      resultats[k,]<-c(iter,id,im,AUC(inp),MSE(inp),resu$cal,resu$ref,curvoptimal$area,curvtrainoptimal$area,curvscored$area,curvscoredz$area,curvrated$area,curvratedz$area,curvscdun$area,curvrtun$area,proptrain)
      k<-k+1
      asd<-proptest*(1-proptest)*(1-(2*AUC(inp)))+1/3
      tit<-paste( "p0",format(proptest,digits=4),"AUC",format(AUC(inp),digits=4),"AUCH",format(AUCH(inp),digits=4), "MSE",format(MSE(inp),digits=4),"MAE",format(MAE(inp),digits=4),"CAL",format(resu$cal,digits=4),"REF",format(resu$ref,digits=4))
      tit<-methods[im]
      
      if(PLOTS) {
        tit=""
        area_unc<-plot_av_curves(inp,curvtrainoptimal$selth,curvoptimal$selth,curvscored$selth,xr$yf)
      #  tit<-paste(tit,"ate" ,format(area_unc$ateop,digits=4),"atr",format(area_unc$atrop,digits=4),"asd",format(area_unc$asd,digits=4),"ard",format(area_unc$ard,digits=4))
      #  tit<-paste(tit,"\n","tate" ,format(2*curvoptimal$area,digits=4),"tatr",format(2*curvtrainoptimal$area,digits=4),"tasd",format(MAE(inp),digits=4),"tard",format(curvrated$area+1/6,digits=4))
        tit<-methods[im]
        if (tit=="Majority") tit<-"Prior Model"
     #  tit<-"Test"
       # tit=""
        plot_curves_testl(list(curvscored,curvoptimal,curvrated,curvtrainoptimal),title=tit,newplot=FALSE)
      #  plot_curves_testl(list(curvoptimal),newplot=FALSE)
        dev.off()
      }
      #print(asd)
      
      if(PLOTS){
        pdf(paste(destdir,methods[im],"ROC.pdf",sep=""))
        ROC(inp,title=tit)
        dev.off()
      } 
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
    
    print(paste(datasets[id],methods[im],ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),ave(filt[1,9]),ave(filt[1,10]),ave(filt[1,11]),sep=","))
    
  }

#dev.off()

