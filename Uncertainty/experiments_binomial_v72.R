

PLOTS<-FALSE

library("RWeka")
library(caret)
source("options.R")
source("Threshold_methods.R")
source("cost_functions.R")
# La classe 0 es la minoritaria
# i la 1 la majoritaria
library("Iso") 


source("plots.R")
km<-RESOLUTION_EMPIRICAL_CURVES 
#PLOTS<-TRUE
#pdf("curvesinf.pdf")
#require("sampling")

args <- commandArgs(trailingOnly = TRUE)
###############################################
###############################################
############# OPERATING OPTIONS ###############
###############################################
alphabeta<-10
Numiter<-1

prop<-0.5#percentage for training

if (length(args)==3)
{
  
  if(args[1]=="Inf") alphabeta<-Inf
  else alphabeta<-as.integer(args[1])
  Numiter<-as.integer(args[2])
  prop<-as.double(args[3])#percentage for training
}

print("alphabeta")
print(alphabeta)
print("Numiter")
print(Numiter)
print("prop")
print(prop)

bfactor <- 2  # 1 is like Hand, and Drummond and Holte, but 2 makes more sense, since it places the maximum expected loss at 1 (and not 0.5 for a balanced dataset)
m<-1

titfile<-paste("Results/new2017bresults_binomial_iter",Numiter,"_p",prop,"_n",alphabeta,".Rdata",sep="")
##########
########
#######     MAIN
#####
########



#datasets<-c("spambase.arff","sick.arff","labor.arff","colic.arff","breast-w.arff","spect_test.arff","cylinder-bands.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")


#datasets<-c("spect_test.arff")

#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#datasets<-c("spect_test.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","credit-a.arff")# per a prop 0.1
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff")
#datasets<-c("tic-tac-toe.arff")
#selected<-2
datasets<-list.files("../data/")
#datasets<-c("spect_test.arff")
#datasets<-c("breast-w.arff","spect_test.arff","liver-disorders.arff","haberman.arff")

methods<-c("J48","J48Unp","logist","NB","Stump","IBK","IBK1","SMV","SMVp","PART","Major","Random")
#methods<-c("SMV")
#pdf(paste(methods[1],"_",datasets[1],".pdf",sep=""))

#pdf("peterexample.pdf")

nomscol<-c("iter","Dataset","Method","AUC","MSE","cal","ref","optimal","trainoptimal","scored","scoredz","rated","ratedz","scdun","rtun","proptrain","proptest","refconv")
numcol<-length(nomscol)

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
  #msel<-1
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
    set.seed(iter)
    w<-datos[sample(nrow(datos)),]
    # Separamos por 50 train 50 test
    indiceStrat<-createDataPartition(w[,posParamEstudio], p = prop,list=FALSE) ##STRATAFIED!!!
    train<-w[indiceStrat,]
    test<-w[-indiceStrat,]
    
    
    #  train<-w[resd[,2],]
    print("data")
    print(tam)
    #  print(propdata)
    print("train")
    print(length(train[,1]))
    #print(train)
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
      
      if (methods[msel]=="Major") probstrain<-rep((1-proptrain+0.001),length(train[,1]))## majority
      else if (methods[msel]=="Random") probstrain<-runif(length(train[,1]))##random
      else if (methods[msel]=="IBK1") ### IBK1 in Rweka computes probs in a strange way, we modify it
                                          ## we predict the  class,and then assign 1 if 
        {
        mpreds<-predict(model, newdata = train)
        probstrain<-rep(0,length(train[,1]))
        probstrain[which(mpreds==mnom)]<-1
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
      x <- t(inptrain)
      zord <- order(x[2,])
      zordrev <- rev(zord)
      screv <- x[,zordrev]
      inptrain <- t(screv) #Decreasing order
      
      
      probs<-matrix(0,length(test[,1]),1)
      
      if (methods[msel]=="Major") probs<-rep((1-proptrain+0.001),length(test[,1]))## majority
      else if (methods[msel]=="Random") probs<-runif(length(test[,1]))##random
      else if (methods[msel]=="IBK1") ### IBK1 in Rweka computes probs in a strange way, we modify it
        ## we predict the  class,and then assign 1 if 
      {
        mpreds<-predict(model, newdata = test)
        probs<-rep(0,length(test[,1]))
        probs[which(mpreds==mnom)]<-1
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
      #       #inp <- cbind(c(1,0,1,0,1,0,0,1),c(1,1,0.5,0.4,0.4,0.2,0,0))
      #       #  inp <- cbind(c(1,1,0,0,0,0,1,0,0,0,0,0),c(0.95,0.9,0.8,0.7,0.65,0.6,0.5,0.4,0.3,0.2,0.15,0.1)) 
      #       #   inp <- cbind(c(1,1,1,1,1,1,1,1,0,1,1,1,1,0),c(0.95,0.9,0.8,0.7,0.65,0.6,0.5,0.4,0.3,0.2,0.15,0.14,0.13,0.1)) 
      #       inptrain<-inp
      #       proptest<-sum(inp[,1]==1)/length(inp[,1])
      #       proptest<-1-proptest
      #       prottrain<-proptest
      #       ff<-rep(proptrain,6)
      #       inp <- cbind(c(1,1,1,0,0,0),ff)
      #        #PETER
      #sort test probs
      x <- t(inp)
      zord <- order(x[2,])
      zordrev <- rev(zord)
      screv <- x[,zordrev]
      inp <- t(screv) #Decreasing order
      
      
      ###cost lines
      resul<-ExpLossC(inp,ProbThresFun) # Brier score (probabilistic threshold)
      dd<-LossCurveC(inp,ProbThresFun)
      #  cscoredriven<-list("points"=dd,"area"=resul)
      
      
      
      ##calculem linies de cost i area
      xrtrain<-MinCost(inptrain)
      
      
      ##calculem linies de cost i area
      xr<-MinCost(inp)
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
      
      curvtrainoptimal<-list()
      curvtrainoptimal$points<-c()
      
      curvscored<-list()
      curvscored$points<-c()
      
      
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
        
        
        #zhat<- rbinom(1,alphabeta,zreal)/alphabeta
        #chat<-zhat*(proptrain-1)/(2*proptrain*zhat-zhat-proptrain)
        
        vcreal[it]<-creal
        vchat[it]<-chat
        
        thredsh<-ThresMin(inp,chat,xr$yf)
        
        
        #print(thredsh)
        coptimal<-comp_cost_single(inp[,1],inp[,2],thredsh,creal)
        #print(coptimal)
        opta<-opta+coptimal
        
        curvoptimal$title<-"optimal"
        curvoptimal$points[it]<-coptimal
        
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
      #print("MSE")
      #print (MSE (inp))
      
      resu<-MSEdecomp(inp)
      convinp<-cbind(inp[,1],pava(inp[,1],decreasing=TRUE))### convexify
      resuconv<-MSEdecomp(convinp)
      
      resultats[k,]<-c(iter,id,im,AUC(inp),MSE(inp),resu$cal,resu$ref,curvoptimal$area,curvtrainoptimal$area,curvscored$area,curvscoredz$area,curvrated$area,curvratedz$area,curvscdun$area,curvrtun$area,proptrain,proptest,resuconv$ref)
      k<-k+1
      asd<-proptest*(1-proptest)*(1-(2*AUC(inp)))+1/3
      tit<-paste( "p0",format(proptest,digits=4),"AUC",format(AUC(inp),digits=4), "MSE",format(MSE(inp),digits=4),"ASD",format(asd,digits=4))
      if(PLOTS) plot_curves_testl(list(curvscored,curvscoredz,curvoptimal,curvrated,curvratedz,curvtrainoptimal),title=tit,newplot=FALSE)
      
      #print(asd)
      
      if(PLOTS) ROC(inp)
    }
  }
}

dades<-data.frame(resultats)
names(dades)<-nomscol

#save(dades,file=titfile)

print("dataset,method,MSE(inp),coptimal$area,ctrainoptimal$area,cscoredriven$area,cratedrive$area")
for (id in 1:length(datasets))
  for (im in 1:length(methods))
    
  {
    filt<-subset(resultats,resultats[,2]==id&resultats[,3]==im)  
    
    print(paste(datasets[id],methods[im],ave(filt[1,5]),ave(filt[1,8]),ave(filt[1,9]),ave(filt[1,10]),sep=","))
    
  }

#dev.off()

