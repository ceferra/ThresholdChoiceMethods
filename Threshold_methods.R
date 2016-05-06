
############ ThresMin(GENERIC)
ThresMin <- function(inp, s,yf)
{
  prbs<-rev(inp[,2])
  #print(length(prbs))
  #print(prbs)
  index <- which(duplicated(prbs))
  if (length(index)>0) prbs<-prbs[-index]
  
  #print(length(prbs))
  #print(prbs)
  
  numl<-length(yf)/2
  ymin<-1000
  lsel<-0
  for (j in 1:numl)
  { #print("segb")
    
    pdt<-yf[2*j]-yf[2*j-1]
    ya<-yf[2*j-1]+(pdt*s)
    
    if(ya<ymin){
      ymin<-ya  
      lsel<-j
    }
  }
  #print(numl)
  #print(length(prbs))
  #  print(s)
  #  print(ymin)
  #print(prbs)
  # print(lsel)
  #  print(numl)
  res<--1
  if (lsel==1) res<--0.1
  else    
    if (lsel==numl) res<-1.1
  else res=(prbs[lsel]+prbs[lsel-1])/2
  #print(res)
  ThresMin<-res
}
############ ScoreUniformThresFun(GENERIC)
ScoreUniformThresFun <- function(inp, s)
{
  t <- runif(1,0,1)
  t
}

############ ScoreUniformThresFun(for costs)
ScoreUniformThresFunC <- function(inp, s)
{
  t <- runif(1,0,1)
  t
}


############ ScoreUniformThresFun(for skews)
ScoreUniformThresFunZ <- function(inp, s)
{
  t <- runif(1,0,1)
  t
}


############ Prob ThresFun(GENERIC)
ProbThresFun <- function(inp, s)
{
  t <- s
  t
}

#aplicatrans sobre s
transf<- function( inp,s, prop)
{
  if (s < prop) {
    s <- s/prop  # normalitzem per a que vaja de 0 a 1
    t<-s*s * prop # tornem al rang
  } else {
    s <- (1-s)/(1-prop)   # normalitzem per a que vaja de 0 a 1
    t<-1 - (1-prop)*(s)^2   # tornem al rang
  }
  
  t 
}

############ Prob squared(GENERIC)
squared <- function(inp, s)
{
  t<-s*s
}

############ Prob squaredbal(GENERIC)
squaredprop <- function(inp, s,prop)
{
  
  
  if (s < prop) {
    t<-(1+(((1/prop)*s-1))^3)/2
  } else {
    t<-(((1/prop)*s-1)/((1/prop)-1))^3/2+0.5
  } 
  
  t
}

############ Prob rootsquared(GENERIC)
rootsquared <- function(inp, s)
{
  t <- sqrt(s)
  t
}


############ Prob rootsquared(GENERIC)
rootsquaredprop <- function(inp, s, prop)
{
  if (s < prop) {
    t<-sqrt(s)
  } else {
    t<-1 - sqrt(1-s)
  } 
  t
}


############ Prob ThresFunC(for costs)
ProbThresFunC <- function(inp, c)
{
  
  #  n0n1 <- nrow(inp)
  #  pi0 <- sum(inp[,1]==0) / n0n1
  #  pi1 <- sum(inp[,1]==1) / n0n1
  #  factorzc <- pi0 / ((1-c)*(1-pi0)+ c * pi0)
  
  t <- ProbThresFun(inp, c)  # original
  
  #  t <- ProbThresFun(inp, c) * factorzc  # radical
  
  t
}

############ Prob ThresFunZ(for skews)
ProbThresFunZ <- function(inp, z)
{
  n0n1 <- nrow(inp)
  pi0 <- sum(inp[,1]==0) / n0n1
  pi1 <- sum(inp[,1]==1) / n0n1
  
  t <- z  # Theoretical, as in the papers. And also radical
  
  factor <- pi1/ ((1-z)*pi0 + (z)*(pi1))
  
  #t <- ProbThresFun(inp, z*factor)
  #  t <- ProbThresFun(inp, z) * factor # equivalent to above. The good ones for the old curves.
  
  #  t <- ProbThresFun(inp, z) / factor
  
  #  factorzc <- pi0 / ((1-z)*(1-pi0)+ z * pi0)
  # t <- ProbThresFun(inp, z) * factorzc
  # t <- ProbThresFun(inp, z) / factorzc
  
  t
}


############ RateDrivenThresFun1  for costs (method 1. All segments equal except the first and the last with half the size)

RateDrivenThresFun1C <- function(inp, c)
{
  
  # sort
  x <- t(inp)
  zord <- order(x[2,])
  zordrev <- rev(zord)
  screv <- x[,zordrev]
  inp <- t(screv) #Decreasing order
  
  # Does not work well with ties
  n0n1 <- nrow(inp)
  pi0 <- sum(inp[,1]==0) / n0n1
  pi1 <- sum(inp[,1]==1) / n0n1
  
  
  # n0n1-1 segments/thresholds
  #  prop <- (n0n1-1) * (1-c)  
  #  t <- inp[prop+1,2] 
  
  # n0n1 segments/thresholds
  prop <- (n0n1) * (1-c)
  #  if (prop == 0) {
  #    t <- +Inf
  #  }  
  #  else {
  #    t <- inp[prop,2] 
  #  }
  
  # n0n1+1 segments/thresholds
  #  prop <- (n0n1+1) * (1-c)
  
  prop <- round(prop) # closest rate
  
  
  if (prop < 1) {
    t <- +Inf
  } else if (prop >= n0n1) { 
    t <- -Inf 
  } else {
    t <- (inp[prop,2] + inp[prop+1, 2]) / 2     
  }
  
  
  t
}




############ RatescoreDrivenThresFun1  for costs (mix between rate and mixed)

RateScoreDrivenThresFun1C <- function(inp, c)
{
  
  t<-RateDrivenThresFun1C(inp,c)
  alfa<-0.5
  t<-t*alfa + c*(1-alfa)
  
  
  t
}



############ RatescoreDrivenThresFun1  for costs (mix between rate and mixed)

ScoreDrivenThres <- function(inp, c)
{
  
  t<-c
  
  
  t
}


############ RateDrivenThresFun3  for costs (method 2. transform s before classic rated)

RateDrivenThresFun3C <- function(inp, c,prop)
{
  m<-transf(inp,c,prop)
  t<-RateDrivenThresFun1C(inp, m)
  t
}

############ RateDrivenThresFund  for costs (method 3. Interpolates by distance)

RateDrivenThresFunCdis <- function(inp, c)
{
  
  v<-rev(inp[,2])
  #  print(v)
  #v<-1 / (1 + exp(-v)) 
  # print(v)
  ntot<-length(v)
  #  normalitzem entre 0 i 1
  v<-v-min(v)
  if (max(v)>0) v<-v/max(v)
  
  
  tr<-0
  if (c<=(1/(ntot+1))) 
  { 
    tR <- 0
    
    return( tR)  
  }
  if (c>=(ntot/(ntot+1))) 
  { 
    tR <- 1
    
    return( tR)  
  }
  
  dist<-c(1:ntot)
  for (i in 1:ntot) {
    dist[i]=abs(c-i/(ntot+1))^2
    if (dist[i]==0)dist[i]=0.0000001
  }
  #print(dist)
  
  #invertim
  dist=1/dist
  #normalitzem distàncies
  dist=dist/sum(dist)
  # calculem el threshold ponderant per distàncies
  tR<-sum(dist*v)
  #esquerra <- sum(v < c)
  #dreta <- sum(v > c)
  #if (esquerra ==0)  tR <-  sum(dist[dreta]*v[dreta])/dreta
  #else if (dreta ==0)  tR <- sum(dist[esquerra]*v[esquerra])/esquerra 
  #    else  tR <- sum(dist[esquerra]*v[esquerra])/esquerra + sum(dist[dreta]*v[dreta])/dreta
  
  tR
}



############ RateDrivenThresFund  for costs (method Using distances)

RateDrivenThresFunCdist <- function(inp, c)
{
  
  #funciona per a probs entre 0 i 1
  v<-rev(inp[,2])  
  if (max(v)>1 || min(v)<0) { print("Warning there are probs outside 0 and 1")}
  
  
  #  print(v)
  #v<-1 / (1 + exp(-v)) 
  # print(v)
  ntot<-length(v)
  #  normalitzem entre 0 i 1
  # v<-v-min(v)
  #if (max(v)>0) v<-v/max(v)
  v1<-c(1:(ntot+2))
  v1[1] =0
  v1[(ntot+2)] =1
  for (i in 2:(ntot+1)) 
  { v1[i]=v[i-1]}
  
  #veiem on cau c (este bucle va mal) 
  for (i in 1:(ntot+1)) 
    if (c>=((i-1)/(ntot+1)) && c<((i)/(ntot+1))) break
  
  #esta entre v1[i] i v1[i+1]
  #print("i")
  #print(i)
  
  dista<- c-((i-1)/(ntot+1))
  distb<- (i/(ntot+1))-c 
  #print(v1)
  #print(c)
  
  #print(dista)
  #print(distb)
  #print("norm")
  
  if (dista==0) {
    tR<-v1[i]
  } else if (distb==0) { 
    tR<-v1[i+1]
  } else {
    #ponderem entre 0 i 1
    dista<-dista*(ntot+1)
    distb<-distb*(ntot+1)
    #invertim
    dista <-1-dista
    distb <-1-distb
    # ponderacio
    
    tR <- dista*v1[i]+distb*v1[i+1]
    
  }
  #print(dista)
  #print(distb)
  #print(tR)
  tR
}

############ ScoreUnif  (returns a random threshold between 0-1)

ScoreUnif <- function(inp, c)
{
  runif(1)
}


############ RateUnif  (reurns a random threshold between 0-1)

RateUnif <- function(inp, c)
{
  #funciona per a probs entre 0 i 1
  v<-rev(inp[,2])  
  
  ntot<-length(v)
  
  v1<-c(1:(ntot+2))
  v1[1] =0
  v1[(ntot+2)] =1
  for (i in 2:(ntot+1)) 
  { v1[i]=v[i-1]}
  
  #calculamos el valor del thrsdold como el punto medio
  v2<-c(1:(ntot+1))
  for (i in 1:(ntot+1)) 
  { v2[i]=(v1[i]+v1[i+1])/2}
  j<-sample(1:(ntot+1), 1)
  v2[j]
}

############ RateDrivenThresFund  for costs (method ROC COST Curve)

RateDrivenThresFunCdisROCCOST <- function(inp, c)
{
  
  #funciona per a probs entre 0 i 1
  v<-rev(inp[,2])  
  if (max(v)>1 || min(v)<0) { print("Warning there are probs outside 0 and 1")}
  
  
  #  print(v)
  #v<-1 / (1 + exp(-v)) 
  # print(v)
  ntot<-length(v)
  #  normalitzem entre 0 i 1
  # v<-v-min(v)
  #if (max(v)>0) v<-v/max(v)
  v1<-c(1:(ntot+2))
  v1[1] =0
  v1[(ntot+2)] =1
  for (i in 2:(ntot+1)) 
  { v1[i]=v[i-1]}
  
  #calculamos el valor del thrsdold como el punto medio
  v2<-c(1:(ntot+1))
  for (i in 1:(ntot+1)) 
  { v2[i]=(v1[i]+v1[i+1])/2}
  
  val<-(ntot)*c
  
  if (val==round(val))
  {tR<-v2[val+1]} #quadra , retornem el valor del threshold
  else
  {#no quadra necesitem fer ponderacio, per aixó estimem probs
    # si ntot = 4 i c=0.21, aleshores val es 1.05. Es ha dir donem 95% a v2[2] i 5% a v2[3]
    jj<-trunc(val)
    pes<-val-jj
    pes<-1-pes
    
    if (runif(1)>pes) tR<-v2[jj+2]
    else tR<-v2[jj+1]
  }
  tR
}

############ RateDrivenThresFund  for costs (method ROC COST Curve)
# metode per al rate driven si ens passes n examples i un c
# computem 2 thresholds i un pes
# per exemple si n=4 i 
# agafem un v (probs examples ordenats) de  0.2 0.4 0.7 0.8
# fem un vec aux  v1 0.0 0.2 0.4 0.7 0.8 1.0
# agafem un vec auxiliar v2 com el punt mig
# v2 0.10 0.30 0.55 0.75 0.90
# si  c = 0.3
# fem c*numexmepl= en este cas 0.3*4 =1.2
# aixo vol dire que tenim que ponderar 80% al v2[1] i 20 % v2[2]
RateDrivenThresFunCdisROCCOST_new <- function(inp, c)
{
  
  #funciona per a probs entre 0 i 1
  v<-rev(inp[,2])  
  if (max(v)>1 || min(v)<0) { print("Warning there are probs outside 0 and 1")}
  
  
  #  print(v)
  #v<-1 / (1 + exp(-v)) 
  # print(v)
  ntot<-length(v)
  #tenim ntot examples
  #  normalitzem entre 0 i 1
  # v<-v-min(v)
  #if (max(v)>0) v<-v/max(v)
  v1<-c(1:(ntot+2))
  v1[1] =-0.0001
  v1[(ntot+2)] =1.0001
  for (i in 2:(ntot+1)) 
  { v1[i]=v[i-1]}
  #print(v1)
  # si ntot =4
  # i v 0.2 0.4 0.7 0.8
  # v1 es 0.0 0.2 0.4 0.7 0.8 1.0
  # v2 es el punt mig
  # v2 0.10 0.30 0.55 0.75 0.90
  
  #calculamos el valor del thrsdold como el punto medio
  v2<-c(1:(ntot+1))

   for (i in 1:(ntot+1)) 
      v2[i]=(v1[i]+v1[i+1])/2
  
  val<-(ntot)*c
  
  if (val==round(val)) #no cal ponderar
     {tR<-c(v2[val+1],v2[val+1],1)} #quadra , retornem el valor del threshold
  else
     {#no quadra necesitem fer ponderacio, per aixó estimem probs
      # si ntot = 5 i c=0.21, aleshores val es 1.05 (5*0.21). Es ha dir donem 95% a v2[2] i 5% a v2[3]
      jj<-trunc(val)
      pes<-val-jj
      pes<-1-pes
      tR<-c(v2[jj+1],v2[jj+2],pes)
      #if (runif(1)>pes) tR<-v2[jj+2]
      #else tR<-v2[jj+1]
  }
  print (v2)
  tR
}



# Cas clàssic
# 
# tenim + + - - , 0.9 0.8 0.7 0.6
# 
# Aixó em dona 5 linies de cost. per a dibuixar la rate driven curve, interpole entre les 5. Es a dir si 0<c<0.25, interpole entre l1 i l2, si 0.25<c<0.5, interpole entre l2 i l3......
# 
# 
# Empats
# 
# tenim + + - - , 0.9 0.8 0.8 0.6
# 
# 
# Es a dir un empat al mig. El que en compte de considerar 5 linies de cost, en considere 4  (no puc partir l'empat) de la següent manera
#                                                                                             
#                          0<c<0.25 interpole entre l1 i l2
#                         0.25<c<0.75  interpole entre l2 i l3
#                         0.75<c<1  interpole entre l3 i l4
#                                                                                             
                                                                                         

RateDrivenThresFunCdisROCCOST_empat <- function(inp, cost)
{
  
  #funciona per a probs entre 0 i 1
  v<-rev(inp[,2])  
  if (max(v)>1 || min(v)<0) { print("Warning there are probs outside 0 and 1")}
  
  #print (v)
  ntot<-length(v)
  #Busquem empats:
  i<-1
  e<-rep(0,ntot)
  
  nrep<-0
  for (i in 2:ntot)
  {
    
    if (v[i-1]==v[i]) 
    {
      e[i]<-1
      nrep<-nrep+1
    }    
  }
  tamrep<-ntot-nrep
  #v2<-rep(0,ntot+1)
  #print(e)
  v2<-unique(v) #filtrem rep
  v2<-v2-0.0000001
  v2<-c(v2,1.001)
  v1<-rep(0,tamrep)
  step<-1/ntot
  acum<-step
  j<-1
  v1[j]<-acum
  
  for (i in 2:ntot)
  {
    if (e[i]==0) j<-j+1
    acum<-acum+step
    v1[j]<-acum
  }
  #print (v1)
  #print (v2)
  #calculamos el valor del thrsdold como el punto medio
  #v2<-c(1:(ntot+1))
 # v2<-c(0.15,0.3,0.6,0.9)
#  esp<-c(0.25,0.75,1)
esp<-v1
  #for (i in 1:(ntot+1)) 
  #   v2[i]=(v1[i]+v1[i+1])/2
  pos<-1
  for (i in 1:length(esp)) 
    if (esp[i]>cost) break
   # print("val i")
  #  print(i)
    jj<-i
   
   if (jj>1)
    {
     a<-0
     b<-esp[jj] -esp[jj-1]
     ncost<-cost-esp[jj-1]
   }
   else 
   {
     a<-0
     b<-esp[jj] 
     ncost<-cost
   }
  
    pes<-1-ncost/(b-a)
    #pes<-1-pes
  #  print("pes")
  #print(pes)
    tR<-c(v2[jj],v2[jj+1],pes)
    #if (runif(1)>pes) tR<-v2[jj+2]
    #else tR<-v2[jj+1]


  tR
}
