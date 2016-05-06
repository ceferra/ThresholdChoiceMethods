


###############################################
###############################################
################ FUNCTIONS ####################
###############################################



############ Calculate an Empirical Loss Curve (for costs)
LossCurveC <- function(inp, ThresFun)
{
  resS <- RESOLUTION_EMPIRICAL_CURVES
  range <- (0:resS) / resS
  ELoss <- 1:(resS + 1)
  
  for (c in range) {
    if (THRESHOLD_CHOICE_METHOD == 4) {
      #Uniform. We need to average
      RESOL <- 100
      LV <- rep(0, RESOL)
      for (i in 1:RESOL) {
        t <- ThresFun(inp, c)
        LV[i] <- LossC(inp, t, c)
      }
      ELoss[round(c * resS + 1)] <- mean(LV)
    }
    else if (THRESHOLD_CHOICE_METHOD == 7) {
      # Interpolating
      
      vec <- RateDrivenThresFun3C(inp, c)
      tL <- vec[1]
      pL <- vec[2]
      tR <- vec[3]
      pR <- vec[4]
      
      ELL <- LossC(inp, tL, c)
      ELR <- LossC(inp, tR, c)
      
      ELoss[round(c * resS + 1)] <- ELL * pL + ELR * pR
      
    }
    else {
      t <- ThresFun(inp, c)
      ELoss[round(c * resS + 1)] <- LossC(inp, t, c)
    }
  }
  
  ELoss
}
#
#############################################################


############ Calculate Loss (for costs) given a classifier and a threshold
LossC <- function(inp, t, c) {
  lo0 <- 0
  lo1 <- 0
  
  n0n1 <- nrow(inp)
  pi0 <- sum(inp[,1] == 0) / n0n1
  pi1 <- sum(inp[,1] == 1) / n0n1
  
  for (i in 1:n0n1) {
    if (inp[i,2] < t) {
      #    if (inp[i,2] <= t) {
      pred <- 0
    }
    else
      pred <- 1
    if (pred != inp[i,1]) {
      if (pred == 1) {
        l0 <-  c  # 1 for 0-1 loss
        lo0 <- lo0 + l0
      }
      else {
        l1 <-  (1 - c)  # 1 for 0-1 loss
        lo1 <- lo1 + l1
      }
    }
  }
  lo0 <- bfactor * lo0
  lo1 <- bfactor * lo1
  (lo0 + lo1) / n0n1
  
}







############ Calculate ExpLoss (for skews)
ExpLossZ <- function(inp, ThresFun)
{
  ELoss <- 0
  resS <- RESOLUTION_EMPIRICAL_CURVES
  range <- (0:resS) / resS
  
  for (z in range) {
    t <- ThresFun(inp, z)
    ELoss <- ELoss + LossZ(inp, t, z)
  }
  
  ELoss / (resS + 1)
}
#
# Example of use:
#
# ExpLossZ(inp,ProbThresFunZ)
# ExpLossZ(inp,NatThresFunZ)





############ Calculate ExpLoss (for costs)
ExpLossC <- function(inp, ThresFun)
{
  ELoss <- 0
  resS <- RESOLUTION_EMPIRICAL_CURVES
  range <- (0:resS) / resS
  
  for (c in range) {
    t <- ThresFun(inp, c)
    ELoss <- ELoss + LossC(inp, t, c)
  }
  
  ELoss / (resS + 1)
}
#
# Example of use:
#
# ExpLossC(inp,ProbThresFunC)
# ExpLossC(inp,NatThresFunC)






############ Calculate an Empirical Loss Curve (for skews)
LossCurveZ <- function(inp, ThresFun)
{
  resS <- RESOLUTION_EMPIRICAL_CURVES
  range <- (0:resS) / resS
  ELoss <- 1:(resS + 1)
  
  for (z in range) {
    if (THRESHOLD_CHOICE_METHOD == 4) {
      #Uniform. We need to average
      RESOL <- 100
      LV <- rep(0, RESOL)
      for (i in 1:RESOL) {
        t <- ThresFun(inp, z)
        LV[i] <- LossZ(inp, t, z)
      }
      ELoss[round(z * resS + 1)] <- mean(LV)
    }
    else if (THRESHOLD_CHOICE_METHOD == 7) {
      # Interpolating
      
      vec <- RateDrivenThresFun3Z(inp, z)
      tL <- vec[1]
      pL <- vec[2]
      tR <- vec[3]
      pR <- vec[4]
      
      ELL <- LossZ(inp, tL, z)
      ELR <- LossZ(inp, tR, z)
      
      ELoss[round(z * resS + 1)] <- ELL * pL + ELR * pR
      
      
    }
    else
    {
      t <- ThresFun(inp, z)
      ELoss[round(z * resS + 1)] <- LossZ(inp, t, z)
    }
  }
  
  ELoss
}
#
# Example of use:
#
# LossCurveZ(inp,ProbThresFunZ)
# LossCurveZ(inp,NatThresFunZ)


############ Calculate Loss (for skews) given a classifier and a threshold
LossZ <- function(inp, t, z) {
  lo0 <- 0
  lo1 <- 0
  
  n0n1 <- nrow(inp)
  pi0 <- sum(inp[,1] == 0) / n0n1
  pi1 <- sum(inp[,1] == 1) / n0n1
  
  for (i in 1:n0n1) {
    if (inp[i,2] < t) {
      #   if (inp[i,2] <= t) {
      pred <- 0
    }
    else
      pred <- 1
    if (pred != inp[i,1]) {
      if (pred == 1) {
        l0 <-  z  # 1 for 0-1 loss
        lo0 <- lo0 + l0
      }
      else {
        l1 <-  (1 - z)  # 1 for 0-1 loss
        lo1 <- lo1 + l1
      }
    }
  }
  lo0 <- bfactor * 0.5 * lo0 / pi0
  lo1 <- bfactor * 0.5 * lo1 / pi1
  (lo0 + lo1) / n0n1
}


### La béstia!!
############ Calculate an Empirical Loss Curve (for costs)
MinCost <- function(inp,title = "")
{
  ###############################################
  #### SOME GENERAL VARIABLES USED ELSEWHERE ####
  
  n0n1 <- nrow(inp)
  x <- t(inp)
  zord <- order(x[2,])
  sc <- x[,zord]
  n1 <- sum(sc[1,])
  n0 <- n0n1 - n1
  pi0 <- n0 / n0n1
  pi1 <- n1 / n0n1
  
  # We modify inp to be ordered by scores (this is needed for several plots)
  # order data into increasing scores
  #######zord <- order(x[2,])
  zordrev <- rev(zord)
  # zordrev <- zord # NO REVERSE
  screv <- x[,zordrev]
  inp <- t(screv) #Decreasing order
  
  if (n0 == 0)
    print("No elements of class 0")
  if (n1 == 0)
    print("No elements of class 1")
  
  # We calculate means
  class0 <- x[,x[1,] == 0]
  class1 <- x[,x[1,] == 1]
  if (n0 > 1) {
    s0 <- mean(class0[2,])
  } else
    s0 <- class0[2]
  if (n1 > 1) {
    s1 <- mean(class1[2,])
  } else
    s1 <- class1[2]
  
  calpi1 <- pi0 * s0 + pi1 * s1
  
  
  # alpha and betad are the parameters in the beta
  # cost distribution ~ c^alpha * (1-c)^betad
  alpha <- 2
  betad <- 2
  
  ###############################################
  ##### ACCUMULATED EMPIRICAL DIST. F0, F1 ######
  
  # Calculate the raw ROC, replacing any tied
  # sequences by a diagonal in the ROC curve.
  
  # The raw ROC starts at F0[1]=0, F1[1]=0, and ends at
  # F0[K1]=n0, F1[K1]=n1.
  
  # F0 and F1 are counts, G0 and G1 are normalised in (0,1)
  # F0 and F1 eliminate merge repeated values.
  # F0rep and F1rep keep the repeated values as different elements in the vector.
  
  # K1 is the number of non-repeated scores. It will be used everywhere
  
  sc <-
    cbind(sc,sc[,n0n1])  # Adds the last example. Now sc is one element longer.
  
  F0 <- c(0:n0n1)
  F1 <- c(0:n0n1)
  
  K1 <- 1
  k <- 2
  for (i in 1:n0n1) {
    F0[k] <- F0[K1] + (1 - sc[1,i])
    F1[k] <- F1[K1] + sc[1,i]
    K1 <- k
    k <- if (sc[2,i + 1] == sc[2,i])
      (k)
    else
      (k + 1)
  }
  
  F0 <- F0[1:K1]
  F1 <- F1[1:K1]
  
  G0nomin <- F0 / n0
  G1nomin <- F1 / n1
  
  
  scshared <- sc[1,]
  j <- 1
  Ac <- sc[1,1]
  for (i in 2:n0n1) {
    if (sc[2,i] == sc[2,i - 1]) {
      j <- j + 1
      Ac <- Ac + sc[1,i]
    }
    else {
      for (k in (i - 1):(i - j)) {
        scshared[k] <- Ac / j
      }
      j <- 1
      Ac <- sc[1,i]
    }
  }
  
  F0rep <- c(0:n0n1)
  F1rep <- c(0:n0n1)
  
  K1rep <- 1
  krep <- 2
  for (i in 1:n0n1) {
    F0rep[krep] <- F0rep[K1rep] + (1 - scshared[i])
    F1rep[krep] <- F1rep[K1rep] + scshared[i]
    
    K1rep <- krep
    krep <- krep + 1
  }
  
  F0rep <- F0rep[1:K1rep]
  F1rep <- F1rep[1:K1rep]
  
  G0nominrep <- F0rep / n0
  G1nominrep <- F1rep / n1
  
  ###############################################
  
  
  
  ###############################################
  ######## CONVEX HULL - MINIMUM COSTS ##########
  
  # Find the upper concave hull
  # G0 and G1 are normalised in (0,1) and optimal
  # Repeated values are merged
  # hc will be the number of segments in the convex hull
  
  G0 <- c(0:(K1 - 1))
  G1 <- c(0:(K1 - 1))
  
  i <- 1
  hc <- 1
  while (i < K1)
  {
    c1 <- c((i + 1):K1)
    for (j in (i + 1):K1)
    {
      u1 <- (F1[j] - F1[i])
      u0 <- (F0[j] - F0[i])
      c1[j] <- u1 / (u1 + u0)
    }
    
    argmin <- i + 1
    c1min <- c1[i + 1]
    for (k in (i + 1):K1)
    {
      argmin <- if (c1[k] <= c1min)
        (k)
      else
        (argmin)
      c1min <- c1[argmin]
    }
    hc <- hc + 1
    G0[hc] <- F0[argmin]
    G1[hc] <- F1[argmin]
    i <- argmin
  }
  G0 <- G0[1:hc] / n0
  G1 <- G1[1:hc] / n1
  
  ###############################################
  ###############################################
  ##### ACCUMULATED EMPIRICAL DIST. F0, F1 ######
  
  # Calculate the raw ROC, replacing any tied
  # sequences by a diagonal in the ROC curve.
  
  # The raw ROC starts at F0[1]=0, F1[1]=0, and ends at
  # F0[K1]=n0, F1[K1]=n1.
  
  # F0 and F1 are counts, G0 and G1 are normalised in (0,1)
  # F0 and F1 eliminate merge repeated values.
  # F0rep and F1rep keep the repeated values as different elements in the vector.
  
  # K1 is the number of non-repeated scores. It will be used everywhere
  
  sc <-
    cbind(sc,sc[,n0n1])  # Adds the last example. Now sc is one element longer.
  
  F0 <- c(0:n0n1)
  F1 <- c(0:n0n1)
  
  K1 <- 1
  k <- 2
  for (i in 1:n0n1) {
    F0[k] <- F0[K1] + (1 - sc[1,i])
    F1[k] <- F1[K1] + sc[1,i]
    K1 <- k
    k <- if (sc[2,i + 1] == sc[2,i])
      (k)
    else
      (k + 1)
  }
  
  F0 <- F0[1:K1]
  F1 <- F1[1:K1]
  
  G0nomin <- F0 / n0
  G1nomin <- F1 / n1
  
  
  scshared <- sc[1,]
  j <- 1
  Ac <- sc[1,1]
  for (i in 2:n0n1) {
    if (sc[2,i] == sc[2,i - 1]) {
      j <- j + 1
      Ac <- Ac + sc[1,i]
    }
    else {
      for (k in (i - 1):(i - j)) {
        scshared[k] <- Ac / j
      }
      j <- 1
      Ac <- sc[1,i]
    }
  }
  
  F0rep <- c(0:n0n1)
  F1rep <- c(0:n0n1)
  
  K1rep <- 1
  krep <- 2
  for (i in 1:n0n1) {
    F0rep[krep] <- F0rep[K1rep] + (1 - scshared[i])
    F1rep[krep] <- F1rep[K1rep] + scshared[i]
    
    K1rep <- krep
    krep <- krep + 1
  }
  
  F0rep <- F0rep[1:K1rep]
  F1rep <- F1rep[1:K1rep]
  
  G0nominrep <- F0rep / n0
  G1nominrep <- F1rep / n1
  
  
  # x-axis using the uniform of the convex hull for cost ratios
  cost <- c(1:(hc + 1))
  cost[1] <- 0
  cost[hc + 1] <- 1
  
  
  
  
  for (i in 2:hc)
  {
    cost[i] <- 1 * pi1 * (G1[i] - G1[i - 1]) /
      (pi0 * (G0[i] - G0[i - 1]) + pi1 * (G1[i] - G1[i - 1]))
  }
  
  
  
  ###############################################
  ###### Expected cost Q (minimal) ##############
  ############## for cost ratio #################
  
  # CALCULATE THE MINIMUM LOSS VS c CURVE (UNNORMALISED, HAND)
  
  Q <- c(1:(hc + 1))
  
  for (i in 1:hc)
  {
    Q[i] <- bfactor * (cost[i] * pi0 * (1 - G0[i]) + (1 - cost[i]) * pi1 * G1[i])
  }
  Q[(hc + 1)] <- 0
  
  
  if (DRAW_COSTRATIO_PLOTS) {
    # only plots the space
    if (PLOT_SPACE) {
      #tit <- paste(EXPECTEDLOSS,BYCOST)
      tit <- title
      if (NOTITLE) {
        par(mar = MARGINSNOTITLE)
        tit <- ""
      }
      plot(
        c(0,1),c(0,0), ylim = c(0,YLIM), main = tit,xlab = COSTRATIO,ylab = LOSS, col =
          "white"
      )
      if (LEGEND) {
        for (i in 1:nrow(inp))
        {
          text(0.2 + i * 0.08,0.9,inp[i,1])
          text(0.2 + i * 0.08,0.8,inp[i,2])
        }
      }
    }
    if (DRAW_OPTIMAL_CURVES) {
      if (OVERLAP_PROB_OPTIMAL) {
        points(
          cost,Q, type = "o", lwd = LWD_OPTIMAL_CURVES, pch = PCH_OPTIMAL_CURVES, col =
            COLOUR_OPTIMAL_CURVES, lty = LTY_OPTIMAL_CURVES
        )
        
      }
      else {
        tit <- paste(EXPECTEDLOSS, BYCOST)
        if (NOTITLE) {
          par(mar = MARGINSNOTITLE)
          tit <- ""
        }
        
        plot(
          cost,Q, type = "l", lwd = LWD_OPTIMAL_CURVES, pch = PCH_OPTIMAL_CURVES, main = tit ,xlab = COSTRATIO,ylab = LOSS, lty = LTY_OPTIMAL_CURVES, col =
            COLOUR_OPTIMAL_CURVES, ylim = c(0,YLIM)
        )
        
      }
    }
    
  }
  # AUCC
  
  AUCC <- 0
  for (i in 2:(hc + 1))
  {
    AUCC <- AUCC + (cost[i] - cost[i - 1]) * (Q[i] + Q[i - 1]) / 2
  }
  
  
  
  
  
  ###############################################
  ###### Expected cost Qnomintots (ALL) #########
  ############## for cost ratio #################
  
  if (!COSTLINESREP) {
    # Reduces the number of lines (eliminates repeated ones), but it computes the red line in a wrong way if there are ties.
    
    # AUCCnomintots
    
    xnomintots <- rep(0,K1 * 4)
    ynomintots <- rep(0,K1 * 4)
    
    Leftacc <- 0
    Rightacc <- 0
    
    Leftmax <- 0
    Rightmax <- 0
    AUCCnomintots <- 0
    for (i in 1:K1)
      #for each cost line
    {
      #extremes of teh cost line
      Leftv <- G1nomin[i] * pi1 * bfactor
      Rightv <- (1 - G0nomin[i]) * pi0 * bfactor
      
      
      #cesar ponderem per llargaria de les X agafant els threshold method de Brier curves
      if (WEIGHTED_LOSSLINE)
      {
        wpm <- 0
        if (i == 1)
          wpm <- inp[,2][K1rep - 1]
        else if (i == K1rep)
          wpm <- 1 - inp[,2][1]
        else
          wpm <- inp[,2][(K1rep + 1 - i) - 1] - inp[,2][(K1rep + 1 - i)]
        #print(wpm)
        w <- wpm * K1rep # weight
        print(paste("line",i))
        print(w / K1)
        # print(inp)
        #  print(K1rep)
        #  print(K1)
      }
      else
      {
        w <- 1 # weight
      }
      Leftacc <- Leftacc + w * Leftv
      Rightacc <- Rightacc + w * Rightv
      
      Leftmax <- max(Leftmax, Leftv)
      Rightmax <- max(Rightmax, Rightv)
      
      
      xnomintots[(i - 1) * 4 + 1] <- 0
      xnomintots[(i - 1) * 4 + 2] <- 1
      xnomintots[(i - 1) * 4 + 3] <- 1
      xnomintots[(i - 1) * 4 + 4] <- 0
      
      ynomintots[(i - 1) * 4 + 1] <- Leftv
      ynomintots[(i - 1) * 4 + 2] <- Rightv
      ynomintots[(i - 1) * 4 + 3] <- 0
      ynomintots[(i - 1) * 4 + 4] <- 0
      
      linia <- (Rightv + Leftv) / 2
      #  print(linia)
      AUCCnomintots <- AUCCnomintots + linia
      #  print(AUCCnomintots)
    }
    
    AUCCnomintots <- AUCCnomintots / K1
    Leftacc <- Leftacc / K1
    Rightacc <- Rightacc / K1
#    print(c(Leftacc,Rightacc))
    
    if (DRAW_COSTRATIO_PLOTS) {
      if (DRAW_LOSSLINE) {
        points(
          c(0,1),c(Leftacc,Rightacc), type = "l", lty = LTY_LOSSLINE, col = COLOUR_LOSSLINE, lwd = LWD_LOSSLINE
        )
      }
      
      if (DRAW_COST_LINES) {
        if (!(OVERLAP_NONOPTIMAL)) {
          tit <- paste(EXPECTEDLOSS, BYCOST)
          if (NOTITLE) {
            par(mar = MARGINSNOTITLE)
            tit <- ""
          }
          plot(
            xnomintots,ynomintots, type = "l", main = tit,xlab = COSTRATIO,ylab = LOSS, lty =
              2, col = COLOUR_COST_LINES
          )
        }
        else {
          points(
            xnomintots,ynomintots, type = "l", lty = 2,  col = COLOUR_COST_LINES
          )
        }
        if (DRAW_LOSSLINE)
          points(
            c(0,1),c(Leftacc,Rightacc), type = "l", lty = LTY_LOSSLINE, col = COLOUR_LOSSLINE, lwd = LWD_LOSSLINE
          )
        
        if (DRAW_COSTSQUARE) {
          points(
            c(0,1),c(0,0), type = "l", col = "grey", lwd = 1
          ) # horizontal
          points(
            c(0,0),c(0,Leftmax), type = "l", col = "grey", lwd = 1
          )  # left vertical
          #    points(c(0,1),c(0,0), type= "l", col = "grey", lwd=1)
          points(
            c(1,1),c(0,Rightmax), type = "l", col = "grey", lwd = 1
          ) # right vertical
          #   points(c(1,1),c(0,1), type= "l", col = "grey", lwd=1)
        }
      }
      
      
      if (DRAW_OPTIMAL_CURVES_AGAIN) {
        points(
          cost,Q, type = "o", lwd = LWD_OPTIMAL_CURVE_AGAIN, col = COLOUR_OPTIMAL_CURVE_AGAIN, lty = LTY_OPTIMAL_CURVES, pch =
            PCH_OPTIMAL_CURVES
        )
      }
      
      
      if (DRAW_PROB_CURVES_AGAIN) {
        lines(
          costprobnew,Qprobnew, type = TYPE_PROB_CURVES, lwd = LWD_PROB_CURVE_AGAIN, col =
            COLOUR_PROB_CURVE_AGAIN, pch = PCH_PROB_CURVES
        )
      }
      
      
    }
    
    ## Xnominots nad Ynominots have elements non relevant
    hh <- length(xnomintots)
    xf <- c(1:(hh / 2))
    yf <- c(1:(hh / 2))
    kk <- 1
    for (i in 1:hh)
    {
      if (((i - 1) %% 4) < 2)
      {
        xf[kk] <- xnomintots[i]
        yf[kk] <- ynomintots[i]
        kk <- kk + 1
      }
    }
    # print(xnomintots)
    # print(ynomintots)
    
    # print(xf)
    #  print(yf)
    # print(cost)
    # print(Q)
    
  }
  
  
  #   plot(cost,xlim=c(0,1),ylim=c(0,1),Q, type= "l", main= "kkk",xlab="oo",axes=FALSE)
  resx = list("res" = AUCC,  "yf" = yf) #retiorne una llista en el valor y les cost lines
  MinCost <- resx
}
############ Calculate MSE (Brier score)
MSE <- function(inp)
{
  MSE <- 0
  for (i in 1:nrow(inp)) {
    MSE = MSE + (inp[i,1] - inp[i,2]) ^ 2
  }
  MSE / nrow(inp)
}

############ Calculate FMeasure
fmeas <- function(inp)
{
  fm <- 0
  thr <- 0.5
  mat <- matrix(0,2,2)
  for (i in 1:nrow(inp)) {
    if (inp[i,2] > thr)
    {
      if (inp[i,1] == 1)
        mat[1,1] <- mat[1,1] + 1
      else
        mat[1,2] <- mat[1,2] + 1
    }
    else
    {
      if (inp[i,1] == 0)
        mat[2,2] <- mat[2,2] + 1
      else
        mat[2,1] <- mat[2,1] + 1
    }
  }
  pr <- mat[1,1] / (mat[1,1] + mat[1,2])
  rec <- mat[1,1] / (mat[1,1] + mat[2,1])
  fm <- 2 * pr * rec / (pr + rec)
  fm
}


############ Calculate Refinement and Calibration loss (following Peter &Katsuma approach, ie, ROC Curve segments)
MSEdecomp <- function(inp)
{
  # BS = CAL + REF
  # we have T examples
  # we split data accordinf to segments in ROC space
  #For each segment i we have ni examples and pi  proportion of positive examples and p^i average prob of positive examp in the segment
  #CAL
  #1/T* sum for each segment i ni* (pi-p^i)^2
  #REF
  #1/T* sum for each segment i ni* pi*(1-pi)
  ##trobem segments
  nseg <-
    rep(0,nrow(inp)) ### un vector amb un index que indica el segment que te
  k <- 1
  nseg[1] = 1
  for (i in 2:nrow(inp)) {
    if (inp[i,2] != inp[i - 1,2])
    {
      k <- k + 1
      nseg[i] = k
      
    }
    else
      nseg[i] = k
  }
  #print(nseg)
  #inp 0.8 0.8 0.5 0.4 0.4 0.4
  #nseg  1 1 2 3 3 3
  numsegs <- nseg [nrow(inp)]
  #calibration
  cal <- 0
  ref <- 0
  for (i in 1:numsegs) {
    segini <- 0
    segfin <- 0
    for (j in 1:nrow(inp))
    {
      if (nseg[j] > i)
        break
      if (nseg[j] == i && segini == 0)
        segini <- j
      segfin <- j
    }
    #  print(segini)
    #  print(segfin)
    tamseg <- segfin - segini + 1
    phati <- inp[segini,2]
    psi <- sum(inp[,1][segini:segfin]) / tamseg
    cal <- cal + tamseg * (phati - psi) ^ 2
    ref <- ref + tamseg * psi * (1 - psi)
    # print(ref)
  }
  #print(cal/nrow(inp))
  #print(ref/nrow(inp))
  res <- list("cal" = cal / nrow(inp),"ref" = ref / nrow(inp))
  res
}

############ Calculate AUC (Area under the cost curve)
AUC <- function(inp)
{
  AUC <- 0
  np <- 0
  nn <- 0
  for (i in 1:nrow(inp)) {
    if (inp[i,1] == 1)
    {
      np = np + 1
      for (j in 1:nrow(inp))
      {
        if (inp[j,1] == 0)
        {
          if (inp[i,2] > inp[j,2])
            AUC = AUC + 1
          if (inp[i,2] == inp[j,2])
            AUC = AUC + 0.5
        }
      }
      
    }
    else
      nn = nn + 1
    
  }
  AUC = AUC / (nn * np)
  AUC
}


########compute the cost

comp_cost <- function(classes,probs,threds)
  
{
  tam <- length(classes)
  
  #Km is the resolution
  res <- c(1:km)
  for (i in 1:km) {
    cost <- 0
    acc <- 0
    sk <- i / km
    for (j in 1:tam) {
      if (probs[j] > threds[i])
        classe <- 1
      else
        classe <- 0
      if (classes[j] == classe)
        acc <- acc + 1
      if (classes[j] != classe)
        if (classe == 0)
          cost <- cost + (1 - sk)
        else
          cost <- cost + sk
        
    }
    
    res[i] <- 2 * cost / (tam)
  }
  #print(acc)
  #sum the cost under the lines
  fff <- 0
  for (i in 1:(km - 1))
    fff <- fff + (1 / km) * ((res[i] + res[i + 1]) / 2)
  #print("area comp_cost")
  # print(fff)
  comp_cost <- list("points" = res,"area" = fff)
}


########compute the cost noise
comp_cost_noise <- function(classes,probs,threds)
  
{
  tam <- length(classes)
  alfa <- 0.2
  #Km is the resolution
  res <- c(1:km)
  for (i in 1:km) {
    cost <- 0
    acc <- 0
    sk <- i / km
    sk <- sk +  runif(1,-alfa,alfa)
    for (j in 1:tam) {
      if (probs[j] > threds[i])
        classe <- 1
      else
        classe <- 0
      if (classes[j] == classe)
        acc <- acc + 1
      if (classes[j] != classe)
        if (classe == 0)
          cost <- cost + (1 - sk)
        else
          cost <- cost + sk
        
    }
    
    res[i] <- 2 * cost / (tam)
  }
  #print(acc)
  #sum the cost under the lines
  fff <- 0
  for (i in 1:(km - 1))
    fff <- fff + (1 / km) * ((res[i] + res[i + 1]) / 2)
  #print("area comp_cost")
  # print(fff)
  comp_cost_noise <- list("points" = res,"area" = fff)
}
# sample classifier, no ties, scores > 0 and < 1
# relatively good

# cls<-c(1,1,1,1,1,0,0,0,0,0)
# prbs<-c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
#thrs<-rep(0.44,100)
# print(comp_cost(cls,prbs,0.44))



########compute the cost with noise using a beta function
#Toma C para calcular el threshold i Chat para los costes
#chat to c model
comp_cost_beta_f <-
  function(classes,probs,threds,alphabeta = 10,mode = TRUE)
    
  {
    tam <- length(classes)
    
    #Km is the resolution
    res <- c(1:km)
    vnoise <- c(1:km)
    #ka <- diferentes valores para la beta dist
    ka <- 1
    #kp puntos por cada ka
    #kp<-100
    kp <- 10
    costtotal = 0
    for (i in 1:km) {
      ct = 0
      #    for (j in 1:ka) {
      cr = i / km
      # beta=(alfa*(1-cr)-1+2*cr)/cr
      
      if (mode == TRUE)
      {
        alpha <- cr * alphabeta + 1
        beta <- alphabeta - alpha + 2 #beta dist mode in cr
      }
      else
      {
        #alpha <- alphabeta * cr
        #beta <- alphabeta* (1 - (cr-0.0000001))  #beta dist mean in cr
        alpha <- (cr - 0.0000001) * (alphabeta + 2)
        beta <- alphabeta - alpha + 2
      }
      ca = 0
      ask <- 0
      for (z in 1:kp) {
        cost <- 0
        acc <- 0
        #    print(paste("alpha",alpha,"beta",beta))
        if (alphabeta != Inf)
          sk <- rbeta(1,alpha,beta)
        else
          sk <- i / km
        ask <- ask + sk
        
        for (j in 1:tam) {
          if (probs[j] > threds[i])
            classe <- 1
          else
            classe <- 0
          if (classes[j] == classe)
            acc <- acc + 1
          if (classes[j] != classe)
            if (classe == 0)
              cost <- cost + (1 - sk)
            else
              cost <- cost + sk
            #  print("costp")
            #print(cost)
        }
        #   print("cost")
        #  print(cost)
        ca = ca + cost
      }
      # print("sk")
      # print(paste(i/km,ask/kp))
      # print(threds[i])
      vnoise[i] = ask / kp
      ct = ct + (ca / kp)
      #  }
      costtotal = (ct / ka)
      res[i] <- 2 * costtotal / (tam)
      #   print("res")
      #    print(res[i])
    }
    #print(acc)
    #sum the cost under the lines
    fff <- 0
    for (i in 1:(km - 1))
      fff <- fff + (1 / km) * ((res[i] + res[i + 1]) / 2)
    #print("area comp_cost")
    # print(fff)
    comp_cost_beta <- list("points" = res,"area" = fff,"noise" = vnoise)
    
  }


########compute the cost with noise using a beta function
#Toma Chat para calcular el threshold i C para los costes
#c to chat model
comp_cost_beta <-
  function(classes,probs,threds,alphabeta = 10,mode = TRUE)
    
  {
    tam <- length(classes)
    
    #Km is the resolution
    res <- c(1:km)
    vnoise <- c(1:km)
    #ka <- diferentes valores para la beta dist
    ka <- 1
    #kp puntos por cada ka
    #kp<-100
    kp <- ITERATIONS_BETA_NOISE
    costtotal = 0
    for (i in 1:km) {
      ct = 0
      #    for (j in 1:ka) {
      cr = i / km
      # beta=(alfa*(1-cr)-1+2*cr)/cr
      
      if (mode == TRUE)
      {
        alpha <- cr * alphabeta + 1
        beta <- alphabeta - alpha + 2 #beta dist mode in cr
      }
      else
      {
        #alpha <- alphabeta * cr
        #beta <- alphabeta* (1 - (cr-0.0000001))  #beta dist mean in cr
        alpha <- (cr - 0.0000001) * (alphabeta + 2)
        beta <- alphabeta - alpha + 2
      }
      ca = 0
      ask <- 0
      for (z in 1:kp) {
        cost <- 0
        acc <- 0
        #    print(paste("alpha",alpha,"beta",beta))
        if (alphabeta != Inf)
          sk <- rbeta(1,alpha,beta)
        else
          sk <- i / km
        ask <- ask + sk
        tt <-
          1 + round(sk * (km - 1)) # los threshols van entre 1 i km, i sk entre 0 i 1
        for (j in 1:tam) {
          # print(tt)
          if (probs[j] > threds[tt])
            classe <- 1
          else
            classe <- 0
          if (classes[j] == classe)
            acc <- acc + 1
          if (classes[j] != classe)
            if (classe == 0)
              cost <- cost + (1 - cr)
            else
              cost <- cost + cr
            #  print("costp")
            #print(cost)
        }
        #   print("cost")
        #  print(cost)
        ca = ca + cost
      }
      # print("sk")
      # print(paste(i/km,ask/kp))
      # print(threds[i])
      vnoise[i] = ask / kp
      ct = ct + (ca / kp)
      #  }
      costtotal = (ct / ka)
      res[i] <- 2 * costtotal / (tam)
      #   print("res")
      #    print(res[i])
    }
    #print(acc)
    #sum the cost under the lines
    fff <- 0
    for (i in 1:(km - 1))
      fff <- fff + (1 / km) * ((res[i] + res[i + 1]) / 2)
    #print("area comp_cost")
    # print(fff)
    comp_cost_beta <- list("points" = res,"area" = fff,"noise" = vnoise)
    
  }

########compute the cost with noise using a beta function for ratedriven threshold method
#Toma Chat para calcular el threshold i C para los costes
#c to chat model
comp_cost_beta_rd <-
  function(classes,probs,threds1,threds2,pes,alphabeta = 10,mode = TRUE)
    
  {
    tam <- length(classes)
    
    #Km is the resolution
    res <- c(1:km)
    vnoise <- c(1:km)
    #ka <- diferentes valores para la beta dist
    ka <- 1
    #kp puntos por cada ka
    #kp<-100
    kp <- ITERATIONS_BETA_NOISE
    costtotal = 0
    for (i in 1:km) {
      ct = 0
      #    for (j in 1:ka) {
      cr = i / km
      # beta=(alfa*(1-cr)-1+2*cr)/cr
      
      if (mode == TRUE)
      {
        alpha <- cr * alphabeta + 1
        beta <- alphabeta - alpha + 2 #beta dist mode in cr
      }
      else
      {
        #alpha <- alphabeta * cr
        #beta <- alphabeta* (1 - (cr-0.0000001))  #beta dist mean in cr
        alpha <- (cr - 0.0000001) * (alphabeta + 2)
        beta <- alphabeta - alpha + 2
      }
      ca = 0
      ask <- 0
      for (z in 1:kp) {
        cost1 <- 0
        cost2 <- 0
        acc <- 0
        #    print(paste("alpha",alpha,"beta",beta))
        if (alphabeta != Inf)
          sk <- rbeta(1,alpha,beta)
        else
          sk <- i / km
        ask <- ask + sk
        tt <-
          1 + round(sk * (km - 1)) # los threshols van entre 1 i km, i sk entre 0 i 1
        for (j in 1:tam) {
          #primera
          # print(tt)
          if (probs[j] > threds1[tt])
            classe <- 1
          else
            classe <- 0
          if (classes[j] == classe)
            acc <- acc + 1
          if (classes[j] != classe)
            if (classe == 0)
              cost1 <- cost1 + (1 - cr)
            else
              cost1 <- cost1 + cr
            
            #segona
            #primera
            # print(tt)
            if (probs[j] > threds2[tt])
              classe <- 1
            else
              classe <- 0
            if (classes[j] == classe)
              acc <- acc + 1
            if (classes[j] != classe)
              if (classe == 0)
                cost2 <- cost2 + (1 - cr)
              else
                cost2 <- cost2 + cr
              #  print("costp")
              #print(cost)
        }
        #   print("cost")
        #  print(cost)
        cost <-
          (pes[tt] * cost1) + ((1 - pes[tt]) * cost2)#weighted average according to pes
        ca = ca + cost
      }
      # print("sk")
      # print(paste(i/km,ask/kp))
      # print(threds[i])
      vnoise[i] = ask / kp
      ct = ct + (ca / kp)
      #  }
      costtotal = (ct / ka)
      res[i] <- 2 * costtotal / (tam)
      #   print("res")
      #    print(res[i])
    }
    #print(acc)
    #sum the cost under the lines
    fff <- 0
    for (i in 1:(km - 1))
      fff <- fff + (1 / km) * ((res[i] + res[i + 1]) / 2)
    #print("area comp_cost")
    # print(fff)
    comp_cost_beta <- list("points" = res,"area" = fff,"noise" = vnoise)
    
  }




#######################33
#calcula el coste dado un threshold y un cr
comp_cost_single <- function(classes,probs,thred,cr)
  
{
  tam <- length(classes)
  cost <- 0
  for (j in 1:tam) {
    # print(tt)
    if (probs[j] > thred)
      classe <- 1
    else
      classe <- 0
    #if (classes[j]==classe) acc<-acc+1
    if (classes[j] != classe)
      if (classe == 0)
        cost <- cost + (1 - cr)
      else
        cost <- cost + cr
      #  print("costp")
      #print(cost)
  }
  comp_cost_single <- 2 * cost / tam
}


#######################33
#calcula el coste dado un threshold y un cr para el caso de rate driven (interpola)
comp_cost_single_rd <- function(classes,probs,threds1,threds2,pes,cr)
  
{
  tam <- length(classes)
  cost1 <- 0
  cost2 <- 0
  for (j in 1:tam) {
    #primera
    # print(tt)
    if (probs[j] > threds1)
      classe <- 1
    else
      classe <- 0
    
    if (classes[j] != classe)
      if (classe == 0)
        cost1 <- cost1 + (1 - cr)
      else
        cost1 <- cost1 + cr
      
      #segona
      #primera
      # print(tt)
      if (probs[j] > threds2)
        classe <- 1
      else
        classe <- 0
      
      if (classes[j] != classe)
        if (classe == 0)
          cost2 <- cost2 + (1 - cr)
        else
          cost2 <- cost2 + cr
        
  }
  
  #   print("cost")
  #  print(cost)
  cost <-
    (pes * cost1) + ((1 - pes) * cost2)#weighted average according to pes
  
  
  comp_cost_single_rd <- 2 * cost / tam
  
}

###################
#From a list of cost lines  a point, and a cost poit returns which cost line is selected
whichLC<-function(costlines,selpoint,cr)
{
  nlin<-length(costlines)/2
  pos<-c(1:nlin)
  k<-1
  for (j in 1:nlin){
    pos[j]=cr*(costlines[k+1]-costlines[k])+costlines[k]
    k<-k+2
  }
  #print(cr)
  #print(pos)
 #print(selpoint)
  pos <-round(pos, digits=6)##si no no va ???
  selpoint <-round(selpoint, digits=6)##si no no va ???
#  print((which(pos==selpoint)))
  
  whichLC<-(which(pos==selpoint)[1])
}
