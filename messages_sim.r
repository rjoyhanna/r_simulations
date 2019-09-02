# finds P(X1 = 2), P(X2 = 2) and P(X2 = 2|X1 = 1) in ALOHA example 

# we want P(x1 = x2 = 0) and P(x1 = x2 = 1) and P(x1 = x2 = 2)
sim <- function(p,q,nreps) {
  
  #OLD COUNTERS
  countx2eq2 <- 0
  countx1eq1 <- 0 
  countx1eq2 <- 0 
  
  #NEW COUNTERS
  countx1eqx2 <- 0
  countceq0 <- 0
  countceq1 <- 0
  countceq2 <- 0
  
  countx2eq2givx1eq1 <- 0 
  # simulate nreps repetitions of the experiment 
  for (i in 1:nreps) { 
    numsend <- 0 # no messages sent so far
    
    #NEW COUNTER
    numcollisions <- 0
    
    # simulate A and B???s decision on whether to send in epoch 1
    for (j in 1:2) 
      if (runif(1) < p) numsend <- numsend + 1
    if (numsend == 1) X1 <- 1 
    
    #NEW
    else {
      numcollisions <- numcollisions + 1
      X1 <- 2 
    }
    
    if (X1 == 2) countx1eq2 <- countx1eq2 + 1 
    # now simulate epoch 2
    # if X1 = 1 then one node may generate a new message
    numactive <- X1 
    if (X1 == 1 && runif(1) < q) numactive <- numactive + 1
    # send? 
    if (numactive == 1) 
      if (runif(1) < p) X2 <- 0
      else X2 <- 1
    else { # numactive = 2 
      numsend <- 0 
      for (i in 1:2) 
        if (runif(1) < p) numsend <- numsend + 1 
      if (numsend == 1) X2 <- 1 
      
      #NEW
      else {
        numcollisions <- numcollisions + 1
        X2 <- 2 
      }
    }
    
    #NEW
    if(X1 == X2) {
      countx1eqx2 <- countx1eqx2 + 1
      if (numcollisions == 0) countceq0 <- countceq0 + 1
      else if (numcollisions == 1) countceq1 <- countceq1 + 1
      else if (numcollisions == 2) countceq2 <- countceq2 + 1
    }
    
    
    if (X2 == 2) countx2eq2 <- countx2eq2 + 1 
    if (X1 == 1) { # do tally for the cond. prob. 
      countx1eq1 <- countx1eq1 + 1
      if (X2 == 2) countx2eq2givx1eq1 <- countx2eq2givx1eq1 + 1
    } 
  } 
  # print results 
  cat("P(X1 = 2):",countx1eq2/nreps,"\n") 
  cat("P(X2 = 2):",countx2eq2/nreps,"\n") 
  cat("P(X2 = 2 | X1 = 1):",countx2eq2givx1eq1/countx1eq1,"\n") 
  
  #NEW RESULTS
  cat("P(0 collisions):",countceq0/countx1eqx2,"\n")
  cat("P(1 collision):",countceq1/countx1eqx2,"\n")
  cat("P(2 collisions):",countceq2/countx1eqx2,"\n")
}

