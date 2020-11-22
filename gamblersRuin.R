conditionGenerator <- function() {
  
  print('Hello. Tell me the probability of A winning');
  pA <- scan(what = double(), nlines = 1, quiet = TRUE);
  pB <- 1-pA;
  print("Probability of A winning");
  print(pA);
  print("tell me the starting cash of A");
  cashA <- scan(what = integer(), nlines = 1, quiet = TRUE);
  
  print("Now tell me the starting cash of B");
  cashB <- scan(what = integer(), nlines = 1, quiet = TRUE);
  
  return(c(pA,cashA,pB,cashB));
  
}


ruinSimulator <- function(conditions){
  pA <- conditions[1];
  cashA <- conditions[2];
  pB <- conditions[3];
  cashB <- conditions[4];
  
  
  while(cashA != 0 & cashB != 0) {
      
    if (rbinom(1, 1, pA)) {
      
      cashA = cashA + 1;
      cashB = cashB - 1;
      
    }
    
    else {
      cashA = cashA - 1;
      cashB = cashB + 1;
      
    }
    
  }
    
  if (cashB == 0) {
      return(1);
  }
  else {
      return (0);
  }
}

n <- 2000;
condVector <- conditionGenerator();
r <- replicate(n, ruinSimulator(condVector));

res <- sum(r)/length(r);

cat("NUMBER OF TRIALS SIMULATED: ", n, '\n');
cat("THE PROBABILITY OF WINNING IS: ", res)



