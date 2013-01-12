#!/usr/bin/env Rscript

optim.beta = function (d,na.rm=T,nan.rm=T,method="ks") {
  d = as.numeric(d);
  if (length(which(is.na(d))) == 0) {
     if (na.rm) d = d[!is.na(d)] else stop("there are NA values in the input and na.rm != TRUE");
  } 
  if (length(which(is.nan(d))) == 0) {
     if (nan.rm) d = d[!is.nan(d)] else stop("there are NaN values in the input and nan.rm != TRUE");
  }
  if (length(which(d < 0 | d > 1)) != 0) stop("the input values must be strictly in the [0,1] range");
  if (length(d) == 0) stop ("you need to provide at least one value after removing NA and NaN");
  
  objective = if (method == "ks") function(par) {
      t = ks.test(d,function(x) pbeta(x,par[1],par[2]));
      t$statistic[1];
  } else if (method == "lk") function(par) {
      -sum(sapply(d,function(x) dbeta(x,par[1],par[2],log=T)));
  } else stop(paste("unknown method",method,sep=" ")); 

  p.start = c(1,1);
  p.lower = c(0.00001,0.00001);
  p.upper = c(1000,1000);
 
  result = optim(par=p.start,lower=p.lower,upper=p.upper,method="L-BFGS-B",fn=objective);
  result$ks.test = ks.test(d,function(x) dbeta(x,result$par[1],result$par[2]));
  result
} 


