weightedRankSumTestWithCorrelation <- function(index,statistics,weight=NULL,correlation=0,df=Inf)
# Two-sample rank sum test using Mann-Whitney U-statistic,
# allowing weight and correlation in test group.
# Developer and time: Yun Zhang, 9-12-2015.
{
  n <- length(statistics)
  n1 <- length(index)
  n2 <- n-n1
  
  ## Mann-Whitney style ranks
  r <- vector()
  statistics0 <- setdiff(statistics, statistics[index])
  for (i in 1:length(index)){r[i] <- sum(statistics[index[i]] > statistics0)}
  
  ## weight
  if(is.null(weight)){weight <- rep(1, n1)}
  U.w <- sum(weight*r)
  n1.w <- sum(weight)
  c1 <- sum(weight^2)
  c2 <- n1.w^2 - c1
  
  ## mean and variance estimates of Mann-Whitney U-statistic
  mu <- n1.w*n2/2
  if(correlation==0 || n1==1) {
    sigma2 <- c1*n2*(n2+2)/12 + c2*n2/12
  } else {
    sigma2 <- asin(1)*c1*n2 + asin(0.5)*c1*n2*(n2-1) + asin(correlation/2)*c2*n2*(n2-1) + asin((correlation+1)/2)*c2*n2
    sigma2 <- sigma2/2/pi
  }
  
  ## potential improvment: easy to have ties, but minor effect
  TIES <- (length(r) != length(unique(r))) 
  if(TIES) {
    NTIES <- table(r)
    adjustment <- sum(NTIES*(NTIES+1)*(NTIES-1)) / (n*(n+1)*(n-1)) 
    sigma2 <- sigma2 * (1 - adjustment)
  }
  
  ## z-score and p-value
  zlowertail <- (U.w+0.5-mu)/sqrt(sigma2)
  zuppertail <- (U.w-0.5-mu)/sqrt(sigma2)
  pvalues <- c(less=pt(zlowertail,df=df), greater=pt(zuppertail,df=df,lower.tail=FALSE))
  pvalues	
}

