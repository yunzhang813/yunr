test_weightedRankSumTestWithCorrelation <- function() {
  set.seed(1)
  stat <- rnorm(100)
  index <- 1:10
  stat[index] <- stat[1:10]+1
  weight <- runif(length(index), 0, 1)
  
  tst <- weightedRankSumTestWithCorrelation(index, stat, weight=weight, correlation=0.1)
  checkEqualsNumeric(as.vector(tst), c(0.97293933,0.02849134), tolerance=1.0e-4)
}
