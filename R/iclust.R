if(FALSE){
  library(psych)
  
  ic.out <- iclust(X, nclusters=3, alpha=0, beta=0, output=3)
  plot(ic.out)
  
  (as.matrix(X) %*% ic.out$loadings)
  
  
  fa.diagram(ic.out4$pattern,Phi=ic.out4$Phi,main="Pattern taken from iclust") 
  
  
  ic.out <- ICLUST(X)
  
  print(ic.out,digits=2)
  
}