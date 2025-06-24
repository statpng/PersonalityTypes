if(FALSE){
  library(lavaan)
  
  cfa.model <- 'f1 =~ O1+O2+O3
              f2 =~ C1+C2+C3
              f3 =~ E1+E2+E3'
  
  fit.cfa <- cfa(cfa.model, X)
  
  
  summary(fit.cfa, fit.measures=TRUE,standardized=TRUE)
  
}