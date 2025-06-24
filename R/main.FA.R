#' @export main.FA
main.FA <- function(X, ID, nfactors=NULL, nfactors2=NULL){
  if(FALSE){
    X=res.Data$X; ID=res.Data$ID; nfactors=5
  }
  
  library(psych) # for FA
  library(car) # for FA

  
  tc <- textConnection("console", "w")
  sink(tc)
  sink(tc, type = "message")
  
  
  # Factor analysis ----
  ## Check the availability of Factor Analysis----
  
  cat("--------Check the availability of Factor Analysis--------\n")
  
  
  cat("--------(1) Kaiser-Meyer-Olkin (KMO) index--------\n")
  
  # (1) Kaiser-Meyer-Olkin (KMO) index.
  fit.KMO <- KMO(cor(X))
  
  cat("> fit.KMO$MSA\n")
  print(paste0( fit.KMO$MSA, " (≥ 0.6)" ) ) # ≥ 0.6
  
  # According to Kaiser’s (1974) guidelines, a suggested cutoff for determining the factorability of the sample data is KMO ≥ 0.6. The total KMO is 0.89, indicating that, based on this test, we can probably conduct a factor analysis.
  
  
  cat("--------(2) Bartlett's test--------\n")
  # (2) Bartlett's test
  fit.bartlett <- cortest.bartlett(R=cor(X), n=nrow(X))
  
  cat("> fit.bartlett$p.value\n")
  print(paste0(fit.bartlett$p.value, " (< 0.05)") ) # < 0.05
  # Test if R is an identity matrix. Low p-value indicates there exists a correlation.
  # Small p.value (<0.05) of the significance level indicate that a factor analysis may be useful with our data.
  
  
  cat("--------Choose the number of factors--------\n")
  
  parallel <- fa.parallel(X, fa="fa", fm="pa", sim=F, se.bars=F, plot=F)
  
  pdf(file=paste0("./", title,"-FA-ScreePlot.pdf"), width=8, height=4)
  plot(parallel, fa="fa", sim=F, se.bars=F)
  dev.off()
  
  pdf(file=paste0("./", title,"-FA-ScreePlot-simple.pdf"), width=8, height=4)
  parallel$fa.values[1:30] %>% {plot(1:length(.), ., type="b", xlab="The number of factors", ylab="Eigenvalues")}
  dev.off()
  
  
  if(is.null(nfactors)){
    nfactors <- max(which(parallel$fa.values > 1))
  }
  
  cat("> nfactors\n")
  print(nfactors)
  
  
  
  cat("--------A result of factor analysis--------\n")
  
  fit.fa <- fa(X, nfactors=nfactors, rotate="oblimin", scores="regression", fm="pa")
  
  
  pdf(file=paste0("./", title,"-FA-Diagram.pdf"), width=5, height=18)
  fa.diagram(fit.fa, sort=F)
  dev.off()
  
  
  cat("> fit.fa\n")
  print(fit.fa)
  
  cat("> fit.fa$loadings\n")
  fit.fa %>% .$loadings %>% {ifelse(abs(.)<0.3,0,.)} %>% print()
  
  
  sparse.loading <- fit.fa %>% .$loadings %>% {ifelse(abs(.)<0.3,0,.)}
  FactorNames <- apply(sparse.loading,2,function(x)names(which(x>0)) %>% gsub("[0-9]+","",.) %>% table %>% which.max %>% names)
  
  Xnew <- fit.fa$scores
  colnames(Xnew) <- FactorNames
  
  
  if(!is.null(nfactors2)){
    
    fit.multi <- fa.multi(X,nfactors,nfactors2, scores="regression", rotate=c("oblimin", "cluster")[1])
    
    parallel2 <- fa.parallel(fit.multi$f1$scores, fa="fa", sim=F, se.bars=F, plot=F)
    
    pdf(file=paste0("./", title,"-FA-SecondOrder-ScreePlot.pdf"), width=10, height=5)
    plot(parallel2, fa="pc", sim=F, se.bars=F, show.legend=T)
    # plot(parallel2, fa="pc", sim=F, se.bars=F, show.legend=F)
    dev.off()
    
    pdf(file=paste0("./", title,"-FA-SecondOrder.pdf"), width=6, height=12)
    fa.multi.diagram(fit.multi, cut=0.29)
    dev.off()
    
    
    cat("> fit.multi\n")
    print(fit.multi)
    
    cat("> fit.multi$f2$loadings\n")
    fit.multi$f2 %>% .$loadings %>% {ifelse(abs(.)<0.29,0,.)} %>% print()
    
    fit.multi.loadings <- fit.multi$f2$loadings
    row.names(fit.multi.loadings) <- FactorNames
    
    FactorNames2 <- fit.multi.loadings %>% {ifelse(abs(.)<0.29,0,.)} %>% 
      apply(2,function(x) names(which(x>0)) %>% gsub("[0-9]+","",.) %>% paste0(collapse=""))
    
    
    Xnew <- list(X=fit.multi$f1$scores, loadings=fit.multi$f2$loadings) %>% {
      scale(.$X) %*% (solve(cor(.$X)) %*% .$loadings)
    }
    colnames(Xnew) <- FactorNames2
    
  }
  
  
  sink(type = "message")
  sink()
  close(tc)
  
  
  # save(ID, X, Xnew, file=paste0(title,"-1.FactorAnalysis.RData"))
  
  
  
  
  result <- NULL
  result$ID <- ID
  result$X <- X
  result$Xnew <- Xnew
  result$console <- console
  result

}
