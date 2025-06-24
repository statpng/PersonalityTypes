#' @export main.Enrichment.range
main.Enrichment.range <- function(Xnew, n.clust=10, nrep=100, seed){
  
  library(mclust)
  library(dplyr)
  
  enrichment.list <- NULL
  for(nn in 2:n.clust){
    
    set.seed(seed)
    fit <- Mclust(Xnew, G = nn, modelNames="VVV")
    
    set.seed(1)
    enrichment.list[[nn]] <- fit.enrichment(fit, n.clust=nn, nrep=100, seed=seed)
    
  }
  
  
  return(enrichment.list)
  
}
