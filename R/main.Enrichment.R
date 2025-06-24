#' @export reassign.enrichment
reassign.enrichment <- function(Xnew, class, mod, wh.class){
  if(FALSE){
    Xnew <- Xnew2
    class <- Xclust$class
    mod <- fit.GMM
    wh.class <- SigCluster
  }
  if(FALSE){
    Xnew <- df[,paste0("PA", 1:5)]
    class <- df$class
    mod <- mod1
    wh.class <- c(2,8,5,7,6,4)
    # Xnew, Xclust$class, fit.GMM, SigCluster
  }
  
  if( all(class %in% wh.class) ) return(class)
  
  Xnew <- Xnew[!class %in% wh.class, ]
  centre <- t(mod$parameters$mean)[wh.class,]
  rownames(centre) <- wh.class
  
  n=nrow(Xnew); nclust=nrow(centre)
  dist.mat <- matrix(NA, n, nclust)
  for( j in 1:n ){
    # if( j %% 100 == 0 ) cat("# of samples:", j, "\n")
    for( i in 1:nclust ){
      dist.mat[j,i] <- norm(Xnew[j,] - centre[i,], "2")
    }
  }
  
  ClusterAssignment <- wh.class[apply(dist.mat,1,which.min)]
  
  class2 <- class
  class2[!class2%in%wh.class] <- ClusterAssignment
  
  class2 %>% table
  #   2    8    5    7    6    4    1    9    3
  # 713  364 2701  317 3930  530    0    0    0
  
  return(class2)
}



#' @export fit.enrichment
fit.enrichment <- function(mod, BIC=NULL, n.clust=NULL, nrep=100, seed){
  library(mclust)
  
  centre <- mod$parameters$mean
  params <- mod$parameters
  modelName <- mod$modelName
  data <- mod$data
  
  rho_obs <- t(centre) %>% dens(modelName=modelName, parameters=params)
  
  out.result <- as.list(1:nrep)
  for( iii in 1:nrep ){
    
    if(iii %% 10 == 0) print(iii)
    set.seed(iii)
    Xperm <- data %>% {matrix(sample(.), nrow(.), ncol(.))}
    if(!is.null(BIC)){
      set.seed(62)
      set.seed(97)
      set.seed(seed)
      mod.perm2 <- Mclust(Xperm, x = BIC, modelNames=modelName, verbose=FALSE)
    } else {
      set.seed(62)
      set.seed(97)
      set.seed(seed)
      mod.perm2 <- Mclust(Xperm, G = n.clust, modelNames=modelName, verbose=FALSE)
    }
    
    out.result[[iii]] <- dens(modelName=modelName, 
                              data=t(centre),
                              parameters=mod.perm2$parameters)
  }
  
  rho_perm <- out.result %>% do.call("rbind", .)
  
  
  TMP <- matrix( replicate(nrow(rho_perm), rho_obs), ncol=nrow(rho_perm) )
  rho_rel <- colMeans( t(TMP) / rho_perm)
  pvalues <- apply( t(TMP) / rho_perm, 2, function(x){
    (sum(x<1)+1) / (length(x)+1)
  })
  
  return( list(rho_obs=rho_obs, rho_perm=rho_perm, rho_rel=rho_rel, pvalues=pvalues) )
}




#' @export main.Enrichment
main.Enrichment <- function(fit.GMM, Xclust, Xnew, title, seed, n.clust=NULL, nrep=100){
  
  if(FALSE){
    fit.GMM=res.GMM.best$fit.best; 
    Xclust=res.GMM.best$Xclust;
    Xnew=res.GMM.best$Xclust;
    title=title;
    n.clust=res.GMM.best$wh.best[1];
    seed=res.GMM.best$wh.best[2]
    nrep=100
  }

  
  set.seed(1)
  # Enrichment analysis -----------------------------------------------------
  res.enrichment <- fit.enrichment(mod=fit.GMM, n.clust=n.clust, nrep=nrep, seed=seed)
  
  
  #
  
  
  # Plot and save the enrichment plot ----
  pdf(file=paste0(title,"-Enrichment.pdf"), width=8, height=4)
  
  with(res.enrichment, {
    plot(rho_rel, log10(pvalues), pch=18, cex=1.2, 
         xlab=bquote("Enrichment, "~rho/tilde(rho) ), ylab=bquote(log[10]~"p-value"), ylim=c(1.1*min(log10(res.enrichment$pvalues)),0))
    with(res.enrichment, text(x=rho_rel, y=0.2, labels=1:length(rho_rel), cex=0.5, xpd=NA))
  })
  
  dev.off()
  
  
  
  
  ## Significant clusters ----
  
  SigCluster <- res.enrichment %>% { order(.$rho_rel,decreasing=TRUE)[sort(.$pvalues, decreasing=FALSE)<0.05] }
  
  cat("SigCluster\n")
  print(SigCluster)
  
  
  
  Xclust2 <- Xclust
  
  # Xclust2$class2 <- reassign.enrichment(Xnew, Xclust$class, fit.GMM, SigCluster)
  # 
  # 
  # write.csv(Xclust2, file=paste0(title,"-Enrichment-Xclust2.csv"), quote=TRUE, row.names=FALSE)
  # 
  # 
  # save(Xnew, Xclust2, fit.GMM, res.enrichment, file=paste0(title,"-3.Enrichment.RData"))

  result <- NULL
  result$Xnew <- Xnew
  result$Xclust2 <- Xclust2
  result$fit.GMM <- fit.GMM
  result$res.enrichment <- res.enrichment
  
  return(result)
  
}
