#' @title Reassign Members of Non-Significant Clusters
#' @description Reassigns data points from non-significant clusters to the nearest
#' significant cluster based on Euclidean distance.
#'
#' @details
#' This function is a post-processing step for clustering. After identifying a set of
#' "significant" or robust clusters (e.g., via enrichment analysis), this function
#' handles the observations that fall into non-significant clusters.
#'
#' The reassignment is based on a nearest-centroid rule. For each data point in a
#' non-significant cluster, the function calculates the L2-norm (Euclidean distance)
#' to the centroids (mean vectors) of all *significant* clusters. The point is then
#' reassigned to the cluster with the minimum distance. This method provides a principled
#' way to consolidate a clustering solution by merging less stable or sparsely populated
#' clusters into more robust, well-defined ones, thereby simplifying the final interpretation.
#'
#' If all initial classes are already within the significant set (`wh.class`), the function
#' returns the original class assignments without change.
#'
#' @param Xnew The numeric data matrix of observations to be reassigned. Should only
#'   contain observations from non-significant clusters.
#' @param class A vector of original cluster assignments for all observations.
#' @param mod The fitted model object (e.g., from `mclust::Mclust`), which must contain
#'   the cluster parameters, specifically `mod$parameters$mean`.
#' @param wh.class A vector containing the labels of the clusters considered "significant".
#'
#' @return A new vector of class assignments of the same length as the original `class`
#'   vector, where members of non-significant clusters have been reassigned.
#'
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




#' @title Assess Cluster Enrichment using a Permutation Test
#' @description Performs a permutation-based statistical test to evaluate the
#' enrichment (i.e., local density) of each cluster centroid from a GMM.
#' @import mclust
#'
#' @details
#' This function provides a statistical assessment of how "real" each cluster is. A robust cluster is expected to form in a region of high data density. This function tests whether the density at each observed cluster centroid is significantly higher than what would be expected by chance.
#'
#' The statistical procedure is as follows:
#' 1.  **Observed Density**: For the original fitted model (`mod`), it calculates the probability density ($\rho_{obs}$) at the location of each cluster's centroid using the estimated GMM parameters.
#' 2.  **Permutation (Null Distribution)**: It generates a null distribution for the density. This is done by:
#'     a.  Creating `nrep` permuted datasets. In each, the values within the data matrix are randomly shuffled, breaking any real structure.
#'     b.  For each permuted dataset, a new GMM is fitted with the same specifications (number of clusters, model name).
#'     c.  The density ($\rho_{perm}$) is calculated at the locations of the *original* centroids using the parameters from the model fitted to the *permuted* data.
#' 3.  **Enrichment and p-value**:
#'     -   The relative enrichment ($\rho_{rel}$) for each cluster is the ratio of the observed density to the mean of the densities from the null distribution: $\rho_{rel} = \rho_{obs} / \text{mean}(\rho_{perm})$. A value > 1 suggests the cluster is in a denser-than-random region.
#'     -   An empirical p-value is calculated for each cluster as the proportion of permutations where the permuted density was greater than or equal to the observed density. This represents the probability of observing such a high density by chance alone. A low p-value suggests the cluster is statistically significant.
#'
#' @param mod The fitted model object from `mclust::Mclust`.
#' @param BIC Optional. An `mclustBIC` object. If provided, `Mclust` is run on permuted data using this to select the number of clusters.
#' @param n.clust The number of clusters to fit on the permuted data. Used if `BIC` is `NULL`.
#' @param nrep The number of permutations to perform to build the null distribution.
#' @param seed A random seed for reproducibility of the GMM fitting on permuted data.
#'
#' @return A list containing:
#' \item{rho_obs}{A vector of the observed densities at each original cluster centroid.}
#' \item{rho_perm}{A matrix of densities from the permutation test (rows are permutations, columns are clusters).}
#' \item{rho_rel}{A vector of the relative enrichment scores for each cluster.}
#' \item{pvalues}{A vector of empirical p-values for each cluster.}
#'
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





#' @title Perform and Visualize Cluster Enrichment Analysis
#' @description A wrapper function that runs the enrichment analysis, identifies
#' significant clusters, and creates a diagnostic plot.
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot text
#'
#' @details
#' This function orchestrates the cluster validation workflow. It takes a fitted GMM
#' result and performs the following steps:
#' 1.  Calls `fit.enrichment` to conduct the permutation-based statistical test for cluster robustness.
#' 2.  Generates a diagnostic "volcano-like" plot, plotting the log-10 p-value against the relative enrichment score ($\rho_{rel}$). This plot visually separates significant clusters (typically in the upper right, with high enrichment and low p-value) from non-significant ones.
#' 3.  Identifies significant clusters (`SigCluster`) based on a significance threshold (e.g., p-value < 0.05).
#' 4.  Returns a list containing the enrichment results and other relevant model objects for downstream use.
#'
#' The function also contains commented-out code to demonstrate how one might use `reassign.enrichment` to create a new, consolidated set of cluster assignments.
#'
#' @param fit.GMM The fitted GMM object from `mclust::Mclust`.
#' @param Xclust A data frame containing the data with a `class` column for cluster assignments.
#' @param Xnew The raw numeric data matrix used for the GMM.
#' @param seed The random seed used in the original GMM fit, passed to `fit.enrichment` for reproducibility.
#' @param n.clust The number of clusters in the model.
#' @param nrep The number of permutations for the enrichment test.
#' @param filename A base string for the output PDF plot file.
#'
#' @return A list containing:
#' \item{Xnew}{The original data matrix.}
#' \item{Xclust2}{The original `Xclust` data frame (note: reassignment is commented out in the provided code).}
#' \item{fit.GMM}{The original fitted GMM object.}
#' \item{res.enrichment}{The list object returned by `fit.enrichment`.}
#'
#' @seealso \code{\link{fit.enrichment}}, \code{\link{reassign.enrichment}}
#' 
#' @export analysis.Enrichment
analysis.Enrichment <- function(fit.GMM, Xclust, Xnew, 
                                seed, n.clust=NULL, nrep=100,
                                filename=NULL){
  
  if(FALSE){
    fit.GMM=res.GMM.best$fit.best; 
    Xclust=res.GMM.best$Xclust;
    Xnew=res.GMM.best$Xclust;
    filename=filename;
    n.clust=res.GMM.best$wh.best[1];
    seed=res.GMM.best$wh.best[2]
    nrep=100
  }

  
  set.seed(1)
  # Enrichment analysis -----------------------------------------------------
  res.enrichment <- fit.enrichment(mod=fit.GMM, n.clust=n.clust, nrep=nrep, seed=seed)
  
  
  #
  
  
  # Plot and save the enrichment plot ----
  pdf(file=paste0(filename,"-Enrichment.pdf"), width=8, height=4)
  
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
  # write.csv(Xclust2, file=paste0(filename,"-Enrichment-Xclust2.csv"), quote=TRUE, row.names=FALSE)
  # 
  # 
  # save(Xnew, Xclust2, fit.GMM, res.enrichment, file=paste0(filename,"-3.Enrichment.RData"))

  result <- NULL
  result$Xnew <- Xnew
  result$Xclust2 <- Xclust2
  result$fit.GMM <- fit.GMM
  result$res.enrichment <- res.enrichment
  
  return(result)
  
}











#' @title Perform Enrichment Analysis Across a Range of Cluster Numbers
#' @description Systematically runs the permutation-based enrichment analysis for a
#' series of Gaussian Mixture Models with different numbers of clusters.
#' @import mclust
#' @importFrom dplyr %>%
#'
#' @details
#' This function serves as a sensitivity analysis tool for cluster robustness. Instead of
#' evaluating a single clustering solution, it iterates through a range of cluster numbers
#' (from 2 to `n.clust`).
#'
#' For each number of clusters `k` in the range, the function performs the following two steps:
#' 1.  Fits a Gaussian Mixture Model with `k` components to the data (`mclust::Mclust`).
#' 2.  Immediately runs the statistical validation procedure (`fit.enrichment`) on that `k`-cluster model.
#'
#' The results of each validation are stored in a list. This allows a user to compare not
#' only the number of significant clusters but also their enrichment scores and p-values
#' across different model complexities (i.e., different `k`). This approach helps in making a
#' more informed decision on the final number of clusters by assessing how the statistical
#' significance of cluster profiles changes as `k` is varied.
#'
#' @param Xnew A numeric matrix or data frame of observations.
#' @param n.clust The maximum number of clusters to test. The analysis will be
#'   performed for `k = 2, 3, ..., n.clust`.
#' @param nrep The number of permutations to perform in each call to `fit.enrichment`.
#' @param seed A random seed to ensure that the initial GMM fit is reproducible for
#'   each number of clusters.
#'
#' @return A list of lists. The k-th element of the returned list (`result[[k]]`) contains
#'   the full output from `fit.enrichment` for the k-cluster model. Elements for k < 2
#'   will be `NULL`.
#'
#' @seealso \code{\link{fit.enrichment}}, \code{\link{analysis.Enrichment}}
#'
#' @export analysis.Enrichment.range
analysis.Enrichment.range <- function(Xnew, n.clust=10, nrep=100, seed){
  
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

