# Due to a nondisclosure agreement and joint data ownership between the Center for [blinded] and Kakao Corporation, 
# we are unable to publicly share the raw scores of individual scale items. 
# Instead, the dataset includes factor scores derived from our factor analysis of the IPIP-NEO-120 responses, 
# along with the composite score(s) for each correlate. Although the raw data necessary for conducting a factor analysis are not included, 
# we have provided the analysis code for transparency. The dataset can be analyzed beginning with the code provided for the GMM analysis.
  



#' @title Calculate Normalized Entropy Criterion (NEC)
#' @description Calculates the Normalized Entropy Criterion for a given clustering model.
#'
#' @details
#' The NEC is used to select the optimal number of clusters. It balances the improvement in model fit (log-likelihood) against the uncertainty in classification (entropy). The NEC is defined as:
#' \deqn{ NEC(K) = \frac{E(K)}{\log L(K) - \log L(1)} }
#' where \eqn{K} is the number of clusters.
#' - \eqn{E(K)} is the total classification entropy, calculated by summing the entropy of the posterior probability distribution for each observation. This function uses Tsallis entropy for this calculation: \eqn{E(K) = \sum_{i=1}^{N} S_q(z_i)}, where \eqn{z_i} is the vector of posterior probabilities for observation \eqn{i}.
#' - \eqn{\log L(K)} is the log-likelihood of the model with \eqn{K} clusters.
#' - \eqn{\log L(1)} is the log-likelihood of the baseline model with one cluster.
#' The optimal number of clusters is the one that minimizes the NEC.
#'
#' @param model A model object, typically from `mclust::Mclust`, which must contain posterior probabilities (`model$z`) and log-likelihood (`model$loglik`).
#' @param base_loglik The log-likelihood of the baseline model (e.g., G=1).
#' @param q The entropic-index for the Tsallis entropy calculation. Defaults to 1 (Shannon Entropy).
#' @return A single numeric value representing the NEC.
#' 
#' @export calculate_nec
calculate_nec <- function(model, base_loglik, q=1){
  probs <- model$z
  eps <- 1e-10
  probs <- pmax(probs, eps)
  # probs <- t( apply(probs, 1, function(x) x/sum(x)) )
  
  # entropy <- -sum(probs * log(probs, base))
  entropy <- sum( apply(probs, 1, tsallis_entropy, q=q) )
  
  loglik_diff <- model$loglik - base_loglik
  nec <- entropy / (loglik_diff)
  
  return(nec)
}



shannon_entropy <- function(p) {
  if (abs(sum(p) - 1) > 1e-9) {
    p <- p / sum(p)
  }
  p <- p[p > 0]
  -sum(p * log(p))
}


tsallis_entropy <- function(p, q) {
  if(abs(sum(p) - 1) > 1e-9){
    p <- p / sum(p)
  }
  if (abs(q - 1) < 1e-9) {
    return(shannon_entropy(p))
  }
  p <- p[p > 0]
  
  (1 - sum(p^q)) / (q - 1) # Tsallis entropy
}






#' @title Find the Best Gaussian Mixture Model using NEC
#' @description Performs GMM clustering for a range of cluster numbers and random seeds,
#' selecting the best model based on the Normalized Entropy Criterion (NEC).
#' @import mclust
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics matplot points abline
#' @importFrom stats runif
#'
#' @details
#' This function systematically explores the solution space of Gaussian Mixture Models to find the optimal number of clusters. It iterates through a specified number of clusters (`1:max.clust`) and a set of random initializations (`seed.set`).
#'
#' For each combination of cluster number and seed, it fits a GMM (`mclust::Mclust` with `modelNames="VVV"`) and calculates the NEC using `calculate_nec`. The `q` value for Tsallis entropy within NEC is hardcoded to 1.5 in this implementation.
#'
#' The function identifies the combination of cluster number and seed that yields the minimum NEC value globally. It generates several diagnostic plots:
#' 1.  A plot of NEC values across all tested seeds, highlighting the best model.
#' 2.  A plot of sorted NEC values for the optimal number of clusters.
#' 3.  Boxplots of the data distributions for the top 3 best clustering results.
#'
#' The function returns a comprehensive list of results, including the matrix of all NEC values and the best model found.
#'
#' @param Xnew A numeric matrix or data frame of observations.
#' @param ID An optional identifier for the analysis run.
#' @param max.clust The maximum number of clusters to test. Default is 10.
#' @param filename A base string for output PDF file names. If `NULL`, plots are not saved.
#' @param subsample.size The number of random seeds to try. Default is 100.
#' @param type The criterion for model selection (currently hardcoded to "NEC").
#' @return A list containing:
#' \item{ID}{The analysis identifier.}
#' \item{Xnew}{The input data.}
#' \item{NEC.mat}{A matrix of NEC values (rows are seeds, columns are cluster numbers).}
#' \item{fit.best}{The `Mclust` object for the best model found (one of the top 3).}
#' \item{wh.best}{A vector with the optimal number of clusters and the corresponding seed.}
#' \item{Xclust}{The input data with an appended `class` column from the best model.}
#'
#' @export analysis.GMM.best
analysis.GMM.best <- function(Xnew=NULL, ID=NULL, max.clust=10, 
                              filename=NULL, subsample.size=100, print.pdf=TRUE, 
                              q=1.0, type="NEC"){
  
  library(mclust) # for GMM
  
  
  NEC.mat <- matrix(NA, 200, max.clust)
  
  count <- 0
  base_loglik <- Mclust(Xnew, G=1, modelNames="VVV")$loglik
  for(j in seed.set){
    count <- count + 1
    print(count)
    fit.mclust <- NULL
    for(i in 1:200){
      set.seed(j)
      fit.mclust[[i]] <- Mclust(Xnew, G=i, modelNames="VVV")
      
      if(i > 1){
        nec.vec <- calculate_nec(model=fit.mclust[[i]], base_loglik=base_loglik, q=q)
        nec.vec
        NEC.mat[j,i] <- nec.vec
      }
      
    }
  }
  
  wh.best <- t(NEC.mat) %>% {which(.==sort(.)[1], arr.ind=TRUE)}
  
  # apply(NEC.mat, 2,function(x) mean(x,na.rm=TRUE))
  
  # pdf(filename%++%"-GMM-NEC-TotalSeeds-MeanCurve.pdf", height=4, width=8)
  # t(NEC.mat) %>% { matplot( cbind(rowMeans(.), .), type="l", lty=c(1,rep(2,ncol(.))), col=c("red", rep("gray50", ncol(.))), ylab="Normalized Entropy Criterion", xlab="The number of clusters") }
  # dev.off()
  # extrafont::embed_fonts(filename%++%"-GMM-NEC-TotalSeeds-MeanCurve.pdf")
  
  pdf(filename%++%"GMM-NEC-TotalSeeds.pdf", height=4, width=8)
  t(NEC.mat) %>% { matplot(., type="l", lty=rep(2,ncol(.)), col=c(rep("gray50", ncol(.))), ylab="Normalized Entropy Criterion", xlab="The number of clusters") }
  wh.best <- t(NEC.mat) %>% { which(.==sort(.)[1], arr.ind=TRUE) }
  t(NEC.mat) %>% { points(wh.best[1], .[wh.best[1],wh.best[2]], col="red", pch=18) }
  abline(v=wh.best[1], col="red", lty=2)
  dev.off()
  extrafont::embed_fonts(filename%++%"GMM-NEC-TotalSeeds.pdf")
  
  pdf(filename%++%"GMM-NEC-sorted.pdf", width=10, height=5)
  plot(sort(NEC.mat[,wh.best[1]]), type='l', ylab="Normalized Entropy Criterion")
  dev.off()
  
  for(ii in rev(1:3)){
    wh.best.ii <- t(NEC.mat) %>% {which(.==sort(.)[ii], arr.ind=TRUE)}
    set.seed(wh.best.ii[2])
    fit.best.ii <- Mclust(Xnew, G=wh.best.ii[1], modelNames="VVV")
    fit.best.ii$seed <- wh.best.ii[2]
    
    Xclust.ii <- cbind.data.frame(Xnew, class=fit.best.ii$classification %>% as.factor)
    attr(Xclust.ii, "seed") <- wh.best.ii[2]
    
    Xclust.boxplot.filename(Xclust.ii, wh.best=wh.best.ii, filename=filename%++%"GMM-Boxplot_top"%++%ii)
  }
  
  result <- NULL
  result$ID <- ID
  result$Xnew <- Xnew
  result$NEC.mat <- NEC.mat
  result$fit.best <- fit.best.ii
  result$wh.best <- wh.best.ii
  result$Xclust <- Xclust.ii
  
  return(result)
}





#' @title Analyze Clustering Stability using Normalized Variation of Information (NVI)
#' @description For a fixed number of clusters, this function assesses the stability of
#' clustering results from different random initializations using NVI.
#' @import mclust
#' @import aricode
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot points abline axis
#'
#' @details
#' After determining a candidate for the optimal number of clusters (`nclust`), this function evaluates the stability of the clustering solution. It selects the top `top` runs (i.e., seeds) for that `nclust` based on the lowest NEC values from `NEC.mat`.
#'
#' It then computes the pairwise Normalized Variation of Information (NVI) between the partitions from these top runs. NVI is a measure of distance between two clusterings, with lower values indicating higher similarity.
#'
#' The function calculates the average NVI for each run against all other top runs. The run with the minimum average NVI is considered the most stable and representative clustering solution.
#'
#' It generates plots to visualize the stability analysis:
#' 1. A line plot of the averaged NVI values for each tested seed.
#' 2. Boxplots for the clusterings, ordered by their stability (from most to least stable).
#'
#' @param Xnew The numeric matrix or data frame of observations.
#' @param nclust The specific number of clusters to analyze for stability.
#' @param NEC.mat The matrix of NEC values produced by `analysis.GMM.best`.
#' @param top The number of top-performing runs (by NEC) to compare. Default is 20.
#' @param filename A base string for output PDF file names.
#' @return A list containing:
#' \item{VI_matrix}{The pairwise NVI matrix between the top runs.}
#' \item{Xclust.NVI}{The input data with an appended `class` column from the most stable model.}
#' \item{fit.best.NVI}{The `Mclust` object for the most stable model.}
#' \item{wh.best.NVI}{A vector with the number of clusters and the seed of the most stable model.}
#'
#' @seealso \code{\link{analysis.GMM.best}}
#' 
#' @export analysis.NVI
analysis.NVI <- function(Xnew, nclust, NEC.mat, top=20, filename=NULL){
  library(aricode)
  
  rank.NEC <- order(NEC.mat[,nclust])[1:top]
  
  fit.best.list <- NULL
  count=0
  for( seed in rank.NEC ){
    count=count+1
    set.seed(seed)
    fit.best.list[[count]] <- Mclust(Xnew, G=nclust, modelNames="VVV")
  }
  class.list <- lapply(fit.best.list, function(x) x$classification)
  
  
  {
    
    num_runs <- length(class.list)
    VI_matrix <- matrix(0, nrow = num_runs, ncol = num_runs)
    for(i in 1:num_runs) {
      for(j in i:num_runs) {
        VI_val <- aricode::NVI(class.list[[i]], class.list[[j]])
        VI_matrix[i,j] <- VI_val
        VI_matrix[j,i] <- VI_val
      }
    }
    
    mean_NVI <- rowMeans(VI_matrix)
    best_run_NVI <- order(mean_NVI, decreasing=FALSE)
    cat("The most stable clustering result is occured at seed", rank.NEC[best_run_NVI[1]], "\n")
    
    pdf(filename%++%"NVI_lineplot.pdf", height=4, width=10)
    plot(mean_NVI, type="l", xaxt="n", xlab="Seed number", ylab="Averaged NVI")
    points(which.min(mean_NVI), min(mean_NVI), col="red", pch=18)
    abline(v=which.min(mean_NVI), col="red", lty=2)
    axis(1, at=1:top, labels=rank.NEC)
    dev.off()
    
    
    rank.NEC[best_run_NVI]
    
    {
      for(h in 1:length(class.list)){
        fit.h <- fit.best.list[[ best_run_NVI[h] ]]
        Xclust.h <- cbind.data.frame(Xnew, class=fit.h$classification %>% as.factor)
        seed = rank.NEC[ best_run_NVI[h] ]
        Xclust.boxplot.filename(Xclust=Xclust.h,
                                wh.best=c(nclust,seed),
                                filename=filename%++%"NVI"%++%length(class.list)%++%"-"%++%h)
      }
    }
  }
  
  
  {
    
    fit.best.NVI <- fit.best.list[[ best_run_NVI[1] ]]
    fit.best.NVI$seed <- rank.NEC[ best_run_NVI[1] ]
    
    Xclust.NVI <- cbind.data.frame(Xnew, class=fit.best.NVI$classification %>% as.factor)
    attr(Xclust.NVI, "seed") <- fit.best.NVI$seed
    
    Xclust.boxplot.filename(Xclust.NVI, wh.best=c(nclust, fit.best.NVI$seed), 
                            filename=filename%++%"NVI-best-")
    
    
  }
  
  
  result <- NULL
  result$VI_matrix <- VI_matrix
  result$Xclust.NVI <- Xclust.NVI
  result$fit.best.NVI <- fit.best.NVI
  result$wh.best.NVI <- c(nclust, rank.NEC[ best_run_NVI[1] ])
  
  result
}
