#' @export run_factor_analysis
run_factor_analysis <- function(df, variables, nfactors = 5) {
  data_matrix <- as.matrix(df[, variables])
  fa_result <- stats::factanal(data_matrix, factors = nfactors, scores = "regression")
  return(fa_result)
}


#' @export run_gmm_analysis
run_gmm_analysis <- function(data_matrix, max_clusters = 10, max_seed = 100, modelNames = "VVV") {
  best_fit <- NULL
  best_bic <- -Inf
  for(seed in 1:max_seed) {
    set.seed(seed)
    fit <- mclust::Mclust(data_matrix, G = 1:max_clusters, modelNames = modelNames)
    if(fit$bic > best_bic) {
      best_bic <- fit$bic
      best_fit <- fit
    }
  }
  return(best_fit)
}





#' @export calculate_nec
calculate_nec <- function(model, base_loglik) {
  probs <- model$z
  eps <- 1e-10
  probs <- pmax(probs, eps)
  entropy <- -sum(probs * log(probs))
  
  loglik_diff <- model$loglik - base_loglik
  nec <- entropy / (loglik_diff)
  
  return(nec)
}



#' @export
main.NEC <- function(Xnew = NULL, ID = NULL, max.clust = 10, seed.seq = 1:100, title = NULL, seed,
                     sampling_prop = 1, print.pdf = TRUE, plot.prop = 1.0, cex = 0.4) {
  
  library(mclust) # for GMM
  
  result <- list()
  for (seed in seed.seq) {
    fit.mclust <- list()
    for (i in 1:max.clust) {
      set.seed(seed)
      fit.mclust[[i]] <- Mclust(Xnew, G = i, modelNames = "VVV")
    }
    base_loglik <- fit.mclust[[1]]$loglik
    nec.vec <- sapply(fit.mclust, calculate_nec, base_loglik = base_loglik)
    n.clust <- which.min(nec.vec)
    
    print(nec.vec)
    
    pdf(file = paste0(title, "-GMM-NEC-seed=", seed, ".pdf"), width = 6, height = 4)
    plot(nec.vec, type = "b", xlab = "The number of clusters", ylab = "Normalized Entropy Criterion")
    dev.off()
    extrafont::embed_fonts(paste0(title, "-GMM-NEC-seed=", seed, ".pdf"))
    
    result[[as.character(seed)]] <- fit.mclust
  }
  
  return(result)
}
