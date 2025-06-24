#' #' Extract and prepare factor data for analysis
#' #' 
#' #' This function extracts personality factor items from a dataframe based on specified factor names,
#' #' removes rows with missing values, and returns a structured result.
#' #' 
#' #' @param df A dataframe containing personality items and ID column
#' #' @param FactorNames A character vector of factor names to extract (e.g., c("O","C","E","A","N"))
#' #' @param title A string used for labeling outputs (default: "title")
#' #' @return A list containing ID vector and factor data matrix
#' #' @export
#' main.Data <- function(df, FactorNames, title = "title") {
#'   # Create pattern to match factor item names (e.g., O1, C2, E3, etc.)
#'   FactorNamesPatterns <- paste0("^(", paste0(FactorNames, collapse = "|"), ")[0-9]+$")
#'   
#'   # Extract factor items and remove rows with NA values
#'   X <- df %>% 
#'     dplyr::select(matches(FactorNamesPatterns)) %>% 
#'     drop_na()
#'   
#'   # Print summary information
#'   cat("Dimension:", dim(X)[1], "observations,", dim(X)[2], "variables\n")
#'   cat("First 2 rows (preview):\n")
#'   print(head(X[, 1:min(15, ncol(X))], 2))
#'   
#'   # Count the number of items per factor
#'   factor_counts <- table(gsub("[0-9]", "", colnames(X)))
#'   cat("Items per factor:\n")
#'   print(factor_counts)
#'   
#'   # Get IDs
#'   ID <- df$ID
#'   
#'   # Return results
#'   return(list(
#'     ID = ID,
#'     X = X
#'   ))
#' }
#' 
#' #' Calculate Normalized Entropy Criterion (NEC)
#' #' 
#' #' This function calculates the NEC for a clustering model.
#' #' Lower NEC values indicate better clustering solutions.
#' #' 
#' #' @param model A clustering model (e.g., from mclust)
#' #' @param base_loglik Base log-likelihood value (typically from 1-cluster model)
#' #' @return NEC value
#' #' @export
#' calculate_nec <- function(model, base_loglik) {
#'   # Get probabilities
#'   probs <- model$z
#'   
#'   # Avoid log(0)
#'   eps <- 1e-10
#'   probs <- pmax(probs, eps)
#'   
#'   # Calculate entropy
#'   entropy <- -sum(probs * log(probs))
#'   
#'   # Calculate NEC
#'   loglik_diff <- model$loglik - base_loglik
#'   nec <- entropy / loglik_diff
#'   
#'   return(nec)
#' }
#' 
#' #' Perform Factor Analysis
#' #' 
#' #' This function performs factor analysis on the provided data and returns
#' #' factor scores and related information.
#' #' 
#' #' @param X Data matrix
#' #' @param ID Vector of IDs
#' #' @param nfactors Number of factors to extract (if NULL, determined automatically)
#' #' @param nfactors2 Number of second-order factors (if NULL, no hierarchical FA is performed)
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @param title Title for output files
#' #' @return A list containing FA results
#' #' @export
#' main.FA <- function(X, ID, nfactors = NULL, nfactors2 = NULL, output_dir = "./output", title = "analysis") {
#'   # Load required packages
#'   requireNamespace("psych", quietly = TRUE)
#'   requireNamespace("car", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Capture console output
#'   console_output <- capture.output({
#'     # Check the availability of Factor Analysis
#'     cat("--------Check the availability of Factor Analysis--------\n")
#'     
#'     cat("--------(1) Kaiser-Meyer-Olkin (KMO) index--------\n")
#'     fit.KMO <- psych::KMO(cor(X))
#'     cat("> fit.KMO$MSA\n")
#'     cat(paste0(fit.KMO$MSA, " (≥ 0.6)\n"))
#'     
#'     cat("--------(2) Bartlett's test--------\n")
#'     fit.bartlett <- psych::cortest.bartlett(R = cor(X), n = nrow(X))
#'     cat("> fit.bartlett$p.value\n")
#'     cat(paste0(fit.bartlett$p.value, " (< 0.05)\n"))
#'     
#'     # Determine number of factors
#'     cat("--------Choose the number of factors--------\n")
#'     parallel <- psych::fa.parallel(X, fa = "fa", sim = FALSE, se.bars = FALSE, plot = FALSE)
#'     
#'     # Save scree plot
#'     pdf(file = file.path(output_dir, paste0(title, "-FA-ScreePlot.pdf")), width = 8, height = 4)
#'     plot(parallel, fa = "pc", sim = FALSE, se.bars = FALSE)
#'     dev.off()
#'     
#'     pdf(file = file.path(output_dir, paste0(title, "-FA-ScreePlot-simple.pdf")), width = 8, height = 4)
#'     plot(1:length(parallel$pc.values[1:30]), parallel$pc.values[1:30], 
#'          type = "b", xlab = "The number of factors", ylab = "Eigenvalues of principal factors")
#'     dev.off()
#'     
#'     # Determine number of factors if not specified
#'     if (is.null(nfactors)) {
#'       nfactors <- max(which(parallel$fa.values > 1))
#'     }
#'     
#'     cat("> nfactors\n")
#'     print(nfactors)
#'     
#'     # Perform factor analysis
#'     cat("--------A result of factor analysis--------\n")
#'     fit.fa <- psych::fa(X, nfactors = nfactors, rotate = "oblimin", scores = "regression", fm = "pa")
#'     
#'     # Save factor diagram
#'     pdf(file = file.path(output_dir, paste0(title, "-FA-Diagram.pdf")), width = 5, height = 18)
#'     psych::fa.diagram(fit.fa, sort = FALSE)
#'     dev.off()
#'     
#'     cat("> fit.fa\n")
#'     print(fit.fa)
#'     
#'     cat("> fit.fa$loadings\n")
#'     sparse.loading <- ifelse(abs(fit.fa$loadings) < 0.3, 0, fit.fa$loadings)
#'     print(sparse.loading)
#'     
#'     # Get factor names
#'     FactorNames <- apply(sparse.loading, 2, function(x) {
#'       names(which(x > 0)) %>% 
#'         gsub("[0-9]+", "", .) %>% 
#'         table() %>% 
#'         which.max() %>% 
#'         names()
#'     })
#'     
#'     Xnew <- fit.fa$scores
#'     colnames(Xnew) <- FactorNames
#'     
#'     # Second-order factor analysis if requested
#'     if (!is.null(nfactors2)) {
#'       fit.multi <- psych::fa.multi(X, nfactors, nfactors2, 
#'                                    scores = "regression", 
#'                                    rotate = "oblimin")
#'       
#'       parallel2 <- psych::fa.parallel(fit.multi$f1$scores, fa = "fa", 
#'                                       sim = FALSE, se.bars = FALSE, plot = FALSE)
#'       
#'       pdf(file = file.path(output_dir, paste0(title, "-FA-SecondOrder-ScreePlot.pdf")), 
#'           width = 10, height = 5)
#'       plot(parallel2, fa = "pc", sim = FALSE, se.bars = FALSE, show.legend = TRUE)
#'       dev.off()
#'       
#'       pdf(file = file.path(output_dir, paste0(title, "-FA-SecondOrder.pdf")), 
#'           width = 6, height = 12)
#'       psych::fa.multi.diagram(fit.multi, cut = 0.29)
#'       dev.off()
#'       
#'       cat("> fit.multi\n")
#'       print(fit.multi)
#'       
#'       cat("> fit.multi$f2$loadings\n")
#'       fit.multi.loadings <- ifelse(abs(fit.multi$f2$loadings) < 0.29, 0, fit.multi$f2$loadings)
#'       print(fit.multi.loadings)
#'       
#'       row.names(fit.multi.loadings) <- FactorNames
#'       
#'       FactorNames2 <- apply(fit.multi.loadings, 2, function(x) {
#'         names(which(x > 0)) %>% 
#'           gsub("[0-9]+", "", .) %>% 
#'           paste0(collapse = "")
#'       })
#'       
#'       Xnew <- with(list(X = fit.multi$f1$scores, loadings = fit.multi$f2$loadings), {
#'         scale(X) %*% (solve(cor(X)) %*% loadings)
#'       })
#'       
#'       colnames(Xnew) <- FactorNames2
#'     }
#'   }, type = "output")
#'   
#'   # Return results
#'   result <- list(
#'     ID = ID,
#'     X = X,
#'     Xnew = Xnew,
#'     console = paste(console_output, collapse = "\n")
#'   )
#'   
#'   return(result)
#' }
#' 
#' #' Run Gaussian Mixture Model with NEC for cluster selection
#' #' 
#' #' This function fits GMMs with varying numbers of clusters and evaluates them using NEC.
#' #' 
#' #' @param Xnew Data matrix (typically factor scores)
#' #' @param ID Vector of IDs (optional)
#' #' @param max.clust Maximum number of clusters to try
#' #' @param seed.seq Sequence of seeds to try
#' #' @param title Title for output files
#' #' @param sampling_prop Proportion of data to sample (default: 1)
#' #' @param print.pdf Whether to print PDF outputs (default: TRUE)
#' #' @param plot.prop Proportion of data to plot (default: 1.0)
#' #' @param cex Point size for plotting (default: 0.4)
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list of GMM fitting results
#' #' @export
#' main.NEC <- function(Xnew = NULL, ID = NULL, max.clust = 10, seed.seq = 1:100, 
#'                      title = "analysis", sampling_prop = 1, print.pdf = TRUE, 
#'                      plot.prop = 1.0, cex = 0.4, output_dir = "./output") {
#'   # Load required package
#'   requireNamespace("mclust", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   result <- NULL
#'   for (seed in seed.seq) {
#'     fit.mclust <- list()
#'     for (i in 1:max.clust) {
#'       set.seed(seed)
#'       fit.mclust[[i]] <- mclust::Mclust(Xnew, G = i, modelNames = "VVV")
#'     }
#'     
#'     base_loglik <- fit.mclust[[1]]$loglik
#'     nec.vec <- sapply(fit.mclust, calculate_nec, base_loglik = base_loglik)
#'     n.clust <- which.min(nec.vec)
#'     
#'     print(nec.vec)
#'     
#'     pdf_path <- file.path(output_dir, paste0(title, "-GMM-NEC-seed=", seed, ".pdf"))
#'     pdf(file = pdf_path, width = 6, height = 4)
#'     plot(nec.vec, type = "b", xlab = "The number of clusters", 
#'          ylab = "Normalized Entropy Criterion")
#'     dev.off()
#'     
#'     if (requireNamespace("extrafont", quietly = TRUE)) {
#'       extrafont::embed_fonts(pdf_path)
#'     }
#'     
#'     result[[as.character(seed)]] <- fit.mclust
#'   }
#'   
#'   return(result)
#' }
#' 
#' #' Run Gaussian Mixture Model analysis
#' #' 
#' #' This function performs GMM analysis on the provided data and returns
#' #' clustering results.
#' #' 
#' #' @param Xnew Data matrix (typically factor scores)
#' #' @param ID Vector of IDs (optional)
#' #' @param max.clust Maximum number of clusters to try
#' #' @param title Title for output files
#' #' @param seed Random seed for reproducibility
#' #' @param model_name GMM model name (default: "VVV")
#' #' @param sampling_prop Proportion of data to sample (default: 1)
#' #' @param print.pdf Whether to print PDF outputs (default: TRUE)
#' #' @param plot.prop Proportion of data to plot (default: 1.0)
#' #' @param cex Point size for plotting (default: 0.4)
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list containing GMM results
#' #' @export
#' main.GMM <- function(Xnew = NULL, ID = NULL, max.clust = 10, title = "analysis", 
#'                      seed = 123, model_name = "VVV", sampling_prop = 1, 
#'                      print.pdf = TRUE, plot.prop = 1.0, cex = 0.4, 
#'                      output_dir = "./output") {
#'   # Load required package
#'   requireNamespace("mclust", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Fit models with different numbers of clusters
#'   fit.mclust <- list()
#'   for (i in 1:max.clust) {
#'     set.seed(seed)
#'     fit.mclust[[i]] <- mclust::Mclust(Xnew, G = i, modelNames = model_name)
#'   }
#'   
#'   # Calculate NEC values
#'   base_loglik <- fit.mclust[[1]]$loglik
#'   nec.vec <- sapply(fit.mclust, calculate_nec, base_loglik = base_loglik)
#'   n.clust <- which.min(nec.vec)
#'   
#'   print(nec.vec)
#'   
#'   # Plot NEC values
#'   pdf_path <- file.path(output_dir, paste0(title, "-GMM-NEC.pdf"))
#'   pdf(file = pdf_path, width = 6, height = 4)
#'   plot(nec.vec, type = "b", xlab = "The number of clusters", 
#'        ylab = "Normalized Entropy Criterion")
#'   dev.off()
#'   
#'   if (requireNamespace("extrafont", quietly = TRUE)) {
#'     extrafont::embed_fonts(pdf_path)
#'   }
#'   
#'   # Use the best model
#'   fit.GMM <- fit.mclust[[n.clust]]
#'   Xclust <- data.frame(Xnew, class = as.factor(fit.GMM$classification))
#'   
#'   # Create result object
#'   result <- list(
#'     Xnew = Xnew,
#'     nec.vec = nec.vec,
#'     n.clust = n.clust,
#'     fit.GMM = fit.GMM,
#'     Xclust = Xclust
#'   )
#'   
#'   # Display model summary
#'   cat("> summary(fit.GMM, parameters = TRUE)$mean\n")
#'   print(summary(fit.GMM, parameters = TRUE)$mean)
#'   
#'   # Plot classification if requested
#'   if (print.pdf) {
#'     # Reduce data for plotting if requested
#'     fit.GMM.reduced <- fit.GMM
#'     if (plot.prop < 1.0) {
#'       samp <- sample(nrow(fit.GMM$data), floor(nrow(fit.GMM$data) * plot.prop))
#'       fit.GMM.reduced$data <- fit.GMM$data[samp, ]
#'       fit.GMM.reduced$n <- floor(plot.prop * fit.GMM.reduced$n)
#'       fit.GMM.reduced$classification <- fit.GMM.reduced$classification[samp]
#'     }
#'     
#'     # Create classification plot
#'     png_path <- file.path(output_dir, paste0(title, "-GMM-Class.png"))
#'     png(filename = png_path, res = 500, width = 8, height = 4, units = "in")
#'     plot(fit.GMM.reduced, what = "classification", cex = cex)
#'     dev.off()
#'   } else {
#'     # Create pairs plot with custom symbols and colors
#'     SYMBOLS <- c(16, 0, 17, 3, 4, 1, 8, 2, 7, 5, 9, 6, 10, 11, 18, 12, 13, 14)
#'     COLORS <- c("dodgerblue2", "red3", "green3", "slateblue", "darkorange", 
#'                 "skyblue1", "violetred4", "forestgreen", "steelblue4", "slategrey", 
#'                 "brown", "black", "darkseagreen", "darkgoldenrod3", "olivedrab", 
#'                 "royalblue", "tomato4", "cyan2", "springgreen2")
#'     
#'     png_path <- file.path(output_dir, paste0(title, "-GMM-Class.png"))
#'     png(filename = png_path, res = 300, width = 8, height = 4, units = "in")
#'     pairs(as.data.frame(fit.GMM$data),
#'           cex = cex,
#'           pch = SYMBOLS[fit.GMM$classification],
#'           col = COLORS[fit.GMM$classification])
#'     dev.off()
#'   }
#'   
#'   return(result)
#' }
#' 
#' #' Run Gaussian Mixture Model with multiple seeds to find best model
#' #' 
#' #' This function runs GMMs with various seeds and numbers of clusters to 
#' #' find the most stable and optimal clustering solution.
#' #' 
#' #' @param Xnew Data matrix (typically factor scores)
#' #' @param ID Vector of IDs (optional)
#' #' @param max.clust Maximum number of clusters to try
#' #' @param title Title for output files
#' #' @param max.seed Maximum number of seeds to try
#' #' @param model_name GMM model name (default: "VVV")
#' #' @param sampling_prop Proportion of data to sample (default: 1)
#' #' @param print.pdf Whether to print PDF outputs (default: TRUE)
#' #' @param plot.prop Proportion of data to plot (default: 1.0)
#' #' @param cex Point size for plotting (default: 0.4)
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list containing GMM results
#' #' @export
#' main.GMM.best <- function(Xnew = NULL, ID = NULL, max.clust = 10, title = "analysis", 
#'                           max.seed = 100, model_name = "VVV", sampling_prop = 1, 
#'                           print.pdf = TRUE, plot.prop = 1.0, cex = 0.4,
#'                           output_dir = "./output") {
#'   # Load required package
#'   requireNamespace("mclust", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Initialize matrix to store NEC values
#'   NEC.mat <- matrix(NA, max.seed, max.clust)
#'   
#'   # Fit models with different seeds and numbers of clusters
#'   for (j in 1:max.seed) {
#'     for (i in 1:max.clust) {
#'       set.seed(j)
#'       fit.mclust <- mclust::Mclust(Xnew, G = i, modelNames = model_name)
#'       
#'       if (i == 1) {
#'         base_loglik <- fit.mclust$loglik
#'       } else if (i > 1) {
#'         nec.vec <- calculate_nec(model = fit.mclust, base_loglik = base_loglik)
#'         NEC.mat[j, i] <- nec.vec
#'       }
#'     }
#'     
#'     if (j %% 10 == 0) {
#'       cat("Completed", j, "of", max.seed, "seeds\n")
#'     }
#'   }
#'   
#'   # Use NEC.mat without removing any rows
#'   NEC.mat2 <- NEC.mat
#'   
#'   # Find best solution
#'   wh.best <- which(t(NEC.mat2) == min(t(NEC.mat2), na.rm = TRUE), arr.ind = TRUE)
#'   
#'   # Plot mean NEC curve
#'   pdf_path1 <- file.path(output_dir, paste0(title, "-GMM-NEC-TotalSeeds-MeanCurve.pdf"))
#'   pdf(pdf_path1, height = 4, width = 8)
#'   matplot(cbind(rowMeans(t(NEC.mat2)), t(NEC.mat2)), type = "l", 
#'           lty = c(1, rep(2, ncol(t(NEC.mat2)))), 
#'           col = c("red", rep("gray50", ncol(t(NEC.mat2)))), 
#'           ylab = "Normalized Entropy Criterion", 
#'           xlab = "The number of clusters")
#'   dev.off()
#'   
#'   # Plot all NEC curves
#'   pdf_path2 <- file.path(output_dir, paste0(title, "-GMM-NEC-TotalSeeds.pdf"))
#'   pdf(pdf_path2, height = 4, width = 8)
#'   matplot(t(NEC.mat2), type = "l", 
#'           lty = rep(2, ncol(t(NEC.mat2))), 
#'           col = rep("gray50", ncol(t(NEC.mat2))), 
#'           ylab = "Normalized Entropy Criterion", 
#'           xlab = "The number of clusters")
#'   points(wh.best[1], t(NEC.mat2)[wh.best[1], wh.best[2]], col = "red", pch = 18)
#'   abline(v = wh.best[1], col = "red", lty = 2)
#'   dev.off()
#'   
#'   if (requireNamespace("extrafont", quietly = TRUE)) {
#'     extrafont::embed_fonts(pdf_path1)
#'     extrafont::embed_fonts(pdf_path2)
#'   }
#'   
#'   # Fit best model
#'   set.seed(wh.best[2])
#'   fit.best <- mclust::Mclust(Xnew, G = wh.best[1], modelNames = model_name)
#'   
#'   # Create result data frame
#'   Xclust <- data.frame(Xnew, class = as.factor(fit.best$classification))
#'   
#'   # Create result object
#'   result <- list(
#'     Xnew = Xnew,
#'     ID = ID,
#'     NEC.mat = NEC.mat,
#'     fit.best = fit.best,
#'     wh.best = wh.best,
#'     Xclust = Xclust
#'   )
#'   
#'   return(result)
#' }
#' 
#' #' Create boxplot of GMM clustering results
#' #' 
#' #' This function creates boxplots to visualize the factor scores within each cluster.
#' #' 
#' #' @param res.GMM.best Result from main.GMM.best function
#' #' @param title Title for output files
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return None (creates a boxplot file)
#' #' @export
#' main.GMM.boxplot <- function(res.GMM.best, title = "analysis", output_dir = "./output") {
#'   # Extract data
#'   Xclust <- res.GMM.best$Xclust
#'   wh.best <- res.GMM.best$wh.best
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Load required packages
#'   requireNamespace("ggpubr", quietly = TRUE)
#'   requireNamespace("tidyr", quietly = TRUE)
#'   requireNamespace("dplyr", quietly = TRUE)
#'   
#'   # Prepare data for plotting
#'   Xclust.df <- tidyr::gather(Xclust, "Factors", "value", -class) %>%
#'     dplyr::mutate(
#'       class = as.factor(class),
#'       Factors = factor(Factors, levels = colnames(Xclust)[colnames(Xclust) != "class"])
#'     )
#'   
#'   # Create boxplot
#'   p <- ggpubr::ggboxplot(
#'     Xclust.df,
#'     x = "Factors", 
#'     y = "value", 
#'     fill = "Factors", 
#'     palette = "jco",
#'     outlier.shape = 3, 
#'     bxp.errorbar = TRUE,
#'     xlab = "Factors", 
#'     ylab = "Factor Scores", 
#'     legend = "none"
#'   ) %>%
#'     ggpubr::facet(facet.by = "class", nrow = 1)
#'   
#'   # Save plot
#'   pdf_path <- file.path(output_dir, paste0(title, "-GMM.best-Boxplot-seed=", 
#'                                            wh.best[2], ",nclust=", wh.best[1], ".pdf"))
#'   ggplot2::ggsave(filename = pdf_path, plot = p, height = 3, width = 10)
#' }
#' 
#' #' Create boxplot of clustering results with custom parameters
#' #' 
#' #' This function creates boxplots with more control over sampling and appearance.
#' #' 
#' #' @param Xclust Data frame with cluster assignments
#' #' @param wh.best Best cluster parameters
#' #' @param title Title for output files
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return None (creates a boxplot file)
#' #' @export
#' main.GMM.boxplot2 <- function(Xclust, wh.best, title = "analysis", output_dir = "./output") {
#'   # Load required packages
#'   requireNamespace("tidyr", quietly = TRUE)
#'   requireNamespace("ggpubr", quietly = TRUE)
#'   requireNamespace("dplyr", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Prepare data for plotting with random sampling
#'   Xclust.df <- dplyr::slice_sample(Xclust, prop = 1) %>%
#'     tidyr::gather("Factors", "value", -class) %>%
#'     dplyr::mutate(
#'       class = as.factor(class),
#'       Factors = factor(Factors, levels = colnames(Xclust)[colnames(Xclust) != "class"])
#'     )
#'   
#'   # Create boxplot
#'   p <- ggpubr::ggboxplot(
#'     Xclust.df,
#'     x = "Factors", 
#'     y = "value", 
#'     fill = "Factors", 
#'     palette = "jco",
#'     outlier.shape = 3, 
#'     bxp.errorbar = TRUE,
#'     xlab = "Factors", 
#'     ylab = "Factor Scores", 
#'     legend = "none"
#'   ) %>%
#'     ggpubr::facet(facet.by = "class", nrow = 1)
#'   
#'   # Save plot
#'   pdf_path <- file.path(output_dir, paste0(title, "-Boxplot-seed=", 
#'                                            wh.best[2], ",nclust=", wh.best[1], ".pdf"))
#'   ggplot2::ggsave(filename = pdf_path, plot = p, height = 3, width = 10)
#' }
#' 
#' #' Reassign non-significant clusters to significant ones
#' #' 
#' #' This function reassigns items from non-significant clusters to their nearest
#' #' significant cluster based on distance to cluster centers.
#' #' 
#' #' @param Xnew Data matrix
#' #' @param class Vector of class assignments
#' #' @param mod Clustering model
#' #' @param wh.class Vector of significant cluster IDs
#' #' @return Updated class assignments
#' #' @export
#' reassign.enrichment <- function(Xnew, class, mod, wh.class) {
#'   # If all classes are already significant, return original classes
#'   if (all(class %in% wh.class)) return(class)
#'   
#'   # Extract data for non-significant classes
#'   Xnew_subset <- Xnew[!class %in% wh.class, ]
#'   
#'   # Get significant cluster centers
#'   centre <- t(mod$parameters$mean)[wh.class, ]
#'   rownames(centre) <- wh.class
#'   
#'   # Initialize distance matrix
#'   n <- nrow(Xnew_subset)
#'   nclust <- nrow(centre)
#'   dist.mat <- matrix(NA, n, nclust)
#'   
#'   # Calculate distances to each significant cluster
#'   for (j in 1:n) {
#'     for (i in 1:nclust) {
#'       dist.mat[j, i] <- sqrt(sum((Xnew_subset[j, ] - centre[i, ])^2))  # Euclidean distance
#'     }
#'   }
#'   
#'   # Assign to nearest significant cluster
#'   ClusterAssignment <- wh.class[apply(dist.mat, 1, which.min)]
#'   
#'   # Update class assignments
#'   class2 <- class
#'   class2[!class2 %in% wh.class] <- ClusterAssignment
#'   
#'   # Print table of reassigned classes
#'   print(table(class2))
#'   
#'   return(class2)
#' }
#' 
#' #' Perform enrichment analysis for clustering solutions
#' #' 
#' #' This function evaluates the significance of clusters by comparing observed
#' #' density with permutation-based null distribution.
#' #' 
#' #' @param mod Clustering model
#' #' @param BIC BIC object (optional)
#' #' @param n.clust Number of clusters
#' #' @param nrep Number of permutations
#' #' @param seed Random seed
#' #' @return A list with enrichment results
#' #' @export
#' fit.enrichment <- function(mod, BIC = NULL, n.clust = NULL, nrep = 100, seed = 123) {
#'   # Require mclust package
#'   requireNamespace("mclust", quietly = TRUE)
#'   
#'   # Extract model parameters
#'   centre <- mod$parameters$mean
#'   params <- mod$parameters
#'   modelName <- mod$modelName
#'   data <- mod$data
#'   
#'   # Calculate observed density
#'   rho_obs <- mclust::dens(modelName = modelName, data = t(centre), parameters = params)
#'   
#'   # Initialize results list
#'   out.result <- vector("list", nrep)
#'   
#'   # Perform permutations
#'   for (iii in 1:nrep) {
#'     if (iii %% 10 == 0) {
#'       cat("Completed", iii, "of", nrep, "permutations\n")
#'     }
#'     
#'     # Create permuted data
#'     set.seed(iii)
#'     Xperm <- matrix(sample(data), nrow(data), ncol(data))
#'     
#'     # Fit model to permuted data
#'     set.seed(seed)
#'     if (!is.null(BIC)) {
#'       mod.perm2 <- mclust::Mclust(Xperm, x = BIC, modelNames = modelName, verbose = FALSE)
#'     } else {
#'       mod.perm2 <- mclust::Mclust(Xperm, G = n.clust, modelNames = modelName, verbose = FALSE)
#'     }
#'     
#'     # Calculate density for permuted data
#'     out.result[[iii]] <- mclust::dens(
#'       modelName = modelName,
#'       data = t(centre),
#'       parameters = mod.perm2$parameters
#'     )
#'   }
#'   
#'   # Combine permutation results
#'   rho_perm <- do.call("rbind", out.result)
#'   
#'   # Calculate relative enrichment
#'   TMP <- matrix(replicate(nrow(rho_perm), rho_obs), ncol = nrow(rho_perm))
#'   rho_rel <- colMeans(t(TMP) / rho_perm)
#'   
#'   # Calculate p-values
#'   pvalues <- apply(t(TMP) / rho_perm, 2, function(x) {
#'     (sum(x < 1) + 1) / (length(x) + 1)
#'   })
#'   
#'   # Return results
#'   return(list(
#'     rho_obs = rho_obs,
#'     rho_perm = rho_perm,
#'     rho_rel = rho_rel,
#'     pvalues = pvalues
#'   ))
#' }
#' 
#' #' Perform enrichment analysis for clustering solutions
#' #' 
#' #' This function evaluates the significance of clusters and saves visualizations.
#' #' 
#' #' @param fit.GMM GMM model (from Mclust)
#' #' @param Xclust Data frame with cluster assignments
#' #' @param Xnew Data matrix
#' #' @param title Title for output files
#' #' @param seed Random seed
#' #' @param n.clust Number of clusters
#' #' @param nrep Number of permutations
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list with enrichment results
#' #' @export
#' main.Enrichment <- function(fit.GMM, Xclust, Xnew, title = "analysis", seed = 123, 
#'                             n.clust = NULL, nrep = 100, output_dir = "./output") {
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Set seed for reproducibility
#'   set.seed(1)
#'   
#'   # Perform enrichment analysis
#'   res.enrichment <- fit.enrichment(
#'     mod = fit.GMM, 
#'     n.clust = n.clust, 
#'     nrep = nrep, 
#'     seed = seed
#'   )
#'   
#'   # Plot and save the enrichment plot
#'   pdf_path <- file.path(output_dir, paste0(title, "-Enrichment.pdf"))
#'   pdf(file = pdf_path, width = 8, height = 4)
#'   
#'   with(res.enrichment, {
#'     plot(rho_rel, log10(pvalues), pch = 18, cex = 1.2, 
#'          xlab = bquote("Enrichment, " ~ rho/tilde(rho)), 
#'          ylab = bquote(log[10] ~ "p-value"), 
#'          ylim = c(1.1 * min(log10(res.enrichment$pvalues)), 0))
#'     text(x = rho_rel, y = 0.2, labels = 1:length(rho_rel), cex = 0.5, xpd = NA)
#'   })
#'   
#'   dev.off()
#'   
#'   # Identify significant clusters
#'   SigCluster <- with(res.enrichment, {
#'     order(rho_rel, decreasing = TRUE)[sort(pvalues, decreasing = FALSE) < 0.05]
#'   })
#'   
#'   cat("Significant Clusters:\n")
#'   print(SigCluster)
#'   
#'   # Create result object
#'   result <- list(
#'     Xnew = Xnew,
#'     Xclust2 = Xclust,
#'     fit.GMM = fit.GMM,
#'     res.enrichment = res.enrichment,
#'     SigCluster = SigCluster
#'   )
#'   
#'   return(result)
#' }
#' 
#' #' Perform enrichment analysis for a range of cluster numbers
#' #' 
#' #' This function evaluates enrichment for different numbers of clusters
#' #' to find the optimal clustering solution.
#' #' 
#' #' @param Xnew Data matrix
#' #' @param n.clust Maximum number of clusters to try
#' #' @param nrep Number of permutations
#' #' @param seed Random seed
#' #' @param model_name GMM model name (default: "VVV")
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list of enrichment results for each number of clusters
#' #' @export
#' main.Enrichment.range <- function(Xnew = NULL, n.clust = 10, nrep = 100, seed = 123, 
#'                                   model_name = "VVV", output_dir = "./output") {
#'   # Require mclust package
#'   requireNamespace("mclust", quietly = TRUE)
#'   
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Initialize results list
#'   results <- vector("list", n.clust)
#'   
#'   # Loop through different numbers of clusters
#'   for (i in 2:n.clust) {  # Start from 2 clusters
#'     cat("Analyzing", i, "clusters...\n")
#'     
#'     # Fit GMM model
#'     set.seed(seed)
#'     fit.GMM <- mclust::Mclust(Xnew, G = i, modelNames = model_name)
#'     
#'     # Perform enrichment analysis
#'     results[[i]] <- fit.enrichment(
#'       mod = fit.GMM,
#'       n.clust = i,
#'       nrep = nrep,
#'       seed = seed
#'     )
#'   }
#'   
#'   return(results)
#' }
#' 
#' #' Complete workflow for personality clustering analysis
#' #' 
#' #' This function combines all steps of the analysis workflow from data preparation
#' #' to enrichment analysis.
#' #' 
#' #' @param df Data frame with personality items
#' #' @param factor_names Character vector of factor names (e.g., c("O","C","E","A","N"))
#' #' @param max_clusters Maximum number of clusters to try
#' #' @param max_seeds Number of random seeds to try
#' #' @param title Analysis title
#' #' @param output_dir Output directory
#' #' @return A list with all analysis results
#' #' @export
#' run_complete_analysis <- function(df, factor_names, max_clusters = 10, max_seeds = 100,
#'                                   title = "personality_analysis", output_dir = "./output") {
#'   
#'   if(FALSE){
#'     title <- c("BIG5_230914", "BIG5_241231")[1]
#'     output_dir = "./output"
#'     
#'     rdata <- R.utils::loadToEnv(path%+%title%_%"[1]df.RData")
#'     df <- rdata$df
#'     factor_names <- c("O","C","E","A","N")
#'     max_clusters = 10
#'     max_seeds = 100
#'   }
#'   
#'   
#'   # Create output directory
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Step 1: Prepare data
#'   cat("Step 1: Preparing data...\n")
#'   res.Data <- main.Data(df, factor_names, title = title)
#'   
#'   # Step 2: Factor analysis
#'   cat("Step 2: Performing factor analysis...\n")
#'   res.FA <- main.FA(res.Data$X, ID = res.Data$ID, nfactors=5, output_dir = output_dir, title = title)
#'   
#'   # Step 3: GMM clustering
#'   cat("Step 3: Finding optimal clustering solution...\n")
#'   res.GMM.best <- main.GMM.best(
#'     Xnew = res.FA$Xnew,
#'     ID = res.FA$ID,
#'     max.clust = max_clusters,
#'     max.seed = max_seeds,
#'     title = title,
#'     output_dir = output_dir
#'   )
#'   
#'   # Step 4: Visualize clusters
#'   xcat("Step 4: Creating cluster visualizations...\n")
#'   main.GMM.boxplot(res.GMM.best, title = title, output_dir = output_dir)
#'   
#'   # Step 5: Enrichment analysis
#'   cat("Step 5: Performing enrichment analysis...\n")
#'   res.Enrichment <- main.Enrichment(
#'     fit.GMM = res.GMM.best$fit.best,
#'     Xclust = res.GMM.best$Xclust,
#'     Xnew = res.GMM.best$Xclust,
#'     title = title,
#'     n.clust = res.GMM.best$wh.best[1],
#'     seed = res.GMM.best$wh.best[2],
#'     output_dir = output_dir
#'   )
#'   
#'   # Step 6: Explore enrichment for different numbers of clusters
#'   cat("Step 6: Exploring enrichment for different numbers of clusters...\n")
#'   res.Enrichment.range <- main.Enrichment.range(
#'     Xnew = res.FA$Xnew,
#'     n.clust = max_clusters,
#'     nrep = 100,
#'     seed = res.GMM.best$wh.best[2],
#'     output_dir = output_dir
#'   )
#'   
#'   # Plot number of significant clusters
#'   pdf_path <- file.path(output_dir, paste0(title, "-Enrichment-NumberOfClustersTrajectory.pdf"))
#'   pdf(file = pdf_path, width = 8, height = 4)
#'   plot(
#'     sapply(res.Enrichment.range, function(x) {
#'       if (is.null(x)) return(0)
#'       sum(p.adjust(x$pvalues, method = "BH") < 0.05)
#'     }),
#'     type = "b",
#'     xlab = "Number of candidate clusters",
#'     ylab = "Number of meaningful clusters"
#'   )
#'   dev.off()
#'   
#'   # Combine results
#'   result <- list(
#'     Data = res.Data,
#'     FA = res.FA,
#'     GMM = res.GMM.best,
#'     Enrichment = res.Enrichment,
#'     Enrichment.range = res.Enrichment.range
#'   )
#'   
#'   # Save results
#'   save_path <- file.path(output_dir, paste0(title, "-complete-analysis.RData"))
#'   save(result, file = save_path)
#'   
#'   cat("Analysis complete! Results saved to:", save_path, "\n")
#'   
#'   return(result)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #' Create a demo dataset for personality clustering
#' #' 
#' #' This function generates a synthetic personality dataset that can be used
#' #' to test the personality clustering analysis functions.
#' #' 
#' #' @param n_subjects Number of subjects to generate (default: 1000)
#' #' @param n_items_per_factor Number of items per factor (default: 10)
#' #' @param n_clusters Number of underlying clusters (default: 4)
#' #' @param seed Random seed (default: 123)
#' #' @return A data frame with synthetic personality data
#' #' @export
#' create_demo_dataset <- function(n_subjects = 1000, n_items_per_factor = 10,
#'                                 n_clusters = 4, seed = 123) {
#'   set.seed(seed)
#'   
#'   # Define factor names
#'   factor_names <- c("O", "C", "E", "A", "N")
#'   
#'   # Create cluster centers (different personality profiles)
#'   cluster_centers <- matrix(0, nrow = n_clusters, ncol = length(factor_names))
#'   
#'   # Define distinct personality profiles for clusters
#'   cluster_centers[1, ] <- c(0.8, 0.7, 0.8, 0.6, 0.3)  # High openness, extroversion
#'   cluster_centers[2, ] <- c(0.3, 0.8, 0.2, 0.7, 0.3)  # High conscientiousness, agreeableness
#'   cluster_centers[3, ] <- c(0.5, 0.3, 0.7, 0.4, 0.7)  # High extroversion, neuroticism
#'   cluster_centers[4, ] <- c(0.6, 0.6, 0.4, 0.8, 0.2)  # High agreeableness, moderate on others
#'   
#'   # Assign subjects to clusters
#'   cluster_assignments <- sample(1:n_clusters, n_subjects, replace = TRUE)
#'   
#'   # Generate trait scores with some noise
#'   trait_scores <- matrix(0, nrow = n_subjects, ncol = length(factor_names))
#'   for (i in 1:n_subjects) {
#'     cluster <- cluster_assignments[i]
#'     trait_scores[i, ] <- cluster_centers[cluster, ] + rnorm(length(factor_names), 0, 0.2)
#'   }
#'   
#'   # Clip values to reasonable range
#'   trait_scores <- pmax(pmin(trait_scores, 1), 0)
#'   
#'   # Generate item responses based on trait scores
#'   item_data <- matrix(0, nrow = n_subjects, ncol = length(factor_names) * n_items_per_factor)
#'   col_names <- character(length(factor_names) * n_items_per_factor)
#'   
#'   col_idx <- 1
#'   for (f in 1:length(factor_names)) {
#'     factor <- factor_names[f]
#'     for (i in 1:n_items_per_factor) {
#'       # Item parameters (difficulty and discrimination)
#'       difficulty <- runif(1, -0.5, 0.5)
#'       discrimination <- runif(1, 0.7, 1.3)
#'       
#'       # Generate item responses (roughly based on IRT)
#'       prob <- plogis(discrimination * (trait_scores[, f] - difficulty))
#'       item_data[, col_idx] <- rbinom(n_subjects, 5, prob)  # 5-point scale
#'       
#'       # Name the column
#'       col_names[col_idx] <- paste0(factor, i)
#'       col_idx <- col_idx + 1
#'     }
#'   }
#'   
#'   # Create data frame
#'   df <- as.data.frame(item_data)
#'   colnames(df) <- col_names
#'   
#'   # Add ID and demographic variables
#'   df$ID <- paste0("S", 1:n_subjects)
#'   df$age <- round(runif(n_subjects, 18, 70))
#'   df$gender <- sample(c("male", "female"), n_subjects, replace = TRUE)
#'   
#'   # Add true cluster assignments for validation
#'   df$true_cluster <- cluster_assignments
#'   
#'   return(df)
#' }
#' 
#' #' Example usage of the package
#' #' 
#' #' This function demonstrates how to use the package with a demo dataset.
#' #' 
#' #' @export
#' example_analysis <- function() {
#'   # Create a demo dataset
#'   cat("Creating demo dataset...\n")
#'   demo_data <- create_demo_dataset(n_subjects = 500)
#'   
#'   # Set output directory
#'   output_dir <- "./demo_output"
#'   
#'   # Run the analysis
#'   cat("Running analysis...\n")
#'   result <- run_complete_analysis(
#'     df = demo_data,
#'     factor_names = c("O", "C", "E", "A", "N"),
#'     max_clusters = 8,
#'     max_seeds = 20,
#'     title = "demo_analysis",
#'     output_dir = output_dir
#'   )
#'   
#'   # Validate results against true clusters
#'   cat("\nValidation against true clusters:\n")
#'   if (requireNamespace("aricode", quietly = TRUE)) {
#'     true_clusters <- demo_data$true_cluster
#'     found_clusters <- result$GMM$Xclust$class
#'     
#'     ari <- aricode::ARI(true_clusters, found_clusters)
#'     nmi <- aricode::NMI(true_clusters, found_clusters)
#'     
#'     cat("Adjusted Rand Index:", ari, "\n")
#'     cat("Normalized Mutual Information:", nmi, "\n")
#'     
#'     if (ari > 0.7) {
#'       cat("Great! The clustering solution closely matches the true clusters.\n")
#'     } else if (ari > 0.4) {
#'       cat("Good! The clustering solution captures many of the true clusters.\n")
#'     } else {
#'       cat("The clustering solution differs from the true clusters.\n")
#'       cat("This could be due to noise or the algorithm finding a different structure.\n")
#'     }
#'   } else {
#'     cat("Install the 'aricode' package to evaluate clustering accuracy.\n")
#'   }
#'   
#'   cat("\nAnalysis complete! Results are in:", output_dir, "\n")
#'   
#'   return(result)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #' Compositional regression analysis for cluster probabilities
#' #' 
#' #' This function performs compositional regression analysis to assess the relationship
#' #' between cluster membership probabilities and outcome variables.
#' #' 
#' #' @param X0 Matrix of cluster probabilities
#' #' @param Y Matrix of outcome variable(s)
#' #' @param type Type of analysis ("bootstrap" or "regular")
#' #' @param nboot Number of bootstrap samples (if type = "bootstrap")
#' #' @return A list containing regression results
#' #' @export
#' png.CompReg <- function(X0, Y, type = "bootstrap", nboot = 1000) {
#'   # Require compositions package
#'   requireNamespace("compositions", quietly = TRUE)
#'   
#'   # Check if X0 is compositional and convert if needed
#'   if (!inherits(X0, "acomp")) {
#'     X0 <- compositions::acomp(X0)
#'   }
#'   
#'   # Center log-ratio transformation
#'   X.clr <- compositions::clr(X0)
#'   
#'   # Prepare result storage
#'   result <- list()
#'   
#'   if (type == "bootstrap") {
#'     # Prepare bootstrap storage
#'     n <- nrow(X0)
#'     coef.boot <- matrix(NA, nboot, ncol(X.clr))
#'     
#'     # Run bootstrap
#'     for (i in 1:nboot) {
#'       if (i %% 100 == 0) {
#'         cat("Bootstrap iteration:", i, "of", nboot, "\n")
#'       }
#'       
#'       # Sample with replacement
#'       idx <- sample(1:n, n, replace = TRUE)
#'       X.boot <- X.clr[idx, ]
#'       Y.boot <- Y[idx, , drop = FALSE]
#'       
#'       # Run regression
#'       fit <- lm(Y.boot ~ X.boot)
#'       coef.boot[i, ] <- coef(fit)[-1]  # Exclude intercept
#'     }
#'     
#'     # Calculate bootstrap statistics
#'     coef.mean <- colMeans(coef.boot)
#'     coef.sd <- apply(coef.boot, 2, sd)
#'     coef.lower <- apply(coef.boot, 2, quantile, 0.025)
#'     coef.upper <- apply(coef.boot, 2, quantile, 0.975)
#'     
#'     # Store results
#'     result[[1]] <- list(
#'       coef.mean = coef.mean,
#'       coef.sd = coef.sd,
#'       coef.lower = coef.lower,
#'       coef.upper = coef.upper,
#'       coef.boot = coef.boot
#'     )
#'   } else {
#'     # Regular regression without bootstrap
#'     fit <- lm(Y ~ X.clr)
#'     result[[1]] <- list(
#'       fit = fit,
#'       coef = coef(fit)[-1],  # Exclude intercept
#'       summary = summary(fit)
#'     )
#'   }
#'   
#'   return(result)
#' }
#' 
#' #' Create forest plot data frame from compositional regression results
#' #' 
#' #' This function prepares data for creating forest plots from compositional regression results.
#' #' 
#' #' @param result Result from png.CompReg function
#' #' @param DV.name Name of the dependent variable
#' #' @param class Vector of cluster names/numbers
#' #' @param width Width scaling factor for visualization
#' #' @return A data frame for forest plot
#' #' @export
#' png.forest.df <- function(result, DV.name = "Outcome", class = NULL, width = 30) {
#'   # Extract regression coefficients and confidence intervals
#'   coef <- result$coef.mean
#'   lower <- result$coef.lower
#'   upper <- result$coef.upper
#'   
#'   # Create cluster names if not provided
#'   if (is.null(class)) {
#'     class <- paste0("Cluster ", 1:length(coef))
#'   } else if (length(class) == length(coef)) {
#'     class <- as.character(class)
#'   } else {
#'     stop("Length of 'class' must match the number of coefficients")
#'   }
#'   
#'   # Create dataframe
#'   df <- data.frame(
#'     class = class,
#'     coef = coef,
#'     lower = lower,
#'     upper = upper,
#'     DV = rep(DV.name, length(coef)),
#'     stringsAsFactors = FALSE
#'   )
#'   
#'   # Add width for visualization
#'   df$width <- width
#'   
#'   class(df) <- c("png.forest.df", class(df))
#'   
#'   return(df)
#' }
#' 
#' #' Plot forest plot from png.forest.df
#' #' 
#' #' This function creates a forest plot from the data frame created by png.forest.df.
#' #' 
#' #' @param x A data frame created by png.forest.df
#' #' @param ... Additional arguments passed to plotting functions
#' #' @return A ggplot2 object
#' #' @export
#' plot.png.forest.df <- function(x, ...) {
#'   # Require ggplot2
#'   requireNamespace("ggplot2", quietly = TRUE)
#'   
#'   # Create plot
#'   p <- ggplot2::ggplot(x, ggplot2::aes(x = coef, y = class)) +
#'     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
#'     ggplot2::geom_errorbarh(
#'       ggplot2::aes(xmin = lower, xmax = upper, height = 0.2),
#'       color = "gray50"
#'     ) +
#'     ggplot2::geom_point(shape = 15, size = 3, color = "black") +
#'     ggplot2::facet_wrap(~ DV, scales = "free_x") +
#'     ggplot2::theme_bw() +
#'     ggplot2::theme(
#'       panel.grid.minor = ggplot2::element_blank(),
#'       axis.title.y = ggplot2::element_blank()
#'     ) +
#'     ggplot2::labs(
#'       x = "Coefficient (95% CI)",
#'       title = "Cluster Influence on Outcome"
#'     )
#'   
#'   return(p)
#' }
#' 
#' #' Run compositional regression analysis for multiple outcome variables
#' #' 
#' #' This function performs compositional regression analysis for multiple outcome
#' #' variables and creates forest plots for visualization.
#' #' 
#' #' @param Xreg Data frame containing cluster probabilities and outcome variables
#' #' @param var_list Vector of variable names to analyze
#' #' @param filter_vars Logical; whether to filter variables (default: FALSE)
#' #' @param adjust_for_covariates Logical; whether to adjust for covariates (default: TRUE)
#' #' @param covariates Vector of covariate names (default: c("age_min", "gender_vote"))
#' #' @param prob_prefix Prefix for probability columns (default: "prob.")
#' #' @param suffix_filter Suffix for filter variables (default: "_after_PERSONALITY")
#' #' @param title Title for output files
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return A list of compositional regression results
#' #' @export
#' run_compositional_regression <- function(Xreg, var_list, filter_vars = FALSE, 
#'                                          adjust_for_covariates = TRUE,
#'                                          covariates = c("age_min", "gender_vote"),
#'                                          prob_prefix = "prob.",
#'                                          suffix_filter = "_after_PERSONALITY",
#'                                          title = "analysis", output_dir = "./output") {
#'   # Create output directory if it doesn't exist
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'   
#'   # Require necessary packages
#'   requireNamespace("dplyr", quietly = TRUE)
#'   requireNamespace("compositions", quietly = TRUE)
#'   requireNamespace("ggplot2", quietly = TRUE)
#'   
#'   # Determine number of clusters
#'   num_clusters <- sum(grepl(paste0("^", prob_prefix), names(Xreg)))
#'   
#'   # Initialize results list
#'   result_list <- vector("list", length(var_list))
#'   names(result_list) <- var_list
#'   
#'   # Process each variable
#'   for (i in seq_along(var_list)) {
#'     var <- var_list[i]
#'     cat("Processing variable:", var, "(", i, "of", length(var_list), ")\n")
#'     
#'     # Prepare data for analysis
#'     if (filter_vars) {
#'       var_filter <- paste0(var, suffix_filter)
#'       
#'       # Filter data
#'       Xreg_filtered <- Xreg %>%
#'         dplyr::select(dplyr::all_of(c(covariates, 
#'                                       paste0(prob_prefix, 1:num_clusters), 
#'                                       var, var_filter))) %>%
#'         dplyr::filter(!!dplyr::sym(var_filter) == 1) %>%
#'         tidyr::drop_na()
#'     } else {
#'       # Use all data
#'       Xreg_filtered <- Xreg %>%
#'         dplyr::select(dplyr::all_of(c(covariates, 
#'                                       paste0(prob_prefix, 1:num_clusters), 
#'                                       var))) %>%
#'         tidyr::drop_na()
#'     }
#'     
#'     # Check if we have enough data
#'     if (nrow(Xreg_filtered) < 10) {
#'       warning("Not enough data for variable ", var, ". Skipping.")
#'       next
#'     }
#'     
#'     # Extract compositional data and transform
#'     X0 <- Xreg_filtered %>%
#'       dplyr::select(dplyr::starts_with(prob_prefix)) %>%
#'       as.data.frame() %>%
#'       compositions::acomp() %>%
#'       as.data.frame() %>%
#'       as.matrix()
#'     
#'     # Extract outcome variable
#'     Y <- Xreg_filtered %>%
#'       dplyr::select(dplyr::all_of(var)) %>%
#'       as.data.frame() %>%
#'       as.matrix()
#'     
#'     # Adjust for covariates if needed
#'     if (adjust_for_covariates) {
#'       Z <- Xreg_filtered %>%
#'         dplyr::select(dplyr::all_of(covariates)) %>%
#'         as.data.frame()
#'       
#'       # Create temporary data frame for regression
#'       data_tmp <- data.frame(y = Y, Z)
#'       colnames(data_tmp)[1] <- "y"
#'       
#'       # Get residuals from covariate model
#'       Y_adj <- stats::lm(y ~ ., data = data_tmp)$residuals %>% as.matrix()
#'       
#'       # Run compositional regression with adjusted outcome
#'       res_CompReg <- png.CompReg(X0 = X0, Y = Y_adj, type = "bootstrap")
#'     } else {
#'       # Run compositional regression without adjustment
#'       res_CompReg <- png.CompReg(X0 = X0, Y = Y, type = "bootstrap")
#'     }
#'     
#'     # Store results
#'     result_list[[i]] <- res_CompReg
#'     
#'     # Create and save forest plot
#'     df_forest <- png.forest.df(
#'       res_CompReg[[1]], 
#'       DV.name = var, 
#'       class = 1:num_clusters, 
#'       width = 30
#'     )
#'     
#'     # Determine file name based on options
#'     file_name <- paste0(
#'       title, 
#'       "_ForestPlot",
#'       ifelse(filter_vars, "-filtered", ""),
#'       ifelse(adjust_for_covariates, "-Adjusted", ""),
#'       "-", var, ".pdf"
#'     )
#'     
#'     # Save plot
#'     pdf_path <- file.path(output_dir, file_name)
#'     grDevices::pdf(pdf_path, width = 6.2, height = 2)
#'     plot(df_forest)
#'     grDevices::dev.off()
#'     
#'     cat("Created forest plot:", file_name, "\n")
#'   }
#'   
#'   # Return results
#'   return(result_list)
#' }
#' 
#' #' Add compositional regression analysis to existing cluster analysis
#' #' 
#' #' This function extends a cluster analysis with compositional regression to
#' #' analyze relationships between cluster membership and outcome variables.
#' #' 
#' #' @param cluster_result Result from run_complete_analysis function
#' #' @param outcome_data Data frame with outcome variables
#' #' @param var_list Vector of outcome variable names to analyze
#' #' @param id_var Name of ID variable to join data
#' #' @param filter_vars Logical; whether to filter variables (default: FALSE)
#' #' @param adjust_for_covariates Logical; whether to adjust for covariates (default: TRUE)
#' #' @param title Title for output files
#' #' @param output_dir Directory to save output files (default: "./output")
#' #' @return Original cluster_result with added compositional regression results
#' #' @export
#' extend_with_compositional_regression <- function(cluster_result, outcome_data, var_list,
#'                                                  id_var = "ID", filter_vars = FALSE,
#'                                                  adjust_for_covariates = TRUE, 
#'                                                  title = "analysis", output_dir = "./output") {
#'   # Extract cluster assignments and probabilities
#'   Xclust <- cluster_result$GMM$Xclust
#'   fit_best <- cluster_result$GMM$fit.best
#'   
#'   # Get cluster probabilities
#'   prob <- fit_best$z
#'   
#'   # Join with outcome data
#'   Xreg <- dplyr::left_join(
#'     data.frame(ID = cluster_result$Data$ID, Xclust, prob = prob),
#'     outcome_data,
#'     by = id_var
#'   )
#'   
#'   # Run compositional regression
#'   comp_reg_results <- run_compositional_regression(
#'     Xreg = Xreg,
#'     var_list = var_list,
#'     filter_vars = filter_vars,
#'     adjust_for_covariates = adjust_for_covariates,
#'     title = title,
#'     output_dir = output_dir
#'   )
#'   
#'   # Add results to original result object
#'   cluster_result$CompositionalRegression <- comp_reg_results
#'   
#'   return(cluster_result)
#' }
#' 
#' #' Create synthetic outcome data for demo purposes
#' #' 
#' #' This function generates synthetic outcome data that can be used
#' #' with the compositional regression functions.
#' #' 
#' #' @param cluster_result Result from run_complete_analysis function
#' #' @param n_outcomes Number of outcome variables to generate (default: 5)
#' #' @param id_var Name of ID variable to include (default: "ID")
#' #' @param effect_sizes Vector of effect sizes for different clusters
#' #' @param seed Random seed for reproducibility
#' #' @return A data frame with synthetic outcome data
#' #' @export
#' create_demo_outcomes <- function(cluster_result, n_outcomes = 5, id_var = "ID",
#'                                  effect_sizes = c(0.2, -0.3, 0.1, 0), seed = 123) {
#'   set.seed(seed)
#'   
#'   # Extract cluster assignments and IDs
#'   Xclust <- cluster_result$GMM$Xclust
#'   IDs <- cluster_result$Data$ID
#'   
#'   # Initialize outcome data frame
#'   outcome_data <- data.frame(ID = IDs)
#'   
#'   # Generate outcome variables
#'   outcome_names <- paste0("Outcome", 1:n_outcomes)
#'   
#'   for (i in 1:n_outcomes) {
#'     # Base outcome with noise
#'     outcome <- rnorm(length(IDs), mean = 50, sd = 10)
#'     
#'     # Add cluster-specific effects
#'     for (j in 1:length(effect_sizes)) {
#'       # Find members of this cluster
#'       cluster_members <- Xclust$class == j
#'       
#'       # Add effect (scaled by cluster size for balance)
#'       effect <- effect_sizes[j] * 10 * (i / 2)  # Vary effect by outcome
#'       outcome[cluster_members] <- outcome[cluster_members] + effect
#'     }
#'     
#'     # Add to data frame
#'     outcome_data[[outcome_names[i]]] <- outcome
#'   }
#'   
#'   # Add demographic variables
#'   outcome_data$age <- round(runif(length(IDs), 18, 70))
#'   outcome_data$gender <- sample(c("male", "female"), length(IDs), replace = TRUE)
#'   
#'   # Add filter variables
#'   for (i in 1:n_outcomes) {
#'     filter_var <- paste0(outcome_names[i], "_after_PERSONALITY")
#'     outcome_data[[filter_var]] <- sample(c(0, 1), length(IDs), replace = TRUE, prob = c(0.1, 0.9))
#'   }
#'   
#'   return(outcome_data)
#' }
#' 
#' #' Example usage of compositional regression
#' #' 
#' #' This function demonstrates how to use the compositional regression functions
#' #' with a demo dataset.
#' #' 
#' #' @export
#' example_compositional_regression <- function() {
#'   # Create a demo dataset and run clustering
#'   cat("Creating demo dataset and performing clustering...\n")
#'   demo_data <- create_demo_dataset(n_subjects = 500)
#'   
#'   # Set output directory
#'   output_dir <- "./demo_output_compreg"
#'   
#'   # Run cluster analysis
#'   cluster_result <- run_complete_analysis(
#'     df = demo_data,
#'     factor_names = c("O", "C", "E", "A", "N"),
#'     max_clusters = 8,
#'     max_seeds = 20,
#'     title = "demo_compreg",
#'     output_dir = output_dir
#'   )
#'   
#'   # Create demo outcome data
#'   cat("Creating demo outcome data...\n")
#'   outcome_data <- create_demo_outcomes(
#'     cluster_result = cluster_result,
#'     n_outcomes = 5,
#'     effect_sizes = c(0.3, -0.2, 0.1, -0.1)
#'   )
#'   
#'   # Run compositional regression
#'   cat("Running compositional regression analysis...\n")
#'   output_vars <- paste0("Outcome", 1:5)
#'   
#'   # Without filtering or adjustment
#'   cat("Standard analysis (no filtering or adjustment)...\n")
#'   result1 <- extend_with_compositional_regression(
#'     cluster_result = cluster_result,
#'     outcome_data = outcome_data,
#'     var_list = output_vars,
#'     filter_vars = FALSE,
#'     adjust_for_covariates = FALSE,
#'     title = "demo_compreg_standard",
#'     output_dir = output_dir
#'   )
#'   
#'   # With filtering
#'   cat("Analysis with filtering...\n")
#'   result2 <- extend_with_compositional_regression(
#'     cluster_result = cluster_result,
#'     outcome_data = outcome_data,
#'     var_list = output_vars,
#'     filter_vars = TRUE,
#'     adjust_for_covariates = FALSE,
#'     title = "demo_compreg_filtered",
#'     output_dir = output_dir
#'   )
#'   
#'   # With adjustment
#'   cat("Analysis with covariate adjustment...\n")
#'   result3 <- extend_with_compositional_regression(
#'     cluster_result = cluster_result,
#'     outcome_data = outcome_data,
#'     var_list = output_vars,
#'     filter_vars = FALSE,
#'     adjust_for_covariates = TRUE,
#'     title = "demo_compreg_adjusted",
#'     output_dir = output_dir
#'   )
#'   
#'   # With both filtering and adjustment
#'   cat("Analysis with filtering and adjustment...\n")
#'   result4 <- extend_with_compositional_regression(
#'     cluster_result = cluster_result,
#'     outcome_data = outcome_data,
#'     var_list = output_vars,
#'     filter_vars = TRUE,
#'     adjust_for_covariates = TRUE,
#'     title = "demo_compreg_filtered_adjusted",
#'     output_dir = output_dir
#'   )
#'   
#'   cat("\nCompositional regression analysis complete!\n")
#'   cat("Results saved to:", output_dir, "\n")
#'   
#'   # Return results with adjustment (most complete)
#'   return(result4)
#' }
#' 
#' #' Complete workflow including compositional regression
#' #' 
#' #' This function combines all steps of the analysis workflow from data preparation
#' #' to compositional regression analysis.
#' #' 
#' #' @param df Data frame with personality items
#' #' @param outcome_data Data frame with outcome variables
#' #' @param factor_names Character vector of factor names (e.g., c("O","C","E","A","N"))
#' #' @param outcome_vars Vector of outcome variables to analyze
#' #' @param max_clusters Maximum number of clusters to try
#' #' @param max_seeds Number of random seeds to try
#' #' @param filter_vars Whether to filter variables based on suffix
#' #' @param adjust_for_covariates Whether to adjust for covariates
#' #' @param title Analysis title
#' #' @param output_dir Output directory
#' #' @return A list with all analysis results
#' #' @export
#' run_complete_analysis_with_outcomes <- function(df, outcome_data, factor_names, 
#'                                                 outcome_vars, max_clusters = 10, 
#'                                                 max_seeds = 100, filter_vars = FALSE,
#'                                                 adjust_for_covariates = TRUE,
#'                                                 title = "personality_analysis", 
#'                                                 output_dir = "./output") {
#'   # Run the main clustering analysis
#'   cat("Running clustering analysis...\n")
#'   cluster_result <- run_complete_analysis(
#'     df = df,
#'     factor_names = factor_names,
#'     max_clusters = max_clusters,
#'     max_seeds = max_seeds,
#'     title = title,
#'     output_dir = output_dir
#'   )
#'   
#'   # Run compositional regression with outcomes
#'   cat("Running compositional regression analysis...\n")
#'   final_result <- extend_with_compositional_regression(
#'     cluster_result = cluster_result,
#'     outcome_data = outcome_data,
#'     var_list = outcome_vars,
#'     filter_vars = filter_vars,
#'     adjust_for_covariates = adjust_for_covariates,
#'     title = title,
#'     output_dir = output_dir
#'   )
#'   
#'   # Save results
#'   save_path <- file.path(output_dir, paste0(title, "-complete-analysis-with-outcomes.RData"))
#'   save(final_result, file = save_path)
#'   
#'   cat("Complete analysis with outcomes finished!\n")
#'   cat("Results saved to:", save_path, "\n")
#'   
#'   return(final_result)
#' }