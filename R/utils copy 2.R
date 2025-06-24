#' ###############################################################################
#' # Package: GenericDataAnalysis
#' # File: R/analysis.R
#' ###############################################################################
#' 
#' ## 1. Data Extraction & Preprocessing
#' #' Extract Factor Data from a Data Frame
#' #'
#' #' Given a data frame and a vector of factor name prefixes (e.g. "O", "C", "E", "A", "N"),
#' #' this function selects the matching columns (assuming column names end with numbers),
#' #' drops rows with missing values, prints some basic diagnostics, and returns a list with
#' #' the selected data matrix and an identifier vector.
#' #'
#' #' @param df A data frame containing the data.
#' #' @param factor_names A character vector of factor prefixes.
#' #' @param id_col The name of the ID column in \code{df}. Defaults to "ID".
#' #' @param title A title used for logging. Defaults to "title".
#' #' @return A list with elements \code{ID} and \code{X} (the extracted factor matrix).
#' #' @export
#' main.Data <- function(df, factor_names, id_col = "ID", title = "title") {
#'   # Build a regex pattern to match columns like "O1", "C2", etc.
#'   pattern <- paste0("^(", paste(factor_names, collapse = "|"), ")[0-9]+$")
#'   X <- df %>% dplyr::select(dplyr::matches(pattern)) %>% tidyr::drop_na()
#'   
#'   message("(Dimension):"); print(dim(X))
#'   message("(Top 2 rows):"); print(head(X, 2))
#'   message("(List of Factors):")
#'   print(table(gsub("[0-9]+", "", colnames(X))))
#'   
#'   if (!id_col %in% colnames(df)) {
#'     stop("ID column not found in data frame.")
#'   }
#'   ID <- df[[id_col]]
#'   list(ID = ID, X = X)
#' }
#' 
#' ###############################################################################
#' ## 2. Factor Analysis
#' #' Perform Factor Analysis on Selected Data
#' #'
#' #' Conducts exploratory factor analysis using the \code{psych} package.
#' #' It computes the KMO index, Bartlett’s test, creates scree plots,
#' #' and then extracts factor scores using oblimin rotation.
#' #' Optionally, a second-order (multilevel) factor analysis can be performed.
#' #'
#' #' @param X A numeric matrix or data frame (usually the output \code{X} from \code{main.Data}).
#' #' @param ID The identifier vector.
#' #' @param nfactors Number of factors to extract. If \code{NULL}, determined by \code{fa.parallel}.
#' #' @param nfactors2 Optional second-order factor analysis; if provided, a multi-factor solution is computed.
#' #' @param title A title prefix for output files.
#' #' @return A list containing the original data \code{X}, the factor scores (\code{Xnew}),
#' #'         and captured console output.
#' #' @export
#' main.FA <- function(X, ID, nfactors = NULL, nfactors2 = NULL, title = "title") {
#'   require(psych)
#'   require(car)
#'   
#'   # Capture all console output for logging.
#'   console <- capture.output({
#'     message("-------- Check Factor Analysis Availability --------")
#'     message("Kaiser-Meyer-Olkin (KMO) index:")
#'     fit.KMO <- psych::KMO(cor(X))
#'     message("KMO MSA: ", round(fit.KMO$MSA, 3), " (should be ≥ 0.6)")
#'     
#'     message("Bartlett's test:")
#'     fit.bartlett <- psych::cortest.bartlett(cor(X), n = nrow(X))
#'     message("Bartlett p-value: ", round(fit.bartlett$p.value, 3), " (should be < 0.05)")
#'     
#'     message("-------- Determine Number of Factors --------")
#'     parallel <- psych::fa.parallel(X, fa = "fa", sim = FALSE, se.bars = FALSE, plot = FALSE)
#'     # Save scree plots
#'     pdf(file = sprintf("./rdata/%s-FA-ScreePlot.pdf", title), width = 8, height = 4)
#'     plot(parallel, fa = "pc", sim = FALSE, se.bars = FALSE)
#'     dev.off()
#'     pdf(file = sprintf("./rdata/%s-FA-ScreePlot-simple.pdf", title), width = 8, height = 4)
#'     plot(seq_along(parallel$pc.values[1:30]), parallel$pc.values[1:30], type = "b",
#'          xlab = "Number of factors", ylab = "Eigenvalues")
#'     dev.off()
#'     
#'     if (is.null(nfactors)) {
#'       nfactors <<- max(which(parallel$fa.values > 1))
#'     }
#'     message("Using ", nfactors, " factors.")
#'     
#'     message("-------- Run Factor Analysis --------")
#'     fit.fa <- psych::fa(X, nfactors = nfactors, rotate = "oblimin", scores = "regression", fm = "pa")
#'     
#'     pdf(file = sprintf("./rdata/%s-FA-Diagram.pdf", title), width = 5, height = 18)
#'     psych::fa.diagram(fit.fa, sort = FALSE)
#'     dev.off()
#'     
#'     message("FA Loadings (thresholded at 0.3):")
#'     print(ifelse(abs(fit.fa$loadings) < 0.3, 0, fit.fa$loadings))
#'     
#'     sparse.loading <- ifelse(abs(fit.fa$loadings) < 0.3, 0, fit.fa$loadings)
#'     FactorNames <- apply(sparse.loading, 2, function(x) {
#'       names(which(x > 0)) %>% gsub("[0-9]+", "", .) %>% table %>% which.max %>% names
#'     })
#'     
#'     Xnew <- fit.fa$scores
#'     colnames(Xnew) <- FactorNames
#'     
#'     if (!is.null(nfactors2)) {
#'       message("-------- Run Second-Order Factor Analysis --------")
#'       fit.multi <- psych::fa.multi(X, nfactors, nfactors2, scores = "regression", rotate = "oblimin")
#'       parallel2 <- psych::fa.parallel(fit.multi$f1$scores, fa = "fa", sim = FALSE, se.bars = FALSE, plot = FALSE)
#'       pdf(file = sprintf("./rdata/%s-FA-SecondOrder-ScreePlot.pdf", title), width = 10, height = 5)
#'       plot(parallel2, fa = "pc", sim = FALSE, se.bars = FALSE, show.legend = TRUE)
#'       dev.off()
#'       pdf(file = sprintf("./rdata/%s-FA-SecondOrder.pdf", title), width = 6, height = 12)
#'       psych::fa.multi.diagram(fit.multi, cut = 0.29)
#'       dev.off()
#'       
#'       message("Second-order FA Loadings (thresholded at 0.29):")
#'       print(ifelse(abs(fit.multi$f2$loadings) < 0.29, 0, fit.multi$f2$loadings))
#'       
#'       fit.multi.loadings <- fit.multi$f2$loadings
#'       row.names(fit.multi.loadings) <- FactorNames
#'       FactorNames2 <- apply(ifelse(abs(fit.multi.loadings) < 0.29, 0, fit.multi.loadings), 2, function(x) {
#'         paste(names(which(x > 0)) %>% gsub("[0-9]+", "", .), collapse = "")
#'       })
#'       
#'       Xnew <- scale(fit.multi$f1$scores) %*% (solve(cor(fit.multi$f1$scores)) %*% fit.multi$f2$loadings)
#'       colnames(Xnew) <- FactorNames2
#'     }
#'   })
#'   
#'   list(ID = ID, X = X, Xnew = Xnew, console = console)
#' }
#' 
#' ###############################################################################
#' ## 3. GMM Clustering & NEC Evaluation
#' #' Calculate Normalized Entropy Criterion (NEC)
#' #'
#' #' Computes the entropy from the posterior probabilities of an mclust model and
#' #' normalizes it by the difference in log-likelihood relative to a base model.
#' #'
#' #' @param model An mclust model object.
#' #' @param base_loglik The log-likelihood from the base model (typically for G = 1).
#' #' @return A numeric value representing the NEC.
#' #' @export
#' calculate_nec <- function(model, base_loglik) {
#'   probs <- model$z
#'   eps <- 1e-10
#'   probs <- pmax(probs, eps)
#'   entropy <- -sum(probs * log(probs))
#'   loglik_diff <- model$loglik - base_loglik
#'   if (loglik_diff <= 0) return(Inf)
#'   entropy / loglik_diff
#' }
#' 
#' #' Evaluate NEC over Multiple Seeds for GMM Clustering
#' #'
#' #' For each seed in a given sequence, run GMM clustering (for cluster counts 1 to \code{max.clust}),
#' #' compute the NEC values, save a PDF plot of NEC, and return the list of model fits.
#' #'
#' #' @param Xnew A numeric data matrix for clustering.
#' #' @param max.clust Maximum number of clusters to test.
#' #' @param seed.seq A vector of seeds for random initialization.
#' #' @param title Title prefix for output files.
#' #' @param print.pdf Logical; whether to save PDF plots.
#' #' @param plot.width Width of the PDF plot.
#' #' @param plot.height Height of the PDF plot.
#' #' @param cex Point size in the plot.
#' #' @return A list (named by seed) where each element contains the list of mclust fits, NEC vector, and best cluster count.
#' #' @export
#' main.NEC <- function(Xnew, max.clust = 10, seed.seq = 1:100, title = "GMM_NEC",
#'                      print.pdf = TRUE, plot.width = 6, plot.height = 4, cex = 0.4) {
#'   require(mclust)
#'   results <- lapply(seed.seq, function(seed) {
#'     set.seed(seed)
#'     fits <- lapply(1:max.clust, function(k) Mclust(Xnew, G = k, modelNames = "VVV"))
#'     base_loglik <- fits[[1]]$loglik
#'     nec.vec <- sapply(fits, calculate_nec, base_loglik = base_loglik)
#'     best_clust <- which.min(nec.vec)
#'     message(sprintf("Seed %d: NEC = %s", seed, paste(round(nec.vec, 3), collapse = ", ")))
#'     if (print.pdf) {
#'       pdf_file <- sprintf("%s-NEC-seed=%d.pdf", title, seed)
#'       pdf(pdf_file, width = plot.width, height = plot.height)
#'       plot(nec.vec, type = "b", xlab = "Number of clusters",
#'            ylab = "Normalized Entropy Criterion", main = sprintf("Seed %d", seed), cex = cex)
#'       dev.off()
#'       extrafont::embed_fonts(pdf_file)
#'     }
#'     list(fits = fits, nec = nec.vec, best_clust = best_clust)
#'   })
#'   names(results) <- as.character(seed.seq)
#'   results
#' }
#' 
#' #' Run GMM Clustering for a Fixed Seed with NEC Evaluation
#' #'
#' #' Runs GMM clustering (with 1 to \code{max.clust} clusters) using a fixed seed,
#' #' selects the best model based on the NEC value, saves diagnostic plots,
#' #' and returns the best model and its clustering.
#' #'
#' #' @param Xnew A numeric data matrix.
#' #' @param max.clust Maximum number of clusters to test.
#' #' @param title Title prefix for output files.
#' #' @param seed Seed for random initialization.
#' #' @param print.pdf Logical; whether to save diagnostic plots.
#' #' @param plot.width Width for PDF plots.
#' #' @param plot.height Height for PDF plots.
#' #' @param cex Point size for plot symbols.
#' #' @return A list with elements: the best GMM model, NEC vector, best cluster count, and clustering data frame.
#' #' @export
#' main.GMM <- function(Xnew, max.clust = 10, title = "GMM", seed,
#'                      print.pdf = TRUE, plot.width = 6, plot.height = 4, cex = 0.4) {
#'   require(mclust)
#'   set.seed(seed)
#'   fits <- lapply(1:max.clust, function(k) Mclust(Xnew, G = k, modelNames = "VVV"))
#'   base_loglik <- fits[[1]]$loglik
#'   nec.vec <- sapply(fits, calculate_nec, base_loglik = base_loglik)
#'   best_clust <- which.min(nec.vec)
#'   message("NEC values: ", paste(round(nec.vec, 3), collapse = ", "))
#'   if (print.pdf) {
#'     pdf_file <- sprintf("%s-NEC.pdf", title)
#'     pdf(pdf_file, width = plot.width, height = plot.height)
#'     plot(nec.vec, type = "b", xlab = "Number of clusters",
#'          ylab = "Normalized Entropy Criterion", main = sprintf("Seed %d", seed), cex = cex)
#'     dev.off()
#'     extrafont::embed_fonts(pdf_file)
#'   }
#'   best_fit <- fits[[best_clust]]
#'   Xclust <- cbind.data.frame(Xnew, class = as.factor(best_fit$classification))
#'   message("Best GMM model summary:")
#'   print(summary(best_fit, parameters = TRUE)$mean)
#'   if (print.pdf) {
#'     best_fit_reduced <- best_fit
#'     samp <- sample(seq_len(nrow(best_fit$data)), size = floor(nrow(best_fit$data) * 1.0))
#'     best_fit_reduced$data <- best_fit$data[samp, , drop = FALSE]
#'     best_fit_reduced$n <- floor(nrow(best_fit$data) * 1.0)
#'     best_fit_reduced$classification <- best_fit$classification[samp]
#'     png_file <- sprintf("%s-GMM-Class.png", title)
#'     png(filename = png_file, res = 500, width = 8, height = 4, units = "in")
#'     plot(best_fit_reduced, what = "classification", cex = cex)
#'     dev.off()
#'   }
#'   list(Xnew = Xnew, nec = nec.vec, best_clust = best_clust, best_fit = best_fit, Xclust = Xclust)
#' }
#' 
#' #' Run GMM Clustering Across Multiple Seeds and Select the Best Model Based on NEC
#' #'
#' #' Iterates over many seeds, computes NEC for each, saves overall NEC diagnostic plots,
#' #' and returns the best model (lowest NEC) along with seed and cluster information.
#' #'
#' #' @param Xnew A numeric data matrix.
#' #' @param max.clust Maximum number of clusters to test.
#' #' @param title Title prefix for output files.
#' #' @param max.seed Maximum number of seeds to try.
#' #' @param print.pdf Logical; whether to save diagnostic plots.
#' #' @param plot.width Width for PDF plots.
#' #' @param plot.height Height for PDF plots.
#' #' @return A list containing the best model, NEC matrix, best seed, best cluster count, and clustering data frame.
#' #' @export
#' main.GMM.best <- function(Xnew, max.clust = 10, title = "GMM_Best", max.seed = 100, 
#'                           print.pdf = TRUE, plot.width = 8, plot.height = 4) {
#'   require(mclust)
#'   NEC.mat <- matrix(NA, nrow = max.seed, ncol = max.clust)
#'   fits_list <- vector("list", max.seed)
#'   for (j in 1:max.seed) {
#'     set.seed(j)
#'     fits <- lapply(1:max.clust, function(k) Mclust(Xnew, G = k, modelNames = "VVV"))
#'     fits_list[[j]] <- fits
#'     base_loglik <- fits[[1]]$loglik
#'     for (i in 2:max.clust) {
#'       NEC.mat[j, i] <- calculate_nec(fits[[i]], base_loglik)
#'     }
#'   }
#'   best_idx <- which(NEC.mat == min(NEC.mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
#'   if (print.pdf) {
#'     pdf_file_mean <- sprintf("%s-NEC-MeanCurve.pdf", title)
#'     pdf(pdf_file_mean, width = plot.width, height = plot.height)
#'     matplot(cbind(rowMeans(NEC.mat, na.rm = TRUE), NEC.mat), type = "l", 
#'             lty = c(1, rep(2, max.clust)),
#'             col = c("red", rep("gray50", max.clust)),
#'             ylab = "Normalized Entropy Criterion", xlab = "Number of clusters")
#'     dev.off()
#'     extrafont::embed_fonts(pdf_file_mean)
#'     
#'     pdf_file_all <- sprintf("%s-NEC-AllSeeds.pdf", title)
#'     pdf(pdf_file_all, width = plot.width, height = plot.height)
#'     matplot(NEC.mat, type = "l", lty = rep(2, max.clust), col = rep("gray50", max.clust),
#'             ylab = "Normalized Entropy Criterion", xlab = "Number of clusters")
#'     best_fit_idx <- which(NEC.mat == min(NEC.mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
#'     points(best_fit_idx[2], NEC.mat[best_fit_idx[1], best_fit_idx[2]], col = "red", pch = 18)
#'     abline(v = best_fit_idx[2], col = "red", lty = 2)
#'     dev.off()
#'     extrafont::embed_fonts(pdf_file_all)
#'   }
#'   best_seed <- best_idx[1]
#'   best_cluster_num <- best_idx[2]
#'   set.seed(best_seed)
#'   best_fit <- Mclust(Xnew, G = best_cluster_num, modelNames = "VVV")
#'   Xclust <- cbind.data.frame(Xnew, class = as.factor(best_fit$classification))
#'   list(Xnew = Xnew,
#'        NEC.mat = NEC.mat,
#'        best_fit = best_fit,
#'        best_seed = best_seed,
#'        best_cluster_num = best_cluster_num,
#'        Xclust = Xclust)
#' }
#' 
#' ###############################################################################
#' ## 4. GMM Clustering Results Visualization
#' #' Create Boxplots of GMM Clustering Results by Group
#' #'
#' #' Converts the clustering result data frame into long format and creates boxplots
#' #' of factor scores for each cluster. The plot is saved as a PDF.
#' #'
#' #' @param res.GMM.best A list returned from \code{main.GMM.best}.
#' #' @param title Title prefix for the output file.
#' #' @export
#' main.GMM.boxplot <- function(res.GMM.best, title = "GMM_Boxplot") {
#'   require(ggpubr)
#'   require(tidyr)
#'   df <- res.GMM.best$Xclust
#'   df_long <- pivot_longer(df, cols = -class, names_to = "Factor", values_to = "Score")
#'   df_long$class <- as.factor(df_long$class)
#'   df_long$Factor <- factor(df_long$Factor, levels = c("O", "C", "E", "A", "N"))
#'   p <- ggpubr::ggboxplot(df_long, x = "Factor", y = "Score", fill = "Factor", palette = "jco",
#'                          outlier.shape = 3, bxp.errorbar = TRUE,
#'                          xlab = "Factors", ylab = "Factor Scores", legend = "none") +
#'     facet_wrap(~ class, nrow = 1)
#'   ggsave(filename = sprintf("%s-GMM.best-Boxplot.pdf", title), plot = p, height = 3, width = 10)
#' }
#' 
#' #' Alternative Boxplot for GMM Clustering Results
#' #'
#' #' Creates an alternative boxplot (with the same data in long format) and saves it as a PDF.
#' #'
#' #' @param Xclust A data frame with clustering results.
#' #' @param wh.best A vector with best cluster number and seed (e.g. c(cluster, seed)).
#' #' @param title Title prefix for the output file.
#' #' @export
#' main.GMM.boxplot2 <- function(Xclust, wh.best, title = "GMM_Boxplot2") {
#'   require(ggpubr)
#'   require(tidyr)
#'   df_long <- pivot_longer(Xclust, cols = -class, names_to = "Factor", values_to = "Score")
#'   df_long$class <- as.factor(df_long$class)
#'   df_long$Factor <- factor(df_long$Factor, levels = c("O", "C", "E", "A", "N"))
#'   p <- ggpubr::ggboxplot(df_long, x = "Factor", y = "Score", fill = "Factor", palette = "jco",
#'                          outlier.shape = 3, bxp.errorbar = TRUE,
#'                          xlab = "Factors", ylab = "Factor Scores", legend = "none") +
#'     facet_wrap(~ class, nrow = 1)
#'   ggsave(filename = sprintf("%s-Boxplot-seed=%s,nclust=%s.pdf", title, wh.best[2], wh.best[1]),
#'          plot = p, height = 3, width = 10)
#' }
#' 
#' ###############################################################################
#' ## 5. Enrichment Analysis
#' #' Reassign Cluster Labels for Enrichment Analysis
#' #'
#' #' For observations not already assigned to a set of selected clusters,
#' #' reassign them based on the minimum Euclidean distance to the cluster centers.
#' #'
#' #' @param Xnew A numeric matrix of scores.
#' #' @param class A vector of current cluster assignments.
#' #' @param mod A fitted mclust model.
#' #' @param wh.class A vector of cluster labels to keep.
#' #' @return A new vector of cluster assignments.
#' #' @export
#' reassign.enrichment <- function(Xnew, class, mod, wh.class) {
#'   if (all(class %in% wh.class)) return(class)
#'   Xnew_sub <- Xnew[!class %in% wh.class, , drop = FALSE]
#'   centre <- t(mod$parameters$mean)[wh.class, , drop = FALSE]
#'   n <- nrow(Xnew_sub)
#'   nclust <- nrow(centre)
#'   dist.mat <- matrix(NA, n, nclust)
#'   for (j in seq_len(n)) {
#'     dist.mat[j, ] <- apply(centre, 1, function(center) norm(as.matrix(Xnew_sub[j, ] - center), type = "2"))
#'   }
#'   cluster_assignment <- wh.class[apply(dist.mat, 1, which.min)]
#'   new_class <- class
#'   new_class[!class %in% wh.class] <- cluster_assignment
#'   new_class
#' }
#' 
#' #' Fit Enrichment Analysis via Permutation
#' #'
#' #' For a given GMM model, this function permutes the data repeatedly and computes
#' #' the density of the cluster centers under the permuted models. It then compares
#' #' the observed density with the permutation distribution to obtain relative densities and p-values.
#' #'
#' #' @param mod A fitted mclust model.
#' #' @param BIC Optional BIC values for model selection.
#' #' @param n.clust Number of clusters.
#' #' @param nrep Number of permutations.
#' #' @param seed Seed for permutation.
#' #' @return A list containing observed density, permuted densities, relative density, and p-values.
#' #' @export
#' fit.enrichment <- function(mod, BIC = NULL, n.clust = NULL, nrep = 100, seed) {
#'   require(mclust)
#'   centre <- mod$parameters$mean
#'   params <- mod$parameters
#'   modelName <- mod$modelName
#'   data <- mod$data
#'   rho_obs <- t(centre) %>% dens(modelName = modelName, parameters = params)
#'   out.result <- lapply(1:nrep, function(iii) {
#'     if (iii %% 10 == 0) message("Permutation: ", iii)
#'     set.seed(iii)
#'     Xperm <- matrix(sample(data), nrow = nrow(data), ncol = ncol(data))
#'     if (!is.null(BIC)) {
#'       set.seed(seed)
#'       mod.perm2 <- Mclust(Xperm, x = BIC, modelNames = modelName, verbose = FALSE)
#'     } else {
#'       set.seed(seed)
#'       mod.perm2 <- Mclust(Xperm, G = n.clust, modelNames = modelName, verbose = FALSE)
#'     }
#'     dens(modelName = modelName, data = t(centre), parameters = mod.perm2$parameters)
#'   })
#'   rho_perm <- do.call(rbind, out.result)
#'   TMP <- matrix(rep(colMeans(rho_obs), each = nrow(rho_perm)), ncol = nrow(rho_perm))
#'   rho_rel <- colMeans(t(TMP) / rho_perm)
#'   pvalues <- apply(t(TMP) / rho_perm, 2, function(x) (sum(x < 1) + 1) / (length(x) + 1))
#'   list(rho_obs = rho_obs, rho_perm = rho_perm, rho_rel = rho_rel, pvalues = pvalues)
#' }
#' 
#' #' Run Enrichment Analysis for a GMM Model
#' #'
#' #' Uses the fitted GMM model and clustering results to perform enrichment analysis.
#' #' Saves a diagnostic enrichment plot and returns the results.
#' #'
#' #' @param fit.GMM The fitted GMM model.
#' #' @param Xclust The clustering result data frame.
#' #' @param Xnew The numeric matrix of factor scores.
#' #' @param title Title prefix for output files.
#' #' @param seed Seed for permutation.
#' #' @param n.clust Optional number of clusters.
#' #' @param nrep Number of permutations.
#' #' @return A list containing the original scores, updated clustering, the fitted model, and enrichment analysis results.
#' #' @export
#' main.Enrichment <- function(fit.GMM, Xclust, Xnew, title, seed, n.clust = NULL, nrep = 100) {
#'   set.seed(1)
#'   res.enrichment <- fit.enrichment(mod = fit.GMM, n.clust = n.clust, nrep = nrep, seed = seed)
#'   pdf_file <- sprintf("%s-Enrichment.pdf", title)
#'   pdf(pdf_file, width = 8, height = 4)
#'   with(res.enrichment, {
#'     plot(rho_rel, log10(pvalues), pch = 18, cex = 1.2,
#'          xlab = bquote("Enrichment, "~rho/tilde(rho)),
#'          ylab = bquote(log[10]~"p-value"),
#'          ylim = c(1.1 * min(log10(pvalues)), 0))
#'     text(rho_rel, rep(0.2, length(rho_rel)), labels = 1:length(rho_rel), cex = 0.5, xpd = NA)
#'   })
#'   dev.off()
#'   SigCluster <- order(res.enrichment$rho_rel, decreasing = TRUE)[which(sort(res.enrichment$pvalues, decreasing = FALSE) < 0.05)]
#'   message("Significant clusters: ", paste(SigCluster, collapse = ", "))
#'   # Optionally, one might reassign cluster labels here using reassign.enrichment()
#'   Xclust2 <- Xclust
#'   list(Xnew = Xnew, Xclust2 = Xclust2, fit.GMM = fit.GMM, res.enrichment = res.enrichment)
#' }
#' 
#' ###############################################################################
#' # End of File: R/analysis.R
#' ###############################################################################
#' 
#' 
#' 
#' 
#' 
#' 
#' library(GenericDataAnalysis)
#' 
#' # 1. 데이터 추출 (예: SPSS나 CSV 파일에서 읽은 데이터 프레임)
#' # df <- read_spss_data("your_data.sav")  # 사용자 정의 데이터 로딩 함수 활용 가능
#' resultData <- main.Data(df, factor_names = c("O", "C", "E", "A", "N"), id_col = "ID", title = "MyStudy")
#' 
#' # 2. 요인분석 수행
#' fa_result <- main.FA(resultData$X, resultData$ID, nfactors = NULL, nfactors2 = NULL, title = "MyStudy")
#' # 요인분석 후 얻은 요인 점수는 fa_result$Xnew
#' 
#' # 3. GMM 클러스터링
#' gmm_result <- main.GMM(Xnew = fa_result$Xnew, max.clust = 10, title = "MyStudy", seed = 42)
#' 
#' # 4. 여러 Seed를 이용한 최적 GMM 선택
#' gmm_best <- main.GMM.best(Xnew = fa_result$Xnew, max.clust = 10, title = "MyStudy", max.seed = 50)
#' 
#' # 5. 클러스터별 박스플롯 생성
#' main.GMM.boxplot(res.GMM.best = gmm_best, title = "MyStudy")
#' main.GMM.boxplot2(Xclust = gmm_best$Xclust, wh.best = c(gmm_best$best_cluster_num, gmm_best$best_seed), title = "MyStudy")
#' 
#' # 6. Enrichment 분석 수행
#' enrich_result <- main.Enrichment(fit.GMM = gmm_best$best_fit, Xclust = gmm_best$Xclust, 
#'                                  Xnew = fa_result$Xnew, title = "MyStudy", seed = 123, nrep = 100)
