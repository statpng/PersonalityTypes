# Due to a nondisclosure agreement and joint data ownership between the Center for [blinded] and Kakao Corporation, 
# we are unable to publicly share the raw scores of individual scale items. 
# Instead, the dataset includes factor scores derived from our factor analysis of the IPIP-NEO-120 responses, 
# along with the composite score(s) for each correlate. Although the raw data necessary for conducting a factor analysis are not included, 
# we have provided the analysis code for transparency. The dataset can be analyzed beginning with the code provided for the GMM analysis.

#' @title Perform First and Second-Order Factor Analysis
#' @description Conducts a comprehensive factor analysis workflow, including data
#' suitability tests, determination of the number of factors, fitting a first-order
#' model, and optionally fitting a second-order (hierarchical) model.
#' @importFrom psych KMO cortest.bartlett fa fa.parallel fa.multi
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot
#' @importFrom stats cor
#' 


#'
#' @details
#' This function automates the key steps of Exploratory Factor Analysis (EFA),
#' a statistical method used to uncover the underlying latent structure (factors)
#' from a set of observed variables.
#'
#' The workflow consists of three main parts:
#'
#' 1.  **Assessing Factorability**: Before fitting a model, the function assesses whether the data is suitable for factor analysis using two standard statistical tests:
#'     -   **Kaiser-Meyer-Olkin (KMO) Test**: Calculates the Measure of Sampling Adequacy (MSA). The KMO index represents the ratio of the squared correlation between variables to the squared partial correlation between variables. A value of 0.6 or higher is generally considered acceptable for factor analysis.
#'     -   **Bartlett's Test of Sphericity**: This tests the null hypothesis that the correlation matrix is an identity matrix (i.e., variables are unrelated). A significant p-value (e.g., < 0.05) is required, indicating that the variables are sufficiently intercorrelated to be factored.
#'
#' 2.  **Determining the Number of Factors**: The function uses Parallel Analysis (`psych::fa.parallel`), a robust method for determining the number of factors to retain. This method compares the eigenvalues of the actual data to the eigenvalues from simulated random data. Factors are retained if their eigenvalues are greater than those from the corresponding random data. A scree plot is generated to visualize this. If `nfactors` is not specified, the function defaults to using the Kaiser criterion (retaining factors with eigenvalues > 1).
#'
#' 3.  **Model Fitting and Score Extraction**:
#'     -   **First-Order Model**: A primary factor model is fitted using Principal Axis Factoring (`fm="pa"`), which is a common EFA extraction method. An oblique rotation (`rotate="oblimin"`) is used, which allows the extracted factors to be correlated. This is often theoretically appropriate in social sciences. The function then calculates factor scores, which are estimates of each individual's standing on the latent factors.
#'     -   **Second-Order Model (Optional)**: If `nfactors2` is specified, a hierarchical factor analysis is performed using `psych::fa.multi`. This technique treats the first-order factors as indicators to uncover a higher-level, more general latent structure. The final `Xnew` object will then contain the scores for these second-order factors.
#'
#' The function saves diagnostic plots (scree plot, factor diagrams) as PDF files and returns the final factor scores, which can be used as input for subsequent analyses like clustering or regression.
#'
#' @param X A numeric matrix or data frame of the observed variables (e.g., item responses).
#' @param ID A vector of identifiers for each row in `X`.
#' @param nfactors An optional integer specifying the number of first-order factors. If `NULL` (the default), it is determined automatically via parallel analysis.
#' @param nfactors2 An optional integer specifying the number of second-order factors. If `NULL` (the default), the second-order analysis is skipped.
#' @param filename A base string for the output PDF file names (e.g., scree plots, diagrams).
#'
#' @return A list containing:
#' \item{ID}{The input identifier vector.}
#' \item{X}{The original input data matrix.}
#' \item{Xnew}{A matrix of the final factor scores. This will be the first-order factor scores if `nfactors2` is NULL, or the second-order factor scores otherwise.}
#' \item{console}{A placeholder for console output (currently not used).}
#'
#' @seealso \code{\link[psych]{fa}}, \code{\link[psych]{fa.parallel}}, \code{\link[psych]{fa.multi}}, \code{\link[psych]{KMO}}
#'
#' @export analysis.FA
analysis.FA <- function(X, ID, nfactors=NULL, nfactors2=NULL, filename=NULL){
  if(FALSE){
    X=res.Data$X; ID=res.Data$ID; nfactors=5
  }
  
  library(psych) # for FA
  library(car) # for FA

  console <- NULL  
  # tc <- textConnection("console", "w")
  # sink(tc)
  # sink(tc, type = "message")
  
  
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
  
  pdf(file=paste0("./", filename,"-FA-ScreePlot.pdf"), width=8, height=4)
  plot(parallel, fa="fa", sim=F, se.bars=F)
  dev.off()
  
  pdf(file=paste0("./", filename,"-FA-ScreePlot-simple.pdf"), width=8, height=4)
  parallel$fa.values[1:30] %>% {plot(1:length(.), ., type="b", xlab="The number of factors", ylab="Eigenvalues")}
  dev.off()
  
  
  if(is.null(nfactors)){
    nfactors <- max(which(parallel$fa.values > 1))
  }
  
  cat("> nfactors\n")
  print(nfactors)
  
  
  
  cat("--------A result of factor analysis--------\n")
  
  fit.fa <- fa(X, nfactors=nfactors, rotate="oblimin", scores="regression", fm="pa")
  
  
  pdf(file=paste0("./", filename,"-FA-Diagram.pdf"), width=5, height=18)
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
    
    pdf(file=paste0("./", filename,"-FA-SecondOrder-ScreePlot.pdf"), width=10, height=5)
    plot(parallel2, fa="pc", sim=F, se.bars=F, show.legend=T)
    # plot(parallel2, fa="pc", sim=F, se.bars=F, show.legend=F)
    dev.off()
    
    pdf(file=paste0("./", filename,"-FA-SecondOrder.pdf"), width=6, height=12)
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
  
  
  # sink(type = "message")
  # sink()
  # close(tc)
  
  
  # save(ID, X, Xnew, file=paste0(filename,"-1.FactorAnalysis.RData"))
  
  
  result <- NULL
  result$ID <- ID
  result$X <- X
  result$Xnew <- Xnew
  result$console <- console
  
  result

}
