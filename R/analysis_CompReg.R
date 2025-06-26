#' @title Run Compositional Regression Analysis
#' @description A high-level wrapper function to perform compositional regression for
#' one or more dependent variables, with options for parametric or non-parametric inference.
#' @importFrom stats qt pt
#' @importFrom compositions acomp clr
#'
#' @details
#' This is the main driver function for the compositional regression analysis. It iterates through each dependent variable (each column in `Y`) and performs a full analysis.
#' The process for each DV is:
#' 1.  Handle missing values.
#' 2.  Apply a log-transformation to the compositional predictors `X0`.
#' 3.  Fit the core `CompReg` model.
#' 4.  Perform statistical inference based on the `type` parameter:
#'     -   `type = "parametric"`: Calculates standard errors, confidence intervals, and p-values based on the t-distribution, assuming model errors are normally distributed.
#'     -   `type = "bootstrap"`: Calls `bootstrap_test` and `permute_test` to derive confidence intervals and p-values from resampling, which does not rely on distributional assumptions.
#'
#' @param X0 A numeric matrix of raw compositional predictors (proportions).
#' @param Y A numeric matrix or data frame where each column is a dependent variable.
#' @param type A string specifying the inference method: `"parametric"` or `"bootstrap"`.
#'
#' @return A list of lists. Each top-level element corresponds to a dependent variable in `Y` and contains the full regression result (estimates, CIs, p-values, etc.).
#'
#' @export png.CompReg
png.CompReg <- function(X0, Y, type=c("bootstrap", "parametric")){
  # library(MASS)
  
  TYPE <- type #c("bootstrap", "parametric")[1]
  
  result <- NULL
  for( k in 1:ncol(Y) ){
    
    Yk <- Y[,k]
    
    X <- X0[!is.na(Yk),]
    y <- Yk[!is.na(Yk)]
    
    n=nrow(X); p=ncol(X)
    
    X.clr <- X %>% compositions::clr()
    X.log <- X %>% log()
    
    
    fit.reg <- CompReg( X.log, scale(y) )
    
    
    if(TYPE == "bootstrap"){
      
      fit.boot <- bootstrap_test(X.log, scale(y), 1000)
      fit.perm <- permute_test(X.log, scale(y), 1000, 0.05)
      
      conf.int <- fit.boot$per_int[,2:3]
      colnames(conf.int) <- c("lower", "upper")
      conf.int <- apply(conf.int, 2, as.numeric)
      
      result.k <- NULL
      result.k$n <- n
      result.k$fit.reg <- fit.reg
      result.k$conf.int <- conf.int
      result.k$pvalue <- as.numeric(fit.perm$p_value_beta$p_value)
      
      result[[k]] <- result.k
      
    } else {
      
      yhat <- fit.reg$mu + X.log %*% fit.reg$beta
      hist(scale(y) - yhat)
      shapiro.test(scale(y) - yhat)
      qqnorm(scale(y) - yhat)
      
      
      # if(FALSE){
      #   fit <- lc.reg(y, X)
      #   fit$be
      #   c(fit$be[1], fit$be[-1]-fit$be[6])
      #   lm(y~1+X.clr)
      #   lm(y~-1+X.clr)
      #   fit.reg$beta
      #   
      #   c(fit.reg$mu, fit.reg$beta)
      #   
      #   fit.reg$beta-fit.reg$beta[5]
      #   
      #   lm(y~-1+X.clr)
      #   fit.reg$beta-fit.reg$beta[5]
      #   
      #   c(fit.reg$mu, fit.reg$beta)
      # }
      
      result.k <- NULL
      result.k$n <- n
      result.k$fit.reg <- fit.reg
      result.k$conf.int <- cbind(
        lower=fit.reg$beta - qt(1-0.05/2, n-p+1) * fit.reg$se,
        upper=fit.reg$beta + qt(1-0.05/2, n-p+1) * fit.reg$se
      )
      result.k$pvalue <- 2 * pt(abs(fit.reg$t), n-p+1, lower.tail=FALSE)
      
      result[[k]] <- result.k
      
    }
    
    
  }
  
  result
}





#' @title Fit a Compositional Regression Model
#' @description Fits a linear regression model with compositional predictors using a
#' constrained least-squares approach.
#' @importFrom MASS ginv
#'
#' @details
#' This function implements the compositional regression model as described in SMMR (Statistical Methods in Medical Research). Compositional predictors (\eqn{X}) are variables that represent proportions of a whole, and thus have a sum-to-k constraint (e.g., sum to 1). Standard OLS is not appropriate for such data.
#'
#' This model uses log-transformed predictors (\eqn{X_{log} = \log(X)}) and estimates coefficients (\eqn{\beta}) under the constraint that their sum is zero: \eqn{\sum_{j=1}^{p} \beta_j = 0}. This constraint ensures model identifiability and allows for proper interpretation of the coefficients. The estimated model is:
#' \deqn{ y = \mu + X_{log} \beta + \epsilon \quad \text{subject to} \quad \sum \beta_j = 0 }
#' The coefficient \eqn{\beta_j} is interpreted as the effect of increasing the proportion of component \eqn{j} relative to the geometric mean of all components. The function uses a projection matrix approach to solve this constrained optimization problem and provides estimates for the intercept (\eqn{\mu}), coefficients (\eqn{\beta}), standard errors (`se`), and t-statistics (`t`).
#'
#' @param X A numeric matrix of log-transformed compositional predictors.
#' @param y A numeric vector for the response variable.
#'
#' @return A list containing:
#' \item{mu}{The estimated intercept.}
#' \item{beta}{A named vector of the estimated regression coefficients.}
#' \item{t}{A named vector of the t-statistics for each coefficient.}
#' \item{se}{A named vector of the standard errors for each coefficient.}
#' \item{M, inv, Q, A}{Matrices used in the constrained estimation process.}
#'
#' @export CompReg
CompReg <- function(X,y){
  # https://jung.snu.ac.kr/papers/SMMR_R2.pdf
  
  n <- nrow(X); p <- ncol(X)
  D <- t(X) %*% X
  inv <- ginv(D)
  Q <- rep(1, ncol(X))
  A <- t(Q)%*%inv%*%Q
  
  #### estimate in original data ####
  beta_ols <- inv%*%t(X)%*%y
  M <- diag(ncol(X))-inv%*%Q%*%solve(A)%*%t(Q)
  
  ## estimate beta
  beta_hat <- M%*%beta_ols; beta_hat <- as.vector(t(beta_hat))
  names(beta_hat) <- colnames(X) 
  
  muhat <- mean( y - X %*% beta_hat )
  
  ## t-statistic
  
  # XinvXty <- 0
  # for( j1 in 1:p ){
  #   for( j2 in 1:p ){
  #     XinvXty = XinvXty + inv[j1,j2] * tcrossprod( X[,j1], crossprod(X[,j2],y) )
  #   }
  # }
  
  SSE_o <- t(y) %*% (y - X %*% inv %*% crossprod(X,y)) + t(beta_ols)%*%Q%*%solve(A)%*%t(Q)%*%beta_ols
  var_beta_o <- as.vector(SSE_o)*( M %*% inv) /(n-p+1)
  t_ref <- beta_hat/sqrt(diag(var_beta_o))
  
  value <- list(mu=muhat, beta = beta_hat, t= t_ref, M = M, inv = inv, Q=Q, A=A, se=sqrt(diag(var_beta_o)))
  return(value)
}




#' @title Compute Statistics for Compositional Regression
#' @description An internal helper function to compute coefficients and t-statistics
#' for compositional regression. This is nearly identical to `CompReg`.
#' @importFrom MASS ginv
#'
#' @param X A numeric matrix of log-transformed compositional predictors.
#' @param y A numeric vector for the response variable.
#'
#' @return A list of key statistics and matrices required for inference.
#' @keywords internal
#' 
#' @export stat
stat <- function(X,y){
  
  n <- nrow(X); p <- ncol(X)
  D <- t(X) %*% X
  inv <- ginv(D)
  Q <- rep(1, ncol(X))
  A <- t(Q)%*%inv%*%Q
  
  #### estimate in original data ####
  beta_ols <- inv%*%t(X)%*%y
  M <- diag(ncol(X))-inv%*%Q%*%solve(A)%*%t(Q)
  
  ## estimate beta
  beta_hat <- M%*%beta_ols; beta_hat <- as.vector(t(beta_hat))
  names(beta_hat) <- colnames(X) 
  
  ## t-statistic
  SSE_o <- t(y)%*%(diag(nrow(X))-X%*%inv%*%t(X))%*%y + t(beta_ols)%*%Q%*%solve(A)%*%t(Q)%*%beta_ols
  var_beta_o <- as.vector(SSE_o)*( M %*% inv) /(n-p+1)
  t_ref <- beta_hat/sqrt(diag(var_beta_o))
  
  value <- list(beta = beta_hat, t= t_ref, M = M, inv = inv, Q=Q, A=A, se=sqrt(diag(var_beta_o)))
  return(value)
}







#' @title Permutation Test for Compositional Regression Coefficients
#' @description Performs a non-parametric permutation test to calculate empirical
#' p-values for the coefficients from a `CompReg` model.
#'
#' @details
#' This function assesses the statistical significance of each compositional predictor
#' without relying on the assumption of normally distributed errors. It works by:
#' 1.  Calculating the observed coefficients (`beta_hat`) and t-statistics (`t_ref`) from the original data.
#' 2.  Creating a null distribution by repeatedly (`rep` times) shuffling the response variable `y`. This shuffling breaks the true relationship between predictors and response.
#' 3.  For each shuffled `y`, a new `CompReg` model is fitted, and its coefficients and t-statistics are stored.
#' 4.  The empirical p-value for each predictor is then calculated as the proportion of times the absolute value of the permuted statistic was greater than or equal to the absolute value of the observed statistic. A small p-value indicates that the observed relationship is unlikely to have occurred by chance.
#'
#' @param X_screen A numeric matrix of log-transformed compositional predictors.
#' @param y A numeric vector for the response variable.
#' @param rep The number of permutation replicates to perform.
#' @param level The significance level (alpha) for flagging results.
#'
#' @return A list containing data frames with calculated p-values and significance flags, based on both the beta coefficients and the t-statistics.
#'
#' @export permute_test
permute_test <- function(X_screen, y, rep, level){
  
  X <- X_screen
  
  #### reference value #### 
  ref <- CompReg(X,y)
  beta_hat <- ref$beta ; t_ref <- ref$t; M <- ref$M; inv <- ref$inv; Q <- ref$Q; A <- ref$A
  
  #### permutation ####
  beta_per <- t_per <- NULL 
  
  for(i in 1:rep){
    
    s <- sample(1:length(y))
    per_y <- y[s] ## permute only y
    stat_per <- CompReg(X, per_y)
    
    ## permuted beta
    sol <- stat_per$beta
    
    ## permuted t-statistic
    t_star <- stat_per$t
    beta_per <- cbind(beta_per, sol); t_per <- cbind(t_per, t_star)
  }
  
  p_value_t <- sig_var_t <- p_value_beta <- sig_var_beta <-  NULL
  
  for(i in 1:length(t_ref)){
    
    t_per_i <- t_per[i,]; t_ref_i <- t_ref[i]
    pt <- mean( abs(t_per_i) >= abs(t_ref_i) )
    
    if(pt <= level){
      p_value_t <- rbind(p_value_t, c(pt, "***"))
    }
    else{
      p_value_t <- rbind(p_value_t, c(pt, ""))
    }
    
  }
  
  p_value_t <- cbind(beta_hat, p_value_t)
  
  
  
  for(i in 1:length(beta_hat)){
    b_per_i <- beta_per[i,]; beta_hat_i <- beta_hat[i]
    pbeta <- ( sum( abs(b_per_i) >= abs(beta_hat_i) ) + 1 ) / (length(b_per_i) + 1 )
    
    if(pbeta <= level){
      p_value_beta <- rbind(p_value_beta, c(pbeta, "***"))
    }
    else{
      p_value_beta <- rbind(p_value_beta, c(pbeta, ""))
    }
    
  }
  
  p_value_beta <- cbind(beta_hat, p_value_beta)
  
  
  colnames(p_value_t) <- colnames(p_value_beta) <- c("beta_hat","p_value","significance")
  rownames(p_value_t) <- rownames(p_value_beta) <- names(beta_hat)
  p_value_t <- as.data.frame(p_value_t); p_value_beta <- as.data.frame(p_value_beta)
  
  ind_sig_t <- rownames(p_value_t[which(as.numeric(as.vector(p_value_t$p_value)) <= level),])
  sig_var_t <- as.data.frame(p_value_t[ind_sig_t,]$beta_hat)
  colnames(sig_var_t) <- ""; rownames(sig_var_t) <- ind_sig_t
  
  ind_sig_beta <- rownames(p_value_beta[which(as.numeric(as.vector(p_value_beta$p_value)) <= level),])
  sig_var_beta <- as.data.frame(p_value_beta[ind_sig_beta,]$beta_hat)
  colnames(sig_var_beta) <- ""; rownames(sig_var_beta) <- ind_sig_beta
  
  
  result <- list(beta_hat= beta_hat, p_value_t = p_value_t, sig_var_t = sig_var_t, p_value_beta = p_value_beta, sig_var_beta = sig_var_beta)
  return(result)
}







#' @title Bootstrap Confidence Intervals for Compositional Regression Coefficients
#' @description Constructs non-parametric confidence intervals for `CompReg`
#' coefficients using bootstrapping.
#'
#' @details
#' This function provides a robust method for estimating the uncertainty of the regression coefficients. It implements the standard case resampling bootstrap:
#' 1.  A bootstrap sample is created by drawing `n` observations with replacement from the original data (`X`, `y`).
#' 2.  A `CompReg` model is fitted to this bootstrap sample.
#' 3.  This process is repeated `rep` times to generate a distribution of the estimated coefficients and t-statistics.
#'
#' The function calculates two types of 95% confidence intervals:
#' -   **Percentile Interval**: The 2.5th and 97.5th percentiles of the bootstrapped coefficient distribution.
#' -   **Studentized (Pivotal) Interval**: A more accurate interval that uses the distribution of the bootstrapped t-statistic. It is generally preferred for its better statistical properties.
#'
#' @param X_screen A numeric matrix of log-transformed compositional predictors.
#' @param y A numeric vector for the response variable.
#' @param rep The number of bootstrap replicates to perform.
#'
#' @return A list containing the original estimates (`beta_hat`, `t_ref`) and data frames for the percentile (`per_int`) and studentized (`stud_int`) confidence intervals.
#'
#' @export bootstrap_test
bootstrap_test <- function(X_screen, y, rep){
  
  X <- X_screen
  
  #### reference value #### 
  ref <- CompReg(X,y)
  # ref <- stat(X,y)
  beta_hat <- ref$beta ; t_ref <- ref$t; se <- ref$se
  
  data <- cbind(y, X); colnames(data)[1] <- "y"
  
  #### bootstrap testing - residual resampling ####
  per_boot <- stud_boot <- NULL 
  
  
  for(i in 1:rep){
    
    s <- sample(1:length(y),size=length(y), replace = TRUE)
    boot <- data[s,]
    boot_y <- boot[,1]; boot_X <- boot[,2:ncol(boot)]
    
    stat_boot <- CompReg(boot_X, boot_y)
    # stat_boot <- stat(boot_X, boot_y)
    
    per_boot <- cbind(per_boot, as.vector(stat_boot$beta))
    t_boot <- (stat_boot$beta-beta_hat)/stat_boot$se
    stud_boot <- cbind(stud_boot, t_boot)
  }
  
  per_quant <- stud_quant <-  per_quant1 <- stud_int1 <- NULL
  
  for(j in 1:length(t_ref)){
    per_quant <- rbind(per_quant, quantile(per_boot[j,], c(0.025,0.975),na.rm=TRUE))
    stud_quant <- rbind(stud_quant, quantile(stud_boot[j,], c(0.025,0.975), na.rm = TRUE))
  }
  
  per_quant <- cbind(round(beta_hat,5), round(per_quant,5))
  stud_quant <- cbind(round(beta_hat,5), round(stud_quant,5))
  
  for(j in 1:nrow(per_quant)){
    
    if(sign(as.numeric(per_quant[j,2]))==sign(as.numeric(per_quant[j,3]))){
      per_quant1 <- rbind(per_quant1, c(per_quant[j,],"***"))
    }
    else{
      per_quant1 <- rbind(per_quant1, c(per_quant[j,],""))
    }
  }
  
  #per_quant1 <- per_quant
  rownames(per_quant1) <- names(beta_hat)
  colnames(per_quant1)[1] <- colnames(stud_quant)[1]<- "beta_hat"
  colnames(per_quant1)[4] <- "significance"
  per_quant1 <- as.data.frame(per_quant1); stud_quant <- as.data.frame(stud_quant)
  
  
  ind_sig_per <- rownames(per_quant1[which(sign(as.numeric(as.vector(per_quant1[,2])))==sign(as.numeric(as.vector(per_quant1[,3])))),])
  sig_var_per <- as.data.frame(per_quant1[ind_sig_per,]$beta_hat)
  colnames(sig_var_per) <- ""; rownames(sig_var_per) <- ind_sig_per
  
  stud_int <- matrix(0,nrow(stud_quant),2) 
  for(i in 1:nrow(stud_quant)){
    stud_int[i,1] <- beta_hat[i] - se[i]*stud_quant[i,3]
    stud_int[i,2] <- beta_hat[i] - se[i]*stud_quant[i,2]
  }
  
  rownames(stud_int) <- names(beta_hat)
  stud_int <- cbind(round(beta_hat,5), round(stud_int,5))
  
  for(j in 1:nrow(stud_int)){
    
    if(sign(as.numeric(stud_int[j,2]))==sign(as.numeric(stud_int[j,3]))){
      stud_int1 <- rbind(stud_int1, c(stud_int[j,],"***"))
    }
    else{
      stud_int1 <- rbind(stud_int1, c(stud_int[j,],""))
    }
  }
  
  stud_int1 <- as.data.frame(stud_int1); rownames(stud_int1) <- names(beta_hat)
  colnames(stud_int1) <- c("beta_hat","2.5%","97.5%","significance")
  
  ind_sig_stud <- rownames(stud_int1[which(sign(as.numeric(as.vector(stud_int1[,2])))==sign(as.numeric(as.vector(stud_int1[,3])))),])
  sig_var_stud <- as.data.frame(stud_int1[ind_sig_stud,]$beta_hat)
  colnames(sig_var_stud) <- ""; rownames(sig_var_stud) <- ind_sig_stud
  
  result <- list(beta_hat= beta_hat, t_ref = t_ref, per_int = per_quant1, stud_int = stud_int1, 
                 sig_var_per = sig_var_per, sig_var_stud = sig_var_stud)
  return(result)
  
}



