#' @export png.forest.df
png.forest.df <- function(result, DV.name, class=NULL, width=30){
  library(forestploter)
  library(tidyverse)
  library(grid)
  
  # result=result.CompReg
  
  nclass <- length(result$fit.reg$beta)
  if(is.null(class)){
    class = 1:nclass
  }
  
  nsmall=3
  
  df.forest <- data.frame(
    # class= class,
    class= paste0("cluster.", class),
    ` `=c(paste(rep(" ", width), collapse = " "), rep("",nclass-1)),
    beta=result$fit.reg$beta %>% round(nsmall),
    se=result$fit.reg$se %>% round(nsmall),
    lower=result$conf.int[,"lower"] %>% round(nsmall),
    upper=result$conf.int[,"upper"] %>% round(nsmall),
    pvalue=result$pvalue %>% format(., scientific=TRUE, digits=3) %>% {ifelse( .== "9.99e-04", "<0.001", .)},
    DV=c(DV.name,rep("",nclass-1)),
    N=c(result$n,rep("",nclass-1))
  )
  # df.forest$class <- factor( df.forest$class, levels=paste0("class.", c(1,3,4,5,6)), labels=c("Conventional", "Socialite", "Average", "Unsettled", "Role Model") )
  
  colnames(df.forest)[2] <- " "
  df.forest$ci <- with( df.forest, paste0( beta %>% {format(round(., digits = nsmall), nsmall=nsmall)}, " (", 
                                           lower %>% {format(round(., digits = nsmall), nsmall=nsmall)}, ", ", 
                                           upper %>% {format(round(., digits = nsmall), nsmall=nsmall)}, ")") )
  
  
  tm <- forest_theme(base_size = 10,
                     title_gp = gpar(cex=0.8),
                     refline_gp = gpar(col="red"),
                     footnote_gp = gpar(col="#636363"))
  
  p <- forest(df.forest %>% dplyr::select(Predictor=class, " ", "Std.Coef. (CI)"=ci, "P-value"=pvalue),
              est = df.forest$beta %>% round(nsmall),
              lower = df.forest$lower %>% round(nsmall),
              upper = df.forest$upper %>% round(nsmall),
              ci_column=2,
              theme=tm,
              title=paste0("DV=",DV.name, ", N=", result$n),
              # xlim = c(0, df.forest$upper %>% max %>% {.*10} %>% ceiling %>% {./10} ),
              sizes = 0.35
  )
  
  p
}





#' @export png.CompReg
png.CompReg <- function(X0, Y, type=c("bootstrap", "parametric")){
  library(MASS)
  
  TYPE <- type #c("bootstrap", "parametric")[1]
  
  result <- NULL
  for( k in 1:ncol(Y) ){
    
    Yk <- Y[,k]
    
    X <- X0[!is.na(Yk),]
    y <- Yk[!is.na(Yk)]
    
    n=nrow(X); p=ncol(X)
    
    X.clr <- X %>% compositions::clr()
    X.log <- X %>% log()
    
    library(MASS)
    fit.reg <- CompReg( X.log, scale(y) )
    
    
    if(TYPE == "bootstrap"){
      
      
      # boot.beta <- lapply(1:1000, function(i){
      #   idx <- sample(nrow(X.log), replace=TRUE)
      #   CompReg( X.log[idx,], scale(y[idx]) )$beta
      # })
      # conf.int <- apply( do.call("rbind", boot.beta), 2, quantile, c(0.025, 0.975) )
      
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
      # result.k$pvalue <- 2 * pt(abs(fit.reg$t), n-p+1, lower.tail=FALSE)
      
      
      result[[k]] <- result.k
      
      
      
    } else {
      
      
      
      yhat <- fit.reg$mu + X.log %*% fit.reg$beta
      hist(scale(y) - yhat)
      shapiro.test(scale(y) - yhat)
      qqnorm(scale(y) - yhat)
      
      
      if(FALSE){
        fit <- lc.reg(y, X)
        fit$be
        c(fit$be[1], fit$be[-1]-fit$be[6])
        lm(y~1+X.clr)
        lm(y~-1+X.clr)
        fit.reg$beta
        
        c(fit.reg$mu, fit.reg$beta)
        
        fit.reg$beta-fit.reg$beta[5]
        
        
        lm(y~-1+X.clr)
        fit.reg$beta-fit.reg$beta[5]
        
        c(fit.reg$mu, fit.reg$beta)
        
      }
      
      
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



