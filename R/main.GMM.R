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



#' @export main.NEC
main.NEC <- function(Xnew=NULL, ID=NULL, max.clust=10, seed.seq=1:100, title=NULL, seed,
                       sampling_prop=1, print.pdf=TRUE, plot.prop=1.0, cex=0.4){
  
  library(mclust) # for GMM
  
  result <- NULL
  for( seed in seed.seq ){
    
    fit.mclust <- NULL
    for( i in 1:max.clust ){
      # set.seed(62)
      # set.seed(97)
      set.seed(seed)
      fit.mclust[[i]] <- Mclust(Xnew, G=i, modelNames="VVV")
    }
    base_loglik <- fit.mclust[[1]]$loglik
    nec.vec <- sapply(fit.mclust, calculate_nec, base_loglik=base_loglik)
    n.clust <- which.min(nec.vec)
    
    print(nec.vec)
    
    pdf(file=paste0(title,"-GMM-NEC-seed="%+%seed%+%".pdf"), width=6, height=4)
    plot(nec.vec, type="b", xlab="The number of clusters", ylab="Normalized Entropy Criterion")
    dev.off()
    extrafont::embed_fonts(paste0(title,"-GMM-NEC-seed="%+%seed%+%".pdf"))
    
    result[[seed]] <- fit.mclust
  }
  
  return(result)
  
}






#' @export main.GMM.best
main.GMM.best <- function(Xnew=NULL, ID=NULL, max.clust=10, path=NULL, title=NULL, max.seed=100, print.pdf=TRUE, type="NEC"){
  
  library(mclust) # for GMM
  
  # set.seed(123)
  # # wh.sample <- sample(1:nrow(Xnew), floor(nrow(Xnew)*0.2))
  # wh.sample <- sample(1:nrow(Xnew), floor(nrow(Xnew)*0.2))
  
  
  
  
  
  # library(otrimle)
  # crit.array <- array(NA, dim=c(max.seed, max.clust, 4))
  # for(j in 1:max.seed){
  #   fit.mclust <- NULL
  #   for(i in 1:max.clust){
  #     set.seed(j)
  #     fit.mclust[[i]] <- mclust::Mclust(Xnew, G=i, modelNames="VVV")
  #     model <- fit.mclust[[i]]
  #     
  #     if(i==1){
  #       base_loglik <- fit.mclust[[1]]$loglik
  #     } else if(i > 1){
  #       nec.vec <- calculate_nec(model=model, base_loglik=base_loglik)
  #       crit.array[j,i,1] <- nec.vec
  #     }
  #     
  #     crit.array[j,i,2] <- mclust::icl(model)
  #     
  #     
  #     # 엔트로피 기반 평가: 모델의 분류 확률 행렬(model$z)를 이용해 전체 엔트로피 계산
  #     compute_entropy <- function(z) {
  #       # 0인 값은 아주 작은 값으로 대체하여 로그 계산 문제 회피
  #       z[z == 0] <- .Machine$double.eps
  #       -sum(z * log(z))
  #     }
  #     entropy_value <- compute_entropy(model$z)
  #     crit.array[j,i,3] <- entropy_value
  #     
  #     
  #     # Classification Likelihood Criterion (CLC) 계산:
  #     # CLC = -2 * (로그 가능도) + 2 * (엔트로피)
  #     clc_value <- -2 * model$loglik + 2 * entropy_value
  #     crit.array[j,i,4] <- clc_value
  #     
  #   }
  # }
  # 
  # save(crit.array, file="crit.array.RData")
  # 
  # crit.array[,,1] %>% t %>% {which(.==sort(.)[1], arr.ind=TRUE)}
  # crit.array[,-1,2] %>% t %>% {which(.==sort(.)[1], arr.ind=TRUE)}
  # crit.array[,-1,3] %>% t %>% {which(.==sort(.,decr=T)[1], arr.ind=TRUE)}
  # crit.array[,-1,4] %>% t %>% {which(.==sort(.)[1], arr.ind=TRUE)}
  
  
  
  
  NEC.mat <- matrix(NA, max.seed, max.clust)
  for(j in 1:max.seed){
    fit.mclust <- NULL
    for(i in 1:max.clust){
      set.seed(j)
      fit.mclust[[i]] <- Mclust(Xnew, G=i, modelNames="VVV")
      
      if(i==1){
        base_loglik <- fit.mclust[[1]]$loglik
      } else if(i > 1){
        nec.vec <- calculate_nec(model=fit.mclust[[i]], base_loglik=base_loglik)
        NEC.mat[j,i] <- nec.vec
      }
      
    }
  }
  
  NEC.mat[c(44,168),4] <- NA
  NEC.mat[c(168),3] <- NA
  
  NEC.mat[,5] %>% sort %>% head
  NEC.mat[,5] %>% order %>% head
  NEC.mat[,4] %>% sort %>% head
  NEC.mat[,4] %>% order %>% head
  
  wh.best <- t(NEC.mat) %>% {which(.==sort(.)[1], arr.ind=TRUE)}
  
  # pdf(path%+%title%+%"-GMM-NEC-TotalSeeds-MeanCurve.pdf", height=4, width=8)
  # t(NEC.mat) %>% { matplot( cbind(rowMeans(.), .), type="l", lty=c(1,rep(2,ncol(.))), col=c("red", rep("gray50", ncol(.))), ylab="Normalized Entropy Criterion", xlab="The number of clusters") }
  # dev.off()
  # extrafont::embed_fonts(title%+%"-GMM-NEC-TotalSeeds-MeanCurve.pdf")
  
  
  pdf(path%+%title%_%"[4]GMM-NEC-TotalSeeds.pdf", height=4, width=8)
  t(NEC.mat) %>% { matplot(., type="l", lty=rep(2,ncol(.)), col=c(rep("gray50", ncol(.))), ylab="Normalized Entropy Criterion", xlab="The number of clusters") }
  wh.best <- t(NEC.mat) %>% { which(.==sort(.)[1], arr.ind=TRUE) }
  t(NEC.mat) %>% { points(wh.best[1], .[wh.best[1],wh.best[2]], col="red", pch=18) }
  abline(v=wh.best[1], col="red", lty=2)
  dev.off()
  extrafont::embed_fonts(path%+%title%_%"[4]GMM-NEC-TotalSeeds.pdf")
  
  
  pdf(path%+%title%_%"[4]GMM-NEC-sorted.pdf", width=10, height=5)
  plot(sort(NEC.mat[,wh.best[1]]), type='l', ylab="Normalized Entropy Criterion")
  dev.off()
  
  
  
  for(ii in rev(1:3)){
    wh.best.ii <- t(NEC.mat) %>% {which(.==sort(.)[ii], arr.ind=TRUE)}
    set.seed(wh.best.ii[2])
    fit.best.ii <- Mclust(Xnew, G=wh.best.ii[1], modelNames="VVV")
    fit.best.ii$seed <- wh.best.ii[2]
    
    Xclust.ii <- cbind.data.frame(Xnew, class=fit.best.ii$classification %>% as.factor)
    attr(Xclust.ii, "seed") <- wh.best.ii[2]
    
    main.GMM.boxplot2(Xclust.ii, wh.best=wh.best.ii, title=path%+%title%+%"GMM-Boxplot_top"%+%ii)
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






#' @export main.GMM.boxplot
main.GMM.boxplot <- function(res.GMM.best, path, title){
  Xclust <- res.GMM.best$Xclust
  wh.best <- res.GMM.best$wh.best
  
  library(ggpubr)
  # Xclust <- cbind.data.frame(Xnew, class=fit.best$classification)
  Xclust.df <- Xclust %>% gather(Factors, value, -class) %>% 
    mutate(class=as.factor(class),
           Factors=factor(Factors, levels=c("O","C","E","A","N")))
  
  Xclust.df %>% 
    ggpubr::ggboxplot(x="Factors", y="value", fill="Factors", palette="jco", 
                      outlier.shape=3, bxp.errorbar=TRUE, 
                      xlab="Factors", ylab="Factor Scores", legend="none") %>% 
    facet(facet.by="class", nrow=1)
  
  ggsave(filename=title%_%"GMM.best-Boxplot-seed="%+%wh.best[2]%+%",nclust="%+%wh.best[1]%+%".pdf", height=3, width=10)

}





#' @export main.GMM.boxplot2
main.GMM.boxplot2 <- function(Xclust, wh.best, title){
  library(tidyr)
  
  # Xclust <- res.GMM.best$Xclust
  # wh.best <- res.GMM.best$wh.best
  
  library(ggpubr)
  # Xclust <- cbind.data.frame(Xnew, class=fit.best$classification)
  Xclust.df <- Xclust %>% slice_sample(prop=1) %>% gather(Factors, value, -class) %>% 
    mutate(class=as.factor(class),
           Factors=factor(Factors, levels=c("O","C","E","A","N")))
  
  Xclust.df %>% 
    ggpubr::ggboxplot(x="Factors", y="value", fill="Factors", palette="jco", 
                      outlier.shape=3, bxp.errorbar=TRUE, 
                      xlab="Factors", ylab="Factor Scores", legend="none") %>% 
    facet(facet.by="class", nrow=1)
  
  ggsave(filename=title%+%"-seed="%+%wh.best[2]%+%",nclust="%+%wh.best[1]%+%".pdf", height=3, width=10)
  
}














#' @export main.NVI
main.NVI <- function(Xnew, nclust, NEC.mat, top=20){
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
        VI_val <- NVI(class.list[[i]], class.list[[j]])
        VI_matrix[i,j] <- VI_val
        VI_matrix[j,i] <- VI_val
      }
    }
    
    mean_NVI <- rowMeans(VI_matrix)
    best_run_NVI <- order(mean_NVI, decreasing=FALSE)
    cat("가장 안정적인 클러스터링 결과는 seed", rank.NEC[best_run_NVI[1]], "에서 나옴.\n")
    
    pdf(path%+%title%+%"_[4]NVI_lineplot.pdf", height=4, width=8)
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
        main.GMM.boxplot2(Xclust=Xclust.h,
                          wh.best=c(nclust,seed),
                          title=path%+%title%_%"[4]NVI"%+%length(class.list)%+%"-"%+%h)
      }
    }
  }
  
  
  {
    
    fit.best.NVI <- fit.best.list[[ best_run_NVI[1] ]]
    fit.best.NVI$seed <- rank.NEC[ best_run_NVI[1] ]
    
    Xclust.NVI <- cbind.data.frame(Xnew, class=fit.best.NVI$classification %>% as.factor)
    attr(Xclust.NVI, "seed") <- fit.best.NVI$seed
    
    main.GMM.boxplot2(Xclust.NVI, wh.best=c(nclust, fit.best.NVI$seed), title=path%+%title%_%"[4]NVI-best-")
    
    
  }
  
  
  result <- NULL
  result$VI_matrix <- VI_matrix
  result$Xclust.NVI <- Xclust.NVI
  result$fit.best.NVI <- fit.best.NVI
  result$wh.best.NVI <- c(nclust, rank.NEC[ best_run_NVI[1] ])
  
  result
}
