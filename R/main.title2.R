if(FALSE){
  devtools::load_all()
  
  library(haven) # for SPSS .sav file
  library(lubridate)
  
  # df1 <- haven::read_sav("./datasets/Personality1.sav") %>% drop_na()
  df2_raw <- haven::read_sav("./datasets/230914 BIG5_cluster_DVs_FINAL3.sav") %>% as_tibble()
  df3_raw <- haven::read_sav("./datasets/241231 BIG5_DVs_FINAL3.sav") %>% as_tibble()
  
  save(df2_raw, file="./datasets/BIG5_230914.RData")
  save(df3_raw, file="./datasets/BIG5_241231.RData")
  
}





if(FALSE){
  setwd("..")
  
  path <- "./rdata.title2/"
  title <- c("BIG5_230914", "BIG5_241231")[2]
  
  
  {
    library(png.GMM)
    library(png.utils)
    library(dplyr)
    library(tidyr)
    library(mclust)
    
    `%+%` <- function(x, y) {
      paste0(x, y)
    }
  }
  
  
  
  if(FALSE){
    
    rdata <- R.utils::loadToEnv("./rdata/"%+%title%+%".RData")
    df_raw <- rdata$df2_raw
    
    df <- df_raw %>%
      filter(if_all(matches("^(O|C|E|A|N)\\d+$"), ~ !is.na(.))) %>%
      mutate(age_min = pmin(age_O, age_C, age_E, age_A, age_N, na.rm = TRUE),
             ID=id)
    
    df$gender_vote <- df %>% select(contains("gender")) %>% {
      apply(., 1, function(x) x[order(table(unlist(x)),decreasing=TRUE)[1]] )
    }
    
    df <- df %>%
      mutate(gender_vote=factor(gender_vote, levels=c("남성", "여성", "성별비공개"), labels=c("male", "female", NA)))
    
    save(df, file=path%+%title%_%"[1]df.RData")
    
  }
  
  
  
  
  if(FALSE){
    # rm(list=ls())
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[1]df.RData")
    df <- rdata$df
    
    set.seed(1)
    res.Data <- main.Data(df, strsplit("OCEAN","")[[1]], title=title)
    res.FA <- main.FA(res.Data$X, ID=res.Data$ID, nfactors=5)
    
    writeLines(res.FA$console, path%+%title%_%"[2]res.FA-console.txt")
    save(df, res.Data, res.FA, file=path%+%title%_%"[2]res.FA.RData")
  }
  
  
  
  if(FALSE){
    rm(list=ls())
    
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[2]res.FA.RData")
    df <- rdata$df
    res.Data <- rdata$res.Data
    res.FA <- rdata$res.FA
    
    Xnew <- res.FA$Xnew
    ID <- res.FA$ID
    
    res.GMM.best <- main.GMM.best(Xnew=Xnew, ID=ID, 
                                  max.clust=10, max.seed=100, title=title%+%"[4]")
    
    
    save(df, res.Data, res.FA, res.GMM.best, file=path%+%title%_%"[3]res.GMM.best.RData")
    
  }
  
  
  
  
  
  
  if(FALSE){
    rm(list=ls())
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[3]res.GMM.best.RData")
    df <- rdata$df
    res.Data <- rdata$res.Data
    res.FA <- rdata$res.FA
    ID <- rdata$res.FA$ID
    Xnew <- rdata$res.FA$Xnew
    res.GMM.best <- rdata$res.GMM.best
    NEC.mat <- res.GMM.best$NEC.mat
    wh.best <- res.GMM.best$wh.best
    
    # if(FALSE){
    #   save(df, res.Data, res.FA, res.GMM.best, file=path%+%title%_%"[3]res.GMM.best.RData")
    # }
    
    
    # {
    #   main.GMM.boxplot2(Xclust=res.GMM.best$Xclust,
    #                     wh.best=res.GMM.best$wh.best,
    #                     title=path%+%title%_%"[4]GMM-Boxplot.best")
    # }
    
    
    
    res.NVI <- main.NVI(Xnew=Xnew, nclust=wh.best[1], NEC.mat=NEC.mat, top=20)
    
    Xclust.NVI_cov <- data.frame(ID=res.FA$ID, 
                                 age_min=df$age_min,
                                 gender_vote=df$gender_vote, 
                                 res.NVI$Xclust.NVI)
    
    write.csv(Xclust.NVI_cov, 
              file=path%+%title%_%"[4]Xclust.NVI_cov.csv", quote=TRUE, row.names=FALSE)
    
    save(df, Xclust.NVI_cov, res.NVI, res.Data, res.FA, res.GMM.best, file=path%+%title%_%"[4]res.NVI.RData")
    
    
    
    
    # 
    {
      set.seed(1)
      res.Enrichment.NVI <- main.Enrichment(fit.GMM=fit.best.NVI, Xclust=Xclust.NVI, Xnew=Xclust.NVI, title=title, n.clust=4, seed=order(NEC.mat[,4])[1:20][ best_run_NVI[1] ])
      
      
      save(df, Xclust.NVI_cov, Xclust.NVI, fit.best.NVI, res.Data, res.FA, res.GMM.best, res.Enrichment.NVI, file=path%+%title%_%"[4]res.Enrichment.NVI.RData")
    }
    
    
    # if( best.method == "NEC" ){
    #   set.seed(1)
    #   res.Enrichment <- main.Enrichment(fit.GMM=res.GMM.best$fit.best, Xclust=res.GMM.best$Xclust, Xnew=res.GMM.best$Xclust, title=title, n.clust=res.GMM.best$wh.best[1], seed=res.GMM.best$wh.best[2])
    #   
    #   
    #   save(df, Xclust_cov, res.Data, res.FA, res.GMM.best, res.Enrichment, file=path%+%title%_%"[4]res.Enrichment.RData")
    # }
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  if(FALSE){
    
    rm(list=ls())
    
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[4]res.Enrichment.RData")
    df <- rdata$df
    res.Data <- rdata$res.Data
    res.FA <- rdata$res.FA
    res.GMM.best <- rdata$res.GMM.best
    res.Enrichment <- rdata$res.Enrichment
    
    Xnew=res.FA$Xnew
    n.clust=10
    nrep=100
    seed=res.GMM.best$wh.best[2]
    
    res.Enrichment.range <- main.Enrichment.range(Xnew=res.FA$Xnew, n.clust=10, nrep=100, seed=res.GMM.best$wh.best[2])
    
    pdf(file=paste0(path%+%title,"-[5]Enrichment-NumberOfClustersTrajectory.pdf"), width=8, height=4)
    plot( sapply(res.Enrichment.range, function(x) sum( p.adjust(x$pvalues, method="BH") < 0.05 )), type="b", xlab="Number of candidate clusters", ylab="Number of meaningful clusters")
    dev.off()
    
    save(df, res.Data, res.FA, res.GMM.best, res.Enrichment, res.Enrichment.range, file=path%+%title%_%"[5]res.Enrichment.range.RData")
    
    
  }
  
  
  
  
}





































# Comp Reg ----
if(FALSE){
  rm(list=ls())
  
  
  
  rdata <- R.utils::loadToEnv(path%+%title%_%"[4]res.NVI.RData")
  df <- rdata$df
  res.Data <- rdata$res.Data
  res.FA <- rdata$res.FA
  res.GMM.best <- rdata$res.GMM.best
  res.NVI <- rdata$res.NVI
  Xnew <- rdata$res.FA$Xnew
  
  library(mclust)
  nclust <- res.NVI$wh.best.NVI[1]
  seed <- res.NVI$wh.best.NVI[2]
  # seed <- ifelse(title=="BIG5_241231", 47, 67)
  
  fit.best <- res.NVI$fit.best.NVI
  Xclust <- read.csv(path%+%title%_%"[4]Xclust.NVI_cov.csv")
  
  
  prob <- fit.best$z
  Xreg <- cbind.data.frame(Xclust, prob=prob, df %>% dplyr::select(SE_after_PERSONALITY:MNG_se)
  ) %>% mutate("COMPAR_opin_after_PERSONALITY"=COMPAR_after_PERSONALITY,
               "COMPAR_abil_after_PERSONALITY"=COMPAR_after_PERSONALITY,
               "COMPAR_ach_after_PERSONALITY"=COMPAR_after_PERSONALITY,
               "COMPAR_gen_after_PERSONALITY"=COMPAR_after_PERSONALITY,
               "PA_after_PERSONALITY"=PANA_after_PERSONALITY,
               "NA_after_PERSONALITY"=PANA_after_PERSONALITY,
               "EBH_immut_after_PERSONALITY"=EBH_after_PERSONALITY,
               "EBH_eff_after_PERSONALITY"=EBH_after_PERSONALITY,
               "EBH_bio_after_PERSONALITY"=EBH_after_PERSONALITY,
               "EBHa_after_PERSONALITY"=EBH_after_PERSONALITY,
               "EBHb_after_PERSONALITY"=EBH_after_PERSONALITY,
               "MNG_after_PERSONALITY"=MNG_after_PERSONALITY,
               "MNG_pr_after_PERSONALITY"=MNG_after_PERSONALITY,
               "MNG_se_after_PERSONALITY"=MNG_after_PERSONALITY
  ) %>% 
    mutate(gender_vote=factor(gender_vote, levels=c("남성", "여성", "성별비공개"), labels=c("male", "female", NA)))
  
  
  
  var.list <- c("SE", "GRAT", "SWLS", "COMPAR", "COMPAR_opin", "COMPAR_abil", "COMPAR_ach", "COMPAR_gen", "OPT", "PA", "NA", "STRESS", "EBH_immut", "EBH_eff", "EBH_bio", "EBHa", "EBHb", "SES", "LONE", "MNG", "MNG_pr", "MNG_se")
  
  
  
  
  if(T){
    
    result.CompReg.filter <- NULL
    for(hh in 1:length(var.list)){
      var <- var.list[hh]
      var_filter <- var %+% "_after_PERSONALITY"
      
      
      Xreg.filter <- Xreg %>% 
        mutate(gender_vote=factor(gender_vote, levels=c("남성", "여성", "성별비공개"), labels=c("male", "female", NA))) %>% 
        dplyr::select(age_min, gender_vote, starts_with("prob."), c(var, var_filter)) %>% 
        filter(get(var_filter)==1)
      
      {
        
        X0 <- Xreg.filter %>% dplyr::select(prob.1:prob.4) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Y, type="bootstrap")
        
        }
      
      result.CompReg.filter[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg.filter[[k]][[1]], DV.name=var.list[k], class=1:4, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-filtered-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
    
    
    result.CompReg <- NULL
    for(hh in 1:length(var.list)){
      var <- var.list[hh]
      
      Xreg.filter0 <- Xreg %>% 
        dplyr::select(age_min, gender_vote, starts_with("prob."), c(var, var_filter))
      
      {
        
        X0 <- Xreg.filter0 %>% dplyr::select(prob.1:prob.4) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Y, type="bootstrap")
        
        }
      
      result.CompReg[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg[[k]][[1]], DV.name=var.list[k], class=1:4, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
  }
  
  
  
  
  
  # adjusted
  if(T){
    
    result.CompReg.filter <- NULL
    for(hh in 1:length(var.list)){
      var <- var.list[hh]
      var_filter <- var %+% "_after_PERSONALITY"
      
      
      Xreg.filter <- Xreg %>% 
        dplyr::select(age_min, gender_vote, starts_with("prob."), c(var, var_filter)) %>% drop_na() %>% 
        filter(get(var_filter)==1)
      
      {
        
        X0 <- Xreg.filter %>% dplyr::select(prob.1:prob.4) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        Z <- Xreg.filter %>% dplyr::select(age_min, gender_vote) %>% as.data.frame %>% as.matrix
        data.tmp <- data.frame(y=Y,z=Z)
        colnames(data.tmp)[1]="y"
        Ynew <- lm(y~., data=data.tmp)$residuals %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Ynew, type="bootstrap")
        
        }
      
      result.CompReg.filter[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg.filter[[k]][[1]], DV.name=var.list[k], class=1:4, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-filtered-Adjusted-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
    
    
    
    
    result.CompReg <- NULL
    for(hh in 1:length(var.list)){
      var <- var.list[hh]
      
      Xreg.filter0 <- Xreg %>% 
        dplyr::select(age_min, gender_vote, starts_with("prob."), c(var, var_filter)) %>% drop_na()
      
      {
        
        X0 <- Xreg.filter0 %>% dplyr::select(prob.1:prob.4) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter0 %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        Z <- Xreg.filter0 %>% dplyr::select(age_min, gender_vote) %>% as.data.frame %>% as.matrix
        data.tmp <- data.frame(y=Y,z=Z)
        colnames(data.tmp)[1]="y"
        Ynew <- lm(y~., data=data.tmp)$residuals %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Ynew, type="bootstrap")
        
        }
      
      result.CompReg[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg[[k]][[1]], DV.name=var.list[k], class=1:4, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-Adjusted-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
    
  }
  
  
}



