rm(list=ls())

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
  
  path <- "./rdata.title1/"
  title <- c("BIG5_230914", "BIG5_241231")[1]
  
  
  {
    devtools::load_all()
    
    # library(png.GMM)
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
                                  max.clust=10, max.seed=200, title=title%+%"[4]")
    
    
    
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
    
    save(df, Xclust.NVI_cov, res.Data, res.FA, res.GMM.best, res.NVI, file=path%+%title%_%"[4]res.NVI.RData")
    
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[4]res.NVI.RData")
    
    df <- rdata$df
    res.Data <- rdata$res.Data
    res.FA <- rdata$res.FA
    ID <- rdata$res.FA$ID
    Xnew <- rdata$res.FA$Xnew
    res.GMM.best <- rdata$res.GMM.best
    res.NVI <- rdata$res.NVI
    NEC.mat <- res.GMM.best$NEC.mat
    wh.best <- res.GMM.best$wh.best
    
    
    
    # boxplot-NVI.best ----
    {
      wh.best.NVI <- res.NVI$wh.best.NVI
      fit.best.NVI <- res.NVI$fit.best.NVI
      Xclust.NVI <- res.NVI$Xclust.NVI
      
      
      main.GMM.boxplot3 <- function(Xclust, wh.best, title){
        library(tidyr)
        library(ggpubr)
        Xclust=Xclust.NVI; wh.best=c(wh.best.NVI[1], wh.best.NVI[2]); title=path%+%title%_%"[4_new]NVI-best-"
        
        if(path == "./rdata.title1/"){
          class.levels <- c(1,4,2,5,3)
        } else if(path == "./rdata.title2/"){
          class.levels <- c(4,2,3,5,1)
        }
        
        Xclust.df <- Xclust %>% slice_sample(prop=1) %>% gather(Factors, value, -class) %>% 
          mutate(class=factor(class, levels=class.levels, labels=c("Average", "Inquisitive Individualist", "Unsettled", "Expressive", "Conventional")),
                 Factors=factor(Factors, levels=c("O","C","E","A","N")))
        
        Xclust.df %>% 
          ggpubr::ggboxplot(x="Factors", y="value", fill="Factors", palette="jco", 
                            outlier.shape=3, bxp.errorbar=TRUE, 
                            xlab="Factors", ylab="Factor Scores", legend="none") %>% 
          facet(facet.by="class", nrow=1)
        
        ggsave(filename=title%+%"-seed="%+%wh.best[2]%+%",nclust="%+%wh.best[1]%+%".pdf", height=3, width=10)
        
      }
      
      
      main.GMM.boxplot3(Xclust=Xclust.NVI, wh.best=c(wh.best.NVI[1], wh.best.NVI[2]), title=path%+%title%_%"[4_new]NVI-best-")
      
      
    }
    
    
    
    # 
    {
      set.seed(1)
      res.Enrichment.NVI <- main.Enrichment(fit.GMM=res.NVI$fit.best.NVI, Xclust=res.NVI$Xclust.NVI, Xnew=res.NVI$Xclust.NVI, title=title, n.clust=res.NVI$wh.best.NVI[1], seed=res.NVI$wh.best.NVI[2])
      
      
      save(df, res.Data, res.FA, res.GMM.best, res.NVI, res.Enrichment.NVI, file=path%+%title%_%"[4]res.Enrichment.NVI.RData")
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
    
    
    rdata <- R.utils::loadToEnv(path%+%title%_%"[4]res.Enrichment.NVI.RData")
    df <- rdata$df
    res.Data <- rdata$res.Data
    res.FA <- rdata$res.FA
    res.GMM.best <- rdata$res.GMM.best
    res.NVI <- rdata$res.NVI
    res.Enrichment.NVI <- rdata$res.Enrichment.NVI
    
    Xnew=res.FA$Xnew
    n.clust=10
    nrep=100
    seed=res.NVI$wh.best.NVI[2]
    
    res.Enrichment.range.NVI <- main.Enrichment.range(Xnew=res.FA$Xnew, n.clust=10, nrep=100, seed=seed)
    
    pdf(file=paste0(path%+%title,"_[5]Enrichment-NumberOfClustersTrajectory.pdf"), width=8, height=4)
    plot( sapply(res.Enrichment.range.NVI, function(x) sum( p.adjust(x$pvalues, method="BH") < 0.05 )), type="b", xlab="Number of candidate clusters", ylab="Number of meaningful clusters")
    dev.off()
    
    save(df, res.Data, res.FA, res.GMM.best, res.NVI, res.Enrichment.NVI, res.Enrichment.range.NVI, file=path%+%title%_%"[5]res.Enrichment.range.NVI.RData")
    
    
  }
  
  
  
  
}





































# Comp Reg ----
if(FALSE){
  # rm(list=ls())
  
  
  
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
        
        X0 <- Xreg.filter %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Y, type="bootstrap")
        
        }
      
      result.CompReg.filter[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg.filter[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
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
        
        X0 <- Xreg.filter0 %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter0 %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Y, type="bootstrap")
        
        }
      
      result.CompReg[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
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
        
        X0 <- Xreg.filter %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
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
      
      df.forest <- png.forest.df(result.CompReg.filter[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-filtered-Adjusted-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
    
    
    
    
    result.CompReg <- NULL
    for(hh in 1:length(var.list)){
      var <- var.list[hh]
      
      Xreg.filter0 <- Xreg %>% 
        dplyr::select(age_min, gender_vote, starts_with("prob."), c(var)) %>% 
        drop_na() %>% 
        filter(!is.na(gender_vote), !is.na(age_min))
      
      {
        
        X0 <- Xreg.filter0 %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
        X.clr <- compositions::clr(X0)
        X.log <- log(X0)
        Y <- Xreg.filter0 %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
        Z <- Xreg.filter0 %>% dplyr::select(age_min, gender_vote) %>% as.data.frame %>% as.matrix
        
        if(sum(is.na(Z))>0){
          X0 <- X0[-which(is.na(Z),arr.ind=TRUE)[1],]
          Y <- Y[-which(is.na(Z),arr.ind=TRUE)[1],]
          Z <- Z[-which(is.na(Z),arr.ind=TRUE)[1],]
        }
        
        data.tmp <- data.frame(y=Y,z=Z)
        colnames(data.tmp)[1]="y"
        Ynew <- lm(y~., data=data.tmp)$residuals %>% as.matrix
        
        res.CompReg <- png.CompReg(X0=X0, Y=Ynew, type="bootstrap")
        
        }
      
      result.CompReg[[hh]] <- res.CompReg
      
    }
    
    
    for( k in 1:length(var.list) ){
      
      df.forest <- png.forest.df(result.CompReg[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
      grDevices::cairo_pdf(path%+%title%+%"_[6]ForestPlot-Adjusted-"%+%var.list[k]%+%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
      
      
    }
    
    
  }
  
  
}









# main.age ----
if(FALSE){
  # detach("package:png.GMM", unload=TRUE)
  # library(png.GMM)
  
  
  # setwd("..")
  
  
  # rm(list=ls())
  # library(dplyr)
  # library(png.GMM)
  # library(png.utils)
  # 
  # `%+%` <- function(x, y) {
  #   paste0(x, y)
  # }
  # 
  # title <- c("BIG5_230914", "BIG5_241231")[1]
  # path <- "./rdata.title1/"
  # 
  # 
  # 
  # 
  # rdata <- R.utils::loadToEnv(path%+%title%_%"[4]res.NVI.RData")
  # df <- rdata$df
  # res.Data <- rdata$res.Data
  # res.FA <- rdata$res.FA
  # res.GMM.best <- rdata$res.GMM.best
  # res.NVI <- rdata$res.NVI
  # Xnew <- rdata$res.FA$Xnew
  
  # library(mclust)
  # nclust <- res.NVI$wh.best.NVI[1]
  # seed <- res.NVI$wh.best.NVI[2]
  # # seed <- ifelse(title=="BIG5_241231", 47, 67)
  # 
  # fit.best <- res.NVI$fit.best.NVI
  # Xclust <- read.csv(path%+%title%_%"[4]Xclust.NVI_cov.csv")
  
  
  age.list <- c("[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)")
  
  for(hh in 1:length(age.list)){
    Xclust.age <- Xclust %>% 
      mutate(agecut = cut( age_min, breaks = 0:8*10, right=FALSE )) %>% 
      filter(agecut==age.list[hh]) %>% 
      dplyr::select(-ID, -gender_vote, -age_min, -agecut)
    
    main.GMM.boxplot2(Xclust=Xclust.age, wh.best=c(nclust,seed), title=path%+%title%+%"_[7]Age-Boxplot-Age="%+%age.list[hh])
  }
  
  
  gender.list <- c("남성", "여성")
  for(hh in 1:length(gender.list)){
    Xclust.gender <- Xclust %>% 
      filter(gender_vote==gender.list[hh]) %>% 
      dplyr::select(-ID, -gender_vote, -age_min)
    
    main.GMM.boxplot2(Xclust=Xclust.gender, wh.best=c(nclust,seed), title=path%+%title%+%"_[7]Gender-Boxplot-Gender="%+%gender.list[hh])
  }
  
  
  
  
  
  Xclust.age.total <- Xclust %>% 
    mutate(agecut = cut( age_min, breaks = 0:8*10, right=FALSE ))
  
  Xclust.age.total.df <- Xclust.age.total %>% dplyr::select(-ID, -age_min) %>% gather(Factors, value, -class, -agecut, -gender_vote) %>% 
    mutate(class=as.factor(class),
           Factors=factor(Factors, levels=c("O","C","E","A","N")),
           gender_vote=factor(gender_vote, levels=c("남성", "여성", "성별비공개"), labels=c("male", "female", NA))
           )
  
  
  age.list <- c("[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)")
  
  Xclust.age.total.df %>% 
    dplyr::select(-gender_vote) %>% 
    filter(!is.na(agecut), agecut %in% age.list) %>% 
    # group_by(agecut, class, Factors) %>% 
    # summarise(value=mean(value)) %>% 
    ggpubr::ggline(x="Factors", y="value", color="agecut", palette=ggsci::pal_frontiers()(6), 
                   add = c("mean_se"),
                   outlier.shape=3, bxp.errorbar=TRUE, 
                      xlab="Factors", ylab="Factor Scores",
                   facet.by="class", nrow=1, ggtheme=theme_pubr()) + guides(color = guide_legend(title="Age",nrow = 1))
  
  
  ggsave(filename=path%+%title%+%"_[7]Age-Boxplot-Age=Total"%+%"-seed="%+%seed%+%",nclust="%+%nclust%+%".pdf", height=3, width=10)
  
  
  Xclust.age.total.df %>%
    filter(!is.na(gender_vote), gender_vote %in% c("male", "female")) %>% 
    # select(-gender_vote) %>% 
    # filter(!is.na(agecut), agecut %in% age.list) %>% 
    # group_by(gender_vote, class, Factors) %>% 
    # summarise(value=mean(value)) %>% 
    ggpubr::ggline(x="Factors", y="value", color="gender_vote", palette=ggsci::pal_frontiers()(2), 
                   add = c("mean_se"),
                   # outlier.shape=3, 
                   # bxp.errorbar=TRUE, 
                   xlab="Factors", ylab="Factor Scores", 
                   facet.by="class", nrow=1, ggtheme=theme_pubr()) + guides(color = guide_legend(title="Gender", nrow = 1))
  
  ggsave(filename=path%+%title%+%"_[7]Gender-Boxplot-Gender=Total"%+%"-seed="%+%seed%+%",nclust="%+%nclust%+%".pdf", height=3, width=10)
  
  
  
  #
  #
  
  
  
}

