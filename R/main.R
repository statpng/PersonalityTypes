run.PersonalityTypes <- function(){
  
  # Due to a nondisclosure agreement and joint data ownership between the Center for [blinded] and Kakao Corporation, 
  # we are unable to publicly share the raw scores of individual scale items. 
  # Instead, the dataset includes factor scores derived from our factor analysis of the IPIP-NEO-120 responses, 
  # along with the composite score(s) for each correlate. Although the raw data necessary for conducting a factor analysis are not included, 
  # we have provided the analysis code for transparency. The dataset can be analyzed beginning with the code provided for the GMM analysis.


  
  # > sessionInfo()
  # R version 4.4.1 (2024-06-14)
  # Platform: aarch64-apple-darwin20
  # Running under: macOS 15.5
  # 
  # Matrix products: default
  # BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
  # LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
  # 
  # locale:
  #   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
  # 
  # time zone: Asia/Seoul
  # tzcode source: internal
  # 
  # attached base packages:
  #   [1] stats     graphics  grDevices utils     datasets  methods  
  # [7] base     
  # 
  # other attached packages:
  #   [1] ggpubr_0.6.0         ggplot2_3.5.2        tidyr_1.3.1         
  # [4] mclust_6.1.1         car_3.1-3            carData_3.0-5       
  # [7] psych_2.5.3          haven_2.5.5          dplyr_1.1.4         
  # [10] PersonalityTypes_0.1
  # 
  # loaded via a namespace (and not attached):
  #   [1] tidyselect_1.2.1     farver_2.1.2         R.utils_2.13.0      
  # [4] fastmap_1.2.0        tensorA_0.36.2.1     promises_1.3.3      
  # [7] digest_0.6.37        mime_0.13            lifecycle_1.0.4     
  # [10] ellipsis_0.3.2       magrittr_2.0.3       compiler_4.4.1      
  # [13] forestploter_1.1.3   rlang_1.1.6          tools_4.4.1         
  # [16] knitr_1.50           ggsignif_0.6.4       labeling_0.4.3      
  # [19] htmlwidgets_1.6.4    pkgbuild_1.4.8       mnormt_2.1.1        
  # [22] xml2_1.3.8           RColorBrewer_1.1-3   pkgload_1.4.0       
  # [25] abind_1.4-8          miniUI_0.1.2         withr_3.0.2         
  # [28] purrr_1.0.4          R.oo_1.27.1          desc_1.4.3          
  # [31] grid_4.4.1           roxygen2_7.3.2       urlchecker_1.0.1    
  # [34] profvis_0.4.0        xtable_1.8-4         aricode_1.0.3       
  # [37] extrafontdb_1.0      GPArotation_2025.3-1 scales_1.4.0        
  # [40] gtools_3.9.5         MASS_7.3-65          cli_3.6.5           
  # [43] ragg_1.4.0           generics_0.1.4       remotes_2.5.0       
  # [46] rstudioapi_0.17.1    robustbase_0.99-4-1  commonmark_1.9.5    
  # [49] sessioninfo_1.2.3    bayesm_3.1-6         cachem_1.1.0        
  # [52] stringr_1.5.1        parallel_4.4.1       vctrs_0.6.5         
  # [55] devtools_2.4.5       Matrix_1.7-3         hms_1.1.3           
  # [58] rstatix_0.7.2        Formula_1.2-5        systemfonts_1.2.3   
  # [61] testthat_3.2.3       glue_1.8.0           DEoptimR_1.1-3-1    
  # [64] stringi_1.8.7        gtable_0.3.6         later_1.4.2         
  # [67] extrafont_0.19       tibble_3.3.0         pillar_1.10.2       
  # [70] compositions_2.0-8   htmltools_0.5.8.1    brio_1.1.5          
  # [73] R6_2.6.1             textshaping_1.0.1    rprojroot_2.0.4     
  # [76] evaluate_1.0.3       shiny_1.10.0         lattice_0.22-7      
  # [79] R.methodsS3_1.8.2    backports_1.5.0      ggsci_3.2.0         
  # [82] memoise_2.0.1        broom_1.0.8          httpuv_1.6.16       
  # [85] Rcpp_1.0.14          Rttf2pt1_1.3.12      gridExtra_2.3       
  # [88] nlme_3.1-168         xfun_0.52            fs_1.6.6            
  # [91] forcats_1.0.0        usethis_3.1.0        pkgconfig_2.0.3  
  
  
  
  
  
  
  # PackageCompilation <- function(){
  #   devtools::document()
  #   devtools::load_all()
  # }
  
  
  
  # rdata <- R.utils::loadToEnv("./rdata.title2/BIG5_241231_[4]res.NVI.RData")
  
  
  if(FALSE){
    
    # Clear all variables from the current environment to ensure a clean start.
    rm(list=ls())
    
    
    #-------------------------------------------------------------------
    # 1. Initial Setup & Data Loading
    #-------------------------------------------------------------------
    
    # (Optional) Load a saved .RData file to resume the analysis from a specific step.
    # rdata <- R.utils::loadToEnv("./rdata.filename3/BIG5_241231[4]res.NVI.RData")
    
    
    
    # Load necessary packages for data manipulation and file reading.
    library(dplyr)
    library(ggplot2)
    
    
    
    # Create a directory to store the results of the analysis.
    R.utils::mkdirs("./result/")
    
    # Define a base path and prefix for all output files to ensure consistent naming.
    filename <- "./result/BIG5_241231"
    
    
    
    # Don't run
    if(FALSE){
      
      library(haven) # for SPSS .sav file
      
      # library(conflicted)
      # conflicted::conflict_prefer("select", "dplyr")
      # conflicted::conflict_prefer("filter", "dplyr")
      
      
      # This section shows how the original SPSS data was read and saved as an RDS file (run once).
      PersonalityTypes_241231 <- haven::read_sav("./datasets/241231 BIG5_DVs_FINAL3.sav") %>% as_tibble()
      saveRDS(PersonalityTypes_241231, file = "./dataset/PersonalityTypes_241231.rds")
      # PersonalityTypes_241231 <- readRDS("./dataset/PersonalityTypes_241231.rds")
      
      # Load the raw dataset from the RDS file.
      df <- readRDS("./datasets/PersonalityTypes_241231.rds", refhook = NULL)
      
      
      {
        var.list <- c("SE", "GRAT", "SWLS", "COMPAR", "COMPAR_opin", "COMPAR_abil", "COMPAR_ach", "COMPAR_gen", "OPT", "PA", "NA", "STRESS", "EBH_immut", "EBH_eff", "EBH_bio", "EBHa", "EBHb", "SES", "LONE", "MNG", "MNG_pr", "MNG_se")
        
        df_new <- df %>% 
          mutate("COMPAR_opin_after_PERSONALITY"=COMPAR_after_PERSONALITY,
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
                 "MNG_se_after_PERSONALITY"=MNG_after_PERSONALITY )
        
        for(hh in 1:length(var.list)){
          set.seed(hh)
          var <- var.list[hh]
          var_filter <- var %++% "_after_PERSONALITY"
          
          df_new[var][ df_new[var_filter]!=1 | is.na(df_new[var_filter]) ] <- NA
        }
        
        df.DV <- df_new %>% dplyr::select(ID=id, SE_after_PERSONALITY:MNG_se) %>% select(-ends_with("_after_PERSONALITY"))
      }
      
      
      
      #-------------------------------------------------------------------
      # 2. Exploratory Factor Analysis (EFA)
      #-------------------------------------------------------------------
      
      # Set a seed for reproducibility of random processes in the analysis.
      # Preprocess the raw data to extract relevant items and handle missing values.
      # Perform Factor Analysis on the preprocessed data to reduce dimensionality and compute factor scores.
      set.seed(1)
      res.Data <- analysis.DataPreprocessing(df, strsplit("OCEAN","")[[1]])
      
      res.FA <- analysis.FA(X=res.Data$X, ID=res.Data$ID, nfactors=5, 
                            filename=filename%++%"[2]")
      
      # Prepare the final cluster data by combining IDs, covariates (age, gender), and the stable cluster assignments.
      ID <- res.FA$ID
      age_min <- res.Data$df$age_min
      gender_vote <- res.Data$df$gender_vote
      Xnew <- res.FA$Xnew
      
      Xnew_cov <- data.frame(ID=ID, age=age_min, gender=gender_vote, Xnew) %>% 
        left_join(df.DV, by="ID")
      
      
      # Save the factor scores with covariates to a CSV/RData file.
      write.csv(Xnew_cov, file=filename%++%"[2]Xnew_cov.csv", quote=TRUE, row.names=FALSE)
      save(Xnew_cov, file=filename%++%"[2]Xnew_cov.RData")
      
    }
    
    
    #-------------------------------------------------------------------
    # 3. Gaussian Mixture Model (GMM) Clustering
    #-------------------------------------------------------------------
    
    # Use the factor scores from EFA as the input dataset for clustering.
    # Carry over the participant IDs.
    Xnew_cov <- R.utils::loadToEnv(filename%++%"[2]Xnew_cov.RData")$Xnew_cov
    Xnew <- Xnew_cov %>% select(E:A)
    ID <- Xnew_cov$ID
    
    
    res.GMM.best <- analysis.GMM.best(Xnew=Xnew, ID=ID, 
                                      max.clust=10, subsample.size=100,
                                      q=1.0, filename=filename%++%"[3]")
    # save(res.GMM.best, file="res.GMM.best_q=1.0.RData")
    
    # Extract the Normalized Entropy Criterion (NEC) matrix from the GMM results.
    # Extract the best number of clusters and the corresponding seed based on the minimum NEC.
    NEC.mat <- res.GMM.best$NEC.mat
    wh.best <- res.GMM.best$wh.best
    
    
    
    #-------------------------------------------------------------------
    # 4. NVI Stability Analysis & Saving Intermediate Results
    #-------------------------------------------------------------------
    
    # For the best number of clusters found, perform a stability analysis using Normalized Variation of Information (NVI).
    res.NVI <- analysis.NVI(Xnew=Xnew, nclust=wh.best[1], 
                            NEC.mat=NEC.mat, top=20,
                            filename=filename%++%"[4]")
    
    
    
    # Prepare the final cluster data by combining IDs, covariates (age, gender), and the stable cluster assignments.
    Xnew_cov.NVI <- data.frame(Xnew_cov, class) %>% select(ID, age, gender, E:A, class, SE:MNG_se)
    
    
    
    # Save the final cluster assignments with covariates to a CSV file.
    write.csv(Xnew_cov.NVI, file=filename%++%"[4]Xnew_cov.NVI.csv", quote=TRUE, row.names=FALSE)
    save(Xnew_cov.NVI, file=filename%++%"[4]Xnew_cov.NVI.RData")
    
    
    # Generate a boxplot visualizing the profiles of the most stable clusters.
    NVI.BoxPlot( res.NVI, filename=filename%++%"[4]" )
    
    
    # Save all result objects from the analysis so far into a single .RData file for easy loading later.
    save(Xnew_cov.NVI, res.FA, res.GMM.best, res.NVI,  file=filename%++%"[4]res.NVI.RData")
    
    
    
    #-------------------------------------------------------------------
    # 5. Enrichment Analysis (for the best model)
    #-------------------------------------------------------------------
    
    # Set seed for reproducibility.
    # Conduct a statistical validation of the chosen clustering solution using a permutation-based enrichment test.
    set.seed(1)
    res.Enrichment.NVI <- analysis.Enrichment(fit.GMM=res.NVI$fit.best.NVI, 
                                              Xclust=res.NVI$Xclust.NVI, 
                                              Xnew=res.NVI$Xclust.NVI, 
                                              n.clust=res.NVI$wh.best.NVI[1], 
                                              seed=res.NVI$wh.best.NVI[2],
                                              filename=filename)
    
    # Save the enrichment analysis results along with all previous results.
    save(Xnew_cov.NVI, res.FA, res.GMM.best, res.NVI, res.Enrichment.NVI, file=filename%++%"[5]res.Enrichment.NVI.RData")
    
    
    
    #-------------------------------------------------------------------
    # 6. Enrichment Analysis (across a range of models)
    #-------------------------------------------------------------------
    
    # Define parameters for the sensitivity analysis.
    # n.clust=10
    # nrep=100
    # seed=res.NVI$wh.best.NVI[2]
    # 
    # # Run the enrichment analysis for a range of cluster numbers (2 to 10).
    # res.Enrichment.range.NVI <- analysis.Enrichment.range(Xnew=res.FA$Xnew, n.clust=10, nrep=100, seed=seed)
    # 
    # # Create a plot to visualize the results of the sensitivity analysis.
    # # This plot shows how the number of statistically meaningful clusters changes as the number of candidate clusters increases.
    # pdf(file=paste0(filename,"[5]Enrichment-NumberOfClustersTrajectory.pdf"), width=8, height=4)
    #   plot( sapply(res.Enrichment.range.NVI, function(x) sum( p.adjust(x$pvalues, method="BH") < 0.05 )), type="b", xlab="Number of candidate clusters", ylab="Number of meaningful clusters")
    # dev.off()
    # 
    # save(df, res.Data, res.FA, res.GMM.best, res.NVI, res.Enrichment.NVI, res.Enrichment.range.NVI, 
    #      file=filename%++%"[5]res.Enrichment.range.NVI.RData")
    
    
    
    #-------------------------------------------------------------------
    # 7. Compositional Regression (CompReg)
    #-------------------------------------------------------------------
    
    # Load the saved RData file containing all previous analysis results.
    rdata <- R.utils::loadToEnv(filename%++%"[4]res.NVI.RData")
    
    
    # Extract necessary objects from the loaded data.
    Xclust <- rdata$Xnew_cov.NVI
    res.GMM.best <- rdata$res.GMM.best
    res.NVI <- rdata$res.NVI
    nclust <- res.NVI$wh.best.NVI[1]
    seed <- res.NVI$wh.best.NVI[2]
    fit.best <- res.NVI$fit.best.NVI

    
    # Extract posterior probabilities of cluster membership for each individual.
    # These probabilities are compositional data because they sum to 1 for each person.
    prob <- fit.best$z
    
    # Create the main regression dataset by combining cluster probabilities with various dependent variables (DVs).
    Xreg <- cbind.data.frame( Xclust, prob=prob)
    
    
    # Define a list of outcome (dependent) variables to be analyzed.
    var.list <- c("SE", "GRAT", "SWLS", "COMPAR", "COMPAR_opin", "COMPAR_abil", "COMPAR_ach", "COMPAR_gen", "OPT", "PA", "NA.", "STRESS", "EBH_immut", "EBH_eff", "EBH_bio", "EBHa", "EBHb", "SES", "LONE", "MNG", "MNG_pr", "MNG_se")
    
    
    
    #-- Unadjusted Analysis (without covariates) ----
    
    # Initialize a list to store results.
    result.CompReg <- NULL
    for(hh in 1:length(var.list)){
      set.seed(hh)
      var <- var.list[hh]
      
      Xreg2 <- Xreg %>% dplyr::select(starts_with("prob."), c(var)) %>% drop_na()
      
      # Prepare the compositional predictors (X0) and the outcome variable (Y).
      X0 <- Xreg2 %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
      X.clr <- compositions::clr(X0)
      X.log <- log(X0)
      Y <- Xreg2 %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
      
      # Run the compositional regression using bootstrap for inference.
      set.seed(hh)
      res.CompReg <- png.CompReg(X0=X0, Y=Y, type="bootstrap")
      
      # Store the result for the current DV.
      result.CompReg[[hh]] <- res.CompReg
    }
    
    # Loop through the results to create and save a forest plot for each DV.
    for( k in 1:length(var.list) ){
      set.seed(hh)
      df.forest <- CompReg.forest(result.CompReg[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
      # grDevices::cairo_pdf(filename%++%"[6]ForestPlot-filtered-"%++%var.list[k]%++%".pdf", width = 6.2, height = 2)
      pdf(filename%++%"[6]ForestPlot-filtered-"%++%var.list[k]%++%".pdf", width = 6.2, height = 2)
      plot( df.forest )
      dev.off()
    }
    
    
    
    #-- Adjusted Analysis (with covariates) ----
    # This section repeats the regression analysis, but adjusts for covariates.
    
    result.CompReg.adj <- NULL
    for(hh in 1:length(var.list)){
      set.seed(hh)
      var <- var.list[hh]
      
      Xreg2 <- Xreg %>% dplyr::select(age, gender, starts_with("prob."), c(var)) %>% drop_na()
      
      # Prepare predictors (X0) and original outcome (Y).
      X0 <- Xreg2 %>% dplyr::select(starts_with("prob")) %>% as.data.frame %>% compositions::acomp() %>% as.data.frame %>% as.matrix
      X.clr <- compositions::clr(X0)
      X.log <- log(X0)
      Y <- Xreg2 %>% dplyr::select(var) %>% as.data.frame %>% as.matrix
      # Define covariates (Z).
      Z <- Xreg2 %>% dplyr::select(age, gender) %>% as.data.frame %>% as.matrix
      
      # Create an adjusted outcome (Ynew) by taking the residuals of Y regressed on Z.
      # This removes the linear effect of the covariates from the outcome variable.
      data.tmp <- data.frame(y=Y,z=Z)
      colnames(data.tmp)[1]="y"
      Ynew <- lm(y~., data=data.tmp)$residuals %>% as.matrix
      
      # Run compositional regression on the adjusted outcome.
      set.seed(hh)
      res.CompReg <- png.CompReg(X0=X0, Y=Ynew, type="bootstrap")
      
      result.CompReg.adj[[hh]] <- res.CompReg
    }
    
    
    # Loop through the adjusted results to create and save a forest plot for each DV.
    for( k in 1:length(var.list) ){
      set.seed(hh)
      df.forest <- CompReg.forest(result.CompReg.adj[[k]][[1]], DV.name=var.list[k], class=1:nclust, width=30)
      
      # grDevices::cairo_pdf(filename%++%"[6]ForestPlot-filtered-Adjusted-"%++%var.list[k]%++%".pdf", width = 6.2, height = 2)
      pdf(filename%++%"[6]ForestPlot-filtered-Adjusted-"%++%var.list[k]%++%".pdf", width = 6.2, height = 2)
        plot( df.forest )
      dev.off()
    }
    
    
    
    #-------------------------------------------------------------------
    # 8. Stratified Analysis by Age and Gender
    #-------------------------------------------------------------------
    
    #-- Stratified by Age ----
    
    age.list <- c("[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)")
    
    for(hh in 1:length(age.list)){
      set.seed(hh)
      # Filter the data for the current age group.
      Xclust.age <- Xclust %>% 
        mutate(agecut = cut( age, breaks = 0:8*10, right=FALSE )) %>% 
        filter(agecut==age.list[hh]) %>% 
        dplyr::select(-ID, -gender, -age, -agecut)
      
      # Create a boxplot of personality profiles for that specific age group.
      Xclust.boxplot.filename(Xclust=Xclust.age, wh.best=c(nclust,seed), 
                              filename=filename%++%"[7]Age-Boxplot-Age="%++%age.list[hh])
    }
    
    
    #-- Stratified by Gender ----
    gender.list <- c("male", "female")
    for(hh in 1:length(gender.list)){
      set.seed(hh)
      # Filter the data for the current gender.
      Xclust.gender <- Xclust %>% 
        filter(gender==gender.list[hh]) %>% 
        dplyr::select(-ID, -gender, -age)
      
      # Create a boxplot of personality profiles for that specific gender.
      Xclust.boxplot.filename(Xclust=Xclust.gender, wh.best=c(nclust,seed), 
                              filename=filename%++%"[7]Gender-Boxplot-Gender="%++%gender.list[hh])
    }
    
    
    #-- Create Line Plots for Comparison ----
    # Prepare data for plotting by converting it to a long format.
    Xclust.age.total <- Xclust %>% 
      mutate(agecut = cut( age, breaks = 0:8*10, right=FALSE ))
    
    Xclust.age.total.df <- Xclust.age.total %>% 
      dplyr::select(-ID, -age) %>% 
      gather(Factors, value, -class, -agecut, -gender) %>% 
      mutate(class=as.factor(class),
             Factors=factor(Factors, levels=c("O","C","E","A","N"))
      )
    
    # Create a line plot comparing mean factor scores across clusters for different age groups.
    Xclust.age.total.df %>% 
      dplyr::select(-gender) %>% 
      filter(!is.na(agecut), agecut %in% age.list) %>% 
      # group_by(agecut, class, Factors) %>% 
      # summarise(value=mean(value)) %>% 
      ggpubr::ggline(x="Factors", y="value", color="agecut", palette=ggsci::pal_frontiers()(6), 
                     add = c("mean_se"),
                     xlab="Factors", ylab="Factor Scores",
                     # size=0.3,
                     # plot_type="l",
                     # point.size=0,
                     facet.by="class", nrow=1, ggtheme=theme_pubr()) + guides(color = guide_legend(title="Age", nrow = 1))
    
    ggsave(filename=paste0(filename,"[7]Age-LinePlot-Age=Total","-seed=",seed,",nclust=",nclust,".pdf"), height=3, width=10)
    
    
    # Create a line plot comparing mean factor scores across clusters for different genders.
    Xclust.age.total.df %>%
      filter(!is.na(gender), gender %in% c("male", "female")) %>% 
      # select(-gender) %>% 
      # filter(!is.na(agecut), agecut %in% age.list) %>% 
      # group_by(gender, class, Factors) %>% 
      # summarise(value=mean(value)) %>% 
      ggpubr::ggline(x="Factors", y="value", color="gender", palette=ggsci::pal_frontiers()(2), 
                     add = c("mean_se"),
                     # size=0.3,
                     # plot_type="l",
                     # bxp.errorbar=TRUE, 
                     xlab="Factors", ylab="Factor Scores", 
                     facet.by="class", nrow=1, ggtheme=theme_pubr()) + guides(color = guide_legend(title="Gender", nrow = 1))
    
    ggsave(filename=paste0(filename,"[7]Gender-LinePlot-Gender=Total","-seed=",seed,",nclust=",nclust,".pdf"), height=3, width=10)
    
    
    
    
  }
  
  
  
  
  
}
