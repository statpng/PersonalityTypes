#' @export plotBIC
plotBIC <- function(title){
  load(paste0(title,"-2.GMM.RData"))
  pdf(file=paste0(title,"-GMM-BICnew.pdf"), width=8, height=4)
  # plot(BICnew, legendArgs = list(x=NA))
  plot(BIC_final)
  dev.off()
  
}


#' @export plot.violin
plot.violin <- function(title, height=5, width=8){
  rdata <- R.utils::loadToEnv(paste0(title,"-2.GMM.RData"))
  
  nclass <- rdata$Xclust$class %>% unique %>% length
  df_long <- rdata$Xclust %>% gather(Factors, Scores, -class)
  df_long %>% 
    ggplot() +
    # geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
    geom_violin(aes(Factors, Scores, fill=class) ) +
    # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
    #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
    facet_wrap(~class, labeller="label_both", ncol=4) +
    # ggsci::scale_fill_lancet(alpha=0.5) +
    # scale_fill_manual(values=png.utils::png.colors(nclass)) +
    scale_fill_manual(values=COLORS[1:nclass]) +
    theme_bw() + theme(legend.position = "none")
  
  ggsave(filename=paste0(title, "-GMM-violin.pdf"), height=height, width=width)
}



# columns=c("E","N","C","O","A","class")
#' @export plot.boxplot
plot.boxplot <- function(title, height=3, width=10){
  
  # rdata <- R.utils::loadToEnv(title)
  rdata <- R.utils::loadToEnv(paste0(title,"-2.GMM.RData"))
  
  COLORS <- c("dodgerblue2", "red3", "green3", "slateblue", "darkorange", "skyblue1", "violetred4",
              "forestgreen", "steelblue4", "slategrey", "brown", "black", "darkseagreen", "darkgoldenrod3",
              "olivedrab", "royalblue", "tomato4", "cyan2", "springgreen2")
  
  if( "ID" %in% colnames(rdata$Xclust) ){
    rdata$Xclust <- rdata$Xclust %>% select(-ID)
  }
  
  nclass <- rdata$Xclust$class %>% unique %>% length
  nfactors <- rdata$Xclust %>% {ncol(.)-1}
  df_long <- rdata$Xclust %>% gather(Factors, Scores, -class)
  df_long %>% 
    ggplot() +
    geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
    # geom_violin(aes(Factors, Scores, fill=class) ) +
    # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
    #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
    facet_wrap(~class, labeller="label_both", nrow=1) +
    # ggsci::scale_fill_lancet(alpha=0.5) +
    # scale_fill_manual(values=png.utils::png.colors(nclass)) +
    scale_fill_manual(values=COLORS[1:nclass]) +
    theme_bw() + theme(legend.position = "none")
  
  ggsave(filename=paste0(title, "-GMM-boxplot.pdf"), height=height, width=width)
  
  
  df_long %>% 
    ggplot() +
    geom_boxplot(aes(class, Scores, fill=Factors), outlier.color=NA) +
    # geom_violin(aes(Factors, Scores, fill=class) ) +
    # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
    #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
    facet_wrap(~Factors, labeller="label_both", nrow=1) +
    # ggsci::scale_fill_lancet(alpha=0.5) +
    scale_fill_manual(values=png.utils::png.colors(nfactors)) +
    theme_bw() + theme(legend.position = "none")
  
  ggsave(filename=paste0(title, "-GMM-boxplot-byFactors.pdf"), height=height, width=width)
  
}




#' @export plot.boxplot.4to6
plot.boxplot.4to6 <- function(title, columns=NULL, nfactors=5, height=3, width=10){
  
  rdata <- R.utils::loadToEnv(title)
  
  
  COLORS <- c("dodgerblue2", "red3", "green3", "slateblue", "darkorange", "skyblue1", "violetred4",
              "forestgreen", "steelblue4", "slategrey", "brown", "black", "darkseagreen", "darkgoldenrod3",
              "olivedrab", "royalblue", "tomato4", "cyan2", "springgreen2")
  
  
  for( idx in 1:3 ){
    
    # if( !is.null(columns) ){
    #   Xclust <- rdata$Xclust %>% dplyr::select(columns)
    #   columns(Xclust)[ncol(Xclust)] <- "class"
    # }
    
    nclass <- rdata$Xclust_4to6[[c("class", "class5", "class6")[idx]]] %>% unique %>% length
    # nfactors <- rdata$Xclust_4to6 %>% {ncol(.)-1}
    
    df_long <- rdata$Xclust_4to6 %>% select(c(columns, class=c("class", "class5", "class6")[idx]) ) %>% gather(Factors, Scores, -class)
    df_long %>% 
      ggplot() +
      geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
      # geom_violin(aes(Factors, Scores, fill=class) ) +
      # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
      #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
      facet_wrap(~class, labeller="label_both", nrow=1) +
      # ggsci::scale_fill_lancet(alpha=0.5) +
      # scale_fill_manual(values=png.utils::png.colors(nclass)) +
      scale_fill_manual(values=COLORS[1:nclass]) +
      theme_bw() + theme(legend.position = "none")
    
    ggsave(filename=paste0(title, "-GMM-boxplot-(",idx,").pdf"), height=height, width=width)
    
    
    df_long %>% 
      ggplot() +
      geom_boxplot(aes(class, Scores, fill=Factors), outlier.color=NA) +
      # geom_violin(aes(Factors, Scores, fill=class) ) +
      # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
      #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
      facet_wrap(~Factors, labeller="label_both", ncol=3) +
      # ggsci::scale_fill_lancet(alpha=0.5) +
      scale_fill_manual(values=png.utils::png.colors(nfactors)) +
      theme_bw() + theme(legend.position = "none")
    
    ggsave(filename=paste0(title, "-GMM-boxplot-byFactors-(",idx,").pdf"), height=height, width=width)
    
  }
  
  
}


# plot.boxplot <- function(title, height=5, width=8){
#   rdata <- R.utils::loadToEnv(paste0(title,"-2.GMM.RData"))
#   
#   nclass <- rdata$Xclust$class %>% unique %>% length
#   df_long <- rdata$Xclust %>% gather(Factors, Scores, -class)
#   df_long %>% 
#     ggplot() +
#     geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
#     # geom_violin(aes(Factors, Scores, fill=class) ) +
#     # geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
#     #             data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.005)) +
#     facet_wrap(~class, labeller="label_both", ncol=3) +
#     # ggsci::scale_fill_lancet(alpha=0.5) +
#     scale_fill_manual(values=png.utils::png.colors(nclass)) +
#     theme_bw() + theme(legend.position = "none")
#   
#   ggsave(filename=paste0(title, "-GMM-boxplot.pdf"), height=height, width=width)
# }









if(FALSE){
  
  
  title <- "Personality1"
  
  load(paste0(title,"-2.GMM.RData"))
  
  
  
  # Display the count of each class
  Xclust$class %>% table %>% table
  
  
  
  
  
  
  
  
  
  # > Clusters that are not in the Significant Cluster have a sample size of less than 200.
  
  
  
  
  
  ## The average value of each factor score per cluster
  panel-tabset
  ### Raw data
  
  df %>% 
    filter(class %in% SigCluster) %>% 
    group_by(class) %>% 
    summarise(PA1_mean=mean(PA1),
              PA2_mean=mean(PA2),
              PA3_mean=mean(PA3),
              PA4_mean=mean(PA4),
              PA5_mean=mean(PA5))
  
  
  ### Model estimation
  
  mod1$parameters$mean %>% t
  
  
  
  
  
  ## Convert the DataFrame to a long form
  
  df_long <- df %>%
    # filter(class %in% SigCluster) %>%
    gather(Factors, Scores, -class)
  
  
  panel-tabset
  ### Total samples
  
  df_long %>%
    ggplot() +
    geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
    geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
                data=df_long %>% group_by(class, Factors) %>% slice_sample(prop=0.1)) +
    facet_wrap(~class, labeller="label_both", ncol=3) +
    ggsci::scale_fill_lancet(alpha=0.5) +
    theme_bw() + theme(legend.position = "none")
  # ggsave(filename="Figure-GMM-cluster-boxplot.pdf", height=5, width=8)
  
  
  ### Significant cluster only
  
  #| fig-height: 2.5
  df_long %>% filter(class %in% SigCluster) %>%
    ggplot() +
    geom_boxplot(aes(Factors, Scores, fill=class), outlier.color=NA) +
    geom_jitter(aes(Factors, Scores), color="grey30", size=0.5, alpha=0.4, width=0.2,
                data=df_long %>% filter(class %in% SigCluster) %>% group_by(class, Factors) %>% slice_sample(prop=0.1)) +
    facet_wrap(~class, labeller="label_both", ncol=3) +
    ggsci::scale_fill_lancet(alpha=0.5) +
    theme_bw() + theme(legend.position = "none")
  # ggsave(filename="Figure-GMM-cluster-boxplot-SigClust.pdf", height=5, width=8)
  
  
  
  
  
  
  
  ## Get class2
  
  #| message: false
  
  # Create a new dataframe (df_new) that includes the new cluster assignment
  df_new <- df
  df_new$class2 <- fun2(Xnew=df[,paste0("PA", 1:5)],
                        class=df$class, mod=mod1,
                        wh.class=c(2,8,5,7,6,4) )
  
  # write.csv(df_new, file="DataFrame-GMM-df_new.csv", quote=FALSE, row.names=FALSE)
  
  
  
  panel-tabset
  ### head
  
  df_new %>% head(2) %>% 
    knitr::kable() %>% 
    kableExtra::kable_styling(font_size = 16)
  
  
  ### table
  
  df_new$class %>% table
  df_new$class2 %>% table
  
  
  
  ### Contigency table
  
  df_new %>% {table(.$class, .$class2)}
  
  
  
  
  
  
  ## New dataframe (df_new2)
  
  df_new2 <- df_new
  df_new2$gender <- rdata$df$sex_consciousness
  df_new2$year <- rdata$df$생년_consciousness
  df_new2$ID <- rdata$df$USER식별정보 %>% {gsub("*\n", "", .) }
  df_new2$year[df_new2$year%in%c(10,70)] <- NA
  
  
   # Male proportion in the total sample
  
  df_new2$gender %>% mean
  
  
   # Male proportion in each cluster
  
  df_new2 %>% group_by(class2) %>% summarise(mean(gender))
  
  # write.csv(df_new2, file="DataFrame-GMM-df_new2.csv", quote=FALSE, row.names=FALSE)
  
  if(FALSE){
    save(BIC, hc1, BIC1, mod1, df, out, SigCluster, df_new, df_new2, file="RData-2.GMM.RData")
  }
  
  
  
  
  
  ## Distribution of factor scores across clusters
  panel-tabset
  
  
  df_new2_long <- df_new2 %>% 
    select(-ID) %>%
    mutate(age=2023-year, gender=as.factor(gender)) %>%
    select(-year) %>% 
    gather(Factors, Scores, -class, -class2, -gender, -age)
  
  
  ### Gender
  
  #| fig-height: 3.5
  
  # ggsave(filename="Figure-GMM-cluster-boxplot-gender.pdf", width=8, height=5)
  
  df_new2_long %>% mutate(group=as.numeric(as.factor(Factors))*10+as.numeric(gender)) %>% 
    ggplot() + geom_boxplot(aes(Factors, Scores, group=group, fill=gender, alpha=0.4), outlier.color="grey30") +
    facet_wrap(~class2, ncol=3) +
    ggsci::scale_fill_lancet() +
    theme_bw() + theme(legend.position="top")
  
  
  ### Gender + Age
  
  #| fig-height: 3.5
  
  # ggsave(filename="Figure-GMM-cluster-scatter.pdf", width=8, height=6)
  
  df_new2_long %>%
    ggplot(aes(age, Scores, color=gender)) +
    geom_point(alpha=0.5) + stat_smooth(method="lm") +
    facet_grid(Factors~class2) +
    ggsci::scale_color_lancet() +
    theme_bw() + theme(legend.position="top")
  
  
  
  
  
  
  
  
  
  
  
  # Reference
  
  # https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html"
  
  
  
  
  
}