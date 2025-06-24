#' @export main.boxplot
main.boxplot <- function(Xclust2){
  
  library(ggpubr)
  library(patchwork)
  
  # rdata <- R.utils::loadToEnv("./result/Personality2-OCEAN-CV50-G=6-3.Enrichment.RData")
  # Xclust2 <- rdata$Xclust
  
  
  library(dplyr)
  library(tidyr)
  
  
  
  #
  
  
  pal.list <- c("npg", "aaas", "nejm", "lancet", "jama", "bmj", "jco", "ucscgb", "d3", "observable", "locuszoom", "igv", "cosmic",  "uchicago", "startrek", "tron", "futurama", "simpsons", "rickandmorty", "flatui", "frontiers", "gsea", "bs5", "material", "tw3")
  
  for(palette in "jco"){
    df2 %>% 
      ggpubr::ggboxplot(x="Factors", y="value", fill="Factors", palette=palette, 
                        outlier.shape=3, bxp.errorbar=TRUE, 
                        xlab="Factors", ylab="Factor Scores", legend="none") %>% 
      facet(facet.by="class", nrow=1)
    # facet(facet.by="class", labeller="label_both", nrow=1)
    
    ggsave(filename=paste0("Personality2-OCEAN-GMM-Boxplot-WithoutClass2-[Age=Total]-[col=",palette,"].pdf"), height=3, width=10)
  }
  
  
}
