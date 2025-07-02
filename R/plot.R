#' @title Generate and Save Boxplots of Factor Scores by Cluster
#' @description Creates a faceted boxplot to visualize the distribution of personality
#' factor scores for each identified cluster and saves it to a PDF file.
#' @import ggplot2
#' @import ggpubr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @details
#' This function serves to interpret the results of a clustering analysis (e.g., GMM).
#' It takes a data frame `Xclust` where each row is an observation, columns represent
#' different factor scores (e.g., O, C, E, A, N), and a 'class' column indicates cluster membership.
#'
#' The data is transformed into a long format, and `ggpubr::ggboxplot` is used to create
#' boxplots of scores for each factor. The plots are faceted by cluster (`facet.by="class"`),
#' allowing for a direct visual comparison of the personality profiles of the different clusters.
#' The resulting plot is saved as a PDF file, with a filename indicating the seed and number of clusters used.
#'
#' @param Xclust A data frame with factor scores and a 'class' column for cluster assignments.
#' @param wh.best A numeric vector of length 2, containing the optimal number of clusters (`wh.best[1]`) and the corresponding random seed (`wh.best[2]`).
#' @param filename A base string for the output PDF file name.
#'
#' @return This function does not return a value. It saves a plot to a PDF file.
#'
#' @export Xclust.boxplot.filename
Xclust.boxplot.filename <- function(Xclust, wh.best, filename){
  library(tidyr)
  
  # Xclust <- res.GMM.best$Xclust
  # wh.best <- res.GMM.best$wh.best
  
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
  
  ggsave(filename=filename%++%"-seed="%++%wh.best[2]%++%",nclust="%++%wh.best[1]%++%".pdf", height=3, width=10)
  
}







#' @title Generate Labeled Boxplots of Factor Scores by Cluster
#' @description An internal helper function to create faceted boxplots similar to
#' `Xclust.boxplot.filename`, but with hardcoded, descriptive labels for each cluster.
#' @import ggplot2
#' @import ggpubr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @details
#' This function is nearly identical to `Xclust.boxplot.filename` but is specialized for a
#' specific analysis where cluster identities have been interpreted and assigned meaningful names
#' (e.g., "Average", "Inquisitive Individualist"). The `class` variable is converted to a
#' factor with these predefined labels, making the output plot more directly interpretable.
#'
#' @param Xclust A data frame with factor scores and a 'class' column.
#' @param wh.best A numeric vector with the number of clusters and the seed.
#' @param filename A base string for the output PDF file name.
#'
#' @return This function does not return a value. It saves a plot to a PDF file.
#' @keywords internal
Xclust.boxplot.filename_new <- function(Xclust, wh.best, filename){
  library(tidyr)
  library(ggpubr)
  # Xclust=Xclust.NVI; wh.best=c(wh.best.NVI[1], wh.best.NVI[2]); filename=filename%++%"[4_new]NVI-best-"
  
  Xclust.df <- Xclust %>% gather(Factors, value, -class) %>% 
    mutate(class=factor(class, levels=c(4,2,3,5,1), labels=c("Average", "Inquisitive Individualist", "Unsettled", "Expressive", "Conventional")),
           Factors=factor(Factors, levels=c("O","C","E","A","N")))
  
  Xclust.df %>% 
    ggpubr::ggboxplot(x="Factors", y="value", fill="Factors", palette="jco", 
                      outlier.shape=3, bxp.errorbar=TRUE, 
                      xlab="Factors", ylab="Factor Scores", legend="none") %>% 
    facet(facet.by="class", nrow=1)
  
  ggsave(filename=filename%++%"-seed="%++%wh.best[2]%++%",nclust="%++%wh.best[1]%++%".pdf", height=3, width=10)
  
}



#' @title Create Boxplot for the Best NVI Result
#' @description A wrapper function that generates a boxplot for the most stable
#' clustering solution identified by the `analysis.NVI` function.
#'
#' @details
#' This function simplifies the process of visualizing the most stable clustering profile.
#' It extracts the necessary components (`wh.best.NVI`, `Xclust.NVI`) from the result
#' object of `analysis.NVI` and passes them to the internal plotting function
#' `Xclust.boxplot.filename_new` to generate a labeled boxplot.
#'
#' @param res.NVI A list object returned by the `analysis.NVI` function.
#' @param filename A base string for the output PDF file name.
#'
#' @return This function does not return a value. It saves a plot to a PDF file.
#' @seealso \code{\link{analysis.NVI}}, \code{\link{Xclust.boxplot.filename_new}}
#' @export NVI.BoxPlot
NVI.BoxPlot <- function(res.NVI, filename=NULL){
  
  wh.best.NVI <- res.NVI$wh.best.NVI
  fit.best.NVI <- res.NVI$fit.best.NVI
  Xclust.NVI <- res.NVI$Xclust.NVI
  
  attr(Xclust.NVI, "seed")

  Xclust.boxplot.filename_new(Xclust=Xclust.NVI, wh.best=c(wh.best.NVI[1], wh.best.NVI[2]), 
                              filename=filename%++%"NVI-best-NewPanel")
}







#' @title Create a Forest Plot for Compositional Regression Results
#' @description Visualizes the results of a compositional regression analysis,
#' showing the estimated coefficients and their confidence intervals.
#' @import forestploter
#' @import grid
#' @importFrom magrittr %>%
#'
#' @details
#' This function takes the result from `png.CompReg` and creates a forest plot using the `forestploter` package. A forest plot is an effective way to display regression results, as it provides a clear visual summary of the magnitude, direction, and uncertainty (confidence interval) of the effect for each predictor. Each row in the plot corresponds to a compositional predictor, with a point estimate and a horizontal line representing the confidence interval. A vertical reference line at zero helps in quickly identifying statistically significant predictors (whose CIs do not cross zero).
#'
#' @param result A single result object for one dependent variable, taken from the list returned by `png.CompReg`.
#' @param DV.name A string with the name of the dependent variable for the plot title.
#' @param class An optional vector of labels for the predictors (clusters).
#' @param width An integer to adjust the spacing for the plot layout.
#'
#' @return A `forestploter` plot object that can be printed to the graphics device.
#'
#' @export CompReg.forest
CompReg.forest <- function(result, DV.name, class=NULL, width=30){
  library(forestploter)
  library(tidyverse)
  library(grid)
  
  nclass <- length(result$fit.reg$beta)
  if(is.null(class)){
    class = 1:nclass
  }
  
  nsmall=3
  
  df.forest <- data.frame(
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
              sizes = 0.35
  )
  
  p
}
