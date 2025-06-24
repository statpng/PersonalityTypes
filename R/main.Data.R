#' @export main.Data
main.Data <- function(df, FactorNames, title="title"){
  
  FactorNamesPatterns <- paste0("^(", paste0(FactorNames,collapse="|"), ")*[0-9]+$")
  
  X <- df %>% dplyr::select(matches(FactorNamesPatterns)) %>% drop_na()
  
  "(Dimension)" %>% print
  dim(X) %>% print
  "(Top 2 rows)" %>% print
  X[1:2,1:15] %>% print
  "(List of Factors)" %>% print
  gsub("[0-9]", "", colnames(X)) %>% table %>% print 
  
  ID <- df$ID
  
  
  result <- NULL
  
  result$ID <- ID
  result$X <- X
  
  result
}
