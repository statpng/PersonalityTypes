#' @title Preprocess Personality Survey Data
#' @description This function preprocesses a data frame containing personality survey
#' data, specifically tailored for a structure where items from different factors
#' (e.g., Big Five "OCEAN" traits) are in separate blocks.
#' @import dplyr
#' @import tidyr
#'
#' @details
#' This function is designed to clean and restructure a specific type of psychological survey data before statistical modeling. The primary goal is to extract a clean numeric matrix of personality items (`X`) and a corresponding processed data frame (`df`).
#'
#' The key preprocessing steps are:
#' 1.  **Filtering for Complete Responses**: The function first identifies all columns corresponding to the personality items (e.g., `O1`, `C1`, `E1`, etc.) based on the `FactorNames` argument. It then filters the data frame to retain only the rows (participants) that have no missing values (`NA`) across *all* of these item columns. This ensures that the subsequent analyses are performed on a complete dataset. This step is statistically important as many multivariate models (like Factor Analysis or GMM) require complete data.
#'
#' 2.  **Harmonizing Demographic Data**:
#'     -   `age_min`: It calculates a single age value for each participant by taking the minimum of all available age columns (e.g., `age_O`, `age_C`, etc.). This resolves potential discrepancies.
#'     -   `gender_vote`: It creates a single, consistent gender variable. For each participant, it inspects all gender columns (e.g., `gender_O`, `gender_C`) and assigns the most frequently occurring value. This "voting" mechanism is a robust method for data harmonization. The resulting variable is then converted to an English factor (`male`, `female`).
#'
#' 3.  **Extracting the Analysis Matrix (`X`)**: A final matrix `X` is created containing only the numeric responses to the personality items. This matrix is the primary input for subsequent multivariate statistical analyses like clustering or factor analysis.
#'
#' The function also prints a summary of the resulting matrix `X` to the console, including its dimensions and the count of items per factor, which serves as a quick diagnostic check.
#'
#' @param df A data frame containing the raw survey data. It is expected to have columns for personality items (e.g., `A1`, `C1`...) and corresponding demographic blocks (e.g., `age_A`, `gender_A`). See the function's code comments for the expected structure.
#' @param FactorNames A character vector of single-letter prefixes used to identify the personality factor items. Defaults to `c("O", "C", "E", "A", "N")` for the Big Five model.
#'
#' @return A list containing three elements:
#' \item{X}{A numeric data frame or matrix containing only the cleaned personality item data, suitable for modeling.}
#' \item{df}{The preprocessed version of the input data frame, including the harmonized `age_min` and `gender_vote` columns and filtered to match the rows in `X`.}
#'
#' @export analysis.DataPreprocessing
analysis.DataPreprocessing <- function(df, FactorNames=strsplit("OCEAN","")[[1]]){
  
  # > colnames(df)
  # [1] "id"                       "finished_A"               "year_A"                  
  # [4] "birthyear_A"              "age_A"                    "gender_A"                
  # [7] "A1"                       "A2"                       "A3"                      
  # [10] "A4"                       "A5"                       "A6"                      
  # [13] "A7"                       "A8"                       "A9"                      
  # [16] "A10"                      "A11"                      "A12"                     
  # [19] "A13"                      "A14"                      "A15"                     
  # [22] "A16"                      "A17"                      "A18"                     
  # [25] "A19"                      "A20"                      "A21"                     
  # [28] "A22"                      "A23"                      "A24"                     
  # [31] "finished_C"               "year_C"                   "birthyear_C"             
  # [34] "age_C"                    "gender_C"                 "C1"                      
  # [37] "C2"                       "C3"                       "C4"                      
  # [40] "C5"                       "C6"                       "C7"                      
  # [43] "C8"                       "C9"                       "C10"                     
  # [46] "C11"                      "C12"                      "C13"                     
  # [49] "C14"                      "C15"                      "C16"                     
  # [52] "C17"                      "C18"                      "C19"                     
  # [55] "C20"                      "C21"                      "C22"                     
  # [58] "C23"                      "C24"                      "finished_E"              
  # [61] "year_E"                   "birthyear_E"              "age_E"                   
  # [64] "gender_E"                 "E1"                       "E2"                      
  # [67] "E3"                       "E4"                       "E5"                      
  # [70] "E6"                       "E7"                       "E8"                      
  # [73] "E9"                       "E10"                      "E11"                     
  # [76] "E12"                      "E13"                      "E14"                     
  # [79] "E15"                      "E16"                      "E17"                     
  # [82] "E18"                      "E19"                      "E20"                     
  # [85] "E21"                      "E22"                      "E23"                     
  # [88] "E24"                      "finished_N"               "year_N"                  
  # [91] "birthyear_N"              "age_N"                    "gender_N"                
  # [94] "N1"                       "N2"                       "N3"                      
  # [97] "N4"                       "N5"                       "N6"                      
  # [100] "N7"                       "N8"                       "N9"                      
  # [103] "N10"                      "N11"                      "N12"                     
  # [106] "N13"                      "N14"                      "N15"                     
  # [109] "N16"                      "N17"                      "N18"                     
  # [112] "N19"                      "N20"                      "N21"                     
  # [115] "N22"                      "N23"                      "N24"                     
  # [118] "finished_O"               "year_O"                   "birthyear_O"             
  # [121] "age_O"                    "gender_O"                 "O1"                      
  # [124] "O2"                       "O3"                       "O4"                      
  # [127] "O5"                       "O6"                       "O7"                      
  # [130] "O8"                       "O9"                       "O10"                     
  # [133] "O11"                      "O12"                      "O13"                     
  # [136] "O14"                      "O15"                      "O16"                     
  # [139] "O17"                      "O18"                      "O19"                     
  # [142] "O20"                      "O21"                      "O22"                     
  # [145] "O23"                      "O24"                      "SE_after_PERSONALITY"    
  # [148] "SE"                       "GRAT_after_PERSONALITY"   "GRAT"                    
  # [151] "SWLS_after_PERSONALITY"   "SWLS"                     "COMPAR_after_PERSONALITY"
  # [154] "COMPAR"                   "COMPAR_opin"              "COMPAR_abil"             
  # [157] "COMPAR_ach"               "COMPAR_gen"               "OPT_after_PERSONALITY"   
  # [160] "OPT"                      "PANA_after_PERSONALITY"   "PA"                      
  # [163] "NA"                       "STRESS_after_PERSONALITY" "STRESS"                  
  # [166] "EBH_after_PERSONALITY"    "EBH_immut"                "EBH_eff"                 
  # [169] "EBH_bio"                  "EBHa"                     "EBHb"                    
  # [172] "SES_after_PERSONALITY"    "SES"                      "LONE_after_PERSONALITY"  
  # [175] "LONE"                     "MNG_after_PERSONALITY"    "MNG"                     
  # [178] "MNG_pr"                   "MNG_se" 
  
  FactorNamesPatterns <- paste0("^(", paste0(FactorNames,collapse="|"), ")*[0-9]+$")
  
  df <- df %>%
    filter(if_all(matches("^(O|C|E|A|N)\\d+$"), ~ !is.na(.))) %>%
    mutate(age_min = pmin(age_O, age_C, age_E, age_A, age_N, na.rm = TRUE))
  
  df$gender_vote <- df %>% select(contains("gender")) %>% {
    apply(., 1, function(x) x[order(table(unlist(x)),decreasing=TRUE)[1]] )
  }
  
  df <- df %>%
    mutate(gender_vote=factor(gender_vote, 
                              levels=c("남성", "여성", "성별비공개"), 
                              labels=c("male", "female", NA)))
  
  
  X <- df %>% dplyr::select(matches(FactorNamesPatterns)) %>% tidyr::drop_na()
  
  "(Dimension)" %>% print
  dim(X) %>% print
  "(Top 2 rows)" %>% print
  X[1:2,1:15] %>% print
  "(List of Factors)" %>% print
  gsub("[0-9]", "", colnames(X)) %>% table %>% print 
  
  
  
  result <- NULL
  
  result$X <- X
  result$df <- df
  
  result
}
