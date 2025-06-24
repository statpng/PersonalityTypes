if(FALSE){
  library(haven) # for SPSS .sav file
  library(lubridate)
  
  # df1 <- haven::read_sav("./datasets/Personality1.sav") %>% drop_na()
  df2_raw <- haven::read_sav("./datasets/230914 BIG5_cluster_DVs_FINAL3.sav") %>% as_tibble()
  df3_raw <- haven::read_sav("./datasets/241231 BIG5_DVs_FINAL3.sav") %>% as_tibble()
  
  save(df2_raw, file="./datasets/BIG5_230914.RData")
  save(df3_raw, file="./datasets/BIG5_241231.RData")
}


if(FALSE){
  library(png.utils)
  library(dplyr)
  
  title <- c("BIG5_230914", "BIG5_241231")[1]
  path <- "./rdata/"
  
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
    mutate(factor(gender_vote, levels=c("남성", "여성", "성별비공개"), labels=c("male", "female", NA)))
  
  save(df, file=path%+%title%_%"[1]df.RData")
}




if(FALSE){
  
  library(haven) # for SPSS .sav file
  library(tidyverse) # for tidy data analyses
  library(dplyr)
  library(psych) # for FA
  library(car) # for FA
  library(mclust) # for GMM
  library(plotly) # for 3d visualization
  library(kableExtra)
  library(knitr)
  library(png.utils)
  
  R.utils::sourceDirectory("../code")
  source("../code/main.Enrichment.R")
  
  setwd("./230914_14rm/")
  title <- "230914_14rm"
  
  
  
}