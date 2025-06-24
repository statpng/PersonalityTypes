#' @export `%_%`
`%_%` <- function(x, y) {
  paste(x, y, sep="_")
}

# scripts/utils.R
#' @export `%+%`
`%+%` <- function(x, y) {
  paste0(x, y)
}

# scripts/load_data.R
#' @export load_raw_data
load_raw_data <- function(file_path) {
  library(haven)
  df <- haven::read_sav(file_path) %>% as_tibble()
  return(df)
}


# scripts/preprocess.R
#' @export preprocess_data
preprocess_data <- function(df, 
                            filter_pattern = "^(O|C|E|A|N)\\d+$", 
                            id_col = "id",
                            age_cols = c("age_O", "age_C", "age_E", "age_A", "age_N"),
                            gender_pattern = "gender", 
                            gender_levels = c("남성", "여성", "성별비공개"),
                            gender_labels = c("male", "female", NA)) {
  
  df <- df %>%
    dplyr::filter(if_all(dplyr::matches(filter_pattern), ~ !is.na(.))) %>%
    dplyr::mutate(age_min = do.call(pmin, c(dplyr::select(., dplyr::all_of(age_cols)), list(na.rm = TRUE))),
                  ID = .[[id_col]])
  
  # 성별 컬럼에서 최빈값 구하기
  gender_cols <- dplyr::select(df, dplyr::contains(gender_pattern))
  gender_vote <- apply(gender_cols, 1, function(x) {
    tbl <- table(x)
    names(tbl)[which.max(tbl)]
  })
  
  df$gender_vote <- factor(gender_vote, levels = gender_levels, labels = gender_labels)
  return(df)
}