library(tidyverse)
library(caret)

read <- function(file) {
  readr::read_csv(
    file,
    col_names = TRUE,
    show_col_types = FALSE
  )
}

drop_unwanted_pct_data <- function(df, adjusted = FALSE){
  if(adjusted){
    df_filtered <- df
  } else {
    df_filtered <- df %>% select(-starts_with("Adj"), -Actual_AdjPerChange)
  }

  return(df_filtered)
}

prep_df_list <- function(files_list, adjusted = FALSE){
  return_list <- list()
  
  for (name in names(files_list)) {
    df <- read(files_list[[name]])
    df <- drop_unwanted_pct_data(df, adjusted = adjusted)
    
    return_list[[name]] <- df
  }
  return(return_list)
}
