#   This file contains functions used to prep data for ML,
#some are likely to be rather verbose, so I do not want them
#to all be in the Rmd file

read <- function(file){
  readr::read_csv(
    file,
    col_names = TRUE,
    show_col_types = FALSE
  )
}

#     Function to prep data immediately after reading in csv,
#calculate percent changes, include volume, ticker, industry, date.
prep_df_post_csv_read <- function(df, ticker, industry) {
  vol_average = mean(df$Volume)
  
  return_df <- data.frame(
    Ticker = ticker,
    Industry = industry,
    Date = df$Date,
    Volume = df$Volume - vol_average,
    Adj_Pct_Change = ((df$"Adj Close" - df$Open) / df$Open * 100),
    Pct_Change = ((df$Close - df$Open) / df$Open * 100)
  )
  return(return_df)
}

#     Function to transform the above dfs into a dataframe I will
# use for learning. Dataframes produced by this dataset have stock
# price percent change, adjusted stock price percent change, and
# transaction volume for the last k months and will be used to predict
# stock price percent change and adjusted stock price percent change.
learning_df_from_adjusted <- function(df, k) {
  learn_df_rows <- nrow(df) - k - 1

  colnames_adj_percent <- paste0("Adj_Pct_Change", 1:k)
  colnames_percent <- paste0("Pct_Change", 1:k)
  colnames_vol <- paste0("Volume", 1:k)

  adj_perc_df <- data.frame(
    matrix(
      NA, nrow = 0, ncol = k, dimnames = list(NULL, colnames_adj_percent)
    )
  )
  perc_df <- data.frame(
    matrix(
      NA, nrow = 0, ncol = k, dimnames = list(NULL, colnames_percent)
    )
  )
  vol_df <- data.frame(
    matrix(
      NA, nrow = 0, ncol = k, dimnames = list(NULL, colnames_vol)
    )
  )

  const_columns <- data.frame(
    Ticker = df$Ticker[0],
    Industry = df$Industry[0],
    Actual_PerChange = numeric(),
    Actual_AdjPerChange = numeric()
  )

  learning_df <- cbind(const_columns, adj_perc_df, perc_df, vol_df)

  i <- 0
  while (i < learn_df_rows) {
    new_data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(learning_df)))
    colnames(new_data) <- colnames(learning_df)

    new_data$Ticker <- df$Ticker[1]
    new_data$Industry <- df$Industry[1]
    new_data$Actual_PerChange <- df$Pct_Change[i + k + 1]
    new_data$Actual_AdjPerChange <- df$Adj_Pct_Change[i + k + 1]

    for (j in 1:k) {
      volcol <- paste0("Volume", j)
      new_data[volcol] <- df$Volume[i + j]

      pct_change_col <- paste0("Pct_Change", j)
      new_data[pct_change_col] <- df$Pct_Change[i + j]

      adj_pct_change_col <- paste0("Adj_Pct_Change", j)
      new_data[adj_pct_change_col] <- df$Adj_Pct_Change[i + j]
    }
    learning_df <- rbind(learning_df, new_data)

    i <- i + 1
  }
  return(learning_df)
}

#     Function to call the above function on a list of dataframes
# and combine to one large df
make_full_learning_df <- function(df_list, k) {
  train_df_list <- list()
  for (df in df_list) {
    df <- learning_df_from_adjusted(df, k)

    train_df_list <- c(train_df_list, list(df))
  }

  learning_df <- do.call(rbind, train_df_list)
  return(learning_df)
}