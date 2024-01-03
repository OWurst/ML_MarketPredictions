#   This file contains functions used to prep data for ML,
#some are likely to be rather verbose, so I do not want them
#to all be in the Rmd file

#     Function to prep data immediately after reading in csv,
#calculate percent changes, include volume, ticker, industry, date.
prep_df_post_csv_read <- function(df, ticker, industry) {
  return_df <- data.frame(
    Ticker = ticker,
    Industry = industry,
    Date = df$Date,
    Volume = df$Volume,
    Adj_Pct_Change = ((df$"Adj Close" - df$Open) / df$Open * 100),
    Pct_Change = ((df$Close - df$Open) / df$Open * 100)
  )
  return(return_df)
}