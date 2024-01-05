# This file contains functions for creating visualizations for data exploration
library(tidyverse)
library(gridExtra)
library(grid)

create_single_stock_line_graph <- function(df, name, y_title, adjusted) {
  y_column <- if (adjusted) {
    df$`Adj Close`
  } else {
    df$Close
  }

  plot <- ggplot(df, aes(x = Date, y = y_column)) + # nolint
    geom_line(color = "blue") + # nolint
    labs(title = name, # nolint
         y = ifelse(y_title, "Closing Price", ""),
         x = NULL)
  return(plot)
}

create_single_stock_histogram <- function(df, name, x_title, adjusted) {
  column <- if (adjusted) { # nolint
    df$Adj_Pct_Change
  } else {
    df$Adj_Pct
  }

  plot <- ggplot(df, aes(x = Pct_Change)) + # nolint
    geom_histogram(color = "black", binwidth = 2, fill = "lightblue") + # nolint
    labs(title = name, # nolint
         y = NULL,
         x =  ifelse(x_title, "Percent Change", ""))
  return(plot)
}

create_industry_graphs <- function(
    df_list, name_list,
    industry, type,
    adjusted = FALSE) {

  list_length <- length(df_list)
  graph_list <- list()

  for (i in 1:list_length){
    df <- df_list[[i]]
    name <- name_list[[i]]


    if (type == "Line_Graph") {
      axis_title <- ifelse(i == 1, TRUE, FALSE)
      plot <- create_single_stock_line_graph(df, name, axis_title, adjusted)
      title_end <- "Stock Prices 2019-2024"
    } else {
      axis_title <- ifelse(i == 2, TRUE, FALSE)
      plot <- create_single_stock_histogram(df, name, axis_title, adjusted)
      title_end <- "Stock Price Percent Change Distribution"
    }

    plot <- list(plot)
    graph_list <- c(graph_list, plot)
  }

  title_start <- ifelse(adjusted, paste("Adjusted", industry), industry)

  plotset <- grid.arrange(
    grobs = graph_list,
    ncol = list_length,
    top = grid.text(
      paste(title_start, title_end),
      gp = gpar(fontsize = 16, fontface = "bold"),
      vjust = .5
    )
  )
  return(plotset)
}