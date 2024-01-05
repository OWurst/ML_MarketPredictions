# This file contains functions for creating visualizations for data exploration
library(tidyverse)
library(gridExtra)
library(grid)

create_single_stock_line_graph <- function(df, name, y_title) {
  plot <- ggplot(df, aes(x = Date, y = Close)) + # nolint
    geom_line(color = "blue") + # nolint
    labs(title = name, # nolint
         y = ifelse(y_title, "Closing Price", ""),
         x = NULL)
  return(plot)
}

create_industry_graphs <- function(df_list, name_list, industry) {
  list_length <- length(df_list)
  graph_list <- list()

  for (i in 1:list_length){
    df <- df_list[[i]]
    name <- name_list[[i]]

    axis_title <- ifelse(i == 1, TRUE, FALSE)

    plot <- create_single_stock_line_graph(df, name, axis_title)
    plot <- list(plot)
    graph_list <- c(graph_list, plot)
  }

  plotset <- grid.arrange(
    grobs = graph_list,
    ncol = list_length,
    top = grid.text(
      paste(industry, "Stock Prices 2019-2024"),
      gp = gpar(fontsize = 16, fontface = "bold"),
      vjust = .5
    )
  )
  return(plotset)
}