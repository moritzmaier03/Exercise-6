#' Plot a dataset
#'
#' This function creates a basic plot of the given dataset.
#'
#' @param data A data frame.
#' @param x The name of the x variable.
#' @param y The name of the y variable.
#' @examples
#' plot_data(iris, "Sepal.Length", "Sepal.Width")
plot_data <- function(data, x, y) {
  plot(data[[x]], data[[y]], main = paste("Plot of", x, "vs", y))
}
