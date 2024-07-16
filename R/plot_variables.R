#' Plot Variables
#'
#' This function creates a scatter plot between two variables in a given dataset.
#'
#' @name plot_variables
#' @param data A data frame containing the data.
#' @param var1 A string representing the first variable.
#' @param var2 A string representing the second variable.
#' @param save_format The format in which the plot should be saved: "pdf", "png" or NULL. By default, the plot is not saved.
#' @param file_name The name of the file if the plot is to be saved. Default is "scatter_plot".
#' @return A ggplot2 object.
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' plot_variables(data, "x", "y")
#' @export
plot_variables <- function(data, var1, var2, save_format = NULL, file_name = "scatter_plot") {
  plot <- ggplot(data, aes_string(x = var1, y = var2)) +
    geom_point() +
    theme_minimal() +
    labs(title = paste("Scatter Plot of", var1, "and", var2), x = var1, y = var2)

  if (!is.null(save_format)) {
    if (save_format == "pdf") {
      ggsave(filename = paste0(file_name, ".pdf"), plot = plot)
    } else if (save_format == "png") {
      ggsave(filename = paste0(file_name, ".png"), plot = plot)
    } else {
      stop("Invalid save format. Please choose 'pdf', 'png', or NULL.")
    }
  } else {
    print(plot)
  }

  return(plot)
}
