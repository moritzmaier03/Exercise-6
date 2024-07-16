#' Calculate and Plot Approximation Ratio
#'
#' This function calculates the approximation ratios for multiple instances and methods, and creates a boxplot.
#'
#' @name calculate_and_plot_approximation_ratio
#' @param solutions A list of numeric vectors representing the solutions found. Each list corresponds to an instance.
#' @param exact_solutions A numeric vector representing the exact solutions. The length of the vector should match the length of the `solutions` list.
#' @param method_names A vector of strings representing the names of the different methods.
#' @param save_format The format in which the plot should be saved: "pdf", "png" or NULL. By default, the plot is not saved.
#' @param file_name The name of the file if the plot is to be saved. Default is "approximation_ratio_plot".
#' @return A list with two elements: `average_ratios` and `plot`.
#' @examples
#' solutions <- list(
#'   method1 = list(c(100, 110, 120), c(200, 210, 220)),
#'   method2 = list(c(105, 115, 125), c(205, 215, 225))
#' )
#' exact_solutions <- c(100, 200)
#' method_names <- c("Method 1", "Method 2")
#' calculate_and_plot_approximation_ratio(solutions, exact_solutions, method_names)
#' @export
library(ggplot2)

calculate_and_plot_approximation_ratio <- function(solutions, exact_solutions, method_names, save_format = NULL, file_name = "approximation_ratio_plot") {
  if(length(solutions) != length(exact_solutions)) {
    stop("The length of solutions must match the length of exact solutions.")
  }

  method_ratios <- lapply(names(solutions), function(method) {
    sapply(seq_along(solutions[[method]]), function(i) {
      mean(solutions[[method]][[i]] / exact_solutions[i])
    })
  })

  average_ratios <- sapply(method_ratios, mean)
  names(average_ratios) <- method_names

  # Prepare data for boxplot
  plot_data <- do.call(rbind, lapply(names(solutions), function(method) {
    data.frame(
      Method = method_names[names(solutions) == method],
      Ratio = unlist(lapply(seq_along(solutions[[method]]), function(i) {
        solutions[[method]][[i]] / exact_solutions[i]
      }))
    )
  }))

  # Create boxplot
  plot <- ggplot(plot_data, aes(x = Method, y = Ratio)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Approximation Ratios", x = "Method", y = "Approximation Ratio")

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

  return(list(average_ratios = average_ratios, plot = plot))
}
