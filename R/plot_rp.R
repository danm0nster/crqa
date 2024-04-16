.packageName <- 'crqa'

# Dan Mønster, July 2023
library(dplyr)
library(ggplot2)

# A ggplot2-based function to produce a recurrence plot from a
# matrix. Much faster than using graphics::image() and nicer than
# crqa::plotRP().
plot_rp <- function(data_matrix,
                    plot_title = "",
                    points = FALSE,
                    flip_y = FALSE) {
  # If data_matrix is a sparse matrix from the Matrix package,
  # convert it to a regular matrix first.
  if ("dgCMatrix" %in% class(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  } else if ("dgRMatrix" %in% class(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  } else if ("ngCMatrix" %in% class(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  data_dims <- dim(data_matrix)
  if (data_dims[1] != data_dims[2]) {
    stop("Matrix is not square. Number of rows should be equal to number of columns.")
  }
  
  data_df <- as.data.frame.table(data_matrix, responseName = "value") %>%
    mutate(value = as.logical(value)) %>% 
    filter(value == TRUE) %>% 
    mutate_if(is.factor, as.integer)
  
  RP <- ggplot(data_df, aes(x = Var1, y = Var2, fill = value)) +
    scale_fill_discrete(type = c("0" = "white", "1" = "black")) +
    coord_fixed(ratio = 1) +
    xlab("") +
    ylab("") +
    labs(title = plot_title) +
    expand_limits(x = 1, y = 1) +
    expand_limits(x = data_dims[1], y = data_dims[2]) +
    theme_classic() +
    theme(legend.position = "none")
  
  if (flip_y) {
    RP <- RP + scale_y_reverse()
  }
  if (points) {
    RP <- RP + geom_point(size = 0.5, stroke = 0, shape = ".")
  } else {
    RP <- RP + geom_tile() 
  }
  print(RP)
}
