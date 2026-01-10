# SCREEPLOT GG
#' @include AllGenerics.R
NULL

# Screeplot ====================================================================
#' @export
#' @method screeplot_gg MultivariateAnalysis
screeplot_gg.MultivariateAnalysis <- function(x, ..., eigenvalues = FALSE, cumulative = FALSE,
                                              labels = TRUE, limit = 10) {
  ## Prepare data
  data <- get_eigenvalues(x)
  data$x <- seq_len(nrow(data))
  data$z <- data[[3L]]

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  if (eigenvalues) {
    data$y <- data[[1L]]
    data$labels <- round(data$y, digits = 1)
    ylab <- tr_("Eigenvalues")
  } else {
    data$y <- data[[2L]]
    data$labels <- paste0(round(data$y, digits = 1), "%")
    if (methods::is(x, "CA")) {
      ylab <- tr_("Proportion of inertia (%)")
    } else {
      ylab <- tr_("Explained variance (%)")
    }
  }

  if (cumulative) {
    k <- max(data$y) / max(data$z)
    data$k <- data$z * k
  }

  ## Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_col(...) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::scale_x_continuous(breaks = data$x) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  if (labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$labels), vjust = -0.5)
  }

  if (cumulative) {
    if (methods::is(x, "CA")) {
      ylab2 <- tr_("Cumulative inertia (%)")
    } else {
      ylab2 <- tr_("Cumulative variance (%)")
    }

    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$k), col = "red") +
      ggplot2::geom_point(ggplot2::aes(y = .data$k), col = "red") +
      ggplot2::scale_y_continuous(
        name = ylab,
        sec.axis = ggplot2::sec_axis(~ . / k, name = ylab2)
      ) +
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(color = "red"),
        axis.text.y.right = ggplot2::element_text(color = "red"),
        axis.ticks.y.right = ggplot2::element_line(color = "red")
      )
  }

  return(p)
}

#' @export
#' @rdname screeplot
#' @aliases screeplot_gg,MultivariateAnalysis-method
setMethod("screeplot_gg", c(x = "MultivariateAnalysis"), screeplot_gg.MultivariateAnalysis)

#' @export
#' @method screeplot_gg PCOA
screeplot_gg.PCOA <- function(x, ..., labels = FALSE, limit = NULL,
                              eigenvalues = NULL, cumulative = NULL) {
  ## Prepare data
  data <- get_eigenvalues(x)
  data$x <- seq_len(nrow(data))
  data$y <- data[[1L]]
  data$labels <- round(data$y, digits = 1)

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ylab <- tr_("Eigenvalues")

  ## Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_col(...) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::scale_x_continuous(breaks = data$x) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  if (labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$labels), vjust = -0.5)
  }

  return(p)
}

#' @export
#' @rdname screeplot
#' @aliases screeplot_gg,PCOA-method
setMethod("screeplot_gg", c(x = "PCOA"), screeplot_gg.PCOA)
