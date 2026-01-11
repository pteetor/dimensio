# VISUALIZE VARIABLES GG
#' @include AllGenerics.R
NULL

# Variables ====================================================================
#' @export
#' @method viz_variables_gg PCA
viz_variables_gg.PCA <- function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                                 labels = list(filter = "contribution", n = 10),
                                 extra_quali = NULL, extra_quanti = NULL,
                                 color = NULL, symbol = NULL, size = 1,
                                 xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                                 legend = list(legend.position = "right")) {
  ## Prepare data
  ## Note: we map symbol to line_type for PCA (arrows)
  coord <- prepare_plot(x, margin = 2, axes = axes, ...,
                        active = active, sup = sup,
                        extra_quali = extra_quali, extra_quanti = extra_quanti,
                        color = color, line_type = symbol, line_width = size)

  ## Plot
  p <- ggplot2::ggplot(data = coord) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey")

  ## Scaled variables?
  if (is_scaled(x)) {
    # Draw unit circle
    circle_data <- data.frame(
      x = cos(seq(0, 2 * pi, length.out = 100)),
      y = sin(seq(0, 2 * pi, length.out = 100))
    )
    p <- p + ggplot2::geom_path(data = circle_data, ggplot2::aes(x = .data$x, y = .data$y),
                                color = "grey")

    # If scaled, limits are usually -1 to 1 unless overridden
    if (is.null(xlim)) xlim <- c(-1, 1)
    if (is.null(ylim)) ylim <- c(-1, 1)
  }

  ## Aesthetics mapping
  mapping <- ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y)

  # Check if extra_quanti is used
  has_quanti <- !all(is.na(coord$extra_quanti))
  has_quali <- !all(is.na(coord$extra_quali))

  if (has_quanti) {
    mapping$colour <- ggplot2::aes(colour = .data$extra_quanti)$colour
    mapping$linewidth <- ggplot2::aes(linewidth = .data$extra_quanti)$linewidth
  } else if (has_quali) {
    mapping$colour <- ggplot2::aes(colour = .data$extra_quali)$colour
    # Map linetype if symbol was provided (mapped to line_type in prepare_plot)
    if (!is.null(symbol)) {
        mapping$linetype <- ggplot2::aes(linetype = .data$extra_quali)$linetype
    }
  } else {
    mapping$colour <- ggplot2::aes(colour = .data$observation)$colour
    mapping$linetype <- ggplot2::aes(linetype = .data$observation)$linetype
  }

  args_geom <- list(mapping = mapping, arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), ...)

  # Use constant size if not mapped
  if (!has_quanti) {
      # Use size[1] to ensure scalar
      args_geom$linewidth <- size[1]
  }

  p <- p + do.call(ggplot2::geom_segment, args_geom)

  ## Scales
  # Size/linewidth scale
  if (has_quanti && length(size) == 2) {
     p <- p + ggplot2::scale_linewidth(range = size)
  }

  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(
        title = main,
        subtitle = sub,
        x = print_variance(x, axes[[1]]),
        y = print_variance(x, axes[[2]])
    )

  if (!is.null(xlim)) p <- p + ggplot2::scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)

  ## Labels
  if (isTRUE(labels)) labels <- list()
  if (is.list(labels)) {
    # Filter labels
    coord_labels <- coord
    if (!is.null(labels$filter) && !is.null(labels$n) && labels$n > 0) {
        if (labels$filter %in% names(coord_labels)) {
            top <- min(nrow(coord_labels), labels$n)
            ord <- order(coord_labels[[labels$filter]], decreasing = TRUE)
            coord_labels <- coord_labels[ord[seq_len(top)], , drop = FALSE]
        }
    }

    p <- p + ggplot2::geom_text(data = coord_labels,
                                mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                                       colour = if(has_quanti) .data$extra_quanti else if(has_quali) .data$extra_quali else .data$observation),
                                vjust = -0.5, show.legend = FALSE)
  }

  ## Legend
  if (is.list(legend) && length(legend) > 0) {
      p <- p + do.call(ggplot2::theme, legend)
  }

  p
}

#' @export
#' @rdname viz_variables
#' @aliases viz_variables_gg,PCA-method
setMethod("viz_variables_gg", c(x = "PCA"), viz_variables_gg.PCA)

# CA ===========================================================================
#' @export
#' @method viz_variables_gg CA
viz_variables_gg.CA <- function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                                labels = FALSE, extra_quali = NULL, extra_quanti = NULL,
                                color = NULL, fill = FALSE, symbol = NULL, size = c(1, 6),
                                xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                                legend = list(legend.position = "right")) {
  ## Prepare data
  coord <- prepare_plot(x, margin = 2, axes = axes, ...,
                        active = active, sup = sup,
                        extra_quali = extra_quali, extra_quanti = extra_quanti,
                        color = color, fill = fill, symbol = symbol, size = size)

  ## Plot
  p <- ggplot2::ggplot(data = coord) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey")

  ## Aesthetics mapping
  mapping <- ggplot2::aes(x = .data$x, y = .data$y)

  has_quanti <- !all(is.na(coord$extra_quanti))
  has_quali <- !all(is.na(coord$extra_quali))

  if (has_quanti) {
    mapping$colour <- ggplot2::aes(colour = .data$extra_quanti)$colour
    mapping$size <- ggplot2::aes(size = .data$extra_quanti)$size
  } else if (has_quali) {
    mapping$colour <- ggplot2::aes(colour = .data$extra_quali)$colour
    mapping$shape <- ggplot2::aes(shape = .data$extra_quali)$shape
  } else {
    mapping$colour <- ggplot2::aes(colour = .data$observation)$colour
    mapping$shape <- ggplot2::aes(shape = .data$observation)$shape
  }

  args_geom <- list(mapping = mapping, ...)
  if (!has_quanti && length(size) == 1) {
      args_geom$size <- size
  }

  p <- p + do.call(ggplot2::geom_point, args_geom)

  ## Scales
  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(
        title = main,
        subtitle = sub,
        x = print_variance(x, axes[[1]]),
        y = print_variance(x, axes[[2]])
    )

  if (!is.null(xlim)) p <- p + ggplot2::scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)

  # Size scale if using size
  if (has_quanti) {
      p <- p + ggplot2::scale_radius(range = size)
  }

  ## Labels
  if (isTRUE(labels)) labels <- list()
  if (is.list(labels)) {
    # Filter labels
    coord_labels <- coord
    if (!is.null(labels$filter) && !is.null(labels$n) && labels$n > 0) {
        if (labels$filter %in% names(coord_labels)) {
            top <- min(nrow(coord_labels), labels$n)
            ord <- order(coord_labels[[labels$filter]], decreasing = TRUE)
            coord_labels <- coord_labels[ord[seq_len(top)], , drop = FALSE]
        }
    }

    p <- p + ggplot2::geom_text(data = coord_labels,
                                mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                                       colour = if(has_quanti) .data$extra_quanti else if(has_quali) .data$extra_quali else .data$observation),
                                vjust = -0.5, show.legend = FALSE)
  }

  ## Legend
  if (is.list(legend) && length(legend) > 0) {
      p <- p + do.call(ggplot2::theme, legend)
  }

  p
}

#' @export
#' @rdname viz_variables
#' @aliases viz_variables_gg,CA-method
setMethod("viz_variables_gg", c(x = "CA"), viz_variables_gg.CA)
