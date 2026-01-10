# BIPLOT GG
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @method biplot_gg CA
biplot_gg.CA <- function(x, ..., axes = c(1, 2),
                         type = c("symetric", "rows", "columns", "contributions"),
                         active = TRUE, sup = TRUE, labels = NULL,
                         col.rows = c("#E69F00", "#E69F00"),
                         col.columns = c("#56B4E9", "#56B4E9"),
                         pch.rows = c(16, 1), pch.columns = c(17, 2),
                         size = c(1, 3),
                         xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                         legend = list(legend.position = "right")) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  ## Type of biplot
  if (type == "symetric") {
    princ_row <- TRUE
    princ_col <- TRUE
  }
  if (type == "rows") {
    princ_row <- TRUE
    princ_col <- FALSE
  }
  if (type == "columns") {
    princ_row <- FALSE
    princ_col <- TRUE
  }
  if (type == "contributions") {
    princ_row <- FALSE
    princ_col <- TRUE
    sup <- FALSE # Override
  }

  ## Get data
  # We use prepare_plot but we will largely ignore the colors it produces,
  # instead we build our own grouping variable for mapping.
  coord_col <-  prepare_plot(x, margin = 2, axes = axes, active = active, sup = sup,
                             principal = princ_col, extra_quali = "observation",
                             color = NULL, symbol = NULL, line_type = NULL)
  coord_row <-  prepare_plot(x, margin = 1, axes = axes, active = active, sup = sup,
                             principal = princ_row, extra_quali = "observation",
                             color = NULL, symbol = NULL, line_type = NULL)

  ## Add grouping for mapping
  coord_row$group <- paste("Rows", coord_row$extra_quali) # e.g. "Rows active", "Rows suppl."
  coord_col$group <- paste("Columns", coord_col$extra_quali)

  ## Handle contributions (size)
  if (type == "contributions") {
    mass_row <- get_masses(x, margin = 1)
    mass_col <- get_masses(x, margin = 2)

    coord_row$x <- coord_row$x * sqrt(mass_row)
    coord_row$y <- coord_row$y * sqrt(mass_row)

    # Scale size based on mass
    coord_row$size_var <- mass_row
    coord_col$size_var <- mass_col
  } else {
    coord_row$size_var <- NA
    coord_col$size_var <- NA
  }

  ## Combine data? No, keep separate to control layers.

  ## Construct manual scales
  # Levels: Rows active, Rows suppl., Columns active, Columns suppl.
  # Note: extra_quali returns "active" or "suppl." (from prepare_plot)
  # But prepare_plot logic uses: data$observation <- ifelse(data$supplementary, "suppl.", "active")

  # Colors
  cols_map <- c(
    "Rows active" = col.rows[1],
    "Rows suppl." = col.rows[2],
    "Columns active" = col.columns[1],
    "Columns suppl." = col.columns[2]
  )

  # Shapes
  shapes_map <- c(
    "Rows active" = pch.rows[1],
    "Rows suppl." = pch.rows[2],
    "Columns active" = pch.columns[1],
    "Columns suppl." = pch.columns[2]
  )

  ## Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey")

  # Rows
  if (type %in% c("symetric", "rows", "contributions")) {
    if (type == "contributions") {
        p <- p + ggplot2::geom_point(data = coord_row,
                                     ggplot2::aes(x = .data$x, y = .data$y,
                                                  colour = .data$group, shape = .data$group,
                                                  size = .data$size_var))
    } else {
        p <- p + ggplot2::geom_point(data = coord_row,
                                     ggplot2::aes(x = .data$x, y = .data$y,
                                                  colour = .data$group, shape = .data$group))
    }
  }

  # Columns
  if (type %in% c("symetric", "columns", "contributions")) {
    if (type == "contributions") {
        p <- p + ggplot2::geom_point(data = coord_col,
                                     ggplot2::aes(x = .data$x, y = .data$y,
                                                  colour = .data$group, shape = .data$group,
                                                  size = .data$size_var))
    } else {
        p <- p + ggplot2::geom_point(data = coord_col,
                                     ggplot2::aes(x = .data$x, y = .data$y,
                                                  colour = .data$group, shape = .data$group))
    }
  }

  # Scales
  p <- p +
    ggplot2::scale_colour_manual(values = cols_map) +
    ggplot2::scale_shape_manual(values = shapes_map) +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(
        title = main,
        subtitle = sub,
        x = print_variance(x, axes[[1]]),
        y = print_variance(x, axes[[2]]),
        color = "Group", shape = "Group"
    )

  if (!is.null(xlim)) p <- p + ggplot2::scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)

  # Size scale for contributions
  if (type == "contributions") {
      p <- p + ggplot2::scale_radius(range = size)
  }

  # Labels
  if (!is.null(labels)) {
    labels <- match.arg(labels, c("rows", "columns", "individuals", "variables"), several.ok = TRUE)

    label_data <- data.frame()
    if (any(labels %in% c("rows", "individuals"))) {
      if (type %in% c("symetric", "rows", "contributions")) {
         label_data <- rbind(label_data, coord_row)
      }
    }
    if (any(labels %in% c("columns", "variables"))) {
      if (type %in% c("symetric", "columns", "contributions")) {
         label_data <- rbind(label_data, coord_col)
      }
    }

    if (nrow(label_data) > 0) {
        # Simple text labels.
        # For more advanced labels (check_overlap, repel), we would need ggrepel or similar.
        # But we stick to ggplot2.
        p <- p + ggplot2::geom_text(data = label_data,
                                    ggplot2::aes(x = .data$x, y = .data$y,
                                                 label = .data$label, colour = .data$group),
                                    vjust = -0.5, show.legend = FALSE)
    }
  }

  # Legend theme
  if (is.list(legend) && length(legend) > 0) {
      p <- p + do.call(ggplot2::theme, legend)
  }

  return(p)
}

#' @export
#' @rdname biplot
#' @aliases biplot_gg,CA-method
setMethod("biplot_gg", c(x = "CA"), biplot_gg.CA)

# PCA ==========================================================================
#' @export
#' @method biplot_gg PCA
biplot_gg.PCA <- function(x, ..., axes = c(1, 2), type = c("form", "covariance"),
                          active = TRUE, sup = TRUE, labels = "variables",
                          col.rows = c("#E69F00", "#E69F00"),
                          col.columns = c("#56B4E9", "#56B4E9"),
                          pch.rows = c(16, 1), lty.columns = c(1, 3),
                          xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                          legend = list(legend.position = "right")) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  ## Type of biplot
  if (type == "form") {
    princ_row <- TRUE
    princ_col <- FALSE
  }
  if (type == "covariance") {
    princ_row <- FALSE
    princ_col <- TRUE
  }

  ## Get data
  coord_col <-  prepare_plot(x, margin = 2, axes = axes, active = active, sup = sup,
                             principal = princ_col, extra_quali = "observation",
                             color = NULL, symbol = NULL, line_type = NULL, ...)
  coord_row <-  prepare_plot(x, margin = 1, axes = axes, active = active, sup = sup,
                             principal = princ_row, extra_quali = "observation",
                             color = NULL, symbol = NULL, line_type = NULL, ...)

  ## Add grouping
  coord_row$group <- paste("Rows", coord_row$extra_quali)
  coord_col$group <- paste("Columns", coord_col$extra_quali)

  ## Manual scales
  cols_map <- c(
    "Rows active" = col.rows[1],
    "Rows suppl." = col.rows[2],
    "Columns active" = col.columns[1],
    "Columns suppl." = col.columns[2]
  )

  shapes_map <- c(
    "Rows active" = pch.rows[1],
    "Rows suppl." = pch.rows[2],
    "Columns active" = NA,
    "Columns suppl." = NA
  )

  linetype_map <- c(
    "Rows active" = NA,
    "Rows suppl." = NA,
    "Columns active" = lty.columns[1], # Numeric or string? lty.columns is usually numeric (1, 3)
    "Columns suppl." = lty.columns[2]
  )

  ## Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey")

  # Rows (points)
  p <- p + ggplot2::geom_point(data = coord_row,
                               ggplot2::aes(x = .data$x, y = .data$y,
                                            colour = .data$group, shape = .data$group))

  # Columns (arrows)
  # Use geom_segment with arrow
  p <- p + ggplot2::geom_segment(data = coord_col,
                                 ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y,
                                              colour = .data$group, linetype = .data$group),
                                 arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")))

  # Scales
  p <- p +
    ggplot2::scale_colour_manual(values = cols_map) +
    ggplot2::scale_shape_manual(values = shapes_map, na.value = NA) +
    ggplot2::scale_linetype_manual(values = linetype_map, na.value = "blank") +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(
        title = main,
        subtitle = sub,
        x = print_variance(x, axes[[1]]),
        y = print_variance(x, axes[[2]]),
        color = "Group", shape = "Group", linetype = "Group"
    )

  if (!is.null(xlim)) p <- p + ggplot2::scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)

  # Labels
  if (!is.null(labels)) {
    # labels can be string "variables" or vector.
    # biplot.PCA default: labels = "variables"

    label_data <- data.frame()
    if (any(labels %in% c("rows", "individuals"))) {
         label_data <- rbind(label_data, coord_row)
    }
    if (any(labels %in% c("columns", "variables"))) {
         label_data <- rbind(label_data, coord_col)
    }

    if (nrow(label_data) > 0) {
        p <- p + ggplot2::geom_text(data = label_data,
                                    ggplot2::aes(x = .data$x, y = .data$y,
                                                 label = .data$label, colour = .data$group),
                                    vjust = -0.5, show.legend = FALSE)
    }
  }

  # Legend theme
  if (is.list(legend) && length(legend) > 0) {
      p <- p + do.call(ggplot2::theme, legend)
  }

  return(p)
}

#' @export
#' @rdname biplot
#' @aliases biplot_gg,PCA-method
setMethod("biplot_gg", c(x = "PCA"), biplot_gg.PCA)
