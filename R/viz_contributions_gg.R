# PLOT CONTRIBUTIONS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_contributions
#' @aliases viz_contributions_gg,MultivariateAnalysis-method
setMethod(
  f = "viz_contributions_gg",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 2, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        horiz = FALSE, col = "grey90", border = "grey10",
                        main = NULL, sub = NULL) {
    ## Prepare data
    data <- prepare_contrib(x, margin = margin, axes = axes, sort = sort,
                            decreasing = decreasing, limit = limit)

    ## Expected average contribution
    # Get the total number of items contributing to the component
    # to calculate the theoretical average contribution (100/p)
    contrib_all <- get_contributions(x, margin = margin)
    theo <- 100 / nrow(contrib_all)

    ## Labels
    msg <- tr_("Contribution to %s (%%)")
    ylab <- sprintf(msg, paste0("F", axes, collapse = "-"))

    ## Plot
    p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_col(fill = col, colour = border, width = 0.8, ...) +
      ggplot2::geom_hline(yintercept = theo, linetype = "dashed", colour = "red") +
      ggplot2::labs(title = main, subtitle = sub, x = NULL, y = ylab) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))

    if (horiz) {
      p <- p + ggplot2::coord_flip()
    } else {
       p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
    }

    p
  }
)
