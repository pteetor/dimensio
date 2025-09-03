Sys.setenv(LANGUAGE = "en") # Force locale

# CA ===========================================================================
data("benthos")
res <- ca(benthos, sup_row = 1:5, sup_col = 1)

## Prepare data ----------------------------------------------------------------
coords <- dimensio:::prepare_plot(res, margin = 1)
expect_identical(tail(coords$label, 5), rownames(benthos)[1:5])
coords <- dimensio:::prepare_plot(res, margin = 2)
expect_identical(tail(coords$label, 1), colnames(benthos)[[1L]])

# PCA ==========================================================================
data("iris")
res <- pca(iris, sup_row = 1:10, sup_col = 2, sup_quali = 5)

## Prepare data ----------------------------------------------------------------
expect_identical(
  dimensio:::prepare_plot(res, margin = 1, extra_quali = "Species"),
  dimensio:::prepare_plot(res, margin = 1, extra_quali = iris$Species)
)

coords <- dimensio:::prepare_plot(res, margin = 2, extra_quali = startsWith(head(colnames(iris), -1), "Petal"))
expect_identical(coords$extra_quali, c(FALSE, TRUE, TRUE, FALSE))
expect_identical(coords$supplementary, c(FALSE, FALSE, FALSE, TRUE))

## Prepare legend --------------------------------------------------------------
spe <- iris$Species
spe[1:10] <- NA

coords <- dimensio:::prepare_plot(res, margin = 1, extra_quali = spe)
leg <- dimensio:::prepare_legend(coords, args = list(x = "topright"))
expect_false(anyNA(leg$legend))

cte <- rep(1, nrow(iris))
coords <- dimensio:::prepare_plot(res, margin = 1, extra_quanti = cte)
leg <- dimensio:::prepare_legend(coords, args = list(x = "topright"))
expect_true(all(lengths(leg) == 1))
