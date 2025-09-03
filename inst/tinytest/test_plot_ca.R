Sys.setenv(LANGUAGE = "en") # Force locale

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("benthos")
  res <- ca(benthos, sup_row = 1:5, sup_col = 1)

  # Coordinates ================================================================
  ## Rows ----------------------------------------------------------------------
  plot_row <- function() viz_rows(res, axes = c(1, 2))
  expect_snapshot_plot(plot_row, "CA_row")

  plot_row <- function() viz_rows(res, extra_quali = "observation", symbol = c(1, 2))
  expect_snapshot_plot(plot_row, "CA_row_sup")

  plot_row <- function() viz_rows(res, extra_quali = "observation", symbol = c(1, 2), active = FALSE)
  expect_snapshot_plot(plot_row, "CA_row_sup_noactive")

  plot_row <- function() viz_rows(res, extra_quali = "observation", symbol = c(1, 2), sup = FALSE)
  expect_snapshot_plot(plot_row, "CA_row_sup_nosup")

  ## Variables -----------------------------------------------------------------
  plot_col <- function() viz_columns(res, axes = c(1, 2), labels = FALSE)
  expect_snapshot_plot(plot_col, "CA_col")

  plot_col <- function() viz_columns(res, extra_quali = "observation", symbol = c(1, 2), labels = FALSE)
  expect_snapshot_plot(plot_col, "CA_col_sup")

  plot_col <- function() viz_columns(res, extra_quali = "observation", symbol = c(1, 2), active = FALSE, labels = FALSE)
  expect_snapshot_plot(plot_col, "CA_col_sup_noactive")

  plot_col <- function() viz_columns(res, extra_quali = "observation", symbol = c(1, 2), sup = FALSE, labels = FALSE)
  expect_snapshot_plot(plot_col, "CA_col_sup_nosup")

  num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  plot_var_group_num <- function() viz_columns(res, extra_quanti = num, size = c(1, 2), labels = FALSE)
  expect_snapshot_plot(plot_var_group_num, "PCA_col_group_num")

  # Eigenvalues ================================================================
  plot_scree <- function() screeplot(res, eigenvalues = TRUE, cumulative = FALSE)
  expect_snapshot_plot(plot_scree, "CA_eigen")

  plot_scree <- function() screeplot(res, eigenvalues = TRUE, cumulative = TRUE)
  expect_snapshot_plot(plot_scree, "CA_eigen_cumulative")

  plot_scree <- function() screeplot(res, eigenvalues = FALSE, cumulative = FALSE)
  expect_snapshot_plot(plot_scree, "CA_variance")

  plot_scree <- function() screeplot(res, eigenvalues = FALSE, cumulative = TRUE)
  expect_snapshot_plot(plot_scree, "CA_variance_cumulative")

  # Contributions ==============================================================
  plot_contrib_1 <- function() viz_contributions(res, margin = 1, axes = c(1, 2))
  expect_snapshot_plot(plot_contrib_1, "CA_row_contrib")

  plot_contrib_2 <- function() viz_contributions(res, margin = 2, axes = 1)
  expect_snapshot_plot(plot_contrib_2, "CA_col_contrib")

  # Squared cosine =============================================================
  plot_cos2 <- function() viz_cos2(res, margin = 2)
  expect_snapshot_plot(plot_cos2, "CA_col_cos2")
}
