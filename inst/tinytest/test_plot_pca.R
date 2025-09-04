Sys.setenv(LANGUAGE = "en") # Force locale

using("tinysnapshot")
source("helpers.R")

data("iris")
res <- pca(iris, sup_row = 1:10, sup_col = 3, sup_quali = 5)

# Coordinates ==================================================================
## Individuals -----------------------------------------------------------------
plot_ind <- function() viz_individuals(res, axes = c(1, 2))
expect_snapshot_plot(plot_ind, "PCA_ind")

plot_ind <- function() viz_individuals(res, extra_quali = "observation")
expect_snapshot_plot(plot_ind, "PCA_ind_sup")

plot_ind <- function() viz_individuals(res, extra_quali = "observation", active = FALSE)
expect_snapshot_plot(plot_ind, "PCA_ind_sup_noactive")

plot_ind <- function() viz_individuals(res, extra_quali = "observation", sup = FALSE)
expect_snapshot_plot(plot_ind, "PCA_ind_sup_nosup")

spe <- iris$Species
plot_ind <- function() viz_individuals(res, extra_quali = spe)
expect_snapshot_plot(plot_ind, "PCA_ind_sup_extra")

plot_ind_quali <- function() viz_individuals(res, extra_quali = spe, symbol = c(1, 2, 3))
expect_snapshot_plot(plot_ind_quali, "PCA_ind_highlight_quali")

plot_ind_cos2 <- function() viz_individuals(res, extra_quanti = "cos2", size = c(0, 3))
expect_snapshot_plot(plot_ind_cos2, "PCA_ind_highlight_cos2")

plot_ind_contrib <- function() viz_individuals(res, extra_quanti = "contrib", size = c(0, 3))
expect_snapshot_plot(plot_ind_contrib, "PCA_ind_highlight_contrib")

plot_ind_group <- function() viz_individuals(res, extra_quali = spe, extra_quanti = "contrib", size = c(0, 3))
expect_snapshot_plot(plot_ind_group, "PCA_ind_highlight_quali_quanti")

## Variables -------------------------------------------------------------------
plot_var <- function() viz_variables(res, axes = c(1, 2), labels = FALSE)
expect_snapshot_plot(plot_var, "PCA_var")

plot_var <- function() viz_variables(res, extra_quali = "observation", labels = FALSE)
expect_snapshot_plot(plot_var, "PCA_var_sup")

plot_var <- function() viz_variables(res, extra_quali = "observation", active = FALSE, labels = FALSE)
expect_snapshot_plot(plot_var, "PCA_var_sup_noactive")

plot_var <- function() viz_variables(res, extra_quali = "observation", sup = FALSE, labels = FALSE)
expect_snapshot_plot(plot_var, "PCA_var_sup_nosup")

num <- c(1, 2, 3, 4)
plot_var_group_num <- function() viz_variables(res, extra_quanti = num, size = c(1, 2), labels = FALSE)
expect_snapshot_plot(plot_var_group_num, "PCA_var_group_num")

cat <- c("Sepal", "Sepal", "Petal", "Petal")
plot_var_group_cat <- function() viz_variables(res, extra_quali = cat, symbol = c(1, 2), labels = FALSE)
expect_snapshot_plot(plot_var_group_cat, "PCA_var_group_cat")

plot_var_contrib <- function() viz_variables(res, extra_quanti = "contrib", size = c(0, 3), labels = FALSE)
expect_snapshot_plot(plot_var_contrib, "PCA_var_highlight_contrib")

# Envelopes ====================================================================
plot_ind_hull <- function() viz_individuals(res, extra_quali = iris$Species, hull = TRUE)
expect_snapshot_plot(plot_ind_hull, "PCA_ind_hull")

params <- list(type = "confidence", level = 0.95)
plot_ind_conf <- function() viz_individuals(res, extra_quali = iris$Species, ellipse = params)
expect_snapshot_plot(plot_ind_hull, "PCA_ind_confidence")

params <- list(type = "tolerance", level = 0.95)
plot_ind_tol <- function() viz_individuals(res, extra_quali = iris$Species, ellipse = params)
expect_snapshot_plot(plot_ind_hull, "PCA_ind_tolerance")

# Eigenvalues ==================================================================
plot_scree <- function() screeplot(res, eigenvalues = TRUE, cumulative = FALSE)
expect_snapshot_plot(plot_scree, "PCA_eigen")

plot_scree <- function() screeplot(res, eigenvalues = TRUE, cumulative = TRUE)
expect_snapshot_plot(plot_scree, "PCA_eigen_cumulative")

plot_scree <- function() screeplot(res, eigenvalues = FALSE, cumulative = FALSE)
expect_snapshot_plot(plot_scree, "PCA_variance")

plot_scree <- function() screeplot(res, eigenvalues = FALSE, cumulative = TRUE)
expect_snapshot_plot(plot_scree, "PCA_variance_cumulative")

# Contributions ================================================================
plot_contrib_1 <- function() viz_contributions(res, margin = 1, axes = c(1, 2))
expect_snapshot_plot(plot_contrib_1, "PCA_ind_contrib")

plot_contrib_2 <- function() viz_contributions(res, margin = 2, axes = 1)
expect_snapshot_plot(plot_contrib_2, "PCA_var_contrib")

# Squared cosine ===============================================================
plot_cos2 <- function() viz_cos2(res, margin = 2)
expect_snapshot_plot(plot_cos2, "PCA_var_cos2")
