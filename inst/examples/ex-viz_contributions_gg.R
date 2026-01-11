## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Plot contributions using ggplot2
viz_contributions_gg(X, axes = 1)
