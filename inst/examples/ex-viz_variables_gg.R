## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Plot variables with ggplot2
viz_variables_gg(X)

## Graphical parameters
## Continuous values
viz_variables_gg(X, extra_quanti = "contribution", size = c(0, 1))

## Discrete values
viz_variables_gg(X, extra_quali = c("Petal", "Petal", "Sepal", "Sepal"),
                 symbol = c(1, 3))
