Sys.setenv(LANGUAGE = "en") # Force locale

# PCA ==========================================================================
data("countries")

row_zeros <- countries
row_zeros[1, ] <- NA
expect_error(pca(row_zeros))

res <- pca(countries, center = TRUE, scale = FALSE, rank = 5)
expect_stdout(show(res), "Principal Components Analysis")
expect_equal(dim(res), 5L)
expect_equal(rownames(res), rownames(countries))
expect_equal(colnames(res), colnames(countries))
expect_equal(dimnames(res), dimnames(countries))

expect_equal(get_data(res), countries)

# Points coordinates
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = TRUE), file = "_snaps/pca_row_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = TRUE), file = "_snaps/pca_col_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = FALSE), file = "_snaps/pca_row_standard.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = FALSE), file = "_snaps/pca_col_standard.rds")

# Tidy coordinates
expect_equal_to_reference(tidy(res, margin = 1), file = "_snaps/pca_row_tidy.rds")
expect_equal_to_reference(tidy(res, margin = 2), file = "_snaps/pca_col_tidy.rds")
expect_equal_to_reference(augment(res, margin = 1), file = "_snaps/pca_row_augment.rds")
expect_equal_to_reference(augment(res, margin = 2), file = "_snaps/pca_col_augment.rds")

# Distances
expect_equal_to_reference(get_distances(res, margin = 1), file = "_snaps/pca_row_distances.rds")
expect_equal_to_reference(get_distances(res, margin = 2), file = "_snaps/pca_col_distances.rds")

# Inertias
expect_equal_to_reference(get_inertia(res, margin = 1), file = "_snaps/pca_row_inertia.rds")
expect_equal_to_reference(get_inertia(res, margin = 2), file = "_snaps/pca_col_inertia.rds")

# Eigenvalues
expect_equal_to_reference(get_eigenvalues(res), file = "_snaps/pca_eigenvalues.rds")

# Correlations
expect_equal_to_reference(get_correlations(res), file = "_snaps/pca_correlations.rds")

# Supplementary observations ===================================================
res <- pca(iris, sup_row = 1:10, sup_col = 2, sup_quali = 5)
expect_identical(
  dimensio:::get_order(res, margin = 1),
  c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L,
    23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L,
    36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L,
    49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L,
    62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L,
    75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L,
    88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L,
    101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L,
    112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L,
    123L, 124L, 125L, 126L, 127L, 128L, 129L, 130L, 131L, 132L, 133L,
    134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L,
    145L, 146L, 147L, 148L, 149L, 150L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
    8L, 9L, 10L)
)
expect_identical(
  dimensio:::get_order(res, margin = 2),
  c(1L, 3L, 4L, 2L)
)

# Predict new coordinates ======================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
cts <- as.data.frame(cts)

res <- pca(cts, center = FALSE, scale = FALSE)
new_rows <- predict(res, cts, margin = 1)
new_cols <- predict(res, cts, margin = 2)

sup_rows <- get_coordinates(res, margin = 1)
sup_cols <- get_coordinates(res, margin = 2)

expect_equivalent(new_rows, sup_rows[, 1:4], ignore_attr = TRUE)
expect_equivalent(new_cols, sup_cols[, 1:4], ignore_attr = TRUE)
