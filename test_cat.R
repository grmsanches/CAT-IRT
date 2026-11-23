#!/usr/bin/env Rscript
# Dependency-free smoke tests for the CAT implementation.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else "test_cat.R"
project_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
source(file.path(project_dir, "cat.R"))

bank <- validate_bank(make_demo_bank(12, seed = 17))
stopifnot(nrow(bank) == 12L, all(bank$a > 0), all(bank$c >= 0 & bank$c < 1))
p <- prob_correct(c(-2, 0, 2), a = 1, b = 0, c = 0.2)
stopifnot(all(p > 0.2 & p < 1), p[1] < p[2], p[2] < p[3])
info <- fisher_information(c(-1, 0, 1), a = 1, b = 0, c = 0.2)
stopifnot(all(is.finite(info)), all(info >= 0))

results <- run_comparison(bank, n_persons = 3, max_items = 8,
                          target_sd = 0.01, grid_points = 101, seed = 99)
stopifnot(nrow(results) == 6L, setequal(unique(results$method), c("fisher", "entropy")),
          all(results$n_items >= 1 & results$n_items <= 8),
          all(is.finite(results$theta_hat)), all(is.finite(results$rmse_error)))
summary <- summarize_comparison(results)
stopifnot(nrow(summary) == 2L, all(summary$rmse >= 0), all(summary$mean_items >= 1))
cat("All CAT smoke tests passed.\n")
