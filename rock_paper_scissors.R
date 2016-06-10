library(Rcpp)
sourceCpp("rock_paper_scissors.cpp")


fill_environment <- function(counts, n_cells = sum(counts)){

	nrow <- round(sqrt(n_cells))
	ncol <- nrow

	if(nrow * ncol < n_cells) {
		ncol <- ncol + 1
	}

	individuals <- rep(1:length(counts), counts)
	extra <- rep(0, (nrow * ncol) - sum(counts))
	individuals <- c(individuals, extra)

	matrix(sample(individuals), nrow, ncol)
}


interaction <- matrix(c(0, 0, 0, 0, 0, 3/4, 0, 0, 0), nrow=3, byrow=T)
base_probs <- c(10/32, 1/4, 1/3)
counts <- c(10,10,10)
environment <- fill_environment(counts)
orig_environment <- environment

n_rows <- nrow(environment)
n_cols <- ncol(environment)
epoch_length <- n_rows * n_cols

rand_row <- sample(1:n_rows, epoch_length, replace=T)
rand_col <- sample(1:n_cols, epoch_length, replace=T)

set.seed(1)
replicate(1000,run_epoch(environment, rand_row, rand_col, base_probs, interaction))
