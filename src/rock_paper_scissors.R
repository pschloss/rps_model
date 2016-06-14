library(Rcpp)

sourceCpp("src/rock_paper_scissors.cpp")

fill_environment <- function(nrow, ncol, freq){
	ncells <- nrow * ncol
	individuals <- sample(x=seq(0,length(freq)-1), size=ncells, prob=freq, replace=TRUE)
	matrix(individuals, nrow, ncol)
}
