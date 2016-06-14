library("testthat")
set.seed(1)

test_that("fill_environment", {

	nrow <- 1e3
	ncol <- 1e3
	ncells <- nrow * ncol

	environment <- fill_environment(nrow,ncol,1)
	expect_equal(nrow(environment), nrow)
	expect_equal(ncol(environment), ncol)
	expect_equal(sum(environment==0), ncells)

	environment <- fill_environment(nrow,ncol,c(0.5,0.5))
	expect_equal(sum(environment==0)/ncells, 0.5, tol=1e-3)
	expect_equal(sum(environment==1)/ncells, 0.5, tol=1e-3)

	environment <- fill_environment(nrow,ncol,c(1/3,1/3,1/3))
	expect_equal(sum(environment==0)/ncells, 1/3, tol=1e-3)
	expect_equal(sum(environment==1)/ncells, 1/3, tol=1e-3)
	expect_equal(sum(environment==2)/ncells, 1/3, tol=1e-3)
})


test_that("compete", {
	interaction <- matrix(c(0, 0, 0, 0, 0, 3/4, 0, 0, 0), nrow=3, byrow=T)
	base_probs <- c(10/32, 1/4, 1/3)
	neighbors <- c(rep(0,3), rep(1,3), rep(2,3), rep(0,3))

	expect_equal(1-mean(replicate(1e5,compete(1, neighbors, base_probs, interaction))==1), base_probs[1], tol=1e-2)

	expect_equal(1-mean(replicate(1e5,compete(2, neighbors, base_probs, interaction))==2), base_probs[2], tol=1e-2)
	neighbors[1] <- 3
	expect_equal(1-mean(replicate(1e5,compete(2, neighbors, base_probs, interaction))==2), base_probs[2]+interaction[2,3]*1/12, tol=1e-2)
	neighbors[2] <- 3
	expect_equal(1-mean(replicate(1e5,compete(2, neighbors, base_probs, interaction))==2), base_probs[2]+interaction[2,3]*2/12, tol=1e-2)
	neighbors[3] <- 3
	expect_equal(1-mean(replicate(1e5,compete(2, neighbors, base_probs, interaction))==2), base_probs[2]+interaction[2,3]*3/12, tol=1e-2)

	expect_equal(1-mean(replicate(1e5,compete(3, neighbors, base_probs, interaction))==3), base_probs[3], tol=1e-2)

})


test_that("rand_range", {
	reps <- 1e4
	t <- as.vector(table(replicate(reps, rand_range(0, 10))))/reps
	expect_equal(length(t), 10)
	expect_equal(t, rep(1/10, 10), tol=1e-2)
})


test_that("give_birth", {
	reps <- 1e4
	expect_equal(sum(replicate(reps, give_birth(rep(1,6)))), reps)
	expect_equal(sum(replicate(reps, give_birth(c(rep(1,3), rep(2,3)))))/reps, 1.5, tol=1e-2)
	expect_equal(sum(replicate(reps, give_birth(c(rep(0,3), rep(1,3), rep(2,3)))))/reps, 1, tol=1e-2)
})


test_that("get_global_neighborhood", {
	a <- matrix(1:20, nrow=4, ncol=5)
	expect_equal(as.vector(get_global_neighborhood(a,0,0)), 2:20)
	expect_equal(as.vector(get_global_neighborhood(a,3,3)), c(1:15,17:20))
})


test_that("get_local_neighborhood", {
	a <- matrix(1:20, nrow=4, ncol=5)
	expect_equal(get_local_neighborhood(a,0,0), c(20,4,8,17,5,18,2,6))
	expect_equal(get_local_neighborhood(a,0,4), c(16,20,4,13,1,14,18,2))
	expect_equal(get_local_neighborhood(a,3,0), c(19,3,7,20,8,17,1,5))
	expect_equal(get_local_neighborhood(a,3,4), c(15,19,3,16,4,13,17,1))
	expect_equal(get_local_neighborhood(a,2,2), c(6,10,14,7,15,8,12,16))
})


test_that("rock_paper_scissors", {

	interaction <- matrix(c(0, 0, 0, 0, 0, 3/4, 0, 0, 0), nrow=3, byrow=T)
	base_probs <- c(10/32, 1/4, 1/3)
	n_epochs <- 500
	n_rows <- 50
	n_cols <- 50

	#global neighborood
	set.seed(1)
	environment <- fill_environment(nrow=n_rows, ncol=n_cols, freq=c(1/4,1/4,1/4,1/4))

	bug_counts <- rock_paper_scissors(environment, n_epochs, base_probs, interaction, local=0)
	plot(log10(bug_counts[,2]+1), type="l", col="green", ylim=c(0,log10(n_rows*n_cols)))
	points(log10(bug_counts[,3]+1), type="l", col="blue")
	points(log10(bug_counts[,4]+1), type="l", col="red")
	legend("topright", legend=c("Resistant", "Sensitive", "Colicin"), col=c("green", "blue", "red"), lty=1, lwd=2)

	expect_equal(nrow(bug_counts), n_epochs+1)
	expect_equal(ncol(bug_counts), length(base_probs)+1)
	expect_false(bug_counts[n_epochs+1,2] == 0)
	expect_equal(bug_counts[n_epochs+1,3], 0)
	expect_equal(bug_counts[n_epochs+1,4], 0)



	#local neighborood
	set.seed(1)
	environment <- fill_environment(nrow=n_rows, ncol=n_cols, freq=c(1/4,1/4,1/4,1/4))

	bug_counts <- rock_paper_scissors(environment, n_epochs, base_probs, interaction, local=1)
	plot(log10(bug_counts[,2]+1), type="l", col="green", ylim=c(0,log10(n_rows*n_cols)))
	points(log10(bug_counts[,3]+1), type="l", col="blue")
	points(log10(bug_counts[,4]+1), type="l", col="red")
	legend("topright", legend=c("Resistant", "Sensitive", "Colicin"), col=c("green", "blue", "red"), lty=1, lwd=2)

	expect_equal(nrow(bug_counts), n_epochs+1)
	expect_equal(ncol(bug_counts), length(base_probs)+1)
	expect_false(bug_counts[n_epochs+1,2] == 0)
	expect_false(bug_counts[n_epochs+1,3] == 0)
	expect_false(bug_counts[n_epochs+1,4] == 0)
})
