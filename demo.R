source("src/rock_paper_scissors.R")

interaction <- matrix(c(0, 0, 0, 0, 0, 3/4, 0, 0, 0), nrow=3, byrow=T)
base_probs <- c(10/32, 1/4, 1/3)

n_epochs <- 4000
n_rows <- 250
n_cols <- 250

#  set.seed(1)	#				100/100 250/250
#  set.seed(2) #								250/250
#  set.seed(3)	#	50/50					250/250
#  set.seed(4) #								250/250xxx
#  set.seed(5) #								250/250
#  set.seed(6)	#
#  set.seed(7) #								250/250
#  set.seed(8) #								250/250 25/25
#  set.seed(9)	#
# set.seed(10)	#				100/100

environment <- fill_environment(nrow=n_rows, ncol=n_cols, freq=c(1/4,1/4,1/4,1/4))

bug_counts <- rock_paper_scissors(environment, n_epochs, base_probs, interaction, local=1)

plot(log10(bug_counts[,2]+1), type="l", col="green", ylim=c(0,log10(n_rows*n_cols)))
points(log10(bug_counts[,3]+1), type="l", col="blue")
points(log10(bug_counts[,4]+1), type="l", col="red")
legend("bottomright", legend=c("Resistant", "Sensitive", "Colicin"), col=c("green", "blue", "red"), lty=1, lwd=2)

tail(bug_counts)
