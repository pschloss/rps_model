source("src/rock_paper_scissors.R")

interaction <- matrix(c(0, 0, 0, 0, 0, 3/4, 0, 0, 0), nrow=3, byrow=T)
base_probs <- c(10/32, 1/4, 1/3)

n_epochs <- 4000
n_rows <- 250
n_cols <- 250

environment <- fill_environment(nrow=n_rows, ncol=n_cols,
																	freq=c(0.25,0.25,0.25,0.25))

bug_counts <- rock_paper_scissors(environment, n_epochs, base_probs,
																									interaction, local=1)
png("time_course.png")
par(mar=c(5,4,1,1))
plot(log10(bug_counts[,2]+1), type="l", col="green",
													ylim=c(0,log10(n_rows*n_cols)),
													xlab="Epochs", ylab="log10 of Number of Type")
points(log10(bug_counts[,3]+1), type="l", col="blue")
points(log10(bug_counts[,4]+1), type="l", col="red")
legend("bottomright", legend=c("Resistant", "Sensitive", "Colicin"),
											col=c("green", "blue", "red"), lty=1, lwd=2)
dev.off()

tail(bug_counts)

png("map.png")
par(mar=c(1,1,1,1))
image(environment, axes=F, col=c("white", "green", "blue", "red"))
dev.off()
