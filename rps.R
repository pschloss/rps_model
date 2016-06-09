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


get_neighborhood <- function(row, column){

	n_rows <- nrow(environment)
	n_cols <- ncol(environment)

	rows <- rep(NA, 3)

	if(row != 1 & row != n_rows){
		rows <- (row-1):(row+1)
	} else if(row==1){
		rows <- c(n_rows, 1, 2)
	} else if(row==n_rows){
		rows <- c(n_rows-1, n_rows, 1)
	}

	cols <- rep(NA, 3)

	if(column != 1 & column != n_cols){
		cols <- (column-1) : (column+1)
	} else if(column == 1){
		cols <- c(n_cols, 1, 2)
	} else {
		cols <- c(n_cols-1, n_cols, 1)
	}

	as.vector(environment[rows, cols])[-5]

}

compete <- function(state, neighbors){

	neighbor_count <- table(factor(neighbors, levels=1:nrow(interaction)))
	probability <- interaction[state, 1] +
														sum(interaction[state,-1] * neighbor_count)
	ifelse(runif(1) < probability, 0, state)
}


update_focus_state <- function(row, col){

	row <- rand_row[1]
	col <- rand_col[2]

	focus_state <- environment[row, col]
	neighborhood <- get_neighborhood(row, col)

	if(focus_state != 0){
		environment[row, col] <<- compete(focus_state, neighborhood)
	} else {
		environment[row, col] <<- sample(neighborhood, 1)
	}

}

interaction <- matrix(c(10/32,	0,	0,		0,
								 				1/4,		0,	0,	3/4,
								 				1/3,		0,	0,		0), nrow=3, byrow=T)

counts <- c(50,50,50)
environment <- fill_environment(counts, n_cells=150*150)
orig_environment <- environment

n_rows <- nrow(environment)
n_cols <- ncol(environment)
n_cells <- length(environment)


for(i in 1:3){
	#one epoch...
	rand_row <- sample(1:n_rows, n_cells, replace=T)
	rand_col <- sample(1:n_cols, n_cells, replace=T)
	mapply(update_focus_state, row=rand_row, col=rand_col)

	if(i %% 10 == 0){print(i)}
}
