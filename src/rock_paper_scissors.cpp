#include <Rcpp.h>
using namespace Rcpp;


int compete(int state, IntegerVector neighbors, NumericVector base_prob, NumericMatrix interaction) {

	NumericVector frequency(base_prob.length()+1, 0);

	for(int i=0;i<neighbors.size();i++){
		frequency(neighbors(i))++;
	}

	for(int i=0;i<frequency.size();i++){
		frequency(i) = frequency(i) / ((double)neighbors.size()-1);
	}
	float probability = base_prob(state-1);

	for(int i=0;i<interaction.ncol();i++){
		// Rcout << interaction(state-1,i) << '\t' << frequency(i+1) << std::endl;
		probability += interaction(state-1, i) * frequency(i+1);
	}
	// Rcout << "------\n";

	if(runif(1)(0) < probability){	state = 0;	}

	return(state);
}

// [[Rcpp::export]]

//return a random integer within  [min, max)
int rand_range(int min, int max){
	int rand_number = min+floor(max*runif(1)(0));
	return(rand_number);
}

// [[Rcpp::export]]

int give_birth(IntegerVector neighbors){
	int rand_number = floor(neighbors.length()*runif(1)(0));
	return neighbors(rand_number);
}

// [[Rcpp::export]]

IntegerVector get_global_neighborhood(IntegerMatrix& grid, int row, int column){
	IntegerVector neighborhood(grid);
	return neighborhood;
}


IntegerVector get_local_neighborhood(IntegerMatrix grid, int row, int column){
//http://stackoverflow.com/questions/18964603/finding-neighbourhood-in-matrices

	int n_rows = grid.nrow();
	int n_cols = grid.ncol();

	int above = (row + n_rows - 1) % n_rows;
	int below = (row + n_rows  + 1) % n_rows;
	int left = (column + n_cols - 1) % n_cols;
	int right = (column + n_cols + 1) % n_cols;

	IntegerVector neighborhood(8);
	neighborhood(0) = grid(above,left);
	neighborhood(1) = grid(above,column);
	neighborhood(2) = grid(above,right);
	neighborhood(3) = grid(row,left);
	neighborhood(4) = grid(row,right);
	neighborhood(5) = grid(below,left);
	neighborhood(6) = grid(below,column);
	neighborhood(7) = grid(below,right);

	return(neighborhood);
}


// [[Rcpp::export]]

IntegerMatrix rock_paper_scissors(IntegerMatrix grid, int n_epochs,
 						NumericVector base_probs, NumericMatrix interaction) {

	int n_bugs = interaction.nrow() + 1;
	n_epochs++;
	IntegerMatrix count_matrix(n_epochs, n_bugs);

	for(int i=0;i<n_bugs;i++){
		count_matrix(0,i) = sum(grid==i);
	}

	int epoch_length = grid.length();

	for(int i=1;i<n_epochs;i++){

		for(int j=0;j<epoch_length; j++){
			int rand_row = rand_range(0,grid.nrow());
			int rand_col = rand_range(0,grid.ncol());

			IntegerVector neighborhood = get_global_neighborhood(grid, rand_row, rand_col);
			//IntegerVector neighborhood = get_local_neighborhood(grid, rand_row, rand_col);


			int status = grid(rand_row, rand_col);

			if(status != 0){
				grid(rand_row, rand_col) = compete(status, neighborhood, base_probs, interaction);
			} else {
				grid(rand_row, rand_col) = give_birth(neighborhood);
			}

		}

		for(int j=0;j<n_bugs;j++){
			count_matrix(i,j) = sum(grid==j);
		}
			Rcout << grid << std::endl << std::endl;

		if(i % 100 == 0)	{
			Rcout << i << std::endl;
		}

	}
	return(count_matrix);
}
