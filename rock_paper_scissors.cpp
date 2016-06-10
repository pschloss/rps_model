#include <Rcpp.h>
using namespace Rcpp;


int compete(int state, IntegerVector neighbors, NumericVector base_prob, NumericMatrix interaction) {

	NumericVector frequency(base_prob.length()+1);

	for(int i=0;i<neighbors.size();i++){
		frequency(neighbors(i))++;
	}
	for(int i=0;i<frequency.size();i++){
		frequency(i) = frequency(i) / neighbors.size();
	}

	float probability = base_prob(state-1);

	for(int i=1;i<interaction.nrow();i++){
		probability += interaction(state-1, i) * frequency(i);
	}

	if(runif(1)(0) < probability){	state = 0;	}

	return(state);
}


int give_birth(IntegerVector neighbors){

	int rand_number = floor(8*runif(1)(0));
	////Rcout << rand_number << std::endl;
	return(neighbors(rand_number));

}


IntegerVector get_neighborhood(IntegerMatrix grid, int row, int column){
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

IntegerMatrix run_epoch(IntegerMatrix grid, IntegerVector row_vector, IntegerVector column_vector, NumericVector base_prob, NumericMatrix interaction) {
	int dummy_counter = 0;

	//Rcout << grid << std::endl;

	int epoch_length = row_vector.length();

	for(int i=0;i<epoch_length; i++){
		int row = row_vector(i) - 1; //needed to switch to 0-based indexing
		int col = column_vector(i) - 1;	//needed to switch to 0-based indexing

		//Rcout << "loop: " << i << std::endl;
		//Rcout << row << '\t' << col << std::endl;
		//Rcout << grid << std::endl;

		IntegerVector neighborhood = get_neighborhood(grid, row, col);

		//Rcout << neighborhood << std::endl;

		int status = grid(row, col);

		//Rcout << "status: " << grid(row, col) << std::endl;
		//Rcout << "status: " << status << std::endl;

		if(status != 0){
			//Rcout << "status != 0" << std::endl;

			grid(row, col) = compete(status, neighborhood, base_prob, interaction);
			//Rcout << "new status: " << grid(row, col) << std::endl;
			//Rcout << grid << std::endl;

		} else {

			//Rcout << "status == 0" << std::endl;
			grid(row, col) = give_birth(neighborhood);
			//Rcout << "new status: " << grid(row, col) << std::endl;
			//Rcout << grid << std::endl;

		}

		if(status != grid(row, col)) dummy_counter++;
	}
	//Rcout << grid << std::endl;

	return(grid);
}
