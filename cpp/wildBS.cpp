// [[Rcpp::depends(RcppEigen)]]
#include "Rcpp.h"
#include "RcppEigen.h"

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::FullPivLU;

struct ClusterData{
  MatrixXd X;
  VectorXd y;
};

using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;
using Rcpp::as;

std::vector<ClusterData> create_cluster_data( const NumericMatrix& xcpp,
                                              const NumericVector& ycpp,
                                              const IntegerVector& gcpp)
{
  Map<MatrixXd> X(as<Map<MatrixXd>>(xcpp));
  Map<VectorXd> y(as<Map<VectorXd>>(ycpp));

  std::vector<ClusterData> cluster_data_vec;
  
  // Get the unique cluster IDs and the number of clusters
  int n = gcpp.size();
  std::map<int, int> count_g;
  for (int i = 0; i < n; i++) count_g[gcpp[i]]++;

  // Create ClusterData object for each cluster
  int k = X.cols();
  std::map<int, int>::iterator start, end;
  start = count_g.begin(), end = count_g.end();
  for (auto it = start; it != end; it++)
  {
    int cluster_id = it->first;
    int n_at_i = it->second;

    Eigen::MatrixXd X_cluster(n_at_i, k);
    Eigen::VectorXd y_cluster(n_at_i);

    // Subset X and Y for the current cluster
    int row_index = 0;
    for (int j = 0; j < n; j++)
    {
      if (gcpp[j] == cluster_id)
      {
        X_cluster.row(row_index) = X.row(j);
        y_cluster(row_index) = y(j);
        row_index++;
      }
    }

    // Create a ClusterData object for the current cluster
    ClusterData cluster_data;
    cluster_data.X = X_cluster;
    cluster_data.y = y_cluster;

    // Add the ClusterData object to the vector
    cluster_data_vec.push_back(cluster_data);
  }

  return cluster_data_vec;
}

using Eigen::SelfAdjointView;
using Eigen::SelfAdjointEigenSolver;
using Eigen::Lower;
using Eigen::Upper;

VectorXd doOLS(const std::vector<ClusterData>& data)
{
  int k = data[0].X.cols(), g = data.size();
  // Rcpp::Rcout << "#columns =" << k << "\n";
  // Rcpp::Rcout << "#clusters =" << g << "\n";
  int n = 0;
  for (int i = 0; i < g; i++) n += data[i].X.rows();
  // Rcpp::Rcout << "#obs =" << n << "\n";

  // construct full design matrix and response vector
  MatrixXd X(n, k);
  VectorXd y(n);
  int start = 0;
  for (int i = 0; i < g; i++)
  {
    const ClusterData& cluster_data = data[i];
    int n_at_i = cluster_data.X.rows();
    X.block(start, 0, n_at_i, k) = cluster_data.X;
    y.segment(start, n_at_i) = cluster_data.y;
    start += n_at_i;
  }

  // run OLS
  MatrixXd XX(MatrixXd(k, k).setZero().selfadjointView<Lower>().rankUpdate(X.adjoint()));
  const FullPivLU<MatrixXd> fplu(XX);
  MatrixXd invXX = fplu.inverse();
  MatrixXd Xy = X.adjoint() * y;
  VectorXd beta = invXX * Xy;

  return beta;
}

// [[Rcpp::export]]
VectorXd OLS( const NumericMatrix &xcpp,
              const NumericVector &ycpp,
              const IntegerVector &gcpp)
{
  std::vector<ClusterData> setup = create_cluster_data(xcpp, ycpp, gcpp);
  // Rcpp::Rcout << "Create dataset!\n";
  return doOLS(setup);
}