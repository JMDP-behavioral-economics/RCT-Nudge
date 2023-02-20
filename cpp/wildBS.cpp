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
  Map<VectorXi> g(as<Map<VectorXi>>(gcpp));

  std::vector<ClusterData> cluster_data_vec;
  
  // Get the unique cluster IDs and the number of clusters
  int n = g.size();
  std::vector<int> unique_clusters;
  std::vector<int>::iterator it_start, it_end;
  it_start = unique_clusters.begin(), it_end = unique_clusters.end();
  int num_clusters = 0;
  for (int i = 0; i < n; i++)
  {
    int cluster_id = g(i);
    if (std::find(it_start, it_end, cluster_id) == it_end)
    {
      unique_clusters.push_back(cluster_id);
      num_clusters++;
    }
  }

  // Create ClusterData object for each cluster
  int k = X.cols();
  for (int i = 0; i < num_clusters; i++)
  {
    int cluster_id = unique_clusters[i];

    int n_at_i = 0;
    for (int j = 0; j < n; j++) if(g(j) == cluster_id) n_at_i++;

    Eigen::MatrixXd X_cluster(n_at_i, k);
    Eigen::VectorXd y_cluster(n_at_i);

    // Subset X and Y for the current cluster
    int row_index = 0;
    for (int j = 0; j < n; j++)
    {
      if (g(j) == cluster_id)
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

VectorXd OLS(const std::vector<ClusterData>& data)
{
  int k = data[0].X.cols(), g = data.size();
  int n = 0;
  for (int i = 0; i < g; i++) n += data[i].X.rows();

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
  MatrixXd Xy = X * y;
  VectorXd beta = invXX * Xy;

  return beta;
}

// [[Rcpp::export]]
VectorXd runOLS(const NumericMatrix &xcpp,
                const NumericVector &ycpp,
                const IntegerVector &gcpp)
{
  std::vector<ClusterData> setup = create_cluster_data(xcpp, ycpp, gcpp);
  Rcpp::Rcout << "Create dataset!\n";
  return OLS(setup);
}