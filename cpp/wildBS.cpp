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

struct CollectData
{
  MatrixXd XX;
  MatrixXd Xy;
};

CollectData MatrixOperation(const std::vector<ClusterData> &data)
{
  int k = data[0].X.cols(), g = data.size();
  int n = 0;
  for (int i = 0; i < g; i++)
    n += data[i].X.rows();

  // construct full design matrix and response vector
  MatrixXd X(n, k);
  VectorXd y(n);
  int start = 0;
  for (int i = 0; i < g; i++)
  {
    const ClusterData &cluster_data = data[i];
    int n_at_i = cluster_data.X.rows();
    X.block(start, 0, n_at_i, k) = cluster_data.X;
    y.segment(start, n_at_i) = cluster_data.y;
    start += n_at_i;
  }

  // matrix operation
  CollectData result;
  
  MatrixXd XX(MatrixXd(k, k).setZero().selfadjointView<Lower>().rankUpdate(X.adjoint()));
  const FullPivLU<MatrixXd> fplu(XX);
  result.XX = fplu.inverse();
  result.Xy = X.adjoint() * y;

  return result;
}

VectorXd get_beta(const CollectData& data)
{
  VectorXd beta = data.XX * data.Xy;
  return beta;
}

MatrixXd CRVE(const std::vector<ClusterData>& data, const VectorXd beta, const MatrixXd invXtX)
{
  int k = beta.size(), g = data.size();
  // compute the cluster-robust variance-covariance matrix (CRVE)
  MatrixXd XeeX(k, k);
  XeeX.setZero();
  int n = 0;
  for (int i = 0; i < g; i++)
  {
    const ClusterData &cluster_data = data[i];
    VectorXd fitted = cluster_data.X * beta;
    VectorXd resid = cluster_data.y - fitted;
    MatrixXd gXeeX(k, k);
    gXeeX = cluster_data.X.adjoint() * resid * resid.adjoint() * cluster_data.X;
    XeeX += gXeeX;
    n += cluster_data.X.rows();
  }

  MatrixXd vcov = invXtX * XeeX * invXtX;

  double nd = n, kd = k, gd = g;
  double factor = ((nd - 1) / (nd - kd)) * (gd / (gd - 1));
  vcov *= factor;

  return vcov;
}

double Wald(const VectorXd& beta, const MatrixXd& vcov, const int& pos, const double& rhs)
{
  double tar_beta = beta(pos - 1);
  double tar_se = sqrt(vcov(pos - 1, pos - 1));
  double wald = (tar_beta - rhs) / tar_se;
  return wald;
}

// [[Rcpp::export]]
NumericMatrix clusterOLS( const NumericMatrix &xcpp,
                          const NumericVector &ycpp,
                          const IntegerVector &gcpp)
{
  std::vector<ClusterData> setup = create_cluster_data(xcpp, ycpp, gcpp);
  CollectData calc = MatrixOperation(setup);
  VectorXd beta = get_beta(calc);
  MatrixXd vcov = CRVE(setup, beta, calc.XX);

  SEXP s_vcov = Rcpp::wrap(vcov);
  NumericMatrix ss_vcov(s_vcov);
  return ss_vcov;
}

// [[Rcpp::export]]
double ClusterOLS_Wald( const NumericMatrix &xcpp,
                        const NumericVector &ycpp,
                        const IntegerVector &gcpp,
                        const int &pos,
                        const double &rhs)
{
  std::vector<ClusterData> setup = create_cluster_data(xcpp, ycpp, gcpp);
  CollectData calc = MatrixOperation(setup);
  VectorXd beta = get_beta(calc);
  MatrixXd vcov = CRVE(setup, beta, calc.XX);
  double wald = Wald(beta, vcov, pos, rhs);
  return wald;
}