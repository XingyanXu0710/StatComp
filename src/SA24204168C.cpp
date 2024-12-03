#include <Rcpp.h>
using namespace Rcpp;

//' @title count loss function 
//' @description count loss function
//' @param x Independent variable (dataframe)
//' @param y Dependent variable (dataframe)
//' @param beta coefficent
//' @param i ith unit
//' @return The loss function for the i-th unit
//' @examples
//' \dontrun{
//' x <- matrix(1:9, nrow = 3, ncol = 3)
//' y <- matrix(10:18, nrow = 3, ncol = 3)
//' count(x, y, 1, 1)
//' }
//' @export
// [[Rcpp::export]]
double count(NumericMatrix x, NumericMatrix y, double beta, int i) {
  // 初始化平方误差的和
  double sum = 0.0;
  
  // 获取 y 和 x 对应行的差异 (y[i, ] - x[i, ] * beta)
  for (int j = 0; j < x.ncol(); j++) {
    sum += pow(y(i, j) - x(i, j) * beta, 2);  // 计算每个元素的差的平方并累加
  }
  
  return sum;
}
