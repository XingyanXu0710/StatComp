#' @title A package used for illustration.
#' @name Illustration
#' @import ggplot2
#' @import dplyr
#' @import boot
#' @import gsl
#' @import lpSolve
#' @import bootstrap
#' @import DAAG
#' @import microbenchmark
NULL

#' @importFrom Rcpp evalCpp
#' @useDynLib SA24204168
NULL

#' @title Perform time differencing on panel data
#' @description Perform differencing on each panel data unit by time cross-section to eliminate the effect of the intercept term
#' @param x Independent variable (dataframe)
#' @param y Dependent variable (dataframe)
#' @return Differenced independent variable (x) and dependent variable (y)
#' @examples
#' \dontrun{
#' x <- data.frame(matrix(1:9, nrow = 3, ncol = 3))
#' y <- data.frame(matrix(10:18, nrow = 3, ncol = 3))
#' dif(x, y)
#' }
#' @export
#' @import Rcpp
dif <- function(x, y) {
  # 确保输入数据框 x 和 y 的行列数符合面板数据的结构
  n_y <- nrow(y)  # y 的行数 (n)
  t_y <- ncol(y)  # y 的列数 (t)
  
  n_x <- nrow(x)  # x 的行数 (n)
  t_x <- ncol(x)  # x 的列数 (t)
  
  # 检查 x 和 y 的行数是否一致
  if (n_y != n_x) {
    stop("The number of rows (n) in x and y must be the same.")
  }
  
  # 检查 x 和 y 的列数是否一致
  if (t_y != t_x) {
    stop("The number of columns (t) in x and y must be the same.")
  }
  
  # 检查 x 和 y 的列数是否大于 1，确保有时间维度
  if (t_y <= 1) {
    stop("The number of columns (t) in both x and y must be greater than 1.")
  }
  
  # 初始化差分后的数据框
  y_diff <- data.frame(matrix(NA, nrow = n_y, ncol = t_y - 1))  # Y 差分后的数据框 (n * (t-1))
  x_diff <- data.frame(matrix(NA, nrow = n_x, ncol = t_x - 1))  # X 差分后的数据框 (n * (t-1))
  
  # 对 Y 和 X 进行时间差分
  for (i in 1:n_y) {
    for (j in 2:t_y) {  # 从第二列开始，计算差分
      y_diff[i, j - 1] <- y[i, j] - y[i, j - 1]  # Y 差分
      x_diff[i, j - 1] <- x[i, j] - x[i, j - 1]  # X 差分
    }
  }
  
  # 修改差分后的数据框列名
  colnames(y_diff) <- paste("Y", 1:(t_y - 1), sep = "")  # Y 差分后的列名：Y1, Y2, ...
  colnames(x_diff) <- paste("X", 1:(t_x - 1), sep = "")  # X 差分后的列名：X1, X2, ...
  
  # 返回差分后的数据框列表
  return(list(y_diff = y_diff, x_diff = x_diff))
}



#' @title Perform group-based panel data regression
#' @description Perform group-based panel data regression
#' @param x Independent variable (dataframe)
#' @param y Dependent variable (dataframe)
#' @param G group number (int)
#' @param kappa Hyperparameters for calculating the IC value
#' @return a list including coefficient, group for each member and IC value
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 10; t <- 10
#' x <- matrix(rnorm(n * t), nrow = n, ncol = t)
#' alpha <- runif(n, 0, 1)
#' epsilon <- matrix(rnorm(n * t, mean = 0, sd = sqrt(0.2)), nrow = n, ncol = t)
#' y <- matrix(NA, nrow = n, ncol = t)
#' for (i in 1:n) {
#'   if (i <= n / 2) {
#'     y[i, ] <- x[i, ] + epsilon[i, ] + alpha[i]
#'   } else {
#'       y[i, ] <- 2 * x[i, ] + epsilon[i, ] + alpha[i]
#'     }
#' }
#' gpd(x, y, 2)
#' }
#' @importFrom stats lm coef kmeans
#' @export
gpd <- function(x, y, G, kappa = 3) {
  # 使用 dif 函数计算差分数据
  y_diff <- dif(x, y)$y_diff
  x_diff <- dif(x, y)$x_diff
  
  # 确保 y_diff 和 x_diff 是 NumericMatrix 类型
  y_diff <- as.matrix(y_diff)  # 转换为 NumericMatrix
  x_diff <- as.matrix(x_diff)  # 转换为 NumericMatrix
  
  beta <- numeric(nrow(y_diff))  # 用于存储每个面板单元的斜率
  IC <- 0   # 计算IC值
  n <- nrow(y_diff)  # 确保定义 n
  g <- numeric(n)
  is_stop <- 0   # 判断循环停止
  flag <- c(Inf, 0, 1)  # 第一个存储count()，第二个存储g，第三个判断停止条件
  
  # 使用差分后的数据进行回归
  for (i in 1:n) {
    model <- lm(unlist(y_diff[i, ]) ~ 0 + unlist(x_diff[i, ]))
    beta[i] <- coef(model)[1]  # 存储斜率
  }
  
  # 使用 K-means 算法进行聚类
  kmeans_result <- kmeans(beta, centers = G)
  beta_center <- as.vector(kmeans_result$centers)
  
  # 迭代更新
  while (is_stop < 100 && flag[3] == 1) {
    flag[3] <- 0
    
    for (i in 1:n) {
      flag[1] <- Inf
      for (j in 1:G) {
        # 将 beta_center[j] 转换为单一数值
        current_count <- count(x_diff, y_diff, beta_center[j], i-1)  # 调用 C++ 函数 count
        if (current_count < flag[1]) {
          flag[1] <- current_count
          flag[2] <- j
        }
      }
      
      if (g[i] != flag[2]) {
        flag[3] <- 1
      }
      g[i] <- flag[2]
    }
    
    # 更新 beta
    for (i in 1:G) {
      indices <- which(g == i)
      if (length(indices) > 0) {
        beta_center[i] <- sum(y_diff[indices, ] * x_diff[indices, ]) / sum(x_diff[indices, ]^2)
      }
    }
    is_stop <- is_stop + 1
  }
  
  
  # 计算IC值
  for(i in 1:n){
    IC <- IC + count(x_diff, y_diff, beta_center[g[i]], i-1)
  }
  IC <- log((IC + 0.1)/n/ncol(x_diff)) + kappa * (n + G) * log(n*ncol(x_diff))/(n*ncol(x_diff)) # 加0.1为了保护log
  
  return(list(beta = beta_center, group = g, IC = IC))
}
