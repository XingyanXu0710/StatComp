## ----warning = FALSE----------------------------------------------------------
# 加载必要的包
library(ggplot2)

# 设置随机数种子，确保可重现性
set.seed(42)

# 生成数据
n <- 20  # 每组20个数据

# 第一组数据: y = x + epsilon
x1 <- runif(n, 0, 1)  # 自变量 x 服从 U(0, 1)
epsilon1 <- rnorm(n, mean = 0, sd = 0.4)  # 噪声 epsilon 服从 N(0, 0.4)
y1 <- x1 + epsilon1  # 计算响应变量 y

# 第二组数据: y = 3x + epsilon
x2 <- runif(n, 0, 1)  # 自变量 x 服从 U(0, 1)
epsilon2 <- rnorm(n, mean = 0, sd = 0.4)  # 噪声 epsilon 服从 N(0, 0.4)
y2 <- 5 * x2 + epsilon2  # 计算响应变量 y

# 合并数据
data <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  group = factor(rep(1:2, each = n))  # 分组标签
)

# 做回归分析
model1 <- lm(y ~ x, data = data[data$group == 1, ])
model2 <- lm(y ~ x, data = data[data$group == 2, ])

# 整体回归模型 (使用所有数据)
model_all <- lm(y ~ x, data = data)

# 画回归图
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point() +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE) +  # 绘制每组的回归曲线
  geom_smooth(data = data, aes(x = x, y = y), method = "lm", color = "black", linetype = "dashed", se = FALSE) +  # 整体回归线，黑色虚线
  labs(title = "Regression Lines for Two Groups with Overall Fit",
       x = "x",
       y = "y") +
  scale_color_manual(values = c("blue", "red")) +  # 设置不同组别的颜色
  theme_minimal()


## -----------------------------------------------------------------------------
function(x, y) {
  # Input: Panel scalar data x and its corresponding response variable y.
  # Output: The differenced data x_diff and y_diff along the time axis.

  n_y <- nrow(y)  
  t_y <- ncol(y)  
  
  n_x <- nrow(x)  
  t_x <- ncol(x)  
  
  if (n_y != n_x) {
    stop("The number of rows (n) in x and y must be the same.")
  }
  
  if (t_y != t_x) {
    stop("The number of columns (t) in x and y must be the same.")
  }
  
  if (t_y <= 1) {
    stop("The number of columns (t) in both x and y must be greater than 1.")
  }
  
  y_diff <- data.frame(matrix(NA, nrow = n_y, ncol = t_y - 1)) 
  x_diff <- data.frame(matrix(NA, nrow = n_x, ncol = t_x - 1))  

  for (i in 1:n_y) {
    for (j in 2:t_y) { 
      y_diff[i, j - 1] <- y[i, j] - y[i, j - 1] 
      x_diff[i, j - 1] <- x[i, j] - x[i, j - 1] 
    }
  }
  
  colnames(y_diff) <- paste("Y", 1:(t_y - 1), sep = "")  
  colnames(x_diff) <- paste("X", 1:(t_x - 1), sep = "")  
  
  return(list(y_diff = y_diff, x_diff = x_diff))
}

## ----eval=FALSE---------------------------------------------------------------
#  double count(NumericMatrix x, NumericMatrix y, double beta, int i) {
#    // 初始化平方误差的和
#    double sum = 0.0;
#  
#    // 获取 y 和 x 对应行的差异 (y[i, ] - x[i, ] * beta)
#    for (int j = 0; j < x.ncol(); j++) {
#      sum += pow(y(i, j) - x(i, j) * beta, 2);  // 计算每个元素的差的平方并累加
#    }
#  
#    return sum;
#  }

## -----------------------------------------------------------------------------
library(SA24204168)
x <- data.frame(matrix(1:9, nrow = 3, ncol = 3))
y <- data.frame(matrix(10:18, nrow = 3, ncol = 3))
dif(x, y)

## -----------------------------------------------------------------------------
set.seed(42)
n <- 10; t <- 10
x <- matrix(rnorm(n * t), nrow = n, ncol = t)
alpha <- runif(n, 0, 1)
epsilon <- matrix(rnorm(n * t, mean = 0, sd = sqrt(0.2)), nrow = n, ncol = t)
y <- matrix(NA, nrow = n, ncol = t)
for (i in 1:n) {
  if (i <= n / 2) {
    y[i, ] <- x[i, ] + epsilon[i, ] + alpha[i]
  } else {
      y[i, ] <- 2 * x[i, ] + epsilon[i, ] + alpha[i]
  }
}
gpd(x, y, 2)

## -----------------------------------------------------------------------------
x <- matrix(1:9, nrow = 3, ncol = 3)
y <- matrix(10:18, nrow = 3, ncol = 3)
count(x, y, 1, 1)

