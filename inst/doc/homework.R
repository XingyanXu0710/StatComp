## -----------------------------------------------------------------------------
# 设置参数
set.seed(0)
sigma <- c(1, 2, 3) 
n <- 1000  # 每个σ生成1000个样本
sample1 <- sigma[1] * sqrt(-2 * log(runif(n)))
sample2 <- sigma[2] * sqrt(-2 * log(runif(n)))
sample3 <- sigma[3] * sqrt(-2 * log(runif(n)))

hist(sample1, breaks = 50, main = paste("sigma =", sigma[1]), probability = TRUE)
generated_samples <- density(sample1)$x[which.max(density(sample1)$y)]
cat("众数是",generated_samples,"此时的sigma是",sigma[1])

## ----echo = FALSE-------------------------------------------------------------
hist(sample2, breaks = 50, main = paste("sigma =", sigma[2]), probability = TRUE)
generated_samples <- density(sample2)$x[which.max(density(sample2)$y)]
cat("众数是",generated_samples,"此时的sigma是",sigma[2])

## ----echo = FALSE-------------------------------------------------------------
hist(sample3, breaks = 50, main = paste("sigma =", sigma[3]), probability = TRUE)
generated_samples <- density(sample3)$x[which.max(density(sample3)$y)]
cat("众数是",generated_samples,"此时的sigma是",sigma[3])

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
p <- c(0.5, 0.5)
k <- sample(0:1, size = n, replace = TRUE, prob = p)
x1 <- rnorm(n)
x2 <- rnorm(n, mean = 3, sd = 1)
x <- (1-k)*x1 + k*x2
hist(x, probability = TRUE, main="Histogram of mixture",
ylim = c(0,0.3))
lines(density(x), col = "red")

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
p <- c(0.25, 0.75)
k <- sample(0:1, size = n, replace = TRUE, prob = p)
x1 <- rnorm(n)
x2 <- rnorm(n, mean = 3, sd = 1)
x <- (1-k)*x1 + k*x2
hist(x, probability = TRUE, main="Histogram of mixture",
ylim = c(0,0.3))
lines(density(x), col = "red")

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
p <- c(0.75, 0.25)
k <- sample(0:1, size = n, replace = TRUE, prob = p)
x1 <- rnorm(n)
x2 <- rnorm(n, mean = 3, sd = 1)
x <- (1-k)*x1 + k*x2
hist(x, probability = TRUE, main="Histogram of mixture",
ylim = c(0,0.3))
lines(density(x), col = "red")

## -----------------------------------------------------------------------------
# 设定参数
lambda <- 1  # 泊松过程的强度参数
shape <- 1   
rate <- 1    # Gamma分布的两个参数
t <- 10      # 观察时间

# 定义复合泊松-伽马过程的模拟函数
compound_poisson_gamma <- function(lambda, shape, rate, t) {
  # 泊松过程：确定给定时间t的事件数
  N_t <- rpois(1, lambda * t)
  
  # 如果事件数为0，返回0，否则生成Gamma随机变量
  if (N_t == 0) {
    return(0)
  } else {
    Y <- rgamma(N_t, shape = shape, rate = rate)  # 生成N_t个Gamma随机变量
    return(sum(Y))  # 返回随机和
  }
}

# 模拟过程
n_simulations <- 10000  # 模拟次数
X_10 <- replicate(n_simulations, compound_poisson_gamma(lambda, shape, rate, t))

# 计算样本均值和方差
mean_X_10 <- mean(X_10)
var_X_10 <- var(X_10)

# 理论均值和方差
theoretical_mean <- lambda * t * (shape / rate)
theoretical_variance <- lambda * t * ((shape / rate^2) + (shape/rate)^2)

# 输出结果
cat("Sample mean of X(10):", mean_X_10, "\n")
cat("Sample variance of X(10):", var_X_10, "\n")
cat("Theoretical mean of X(10):", theoretical_mean, "\n")
cat("Theoretical variance of X(10):", theoretical_variance, "\n")


## -----------------------------------------------------------------------------
# 设定参数
lambda <- 1  # 泊松过程的强度参数
shape <- 2   
rate <- 2    # Gamma分布的两个参数
t <- 10      # 观察时间

# 定义复合泊松-伽马过程的模拟函数
compound_poisson_gamma <- function(lambda, shape, rate, t) {
  # 泊松过程：确定给定时间t的事件数
  N_t <- rpois(1, lambda * t)
  
  # 如果事件数为0，返回0，否则生成Gamma随机变量
  if (N_t == 0) {
    return(0)
  } else {
    Y <- rgamma(N_t, shape = shape, rate = rate)  # 生成N_t个Gamma随机变量
    return(sum(Y))  # 返回随机和
  }
}

# 模拟过程
n_simulations <- 10000  # 模拟次数
X_10 <- replicate(n_simulations, compound_poisson_gamma(lambda, shape, rate, t))

# 计算样本均值和方差
mean_X_10 <- mean(X_10)
var_X_10 <- var(X_10)

# 理论均值和方差
theoretical_mean <- lambda * t * (shape / rate)
theoretical_variance <- lambda * t * ((shape / rate^2) + (shape/rate)^2)

# 输出结果
cat("Sample mean of X(10):", mean_X_10, "\n")
cat("Sample variance of X(10):", var_X_10, "\n")
cat("Theoretical mean of X(10):", theoretical_mean, "\n")
cat("Theoretical variance of X(10):", theoretical_variance, "\n")


## -----------------------------------------------------------------------------
# 设定参数
lambda <- 2  # 泊松过程的强度参数
shape <- 2   
rate <- 2    # Gamma分布的两个参数
t <- 10      # 观察时间

# 定义复合泊松-伽马过程的模拟函数
compound_poisson_gamma <- function(lambda, shape, rate, t) {
  # 泊松过程：确定给定时间t的事件数
  N_t <- rpois(1, lambda * t)
  
  # 如果事件数为0，返回0，否则生成Gamma随机变量
  if (N_t == 0) {
    return(0)
  } else {
    Y <- rgamma(N_t, shape = shape, rate = rate)  # 生成N_t个Gamma随机变量
    return(sum(Y))  # 返回随机和
  }
}

# 模拟过程
n_simulations <- 10000  # 模拟次数
X_10 <- replicate(n_simulations, compound_poisson_gamma(lambda, shape, rate, t))

# 计算样本均值和方差
mean_X_10 <- mean(X_10)
var_X_10 <- var(X_10)

# 理论均值和方差
theoretical_mean <- lambda * t * (shape / rate)
theoretical_variance <- lambda * t * ((shape / rate^2) + (shape/rate)^2)

# 输出结果
cat("Sample mean of X(10):", mean_X_10, "\n")
cat("Sample variance of X(10):", var_X_10, "\n")
cat("Theoretical mean of X(10):", theoretical_mean, "\n")
cat("Theoretical variance of X(10):", theoretical_variance, "\n")


## -----------------------------------------------------------------------------
set.seed(0)
# 使用接受拒绝法生成 n 个服从 Beta(3,3) 分布的样本
accept_reject_beta <- function(n) {
  # 初始化接受的样本集合
  accepted_samples <- numeric(n)
  
  # Beta(3,3) 的目标密度函数（未归一化）
  target_density <- function(x) {
    return(x^2 * (1 - x)^2)
  }
  
  # 设定常数 c (选择较大的 c 可以提高接受概率)
  c <- 1.5  # 因为 Beta(3,3) 的最大密度值约为 1.5
  
  # 计数已接受的样本数
  count <- 0
  
  # 开始接受拒绝过程
  while (count < n) {
    # 从均匀分布 U(0,1) 中生成候选样本
    x_candidate <- runif(1)
    
    # 生成一个 U(0, 1) 的随机数作为接受标准
    u <- runif(1)
    
    # 根据接受概率决定是否接受候选样本
    if (u <= target_density(x_candidate) / c) {
      # 接受该候选样本
      count <- count + 1
      accepted_samples[count] <- x_candidate
    }
  }
  
  return(accepted_samples)
}

# 定义蒙特卡洛估计函数，使用接受拒绝法生成 Beta(3,3) 样本
monte_carlo_beta_cdf <- function(x, n = 100000) {
  # 使用接受拒绝法生成 n 个 Beta(3, 3) 分布的随机样本
  samples <- accept_reject_beta(n)
  # 计算 F(x) = P(X <= x)，使用向量化比较
  cdf_estimates <- colMeans(outer(samples, x, "<="))
  return(cdf_estimates)
}

# 给定的 x 值
x_values <- seq(0.1, 0.9, by = 0.1)

# 使用蒙特卡洛方法计算 CDF
monte_carlo_estimates <- monte_carlo_beta_cdf(x_values)

# 使用 R 自带的 pbeta 函数计算真实的 CDF
pbeta_estimates <- pbeta(x_values, 3, 3)

# 打印结果并比较
results <- data.frame(x = x_values, 
                      MonteCarlo = monte_carlo_estimates, 
                      pbeta = pbeta_estimates)

print(results)

## -----------------------------------------------------------------------------
set.seed(0)

# 设置Rayleigh分布的sigma参数
sigma <- 1
# 设置样本数量
n <- 1000

# 生成n个独立的Rayleigh分布样本
U1 <- runif(n)  # 生成均匀分布的随机变量
U2 <- runif(n)  # 生成另一个独立的均匀分布随机变量
X1 <- sigma * sqrt(-2 * log(U1))  # 生成第一个Rayleigh样本
X2 <- sigma * sqrt(-2 * log(U2))  # 生成第二个独立Rayleigh样本


# 使用对偶变量生成n个Rayleigh分布样本
U <- runif(n)  # 生成均匀分布的随机变量
U_prime <- 1 - U  # 对偶变量
X <- sigma * sqrt(-2 * log(U))  # 生成Rayleigh样本
X_prime <- sigma * sqrt(-2 * log(U_prime))  # 生成对偶的Rayleigh样本

# 生成独立变量样本和对偶变量样本
independent_samples <- (X1 + X2) / 2
antithetic_samples <- (X + X_prime) / 2
  
# 计算独立变量的方差和对偶变量的方差
var_independent <- var(independent_samples)
var_antithetic <- var(antithetic_samples)
  
# 计算方差减少的百分比
reduction_percent <- 100 * (var_independent - var_antithetic) / var_independent

cat("方差减少的百分比:", reduction_percent, "%\n")


## -----------------------------------------------------------------------------
set.seed(0)

c1 <- 1.6487
c2 <- 2.7183

# 从重要性函数 f1 和 f2中抽样
samples1 <- function(n) {
  u <- runif(n, 0, 1)
  return(sqrt(-2*log(exp(-0.5)-u/c1)))
}

samples2 <- function(n) {
  u <- runif(n, 0, 1)
  return(1-log(1-u))
}

# 生成样本
n <- 10000  # 样本数量

# 从 f1 生成样本
samples_f1 <- samples1(n) 

# 从 f2 生成样本
samples_f2 <- samples2(n) 

# 计算重要性权重
weights_f1 <- samples_f1*c1/sqrt(2*pi)  # 计算 f1 的权重
weights_f2 <- samples_f2^2/sqrt(2*pi)*exp(-samples_f2^2/2)/exp(-samples_f2)  # 计算 f2 的权重

# 计算期望估计
estimate_f1 <- mean(weights_f1)
estimate_f2 <- mean(weights_f2)

# 计算方差
variance_f1 <- var(weights_f1)
variance_f2 <- var(weights_f2)

# 输出结果
cat("估计值 f1:", estimate_f1, "\n")
cat("估计值 f2:", estimate_f2, "\n")
cat("方差 f1:", variance_f1, "\n")
cat("方差 f2:", variance_f2, "\n")

## -----------------------------------------------------------------------------
# 快速排序函数
quick_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  } else {
    pivot <- arr[1]  # 基准
    left <- arr[arr < pivot]  # 左边部分
    right <- arr[arr > pivot]  # 右边部分
    middle <- arr[arr == pivot]  # 相等部分
    return(c(quick_sort(left), middle, quick_sort(right)))
  }
}

# 进行 Monte Carlo 实验
set.seed(123)  # 为了结果可重复

# 定义 n 的取值
n_values <- c(10^4, 2*10^4, 4*10^4, 6*10^4, 8*10^4)

# 保存平均时间的向量
avg_times <- c()

# 对每个 n 进行 100 次排序
for (n in n_values) {
  times <- numeric(100)  # 保存100次排序时间
  for (i in 1:100) {
    random_numbers <- sample(1:n)  # 生成随机排列的数字
    start_time <- Sys.time()  # 开始时间
    sorted_numbers <- quick_sort(random_numbers)  # 快速排序
    end_time <- Sys.time()  # 结束时间
    times[i] <- as.numeric(end_time - start_time, units = "secs")  # 计算排序耗时
  }
  avg_times <- c(avg_times, mean(times))  # 计算平均时间
}

# 将 n 和 log(n) 结合起来
log_n <- n_values * log(n_values)

# 进行回归分析
regression <- lm(avg_times ~ log_n)

# 可视化回归结果
plot(log_n, avg_times, main = "Regression of Sorting Time on n log(n)",
     xlab = "n log(n)", ylab = "Average Sorting Time (seconds)",
     pch = 19, col = "blue")
abline(regression, col = "red")


## -----------------------------------------------------------------------------
# 设置随机种子以确保结果可重复
set.seed(0)

# 蒙特卡洛实验参数
n <- 1000  # 每次实验的样本大小
num_simulations <- 10000  # 总实验次数

# 存储偏度的平方根
skewness_sqrt_b1 <- numeric(num_simulations)

# 进行蒙特卡洛实验
for (i in 1:num_simulations) {
  # 生成 n 个正态分布的随机样本
  sample <- rnorm(n)
  
  # 计算样本偏度
  skewness_b1 <- sum((sample - mean(sample))^3) / ((n - 1) * sd(sample)^3)
  
  # 计算偏度的平方根并存储
  skewness_sqrt_b1[i] <- skewness_b1
}

# 计算所需的分位数
quantiles <- quantile(skewness_sqrt_b1, probs = c(0.025, 0.05, 0.95, 0.975))
print(quantiles)

# 计算标准误差
q_probs <- c(0.025, 0.05, 0.95, 0.975)
standard_errors <- sqrt(q_probs * (1 - q_probs) / (n * dnorm(qnorm(q_probs))^2))
print(standard_errors)

# 估计的标准差与正态分布近似的分位数
estimated_quantiles <- qnorm(q_probs, mean = 0, sd = sqrt(6/n))
print(estimated_quantiles)

# 比较结果
comparison <- data.frame(Quantiles = c(0.025, 0.05, 0.95, 0.975),
                         Monte_Carlo = quantiles,
                         Standard_Error = standard_errors,
                         Normal_Approximation = estimated_quantiles)
print(comparison)


## -----------------------------------------------------------------------------
# 设置随机种子以便重现
set.seed(2)

# 设定参数
n <- 100  # 样本大小
num_simulations <- 1000  # 模拟次数

# 正态分布数据
pearson_results <- numeric(num_simulations)
spearman_results <- numeric(num_simulations)

for (i in 1:num_simulations) {
  # 生成双变量正态分布数据
  x <- runif(n, 0, 1)
  y <- x/2 + rt(n, df = 1)  # 让x和y相关，且相关性不高
  # 皮尔逊检验
  pearson_test <- cor.test(x, y)
  pearson_results[i] <- ifelse(pearson_test$p.value < 0.05, 1, 0)

  # 斯皮尔曼检验
  spearman_test <- cor.test(x, y, method = "spearman")
  spearman_results[i] <- ifelse(spearman_test$p.value < 0.05, 1, 0)
}

# 计算功效
pearson_power <- mean(pearson_results)
spearman_power <- mean(spearman_results)

cat("Bivariate Normal Distribution:\n")
cat("Pearson Power:", pearson_power, "\n")
cat("Spearman Power:", spearman_power, "\n")

## -----------------------------------------------------------------------------
# 设置参数
set.seed(123)
N <- 1000           # 总假设数量
m <- 10000          # 模拟重复次数
alpha <- 0.1        # 显著性水平

# 初始化结果
results <- matrix(0, nrow = 3, ncol = 2)
colnames(results) <- c("Bonferroni correction", "B-H correction")
rownames(results) <- c("FWER", "FDR", "TPR")

# 模拟过程
for (i in 1:m) {
  # 生成 p-values
  p_values <- c(runif(950), rbeta(50, 0.1, 1)) 
  
  # 对p值进行两种调整
  B_H <- p.adjust(p_values, method = "BH")
  Bonferroni <- p.adjust(p_values, method = "bonferroni")

  # 计算TP、FP等指标
  reject_Bonferroni <- Bonferroni < alpha
  reject_B_H <- B_H < alpha
  true <- c(rep(FALSE, 950), rep(TRUE, 50))   # 标记真实备择假设

  
  # 计算FWER
  FP_Bonferroni <- sum(reject_Bonferroni & !true)     # 错误拒绝次数
  if((FP_Bonferroni > 0)){
    results["FWER", "Bonferroni correction"] <- results["FWER", "Bonferroni correction"] + 1/m
  }
  FP_B_H <- sum(reject_B_H & !true)
  if(FP_B_H > 0){
    results["FWER", "B-H correction"] <- results["FWER", "B-H correction"] + 1/m
  }
  
  # 计算FDR
  results["FDR", "Bonferroni correction"] <- results["FDR", "Bonferroni correction"] + (FP_Bonferroni / max(1, sum(reject_Bonferroni)))/m
  results["FDR", "B-H correction"] <- results["FDR", "B-H correction"] + (FP_B_H / max(1, sum(reject_B_H)))/m
  
  # 计算TPR
  TP_Bonferroni <- sum(reject_Bonferroni & true)      # Bonferroni正确拒绝次数
  TP_B_H <- sum(reject_B_H & true)              # B_H正确拒绝次数
  results["TPR", "Bonferroni correction"] <- results["TPR", "Bonferroni correction"] + (TP_Bonferroni / 50)/m
  results["TPR", "B-H correction"] <- results["TPR", "B-H correction"] + (TP_B_H / 50)/m
}

print(results)

## -----------------------------------------------------------------------------
# 加载boot包
library(boot)

set.seed(123)
# 数据集
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 计算最大似然估计MLE
lambda_mle <- length(data) / sum(data)
lambda_mle

# 定义一个函数来计算MLE
lambda_mle_func <- function(data, index) {
  sample_data <- data[index]
  return(length(sample_data) / sum(sample_data))
}

# 使用boot函数进行bootstrap抽样，计算偏差和标准差
result <- boot(data, lambda_mle_func, 1000)

# 查看bootstrap结果
result

## -----------------------------------------------------------------------------
# 加载必要的库
library(boot)
set.seed(123)  # 确保结果可重复
data <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 定义一个函数，用于计算平均故障时间 1/λ
mean_time <- function(data, index) {
  return(mean(data[index]))  # 返回 1/λ 即平均时间
}

# 使用 bootstrapping 方法进行估计
bootstrap_results <- boot(data, mean_time, 10000)

# 计算不同方法的95%置信区间
ci_normal <- boot.ci(bootstrap_results, type = "norm")
ci_basic <- boot.ci(bootstrap_results, type = "basic")
ci_percentile <- boot.ci(bootstrap_results, type = "perc")
ci_bca <- boot.ci(bootstrap_results, type = "bca")

# 输出结果
cat("Normal Method CI: ", ci_normal$normal[2], ci_normal$normal[3], "\n")
cat("Basic Method CI: ", ci_basic$basic[4], ci_basic$basic[5], "\n")
cat("Percentile Method CI: ", ci_percentile$percent[4], ci_percentile$percent[5], "\n")
cat("BCa Method CI: ", ci_bca$bca[4], ci_bca$bca[5], "\n")

## -----------------------------------------------------------------------------
library(bootstrap)

## -----------------------------------------------------------------------------
# 数据加载
data <- scor

# 样本协方差占比如下
eigenvalues <- eigen(cov(data))$values
theta_hat <- eigenvalues[1] / sum(eigenvalues)

# Jackknife部分
n <- nrow(data)
theta <- numeric(n)

for (i in 1:n) {
  eigvals_jack <- eigen(cov(data[-i, ]))$values  # 去掉第i个样本的协方差矩阵的特征值
  theta[i] <- eigvals_jack[1] / sum(eigvals_jack)
}

# Jackknife估计的偏差与标准差
cat("Jackknife估计的偏差与标准差分别为: ", (n - 1) * (mean(theta) - theta_hat), "\t", sqrt((n - 1) * mean((theta - mean(theta))^2)))

## -----------------------------------------------------------------------------
library(DAAG)

## -----------------------------------------------------------------------------
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)
# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  
  J4 <- lm(y ~ x + I(x^2) + I(x^3))
  yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] + J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3
  e4[k] <- magnetic[k] - yhat4
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## -----------------------------------------------------------------------------
Y <- magnetic
X <- chemical

# 比较调整后的 R^2
cat(summary(lm(Y ~ X))$adj.r.squared, summary(lm(Y ~ X + I(X^2)))$adj.r.squared, summary(lm(log(Y) ~ X))$adj.r.squared, summary(lm(Y ~ X + I(X^2) + I(X^3)))$adj.r.squared)
detach(ironslag)

## -----------------------------------------------------------------------------
# 课本上的数据
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

# 定义 Cramér-von Mises 检验的统计量计算函数
cvm_stat <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  combined <- sort(c(x, y))
  Fx <- ecdf(x)(combined)
  Fy <- ecdf(y)(combined)
  Tn <- (nx * ny / (nx + ny)^2) * sum((Fx - Fy)^2)
  return(Tn)
}

# 原始样本的统计量
T_obs <- cvm_stat(x, y)

# 置换检验
set.seed(123) # 为了保证结果可重复
B <- 1000 # 置换次数
T_perm <- numeric(B)

for (i in 1:B) {
  combined <- sample(c(x, y))
  x_perm <- combined[1:length(x)]
  y_perm <- combined[(length(x) + 1):length(combined)]
  T_perm[i] <- cvm_stat(x_perm, y_perm)
}

# 计算 p 值
p_value <- mean(c(T_obs, T_perm) >= T_obs)
cat("Cramér-von Mises 置换检验的 p 值:", p_value, "\n")

hist(T_perm, main = "", freq = FALSE, breaks = "scott")
points(T_obs, 0, cex = 1, pch = 16) #observed T

## -----------------------------------------------------------------------------
# 生成两个样本数据
set.seed(123)
x <- rnorm(30)
y <- rnorm(30)

# 计算原始样本的Spearman秩相关系数
spearman_stat <- cor(x, y, method = "spearman")

# 使用 cor.test 来获得 p 值
cor_test_result <- cor.test(x, y, method = "spearman")

# 置换检验
R <- 1000 # 置换次数
perm_stats <- numeric(R)

for (i in 1:R) {
  perm_stats[i] <- cor(x, sample(y), method = "spearman") # 随机置换y并计算Spearman相关系数
}

# 计算置换检验的 p 值
p_value_perm <- mean(abs(c(perm_stats, spearman_stat)) >= abs(spearman_stat))
cat("cor.test 的 p 值:", cor_test_result$p.value,"置换检验的 p 值:", p_value_perm, "\n")

## -----------------------------------------------------------------------------
set.seed(123) 
n <- 10000  # 总采样数
x <- numeric(n)  # 初始化样本存储向量
x[1] <- 0  # 初始化

for (i in 2:n) {
  y <- x[i - 1] + rnorm(1, mean = 0, sd = 1)  # 使用正态分布作为提议分布
  if (runif(1) < dcauchy(y) / dcauchy(x[i - 1])) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1] 
  }
}

samples <- x[(1000 + 1):n] # 去掉前1000个值

# 比较样本和理论柯西分布的分位数
cat("分位数为：",seq(0.1,0.9,0.1),"\n")
cat("样本分位数：",quantile(samples, probs = seq(0.1, 0.9, by = 0.1)),"\n")
cat("理论分位数：",qcauchy(seq(0.1, 0.9, by = 0.1)))

## -----------------------------------------------------------------------------
set.seed(123)      
n <- 10            # 总样本数
a <- 1             
b <- 2             # Beta分布的参数a和b

# 初始化链
x <- numeric(1000)  # 迭代1000次
y <- numeric(1000)
x[1] <- rbinom(1, n, 0.5)  # 初始化x的值
y[1] <- rbeta(1, x[1] + a, n - x[1] + b)  # 初始化y的值

# Gibbs采样
for (i in 2:1000) {
  # 更新条件分布
  x[i] <- rbinom(1, size = n, prob = y[i - 1])
  y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
}


x <- x[(100 + 1):1000]
y <- y[(100 + 1):1000]  # 去掉前100个数避免初始值影响

# 可视化生成样本的分布情况
plot(x, y, main = "Gibbs", pch = 16, col ="blue")


## -----------------------------------------------------------------------------
set.seed(123) 
n <- 10000  # 总采样数
x <- numeric(n)  # 初始化样本存储向量
x[1] <- 0  # 初始化

for (i in 2:n) {
  y <- x[i - 1] + rnorm(1, mean = 0, sd = 1)  # 使用正态分布作为提议分布
  if (runif(1) < dcauchy(y) / dcauchy(x[i - 1])) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1] 
  }
}
c1 <- x[(1000 + 1):n] # 第一组



x <- numeric(n)  # 初始化样本存储向量
x[1] <- 0  # 初始化

for (i in 2:n) {
  y <- x[i - 1] + rnorm(1, mean = 0, sd = 1)  # 使用正态分布作为提议分布
  if (runif(1) < dcauchy(y) / dcauchy(x[i - 1])) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1] 
  }
}
c2 <- x[(1000 + 1):n] # 第二组



x <- numeric(n)  # 初始化样本存储向量
x[1] <- 0  # 初始化

for (i in 2:n) {
  y <- x[i - 1] + rnorm(1, mean = 0, sd = 1)  # 使用正态分布作为提议分布
  if (runif(1) < dcauchy(y) / dcauchy(x[i - 1])) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1] 
  }
}
c3 <- x[(1000 + 1):n] # 第三组

B <- (n-1000)/2*var(c(mean(c1), mean(c2), mean(c3)))
W <- 1/3*sum(var(c1), var(c2), var(c3))
R_hat <- (8999/9000*W+1/9000*B)/W
R_hat

## -----------------------------------------------------------------------------
set.seed(123)      
n <- 10            # 总样本数
a <- 1             
b <- 2             # Beta分布的参数a和b

# 初始化链
x <- numeric(1000)  # 迭代1000次
y <- numeric(1000)
x[1] <- rbinom(1, n, 0.5)  # 初始化x的值
y[1] <- rbeta(1, x[1] + a, n - x[1] + b)  # 初始化y的值

# Gibbs采样
for (i in 2:1000) {
  # 更新条件分布
  x[i] <- rbinom(1, size = n, prob = y[i - 1])
  y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
}

x1 <- x[(100 + 1):1000]
y1 <- y[(100 + 1):1000]  # 去掉前100个数避免初始值影响



x <- numeric(1000)  # 迭代1000次
y <- numeric(1000)
x[1] <- rbinom(1, n, 0.5)  # 初始化x的值
y[1] <- rbeta(1, x[1] + a, n - x[1] + b)  # 初始化y的值

# Gibbs采样
for (i in 2:1000) {
  # 更新条件分布
  x[i] <- rbinom(1, size = n, prob = y[i - 1])
  y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
}

x2 <- x[(100 + 1):1000]
y2 <- y[(100 + 1):1000]  # 去掉前100个数避免初始值影响



x <- numeric(1000)  # 迭代1000次
y <- numeric(1000)
x[1] <- rbinom(1, n, 0.5)  # 初始化x的值
y[1] <- rbeta(1, x[1] + a, n - x[1] + b)  # 初始化y的值

# Gibbs采样
for (i in 2:1000) {
  # 更新条件分布
  x[i] <- rbinom(1, size = n, prob = y[i - 1])
  y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
}

x3 <- x[(100 + 1):1000]
y3 <- y[(100 + 1):1000]  # 去掉前100个数避免初始值影响

B <- 900/2*(var(c(mean(x1), mean(x2), mean(x3))) + var(c(mean(y1), mean(y2), mean(y3))))
W <- 1/3*(sum(var(x1), var(x2), var(x3)) + sum(var(y1), var(y2), var(y3)))
R_hat <- (899/900*W+1/900*B)/W
R_hat

## -----------------------------------------------------------------------------
library(gsl) 
# 计算第 k 项函数
kth_term <- function(k, d, a) {
  term <- (-1)^k / (factorial(k) * 2^k) * sqrt(sum(a^2))^(2 * k + 2) / ((2 * k + 1) * (2 * k + 2)) * exp(lgamma((d + 1) / 2) + lgamma(k + 3 / 2) - lgamma(k + d / 2 + 1))
  return(term)
}

# 对较大的k和d测试
d <- 400
a <- c(1, 2) 
k <- 100
kth_term(k, d, a)

## -----------------------------------------------------------------------------
sum_series <- function(d, a, tol = 1e-10) {
  sum <- 0
  k <- 0
  repeat {
    term <- kth_term(k, d, a)
    sum <- sum + term
    if (abs(term) < tol) break 
    k <- k + 1
  }
  return(sum)
}

## -----------------------------------------------------------------------------
a <- c(1, 2)
d <- 100
result <- sum_series(d, a)
result

## -----------------------------------------------------------------------------
# 定义左侧的积分函数
left_integral <- function(k, c_k_minus_1) {
  integrand <- function(u) {
    (1 + u^2 / (k - 1))^(-k / 2)
  }
  integrate(integrand, lower = 0, upper = c_k_minus_1)$value
}

# 定义右侧的积分函数
right_integral <- function(k, c_k) {
  integrand <- function(u) {
    (1 + u^2 / k)^(-(k + 1) / 2)
  }
  integrate(integrand, lower = 0, upper = c_k)$value
}

# 定义 c_k 公式
c_k <- function(a, k) {
  sqrt((a^2 * k) / (k + 1 - a^2))
}

target_function <- function(a, k) {
  
  c_k_minus_1 <- c_k(a, k - 1)
  c_k_value <- c_k(a, k)
  
  # 计算左右两边的值
  left_value <- (2 * gamma(k / 2)) / (sqrt(pi * (k - 1)) * gamma((k - 1) / 2)) * left_integral(k, c_k_minus_1)
  right_value <- (2 * gamma((k + 1) / 2)) / (sqrt(pi * k) * gamma(k / 2)) * right_integral(k, c_k_value)
  
  # 返回左右差值
  left_value - right_value
}

# 这里区间取成0,1到sqrt(k)-0.05是为了保护分母
solve_a <- function(k, interval = c(0.1, sqrt(k)-0.05)) {
  uniroot(function(a) target_function(a, k), interval = interval)$root
}

# 在k=5时计算a的值
k_value <- 5
a_solution <- solve_a(k_value)
a_solution


## -----------------------------------------------------------------------------
# 初始化数据
observed_Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1
n <- length(observed_Y)

# 初始猜测的 lambda 值
lambda <- 1
tol <- 1e-6
max_iter <- 1000
log_likelihood_diff <- Inf
iteration <- 0

while (log_likelihood_diff > tol && iteration < max_iter) {
  iteration <- iteration + 1
  
  # E-step: 计算被截尾的 Y_i = 1 的缺失部分
  expected_T <- observed_Y
  censored_indices <- which(observed_Y == tau)
  
  if (length(censored_indices) > 0) {
    expected_T[censored_indices] <- tau + lambda
  }
  
  # M-step: 更新 lambda
  new_lambda <- mean(expected_T)
  
  # 检查收敛
  log_likelihood_diff <- abs(new_lambda - lambda)
  lambda <- new_lambda
}

# 输出结果
cat("使用EM算法估计得到的 lambda 值为:", 1/lambda, "\n")

# 使用直接观测数据的 MLE 估计
observed_mle_lambda <- mean(observed_Y)
cat("观测数据的 MLE lambda 值为:", 1/observed_mle_lambda, "\n")

## ----warning=FALSE------------------------------------------------------------
library(lpSolve)
# 定义目标函数的系数
objective <- c(4, 2, 9)  # 对应 4x + 2y + 9z

# 定义约束矩阵
constraints <- matrix(c(
  2, 1, 1,  # 2x + y + z
  1, -1, 3  # x - y + 3z
), nrow = 2, byrow = TRUE)

# 定义约束的右侧值
rhs <- c(2, 3)

# 定义约束的方向
direction <- c("<=", "<=")

# 求解线性规划问题 (最小化问题)
result <- lp("min", objective, constraints, direction, rhs, all.int = FALSE)

# 显示结果
cat("Optimal value of the objective function:", result$objval, "\n")
cat("Optimal values of x, y, z:", result$solution, "\n")

## -----------------------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 创建一个空列表存储模型结果
models_for <- list()

# 使用 for 循环拟合模型
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}
models_for

## -----------------------------------------------------------------------------
# 使用 lapply 拟合模型
models_lapply <- lapply(formulas, function(f) lm(f, data = mtcars))
models_lapply

## -----------------------------------------------------------------------------
set.seed(123)  
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

# 创建一个空列表存储模型结果
models_for <- list()

# 使用 for 循环拟合模型
for (i in seq_along(bootstraps)) {
  models_for[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 打印第一个模型的摘要
summary(models_for[[1]])


# 定义一个用于拟合模型的函数
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 使用 lapply 调用预定义的函数
models_lapply_no_anon <- lapply(bootstraps, fit_model)

# 打印第一个模型的摘要
summary(models_lapply_no_anon[[1]])

## -----------------------------------------------------------------------------
# 定义提取 R^2 的函数
rsq <- function(mod) summary(mod)$r.squared

# 使用 for 循环结果提取 R^2
r_squared_for <- sapply(models_for, rsq)

# 查看结果
r_squared_for

## -----------------------------------------------------------------------------
# 使用 lapply 结果提取 R^2
r_squared_lapply <- sapply(models_lapply, rsq)

# 查看结果
r_squared_lapply

## -----------------------------------------------------------------------------
# 使用无匿名函数的 lapply 结果提取 R^2
r_squared_lapply_no_anon <- sapply(models_lapply_no_anon, rsq)

# 查看结果
r_squared_lapply_no_anon

## -----------------------------------------------------------------------------
# 模拟 100 次 t 检验
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
# 提取每次试验的 p 值
p_values <- sapply(trials, function(trial) trial$p.value)

# 查看前几个 p 值
head(p_values)

## -----------------------------------------------------------------------------
# 自定义函数，结合 Map() 和 vapply()
parallel_apply <- function(FUN, ..., FUN.VALUE) {
  # 使用 Map 并行应用函数到所有输入
  results <- Map(FUN, ...)
  
  # 使用 vapply 确保输出为指定类型的向量或矩阵
  vapply(results, identity, FUN.VALUE)
}

# 示例 1：两个输入的并行相加
# 定义输入列表
input1 <- list(1, 2, 3)
input2 <- list(4, 5, 6)

# 定义需要应用的函数
my_function <- function(a, b) a + b

# 调用自定义函数
result <- parallel_apply(my_function, input1, input2, FUN.VALUE = numeric(1))
print(result)  # 输出: [1] 5 7 9

## -----------------------------------------------------------------------------
# 定义输入列表
input1 <- list(1, 2, 3)
input2 <- list(4, 5, 6)

# 定义需要应用的函数，返回一个长度为2的向量
my_function <- function(a, b) c(a, b)

# 调用自定义函数
result <- parallel_apply(my_function, input1, input2, FUN.VALUE = numeric(2))
print(result)
# 输出：一个 2 行矩阵
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

## -----------------------------------------------------------------------------
# 自定义快速卡方检验函数
fast_chisq_test <- function(x, y) {
  # 检查输入是否为数值向量，且无缺失值
  if (!is.numeric(x) || !is.numeric(y)) stop("Inputs must be numeric vectors.")
  if (anyNA(x) || anyNA(y)) stop("Inputs must not contain missing values.")
  
  # 构建二维列联表
  contingency_table <- table(x, y)
  
  # 计算行和、列和以及总和
  row_sums <- rowSums(contingency_table)
  col_sums <- colSums(contingency_table)
  total <- sum(contingency_table)
  
  # 计算期望值
  expected <- outer(row_sums, col_sums) / total
  
  # 计算卡方统计量
  observed <- as.numeric(contingency_table)
  expected <- as.numeric(expected)
  chisq_statistic <- sum((observed - expected)^2 / expected)
  
  # 返回卡方统计量
  return(chisq_statistic)
}

# 示例：测试代码
x <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
y <- c(1, 1, 2, 2, 1, 1, 2, 2, 1, 2)
fast_chisq_test(x, y)


## -----------------------------------------------------------------------------
# 比较运行速度
set.seed(123)
x <- sample(1:3, 1e7, replace = TRUE)
y <- sample(1:3, 1e7, replace = TRUE)

# 原始 chisq.test
system.time({
  chisq_result <- chisq.test(x, y)$statistic
})

# 快速实现 fast_chisq_test
system.time({
  fast_result <- fast_chisq_test(x, y)
})

# 验证两者结果一致性
# 忽略名字属性
all.equal(unname(chisq_result), fast_result)  # TRUE

## -----------------------------------------------------------------------------
fast_table <- function(x, y) {
  # 假设 x 和 y 都是整数向量，并且没有缺失值
  n <- max(length(x), length(y))
  
  # 直接计算二维频数表
  res <- matrix(0, nrow = max(x), ncol = max(y))
  
  for(i in seq_len(n)) {
    res[x[i], y[i]] <- res[x[i], y[i]] + 1
  }
  
  return(res)
}

fast_chisq_test <- function(x, y) {
  # 计算快速频次表
  freq_table <- fast_table(x, y)
  
  # 计算卡方统计量
  expected <- outer(rowSums(freq_table), colSums(freq_table), "*") / sum(freq_table)
  chisq_stat <- sum((freq_table - expected)^2 / expected)
  
  return(chisq_stat)
}

# 验证结果的一致性
chisq_result <- chisq.test(x, y)$statistic
fast_result <- fast_chisq_test(x, y)
all.equal(unname(chisq_result), fast_result) 

## -----------------------------------------------------------------------------
library(Rcpp)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  # 使用Rcpp实现Gibbs采样器
#  dir_cpp <- 'E:/Rstudio/R_project/Rcpp/'
#  sourceCpp(paste0(dir_cpp,"gibbs_sampler.cpp"))
#  
#  # 参数设置
#  set.seed(123)
#  a <- 2; b <- 2
#  n <- 10
#  num_iter <- 10000
#  gibbs_chain <- gibbs_sampler(num_iter, a, b, n, 0.5)
#  gibbs_df <- data.frame(iter = 1:num_iter, x = gibbs_chain[, 1], y = gibbs_chain[, 2])
#  
#  ggplot(gibbs_df, aes(x = x)) +
#    geom_histogram(color = "black", fill = "blue", bins = 20, alpha = 0.7) +
#    labs(title = "样本直方图：x", x = "x", y = "频率")
#  
#  ggplot(gibbs_df, aes(x = y)) +
#    geom_histogram(color = "black", fill = "red", bins = 20, alpha = 0.7) +
#    labs(title = "样本直方图：y", x = "y", y = "频率")

## ----eval=FALSE---------------------------------------------------------------
#  source(paste0(dir_cpp,'GibbsR.R'))
#  # 参数设置
#  set.seed(123)
#  num_iter <- 10000
#  a <- 2; b <- 2; n <- 10; y_init <- 0.5
#  
#  # 运行R版本的Gibbs采样器
#  gibbs_chain_r <- gibbs_sampler_r(num_iter, a, b, n, y_init)

## ----eval=FALSE---------------------------------------------------------------
#  gibbs_chain_cpp <- gibbs_sampler(num_iter, a, b, n, y_init)
#  gibbs_chain_cpp <- data.frame(x = gibbs_chain_cpp[, 1], y = gibbs_chain_cpp[, 2])
#  
#  # 比较x的随机数
#  qqplot(
#    gibbs_chain_r$x, gibbs_chain_cpp$x,
#    main = "x Q-Q Plot",
#    xlab = "R",
#    ylab = "Rcpp",
#    pch = 20, col = "blue"
#  )
#  abline(0, 1, col = "red", lwd = 2)
#  
#  # 比较y的随机数
#  qqplot(
#    gibbs_chain_r$y, gibbs_chain_cpp$y,
#    main = "y Q-Q Plot",
#    xlab = "R",
#    ylab = "Rcpp",
#    pch = 20, col = "blue"
#  )
#  abline(0, 1, col = "red", lwd = 2)

## ----eval=FALSE---------------------------------------------------------------
#  # 加载 microbenchmark 包
#  library(microbenchmark)
#  
#  # 设置参数
#  num_iter <- 10000
#  a <- 2; b <- 2; n <- 10; y_init <- 0.5
#  
#  # 测试性能
#  benchmark_results <- microbenchmark(
#    R_version = gibbs_sampler_r(num_iter, a, b, n, y_init),
#    Rcpp_version = gibbs_sampler(num_iter, a, b, n, y_init),
#    times = 10 # 每个函数运行10次
#  )
#  
#  # 显示结果
#  print(summary(benchmark_results)[, c("expr", "min", "median", "mean", "max")])
#  
#  # 可视化性能比较
#  boxplot(benchmark_results, main = "Performance Comparison", ylab = "Execution Time (ns)")

