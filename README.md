# Hotelling-Test
测试单变量的假设可以用t-test,多变量就用到T^2 test了，也就是Hotelling test

# 生成男性身高体重500个数据

```{r}
set.seed(123)
library(mvtnorm)
library(MASS)
# 指定covariance 矩阵
man_sigma = matrix(c(100,25,25,36),nrow = 2,byrow = T)
man = mvrnorm(500,c(175,72),man_sigma)
plot(man[,1],man[,2])
abline(lm(man[,2]~man[,1]),col='blue')
```

![ad](https://github.com/Tony980624/Hotelling-Test/blob/main/file/Rplot.png)

# 生产女人身高体重500个数据

```
woman = mvrnorm(500,c(164,50),matrix(c(64,36,36,36),nrow = 2,byrow = T))
```

# 计算共同协方差矩阵

```
common_cov = (499*man_sigma + 499*woman_sigma)/998
```

# 计算T^2值,F值，和p值

```
mean_diff = c(175,72)-c(164,50)
T2 = (500 * 500) / (500 + 500) * t(mean_diff) %*% solve(common_cov) %*% mean_diff
p = 2  # 变量的维度
n_X = 500  # 男性样本数
n_Y = 500  # 女性样本数

F_value <- (n_X + n_Y - p - 1) / (p * (n_X + n_Y - 2)) * T2
p_value <- pf(F_value, p, n_X + n_Y - p - 1, lower.tail = FALSE)

p_value
```
