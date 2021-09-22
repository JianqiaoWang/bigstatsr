rm(list = ls())
set.seed(1)
library(bigstatsr)
# simulating some data
#--------------------------------------------
N <- 4000
M <- 730
X <- bigstatsr::FBM(N, M, init = rnorm(N * M, sd = 5))
y <- rowSums(X[, 1:10]) + rnorm(N)
covar <- matrix(rnorm(N * 3), N)
ind.train <- sort(sample(nrow(X), 500))
ind.test <- setdiff(rows_along(X), ind.train)
# fitting model for multiple lambdas and alphas
test <- big_spLinReg.cv(X, y[ind.train], ind.train = ind.train,
                     alphas = 1, K = 10, warn = FALSE, ncores = 3, n.abort = 20)
pred <- predict.cv(test, X, ind.row = ind.test)
head(test$beta)
tail(test$lambda)

#--------------------------------------------
N <- 230
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y01 <- as.numeric((rowSums(X[, 1:10]) + 2 * rnorm(N)) > 0)
covar <- matrix(rnorm(N * 3), N)
ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)
# fitting model for multiple lambdas and alphas
test <- big_spLogReg.cv(X, y01[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ],
                     alphas = 1, K = 3, warn = FALSE)
pred <- predict.cv(test, X, ind.row = ind.test)
head(test$beta)
tail(test$lambda)

#--------------------------------------------
glmfit = glmnet::cv.glmnet(X[ind.train,], y[ind.train])
head(coef(glmfit, s = "lambda.min"))
glmfit$lambda.min


test <- big_spLinReg.cv(X, y[ind.train], ind.train = ind.train,
                        alphas = 1, K = 10, warn = FALSE, ncores = 3, n.abort = 20)
pred <- predict.cv(test, X, ind.row = ind.test)
head(test$beta)
tail(test$lambda)
