rm(list = ls())
set.seed(1)
library(bigstatsr)
# simulating some data
N <- 4000
M <- 730
X <- bigstatsr::FBM(N, M, init = rnorm(N * M, sd = 5))
y <- rowSums(X[, 1:10]) + rnorm(N)
covar <- matrix(rnorm(N * 3), N)
ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)


devtools::load_all("./")

# fitting model for multiple lambdas and alphas
test <- bigstatsrExtra::big_spLinReg(X, y[ind.train], ind.train = ind.train,
                     alphas = 1, K = 3, warn = FALSE, ncores = 3)


pred <- predict.cv(test, X, ind.row = ind.test)

glmfit = glmnet::cv.glmnet(X[ind.train,], y[ind.train])

coef(glmfit, s = "lambda.min")
glmfit$lambda.min
