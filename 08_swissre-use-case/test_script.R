setwd("D:/09_analytics_new_start/07_swiss_re")

train_File = "train_dataset.csv"

train_data = read.csv(train_File)
X = as.matrix(train_data[,2:5])
y = as.matrix(train_data[,1])

ols.lf1 <- function(theta, y, X) {
  beta <- theta[-1]
  sigma2 <- theta[1]
  if (sigma2 <= 0) return(NA)
  n <- nrow(X)
  print (beta)
  e <- y - X%*%beta                                  # t() = matrix transpose
  logl <- ((-n/2)*log(2*pi)) - ((n/2)*log(sigma2)) - ((t(e)%*%e)/(2*sigma2))
  return(-logl) # since optim() does minimisation by default.
}

ols.lf1(c(1,1,1,1,1),y,X)

-20989185.585176494

# Analytical derivatives
ols.gradient <- function(theta, y, X) {
  beta <- theta[-1]
  sigma2 <- theta[1]
  e <- y - X%*%beta
  n <- nrow(X)
  
  g <- numeric(length(theta))

    g[1] <- (-n/(2*sigma2)) + (t(e)%*%e)/(2*sigma2*sigma2) # d logl / d sigma
    g[-1] <- (t(X) %*% e)/sigma2                           # d logl / d beta
  
  return(-g)
}




ols.gradient(c(1,1,1,1,1),y,X)



cat("\nGradient-free (constrained optimisation) --\n")
m1 = optim(c(1,1,1,1,1), method="L-BFGS-B", fn=ols.lf1,
           lower=c(1e-6, 1e-6, 1e-6,-Inf,-Inf), upper=rep(Inf,5), y=y, X=X)

cat("\nUsing the gradient (constrained optimisation) --\n")
m2 = optim(c(1,1,1, 1, 1), method="L-BFGS-B", fn=ols.lf1, gr=ols.gradient,
           lower=c(1e-6, 1e-6, 1e-6,-Inf,-Inf), upper=rep(Inf,3), y=y, X=X)

print (m1$par)

print (m2$par)


g <- 1

g

g[1] = 2

g[-1] = 3
g
