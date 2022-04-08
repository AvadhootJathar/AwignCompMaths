# Large scale regression problem

rm(list = ls(all.names = TRUE))
library(matlib)

x = NULL
y = 1.50
act_coefs = rnorm(n = 200, mean = 1, sd = 2.3)
sdev = runif(n=200, min = 0.25, max = 10.2)
for (i in c(1:200)){
  x[[length(x)+1]] = rnorm(n=1000000,mean = 0,sd=sdev[i])
  y = y + x[[i]]*act_coefs[i]
}
x = data.frame(x)  
X = as.matrix(x)
X = cbind(rep(1,1000000),X)
colnames(X) = c("Intercept",paste("X",c(1:200),sep = ""))
rm(x)

start = Sys.time()
betas = inv(t(X)%*%X)%*%(t(X)%*%y)
end = Sys.time()
end - start

X = X[,-1]
start = Sys.time()
out = lm(y~X)
end = Sys.time()
end - start
#summary(out)

# Computation time for QR decomposition of X
start = Sys.time()
q_and_r = qr(X)
end = Sys.time()
end - start

# Computation time for Matrix inverse of X'X
# This computation is by varying dimension size
test_size_X = c(5,10,20,40,100,200)
# matrix to store the computation time to invert
time_to_invert = mat.or.vec(length(test_size_X),2)

l = 0
for (k in test_size_X){
  l = l + 1
  sdevk = runif(k, min = .25, max = 10.0)
  xk = NULL
  for (j in c(1:k)){
    xk[[length(xk)+1]] = rnorm(n=1000000,mean = 0,sd = sdevk[j])
  }
  xk = data.frame(xk)
  xk = as.matrix(xk)
  time_to_invert[l,1] = k
  time_to_invert[l,2] = system.time(inv(t(xk)%*%xk))[3][1]
  cat("processed X of size:",k,"\n")
}
# Check the time required to invert X of different dimensions
print(time_to_invert)
