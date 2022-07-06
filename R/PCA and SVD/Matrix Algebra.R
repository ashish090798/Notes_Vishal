a <- c(0,8,12,20)
b <- c(8,9,11,12)
c <- c(10,10,10,10)

# Standard Deviation
sd_a <- sd(a)
sd_b <- sd(b)
sd_c <- sd(c)

# Covariance
cov_ab <- cov(a,b)
cov_bc <- cov(b,c)
cov_ac <- cov(a,c)

# Dot products
sum(a*b)

# Orthogonal Vectors (dot product = 0)
p <- c(2,1,-2,4)
q <- c(3,-6,4,2)
sum(p*q)

# Normal Vector (a vector of magnitutde 1)
v <- c(2,4,1,2)
norm_v <- v/sqrt(sum(v^2))
print(norm_v)
# norm_v is a normal vector as its magnitude is 1
sqrt(sum(norm_v^2))

# Orthonormal Vectors (unit vectors that are orthogonal to each other)
u <- c(2/5, 1/5, -2/5, 4/5)
v <- c(3/sqrt(65), -6/sqrt(65), 4/sqrt(65), 2/sqrt(65))
sqrt(sum(u^2))
sqrt(sum(v^2))
sum(u*v)

# Gram-Schmidt Orthonormalization Process
A <- rbind(c(1,2,1), c(0,2,0), c(2,3,1), c(1,1,0))
print(A)
qr.Q(qr(A)) # Gives the GS output

# Transpose of a Matrix
A <- matrix(c(3,1,1,-1,3,1), nrow = 2, ncol = 3, byrow = TRUE)
print(A)
A_T <- t(A)
print(A_T)

# Eigenvalues & Eigenvectors (of a square matrix)
sq_A <- A%*%A_T
print(sq_A)

ev <- eigen(sq_A)
values <- ev$values
print(values)
vectors <- ev$vectors
print(vectors)

### PCA
# Let's start with some data
A <- matrix(c(2.5,2.4,0.5,0.7,2.2,2.9,1.9,2.2,3.1,3.0,2.3,2.7,2,1.6,1,1.1,1.5,1.6,1.1,0.9), nrow = 10, ncol = 2, byrow = TRUE)
print(A)

# Step 1: Subtract the mean to center the data
center_apply <- function(x) {apply(x, 2, function(y) y - mean(y))}
A_adj <- center_apply(A)
print(A_adj)

# Step 2: Calculate the covariance matrix of A_adj
cov_A <- cov(A_adj)
print(cov_A)

# Step 3: Calculate the eigenvectors and eigenvalues
# of the covariance matrix
ev <- eigen(cov_A)
vectors <- ev$vectors
print(vectors)
values <- ev$values
print(values)
plot(A)
plot(A_adj)
# Lets also plot the eigen vectors on the same plot
# Clearly the eigenvectors are orthogonal to each other
# Eigenvector corresponding to the highest Eigenvalue is the
# Principle Component of the dataset

# SVD
# U contains orthonormal eigenvectors of A%*%A_T
U <- vectors
print(U)

# V contains orthonormal eigenvectors of A_T%*%A
sq_A_2 <- A_T%*%A
print(sq_A_2)
ev_2 <- eigen(sq_A_2)
values_2 <- ev_2$values
print(values_2)
vectors_2 <- ev_2$vectors
print(vectors_2)
V <- vectors_2
print(V)

# S contains square roots of the non-zero eigenvalues along the diagonal
S <- matrix(c(sqrt(12),0,0,0,sqrt(12),0), nrow = 2, ncol = 3, byrow = TRUE)
print(S)

# A = -(U %*% S %*% t(V))
print(-(U %*% S %*% t(V)))

