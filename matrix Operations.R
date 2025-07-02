#Matrix Operations
A=matrix(c(1,2,3,4,5,6,7,3,9),nrow = 3,ncol = 3,byrow = TRUE)
B=matrix(c(1,2,3,4,5,6,7,3,9),nrow = 3,ncol = 3,byrow = FALSE)
det(A) #Determinant
I=solve(A) #Inverse
Add=A+B #Addition 
Mult=A%*%B
t(A) #Transpose
eigen(A) #eigenvalues of A
