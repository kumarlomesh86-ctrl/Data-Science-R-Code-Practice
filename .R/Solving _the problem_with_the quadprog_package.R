library(quadprog)
nss = 1
#Equals 1 if no short sales allowed
Bmat = matrix(0 ,n,n)
#No Short sales matrix
diag(Bmat) = 1
Amat = matrix(c(mu,1 ,1 ,1) ,n,2)
if(nss==1){Amat=matrix(c(Amat,Bmat),n,2+n)}
dvec = matrix(0 ,n,1)
bvec = matrix(c(Er ,1) ,2 ,1)
if(nss==1){bvec=t(c(bvec,matrix(0,3,1)))}
sol = solve.QP(cv ,dvec ,Amat,bvec ,meq=2)
print ( sol$solution )