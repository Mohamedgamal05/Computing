#################Lab9#################
## we first initialize our data set 
# 1:head 0:tail
x1=c(1, 0 ,0 ,0 ,1, 1 ,0 ,1 ,0, 1)#5H,5T
x2=c(1, 1 ,1 ,1 ,0, 1 ,1 ,1 ,1, 1)#9H,1T
x3=c(1, 0 ,1 ,1 ,1, 1 ,1 ,0 ,1, 1)
x4=c(1, 0 ,1 ,0 ,0, 0 ,1 ,1 ,0, 0)
x5=c(0, 1 ,1 ,1 ,0, 1 ,1 ,1 ,0, 1)
# Fill matrix with our data by rows(rbind)
obs=rbind(x1,x2,x3,x4,x5)
# Putting Initial conditions for Probabilities P(A) , P(B)
propa_initial=c(.6,.5)
# Some Initial numbers and steps will be needed in the code
errr=10
propb=0
propa=propa_initial


# E-step (Expectation Step)
mstep <- function (obs,propa) {
  dat <- matrix (0,ncol = 4, nrow = 5)
  for (i in 1:5) { 
    # Calculate number of heads and tails for each row and summing them 
    headnum=sum(obs[i,]==1)
    tailnum=sum(obs[i,]==0)#
    #Calculating likelihood for A & B using binomial dist law 
    lik_A=(propa[1])^headnum*(1-propa[1])^tailnum
    lik_B=(propa[2])^headnum*(1-propa[2])^tailnum
    #Turning this likelihood to probabilities
    pro_A=lik_A/(lik_A+lik_B)
    pro_B=lik_B/(lik_A+lik_B)
    #Start filling table (which is in lecture) for each row 
    dat[i,1]=pro_A*headnum
    dat[i,2]=pro_A*tailnum
    dat[i,3]=pro_B*headnum
    dat[i,4]=pro_B*tailnum
  }
  # Summing all results
  colSums(dat)
}


# M-step (maximization Step)
while(errr > 10^-6)
{
  propb=propa
  pro_vec=mstep(obs,propa)
  proA_est=pro_vec[1]/(pro_vec[1]+pro_vec[2])
  proB_est=pro_vec[3]/(pro_vec[3]+pro_vec[4])
  propa=c(proA_est,proB_est)
  errr=sum(abs(propa-propb))
}

propa
errr
