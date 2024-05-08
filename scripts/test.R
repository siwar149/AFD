# Vamos por pasos
z <- matrix(c(80, 20, 50, 30,
              10, 120, 30, 60,
              80, 100, 175, 100,
              40, 120, 90, 140), nrow = 4, byrow = T)

x <- c(290, 400, 725, 670)

L <- solve(diag(4)-(z %*% solve(diag(x))))

F <- matrix(c(50, 60,
              80, 100,
              150, 120,
              180, 100), nrow = 4, byrow = T)

F1 <- rowSums(F)

# satelite account on some pressure
s <- c(3500, 2100, 17500, 12250)

# intensity vector
e <- s / x

T <- diag(e) %*% L %*% diag(F1)

#t1 <- diag(e) %*% L %*% diag(F[,1]) # this is meaningless
#colSums(t1)
#s

Tpf <- rowSums(T) # production
Tpf - s # this is consistent

Tcf <- colSums(T) # consumption
as.matrix(Tcf)


# This, does get the consumption based footprint (disagregated by country)
t <- t(L) %*% e * F1

tis <- t
tis[3:4] <- 0

dt <- -0.1 * sum(t[1:2]) * (tis / sum(t))


dF <- dt / (t(L) %*% e)
abs(dF) / F1


# Calculate variation in output
dx <- L %*% dF
abs(dx) / x








# And the production based footprint? (For later)

Tis <- Tcf
Tis[3:4] <- 0
weightsis <- Tis / sum(Tis)
reducT=0
counter=0
deltaf = matrix(0,nrow=length(Tis),ncol=1)
while(reducT< -0.01*sum(Tcf[1:2])|counter<10000){
  ishock = sample(1:length(Tis), size = 1, prob = weightsis)
  deltaf[ishock]=deltaf[ishock]-1
  deltaTis = t(L) %*% e * deltaf
  reducT=sum(deltaTis[1:2,])
  counter=counter+1
}


Tis = runif(9680,0,50)
weigthsis = Tis/sum(Tis)
reducT=0
counter=0
deltaf= matrix(0,nrow=length(Tis),ncol=1)
while(reducT>-0.01|counter<10000){
  ishock = sample(1:length(Tis), size = 1, prob = weigthsis)
  deltaf[ishock]=deltaf[ishock]+1
  deltaTis = diag(e)*L*deltaf
  reducT=rsum(deltaTis[1:""])
  counter=counter+1
} 