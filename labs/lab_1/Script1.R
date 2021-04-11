## 1
set.seed(1414)

# rpois = generate poisson rv 
y20   <- rpois(n=20, lambda = .5) 
y100  <- rpois(n = 100, lambda = .5)
y1000 <- rpois(n = 1000, lambda = .5)

# c() =  combines its arguments to form a vector
a <- c(1, 1, 1, 2, .5)
b <- c(.5, 2, 10, 2, 0)


priordist <- function(theta, k){
    if(k==5){return(1/sqrt(theta))}
    return(dgamma(theta, a[k], b[k]))
}

xl <- c(0,3)
yl <- c(0,10)

# prepare multiple plot in a 2*2 matrix
par(mfrow=c(2,2))

# empty plot
plot(c(),c(),xlim=xl,ylim=yl,main="Prior",xlab="",ylab="")

# Plot 1- curve priordist for x with 5 different settings
for(k in 1:5) 
  curve(priordist(x,k), add=T, col=k)

legend("topright",
       legend=c("Gamma1", "Gamma2",
                "Gamma3", "Gamma4",
                "Jeffreys'"),
       col=c(1:5), lty=1, cex=0.8)

## 2
# Posterior distribution: Gamma(alpha=sum(y)+alpha_prior, beta=beta+len(y))
postdist <- function(theta,y,k){
  return(dgamma(theta,a[k]+sum(y),b[k]+length(y)))
}

plot(c(),c(),xlim=xl,ylim=yl,main="n=20",xlab="",ylab="")

# Plot 2 - curve postdist for x with 5 different prior settings with y20
for(k in 1:5) 
  curve(postdist(x,y20,k), add=T, col=k)


legend("topright",
  legend=c("Gamma1", "Gamma2",
    "Gamma3", "Gamma4",
    "Jeffreys'"),
  col=c(1:5), lty=1, cex=0.8)

# Plot 3- curve postdist for x with 5 different prior settings with y100
plot(c(),c(),xlim=xl,ylim=yl,main="n=100",xlab="",ylab="")
for(k in 1:5) 
  curve(postdist(x,y100,k), add=T, col=k)

legend("topright",
       legend=c("Gamma1", "Gamma2",
                "Gamma3", "Gamma4",
                "Jeffreys'"),
       col=c(1:5), lty=1, cex=0.8)

# Plot 4 - curve postdist for x with 5 different prior settings with y1000
plot(c(),c(),xlim=xl,ylim=yl,main="n=1000",xlab="",ylab="")
for(k in 1:5) 
  curve(postdist(x,y1000,k), add=T, col=k)

## 3

## posterior expectations
## For a Gamma E(theta) = alpha / beta
(a+sum(y20))/(b+20)
(a+sum(y100))/(b+100)
(a+sum(y1000))/(b+1000)

##MAP
## Posterior mode Mode(theta) = alpha-1 / beta
(a+sum(y20)-1)/(b+20)
(a+sum(y100)-1)/(b+100)
(a+sum(y1000)-1)/(b+1000)


## 4
x <- seq(0,3,by=0.001)
post20.1 <- postdist(x,y20,1) 
plot(x,post20.1, type="l")
x[post20.1==max(post20.1)]

(a[1]+sum(y20)-1)/(b[1]+20)

abline(v=(a[1]+sum(y20)-1)/(b[1]+20), lty=2, col="darkgray")

ppost20.1 <- post20.1/sum(post20.1)
plot(x,ppost20.1, type="l")
sum(ppost20.1) #1
ppost20.1s <- sort(ppost20.1, decreasing = TRUE)
hpdy <- min(ppost20.1s[cumsum(ppost20.1s)< .95])
abline(h=hpdy, lty=2, col="red")

range(x[ppost20.1>=hpdy])
# make two points at coordinates with xs = range(x[pppost>hpdy]) y = (hpdy, hpdy)
points(range(x[ppost20.1>=hpdy]),rep(hpdy,2),pch=20, col="red")
# make two points at coordinates with xs = range(x[pppost>hpdy]) y = (0 , 0)
points(range(x[ppost20.1>=hpdy]),rep(0,2),pch=20, col="red")

#credible intervals
# qgamma = qtile function of gamma distr
#  quantile function Q returns the value x such that Pr(X<=x)=p
qgamma(c(.025,.975),1+sum(y20),20+.5)

points(qgamma(c(.025,.975),1+sum(y20),20+.5),rep(0,2),
  pch=13, col="darkgreen")

