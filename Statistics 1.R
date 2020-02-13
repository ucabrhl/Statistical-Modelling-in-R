#CS1B Y1 Assignment

#-------------------------------------------------------------
#Y1.1 Binomial distribution
#-------------------------------------------------------------

n <- 1000
p <- 0.05

#(i) median

qbinom(0.5,n,p)

#answer:50

#(ii)P(45 <= D <= 59)
#(a) exact probability

pbinom(59,n,p) - pbinom(44,n,p)

#answer: 0.6985849

#(b) Poisson approx

#Bin(n,p) ~ Poi(np) = Poi(50)

ppois(59,n*p) - ppois(44,n*p)

#answer: 0.6866947

#(c) Normal approx

#Bin(n,p) ~ N(np,npq) = N(50,47.5)

pnorm(59.5,n*p,sqrt(n*p*(1-p))) - pnorm(44.5,n*p,sqrt(n*p*(1-p)))

#answer: 0.7035325

#-------------------------------------------------------------
#Y1.2 Simulation of unknown distribution
#-------------------------------------------------------------

#(i)(a) Simulate U(0,1)

set.seed(13)
u <- runif(1000,0,1)
u

#(i)(b)

#The CDF is 1-(1+x)^(-1)
#Setting u equal to the CDF and rearranging gives x= (1-u)^(-1) - 1

x <- (1-u)^(-1)-1
x

#(ii)	Simulations
#(a) Labelled empirical PDF

plot(density(x),xlim=c(0,200),main="Empirical PDF of simulations", xlab="x",col="blue")

#(b)

mean(x)

#answer: 14.54969

sd(x)

#answer: 199.7557

skew <- sum((x-mean(x))^3)/length(x)
skew/(sd(x)^3)

#answer: 27.72327

#The huge sd for such a small mean indicates that we have a very long tail
#this is confirmed by the massive coefficient of skewness


#-------------------------------------------------------------
#Y1.3 Binomial confidence intervals
#-------------------------------------------------------------

#(i) width of exact confidence interval

test <- binom.test(10,20)
test$conf[2]-test$conf[1]

#answer: 45.6%

#(ii) greatest width when n=10

width <- rep(0,20)

for (i in 1:20)
{test <-binom.test(i,20);width[i]<-test$conf[2]-test$conf[1]}

width

max(width)

#examining the widths, we can see the greatest occurs when n=10


#-------------------------------------------------------------
#Y1.4 Exponential modelling
#-------------------------------------------------------------

#(i) Non-parametric 99% confidence interval

#Since estimate for lambda is 1/sample mean we could obtain 99% confidence
#interval for the sample mean and find its reciprocal

x <- c(14, 4, 3, 2, 3, 1, 5, 10, 4, 23)

bm <- rep(0,1000)
set.seed(17)
for(i in 1:1000)
{y <- sample(x,replace=TRUE); bm[i] <- mean(y)}

#which is identical to:

set.seed(17)
bm <- replicate(1000,mean(sample(x,replace=TRUE)))

#99% confidence interval for the mean
ci <- quantile(bm,c(0.005,0.995))

#Hence 99% confidence interval for lambda
1/ci

#(ii) sample means

#(a) simulate 1000 sample means from Exp(0.145)

xbar<-rep(0,1000)

set.seed(19)
for (i in 1:1000)
{x<-rexp(10,0.145);xbar[i]<-mean(x)}

#which is identical to:

set.seed(19)
xbar <- replicate(1000,mean(rexp(10,0.145)))

#(b) Histogram

hist(xbar, prob=TRUE, main="Histogram of sample means from Exp(0.145)",xlab="Sample mean")

#(c) Probability

length(xbar[xbar<5])/length(xbar)

#answer: 0.183

#(iii) Superimpose gamma

#gamma(10,10*0.145) = gamma(10,1.45)

xvals <- seq(0,20,by=0.01)
lines(xvals,dgamma(xvals,10,1.45),type="l",col="blue")

#fairly similar

#(iv) Exact probability

pgamma(5,10,1.45)

#answer: 0.1957324

#About 7% difference in answers so not that close 

#(v) compare sample means and gamma
#(a) Simulate 1,000 values from gamma

set.seed(21)
sim <- rgamma(1000,10,1.45)

#(b) QQ plot

qqplot(sim,xbar,xlab="gamma quantiles",ylab="sample mean quantiles")
abline(0,1,col="red",lty=2,lwd=2)

#Good in the middle
#At lower end sample means are slightly higher than expected - so lighter lower tail
#At upper end sample means are much lower than expected - so lighter upper tail


#Not a very good fit
#Larger sample needed?