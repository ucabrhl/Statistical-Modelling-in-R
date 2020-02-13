#CS1B Y2 Assignment

#-------------------------------------------------------------
#Y2.1
#-------------------------------------------------------------

#(i) Test equal variances

A <- c(55.4,53.2,56.0,50.1,51.8)
B <- c(49.2,47.9,52.2,50.8,48.3)

var.test(A,B)

#p-value of 0.5576
#Insufficient evidence to reject H0.  Variances equal.

#(ii) Test whether mean of A greater than mean of B

#Since the variances can be considered equal - use the equal variance t test

t.test(A,B,var.equal=TRUE,alt="greater")

#p-value of 0.01446
#Sufficient evidence to reject H0.  Brand A covers a greater area than Brand B.

#(iii) Non-parametric test

ObsT <- mean(A)-mean(B)
ObsT

results <- c(A,B)

index <- 1:length(results)

p<-combn(index,length(A))

n <- ncol(p)

dif<-rep(0,n)

for (i in 1:n) 
{dif[i]<-mean(results[p[,i]])-mean(results[-p[,i]])}

length(dif[dif>=ObsT])/length(dif)

#empirical p-value is 0.01984127
#Sufficient evidence to reject H0.  Brand A covers a greater area than Brand B.

#-------------------------------------------------------------
#Y2.2
#-------------------------------------------------------------

#distribution variance
dvar <- 400

#prior mean and variance
pmean <- 270
pvar <- 225

#number of years
n <- 5

xp <- rep(0,1000)
set.seed(13)
for (i in 1:1000)
{mu <- rnorm(1,pmean,sqrt(pvar))
x <- rnorm(n,mu,sqrt(dvar))
xp[i] <- (sum(x)/dvar + pmean/pvar)/(n/dvar + 1/pvar)}

mean(xp)

#answer:269.2377

#-------------------------------------------------------------
#Y2.3
#-------------------------------------------------------------

#(i) correlation

cor(swiss,method="spearman")

# Agriculture 0.2426643, Examination -0.66090300, Education -0.44325769,  
# Catholic 0.41364556, Infant.Mortality  0.4371367

#(ii) Scattergraph

attach(swiss)
plot(Education,Fertility,pch=3,main="Scattergraph of Fertility rate vs education")

#(iii)(a) Fit linear regression model

model <- lm(Fertility~Education)
summary(model)

#intercept parameter = 79.6101 and slope parameter = -0.8624
#The p-values are both very significant indicating that the parameters are non-zero

#(iii)(b) Add regression line

abline(model,col="red",lty=3)

#(iii)(c) Problem with model

#R2 = 0.4406   (adjusted R2 = 0.4282)

#The low coefficient of determination is due to the large spread of results
#There are also very few provinces with high levels of education 

#(iv) Residual and 90% confidence interval for 42nd province

resid(model)[42]

#answer: 12.38515

#90% confidence interval
newdata <- data.frame(Agriculture=17.6,Examination=35,Education=32,Catholic=16.92,Infant.Mortality=23.0)

#or since it only has Education in the model we could use:
newdata <-data.frame(Education=32)

predict(model,newdata,interval="prediction",level=0.9)

#answer: (35.2, 68.8)

#(v) Forward selection

#starting model: Education with adjusted R2 = 0.4282

#add 2nd covariate
fit1a <- update(model,.~.+Agriculture)
summary(fit1a)
#adjusted R2 = 0.4242 worse

fit1b <- update(model,.~.+Catholic)
summary(fit1b)
#adjusted R2 = 0.5552 best

fit1c <- update(model,.~.+Examination)
summary(fit1c)
#adjusted R2 = 0.483 better

fit1d <- update(model,.~.+Infant.Mortality)
summary(fit1d)
#adjusted R2 = 0.545 better

#so fit1b improves the adjusted R2 the most and has all parameters significant

#add 3rd covariate
fit2a <- update(fit1b,.~.+Agriculture)
summary(fit2a)
#adjusted R2 = 0.6173 better

fit2b <- update(fit1b,.~.+Examination)
summary(fit2b)
#adjusted R2 = 0.5452 better

fit2c <- update(fit1b,.~.+Infant.Mortality)
summary(fit2c)
#adjusted R2 = 0.639 best

#so fit2c improves the adjusted R2 the most and has all parameters significant

#add 4th covariate
fit3a <- update(fit2c,.~.+Agriculture)
summary(fit3a)
#adjusted R2 = 0.6707 best

fit3b <- update(fit2c,.~.+Examination)
summary(fit3b)
#adjusted R2 = 0.6319 worse

#so fit3a improves the adjusted R2 the most and has all parameters significant

#add 5th covariate
fit4a <- update(fit3a,.~.+Examination)
summary(fit4a)
#adjusted R2 = 0.671 marginally better - but not all parameters significant

#add first Education interaction term
fit5a <- update(fit3a,.~.+Education:Agriculture)
summary(fit5a)
#adjusted R2 = 0.6628 worse

fit5b <- update(fit3a,.~.+Education:Catholic)
summary(fit5b)
#adjusted R2 = 0.699 best

#Examination was not a main effect and so should not be considered for interaction

fit5c <- update(fit3a,.~.+Education:Infant.Mortality)
summary(fit5c)
#adjusted R2 = 0.6779 better

#so fit5b improves the adjusted R2 the most and has all parameters significant

#add second Education interaction term

fit6a <- update(fit5b,.~.+Education:Agriculture)
summary(fit6a)
#adjusted R2 = 0.6917 worse

#Examination was not a main effect and so should not be considered for interaction

fit6b <- update(fit5b,.~.+Education:Infant.Mortality)
summary(fit6b)
#adjusted R2 = 0.6957 worse

#do not add any more covariates
#final model is fit5b

#Note, it is usual to double check that main effects which weren't significant before
#interaction terms were added are still not significant.
#If students check +Examination again they see it has an 
#adjusted R2 = 0.7073 which is slightly better - but not all parameters significant
#so again it won't be added

#(vi)(a) Residual and 90% confidence interval 

resid(fit5b)[42]
fit5b$resid[42]
#answer: 3.568406

#90% confidence interval
newdata <- data.frame(Agriculture=17.6,Examination=35,Education=32,Catholic=16.92,Infant.Mortality=23.0)

predict(fit5b,newdata,interval="prediction",level=0.9)

#answer: (47.6, 74.1)

#(vi)(b) Comment

#For the second model, the residual is smaller and the confidence interval is narrower 
#both indicate that the model is a better fit