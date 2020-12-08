

### include data overview,data transformation, validation, cross validation, robust regression
####author name:Yuhan Zhang, Shiyi Zhou


############read data########
redwine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
redwine <- read.csv(redwine_url, header = TRUE, sep = ";")
redwine


#######data overview######
install.packages('corrplot')
library(corrplot)
install.packages('corrgram')
library(corrgram)
install.packages('ggplot2')
library(ggplot2)
install.packages("MPV")
install.packages("faraway")
library(MPV) 
library(faraway)

plot(redwine)
corrplot(cor(redwine))
corrgram(x=cor(redwine))
vif(redwine)
ggplot(gather(wine), 
       aes(value)) + geom_histogram(color = "white") + 
  facet_wrap(~key, scales = "free")

##########data transformation###########
##1 fixed acidity
log10_fa <- log10(redwine$fixed.acidity)
quality <- (redwine$quality)
cor(quality, log10_fa) ##0.114

cor(redwine$quality, redwine$fixed.acidity)  ##0.124


##2 volatile acidity
log10_va <- log10(redwine$volatile.acidity)
cor(quality, log10_va) ##-0.391

cor(quality, redwine$volatile.acidity)  ##-0.391

##3 citric acid
log10_ca <- log10(redwine$citric.acid)
cor(quality, log10_ca) ##NaN

cor(quality, redwine$citric.acid)  ##-0.226

##4 - residual sugar log!!
log10_rs <- log10(redwine$residual.sugar)
cor(quality, log10_rs) ##0.0235

cor(quality, redwine$residual.sugar)  ##0.0137

##5 - chlorides log!!
log10_chlorides <- log10(redwine$chlorides)
cor(quality, log10_chlorides) ##-0.176

cor(quality, redwine$chlorides)  ##-0.129

##6 - free sulfur dioxide
log10_fsd <- log10(redwine$free.sulfur.dioxide)
cor(quality, log10_fsd) ##-0.0501

cor(quality, redwine$free.sulfur.dioxide)  ##-0.0507

##7 - total sulfur dioxide
log10_tsd <- log10(redwine$total.sulfur.dioxide)
cor(quality, log10_tsd) ##-0.17

cor(quality, redwine$total.sulfur.dioxide)  ##-0.185

##8 - density
log10_density <- log10(redwine$density)
cor(quality, log10_density) ##-0.175

cor(quality, redwine$density)  ##-0.175

##10 - sulphates log!!
log10_sulphates <- log10(redwine$sulphates)
cor(quality, log10_sulphates) ##0.309

cor(quality, redwine$sulphates)  ##0.251

##11 - alcohol
log10_alcohol <- log10(redwine$alcohol)
cor(quality, log10_alcohol) ##0.477

cor(quality, redwine$alcohol)  ##0.476


##12 volatile acidity & total sulfur acidity log
quality <- (redwine$quality)
log10_com1 <- log10(redwine$volatile.acidity*redwine$total.sulfur.dioxide)
cor(quality, log10_com1) ## -0.315

cor(quality, redwine$volatile.acidity*redwine$total.sulfur.dioxide)##-0.2789


##13 sulphates & alcohol log
log10_com2 <- log10(redwine$sulphates*redwine$alcohol)
cor(quality, log10_com2) ## 0.453

cor(quality, redwine$sulphates*redwine$alcohol)##0.413

##14 volatile acidity& alcohol
log10_com3 <- log10(redwine$volatile.acidity*redwine$alcohol)
cor(quality, log10_com3) ## -0.265

cor(quality, redwine$volatile.acidity*redwine$alcohol)##-0.261

##########models############
full    <- lm(quality~., data=train_data)
mod1 <- lm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data=redwine)

mod2 <- lm(quality~ fixed.acidity +volatile.acidity +citric.acid + I(log10(residual.sugar)) +I(log10(chlorides)) 
           +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +I(log10(sulphates)) +alcohol, data=redwine)
mod3 <- lm(quality~volatile.acidity +I(log10(chlorides)) +total.sulfur.dioxide +I(log10(sulphates))+alcohol, data=redwine)
mod4 <- lm(quality~ fixed.acidity + volatile.acidity +citric.acid +residual.sugar+chlorides 
           +free.sulfur.dioxide +total.sulfur.dioxide +pH+density +sulphates +alcohol 
           +I(log(volatile.acidity * total.sulfur.dioxide)) +I(log(sulphates*alcohol))+alcohol*volatile.acidity, data=redwine)
mod5<- lm(quality~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), data=redwine  )
fit_final <- lm(quality ~ volatile.acidity + I(log10(chlorides)) + 
                  free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                  alcohol, data = redwine)

step <- stepAIC(mod4, direction = "both")



############validation##########
n=nrow(redwine)
set.seed(666)
nsamp=ceiling(0.7*n)

training_samps=sample(1:n,nsamp)
train_data<-redwine[training_samps,]
test_data<-redwine[-training_samps,]
train_data
test_data

###validation for model 0###
y_hat <- predict(full, newdata=test_data)
r_2=cor(y_hat,test_data$quality)^2
r_2 ###0.33427


###validation for model 1###
y_hat1 <- predict(mod1, newdata=test_data)
r_2_1=cor(y_hat1,test_data$quality)^2
r_2_1 ###0.3449

###validation for model 2 ###
y_hat2 <- predict(mod2, newdata=test_data)
r_2_2=cor(y_hat2,test_data$quality)^2
r_2_2 ###0.3605


###validation for model 3###
y_hat3 <- predict(mod3, newdata=test_data)
r_2_3=cor(y_hat3,test_data$quality)^2
r_2_3 ###0.3573

###validation for model 4###
y_hat4 <- predict(mod4, newdata=test_data)
r_2_4=cor(y_hat4,test_data$quality)^2
r_2_4 ###0.3830


###validation for model fit###
y_hatfit <- predict(fit_final, newdata=test_data)
r_2_f=cor(y_hatfit,test_data$quality)^2
r_2_f ###0.3578


###validation for model 5###
y_hat5 <- predict(mod5, newdata=test_data)
r_2_5=cor(y_hatfit,test_data$quality)^2
r_2_5 ###0.3578


############robest regression#############
###model 0###
install.packages("MASS")
library(MASS)
model_0 <- lm(quality ~., data = redwine)
rr.model_0 <- rlm(quality ~., data = redwine, psi=psi.huber)
summary(rr.model_0)
plot(model_0, which=c(1,2,3,5))
plot(rr.model_0, which=c(1,2,3,5))
rr.model_0$w
redwine[c(653,833,1277),]
###most data are significant except 653, 833, 1277.

###model 1###
model_1 <- lm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data = redwine)
rr.model_1 <- rlm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data = redwine, psi=psi.huber)
summary(rr.model_1)
plot(model_1, which=c(1,2,5))
plot(rr.model_1, which=c(1,2,5))
redwine[c(46,833,1506),]
rr.model_1$w
###most data are significant except 46, 833, 1506.

###model 2###
model_2 <- lm(quality ~ fixed.acidity +volatile.acidity +citric.acid + I(log10(residual.sugar)) +I(log10(chlorides)) 
              +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +I(log10(sulphates)) +alcohol, data = redwine)
rr.model_2 <- rlm(quality ~ fixed.acidity +volatile.acidity +citric.acid + I(log10(residual.sugar)) +I(log10(chlorides)) 
                  +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +I(log10(sulphates)) +alcohol, data = redwine, psi=psi.huber)
summary(rr.model_2)
plot(model_2, which=c(1,2,5))
plot(rr.model_2, which=c(1,2,5))
redwine[c(653,833,1277),]
rr.model_2$w
###most data are significant except 653, 833, 1277.

###model 3###
model_3 <- lm(quality ~volatile.acidity +I(log10(chlorides)) +total.sulfur.dioxide +I(log10(sulphates))+alcohol, data=redwine)
rr.model_3 <- rlm(quality ~volatile.acidity +I(log10(chlorides)) +total.sulfur.dioxide +I(log10(sulphates))+alcohol, data=redwine)
summary(rr.model_3)
plot(model_3, which=c(1,2,5))
plot(rr.model_3, which=c(1,2,5))
redwine[c(46,833,1506),]
rr.model_3$w
###most data are significant except 46, 833, 1506.

###model 4###
model_4 <- lm(quality~ fixed.acidity + volatile.acidity +citric.acid +residual.sugar+chlorides 
              +free.sulfur.dioxide +total.sulfur.dioxide +pH+density +sulphates +alcohol 
              +I(log(volatile.acidity * total.sulfur.dioxide)) +I(log(sulphates*alcohol))+alcohol*volatile.acidity, data=redwine)
rr.model_4 <- rlm(quality ~ fixed.acidity + volatile.acidity +citric.acid +residual.sugar+chlorides 
                  +free.sulfur.dioxide +total.sulfur.dioxide +pH+density +sulphates +alcohol 
                  +I(log(volatile.acidity * total.sulfur.dioxide)) +I(log(sulphates*alcohol))+alcohol*volatile.acidity, data=redwine)
summary(rr.model_4)
plot(model_4, which=c(1,2,3,5))
plot(rr.model_4, which=c(1,2,3,5))
redwine[c(46,833,1506),]
rr.model_4$w

###model fit###
model_fit <- lm(quality~ volatile.acidity + I(log10(chlorides)) + 
                free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                alcohol, data=redwine)
rr.model_fit <- rlm(quality ~ volatile.acidity + I(log10(chlorides)) + 
                    free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                    alcohol, data=redwine)
summary(rr.model_fit)
plot(model_fit, which=c(1,2,3,5))
plot(rr.model_fit, which=c(1,2,3,5))
redwine[c(653,833,1277),]
rr.model_fit$w

###model 5###
mod5 <- lm(quality~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), data=redwine)
rr.mod5 <- rlm(quality ~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), data=redwine)
summary(rr.mod5)
plot(mod5, which=c(1,2,3,5))
plot(rr.mod5, which=c(1,2,3,5))
redwine[c(653,833,1277),]
rr.model_fit$w


###summary
install.packages("MASS")
library(MASS)
set.seed(666)
index= sample.int(1050, size=1050, replace=TRUE)


slopeFULL <- rlm(quality ~., psi=psi.huber,data=redwine[index,])$coef
slope1 <- rlm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, psi=psi.huber,data=redwine[index,])$coef
slope2 <- rlm(quality ~ fixed.acidity +volatile.acidity +citric.acid + I(log10(residual.sugar)) +I(log10(chlorides)) 
              +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +I(log10(sulphates)) +alcohol, psi=psi.huber,data=redwine[index,])$coef
slope3 <- rlm(quality ~volatile.acidity +I(log10(chlorides)) +total.sulfur.dioxide +I(log10(sulphates))+alcohol, psi=psi.huber,data=redwine[index,])$coef
slope4 <- rlm(quality~ fixed.acidity + volatile.acidity +citric.acid +residual.sugar+chlorides 
              +free.sulfur.dioxide +total.sulfur.dioxide +pH+density +sulphates +alcohol 
              +I(log(volatile.acidity * total.sulfur.dioxide)) +I(log(sulphates*alcohol))+alcohol*volatile.acidity, psi=psi.huber,data=redwine[index,])$coef
slope.fit <- rlm(quality ~ volatile.acidity + I(log10(chlorides)) + 
                  free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                  alcohol, psi=psi.huber,data=redwine[index,])$coef
slope5 <- rlm(quality ~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), psi=psi.huber,data=redwine[index,])$coef

slopeFULL
slope1
slope2
slope3
slope4
slope5
slope.fit



#####################cross validation###################
white_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
redwine <- read.csv(white_url, header = TRUE, sep = ";")
redwine

data(redwine)

MSPE1 = NULL
MSPE2 = NULL
MSPE3 = NULL
MSPE4 = NULL
MSPE5 = NULL
MSPE_fit= NULL
MSPE_full = vector("numeric",50)
R.sq1= c()
R.sq2 = c()
R.sq3 = c()
R.sq4= c()
R.sq5= c()
R.sq_full = c()
R.sq_fit = c()

for(i in 1:50){
  nsamp=ceiling(0.8*n)
  training_samps=sample(1:n,nsamp)
  training_samps=sample.int(n,nsamp,replace = FALSE)
  training_samps=sort(training_samps)
  train_data  <- redwine[training_samps, ]
  test_data  <-   redwine[-training_samps, ]
  
  volatile.acidity.alcohol=redwine$alcohol*redwine$volatile.acidity
  full    <- lm(quality~., data=train_data)
  mod1 <- lm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data=redwine)
  
  mod2 <- lm(quality~ fixed.acidity +volatile.acidity +citric.acid + I(log10(residual.sugar)) +I(log10(chlorides)) 
             +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +I(log10(sulphates)) +alcohol, data=redwine)
  mod3 <- lm(quality~volatile.acidity +I(log10(chlorides)) +total.sulfur.dioxide +I(log10(sulphates))+alcohol, data=redwine)
  mod4 <- lm(quality~ fixed.acidity + volatile.acidity +citric.acid +residual.sugar+chlorides 
               +free.sulfur.dioxide +total.sulfur.dioxide +pH+density +sulphates +alcohol 
               +I(log(volatile.acidity * total.sulfur.dioxide)) +I(log(sulphates*alcohol))+alcohol*volatile.acidity, data=redwine)
  fit_final <- lm(quality ~ volatile.acidity + I(log10(chlorides)) + 
                    free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                    alcohol, data = train_data)
  mod5<- lm(quality~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), data=redwine  )
  MSPE1[i] = mean((test_data$quality- predict(mod1, newdata= test_data))^2)
  MSPE2[i] = mean((test_data$quality- predict(mod2, newdata= test_data))^2)
  MSPE3[i] = mean((test_data$quality- predict(mod3, newdata= test_data))^2)
  MSPE4[i] = mean((test_data$quality- predict(mod4, newdata= test_data))^2)
  MSPE5[i] = mean((test_data$quality- predict(mod5, newdata= test_data))^2)
  MSPE_full[i] = mean((test_data$quality- predict(full, newdata= test_data))^2)
  MSPE_fit[i] = mean((test_data$quality- predict(fit_final, newdata= test_data))^2)
  R.sq1[i] = cor(predict(mod1, newdata= test_data),test_data$quality)
  R.sq2[i] = cor(predict(mod2, newdata= test_data),test_data$quality)
  R.sq3[i] = cor(predict(mod3, newdata= test_data),test_data$quality)
  R.sq4[i] = cor(predict(mod4, newdata= test_data),test_data$quality)
  R.sq5[i] = cor(predict(mod5, newdata= test_data),test_data$quality)
  R.sq_full[i] =cor(predict(full, newdata= test_data),test_data$quality)
  R.sq_fit[i] =cor(predict(fit_final, newdata= test_data),test_data$quality)
}

mean(MSPE_full)
mean(MSPE1)
mean(MSPE2)
mean(MSPE3)
mean(MSPE4)
mean(MSPE5)
mean(MSPE_fit)


mean(R.sq_full)
mean(R.sq1)
mean(R.sq2)
mean(R.sq3)
mean(R.sq4)
mean(R.sq5)
mean(R.sq_fit)

AIC(model_fit)
summary(model_fit)
anova(model_fit)

AIC(mod5)
summary(mod5)
anova(mod5)

#########rstudent plot
install.packages("car")
library(car)
plot(redwine,rstudent(mod5),main="residual plot of mod5")
hist(rstudent(mod4.5))
hist(rstudent(mod5))
summary(mod5)

######residual plot for models
resid0 <-resid(mod0)
plot(resid0)

resid1 <-resid(mod1)
plot(resid1)

resid2 <-resid(mod2)
plot(resid2)

resid3 <-resid(mod3)
plot(resid3)

resid4 <-resid(mod4)
plot(resid4)

resid5 <-resid(mod5)
plot(resid5)

resid5 <-resid(model_fit)
plot(resid5)

###summary for models

summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(model_fit)

### find model fit
fit <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
            I(log10(residual.sugar)) + I(log10(chlorides)) + free.sulfur.dioxide + 
            total.sulfur.dioxide + density + pH + I(log10(sulphates)) + 
            alcohol, data = train_data)
summary(fit)
select <- stepAIC(fit, direction = "both")
fit_final <- lm(quality ~ volatile.acidity + I(log10(chlorides)) + 
                  free.sulfur.dioxide + total.sulfur.dioxide + pH + I(log10(sulphates)) + 
                  alcohol, data = train_data)
summary(fit_final)
plot(fit_final)


###find mod5
step <- stepAIC(mod4, direction = "both")
mod5<- lm(quality~ residual.sugar+I(log(volatile.acidity * total.sulfur.dioxide))+total.sulfur.dioxide+density+chlorides+pH+volatile.acidity+sulphates+I(log(sulphates * alcohol)), data=redwine  )
summary(mod5)