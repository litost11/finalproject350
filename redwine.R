redwine <- read.csv("C:/Users/User/Desktop/stat350/term project/winequality-red.csv", sep =";")

install.packages('corrplot')
install.packages('psych')
library(psych)
library(ggplot2)
library(corrplot)
library(tidyr)

#describe the raw data
summary(redwine)
str(redwine)
colnames(redwine)

cor(redwine)
paires(redwine)
plot(redwine)
corrplot(cor(redwine))


#Collinearity between Attributes
res2<- rcorr(as.matrix(redwine))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


wine <- gather(redwine)
ggplot(gather(wine), 
       aes(value)) + geom_histogram(color = "white") + 
       facet_wrap(~key, scales = "free")
ggpairs(redwine)

#Multivariable Linear Regression
########################### FULL model 0 ############################################
model_0 <- lm(quality ~., data = redwine)
summary(model)
par(mfrow = c(2,2))
plot(model_0)
anova(model_0)


n = nrow(redwine) #Use backward elimination to identify the "best model".
qt(0.95,n-2) #critical value

########################### reduced full model 1 ##########################################
#step1#
drop_fixed.acidity = resid(lm(quality ~ .-fixed.acidity, data=redwine))
summary(lm(drop_fixed.acidity~redwine$fixed.acidity))$coef[2,3]  #0.3465581 remove

drop_volatile.acidity = resid(lm(quality ~ .-volatile.acidity, data=redwine))
summary(lm(drop_volatile.acidity~redwine$volatile.acidity))$coef[2,3] #-6.636638

drop_citric.acid = resid(lm(quality ~ .-citric.acid, data=redwine))
summary(lm(drop_citric.acid~redwine$citric.acid))$coef[2,3] #-0.7033369 remove

drop_residual.sugar = resid(lm(quality ~ .-residual.sugar, data=redwine))
summary(lm(drop_residual.sugar~redwine$residual.sugar))$coef[2,3] #0.8367781 remove

drop_chlorides = resid(lm(quality ~ .-chlorides, data=redwine))
summary(lm(drop_chlorides~redwine$chlorides))$coef[2,3]  # -3.676011

drop_free.sulfur.dioxide = resid(lm(quality ~ .-free.sulfur.dioxide, data=redwine))
summary(lm(drop_free.sulfur.dioxide~redwine$free.sulfur.dioxide))$coef[2,3] #1.437249 remove

drop_total.sulfur.dioxide = resid(lm(quality ~ .-total.sulfur.dioxide, data=redwine))
summary(lm(drop_total.sulfur.dioxide~redwine$total.sulfur.dioxide))$coef[2,3] #-3.028548

drop_density = resid(lm(quality ~ .-density, data=redwine))
summary(lm(drop_density~redwine$density))$coef[2,3] #-0.3291462 remove

drop_pH = resid(lm(quality ~ .-pH, data=redwine))
summary(lm(drop_pH~redwine$pH))$coef[2,3]  #-1.185661 remove

drop_sulphates = resid(lm(quality ~ .-sulphates, data=redwine))
summary(lm(drop_sulphates~redwine$sulphates))$coef[2,3] #6.683795

drop_alcohol = resid(lm(quality ~ .-alcohol, data=redwine))
summary(lm(drop_alcohol~redwine$alcohol))$coef[2,3] #5.875606
#rest: volatile.acidity, chlorides, total.sulfur.dioxide, sulphates, alcohol


#step2
model_step1 <- lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH, data=redwine)
summary(model_step1)


drop_2_volatile = resid(lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH-volatile.acidity, data=redwine))
summary(lm(drop_2_volatile~redwine$volatile.acidity))$coef[2,3]  #-11.01443

drop_2_chlorides = resid(lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH-chlorides, data=redwine))
summary(lm(drop_2_chlorides~redwine$chlorides))$coef[2,3]  #-3.846774

drop_2_total.sulfur = resid(lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH-total.sulfur.dioxide, data=redwine))
summary(lm(drop_2_total.sulfur~redwine$total.sulfur.dioxide))$coef[2,3]  #-4.433426

drop_2_sulphates = resid(lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH-sulphates, data=redwine))
summary(lm(drop_2_sulphates~redwine$sulphates))$coef[2,3]  #7.203942

drop_2_alcohol = resid(lm(quality ~ .-fixed.acidity-citric.acid-residual.sugar-free.sulfur.dioxide-density-pH-alcohol, data=redwine))
summary(lm(drop_2_alcohol~redwine$alcohol))$coef[2,3]  # 15.42303
#keep: volatile.acidity, chlorides, total.sulfur.dioxide, sulphates, alcohol
model_1 <- lm(quality ~volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data=redwine)
summary(model_1)
par(mfrow = c(2,2))
plot(model_1)
anova(model_1)


########################### data transformation of full model 2 ############################################
##4 - residual sugar log!!
quality <- (redwine$quality)
log10_rs <- log10(redwine$residual.sugar)
cor(quality, redwine$rs)  ##0.0137
cor(quality, log10_rs) ##0.0235

##5 - chlorides log!!
log10_chlorides <- log10(redwine$chlorides)
cor(quality, redwine$chlorides)  ##-0.129
cor(quality, log10_chlorides) ##-0.176

##10 - sulphates log!!
log10_sulphates <- log10(redwine$sulphates)
cor(quality, redwine$sulphates)  ##0.251
cor(quality, log10_sulphates) ##0.309

#linear regression model for data transformation
model_2 <- lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
         +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine)
summary(model_2)
par(mfrow = c(2,2))
plot(model_2)
anova(model_2)

########################### backforward of data transformation model 3 ############################################
dtDrop_fixed <- resid(lm(quality~ volatile.acidity +citric.acid + log10_rs +log10_chlorides 
         +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_fixed~redwine$fixed.acidity))$coef[2,3]  #0.4874182 remove

dtDrop_volatile <- resid(lm(quality~ fixed.acidity +citric.acid + log10_rs +log10_chlorides 
                         +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_volatile~redwine$volatile.acidity))$coef[2,3] # -6.454224
 
dtDrop_citric <- resid(lm(quality~ fixed.acidity +volatile.acidity + log10_rs +log10_chlorides 
                            +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_citric~redwine$citric.acid ))$coef[2,3] #-1.071319 remove

dtDrop_log10_rs <- resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_chlorides 
                           +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_log10_rs~log10_rs))$coef[2,3] #1.088878 remove

dtDrop_log10_chlorides <- resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +free.sulfur.dioxide
                                   +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_log10_chlorides~log10_chlorides))$coef[2,3] #-3.49189

dtDrop_free <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                       +total.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_free~redwine$free.sulfur.dioxide ))$coef[2,3] #1.275013 remove

dtDrop_total <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                       +free.sulfur.dioxide +density +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_total~redwine$total.sulfur.dioxide))$coef[2,3]   # -2.899796

dtDrop_density <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                          +free.sulfur.dioxide +total.sulfur.dioxide +pH +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_density~redwine$density))$coef[2,3]   # -0.4428797 remove

dtDrop_pH <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                          +free.sulfur.dioxide +total.sulfur.dioxide +density +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop_pH~redwine$pH))$coef[2,3]    # -1.216218 remove

dtDrop_log10_sulphates <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                     +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +alcohol, data=redwine))
summary(lm(dtDrop_log10_sulphates~log10_sulphates))$coef[2,3]    # 7.950868


dtDrop_alcohol <-resid(lm(quality~ fixed.acidity +volatile.acidity +citric.acid + log10_rs +log10_chlorides 
                     +free.sulfur.dioxide +total.sulfur.dioxide +density +pH +log10_sulphates, data=redwine))
summary(lm(dtDrop_alcohol~redwine$alcohol))$coef[2,3]       #4.765552

#drop fixed.acidity, citric.acid, log10_rs, free.sulfur.dioxide, density and pH
dtDrop <-lm(quality~volatile.acidity +log10_chlorides +total.sulfur.dioxide +log10_sulphates +alcohol, data=redwine)
summary(dtDrop)

dtDrop2_volatile <-resid(lm(quality~log10_chlorides +total.sulfur.dioxide +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop2_volatile~redwine$volatile.acidity))$coef[2,3] #-10.21034

dtDrop2_log10_chlorides <-resid(lm(quality~volatile.acidity +total.sulfur.dioxide +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop2_log10_chlorides~log10_chlorides))$coef[2,3] #-3.580853

dtDrop2_total <-resid(lm(quality~volatile.acidity +log10_chlorides +log10_sulphates +alcohol, data=redwine))
summary(lm(dtDrop2_total~redwine$total.sulfur.dioxide))$coef[2,3] #-4.367705

dtDrop2_log10_sulphates <- resid(lm(quality~volatile.acidity +log10_chlorides +total.sulfur.dioxide +alcohol, data=redwine))
summary(lm(dtDrop2_log10_sulphates~log10_sulphates))$coef[2,3] #8.45562

dtDrop2_alcohol <-resid(lm(quality~volatile.acidity +log10_chlorides +total.sulfur.dioxide +log10_sulphates, data=redwine))
summary(lm(dtDrop2_alcohol~redwine$alcohol))$coef[2,3] #14.223

#keep: volatile.acidity, log10_chlorides, total.sulfur.dioxide, log10_sulphates, alcohol
model_3 <-lm(quality~volatile.acidity +log10_chlorides +total.sulfur.dioxide +log10_sulphates +alcohol, data=redwine)
summary(model_3)
plot(model_3)
anova(model_3)


########################### add new datapoints model 4 ############################################
##12 volatile acidity & total sulfur acidity !!
quality <- (redwine$quality)
log10_com1 <- log10(redwine$volatile.acidity*redwine$total.sulfur.dioxide)
cor(quality, redwine$volatile.acidity*redwine$total.sulfur.dioxide)##-0.2789
cor(quality, log10_com1) ## -0.315

##13 sulphates & alcohol !!
log10_com2 <- log10(redwine$sulphates*redwine$alcohol)
cor(quality, redwine$sulphates*redwine$alcohol)##0.413
cor(quality, log10_com2) ## 0.453

model_4<-lm(quality~.+log10_com1+log10_com2, data=redwine)
summary(model_4)
par(mfrow = c(2,2))
plot(model_4)
anova(model_4)


AIC(model_4)
