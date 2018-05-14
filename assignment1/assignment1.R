# Code for Assignment1 STAT4038 - Dingying Li
moorhen<-read.csv('moorhen.csv',header = T)
attach(moorhen)

# Q1 A Plot shield against weight

plot(Weight,Shield,xlab='Weight(mg)', ylab='Shield(mm^2)', main='Relationship between Shield and Weight')
identify(Weight,Shield)

# Q1 B
cor.test(Weight, Shield)
#cor.test(Shield, Weight)

# Q1 C
log(Weight)
log(Shield)
plot(log(Weight), log(Shield), main='Relationship between log(Shield) and log(Weight)')
identify(log(Weight), log(Shield))

#plot(log(Weight[-c(27)]), log(Shield[-c(27)]))
cor.test(log(Weight), log(Shield)) # t = 1.9709, max!
cor.test((Weight), log(Shield))
cor.test(log(Weight), (Shield))
cor.test(sqrt(Weight), sqrt(Shield))
cor.test((Weight), sqrt(Shield))
cor.test(sqrt(Weight), (Shield))

# Q1 D
moorhen.lm <-lm(log(Shield)~log(Weight))
summary(moorhen.lm)
abline(moorhen.lm$coefficients)
plot(moorhen.lm, which=1)
plot(moorhen.lm, which=2)
plot(moorhen.lm, which=4)
barplot(hat(log(Weight)), main='Leverage plot of hat(log(Weight))')

plot(log(Weight), moorhen.lm$residuals)

# Q1 E
anova(moorhen.lm)

# Q1 F
summary(moorhen.lm)

# Q1 G
moorhen_non.lm <- lm(Shield ~ Weight)
plot(Weight,Shield, xlim=c(0,800), ylim=c(0,550), main='Relationship between Shield and Weight', xlab='Weight(mg)', ylab='Shield(mm^2)')
arr <- c(1:800)
pred <- predict(moorhen.lm, newdata=data.frame(Weight=arr))
lines(arr,exp(pred[order(pred)]),col='red')
abline(moorhen_non.lm$coefficients, col='blue', lty=2)
legend(0,550,legend=c('Transformed SLR','Untransformed SLR'),
       col=c('red','blue'), lty=1:2, box.lty=1, cex=0.8)


#Q2 A
fat<-read.csv('fat.csv',header=TRUE)
attach(fat)
plot(BMI,body.fat,main='Relationship between body.fat and BMI')
cor.test(BMI,body.fat)

#Q2 B
fat.lm<- lm(body.fat~BMI)
plot(fat.lm)
plot(fat.lm, which=1)
plot(fat.lm, which=2)
plot(fat.lm, which=4) # cook's distance


#Q2 C
fat.log.lm <- lm(body.fat~log(BMI))
plot(fat.log.lm, which=1)
plot(fat.log.lm, which=2)
plot(fat.log.lm, which=4)
fat.loglog.lm <- lm(log(body.fat) ~ log(BMI))
(body.fat)

#Q2 D
anova(fat.log.lm)
summary(fat.log.lm)

#Q2 E
plot(log(BMI),body.fat,main='Relationship between body.fat and log(BMI)',xlim=c(2.7,3.9))

log_BMI = log(BMI)
fat_log.lm <- lm(body.fat~log_BMI)
abline(fat.log.lm$coefficients)
bmi_predict <- c(17.25, 21.75, 27.5, 32.5)
bmi_log <- log(bmi_predict)
confidence <- predict(fat_log.lm, 
newdata=data.frame(log_BMI=bmi_log), interval='confidence')
confidence

arr_bmi <- c(15:50)
confidence_plot <- predict(fat_log.lm,
newdata=data.frame(log_BMI=log(arr_bmi)),interval='confidence')
prediction_plot <- predict(fat_log.lm,
newdata=data.frame(log_BMI=log(arr_bmi)),interval='prediction')
lines(log(arr_bmi),prediction_plot[,1],lty=1)
lines(log(arr_bmi),prediction_plot[,2],lty=2)
lines(log(arr_bmi),prediction_plot[,3],lty=2)
lines(log(arr_bmi),confidence_plot[,2],lty=3)
lines(log(arr_bmi),confidence_plot[,3],lty=3)
abline(v=log(bmi_predict),lty=4)
legend(2.7,45,legend=c('SLR model','prediction interval','confidence interval','predicted values'), 
       col='black',lty=c(1,2,3,4), box.lty=1, cex=0.8)

