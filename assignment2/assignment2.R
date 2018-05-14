# R code for assignment2 - Dingying Li
# Q1(a)

moorhen <- read.csv('moorhen.csv', header=T)
attach(moorhen)

pairs(moorhen, main='Scatterplot matrix')
cor(moorhen)

# Q1(b)
moorhen.lm <- lm(Shield ~ Weight + Stern + Hb + TandT + Adult)
moorhen.lm
plot(moorhen.lm, which=1) # residual vs fitted plot

# Q1(c)
moorhen.loglm <- lm(log(Shield) ~ Weight + Stern + Hb + TandT + Adult)
moorhen.loglm
plot(moorhen.loglm, which=1) # residual vs fitted plot


# Q1(d)
anova(moorhen.loglm)
summary(moorhen.loglm)

moorhen.loglm_a <- lm(log(Shield) ~ Stern + Adult + cbind(Weight, Hb, TandT))
anova(moorhen.loglm_a)
moorhen.loglm_b <- lm(log(Shield) ~ Stern + Adult + Weight + cbind(Hb, TandT))
anova(moorhen.loglm_b)
moorhen.loglm_c <- lm(log(Shield) ~ Stern + Adult + Weight + Hb + TandT)
anova(moorhen.loglm_c)


# Q1(e)
moorhen.e_lm <- lm(log(Shield) ~ Adult + Stern)
summary(moorhen.e_lm)
plot(fitted(moorhen.e_lm), rstandard(moorhen.e_lm), xlab="Fitted values", ylab="Internally studentised residuals", main="Internally studentised residuals vs Fitted values")
identify(fitted(moorhen.e_lm), rstandard(moorhen.e_lm))
abline(0,0, lty=2)

plot(moorhen.e_lm, which=2) # normal Q-Q
plot(moorhen.e_lm, which=4) # cook's distance

# Q1(f)
plot(Stern[Adult==1], Shield[Adult==1], pch=1, xlim=c(57,70), ylim=c(50,550), xlab="Stern",
     ylab="Shield", main="Shield vs Stern")
points(Stern[Adult==0], Shield[Adult==0], pch=2)
pred_adult <- predict(moorhen.e_lm, data.frame(Adult=1, Stern=c(570:700)/10))
pred_juvenile <- predict(moorhen.e_lm, data.frame(Adult=0, Stern=c(570:700)/10))
lines(x=c(570:700)/10, y=exp(pred_adult), lty=1)
lines(x=c(570:700)/10, y=exp(pred_juvenile), lty=2)
legend(57,550,c('adult','juvenile', 'prediction for adult', 'prediction for juvenile'),pch=c(1,2,NA,NA),lty=c(NA,NA,1,2),cex=0.6)

# Q1(g)
summary(moorhen.e_lm)
interval <- confint(moorhen.e_lm, "Adult")
interval
exp(interval)

# Q2(a)
fat <- read.csv('fat.csv', header=T) 
attach(fat)
# correct case 42: replace height to 69.5, rather than 29.5
height[42]
height[42] <- 69.5
height[c(40:50)]

# Q2(b)
vif <- function(xmatrix) {
  if (class(xmatrix) == "matrix" | class(xmatrix) == "data.frame")
    diag(solve(cor(xmatrix))) 
  else  
    diag(solve(cor(model.matrix(xmatrix)[,-1])))
  # assuming a linear model object, if not a matrix
}

pairs(~body.fat+age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+bicep+forearm+wrist)
# use external library corrplot to get better visualization
C <- cor(data.frame(body.fat,age,weight,height,neck,chest,abdomen,hip,thigh,knee,ankle,bicep,forearm,wrist))
C
library(corrplot)
corrplot(C, method='circle')

pairs(body.fat+age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+bicep+forearm+wrist)
pairs(~log(body.fat+1)+log(age)+log(weight)+log(height)+log(neck)+log(chest)+log(abdomen)+log(hip)+log(thigh)+log(knee)+log(ankle)+log(bicep)+log(forearm)+log(wrist))

# Choose Model, change 'wrist' to any body measurement and run following 3 lines
test.lm <- lm(log(body.fat+1) ~log(age)+log(weight)+log(height)+log(wrist))
vif(test.lm)
anova(test.lm)

fat.lm <- lm(log(body.fat+1) ~log(age)+log(weight)+log(height)+log(wrist))
anova(fat.lm)
summary(fat.lm)
vif(fat.lm)

plot(fitted(fat.lm), rstandard(fat.lm), 
     xlab="Fitted values", ylab="Internally studentised residuals", 
     main="Internally studentised residuals vs Fitted values")
identify(fitted(fat.lm), rstandard(fat.lm))
abline(0, 0, lty=2)
plot(fat.lm, which=2)
plot(fat.lm, which=4)

# Q2(c)
body.fat.r <- body.fat[c(-39,-172,-182)]
age.r <- age[c(-39,-172,-182)]
weight.r <- weight[c(-39,-172,-182)]
height.r <- height[c(-39,-172,-182)]
wrist.r <- wrist[c(-39,-172,-182)]

fat.lm_c0 <- lm(log(body.fat.r+1) 
               ~log(age.r)+log(weight.r)+
                 log(height.r)+log(wrist.r))
anova(fat.lm_c0)
summary(fat.lm_c0)
vif(fat.lm_c0)
plot(fitted(fat.lm_c0), rstandard(fat.lm_c0))

body.fat.r <- body.fat[c(-39)]
age.r <- age[c(-39)]
weight.r <- weight[c(-39)]
height.r <- height[c(-39)]
wrist.r <- wrist[c(-39)]

fat.lm_c1 <- lm(body.fat.r~age.r+log(weight.r)+log(height.r)+log(wrist.r))
anova(fat.lm_c1)
vif(fat.lm_c1)
plot(fitted(fat.lm_c1), rstandard(fat.lm_c1), 
     xlab="Fitted values", ylab="Internally studentised residuals", 
     main="Internally studentised residuals vs Fitted values")
identify(fitted(fat.lm_c1), rstandard(fat.lm_c1))
abline(0, 0, lty=2)
plot(fat.lm_c1, which=2)
plot(fat.lm_c1, which=4)

fat.lm_c2 <- lm(body.fat.r
                ~age.r+weight.r+
                  height.r+wrist.r)
anova(fat.lm_c2)
vif(fat.lm_c2)
plot(fitted(fat.lm_c2), rstandard(fat.lm_c2))

# Q2(d)
anova(fat.lm_c1)
summary(fat.lm_c1)

# Q2(e)
length(weight[BMI<=18.5])
length(weight[BMI>18.5 & BMI<=25])
length(weight[BMI>25 & BMI<=30])
length(weight[BMI>30])

avg.weight.un <- mean(weight[BMI<=18.5]) # underweight
avg.weight.no <- mean(weight[BMI>18.5 & BMI<=25]) # normal
avg.weight.ov <- mean(weight[BMI>25 & BMI<=30]) # overweight
avg.weight.ob <- mean(weight[BMI>30]) # obese

avg.age.un <- mean(age[BMI<=18.5]) # underweight
avg.age.no <- mean(age[BMI>18.5 & BMI<=25]) # normal
avg.age.ov <- mean(age[BMI>25 & BMI<=30]) # overweight
avg.age.ob <- mean(age[BMI>30]) # obese

avg.height.un <- mean(height[BMI<=18.5]) # underweight
avg.height.no <- mean(height[BMI>18.5 & BMI<=25]) # normal
avg.height.ov <- mean(height[BMI>25 & BMI<=30]) # overweight
avg.height.ob <- mean(height[BMI>30]) # obese

avg.wrist.un <- mean(wrist[BMI<=18.5]) # underweight
avg.wrist.no <- mean(wrist[BMI>18.5 & BMI<=25]) # normal
avg.wrist.ov <- mean(wrist[BMI>25 & BMI<=30]) # overweight
avg.wrist.ob <- mean(wrist[BMI>30]) # obese

avg.weight <- c(avg.weight.un,
                avg.weight.no,
                avg.weight.ov,
                avg.weight.ob)

avg.age <- c(avg.age.un,
             avg.age.no,
             avg.age.ov,
             avg.age.ob)

avg.height <- c(avg.height.un,
                avg.height.no,
                avg.height.ov,
                avg.height.ob)

avg.wrist <- c(avg.wrist.un,
               avg.wrist.no,
               avg.wrist.ov,
               avg.wrist.ob)

predict(fat.lm_c1, newdata=data.frame(age.r=avg.age, 
                                      weight.r=avg.weight, 
                                      height.r=avg.height,
                                      wrist.r=avg.wrist), interval="confidence")


