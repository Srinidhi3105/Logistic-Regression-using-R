affair <- read.csv(file.choose())
View(affair)
plot(affair)
attach(affair)
str(affair)

#model building
fit1 <- glm(affairs~.,data = affair)
summary(fit1)

prob1 <- predict(fit1,type="response")

logit <- glm(factor(affairs)~factor(age)+factor(yearsmarried)+factor(religiousness)+factor(education)+factor(occupation)+factor(rating)+gender+children,family = binomial,data = affair)
summary(logit)

#we have a problem with significant values of gender,occupation,education,children

sum(is.na(affair))

#remove the variables with no significance and build the model
logit1 <- glm(factor(affairs)~factor(age)+factor(yearsmarried)+factor(religiousness),family=binomial,data=affair)
summary(logit1)

exp(coef(logit1))

table(affair$affairs)

#confusion matrix table
prob <- predict(logit1,type=c("response"),affair)
prob
confusion <- table(prob>0.5,affair$affairs)
confusion

#model accuracy
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy

