
library("e1071")

gluc <- read.csv("datosglu.csv", skip = 0,
                 stringsAsFactors = FALSE)

## classification mode
# alternatively the traditional interface:
x <- subset(gluc, select = -Diagnostico)
y <- Diagnostico

y <- as.factor(y)
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:3,]

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)


svm_model_after_tune <- svm(x, y, kernel="radial", cost=100, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

predictions <- as.vector(pred)

write.csv(predictions, file = "mydata.csv")
