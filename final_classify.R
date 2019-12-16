ben.asym = measureasymm.dir("benign")
mal.asym = measureasymm.dir("malignant")

ben.int = measureintensity.dir("benign")
mal.int = measureintensity.dir("malignant")

ben.intvar = measureintensityvar.dir("benign")
mal.intvar = measureintensityvar.dir("malignant")

ben.blue = measureblue.dir("benign")
mal.blue = measureblue.dir("malignant")

ben.red = measurered.dir("benign")
mal.red = measurered.dir("malignant")

ben.bluevar = measurebluevar.dir("benign")
mal.bluevar = measurebluevar.dir("malignant")

ben.redvar = measureredvar.dir("benign")
mal.redvar = measureredvar.dir("malignant")

ben.edge = measureedge.dir("benign")
mal.edge = measureedge.dir("malignant")

y = numeric(length(list.files("benign")))
y = c(y, rep(1, length(list.files("malignant"))))

x = data.frame(intensity = c(ben.int, mal.int), 
               #intensitysd = sqrt(c(ben.intvar, mal.intvar)),
               asymmetric = c(ben.asym, mal.asym), 
               blue = c(ben.blue, mal.blue), 
               red = c(ben.red, mal.red), 
               bluesd = sqrt(c(ben.bluevar, mal.bluevar)), 
               redsd = sqrt(c(ben.redvar, mal.redvar)),
               edge = c(ben.edge, mal.edge),
               malignant = y)

fit = glm(malignant ~ ., data = x, family = "binomial")

pred = predict.glm(fit, x, type = "response") > .5
mean(pred == y)

n = nrow(x)
#train = sample(1:n, .75 * n)
train.x = x[train,]
test.x = x[-train,]
train.x.mat = model.matrix(~ . -1 - malignant, train.x)
test.x.mat = model.matrix(~ . -1 - malignant, test.x)

fit = glm(malignant ~ ., data = train.x, family = "binomial")

pred = predict.glm(fit, test.x, type = "response") > .5
mean(pred == test.x$malignant)

library(glmnet)

x.mat = model.matrix(~ . -1 - malignant, x)
fit.lasso = cv.glmnet(x.mat, y)
pred = predict.cv.glmnet(fit.lasso, x.mat) > .5
mean(pred == x$malignant)

fit.lasso = cv.glmnet(train.x.mat, train.x$malignant)
pred = predict.cv.glmnet(fit.lasso, test.x.mat) > .5
mean(pred == test.x$malignant)

library(e1071)
fit.svm = svm(malignant ~ ., data = x, type = "C-classification", kernel = "radial", scale = F, cost = 5)
pred = predict(fit.svm, x)
mean(pred == x$malignant)

fit.svm = svm(malignant ~ ., data = train.x, type = "C-classification", kernel = "radial", scale = F, cost = 5)
pred = predict(fit.svm, test.x)
mean(pred == test.x$malignant)

fit.bayes = naiveBayes(malignant ~ ., data = x)
pred = predict(fit.bayes, newdata = x, type = "raw")
mean((pred[,2] > pred[,1]) == x$malignant)

library(randomForest)
fit.rf = randomForest(train.x.mat, as.factor(train.x$malignant), ntree = 1000)
pred = predict(fit.rf, test.x.mat)
mean(pred == test.x$malignant)

library(caret)
set.seed(1)

inTrain = createDataPartition(y = as.factor(x$malignant), p = .75, list = F)
training = x[inTrain,]
testing = x[-inTrain,]
caret.rf.fit = train(as.factor(malignant) ~ ., data = training, method = "rf")
mean(predict(caret.rf.fit, testing) == testing$malignant)

caret.lasso.fit = train(factor(malignant) ~ ., data = training, method = "glmnet")
mean(predict(caret.lasso.fit, testing) == testing$malignant)
