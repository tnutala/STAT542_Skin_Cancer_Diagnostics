ben.asym = measureasymm.dir("benign")
mal.asym = measureasymm.dir("malignant")

ben.int = measureintensity.dir("benign")
mal.int = measureintensity.dir("malignant")

ben.blue = measureblue.dir("benign")
mal.blue = measureblue.dir("malignant")

ben.red = measurered.dir("benign")
mal.red = measurered.dir("malignant")

ben.bluevar = measurebluevar.dir("benign")
mal.bluevar = measurebluevar.dir("malignant")

ben.redvar = measureredvar.dir("benign")
mal.redvar = measureredvar.dir("malignant")

x = data.frame(intensity = c(ben.int, mal.int), 
               asymmetric = c(ben.asym, mal.asym), 
               blue = c(ben.blue, mal.blue), 
               red = c(ben.red, mal.red), 
               bluesd = sqrt(c(ben.bluevar, mal.bluevar)), 
               redsd = sqrt(c(ben.redvar, mal.redvar)),
               malignant = y)

fit = glm(malignant ~ ., data = x, family = "binomial")

pred = predict.glm(fit, x, type = "response") > .5
mean(pred == y)

n = nrow(x)
train = sample(1:n, .75 * n)
train.x = x[train,]
test.x = x[-train,]

fit = glm(malignant ~ ., data = train.x, family = "binomial")

pred = predict.glm(fit, test.x, type = "response") > .5
mean(pred == test.x$malignant)

