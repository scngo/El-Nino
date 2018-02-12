elnino <- read.csv(file="elnino.csv",head=TRUE,sep=",")
elnino <- elnino[61:nrow(elnino),]
#fix(elnino)
#summary(elnino[,3:14])

#barplot(elnino$CTI[541:732], main="CTI Evolution (1895-1910)", xlab="Month",ylim=c(-2,3))
#barplot(elnino$CTI[1761:1932], main="CTI Evolution (1995-2010)", xlab="Month",ylim=c(-2,3))


# Logistic regression sign
elnino1 <- elnino[601:1782,3:9]
#elnino1$CTI <- elnino[604:1785, 3]
elnino1 <- na.omit(elnino1)
#cor(elnino1)
CTIsign <- rep(0, nrow(elnino1))
CTIsign[elnino1$CTI > 0] <- 1
elnino1 <- data.frame(elnino1, CTIsign)
elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

fit.glm <- glm(CTIsign ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, family = binomial)
summary(fit.glm)

probs <- predict(fit.glm, elnino1.train, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.train$CTIsign)

probs <- predict(fit.glm, elnino1.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.test$CTIsign)

# 2
elnino1 <- elnino[601:1782,3:9]
elnino1 <- na.omit(elnino1)
CTIsign <- rep(0, nrow(elnino1))
CTIsign[elnino1$CTI > 0.5] <- 1
CTIsign[elnino1$CTI < -0.5] <- 1
elnino1 <- data.frame(elnino1, CTIsign)
elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

fit.glm <- glm(CTIsign ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, family = binomial)
summary(fit.glm)

probs <- predict(fit.glm, elnino1.train, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.train$CTIsign)

probs <- predict(fit.glm, elnino1.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.test$CTIsign)

# 3
elnino2 <- na.omit(elnino)
CTIsign <- rep(0, nrow(elnino2))
CTIsign[elnino2$CTI > 0] <- 1
elnino2 <- data.frame(elnino2, CTIsign)

fit.glm <- glm(CTIsign ~ .-CTI, data = elnino2, family = binomial)
summary(fit.glm)

probs <- predict(fit.glm, elnino2, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino2$CTIsign)


# LDA
library(MASS)
elnino1 <- elnino[601:1782,3:9]
elnino1 <- na.omit(elnino1)
#cor(elnino1)
CTIsign <- rep(0, nrow(elnino1))
CTIsign[elnino1$CTI > 0.5] <- 1
CTIsign[elnino1$CTI < -0.5] <- -1
elnino1 <- data.frame(elnino1, CTIsign)
elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

fit.lda <- lda(CTIsign ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train)
fit.lda

pred.lda <- predict(fit.lda, elnino1.train)
table(pred.lda$class, elnino1.train$CTIsign)

pred.lda <- predict(fit.lda, elnino1.test)
table(pred.lda$class, elnino1.test$CTIsign)

# KNN
library(class)
elnino1 <- elnino[601:1782,3:9]
#elnino1$CTI <- elnino[602:1783, 3]
elnino1 <- na.omit(elnino1)
CTIsign <- rep(0, nrow(elnino1))
CTIsign[elnino1$CTI > 0.5] <- 1
CTIsign[elnino1$CTI < -0.5] <- -1
elnino1 <- data.frame(elnino1, CTIsign)
elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

train.X <- elnino1.train[, -1]
test.X <- elnino1.test[, -1]
train.Y <- elnino1.train$CTIsign
pred.knn <- knn(train.X, test.X, train.Y, k = 1)
table(pred.knn, elnino1.test$CTIsign)


# Linear regression
elnino1 <- elnino[601:1782,3:9]
elnino1 <- na.omit(elnino1)
elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

fit.lm <- lm(CTI ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1)
summary(fit.lm)

estCTI <- predict(fit.lm, elnino1.test, type = "response")
test.avg <- mean(elnino1.test$CTI)
r2 <- 1 - mean((estCTI - elnino1.test$CTI)^2) / mean((test.avg - elnino1.test$CTI)^2)
r2

# PCR
library(pls)
elnino1 <- elnino[601:1782,3:9]
elnino1 <- data.frame(elnino1, CTIfuture)
elnino1 <- na.omit(elnino1)

elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1175, ]

fit.pcr <- pcr(CTI ~  SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, elnino1.test, ncomp = 2)
mean((pred.pcr - elnino1.test$CTI)^2)

fit.pls <- plsr(CTI ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, elnino1.test, ncomp = 1)
mean((pred.pls - elnino1.test$CTI)^2)

test.avg <- mean(elnino1.test$CTI)
pcr.r2 <- 1 - mean((pred.pcr - elnino1.test$CTI)^2) / mean((test.avg - elnino1.test$CTI)^2)
pcr.r2
pls.r2 <- 1 - mean((pred.pls - elnino1.test$CTI)^2) / mean((test.avg - elnino1.test$CTI)^2)
pls.r2








# Change
ChangeCTI1 <- elnino$CTI[601:1782] - elnino$CTI[600:1781]
ChangeCTI2 <- elnino$CTI[602:1783] - elnino$CTI[601:1782]
plot(ChangeCTI1,ChangeCTI2)
fit.lm <- lm(ChangeCTI2 ~ ChangeCTI1)
summary(fit.lm)
abline(-0.2202424, 0.0008105)


# Logistic regression past data
# most significant -3 -12
elnino1 <- elnino[601:1782,3:9]
CTIfuture <- elnino[605:1786, 3]
elnino1 <- data.frame(elnino1, CTIfuture)
elnino1 <- na.omit(elnino1)

CTIchange <- rep(0, nrow(elnino1))
CTIchange[elnino1$CTIfuture - elnino1$CTI > 0] <- 1
elnino1 <- data.frame(elnino1, CTIchange)

elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1178, ]

fit.glm <- glm(CTIchange ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, family = binomial)
summary(fit.glm)

probs <- predict(fit.glm, elnino1.train, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.train$CTIchange)

probs <- predict(fit.glm, elnino1.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, elnino1.test$CTIchange)


# LDA change
library(MASS)
elnino1 <- elnino[601:1782,3:9]
CTIfuture <- elnino[605:1786, 3]
elnino1 <- data.frame(elnino1, CTIfuture)
elnino1 <- na.omit(elnino1)

CTIchange <- rep(0, nrow(elnino1))
CTIchange[elnino1$CTIfuture - elnino1$CTI > 0] <- 1
elnino1 <- data.frame(elnino1, CTIchange)

elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1175, ]

fit.lda <- lda(CTIchange ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train)
fit.lda

pred.lda <- predict(fit.lda, elnino1.train)
table(pred.lda$class, elnino1.train$CTIchange)

pred.lda <- predict(fit.lda, elnino1.test)
table(pred.lda$class, elnino1.test$CTIchange)


# KNN
library(class)
elnino1 <- elnino[601:1782,3:9]
CTIfuture <- elnino[602:1783, 3]
elnino1 <- data.frame(elnino1, CTIfuture)
elnino1 <- na.omit(elnino1)

CTIchange <- rep(0, nrow(elnino1))
CTIchange[elnino1$CTIfuture - elnino1$CTI > 0] <- 1
elnino1 <- data.frame(elnino1, CTIchange)

elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1175, ]

train.X <- elnino1.train[, -1]
test.X <- elnino1.test[, -1]
train.Y <- elnino1.train$CTIchange
pred.knn <- knn(train.X, test.X, train.Y, k = 1)
table(pred.knn, elnino1.test$CTIchange)


# PCR
library(pls)
elnino1 <- elnino[601:1782,3:9]
CTIfuture <- elnino[604:1785, 3]
elnino1 <- data.frame(elnino1, CTIfuture)
elnino1 <- na.omit(elnino1)

elnino1.train <- elnino1[1:900, ]
elnino1.test  <- elnino1[901:1175, ]

fit.pcr <- pcr(CTIfuture ~  SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, elnino1.test, ncomp = 4)
mean((pred.pcr - elnino1.test$CTIfuture)^2)

fit.pls <- plsr(CTIfuture ~ SOI + PDO + NAO + AMO + IOD + AO, data = elnino1.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, elnino1.test, ncomp = 2)
mean((pred.pls - elnino1.test$CTIfuture)^2)

test.avg <- mean(elnino1.test$CTIfuture)
pcr.r2 <- 1 - mean((pred.pcr - elnino1.test$CTIfuture)^2) / mean((test.avg - elnino1.test$CTIfuture)^2)
pcr.r2
pls.r2 <- 1 - mean((pred.pls - elnino1.test$CTIfuture)^2) / mean((test.avg - elnino1.test$CTIfuture)^2)
pls.r2

