# PREDICT 422 Practical Machine Learning
#Breanna Leander, Final Project


# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

library(ggplot2)
library(dplyr)
# load the data
charity <- read.csv(file.choose()) # load the "charity.csv" file

summary(charity)

# predictor transformations
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
charity.t$incm <- log(charity.t$incm)
charity.t$inca <- log(charity.t$inca)


#EDA plots of variables for DONR
attach(charity)
table(reg1, donr)   #65 donors
table(reg2, donr)   #1401 donors
table(reg3, donr)   #259 donors
table(reg4, donr)   #276 donors
#region 5 has 403 donors

ggplot(charity, aes(reg1, donr))+geom_count()
ggplot(charity, aes(reg2, donr))+geom_count()
ggplot(charity, aes(reg3, donr))+geom_count()
ggplot(charity, aes(reg4, donr))+geom_count()

ggplot(charity, aes(home, donr)) + geom_count()
table(home, donr)   #2928 homeowners that donated and 66 non homeowners that donated

table(chld, donr)
charity.t$donr2 <- ifelse(donr==1, "yes", "no")
ggplot() + geom_bar(aes(y=donr2, x=chld, fill=donr2), data=charity.t, stat="identity")

ggplot() + geom_bar(aes(y=donr2, x=hinc, fill=donr2), data=charity.t, stat="identity")
ggplot() + geom_bar(aes(y=donr2, x=genf, fill=donr2), data=charity.t, stat="identity")
ggplot() + geom_bar(aes(y=donr2, x=wrat, fill=donr2), data=charity.t, stat="identity")
ggplot() + geom_boxplot(aes(y=avhv, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=incm, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=inca, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=plow, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=npro, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=tgif, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=lgif, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=rgif, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=tdon, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=tlag, x=donr2), data=charity.t)
ggplot() + geom_boxplot(aes(y=agif, x=donr2), data=charity.t)

###############################
#Plots to show differences between parts
ggplot() + geom_boxplot(aes(y=damt, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=hinc, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=genf, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=wrat, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=avhv, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=incm, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=inca, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=plow, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=npro, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=tgif, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=lgif, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=rgif, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=tdon, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=tlag, x=part), data=charity.t)
ggplot() + geom_boxplot(aes(y=agif, x=part), data=charity.t)
#################################
#remember pairs and qqplots and hist() for histograms
pairs(~home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + donr, data = charity.t, subset = part!="test")
pairs(~reg1 + reg2 + reg3 + reg4 + tgif + lgif + rgif + tdon + tlag + agif + donr, data = charity.t, subset = part!="test")

#####################################
#Correlations
cor(charity.t[,2:23], method="pearson")

#Below variables are highly correlated with each other (>=.7)
#avhv/plow/incm/inca
#npro/tgif
#agif/rgif/lgif


#look for missing values
sum(is.na(charity.t$reg1))
sum(is.na(charity.t$reg2))
sum(is.na(charity.t$reg3))
sum(is.na(charity.t$reg4))
sum(is.na(charity.t$home))
sum(is.na(charity.t$chld))
sum(is.na(charity.t$hinc))
sum(is.na(charity.t$genf))
sum(is.na(charity.t$wrat))
sum(is.na(charity.t$avhv))
sum(is.na(charity.t$incm))
sum(is.na(charity.t$inca))
sum(is.na(charity.t$plow))
sum(is.na(charity.t$npro))
sum(is.na(charity.t$tgif))
sum(is.na(charity.t$lgif))
sum(is.na(charity.t$rgif))
sum(is.na(charity.t$tdon))
sum(is.na(charity.t$tlag))
sum(is.na(charity.t$agif))
sum(is.na(charity.t$donr))
sum(is.na(charity.t$damt))
#data is tidy - no missing values

qqnorm(charity.t$home)
qqnorm(charity.t$chld)
qqnorm(charity.t$hinc)
qqnorm(charity.t$avhv)
qqnorm(charity.t$incm)
qqnorm(charity.t$inca)
qqnorm(charity.t$plow)
qqnorm(charity.t$npro)
qqnorm(charity.t$tgif)
qqnorm(log(charity.t$tgif))  #should log transform this variable
qqnorm(charity.t$lgif)
qqnorm(log(charity.t$lgif))  #consider log transforming this variable
qqnorm(charity.t$rgif)
qqnorm(log(charity.t$rgif))   #consider log transformation
qqnorm(charity.t$tdon)
qqnorm(charity.t$tlag)
qqnorm(charity.t$agif)
qqnorm(log(charity.t$agif))  #consider log transformation


######transform further predictors for standardization and interaction terms
2007+3984+2018
#total of 8009 observations altogether
#2007 in the test file
#3984 in the training file
#2018 in the valid file
###
#####transform the variables that are skewed (qqplot)
charity.t$tgif <- log(charity.t$tgif)
charity.t$lgif <- log(charity.t$lgif)
charity.t$agif <- log(charity.t$agif)
charity.t$rgif <- log(charity.t$rgif)


####create a separate data frame based on charity.t with interaction terms
charity.t2 <- charity.t
charity.t2$avhvplowincm <- (charity.t2$avhv*charity.t2$plow*charity.t2$incm)
charity.t2$agifrgiflgif <- (charity.t2$agif*charity.t2$rgif*charity.t2$lgif)
charity.t2$nprotgif <- (charity.t2$npro*charity.t2$tgif)
qqnorm(charity.t2$avhvplowincm)
qqnorm(charity.t2$agifrgiflgif)
qqnorm(charity.t2$nprotgif)
# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

#############################################
#set up data for analysis using charity.t2 with interaction terms

charity.t2 <- charity.t2[,c(1:21,25:27,22:24)]
data.train2 <- charity.t2[charity.t2$part=="train",]
x.train2 <- data.train2[,2:24]
c.train2 <- data.train2[,25] # donr
n.train.c2 <- length(c.train2) # 3984
y.train2 <- data.train2[c.train2==1,26] # damt for observations with donr=1
n.train.y2 <- length(y.train2) # 1995

data.valid2 <- charity.t2[charity.t2$part=="valid",]
x.valid2 <- data.valid2[,2:24]
c.valid2 <- data.valid2[,25] # donr
n.valid.c2 <- length(c.valid2) # 2018
y.valid2 <- data.valid2[c.valid2==1,26] # damt for observations with donr=1
n.valid.y2 <- length(y.valid2) # 999

data.test2 <- charity.t2[charity.t2$part=="test",]
n.test2 <- dim(data.test2)[1] # 2007
x.test2 <- data.test2[,2:24]

x.train.mean2 <- apply(x.train2, 2, mean)
x.train.sd2 <- apply(x.train2, 2, sd)
x.train.std2 <- t((t(x.train2)-x.train.mean2)/x.train.sd2) # standardize to have zero mean and unit sd
apply(x.train.std2, 2, mean) # check zero mean
apply(x.train.std2, 2, sd) # check unit sd
data.train.std.c2 <- data.frame(x.train.std2, donr=c.train2) # to classify donr
data.train.std.y2 <- data.frame(x.train.std2[c.train2==1,], damt=y.train2) # to predict damt when donr=1

x.valid.std2 <- t((t(x.valid2)-x.train.mean2)/x.train.sd2) # standardize using training mean and sd
data.valid.std.c2 <- data.frame(x.valid.std2, donr=c.valid2) # to classify donr
data.valid.std.y2 <- data.frame(x.valid.std2[c.valid2==1,], damt=y.valid2) # to predict damt when donr=1

x.test.std2 <- t((t(x.test2)-x.train.mean2)/x.train.sd2) # standardize using training mean and sd
data.test.std2 <- data.frame(x.test.std2)

##################################################################
#plots with data.train.stdy2 to look at relationships with damt

plot(data.train.std.y2$avhv, data.train.std.y2$damt)
plot(data.train.std.y2$hinc, data.train.std.y2$damt)
plot(data.train.std.y2$wrat, data.train.std.y2$damt)
plot(data.train.std.y2$incm, data.train.std.y2$damt)
plot(data.train.std.y2$inca, data.train.std.y2$damt)
plot(data.train.std.y2$npro, data.train.std.y2$damt)
plot(data.train.std.y2$tgif, data.train.std.y2$damt)
plot(data.train.std.y2$lgif, data.train.std.y2$damt)
plot(data.train.std.y2$rgif, data.train.std.y2$damt)
plot(data.train.std.y2$tdon, data.train.std.y2$damt)
plot(data.train.std.y2$tlag, data.train.std.y2$damt)
plot(data.train.std.y2$agif, data.train.std.y2$damt)
plot(data.train.std.y2$avhvplowincm, data.train.std.y2$damt)
plot(data.train.std.y2$agifrgiflgif, data.train.std.y2$damt)
plot(data.train.std.y2$nprotgif, data.train.std.y2$damt)
hist(data.train.std.y2$damt)
hist(data.valid.std.y2$damt)
cor(charity.t[,2:23], method="pearson")
qqnorm(data.train.std.y2$damt)
qqnorm(data.valid.std.y2$damt)
cor(data.train.std.y2$damt,data.train.std.y2[,1:22], method="pearson")

##### CLASSIFICATION MODELING #####################################

# linear discriminant analysis

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1363.0 11643.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 647  8
#              1 372 991
# check n.mail.valid = 372+991 = 1363
# check profit = 14.5*991-2*1363 = 11643.50

# logistic regression

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1330.0 11637

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 675  13
#              1 344 986


############
###Try Quadratic Discriminant Analysis (QDA)
model.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
# 1396, $11,229.5

cutoff.qda1 <- sort(post.valid.qda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda1 <- ifelse(post.valid.qda1>cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table
#               c.valid
#chat.valid.qda1   0   1
#              0 590  32
#              1 429 967

###########################################################################3
#########GLM WITH CROSS VALIDATION################3
library(boot)
model.glmcv <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))
cv.err=cv.glm(data.train.std.c, model.glmcv)
cv.err$delta
#0.08505642

post.valid.glmcv <- predict(model.glmcv, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.glmcv <- cumsum(14.5*c.valid[order(post.valid.glmcv, decreasing=T)]-2)
plot(profit.glmcv) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.glmcv) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.glmcv)) # report number of mailings and maximum profit
# 1330.0 11637

cutoff.glmcv <- sort(post.valid.glmcv, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.glmcv <- ifelse(post.valid.glmcv>cutoff.glmcv, 1, 0) # mail to everyone above the cutoff
table(chat.valid.glmcv, c.valid) # classification table
#               c.valid
#chat.valid.glmcv   0   1
#              0 675  13
#              1 344 986

###########################################
###fit a classification tree
library(tree)
data.train.std.c$donr2 <- as.factor(ifelse(data.train.std.c$donr==1, "Yes", "No"))
model.tree = tree(donr2 ~ .-donr, data.train.std.c)
summary(model.tree)
plot(model.tree)
text(model.tree, pretty=0)

post.valid.tree <- predict(model.tree, data.valid.std.c, type="class")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.tree <- cumsum(14.5*c.valid[order(post.valid.tree, decreasing=T)]-2)
plot(profit.tree) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree)) # report number of mailings and maximum profit
# 1168.0 11149.00
#############################
####Bagging
library(randomForest)
set.seed(1)
model.randomForest <- randomForest(donr2~.-donr, data=data.train.std.c, mtry=20, importance=TRUE)
model.randomForest

post.valid.rf <- predict(model.randomForest, data.valid.std.c, type="class")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.rf <- cumsum(14.5*c.valid[order(post.valid.rf, decreasing=T)]-2)
plot(profit.rf) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf))
#1035, $11052.5
#######################################################################
####True Random Forest with mtry=6
set.seed(1)
model.randomForest1 <- randomForest(donr2~.-donr, data=data.train.std.c, mtry=6, importance=TRUE)
model.randomForest1

post.valid.rf1 <- predict(model.randomForest1, data.valid.std.c, type="class")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.rf1 <- cumsum(14.5*c.valid[order(post.valid.rf1, decreasing=T)]-2)
plot(profit.rf1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf1))
#1054, $11,232

###############################################################################
#True Random Forest with mtry=12
set.seed(1)
model.randomForest2 <- randomForest(donr2~.-donr, data=data.train.std.c, mtry=12, importance=TRUE)
model.randomForest2

post.valid.rf2 <- predict(model.randomForest2, data.valid.std.c, type="class")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.rf2 <- cumsum(14.5*c.valid[order(post.valid.rf2, decreasing=T)]-2)
plot(profit.rf2) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf2))
#1055, $11,186.5

###############################################################################
####True Random Forest with mtry=3
set.seed(1)
model.randomForest3 <- randomForest(donr2~.-donr, data=data.train.std.c, mtry=3, importance=TRUE)
model.randomForest3

post.valid.rf3 <- predict(model.randomForest3, data.valid.std.c, type="class")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.rf3 <- cumsum(14.5*c.valid[order(post.valid.rf3, decreasing=T)]-2)
plot(profit.rf3) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf3) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf3))
#1072, $11,196

###############################################################################
####Boosting
library(gbm)
set.seed(1)
model.boosting <- gbm(donr~.-donr2, data=data.train.std.c, distribution="bernoulli", n.trees=5000, interaction.depth=4)
model.boosting
summary(model.boosting)
post.valid.boosting <- predict(model.boosting, data.valid.std.c, n.trees=5000, type="response")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.boosting <- cumsum(14.5*c.valid[order(post.valid.boosting, decreasing=T)]-2)
plot(profit.boosting) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boosting) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boosting))
#1274, $11,836.00

###############################################################################
####Boosting with interaction depth of 6 and shrinkage to 0.2
set.seed(1)
model.boosting2 <- gbm(donr~.-donr2, data=data.train.std.c, distribution="bernoulli", n.trees=5000, interaction.depth=6, shrinkage=0.01, verbose=FALSE)
model.boosting2
summary(model.boosting2)
post.valid.boosting2 <- predict(model.boosting2, data.valid.std.c, n.trees=5000, type="response")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.boosting2  <- cumsum(14.5*c.valid[order(post.valid.boosting2, decreasing=T)]-2)
plot(profit.boosting2) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boosting2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boosting2))
#1245, $11,879.50
post.valid.boosting2 <- ifelse (post.valid.boosting2 > 0.5,1,0)
confusionMatrix(post.valid.boosting2, data.valid.std.c$donr)
#Accuracy = 90.98% 
###############################################################################
#XGBoost
install.packages("xgboost")
library(xgboost)
labels <- data.train.std.c$donr 
ts_label <- data.valid.std.c$donr
new_tr <- model.matrix(~.+0,data = data.train.std.c[,1:20]) 
new_ts <- model.matrix(~.+0,data = data.valid.std.c[,1:20])
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dvalid <- xgb.DMatrix(data = new_ts,label=ts_label)
set.seed(1)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 36, watchlist = list(val=dvalid,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
xgbpred <- predict (xgb1,dvalid)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
install.packages("caret")
library(caret)
confusionMatrix (xgbpred, ts_label)
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])
#Accuracy = 90.88%

profit.xgboost  <- cumsum(14.5*c.valid[order(xgbpred, decreasing=T)]-2)
plot(profit.xgboost) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.xgboost) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.xgboost))
#1063, $11,489.50
###############################################################################
#XGBoost with different parameters

set.seed(1)
params1 <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv1 <- xgb.cv( params = params1, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
xgb11 <- xgb.train (params = params1, data = dtrain, nrounds = 17, watchlist = list(val=dvalid,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
xgbpred1 <- predict (xgb11,dvalid)
xgbpred1 <- ifelse (xgbpred1 > 0.5,1,0)

confusionMatrix (xgbpred1, ts_label)
mat1 <- xgb.importance (feature_names = colnames(new_tr),model = xgb11)
xgb.plot.importance (importance_matrix = mat[1:20])

###############################################################################
####Boosting with interaction depth of 6 and shrinkage to 0.2 with interaction parameters
set.seed(1)
model.boostingnew <- gbm(donr~., data=data.train.std.c2, distribution="bernoulli", n.trees=5000, interaction.depth=6, shrinkage=0.01, verbose=FALSE)
model.boostingnew
summary(model.boostingnew)
post.valid.boostingnew <- predict(model.boostingnew, data.valid.std.c2, n.trees=5000, type="response")
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.boostingnew  <- cumsum(14.5*c.valid[order(post.valid.boostingnew, decreasing=T)]-2)
plot(profit.boostingnew) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boostingnew) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boostingnew))
#1252, $11,880.00
post.valid.boostingnew <- ifelse (post.valid.boostingnew > 0.5,1,0)
confusionMatrix(post.valid.boostingnew, data.valid.std.c2$donr)
#Accuracy = 90.78% 

plot(model.boostingnew)
text(model.boostingnew, pretty=0)
###########################################################################
####try support vector machines
library(e1071)
set.seed(1)
data.train.std.c2$donr2 <- ifelse(data.train.std.c2$donr==1, "yes", "no")
data.train.std.c2$donr2 <- as.factor(data.train.std.c2$donr2)
model.svmfit <- svm(donr2~ .-donr, data=data.train.std.c2, kernel="radial", gamma=1, cost=1)
model.svmfit
summary(model.svmfit)
plot(model.svmfit, data.train.std.c2)
post.valid.svmfit <- predict(model.svmfit, data.valid.std.c2)

profit.svmfit  <- cumsum(14.5*c.valid[order(post.valid.svmfit, decreasing=T)]-2)
plot(profit.svmfit) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svmfit) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svmfit))
#2017, $10,451.5
####################################################################
###need to try the tune.out on page 361 with this model and think through which is best
set.seed(1)
tune.out <- tune(svm, donr2~.-donr,data=data.train.std.c2, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 10)))
summary(tune.out)
bestsvmmod <- tune.out$best.model
post.valid.bestsvm <- predict(bestsvmmod, data.valid.std.c2)
profit.bestsvm <- cumsum(14.5*c.valid[order(post.valid.bestsvm, decreasing=T)]-2)
plot(profit.bestsvm) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.bestsvm) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.bestsvm))
#1823, $10,535 using a cost of 0.01 per the 10 fold cross validation tuning

#########################################################################
#Support Vector Machine with radial kernel
set.seed(1)
tune.out2 <- tune(svm, donr2~.-donr,data=data.train.std.c2, kernel="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 10)))
summary(tune.out2)
bestsvmmod2 <- tune.out2$best.model
post.valid.bestsvm2 <- predict(bestsvmmod2, data.valid.std.c2)
profit.bestsvm2 <- cumsum(14.5*c.valid[order(post.valid.bestsvm2, decreasing=T)]-2)
plot(profit.bestsvm2) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.bestsvm2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.bestsvm2))
#1066, $11,106.5 using a cost of 5 per the 10 fold cross validation tuning
#####################################################################################33
# select model.boostingnew since it has maximum profit in the validation sample
data.test.std$avhvplowincm <- data.test.std$avhv*data.test.std$plow*data.test.std$incm
data.test.std$agifrgiflgif <- data.test.std$agif*data.test.std$rgif*data.test.std$lgif
data.test.std$nprotgif <- data.test.std$npro*data.test.std$tgif
post.test <- predict(model.boostingnew, data.test.std, n.trees=5000, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.boosting2)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set
n.mail.test

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1702  305
# based on this model we'll mail to the 305 highest posterior probabilities

# See below for saving chat.test into a file for submission



##### PREDICTION MODELING for Donor Amount (DAMT) ######

# Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.556378
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.161215

###########
#try model with interaction terms
model.ls_1 <- lm(damt ~ .,data.train.std.y2)
summary(model.ls_1)
pred.valid.ls_1 <- predict(model.ls_1, newdata = data.valid.std.y2) # validation predictions
mean((y.valid - pred.valid.ls_1)^2) # mean prediction error
# 1.518835
sd((y.valid - pred.valid.ls_1)^2)/sqrt(n.valid.y) # std error
# 0.1607758

###########
# drop wrat and inca and tlag for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + plow + +npro+ tgif + lgif + rgif + tdon + agif, 
                data.train.std.y2)
summary(model.ls2)
pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y2) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.557404
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1608381

##########
#try decision trees/boosting/bagging/gams/ridge regression/lasso???

#ridge regression
library(glmnet)
grid=10^seq(10,-2, length=100)
x=model.matrix(damt~., data.train.std.y2)
y=data.train.std.y2$damt
model.ridge <- glmnet(x, y, alpha=0, lambda=grid)
model.ridge$lambda
####need to predict and determine mean prediction error
x2=model.matrix(damt~., data.valid.std.y2)
y2=data.valid.std.y2$damt
prediction.ridge <- predict(model.ridge, s=0.000, newx=x2)
mean((y.valid - prediction.ridge)^2) # mean prediction error
#1.763378 when s=4, 1.525118 when s is 0 or 0.001



###########################
#Decision Tree attempt

set.seed(1)
model.tree.damt <- tree(damt~., data.train.std.y2)
summary(model.tree.damt)  #10 terminal nodes
cv.treedamt <- cv.tree(model.tree.damt)  #chooses 10 variables
plot(cv.treedamt$size, cv.treedamt$dev, type="b")
prediction.treedamt <- predict(model.tree.damt, newdata=data.valid.std.y2)
mean((y.valid - prediction.treedamt)^2)
#2.20218

##################################
#try decision trees with boosting - after playing with parameters, found that default shrinkage, 10000 trees and interaction depth of 4 was optimal
set.seed(1)
model.boostdamt <- gbm(damt~., data=data.train.std.y2, distribution="gaussian", n.trees=10000, interaction.depth=4, shrinkage=0.001)
summary(model.boostdamt)
prediction.boostdamt <- predict(model.boostdamt, newdata=data.valid.std.y2, n.trees=10000)
mean((y.valid - prediction.boostdamt)^2)
#1.374184 MPE
###################################################
##try randomForest with bagging
set.seed(1)
model.rfbagged <- randomForest(damt~., data=data.train.std.y2, mtry=22, importance=TRUE)
model.rfbagged
prediction.rfbagged <- predict(model.rfbagged, newdata=data.valid.std.y2)
mean((y.valid - prediction.rfbagged)^2)
#1.699393 MPE
#######################################################
#####try partial least squares with cross validation 
set.seed(1)
library(pls)
model.plscv <- plsr(damt~., data=data.train.std.y2, scale=TRUE, validation="CV")
summary(model.plscv)
validationplot(model.plscv, val.type="MSEP")
#15 comps results in the lowest cross validation error of 1.171
prediction.plscv <- predict(model.plscv, data.valid.std.y2, ncomp=15)
mean((y.valid-prediction.plscv)^2)
#1.517703 MPE

###############################################
#try principal components
set.seed(2)
model.pcrcv <- pcr(damt~., data=data.train.std.y2, scale=TRUE, validation="CV")
summary(model.pcrcv)
validationplot(model.pcrcv, val.type="MSEP") #lowest occurs with 21 comps
prediction.pcrcv <- predict(model.pcrcv, data.valid.std.y2, ncomp=21)
mean((y.valid-prediction.pcrcv)^2)
#1.55937 MPE  didn't reduce dimensions
#################################
####try K Nearest Neighbors
library(class)
set.seed(1)
data.train.knn <- data.train.std.y[,1:21]
data.valid.knn <- data.valid.std.y[,1:21]
train.damt = as.numeric(data.train.knn$damt)
data.valid.knn$damt <- as.numeric(data.valid.knn$damt)
model.knn1 <- knnreg(data.train.knn,data.valid.knn, train.damt, k=3) 
prediction.knn <- predict(model.knn1, newdata=data.valid.knn)
######################################


# Results

# MPE           Model
# 1.56339       LS1 (no interaction terms)
# 1.515914      LS_1 (interaction terms)
# 1.564612      LS2  (drop wrat inca and tlag)
# 1.525595      ridge (with s=0 or s=0.001)
# 1.518047      partial least squares with cross validation
# 1.373821      Boosted Decision Tree with ntrees=10,000, interaction depth 4 and shrinkage of 0.001

#################
#Retrain the models on the entire training and validation sets
data.full.std.fordamt <- rbind(data.train.std.y2, data.valid.std.y2)
data.full.std.fordonr <- rbind(data.train.std.c2[,1:24], data.valid.std.c2)
set.seed(1)
model.boostdamt2 <- gbm(damt~., data=data.full.std.fordamt, distribution="gaussian", n.trees=10000, interaction.depth=4, shrinkage=0.001)

# select model.boostdamt since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.boostdamt2, n.trees=10000,newdata = data.test.std2) # test predictions

####adjust the donr predictions to train on the full train and validation set
# select model.boostingnew since it has maximum profit in the validation sample
model.boostingnew2 <- gbm(donr~., data=data.full.std.fordonr, distribution="bernoulli", n.trees=5000, interaction.depth=6, shrinkage=0.01, verbose=FALSE)

post.test <- predict(model.boostingnew2, data.test.std2, n.trees=5000, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.boostingnew2)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set
n.mail.test

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)


############################################################################
############################################################################
############################################################################
# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="BJL_2.csv", row.names=FALSE) # use your initials for the file name

