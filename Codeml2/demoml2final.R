## Load the test and train data
train_set = read.csv("ml2_train.csv", header = TRUE)
test_set = read.csv("ml2_test.csv", header = TRUE)


## Checking for NA values and their treatment by replacing with mean
sum(is.na(train_set$X2016.Deposits))
sum(is.na(train_set$X2010.Deposits))
sum(is.na(train_set$X2011.Deposits))
sum(is.na(train_set$X2012.Deposits))
sum(is.na(train_set$X2013.Deposits))
sum(is.na(train_set$X2014.Deposits))
sum(is.na(train_set$X2015.Deposits))

x2010.mean <- mean(train_set$X2010.Deposits, na.rm=TRUE)
train_set$X2010.Deposits[is.na(train_set$X2010.Deposits)] = x2010.mean

x2011.mean <- mean(train_set$X2011.Deposits, na.rm=TRUE)
train_set$X2011.Deposits[is.na(train_set$X2011.Deposits)] = x2011.mean

x2012.mean <- mean(train_set$X2012.Deposits, na.rm=TRUE)
train_set$X2012.Deposits[is.na(train_set$X2012.Deposits)] = x2012.mean

x2013.mean <- mean(train_set$X2013.Deposits, na.rm=TRUE)
train_set$X2013.Deposits[is.na(train_set$X2013.Deposits)] = x2013.mean

x2014.mean <- mean(train_set$X2014.Deposits, na.rm=TRUE)
train_set$X2014.Deposits[is.na(train_set$X2014.Deposits)] = x2014.mean

x2015.mean <- mean(train_set$X2015.Deposits, na.rm=TRUE)
train_set$X2015.Deposits[is.na(train_set$X2015.Deposits)] = x2015.mean


## Removal of outliers from the train set
remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

set.seed(101)
x <- train_set$X2016.Deposits
train_set$X2016.Deposits <- remove_outliers(x)

# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(train_set$X2016.Deposits)

x <- train_set$X2015.Deposits
train_set$X2015.Deposits <- remove_outliers(x)

# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(train_set$X2015.Deposits)

x <- train_set$X2014.Deposits
train_set$X2014.Deposits <- remove_outliers(x)

x <- train_set$X2013.Deposits
train_set$X2013.Deposits <- remove_outliers(x)

x <- train_set$X2012.Deposits
train_set$X2012.Deposits <- remove_outliers(x)

x <- train_set$X2011.Deposits
train_set$X2011.Deposits <- remove_outliers(x)

x <- train_set$X2010.Deposits
train_set$X2010.Deposits <- remove_outliers(x)

## Code for first model

# model = lm(formula = train_set$X2016.Deposits ~ train_set$X2015.Deposits + train_set$X2014.Deposits+ train_set$X2013.Deposits+
#            train_set$X2012.Deposits+ train_set$X2011.Deposits+ train_set$X2010.Deposits,
#            data = train_set)
# summary(model)
# 
# anova(model)
# par(mfrow=c(2,2))
# plot(model)


## Code for second model1 which our final model used for predictions
model1 = lm(formula = train_set$X2016.Deposits ~ train_set$X2015.Deposits + train_set$X2012.Deposits,
           data = train_set)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))

## Removal of outliers from the test set

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

x <- test_set$X2015.Deposits
test_set$X2015.Deposits <- remove_outliers(x)

# par(mfrow = c(1, 2))
# boxplot(x)
# boxplot(train_set$X2015.Deposits)
# par(mfrow = c(1, 1))

x <- test_set$X2014.Deposits
test_set$X2014.Deposits <- remove_outliers(x)

x <- test_set$X2013.Deposits
test_set$X2013.Deposits <- remove_outliers(x)

x <- test_set$X2012.Deposits
test_set$X2012.Deposits <- remove_outliers(x)

x <- test_set$X2011.Deposits
test_set$X2011.Deposits <- remove_outliers(x)

x <- test_set$X2010.Deposits
test_set$X2010.Deposits <- remove_outliers(x)


## Code to create the cluster for different ranges

## Cluster A
myfun  = function(x, na.rm = TRUE){
     subset(test_set, x>=20000 & x< 90000)
                       
}

x <- test_set$X2014.Deposits
cluster_A <- myfun(x)

x <- test_set$X2015.Deposits
cluster_A <- myfun(x)

x <- test_set$X2013.Deposits
cluster_A <- myfun(x)

x <- test_set$X2012.Deposits
cluster_A <- myfun(x)

x <- test_set$X2011.Deposits
cluster_A <- myfun(x)

x <- test_set$X2010.Deposits
cluster_A <- myfun(x)

# class(cluster_A)

predictions = predict.lm(model1, newdata=cluster_A)
# predictions
sum(is.na(predictions))
head(pred_A <- na.omit(predictions), n=30)


## Code for Cluster B
myfun  = function(x, na.rm = TRUE){
    subset(test_set, x<20000)
    
}

x <- test_set$X2014.Deposits
cluster_B <- myfun(x)

x <- test_set$X2015.Deposits
cluster_B <- myfun(x)

x <- test_set$X2013.Deposits
cluster_B <- myfun(x)

x <- test_set$X2012.Deposits
cluster_B <- myfun(x)

x <- test_set$X2011.Deposits
cluster_B <- myfun(x)

x <- test_set$X2010.Deposits
cluster_B <- myfun(x)

predictions2 = predict.lm(model1, cluster_B)
# prediction2
sum(is.na(predictions2))
head(pred_B <- na.omit(predictions2), n=30)


## Code for  Cluster C
myfun  = function(x, na.rm = TRUE){
    subset(test_set, x>=90000)
    
}

x <- test_set$X2014.Deposits
cluster_C <- myfun(x)

x <- test_set$X2015.Deposits
cluster_C <- myfun(x)

x <- test_set$X2013.Deposits
cluster_C <- myfun(x)

x <- test_set$X2012.Deposits
cluster_C <- myfun(x)

x <- test_set$X2011.Deposits
cluster_C <- myfun(x)

x <- test_set$X2010.Deposits
cluster_C <- myfun(x)

predictions3 = predict.lm(model1, cluster_C)
head(predictions3, n=30)


sum(is.na(predictions3))
pred_C <- na.omit(predictions3)
head(pred_C, n=30)


## Code to create final file
pred_A = as.data.frame(pred_A)
names(pred_A)= c("X2016 Deposit")
a=subset(pred_A, pred_A>=20000 & pred_A<90000)
b=subset(pred_A, pred_A<20000)
c=subset(pred_A, pred_A>=90000)

pred_B = as.data.frame(pred_B)
names(pred_B)= c("X2016 Deposit")
c=subset(pred_B, pred_B>=20000 & pred_A<90000)
d=subset(pred_B, pred_B<20000)
e=subset(pred_B, pred_B>=90000)

pred_C = as.data.frame(pred_C)
names(pred_C)= c("X2016 Deposit")
f=subset(pred_C, pred_C>=20000 & pred_A<90000)
g=subset(pred_C, pred_C<20000)
h=subset(pred_C, pred_C>=90000)

output=rbind(a,b,c,d,e,f,g,h)

write.csv(output,file = file.choose(new = T))
 







