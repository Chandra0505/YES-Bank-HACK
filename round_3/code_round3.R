## Load the test and train data
setwd("~/YES Bank HACK/round_3")
train_set = read.csv("Yes_Bank_Train.csv", header = TRUE)
test_set = read.csv("Yes_Bank_Train_int.csv", header = TRUE)
sample_set = read.csv("sample_clusters.csv", header = TRUE)

str(train_set)
sum(is.na(train_set$credit_amount))

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .30), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

x <- train_set$credit_amount
train_set$credit_amount <- remove_outliers(x)

boxplot(train_set$credit_amount)
