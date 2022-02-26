df <- read.csv('C:/Users/DEVU/Desktop/ML/Data Files/2. ST Academy - Classification models resource files/House-Price.csv', header = TRUE)
summary(df)
uv <- 3*quantile(df$n_hot_rooms, .99)
lv <- .3*quantile(df$rainfall, 0.01)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv
df$rainfall[df$rainfall < lv] <- lv
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE)

summary(df$n_hos_beds)
summary(df$n_hot_rooms)
require(dummies)
df <- dummy.data.frame(data = df)

df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
df <- df[,-6:-9]
df<- df[,-8]
df<- df[,-13]

glm.fit =glm(Sold~price , data = df, family = binomial)
summary(glm.fit)

glm.fit =glm(Sold~. , data = df, family = binomial)
summary(glm.fit)

glm.proba = predict(glm.fit,type = "response")
glm.proba[1:10]

glm.pred = rep("No", 506)
glm.pred[glm.proba>0.5] ="Yes"

glm.pred[1:20]

table(glm.pred, df$Sold)

require('MASS')
lda.fit = lda(Sold~., data = df)
lda.fit
lda.pred = predict(lda.fit, df)
lda.pred$posterior

lda.class = lda.pred$class
table(lda.class, df$Sold)

sum(lda.pred$posterior[,1]>0.8)

qda.fit = qda(Sold~., data = df)
qda.pred = predict(qda.fit, df)
qda.pred$posterior
qda.class = qda.pred$class
table(qda.class, df$Sold)
sum(qda.pred$posterior[,1]>0.8)


require(caTools)

set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
train_set = subset(df,split==TRUE)
test_set = subset(df,split==FALSE)

train.fit = glm(Sold~., data = train_set,family = binomial)

test.prob =predict(train.fit,test_set,type = 'response')
test.pred = rep("NO",120)
test.pred[test.prob>0.5] = "YES"

table(test.pred,test_set$Sold)
