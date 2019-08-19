install.packages("rpart")
library(rpart)

loans<-read.csv("loans.csv")
loans<-loans[,c(-1,-2)]

set.seed(24)
sp<- sample(nrow(loans),0.7*nrow(loans))

train<-loans[sp,]
test<-loans[-sp,]


m <- rpart(default ~ .,
           data = train,
           method = "class")
p<-predict(m,test,type="class")

table(p,test$default)
