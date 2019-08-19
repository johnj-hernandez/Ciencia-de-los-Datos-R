library(mlbench);library(caret)
datos<-data("Sonar")

# Fit a model with a deeper tuning grid
model <- train(Class~., data = Sonar,
               method = "ranger", tuneLength = 10)

#for knn
model <- train(Class~., data = Sonar,
               method = "knn", tuneLength = 10)


# Plot the results
plot(model)

install.packages("ranger")
# Define a custom tuning grid
myGrid <- data.frame(
  .mtry = c(11,13,16),
  .splitrule = "extratrees",
  .min.node.size = 5
)
# Fit a model with a custom tuning grid
#el method es el nombre de la funcion a utilizar
set.seed(42)
model <- train(Class ~ ., data = Sonar, method = "knn",
               tuneGrid = myGrid, verbose = FALSE)


# Plot the results
plot(model)

model$metric
#una exploracion grande 
#luego una mas especifcia con tune grid





#VALIDACION CRUZADA
overfit <- read.csv("http://s3.amazonaws.com/
assets.datacamp.com/
production/course_1048/datasets/overfit.csv")


# Make a custom trainControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # Super important!
  verboseIter = TRUE
)

set.seed(42)
model <- train(y ~ ., overfit, method = "glmnet",
               trControl = myControl)
# Plot results
plot(model)