
Titanic_Raw$Age[is.na(Titanic_Raw$Age)] <- mean(Titanic_Raw$Age,na.rm=T)
training_data2 <- Titanic_Raw[sample(nrow(Titanic_Raw), 800), ]
training_data_ids <- training_data2$PassengerId
orig_data_ids <- Titanic_Raw$PassengerId

testing_data_ids <- vector("list", 91)
i <- 1
for (id in orig_data_ids) { 
  if (!(id %in% training_data_ids)) { 
      testing_data_ids[[i]] <- id
      i <- i + 1
  }
}

testing_data4 <- subset(Titanic_Raw, PassengerId %in% testing_data_ids)

is.na(training_data3) <- training_data3==''
is.na(testing_data4) <- testing_data4==''
training_data3 <- subset(training_data2,select=c(2,3,5,6,7,8,10,12))
library(Amelia)
missmap(training_data3, main = "Missing values vs observed")
model <- glm(Survived ~.,family=binomial(link='logit'),data=training_data3)
summary_model <- summary(model)

testing_data5 <- subset(testing_data4,select=c(2,3,4,5,6,7,8,10,12))
fitted.results <- predict(model,testing_data5,type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != testing_data4$Survived)
print(paste('Accuracy',1-misClasificError))




