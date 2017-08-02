
#filling in the gaps for the ages, by putting in the mean of the rest
Titanic_Raw$Age[is.na(Titanic_Raw$Age)] <- mean(Titanic_Raw$Age,na.rm=T)
Titanic_Raw$Sex <- gsub(",","", Titanic_Raw$Sex)
Titanic_Raw$Sex[Titanic_Raw$Sex == "male"] <- 0
Titanic_Raw$Sex[Titanic_Raw$Sex == "female"] <- 1
Titanic_Raw$Embarked[Titanic_Raw$Embarked == "C" | Titanic_Raw$Embarked == "c"] <- 0
Titanic_Raw$Embarked[Titanic_Raw$Embarked == "S" | Titanic_Raw$Embarked == "s"] <- 1
Titanic_Raw$Embarked[Titanic_Raw$Embarked == "Q" | Titanic_Raw$Embarked == "q"] <- 2

#choosing the training data by randomly selecting 800 rows
training_data2 <- Titanic_Raw[sample(nrow(Titanic_Raw), 800), ]

#finding the id lists for the training data and the total data
training_data_ids <- training_data2$PassengerId
orig_data_ids <- Titanic_Raw$PassengerId

#putting the ids in the original not in training into testing id list
testing_data_ids <- vector("list", 91)
i <- 1
for (id in orig_data_ids) { 
  if (!(id %in% training_data_ids)) { 
      testing_data_ids[[i]] <- id
      i <- i + 1
  }
}

#extracting the testing data using the testing id list and the original data
testing_data4 <- subset(Titanic_Raw, PassengerId %in% testing_data_ids)

#selecting all the pertaining columns, exlcuding unneccesary categorical variables,
#such as fares, ticket numbers, names, etc
training_data3 <- subset(training_data2,select=c(2,3,5,6,7,8,12))
testing_data5 <- subset(testing_data4,select=c(2,3,4,5,6,7,8,12))

#filling all the remaining empty spaces in the training and test data with NA
is.na(training_data3) <- training_data3==''
is.na(testing_data4) <- testing_data4==''

#used to make sure that all the neccessary values are filled in
#library(Amelia)
#missmap(training_data3, main = "Missing values vs observed")

#create a logistical model (for survival), using the training data. also find the summary
model <- glm(Survived ~.,family=binomial(link='logit'),data=training_data3)
summary_model <- summary(model)

#finding the prediction, using the testing data
fitted.results <- predict(model,testing_data5,type='response')
#making the predicted values either 1 or 0 (rounding up or down)
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
#finding error by taking mean of values that are wrong
misClasificError <- mean(fitted.results != testing_data4$Survived)

#accuracy reading 
print(paste('Accuracy',1-misClasificError))




