PATH <- "D:/09_analytics_new_start/02_carEvaluation"

setwd(PATH)

train_data <- read.csv("car.data",stringsAsFactors = FALSE)

str(train_data)

name_data_frame = c("buying","maint","doors","persons","lug_boot","safety","response")

names(train_data) = name_data_frame

str(train_data)

train_data[,name_data_frame] = lapply(train_data[,name_data_frame],factor)

class(train_data)

str(train_data)

round(prop.table(table(train_data$response))*100)

final_train_data <- train_data[FALSE,]
final_test_data <- train_data[FALSE,]

for(var_response in unique(train_data$response)){
  print(paste("Processing data ", var_response))
  temp_frame <- train_data[train_data$response == var_response,]
  bound <- floor((nrow(temp_frame)/4)*3)
  temp_frame <- temp_frame[sample(nrow(temp_frame)),]
  df.train <- temp_frame[1:bound,]
  df.test <- temp_frame[(bound+1):nrow(temp_frame),]
  final_train_data <- rbind(final_train_data,df.train)
  final_test_data <- rbind(final_test_data,df.test)
}

table(train_data$response)
table(final_train_data$response)
table(final_test_data$response)

library(rpart)

model = rpart(response ~ buying + maint + doors + persons + lug_boot + safety,  data = final_train_data, method = "class")
pred = predict( model, final_test_data[,1:6],method = "class")

df = as.data.frame(pred > .5)
pred = as.data.frame(apply(df , 1, function(x) colnames(df) [which(x==TRUE)]))

str(pred)

round(prop.table (table (Pred = unlist(pred), Actual = final_test_data$response))*100)
