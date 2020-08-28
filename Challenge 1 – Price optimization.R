library(dplyr)
library(randomForest)
library(lubridate)

options(stringsAsFactors=TRUE)


user_table <- read.table("data/challenge_1/user_table.csv", header = TRUE, sep = ",", quote = "\"")
#user_table <- read_csv("data/challenge_1/user_table.csv")
#test_results <- read_csv("data/challenge_1/test_results.csv", col_types = cols(timestamp = col_character()))
test_results <- read.table("data/challenge_1/test_results.csv", header = TRUE, sep = ",", quote = "\"")

# inner join
user_test_results = test_results%>%
                    inner_join(user_table)

# creating new features

user_test_results = user_test_results%>%
                    mutate(
                      timestamp = ymd_hms(timestamp),
                      day = as.factor(lubridate::day(timestamp)),
                      weekday =as.factor(unclass(lubridate::wday(timestamp, label = TRUE))),
                      hour = as.factor(lubridate::hour(timestamp)),
                      test=as.factor(test),
                      converted=as.factor(converted)
                    )

user_test_results_data = user_test_results%>%
                         select(source, device, operative_system, test, city, day, weekday, hour, converted)%>%
                         na.omit()

library(h2o)

localH2O = h2o.init()

thermal_h2o_df <- as.h2o(user_test_results_data)

splits <- h2o.splitFrame(
            thermal_h2o_df, ##  splitting the H2O frame we read above
            c(0.6,0.2),       ##  create splits of 60% and 20%; 
            ##  H2O will create one more split of 1-(sum of these parameters)
            ##  so we will get 0.6 / 0.2 / 1 - (0.6 + 0.2) = 0.6/0.2/0.2
            seed=12345
          )    ##  setting a seed will ensure reproducible results (not R's seed)


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex")
test <- h2o.assign(splits[[3]], "test.hex") 

## h2o.randomForest function

rf1 <- h2o.randomForest(         
          training_frame = train,        ## training
          validation_frame = valid,      ## validation
          y=9,                           ## the target index
          ntrees = 50,                   ## use a maximum of 200 trees to create the
          balance_classes = TRUE,        # treat the unbalanced problem
          #stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
          score_each_iteration = T,      ## Predict against training and validation for
          seed = 1000000,
          nfolds = 5,
          nbins_cats = 30,
          stopping_metric = "mean_per_class_error"
       )

h2o.confusionMatrix(rf1)

summary(rf1) 

rf1@model$validation_metrics


h2o.saveModel(rf1, path = paste0(getwd(),"/h2o model"),  force = TRUE)


finalRf_predictions<-h2o.predict(
                        object = rf1,
                        newdata = test
                      )

final_results = cbind( as.data.frame(test), as.data.frame(finalRf_predictions))


### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)

# What are your main findings looking at the data? What is your overall view into user behavior, especially focusing on actionable insights that might increase conversion rate?
# US
# peak is between 10-12h
# dia da semana?
# horario?
# dara driven- random forest approach

#install.packages("randomForest")


#What is your recommendation to the company in terms of setting an optimum price for their product?



#Can you optimize the number of days that the test is run? After how many days you would have stopped the test? Why?


results = lm( user_test_results_data$converted ~ ., data=user_test_results_data)
anova(results)
