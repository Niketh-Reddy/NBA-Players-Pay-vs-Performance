library("tidyverse")
library("sqldf")
library("readxl")
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("corrplot")
library("mice")
library("car")
library("estimatr")
library("caret")
library("hoopR")
library("writexl")
library("rvest")
library("RCurl")



df <- read_excel("D:/MMA 860 Acquisition and Management of Data/Datasets/Final Project/MMA860 - TeamProject - NBA Salaries and Statistics Data - 2011-2021.xlsx")
df_copy <- read_excel("D:/MMA 860 Acquisition and Management of Data/Datasets/Final Project/MMA860 - TeamProject - NBA Salaries and Statistics Data - 2011-2021 - Copy.xlsx")

#Deleting rows with empty data (as data is MCAR) and removing duplicated columns
df_copy <- df_copy %>% drop_na
nrow(df_copy)
# Number of rows is 482. So 48 out of 550 i.e 9.6% of data has been deleted
drop <- c("POSITION")
df_copy = df_copy[,!(names(df_copy) %in% drop)]

summary(df_copy)

#Breaking the data into TRAIN and TEST
sample <- sample.int(n = nrow(df_copy), size=floor(.7*nrow(df_copy)), replace = F)
train <- df_copy[sample,]
test <- df_copy[-sample,]

#let's build our model, but only on the train data set this time.
run1 <- lm(SALARY ~ ., train)
summary(run1)

#running a regression with significant variables only
run2 <- lm(SALARY ~ `2014` +`2015` +`2016` +`2017` +`2018` +`2019`+`2020`+`2021`+
             TEAM_DET+TEAM_LAL+AGE+GS+AST+PTS+GMS_Attendance,train)
summary(run2)

run3 <- lm(SALARY ~ `2016` +`2017` +`2018` +`2019`+`2020`+`2021`+TEAM_LAL+AGE+PTS, train)
summary(run3)
plot(run3)
#Age is significant. #TEAM_LAL is significant#PTS is significant

#let's predict the salaries on our test data
pred <- predict(run3,test)
summary(pred)

#test & train comparisons
data.frame( R2 = R2(pred, test$SALARY),
            RMSE = RMSE(pred, test$SALARY),
            MAE = MAE(pred, test$SALARY))

#Good Match between train and test dataset regression results.
#Now using the model to get predicted salaries of players on entire dataset
pred_full <- predict(run3,df)
summary(pred_full)

#Merging predicted salaries to the salary dataframe
df_with_predictions <- cbind(df,pred_full)
View(df_with_predictions)

#Calculating the difference between the acutal salary and the predicted salary
df_with_predictions$SalariesDiff <- df_with_predictions$SALARY - df_with_predictions$pred_full
#Identifying the overvalued and undervalued salaries
df_with_predictions$Over_Undervalue <- ifelse(df_with_predictions$SalariesDiff > 0, "Overvalued", "Undervalued")

write_xlsx(df_with_predictions,path="D:/MMA 860 Acquisition and Management of Data/Datasets/Final Project/MMA860 - TeamProject - NBA Salaries Predicted Values - 2011-2021.xlsx")

#Finding the overvalued players in 2021 
df_2021_overvalued <- filter(df_with_predictions, df_with_predictions$YEAR == "2021" & df_with_predictions$Over_Undervalue == "Overvalued")
nrow(df_2021_overvalued)
#20 overvalued players in 2021
#Below command gives the list of overrvalued players in 2021
paste(df_2021_overvalued$"FIRST NAME",df_2021_overvalued$"LAST NAME")

write_xlsx(df_2021_overvalued,path="D:/MMA 860 Acquisition and Management of Data/Datasets/Final Project/MMA860 - TeamProject - NBA Salaries Predicted Values - Overvalued - 2021.xlsx")

socialmedia <- read_excel("C:\\Users\\Renee\\OneDrive\\Desktop\\MMA860 - TeamProject - Social Media - 2021.xlsx")
socialmedia_M <- mice(socialmedia, m=5, maxit = 30, meth = "pmm", seed = 1)
socialmedia_M_Complete <- mice::complete(socialmedia_M, 1)

socialmedia_Reg <- with(socialmedia_M, lm(SalariesDiff ~ Twitter + Instagram))
summary(socialmedia_Reg)
summary(pool(socialmedia_Reg))

write_xlsx(socialmedia_M_Complete ,path="C:\\Users\\Renee\\OneDrive\\Desktop\\MMA860 - TeamProject - NBA Salaries Predicted Values - Overvalued with Social Media - 2021.xlsx")
