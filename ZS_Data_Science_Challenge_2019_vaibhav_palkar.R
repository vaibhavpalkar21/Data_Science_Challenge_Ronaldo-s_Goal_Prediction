# ZS Data Science Challenge 2019 by Vaibhav Palkar

options(max.print=100000)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(cowplot)
library("scales")
library(dummies)
library(MASS)
library(caTools)
library(car)
library(e1071)
library(caret)
library(randomForest)


## Read dataset
datafile <- read.csv("data.csv", stringsAsFactors = FALSE, header = TRUE)
View(datafile)
str(datafile)

# lets first keep is_goal column at end and save file as ronaldo_data
ronaldo_data <- datafile[,c(1:10, 12:28, 11)]
View(ronaldo_data)
dim(ronaldo_data) # dimesnions 30697    28

colnames(ronaldo_data)

## data cleaning and understanding

#MANCHESTER UNITED IS RONALDOS HOME TEAM, so when he is playing in home mean he is playing in MAN U

# check blank values column wise
blank_data <- sapply(ronaldo_data, function(x) length(which(x == "")))
blank_data
#check NA values in dataset
sum(is.na(ronaldo_data)) # large NA values found 27795
plot_missing(ronaldo_data)

colnames(ronaldo_data[apply(ronaldo_data,2,anyNA)])

summary(ronaldo_data)
# we have highest number of NA in our is_goal column

# "match_event_id" NA's   :1563, "location_x" NA's   :1461, "location_y" NA's   :1540  
# "remaining_min" NA's   :1562, "power_of_shot" NA's   :1486, "knockout_match" NA's   :1517,
# "remaining_sec" NA's   :1594     "distance_of_shot" NA's   :1567, "shot_id_number" NA's   :1563
# "remaining_min.1" NA's   :1535, "power_of_shot.1" NA's   :1539, "knockout_match.1" NA's   :1493
# "remaining_sec.1" NA's   :1539, "distance_of_shot.1" NA's   :1568, "is_goal" NA's   :6268   

unique(ronaldo_data$is_goal)
# 1 represent goal is score, 0 reresents not score and NA represents no data available for that entry
sum(is.na(ronaldo_data$is_goal))
# 6268 no data available

table(ronaldo_data$is_goal)
# 0     1 
# 13550 10879

# We are interested in prediction for data where goal is score is missing

## lets separate data which has blank entry for is_goal using dplyr library
ronaldo_no_goal_data <- filter(ronaldo_data, is.na(ronaldo_data$is_goal))
str(ronaldo_no_goal_data)
sum(is.na(ronaldo_no_goal_data)) # 10666

# lets check the goal score percentage
table(ronaldo_data$is_goal)
goal_score_percentage <- 10879/(13550+10879)*100
goal_score_percentage # 44.53314


## Data cleaning
# lets remove first column which only have row numbers and which is not of any use
str(ronaldo_data)
ronaldo_data <- ronaldo_data[,-c(1)]
View(ronaldo_data)
summary(ronaldo_data)
length(row.names(ronaldo_data[apply(ronaldo_data,1,anyNA),]))
# we have huge amount of data where we have NA values 12501

# lets check missing values for individual fields

# here we have few duplicate columns which we will consider later
# lets do it for categorical values 

# match_event_id
summary(ronaldo_data$match_event_id) # NA 1563
table(ronaldo_data$match_event_id)
sum(duplicated(ronaldo_data$match_event_id))
# match event id is unneccessary column which should be removed, so removong it
ronaldo_data <- ronaldo_data[,-1]

# location_x
summary(ronaldo_data$location_x) # NA 1461
table(ronaldo_data$location_x)
median(ronaldo_data$location_x, na.rm = TRUE)

# here we cant take mean value to replace with NA values for location_x
# here we are imputing NA values with 0 which is median and also has max number of value means mode value
ronaldo_data[which(is.na(ronaldo_data$location_x)), "location_x"] <- 0
table(ronaldo_data$location_x)

# location_y
summary(ronaldo_data$location_y) # NA 1540
table(ronaldo_data$location_y) # the mode value is 0
median(ronaldo_data$location_y,na.rm = TRUE)
mean(ronaldo_data$location_y,na.rm = TRUE)
# here we cant take mean value to replace with NA values for location_y
# here we are imputing NA values with 0 which is mode value
ronaldo_data[which(is.na(ronaldo_data$location_y)),"location_y"] <- 0
table(ronaldo_data$location_y)

# remaining_min
summary(ronaldo_data$remaining_min) # 1562 NA
table(ronaldo_data$remaining_min)
mean(ronaldo_data$remaining_min, na.rm = TRUE)
median(ronaldo_data$remaining_min, na.rm = TRUE)
#here we are imputing NA values with median value '5' 
ronaldo_data[which(is.na(ronaldo_data$remaining_min)),"remaining_min"] <- "5"
table(ronaldo_data$remaining_min)

# remaining_min.1
# this is duplicate column so remove it
ronaldo_data <- ronaldo_data[,-21]

# power_of_shot
summary(ronaldo_data$power_of_shot) # 1486 NA
table(ronaldo_data$power_of_shot)

#here we are imputing NA values with mode value '3' 
ronaldo_data[which(is.na(ronaldo_data$power_of_shot)),"power_of_shot"] <- "3"
table(ronaldo_data$power_of_shot)

# power_of_shot.1
# here this is duplicate column and the values are almost same but just few are differet in decimal limit
# lets remove it
summary(ronaldo_data$power_of_shot.1) # 1539 NA
table(ronaldo_data$power_of_shot.1)
ronaldo_data <- ronaldo_data[,-21]

# knockout_match
summary(ronaldo_data$knockout_match) # 1517 NA
table(ronaldo_data$knockout_match)

#here we are imputing NA values with mode value '0' 
ronaldo_data[which(is.na(ronaldo_data$knockout_match)),"knockout_match"] <- "0"
table(ronaldo_data$knockout_match)

# knockout_match.1
summary(ronaldo_data$knockout_match.1) # 1493 NA
table(ronaldo_data$knockout_match.1)
quantile(ronaldo_data$knockout_match.1,seq(0,1,0.01), na.rm = TRUE)
# we can see ther is suuden jump after 1-79% so we can cap it but after it it will be same as 0 and 1 output
# as we already have knock out fields
# So, here we are removing it

ronaldo_data <- ronaldo_data[,-21]

# game season
# game of season wont making any sense so we will remove it
summary(ronaldo_data$game_season) # 1493 NA
table(ronaldo_data$game_season) # 1493 NA
sum(is.na(ronaldo_data$game_season))
ronaldo_data <- ronaldo_data[,-6]

# remaining_sec
summary(ronaldo_data$remaining_sec) # NA 1594
table(ronaldo_data$remaining_sec)
quantile(ronaldo_data$remaining_sec, seq(0,1,0.01), na.rm = TRUE)

mean(ronaldo_data$remaining_sec, na.rm = TRUE)
median(ronaldo_data$remaining_sec, na.rm = TRUE)

# here we are imputing NA values with median value '28'
ronaldo_data[which(is.na(ronaldo_data$remaining_sec)), "remaining_sec"] <- 28
table(ronaldo_data$remaining_sec)

# remaining_sec.1
summary(ronaldo_data$remaining_sec.1) # NA 1539
table(ronaldo_data$remaining_sec.1)
quantile(ronaldo_data$remaining_sec.1, seq(0,1,0.01), na.rm = TRUE)
mean(ronaldo_data$remaining_sec.1,na.rm = TRUE)
median(ronaldo_data$remaining_sec.1,na.rm = TRUE)
# here this is duplicate column and the values are almost in same range but 
# just few are greater that 59 and with differet in decimal limit, which count is low
# we can remove it
ronaldo_data <- ronaldo_data[,-20]

# distance_of_shot
summary(ronaldo_data$distance_of_shot) # NA 1567
table(ronaldo_data$distance_of_shot)
quantile(ronaldo_data$distance_of_shot, seq(0,1,0.01), na.rm = TRUE)

# here most of the shot distance is not more than 50
# here we are imputing NA values with mode value '20'
ronaldo_data[which(is.na(ronaldo_data$distance_of_shot)), "distance_of_shot"] <- 20
table(ronaldo_data$distance_of_shot)

# distance_of_shot.1
# here this is duplicate column and the values are almost in same range but 
# just few are below that 20 and greater that 99 and with differet in decimal limit
# we can cap outliers but at the end it will be same as original columns, so lets remove it
ronaldo_data <- ronaldo_data[,-20]

# area of shot
# this data gives informatin about side of ground from shot played
summary(ronaldo_data$area_of_shot)
sum(is.na(ronaldo_data$area_of_shot))  # NA 0
table(ronaldo_data$area_of_shot)
# lest check blank data
length(which(ronaldo_data$area_of_shot == "")) # 1502 blank values
# lets impute blank data with mode value Center(C)
ronaldo_data$area_of_shot[ronaldo_data$area_of_shot == ""] <- "Center(C)"


# shot basics
summary(ronaldo_data$shot_basics)
sum(is.na(ronaldo_data$shot_basics))  # NA 0
table(ronaldo_data$shot_basics)
# lest check blank data
length(which(ronaldo_data$shot_basics == "")) # 1575 blank values
# lets impute blank data with mdoe values Mid Range
ronaldo_data$shot_basics[ronaldo_data$shot_basics == ""] <- "Mid Range"


# range of shot
summary(ronaldo_data$range_of_shot)
sum(is.na(ronaldo_data$range_of_shot))
table(ronaldo_data$range_of_shot)
# lest check blank data
length(which(ronaldo_data$range_of_shot == "")) # 1564 blank values
# lets impute blank data with mdoe values Less Than 8 ft.
ronaldo_data$range_of_shot[ronaldo_data$range_of_shot == ""] <- "Less Than 8 ft."

# team name
summary(ronaldo_data$team_name)
sum(is.na(ronaldo_data$team_name))
table(ronaldo_data$team_name)
# this column has only name of team ronaldo has played, so we can remove it as it is not important
ronaldo_data <- ronaldo_data[,-11]

# date of game
summary(ronaldo_data$date_of_game)
sum(is.na(ronaldo_data$date_of_game))
table(ronaldo_data$date_of_game)
length(which(ronaldo_data$date_of_game == ""))
# here we have humongous data for game, where when game is played, keeping this data dont amke any sense
# So, lets remove it
ronaldo_data <- ronaldo_data[,-11]

# home.away
# here we have to convert game detail to is playing away or home
summary(ronaldo_data$home.away)
length(which(ronaldo_data$home.away == "")) # 1497 blank values

ronaldo_data$home.away <- gsub("[a-zA-Z ]", "",ronaldo_data$home.away)
ronaldo_data$home.away <- gsub("[@]", "away",ronaldo_data$home.away)
ronaldo_data$home.away <- gsub("[[:punct:]]", "home",ronaldo_data$home.away)

# here we will replace blank values with mode game type(home or away)
table(ronaldo_data$home.away) 
# lets replace blank values with mode value away
ronaldo_data$home.away[ronaldo_data$home.away == ""] <- "away" 

# shot id number
summary(ronaldo_data$shot_id_number)
sum(is.na(ronaldo_data$shot_id_number)) # NA 1563
min(ronaldo_data$shot_id_number, na.rm = TRUE)
max(ronaldo_data$shot_id_number, na.rm = TRUE)
length(which(ronaldo_data$shot_id_number == "")) # blank values 0
# this data has shot id for each shot played by ronaldo and it will be helpful the predict goal for each shot
# So, we will keep this for refenrence
# but here few id's are missing and we can impute it with sequence
ronaldo_data$shot_id_number <- seq.int(nrow(ronaldo_data))

# lat.lng
summary(ronaldo_data$lat.lng)
table(ronaldo_data$lat.lng)
length(which(ronaldo_data$lat.lng == "")) # blanks values 1565
length(unique(ronaldo_data$lat.lng)) # 39
# this could mean that this is 39 different ground where ronaldo has played game
# but this is not giving any intersting idea about it

# latitude an dlongitude have 3 dimensional data
# so we can convert it to x,y,z and normalize it
#ronaldo_data$lattitude <- substr(ronaldo_data$lat.lng, start = 1, stop = 9)
#ronaldo_data$longitude <- substr(ronaldo_data$lat.lng, start = 11, stop = 22)

# now we can normalize the lattitude and longitude
# aand lets remove the original lan/lat column
ronaldo_data <- ronaldo_data[,-13]
#ronaldo_data <- ronaldo_data[,c(1:19,21:22,20)]

# type of shot
summary(ronaldo_data$type_of_shot)
sum(is.na(ronaldo_data$type_of_shot)) # NA 0
table(ronaldo_data$type_of_shot)
length(which(ronaldo_data$type_of_shot == "")) # 15280
# here large number of data is missing around 50%, we cang treat it, so better to remove it
ronaldo_data <- ronaldo_data[,-13]

# type of combined shot
summary(ronaldo_data$type_of_combined_shot)
sum(is.na(ronaldo_data$type_of_combined_shot)) # NA 0
table(ronaldo_data$type_of_combined_shot)
length(which(ronaldo_data$type_of_combined_shot == "")) # 15417
# here large number of data is missing around 50%, we cang treat it, so better to remove it
ronaldo_data <- ronaldo_data[,-13]

# match id
summary(ronaldo_data$match_id)
sum(is.na(ronaldo_data$match_id)) # NA 0
table(ronaldo_data$match_id)
length(which(ronaldo_data$match_id == "")) # blank 0
# match id column dont have any importance so we can remove it
ronaldo_data <- ronaldo_data[,-13]

# team id
summary(ronaldo_data$team_id)
table(ronaldo_data$team_id)
# match id column dont have any importance so we can remove it
ronaldo_data <- ronaldo_data[,-13]

# # lattitude
# summary(ronaldo_data$lattitude)
# table(ronaldo_data$lattitude)
# length(which(ronaldo_data$lattitude == "")) # blank values 1565
# mean(as.numeric(ronaldo_data$lattitude), na.rm = TRUE)
# median(ronaldo_data$lattitude)
# # lets replace blank values with median value "42.982923"
# ronaldo_data$lattitude[ronaldo_data$lattitude == ""] <- 42.982923
# 
# # longitude
# summary(ronaldo_data$longitude)
# table(ronaldo_data$longitude)
# length(which(ronaldo_data$longitude == "")) # blank values 1565
# mean(as.numeric(ronaldo_data$longitude), na.rm = TRUE)
# median(ronaldo_data$longitude)
# # lets replace blank values with median value "-71.446094"
# ronaldo_data$lattitude[ronaldo_data$lattitude == ""] <- -71.446094


blank_data <- sapply(ronaldo_data, function(x) length(which(x == "")))
blank_data

summary(ronaldo_data)

## Exploratory Data Analysis

# bar charts for categorical variable with goal percentage
# common function for all categorical variables
plot_goal_percent <- function(cat_var, var_name){
  a <- aggregate(is_goal~cat_var, ronaldo_data, sum)
  b <- aggregate(is_goal~cat_var, ronaldo_data, length)
  colnames(a)[2] <- "total"
  colnames(b)[2] <- "count"
  ab <- merge(a,b)
  colnames(ab) <- c(var_name,"total","count")
  ab$goal_rate <- round(ab$total*100/ab$count,2)
  ggplot(ab, aes(a[,1], goal_rate, label = goal_rate)) + 
    geom_bar(stat = 'identity',fill='plum') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = 2) + xlab(var_name)
  
}

plot_goal_percent(ronaldo_data$power_of_shot, "power_of_shot") 
# for power of shot 1 & 2 rate is highest but all are in same 40 to 47 range
plot_goal_percent(ronaldo_data$knockout_match, "knockout_match")
# for both knockoout and not knockout goal rate is almost same
plot_goal_percent(ronaldo_data$area_of_shot, "area_of_shot")
# goal percentage from centre(c) is highest and from mid ground is lowest, others are in same range
plot_goal_percent(ronaldo_data$shot_basics, "shot_basics")
# for goal area shot basics has highest goal rate and for mid ground line lowest, also for goal line, 
# left corner, mid range gas good goal rate, penalty and right corner
plot_goal_percent(ronaldo_data$range_of_shot, "range_of_shot")
# when range of shot is less than and between 8to16 feet then goal rate is highest
plot_goal_percent(ronaldo_data$home.away, "home.away")
# goal rate is highest when match is played in home ground

# histogram plots for numeric variable
# common function for all numeric variables
plot_num_var <- function(num_var, var_name) {
  ggplot(ronaldo_data, aes(num_var)) + geom_histogram(fill="steelblue", bins= 15, col="grey") + 
    xlab(var_name)
}
plot_num_var(ronaldo_data$location_x, "location_x")
# for location highest number of time location of ball hit was from '0' means centre of ground
plot_num_var(ronaldo_data$location_y, "location_y")
# for location_y highest number of time location of ball hit was from '0' means centre of ground
plot_num_var(as.numeric(ronaldo_data$remaining_min), "remaining_min")
plot_num_var(ronaldo_data$remaining_sec, "remaining_sec")
plot_num_var(ronaldo_data$distance_of_shot, "distance_of_shot")
# whenever ronaldo has hit the ball then distance of shot is always below 50
plot_num_var(as.numeric(ronaldo_data$remaining_min.1), "remaining_min.1")
plot_num_var(ronaldo_data$remaining_sec.1, "remaining_sec.1")

# lest separate numeric columns
numericcols <- c('location_x', 'location_y', 'remaining_min', 'remaining_sec', 'distance_of_shot', 'shot_id_number')

# lets separate categorical columns
factorcols <- c('power_of_shot', 'knockout_match', 'area_of_shot', 'shot_basics', 'range_of_shot', 'home.away', 'is_goal')

ronaldo_data[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(ronaldo_data[, x])))
ronaldo_data[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(ronaldo_data[, x])))


# # lets scale numeric data
# ronaldo_data$location_x <- scale(ronaldo_data$location_x)
# ronaldo_data$location_y <- scale(ronaldo_data$location_y)
# ronaldo_data$remaining_min <- scale(ronaldo_data$remaining_min)
# ronaldo_data$remaining_sec <- scale(ronaldo_data$remaining_sec)
#ronaldo_data$distance_of_shot <- scale(ronaldo_data$distance_of_shot)
# ronaldo_data$remaining_min.1 <- scale(ronaldo_data$remaining_min.1)
# ronaldo_data$remaining_sec.1 <- scale(ronaldo_data$remaining_sec.1)

# check data
str(ronaldo_data)


# Model Building 
# Build a logistic regression model 

#creating dummy variables

ronaldo_data$is_goal <- as.integer(as.character(ronaldo_data$is_goal))

ronaldo_data <- dummy.data.frame(ronaldo_data)

ronaldo_data$is_goal <- as.factor(ronaldo_data$is_goal)

str(ronaldo_data)

# splitting into train, test data, validation

set.seed(100)

## now let separate the is_goal missing data which we will use to predict goal
ronaldo_data_test <- filter(ronaldo_data, is.na(ronaldo_data$is_goal))

ronaldo_data_train_valid <- filter(ronaldo_data, !is.na(ronaldo_data$is_goal))
# lets split data into train and validation
split_indices <- sample.split(ronaldo_data_train_valid$is_goal, SplitRatio = 0.80)

train <- ronaldo_data_train_valid[split_indices, ]

test <- ronaldo_data_train_valid[!split_indices, ]

nrow(train)/nrow(ronaldo_data)

nrow(test)/nrow(ronaldo_data)

## generating models
model_1 = glm(is_goal ~ ., data = train, family = "binomial")
summary(model_1)
# Null deviance: 26858  on 19542  degrees of freedom
# Residual deviance: 25952  on 19513  degrees of freedom
# AIC: 26012
# there are many insignificant variables


# we will use step-wise function to remove the extremely insignificant variables
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#remove insignificant variables and multicollinear ones from the model on the basis of VIF and p-value
model_3 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + distance_of_shot + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` + `shot_basicsGoal Area` + 
                 `shot_basicsGoal Line` + `shot_basicsMid Range` + `shot_basicsPenalty Spot` + 
                 `range_of_shot16-24 ft.` + `range_of_shot24+ ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)

# dshot_basicsMid Range has highest vif 26.21 lets remove it

model_4 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + distance_of_shot + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` + `shot_basicsGoal Area` + 
                 `shot_basicsGoal Line` + `shot_basicsPenalty Spot` + 
                 `range_of_shot16-24 ft.` + `range_of_shot24+ ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

# `range_of_shot24+ ft. has highest vif value 5.88 and also high p-value lets remove it
# so, we will not remove it and will remove distance_of_shot and will observe change
model_5 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + distance_of_shot + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` + `shot_basicsGoal Area` + 
                 `shot_basicsGoal Line` + `shot_basicsPenalty Spot` + 
                 `range_of_shot16-24 ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)

# distance_of_shot and shot_basicsGoal Area have high vif , but highly significant cant remove it
# next shot_basicsPenalty Spot has high vif and insignificant also, lets remove it
model_6 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + distance_of_shot + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` + `shot_basicsGoal Area` + 
                 `shot_basicsGoal Line` +
                 `range_of_shot16-24 ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_6)
vif(model_6)

# shot_basicsGoal Area again has high vif abd significant also, lets remove and observe its effect on other parameters
model_7 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + distance_of_shot + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` +
                 `shot_basicsGoal Line` +
                 `range_of_shot16-24 ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_7)
vif(model_7)
# distance_of_shot high vif. lets remove it

model_8 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` +
                 `shot_basicsGoal Line` +
                 `range_of_shot16-24 ft.` + `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_8)
vif(model_8)

# now our vif value is around 2 and its good , 
# so from here we will remove insignificant variable base of p-value, all above 0.05
# lets remove range_of_shot16-24 ft.
model_9 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 + power_of_shot5 + 
                 remaining_sec + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` +
                 `shot_basicsGoal Line` +
                 `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_9)

# lets remove power_of_shot5
model_10 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                 power_of_shot1 + power_of_shot2 + power_of_shot3 +
                 remaining_sec + `area_of_shotCenter(C)` + 
                 `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                 `area_of_shotRight Side Center(RC)` +
                 `shot_basicsGoal Line` +
                 `range_of_shot8-16 ft.`, 
               family = "binomial", data = train)
summary(model_10)

# now we have all significant variables, but we can go till upto most highly significant varible
# lets remove remaining_sec
model_11 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                  power_of_shot1 + power_of_shot2 + power_of_shot3 +
                  `area_of_shotCenter(C)` + 
                  `area_of_shotLeft Side(L)` + `area_of_shotMid Ground(MG)` + 
                  `area_of_shotRight Side Center(RC)` +
                  `shot_basicsGoal Line` +
                  `range_of_shot8-16 ft.`, 
                family = "binomial", data = train)
summary(model_11)

# lets remove area_of_shotMid Ground(MG)
model_12 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                  power_of_shot1 + power_of_shot2 + power_of_shot3 +
                  `area_of_shotCenter(C)` + 
                  `area_of_shotLeft Side(L)` +
                  `area_of_shotRight Side Center(RC)` +
                  `shot_basicsGoal Line` +
                  `range_of_shot8-16 ft.`, 
                family = "binomial", data = train)
summary(model_12)

# lets remove power_of_shot2
model_13 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                  power_of_shot1 + power_of_shot3 +
                  `area_of_shotCenter(C)` + 
                  `area_of_shotLeft Side(L)` +
                  `area_of_shotRight Side Center(RC)` +
                  `shot_basicsGoal Line` +
                  `range_of_shot8-16 ft.`, 
                family = "binomial", data = train)
summary(model_13)

# lets remove power_of_shot3
model_14 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                  power_of_shot1 + 
                  `area_of_shotCenter(C)` + 
                  `area_of_shotLeft Side(L)` +
                  `area_of_shotRight Side Center(RC)` +
                  `shot_basicsGoal Line` +
                  `range_of_shot8-16 ft.`, 
                family = "binomial", data = train)
summary(model_14)

# lets remove power_of_shot1 
model_15 <- glm(formula = is_goal ~ location_x + location_y + remaining_min + 
                  `area_of_shotCenter(C)` + 
                  `area_of_shotLeft Side(L)` +
                  `area_of_shotRight Side Center(RC)` +
                  `shot_basicsGoal Line` +
                  `range_of_shot8-16 ft.`, 
                family = "binomial", data = train)
summary(model_15)

# in final model here we have highly significant variables

# coclusion from model
# location_x,  location_y, area_of_shotCenter, remaining_min, area_of_shotLeft Side(L), 
# area_of_shotRight Side Center(RC), shot_basicsGoal line, range_of_shot8-16 ft.

#Finalizing model_15
final_model<-model_15

## MODEL EVALUATION

#PRediciton on test data
test_pred = predict(final_model, type = "response", newdata = test[,-36])

summary(test_pred)
# We are getting probability range between 4.623% to 65.19% with mean as 44.26% and median as 42.25%
# Third qurtile as 53.10%

# Putting prediction in test data frame
test$pred_probability <- test_pred

# Let's use the probability cutoff of 44.26% from summary mean

test_predicted_goal <- factor(ifelse(test_pred >= 0.4426, 1, 0))
test_actual_goal <- test$is_goal

table(test_actual_goal, test_predicted_goal)

# # Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(test_predicted_goal, test$is_goal, positive = "1")

conf # 

acc <- conf$overall[1]
acc # 57.75 %
sens <- conf$byClass[1]
sens # 51.56 %
spec <- conf$byClass[2]
spec # 62.73 %

## Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  test_predicted_goal <- factor(ifelse(test_pred >= cutoff, 1, 0))
  conf <- confusionMatrix(test_predicted_goal, test_actual_goal, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_pred)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.04623 0.36723 0.42555 0.44268 0.53103 0.65198 

# Creating cutoff values from 0.0367 to 0.6100 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.036,.61,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.70,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------   
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.025)]
cutoff #0.4186667

# Let's choose a cutoff value of 41.86667% for final model
test_predicted_goal <- factor(ifelse(test_pred >= 0.4186, 1, 0))

conf_final <- confusionMatrix(test_predicted_goal, test$is_goal, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 55.73066 %

sens # 59.05331 %

spec # 0.5306273

## now lets predict the probability for misisng goal values
#PRediciton on ronaldo_data_test (our final missing data)
test_pred_missing = predict(final_model, type = "response", newdata = ronaldo_data_test[,-36])

summary(test_pred)
# We are getting probability range between 4.623% to 65.19% with mean as 44.26% and median as 42.25%
# Third qurtile as 53.10%

# Putting prediction in test data frame
ronaldo_data_test$predicted_probability <- test_pred_missing

ronaldo_goal_prediction <- ronaldo_data_test[,c(35,37)]
write.csv(ronaldo_goal_prediction, file = "ronaldo_goal_prediction.csv")
