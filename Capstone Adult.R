library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(scales)

#######
# 2. Data
## 2.1.	 Import and Cleaning 

#downloading dataset

adult <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                    header = FALSE, sep = ",", na=" ?")
dim(adult)

#giving names for each column

names(adult) = c("age","workclass","fnlwgt","education","education_num","marial_status","occupation",
                 "relationship","race","sex","capital_gain","capital_loss","hours_per_week",
                 "native_country", "income")

#Structure of the dataset
str(adult)

# ommitting all observations with nas and checking the new dimension
adult <- na.omit(adult)
dim(adult)


# variable fnlwgt will be dropped out, weighting variable

adult[[ "fnlwgt"]] <- NULL


#mapping age of 15, 25, 45, 65, 100 into Young, Middle_age, Senior, Old

adult[[ "age"]] <- ordered(cut(adult[[ "age"]], c(15,25,45,65,100)), 
                           labels= c("Young","Middle_age","Senior","Old"))


#education reordering
adult$education<-ordered(adult$education,levels=c(" Preschool"," 1st-4th"," 5th-6th",
                                                  " 7th-8th"," 9th"," 10th"," 11th"," 12th",
                                                  " HS-grad"," Some-college"," Assoc-acdm",
                                                  " Assoc-voc"," Bachelors"," Masters",
                                                  " Prof-school"," Doctorate"))

adult$education<-as.factor(adult$education)


#mapping hours-per-week into Part_time, Full_time, Over_time, Workaholic

adult[[ "hours_per_week"]] <- ordered(cut(adult[[ "hours_per_week"]],
                                          c(0,25,40,60,110)),
                                      labels = c("Part_time", "Full_time", "Over_time", "Workaholic"))


#mapping capital_gain

adult[[ "capital_gain"]] <- ordered(cut(adult[[ "capital_gain"]],
                                        c(-Inf,0,median(adult[[ "capital_gain"]][adult[[ "capital_gain"]]>0]),
                                          Inf)), labels = c("None", "Low", "High"))

#mapping capital_loss

adult[[ "capital_loss"]] <- ordered(cut(adult[[ "capital_loss"]],
                                        c(-Inf,0, median(adult[[ "capital_loss"]][adult[[ "capital_loss"]]>0]),
                                          Inf)), labels = c("None", "Low", "High"))




#native-country, there are 41 different countries
#mapping them into smaller groups
levels(adult$`native_country`)

# defining different regions
east_asia <- c(" Cambodia", " China", " Hong", " Laos", " Thailand",
               " Japan", " Taiwan", " Vietnam")

central_asia <- c(" India", " Iran")

central_america <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", 
                     " Puerto-Rico",  " Dominican-Republic", " El-Salvador", 
                     " Haiti", " Honduras", " Mexico", " Trinadad&Tobago")

south_america <- c(" Ecuador", " Peru", " Columbia")


west_europe <- c(" England", " Germany", " Holand-Netherlands", " Ireland", 
                 " France", " Greece", " Italy", " Portugal", " Scotland")

east_europe <- c(" Poland", " Yugoslavia", " Hungary")

adult <- mutate(adult, 
                native_region = ifelse(native_country %in% east_asia, " East-Asia",
                                       ifelse(native_country %in% central_asia, " Central-Asia",
                                              ifelse(native_country %in% central_america, " Central-America",
                                                     ifelse(native_country %in% south_america, " South-America",
                                                            ifelse(native_country %in% west_europe, " Europe-West",
                                                                   ifelse(native_country %in% east_europe, " Europe-East",
                                                                          ifelse(native_country == " United-States", " United-States", 
                                                                                 " Others" ))))))))


adult$native_region <- factor(adult$native_region, ordered = FALSE)

# dropping native_country column, as it has become absolete

adult[[ "native_country"]] <- NULL

# Checking the final structure of the dataset
str(adult)

########
## 2.2.	Exploration
#
# Percentage distribution of income across different occupations

adult %>% count(occupation, income) %>% 
  left_join(adult %>% count(occupation), by="occupation") %>% 
  mutate(pct=(n.x/n.y)*100, ypos=0.6*n.x) %>% 
  ggplot(aes(x=reorder(occupation,n.x), n.x, fill=income)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position="stack",aes(label=paste0(sprintf ("%1.1f", pct),"%"), y=ypos))+
  coord_flip()+
  labs(title = "Income Split Within Different Occupations ")

#Distribution across occupations and sex

adult %>% count(occupation, income, sex) %>% 
  left_join(adult %>% count(occupation), by="occupation") %>% 
  ggplot(aes(x=reorder(occupation,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Occupations And Sex")

#Distribution across occupations and age

adult %>% count(occupation, income, age) %>% 
  left_join(adult %>% count(occupation), by="occupation") %>% 
  ggplot(aes(x=reorder(occupation,n.x), n.x, fill=age)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Occupations And Age")

#Distribtuion across age and sex
adult %>% count(age, income, sex) %>% 
  left_join(adult %>% count(age), by="age") %>% 
  ggplot(aes(x=reorder(age,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Age and Sex")

#Distribution across marial_status and sex
adult %>% count(marial_status, income, sex) %>% 
  left_join(adult %>% count(marial_status), by="marial_status") %>% 
  ggplot(aes(x=reorder(marial_status,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Marial Status and Sex")


#Distribution across work hours_per_week and sex
adult %>% count(hours_per_week, income, sex) %>% 
  left_join(adult %>% count(hours_per_week), by="hours_per_week") %>% 
  ggplot(aes(x=reorder(hours_per_week,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Work Hours Per Week and Sex")


#Distribution across relationship and sex
adult %>% count(relationship, income, sex) %>% 
  left_join(adult %>% count(relationship), by="relationship") %>% 
  ggplot(aes(x=reorder(relationship,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Relationship and Sex")



#Distribution across education and sex
adult %>% count(education, income, sex) %>% 
  left_join(adult %>% count(education), by="education") %>% 
  ggplot(aes(x=education, n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Education and Sex")

# Education in number of years
adult %>% count(education_num, income) %>% 
  left_join(adult %>% count(education_num), by="education_num") %>% 
  mutate(pct=(n.x/n.y)*100, ypos=0.5*n.x) %>% 
  ggplot(aes(x=education_num, n.x, fill=income)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position="stack",aes(label=paste0(sprintf ("%1.1f", pct),"%"), y=ypos))+
  scale_y_log10()+
  coord_flip()+
  labs(title = "Income Split Within Different Education Years ")



#Distribution across workclass and sex
adult %>% count(workclass, income, sex) %>% 
  left_join(adult %>% count(workclass), by="workclass") %>% 
  ggplot(aes(x=reorder(workclass,n.x), n.x, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip()+
  labs(title = "Income Split Across Workclass and Sex")

#Distribution Within workclasses
adult %>% count(workclass, income) %>% 
  left_join(adult %>% count(workclass), by="workclass") %>% 
  mutate(pct=(n.x/n.y)*100, ypos=0.5*n.x) %>% 
  ggplot(aes(x=reorder(workclass,n.x), n.x, fill=income)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position="stack",aes(label=paste0(sprintf ("%1.1f", pct),"%"), y=ypos))+
  scale_y_log10()+
  coord_flip()+
  labs(title = "Income Split Within Different Workclasses ")


# Income Split across races

adult %>% count(race, income) %>% 
  left_join(adult %>% count(race), by="race") %>% 
  mutate(pct=(n.x/n.y)*100, ypos=0.5*n.x) %>% 
  ggplot(aes(x=reorder(race,n.x), n.x, fill=income)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position="stack",aes(label=paste0(sprintf ("%1.1f", pct),"%"), y=ypos))+
  scale_y_log10()+
  coord_flip()+
  labs(title = "Income Split Within Different Races ")

# $Income Split across Native_regions

adult %>% count(native_region, income) %>% 
  left_join(adult %>% count(native_region), by="native_region") %>% 
  mutate(pct=(n.x/n.y)*100, ypos=0.5*n.x) %>% 
  ggplot(aes(x=reorder(native_region,n.x), n.x, fill=income)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position="stack",aes(label=paste0(sprintf ("%1.1f", pct),"%"), y=ypos))+
  scale_y_log10()+
  coord_flip()+
  labs(title = "Income Split Within Different Native Regions ")


#capital gain
adult %>% count(occupation, income, capital_gain) %>% 
  left_join(adult %>% count(occupation), by = "occupation" )%>% 
  ggplot(aes(x=reorder(occupation,n.x), n.x, fill=capital_gain)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip() +
  labs(title = "Income Split Across Occupations and Capital Gains")

#capital loss
adult %>% count(occupation, income, capital_loss) %>% 
  left_join(adult %>% count(occupation), by = "occupation" )%>% 
  ggplot(aes(x=reorder(occupation,n.x), n.x, fill=capital_loss)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(income))+
  coord_flip() +
  labs(title = "Income Split Across Occupations and Capital Losses")


#####

# 3.	Statistical Analysis
##3.1. Logistic Regression

# Illustration of logit function

x_test <- seq(1:100)
y_test <- 1/(1+exp((-1)*(-5+(0.10)*x_test)))
plot(x_test, y_test)


##3.2.  Model Estimation

#changing ordered variables age,education, capital_gains, capital_loss, hours_per_week into unordered factors

adult2 <- adult
adult2$age <- factor(adult2$age, ordered = FALSE)
#adult2$education_num <- factor(adult2$education_num, ordered = FALSE)
adult2$education <- factor(adult2$education, ordered = FALSE)
adult2$capital_gain <- factor(adult2$capital_gain, ordered = FALSE)
adult2$capital_loss <- factor(adult2$capital_loss, ordered = FALSE)
adult2$hours_per_week <- factor(adult2$hours_per_week, ordered = FALSE)


# creating a train and test set
library(caret)
set.seed(755)

test_index <- createDataPartition(y = adult2$income, times = 1, p = 0.2, list = FALSE)
train_set <- adult2[-test_index,]
test_set <- adult2[test_index,]

######

# logistic regression
# first run

glmfit <- glm(income~., data=train_set, family=binomial)
summary(glmfit)


## 3.3. Output Reading
## 3.4. Optimizing the Model
### 3.4.1. Multicollinearity

# first run with variables results into NA estimates for education_num, 
# my suspiscion is education and education_num are highly correlated
# new run without education

glmfit <- glm(income~.-education, data=train_set, family=binomial)
summary(glmfit)

# Are any other multicollinearities among the independent variabels?
# car package; test of multicollinearity of independent variabels
# referece: http://scg.sdsu.edu/logit_r/

library(car)
vif(glmfit)


# attempt to eliminate variables with a GVIF higher than 5, 
# the variables marial_status and relationship do have GVIF values >5.
# regression without variable relationship

### 3.4.2 Deviance

glmfit <- glm(income~.-education-relationship, data=train_set, family=binomial)
summary(glmfit)

vif(glmfit) # all remaining independent variables no longer correlated

# regression without variable marial_stauts
# residual deviance and AIC number are worse than witout marial_status instead
# all remaing variable independent variables no longer correlated 

glmfit <- glm(income~.-education-marial_status, data=train_set, family=binomial)
summary(glmfit)

vif(glmfit) # all remaining independent variables no longer correlated

## 3.5. Interpretation of Model Estimates
## 3.6. Finding the Right Threshold

# proposed probabilities given estimates from logistic model
glm_hat <- predict(glmfit, data = train_set, type = "response")


# As threshold whether someone earns over 50k, a probability of 0.5 is assumed
table(ActualValue=train_set$income, PredictedValue = glm_hat >0.5)



#finding threshold number; package ROCR
library(ROCR)
ROCRPred = prediction(glm_hat, train_set$income)
ROCRPerf <- performance(ROCRPred, "tpr","fpr")
plot(ROCRPerf, colorize=TRUE  , print.cutoffs.at=seq(0.1, by=0.1))

# From the ROCR plot, the best ration is around 0.3

table(ActualValue=train_set$income, PredictedValue = glm_hat >0.3)


## 3.7 Validation
#  validation with test dataset

glm_hat_test <- predict(glmfit, newdata = test_set, type = "response")

table(ActualValue=test_set$income, PredictedValue = glm_hat_test >0.3)



