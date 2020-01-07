### Library ####

library("data.table")
library("tibble")
library("parallel")
library("MASS")
library("psych")
library("foreach")
library("dummies")
library("dplyr")
library("nortest")
library("PerformanceAnalytics")
library("e1071")
library("coefplot")
library("keras")
library("tensorflow")
library("ISLR")
library("tree")
library("ggplot2")
library("gridExtra")
library("rpart")
library("caret")
library("pROC")
library("rpart.plot")
library("class")
library("caret")
library("ggpubr")
library("DMwR")


set.seed(123)
options(max.print = 10000)

### Import Data ###

setwd("C:/Users/Ali/Desktop/CMHC")
Data_Raw <- read.csv("Employee Test data No. 1.csv",sep=",");
colnames(Data_Raw)


### Summary, Data envelopment analysis(DEA) and Visualization ####

sapply(Data_Raw, function(x) sum(is.na(x))) # Finding NA
sapply(Data_Raw, function(x) length(unique(x)))

g1 <- ggplot(Data_Raw, 
             aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="Yes",]$MonthlyIncome),color = "blue")+
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="No",]$MonthlyIncome),color = "red")


g2 <- ggplot(Data_Raw, 
             aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="Yes",]$HourlyRate),color = "blue")+
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="No",]$HourlyRate),color = "red")


g3 <- ggplot(Data_Raw, 
             aes(x = YearsAtCompany, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="Yes",]$YearsAtCompany),color = "blue")+
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="No",]$YearsAtCompany),color = "red")


g4 <- ggplot(Data_Raw, 
             aes(x = TotalWorkingYears, fill = Attrition)) + 
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="Yes",]$TotalWorkingYears),color = "blue")+
  geom_vline(xintercept = mean(Data_Raw[Data_Raw$Attrition=="No",]$TotalWorkingYears),color = "red")

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

# BusinessTravel
g1 <- ggplot(Data_Raw,aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

Travel_Summary <- Data_Raw %>% select(Attrition, BusinessTravel) %>% group_by(Attrition, BusinessTravel) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

BusinessTravel_Count <- ggplot(Travel_Summary, aes(x=Attrition, y=Count, fill=BusinessTravel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~BusinessTravel) + 
  geom_label(aes(label=Count, fill = BusinessTravel), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Attrition by Number of Business Travel")


BusinessTravel_Percentage <- ggplot(Travel_Summary, aes(x=Attrition, y=Precent, fill=BusinessTravel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~BusinessTravel) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = BusinessTravel), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Attrition by Percentage of Business Travel")



ggarrange(g1,ggarrange(BusinessTravel_Count, BusinessTravel_Percentage,nrow=2), nrow=2)


# Department
g1 <- ggplot(Data_Raw,aes(x = Department, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

Department_Summary <- Data_Raw %>% select(Attrition, Department) %>% group_by(Attrition, Department) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

Department_Count <- ggplot(Department_Summary, aes(x=Attrition, y=Count, fill=Department)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Department) + 
  geom_label(aes(label=Count, fill = Department), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number Attrition by  Department")


Department_Percentage <- ggplot(Department_Summary, aes(x=Attrition, y=Precent, fill=Department)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Department) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = Department), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by  Department")



ggarrange(g1,ggarrange(Department_Count, Department_Percentage,nrow=2), nrow=2)


# Education
g1 <- ggplot(Data_Raw,aes(x = Education, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

Education_Summary <- Data_Raw %>% select(Attrition, Education) %>% group_by(Attrition, Education) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

Education_Count <- ggplot(Education_Summary, aes(x=Attrition, y=Count, fill=Education)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Education) + 
  geom_label(aes(label=Count, fill = Education), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by Education")


Education_Percentage <- ggplot(Education_Summary, aes(x=Attrition, y=Precent, fill=Education)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Education) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = Education), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by Education")


ggarrange(g1,ggarrange(Education_Count, Education_Percentage,nrow=2), nrow=2)

# EducationField
g1 <- ggplot(Data_Raw,aes(x = EducationField, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

EducationField_Summary <- Data_Raw %>% select(Attrition, EducationField) %>% group_by(Attrition, EducationField) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

EducationField_Count <- ggplot(EducationField_Summary, aes(x=Attrition, y=Count, fill=EducationField)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~EducationField) + 
  geom_label(aes(label=Count, fill = EducationField), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by EducationField")


EducationField_Percentage <- ggplot(EducationField_Summary, aes(x=Attrition, y=Precent, fill=EducationField)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~EducationField) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = EducationField), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by EducationField")


ggarrange(g1,ggarrange(EducationField_Count, EducationField_Percentage,nrow=2), nrow=2)


# Gender
g1 <- ggplot(Data_Raw,aes(x = Gender, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

Gender_Summary <- Data_Raw %>% select(Attrition, Gender) %>% group_by(Attrition, Gender) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

Gender_Count <- ggplot(Gender_Summary, aes(x=Attrition, y=Count, fill=Gender)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Gender) + 
  geom_label(aes(label=Count, fill = Gender), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by Gender")


Gender_Percentage <- ggplot(Gender_Summary, aes(x=Attrition, y=Precent, fill=Gender)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~Gender) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = Gender), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by Gender")


ggarrange(g1,ggarrange(Gender_Count, Gender_Percentage,nrow=2), nrow=2)


# JobLevel
g1 <- ggplot(Data_Raw,aes(x = JobLevel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

JobLevel_Summary <- Data_Raw %>% select(Attrition, JobLevel) %>% group_by(Attrition, JobLevel) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

JobLevel_Count <- ggplot(JobLevel_Summary, aes(x=Attrition, y=Count, fill=JobLevel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobLevel) + 
  geom_label(aes(label=Count, fill = JobLevel), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by JobLevel")


JobLevel_Percentage <- ggplot(JobLevel_Summary, aes(x=Attrition, y=Precent, fill=JobLevel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobLevel) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = JobLevel), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by JobLevel")



ggarrange(g1,ggarrange(JobLevel_Count, JobLevel_Percentage,nrow=2), nrow=2)


# JobRole
ggplot(Data_Raw,aes(x = JobRole, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

JobRole_Summary <- Data_Raw %>% select(Attrition, JobRole) %>% group_by(Attrition, JobRole) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

JobRole_Count <- ggplot(JobRole_Summary, aes(x=Attrition, y=Count, fill=JobRole)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobRole) + 
  geom_label(aes(label=Count, fill = JobRole), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by JobRole")


JobRole_Percentage <- ggplot(JobRole_Summary, aes(x=Attrition, y=Precent, fill=JobRole)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobRole) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = JobRole), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by JobRole")



grid.arrange(JobRole_Count, JobRole_Percentage, nrow=2)


# MaritalStatus
ggplot(Data_Raw,aes(x = MaritalStatus, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

MaritalStatus_Summary <- Data_Raw %>% select(Attrition, MaritalStatus) %>% group_by(Attrition, MaritalStatus) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

MaritalStatus_Count <- ggplot(MaritalStatus_Summary, aes(x=Attrition, y=Count, fill=MaritalStatus)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~MaritalStatus) + 
  geom_label(aes(label=Count, fill = MaritalStatus), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by MaritalStatus")


MaritalStatus_Percentage <- ggplot(MaritalStatus_Summary, aes(x=Attrition, y=Precent, fill=MaritalStatus)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~MaritalStatus) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = MaritalStatus), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by MaritalStatus")



grid.arrange(MaritalStatus_Count, MaritalStatus_Percentage, nrow=2)


# OverTime
g1 <- ggplot(Data_Raw,aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

OverTime_Summary <- Data_Raw %>% select(Attrition, OverTime) %>% group_by(Attrition, OverTime) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

OverTime_Count <- ggplot(OverTime_Summary, aes(x=Attrition, y=Count, fill=OverTime)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~OverTime) + 
  geom_label(aes(label=Count, fill = OverTime), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by OverTime")


OverTime_Percentage <- ggplot(OverTime_Summary, aes(x=Attrition, y=Precent, fill=OverTime)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~OverTime) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = OverTime), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by OverTime")



ggarrange(g1,ggarrange(OverTime_Count, OverTime_Percentage,nrow=2), nrow=2)



# StockOptionLevel
g1 <- ggplot(Data_Raw,aes(x = StockOptionLevel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

StockOptionLevel_Summary <- Data_Raw %>% select(Attrition, StockOptionLevel) %>% group_by(Attrition, StockOptionLevel) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

StockOptionLevel_Count <- ggplot(StockOptionLevel_Summary, aes(x=Attrition, y=Count, fill=StockOptionLevel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~StockOptionLevel) + 
  geom_label(aes(label=Count, fill = StockOptionLevel), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by StockOptionLevel")


StockOptionLevel_Percentage <- ggplot(StockOptionLevel_Summary, aes(x=Attrition, y=Precent, fill=StockOptionLevel)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~StockOptionLevel) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = StockOptionLevel), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by StockOptionLevel")


ggarrange(g1,ggarrange(StockOptionLevel_Count, StockOptionLevel_Percentage,nrow=2), nrow=2)

# JobInvolvement
g1 <- ggplot(Data_Raw,aes(x = JobInvolvement, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

JobInvolvement_Summary <- Data_Raw %>% select(Attrition, JobInvolvement) %>% group_by(Attrition, JobInvolvement) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

JobInvolvement_Count <- ggplot(JobInvolvement_Summary, aes(x=Attrition, y=Count, fill=JobInvolvement)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobInvolvement) + 
  geom_label(aes(label=Count, fill = JobInvolvement), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by JobInvolvement")


JobInvolvement_Percentage <- ggplot(JobInvolvement_Summary, aes(x=Attrition, y=Precent, fill=JobInvolvement)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobInvolvement) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = JobInvolvement), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by JobInvolvement")


ggarrange(g1,ggarrange(JobInvolvement_Count, JobInvolvement_Percentage,nrow=2), nrow=2)

# JobSatisfaction
g1 <- ggplot(Data_Raw,aes(x = JobSatisfaction, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat="count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count",vjust = -.5) +
  facet_grid(~Attrition) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

JobSatisfaction_Summary <- Data_Raw %>% select(Attrition, JobSatisfaction) %>% group_by(Attrition, JobSatisfaction) %>% 
  summarize(Count=n()) %>% mutate(Precent=round(prop.table(Count),2) * 100)

JobSatisfaction_Count <- ggplot(JobSatisfaction_Summary, aes(x=Attrition, y=Count, fill=JobSatisfaction)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobSatisfaction) + 
  geom_label(aes(label=Count, fill = JobSatisfaction), colour = "white")  + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Number of Attrition by JobSatisfaction")


JobSatisfaction_Percentage <- ggplot(JobSatisfaction_Summary, aes(x=Attrition, y=Precent, fill=JobSatisfaction)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~JobSatisfaction) + 
  geom_label(aes(label=paste0(Precent, "%"), fill = JobSatisfaction), colour = "white") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Percentage Attrition by JobSatisfaction")


ggarrange(g1,ggarrange(JobSatisfaction_Count, JobSatisfaction_Percentage,nrow=2), nrow=2)


### Balanced Dataset (Resampleing (Synthetic Minority Over-sampling Technique (SMOTE))) ####

Smoted_Data <- SMOTE(Attrition~., Data_Raw)




g1 <- ggplot(Data_Raw, aes(x = Age, y= HourlyRate, shape = Attrition, color=Attrition)) + 
  ggtitle("Original Data") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point()

g2 <- ggplot(Smoted_Data, aes(x = Age, y= HourlyRate, shape = Attrition, color=Attrition)) + 
  ggtitle("SMOTE Data") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point()
grid.arrange(g1, g2, ncol = 2, nrow = 1)

table(Data_Raw$Attrition)
table(Smoted_Data$Attrition)


## Random ##
## 75% of the sample size

levels(Smoted_Data$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(Smoted_Data$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
Smoted_Data <- Smoted_Data[c(-9,-10,-22,-27)]

smp_size <- floor(0.75 * nrow(Smoted_Data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Smoted_Data)), size = smp_size)

train_smoted <- Smoted_Data[train_ind, ]
test_smoted <- Smoted_Data[-train_ind, ]

### Data Cleaning and Spilliting ####

levels(Data_Raw$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(Data_Raw$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
Data_Raw <- Data_Raw[c(-9,-10,-22,-27)]

## Random ##
## 75% of the sample size
smp_size <- floor(0.75 * nrow(Data_Raw))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Data_Raw)), size = smp_size)

train_org <- Data_Raw[train_ind, ]
test_org <- Data_Raw[-train_ind, ]

### Modelling ####

### Decision Tree ####

## Orginal Data

Decision_Tree <- rpart(Attrition ~ ., data=train_org)

rpart.plot(Decision_Tree, 
           type = 5, 
           extra = 100, 
           tweak = 2, 
           fallen.leaves = F)


tree_pred <- predict(Decision_Tree, Data_Raw[-train_ind,], type="class")
with(Data_Raw[-train_ind,], table(tree_pred, Data_Raw[-train_ind,]$Attrition))

pROC <- roc(as.numeric(test_org$Attrition), as.numeric(tree_pred),
            smoothed = TRUE,
            # arguments for ci
            ci=TRUE, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC)
plot(sens.ci, type="shape", col="lightblue")

plot(sens.ci, type="bars")


# confusion matrix
Decision_Tree_Confusion_Matrix <- confusionMatrix(tree_pred, Data_Raw[-train_ind,]$Attrition, mode="prec_recall")

Decision_Tree_Confusion_Matrix
Decision_Tree_Confusion_Matrix$byClass["Precision"]
Decision_Tree_Confusion_Matrix$byClass["Recall"]
Decision_Tree_Confusion_Matrix$byClass["F1"]


var_imp_tree <- data.frame(Decision_Tree$variable.importance)
var_imp_tree$features <- rownames(var_imp_tree)
var_imp_tree <- var_imp_tree[, c(2, 1)]
var_imp_tree$importance <- round(var_imp_tree$Decision_Tree.variable.importance, 2)
var_imp_tree$Decision_Tree.variable.importance <- NULL

colorCount <- length(unique(var_imp_tree$features))

ggplot(var_imp_tree, aes(x=reorder(features, importance), y=importance, fill=features)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  theme(legend.position="none")  + 
  geom_label(aes(label=paste0(importance, "%")), colour = "white") + 
  labs(title="Feature Importance for our Decision Tree Model", x="Features", y="Importance") 




dtreepr <- prune.rpart(Decision_Tree, cp=0)
dtreepr
predspr <- predict(dtreepr, test_org, type = "class")

rocvpr <- roc(as.numeric(test_org$Attrition), as.numeric(predspr))
rocvpr$auc

rpart.plot(dtreepr, 
           type = 5, 
           extra = 100, 
           tweak = 2, 
           fallen.leaves = F)

## Smoted Data

Decision_Tree <- rpart(Attrition ~ ., data=train_smoted)

rpart.plot(Decision_Tree, 
           type = 5, 
           extra = 100, 
           tweak = 2, 
           fallen.leaves = F)




tree_pred <- predict(Decision_Tree, test_smoted, type="class")
with(Smoted_Data[-train_ind,], table(tree_pred, test_smoted$Attrition))

pROC1 <- roc(as.numeric(test_smoted$Attrition), as.numeric(tree_pred),
             smoothed = TRUE,
             # arguments for ci
             ci=TRUE, ci.alpha=0.9, stratified=FALSE,
             # arguments for plot
             plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
             print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC)
plot(sens.ci, type="shape", col="lightblue")

plot(sens.ci, type="bars")


# confusion matrix
Decision_Tree_Confusion_Matrix <- confusionMatrix(tree_pred, test_smoted$Attrition, mode="prec_recall")

Decision_Tree_Confusion_Matrix
Decision_Tree_Confusion_Matrix$byClass["Precision"]
Decision_Tree_Confusion_Matrix$byClass["Recall"]
Decision_Tree_Confusion_Matrix$byClass["F1"]


var_imp_tree <- data.frame(Decision_Tree$variable.importance)
var_imp_tree$features <- rownames(var_imp_tree)
var_imp_tree <- var_imp_tree[, c(2, 1)]
var_imp_tree$importance <- round(var_imp_tree$Decision_Tree.variable.importance, 2)
var_imp_tree$Decision_Tree.variable.importance <- NULL

colorCount <- length(unique(var_imp_tree$features))

ggplot(var_imp_tree, aes(x=reorder(features, importance), y=importance, fill=features)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  theme(legend.position="none")  + 
  geom_label(aes(label=paste0(importance, "%")), colour = "white") + 
  labs(title="Feature Importance for our Decision Tree Model", x="Features", y="Importance") 




dtreepr <- prune.rpart(Decision_Tree, cp=0)
dtreepr
predspr <- predict(dtreepr, test_smoted, type = "class")

rocvpr <- roc(as.numeric(test_smoted$Attrition), as.numeric(predspr))
rocvpr$auc

rpart.plot(dtreepr, 
           type = 5, 
           extra = 100, 
           tweak = 2, 
           fallen.leaves = F)


### SVM ####

## Orginal Data

## set the seed to make your partition reproducible


Tuned_SVM <- tune(svm, Attrition ~., data = train_org[1:100,],type="C-classification",kernel="polynomial",
                  ranges = list(gamma = 2^(-1:3), cost = 2^(2:4)),
                  tunecontrol = tune.control(nrepeat = 10, sampling = "cross", cross = 10))
Tuned_SVM

SVM <- train(Attrition~.,
             data=train_org, 
             method = "svmPoly", 
             gamma=Tuned_SVM$best.parameters$gamma, 
             Cost=Tuned_SVM$best.parameters$cost,
             trControl = trainControl(method = "cv", savePred=T, classProb=T))

# SVM <- svm(Attrition ~., data = train_org,
#            type="C-classification",kernel="linear", gamma=Tuned_SVM$best.parameters$gamma, Cost=Tuned_SVM$best.parameters$cost)

summary(SVM)
SVM_Predict <- predict(SVM,test_org)

with(Data_Raw[-train_ind,], table(SVM_Predict, test_org$Attrition))


# confusion matrix
SVM_Confusion_Matrix <- confusionMatrix(SVM_Predict, Data_Raw[-train_ind,]$Attrition, mode="prec_recall")

SVM_Confusion_Matrix
SVM_Confusion_Matrix$byClass["Precision"]
SVM_Confusion_Matrix$byClass["Recall"]
SVM_Confusion_Matrix$byClass["F1"]

ROC_SVM <- roc(as.numeric(test_org$Attrition), as.numeric(SVM_Predict))
ROC_SVM$auc


IMP_SVM <- varImp(SVM, scale = FALSE)
IMP_SVM

plot(IMP_SVM)

## Smoted Data

## set the seed to make your partition reproducible

Tuned_SVM <- tune(svm, Attrition ~., data = train_smoted[1:100,],type="C-classification",kernel="polynomial",
                  ranges = list(gamma = 2^(-1:3), cost = 2^(2:4)),
                  tunecontrol = tune.control(nrepeat = 10, sampling = "cross", cross = 10))
Tuned_SVM


SVM <- train(Attrition~.,
             data=train_smoted, 
             method = "svmPoly", 
             gamma=Tuned_SVM$best.parameters$gamma, 
             Cost=Tuned_SVM$best.parameters$cost,
             trControl = trainControl(method = "cv", savePred=T, classProb=T))

# SVM <- svm(Attrition ~., data = train_smoted,
#            type="C-classification",kernel="linear", gamma=Tuned_SVM$best.parameters$gamma, Cost=Tuned_SVM$best.parameters$cost)

summary(SVM)
SVM_Predict <- predict(SVM,test_smoted)

with(Data_Raw[-train_ind,], table(SVM_Predict, test_smoted$Attrition))


# confusion matrix
SVM_Confusion_Matrix <- confusionMatrix(SVM_Predict, test_smoted$Attrition, mode="prec_recall")

SVM_Confusion_Matrix
SVM_Confusion_Matrix$byClass["Precision"]
SVM_Confusion_Matrix$byClass["Recall"]
SVM_Confusion_Matrix$byClass["F1"]

ROC_SVM <- roc(as.numeric(test_smoted$Attrition), as.numeric(SVM_Predict))
ROC_SVM$auc


IMP_SVM <- varImp(SVM, scale = FALSE)
IMP_SVM

plot(IMP_SVM)

### Neural Network ####

## Orginal Data
NNET <- train(train_org[,-2], train_org[,2],
              method = "nnet",
              preProcess = "range",
              tuneLength = 2,
              trace = FALSE,
              maxit = 100)


NNET_Predict <- predict(NNET,test_org)

with(Data_Raw[-train_ind,], table(NNET_Predict, test_org$Attrition))


# confusion matrix
NNET_Confusion_Matrix <- confusionMatrix(NNET_Predict, Data_Raw[-train_ind,]$Attrition, mode="prec_recall")

NNET_Confusion_Matrix
NNET_Confusion_Matrix$byClass["Precision"]
NNET_Confusion_Matrix$byClass["Recall"]
NNET_Confusion_Matrix$byClass["F1"]

ROC_NNET <- roc(as.numeric(test_org$Attrition), as.numeric(NNET_Predict))
ROC_NNET$auc

IMP_NNET <- varImp(NNET)
IMP_NNET

plot(IMP_NNET)

## Smoted Data
NNET <- train(train_smoted[,-2], train_smoted[,2],
              method = "nnet",
              preProcess = "range",
              tuneLength = 2,
              trace = FALSE,
              maxit = 100)


NNET_Predict <- predict(NNET,test_smoted)

with(test_smoted, table(NNET_Predict, test_smoted$Attrition))


# confusion matrix
NNET_Confusion_Matrix <- confusionMatrix(NNET_Predict, test_smoted$Attrition, mode="prec_recall")

NNET_Confusion_Matrix
NNET_Confusion_Matrix$byClass["Precision"]
NNET_Confusion_Matrix$byClass["Recall"]
NNET_Confusion_Matrix$byClass["F1"]

ROC_NNET <- roc(as.numeric(test_smoted$Attrition), as.numeric(NNET_Predict))
ROC_NNET$auc

IMP_NNET <- varImp(NNET)
IMP_NNET

plot(IMP_NNET)

### Logistic Regression ####

## Orginal Data

LR <- train(Attrition ~.,data = train_org,
            method = "regLogistic",
            trControl = trainControl(method = "cv", savePred=T, classProb=T))


LR_Predict <- predict(LR,test_org)

with(Data_Raw[-train_ind,], table(LR_Predict, test_org$Attrition))


# confusion matrix
LR_Confusion_Matrix <- confusionMatrix(LR_Predict, Data_Raw[-train_ind,]$Attrition, mode="prec_recall")

LR_Confusion_Matrix
LR_Confusion_Matrix$byClass["Precision"]
LR_Confusion_Matrix$byClass["Recall"]
LR_Confusion_Matrix$byClass["F1"]

ROC_LR <- roc(as.numeric(test_org$Attrition), as.numeric(LR_Predict))
ROC_LR$auc

IMP_LR <- varImp(LR)
IMP_LR

plot(IMP_LR)


## Smoted Data

LR <- train(Attrition ~.,data = train_smoted,
            method = "regLogistic",
            trControl = trainControl(method = "cv", savePred=T, classProb=T))


LR_Predict <- predict(LR,test_smoted)

with(test_smoted, table(LR_Predict, test_smoted$Attrition))


# confusion matrix
LR_Confusion_Matrix <- confusionMatrix(LR_Predict, test_smoted$Attrition, mode="prec_recall")

LR_Confusion_Matrix
LR_Confusion_Matrix$byClass["Precision"]
LR_Confusion_Matrix$byClass["Recall"]
LR_Confusion_Matrix$byClass["F1"]

ROC_LR <- roc(as.numeric(test_smoted$Attrition), as.numeric(LR_Predict))
ROC_LR$auc

IMP_LR <- varImp(LR)
IMP_LR

plot(IMP_LR)

### KNN ####

## Orginal Data

KNN <- train(Attrition ~.,data = train_org,
             method = "knn",
             preProcess = c("center", "scale"),
             tuneLength = 10,
             trControl = trainControl(method = "cv"))


KNN_Predict <- predict(KNN,test_org)

with(Data_Raw[-train_ind,], table(KNN_Predict, test_org$Attrition))


# confusion matrix
KNN_Confusion_Matrix <- confusionMatrix(KNN_Predict, Data_Raw[-train_ind,]$Attrition, mode="prec_recall")

KNN_Confusion_Matrix
KNN_Confusion_Matrix$byClass["Precision"]
KNN_Confusion_Matrix$byClass["Recall"]
KNN_Confusion_Matrix$byClass["F1"]

ROC_KNN <- roc(as.numeric(test_org$Attrition), as.numeric(KNN_Predict))
ROC_KNN$auc

IMP_KNN <- varImp(KNN)
IMP_KNN

plot(IMP_KNN)

## Smoted Data

KNN <- train(Attrition ~.,data = train_smoted,
             mmethod = "knn",
             preProcess = c("center", "scale"),
             tuneLength = 10,
             trControl = trainControl(method = "cv"))

KNN_Predict <- predict(KNN,test_smoted)

with(test_smoted, table(KNN_Predict, test_smoted$Attrition))


# confusion matrix
KNN_Confusion_Matrix <- confusionMatrix(KNN_Predict, test_smoted$Attrition, mode="prec_recall")

KNN_Confusion_Matrix
KNN_Confusion_Matrix$byClass["Precision"]
KNN_Confusion_Matrix$byClass["Recall"]
KNN_Confusion_Matrix$byClass["F1"]

ROC_KNN <- roc(as.numeric(test_smoted$Attrition), as.numeric(KNN_Predict))
ROC_KNN$auc

IMP_KNN <- varImp(KNN)
IMP_KNN

plot(IMP_KNN)

### Random Forest####

## Orginal Data

RF <- train(Attrition ~.,data = train_org,
            method = "rf",
            ntree = 10,
            trControl = trainControl(method = "none"))

RF_Predict <- predict(RF,test_org)

with(test_org, table(RF_Predict, test_org$Attrition))


# confusion matrix
RF_Confusion_Matrix <- confusionMatrix(RF_Predict, test_org$Attrition, mode="prec_recall")

RF_Confusion_Matrix
RF_Confusion_Matrix$byClass["Precision"]
RF_Confusion_Matrix$byClass["Recall"]
RF_Confusion_Matrix$byClass["F1"]

ROC_RF <- roc(as.numeric(test_org$Attrition), as.numeric(RF_Predict))
ROC_RF$auc

IMP_RF <- varImp(RF)
IMP_RF

plot(IMP_RF)

## Smoted Data

RF <- train(Attrition ~.,data = train_smoted,
            method = "rf",
            ntree = 10,
            trControl = trainControl(method = "none"))

RF_Predict <- predict(RF,test_smoted)

with(test_smoted, table(RF_Predict, test_smoted$Attrition))


# confusion matrix
RF_Confusion_Matrix <- confusionMatrix(RF_Predict, test_smoted$Attrition, mode="prec_recall")

RF_Confusion_Matrix
RF_Confusion_Matrix$byClass["Precision"]
RF_Confusion_Matrix$byClass["Recall"]
RF_Confusion_Matrix$byClass["F1"]

ROC_RF <- roc(as.numeric(test_smoted$Attrition), as.numeric(RF_Predict))
ROC_RF$auc

IMP_RF <- varImp(RF)
IMP_RF

plot(IMP_RF)
