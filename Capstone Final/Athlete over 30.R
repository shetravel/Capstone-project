library(dplyr)
library(ggplot2)

load(athlete_events)
str(athlete_events)

#check which columns have NA 
colnames(athlete_events)[colSums(is.na(athlete_events)) > 0]

#fix NA in Age by taking mean
Athlete_event_whole <- mutate(athlete_events, Age = ifelse(is.na(Age), mean(Age,na.rm = TRUE), Age))

load('athlete_event_whole.RData')

#clean up the NA for medals
Athlete_event_whole <- mutate(Athlete_event_whole, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))

#create binary results for Age and Medal
Athlete_event_whole$Age_over_30 <- ifelse(Athlete_event_whole$Age >= 30,1,0)
Athlete_event_whole$Medal_value <- ifelse(Athlete_event_whole$Medal == "No Medal earned", 0,1)

#Remove columns that are not needed
Athlete_event_whole$Height = NULL
Athlete_event_whole$Weight = NULL
Athlete_event_whole$Event = NULL

#Clean up major country changes of German and Russia to combine
Athlete_Top_Teams <- mutate(Athlete_event_whole, Team = ifelse(Team == "Soviet Union", "Russia", Team))
Athlete_Top_Teams <- mutate(Athlete_event_whole, Team = ifelse(Team == "East Germany", "Germany", Team))
Athlete_Top_Teams <- mutate(Athlete_event_whole, Team = ifelse(Team == "West Germany", "Germany", Team))

#Keep only rows of top 7 teams
Athlete_Top_Teams <- subset(Athlete_Top_Teams, Team == "United States" | Team == "France" | Team == "Italy" | Team =="Great Britain" | 
                              Team == "Germany" | Team == "Russia" | Team == "Japan")

#compare rowing vs equestrians if average age has moved up
Compare_Row_Equest_all <- subset(Athlete_event_whole, Sport == "Rowing" | Sport == "Equestrianism")
table(Compare_Row_Equest_all$Sport)
table(Compare_Row_Equest_all$Year)


#review the mean age
mean(Athlete_event_whole$Age)
mean(Athlete_Top_Teams$Age)
mean(Compare_Row_Equest_all$Age)




# Which events in your dataset have the highest proportion of participants aged over 30?
Athlete_event_whole %>%
  group_by(Sport) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))
#Which countries have the highest proportion of participants aged over 30? 
Athlete_event_whole %>%
  group_by(NOC) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#the highest proportion of participants aged over 30 looking at Rowing and Equestrian
Compare_Row_Equest_all %>%
  group_by(Sport) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#the highest proportion of participants aged over 30 for top 7 countries
Athlete_Top_Teams %>%
  group_by(Team) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

# Which events in your dataset have the highest proportion of participants aged over 30 with medals?
Athlete_event_whole %>%
  group_by(Medal) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#Percentage of those over 30 over the years
Athlete_event_whole %>%
  group_by(Year) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#Percentage of those over 30 over the years that earn a medal
Athlete_event_whole %>%
  group_by(Year,Medal_value) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(Year))


# Split out the Train and Test groups
Train_Equ <- subset(Athlete_event_whole, Sport == "Equestrianism")
Test_Row <- subset(Athlete_event_whole, Sport == "Rowing")

str(Train_Equ)
colnames(Train_Equ)[colSums(is.na(Train_Equ)) > 0]

#Mean Age over time of Training set
mean(Train_Equ$Age)

#review of the mean
meanfun <- function(x) {
  c(min = min(x), max =max(x), 
    mean =mean(x))}

meanfun2 <- function(x) {
  c(min = min(x), max =max(x), 
  mean =mean(x), median =median(x), 
    std = sd(x))}

#Age.Year.mean <- tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year, meanfun)
#Age_Year_mean<- Mean.Age = c(27.5,26.81,25.34,24.48,27.87,28.36,28.47,29.93,28.08,29.33,29.44,28.32,29.11,27.36,27.28,27.43,27.73,26.47,25.7,27.05,27.72,28.35,29.04,29.76,29.74,30.55,31.28,31.09)
#Age_Year_mean<- Year = c(1900,1904,1906, 1908, 1912, 1920, 1924, 1928, 1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)
#table(Age.Year.mean)
#tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year, meanfun)
#Mean.Age <- {c(mean = mean(Compare_Row_Equest_all$Age), median=median(Compare_Row_Equest_all$Age))} %% tapply(Compare_Row_Equest_all$Age, Compare_Row_Equest_all$Year)
#    table(Mean.Age) 

tapply(Athlete_event_whole$Age, Athlete_event_whole$Year, meanfun)

Train_Equ %>%
  group_by(Year) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#Equestrian review of over 30 earning medal over the years
Train_Equ %>%
  group_by(Year,Medal_value) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
   ) %>%
  mutate(
      percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(Year))

#mean age over the years of training set
tapply(Train_Equ$Age, Train_Equ$Year, meanfun)

library(plyr)
## test plots to see patterns
library(ggplot2)

#plot to review age & sport for all
ggplot(Athlete_event_whole, aes(x=Age, y=Sport)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line


#ggplot(Age.Year.mean, aes(x=Age, y=Sport)) +
#  geom_point(shape=1) +    # Use hollow circles
#  geom_smooth(method=lm)   # Add linear regression line

#plot to review age & Sport for all with different output
ggplot(Athlete_event_whole, aes(x=Age, y=Sport)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity
#plot to add color on over 30
ggplot(Athlete_Top_Teams, aes(x=Sport, y=Team, color >= 30(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity


ggplot(Athlete_event_whole, aes(x=Team, y=Sport)) +
  geom_point(aes(color = Age))
#plot team and Age and reflect on if earned a medal
ggplot(Train_Equ, aes(x=Team, y=Age)) +
  geom_point(aes(color = Medal_value))

#plot to review Rowing & Equestrian based on more recent years
ggplot(Compare_Row_Equest_all, aes(x=Year>1960, y=(mean(Age)))) +
  geom_point()

#plot to review year and team
ggplot(Athlete_event_whole, aes(x=Year, y=Team)) +
  geom_point()


#plot to compare sex vs age
ggplot(Train_Equ, aes(x=Sex, y=Age)) +
  geom_point(aes(color = Medal_value))

#plot over time
ggplot(Athlete_event_whole, aes(x=Age, y=Year)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
#plot over time 2
ggplot(Athlete_event_whole, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

ggplot(Compare_Row_Equest_all, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

ggplot(Athlete_Top_Teams, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
#plot over time - training set
ggplot(Train_Equ, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

#plot over time 3
ggplot(Athlete_event_whole, aes(x=Year, y= Age_over_30)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
#Line plot over time Train set
ggplot(Train_Equ, aes(x=Year, y= Age_over_30)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

#line plot over Year, Age
ggplot(Train_Equ, aes(Year, Age, group=Age_over_30)) +
  geom_line(color= "blue")+
  geom_point() 

#bar chart looking at Year by Age
ggplot(Athlete_event_whole, aes(x = Year, fill = Age)) +
  geom_bar(position = "dodge")

#bar chart to look at Age by team
ggplot(Athlete_event_whole, aes(x = Age, fill = Team)) +
  geom_bar(position = "dodge")

#bar chart to look at over 30 by team
ggplot(Athlete_event_whole, aes(x = Age_over_30, fill = Team)) +
  geom_bar(position = "dodge")

#bar chart looking at Age over 30 by sport
ggplot(Athlete_event_whole, aes(x = Sport, fill = Age_over_30)) +
  geom_bar(position = "dodge")
#bar chart looking at year by age over 30
ggplot(Athlete_event_whole, aes(x = Year, fill = Age_over_30)) +
  geom_bar(position = "dodge")
#bar chart Training set looking at year by age over 30
ggplot(Train_Equ, aes(x = Year, fill = Age_over_30)) +
  geom_bar(position = "dodge")

#line chart to review mean age
ggplot(Train_Equ, aes(Year, Age, group=mean(Age))) +
  geom_line(color = "blue") +
  geom_point()

#plot of Year and team data
ggplot(Athlete_event_whole, aes(Year, Team)) +
  geom_line()  

ggplot(Athlete_event_whole, aes(Year, Age, group=Team)) +
  geom_line(color = "blue")+
  geom_point() 
#histrogram of Age & medals
ggplot(Athlete_event_whole, aes(Age, fill = factor(Medal))) +
  geom_histogram(binwidth = 1)
#histogram on Age & medals of training set
ggplot(Train_Equ, aes(Age, fill = factor(Medal))) +
  geom_histogram(binwidth = 1)

#bar plot on Summer / Winter age
ggplot(Athlete_event_whole, aes (x = Year, fill= factor(Age))) +
  geom_histogram(binwidth = 1) +
    facet_grid(Season ~ .)
#bar plot on Summer / Winter over 30
ggplot(Athlete_event_whole, aes (x = Year, fill= factor(Age_over_30))) +
  geom_histogram(binwidth = 1) +
  facet_grid(Season ~ .)
#bar plot on train set
ggplot(Train_Equ, aes (x = Year, fill= factor(Age_over_30))) +
  geom_histogram(binwidth = 1) +
  facet_grid(Medal_value ~ .)

#plot to add color on over 30
ggplot(Train_Equ, aes(x=Sport, y=Team, color >= 30(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity

#simple scatterplot
ggplot(Train_Equ, aes(Year, Team, color = Age)) +
  geom_point()
ggplot(Train_Equ, aes(Year, Age, color = Medal_value)) +
  geom_point(size = 3, alpha = 0.5)


#plot for Training set specifying years and NOC
ggplot(Train_Equ, aes(Year, Age, color = NOC), xlim = c(1950, 2016)) +
  geom_point(size = 3, alpha = 0.5)

#plot breakdown by Medal
ggplot(Train_Equ, aes(Year, Age)) +
  geom_point() +
  facet_grid(Medal ~.)
#plot for mean age
ggplot(Athlete_event_whole, aes(Year, mean(Age))) +
  geom_point() +
  facet_grid(Medal ~.)

#mean age train
ggplot(Train_Equ, aes(Year, mean(Age))) +
  geom_point() +
  facet_grid(Medal ~.)

# medal over 30
ggplot(Athlete_event_whole, aes(Team, Age)) +
  geom_point() +
  facet_grid(Medal ~.)

# medal over 30 Train set
ggplot(Train_Equ, aes(Team, Age)) +
  geom_point() +
  facet_grid(Medal ~.)

#scatterplot to review Year, Age, Medal
posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Train_Equ, aes(x = Year, y = Age, color = Medal)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(.~Team)

#create tables for sport over 30 and medals by sport over 30
table(Athlete_event_whole$Age_over_30, Athlete_event_whole$Sport, Athlete_event_whole$Year >=1980)
Sport_over30 <- table(Athlete_event_whole$Age_over_30, Athlete_event_whole$Sport)
Medal_Sport_over30 <- table(Athlete_event_whole$Age_over_30, Athlete_event_whole$Sport, Top_7_Team$Medal != "No Medal earned")
Medal_Sport_over30_recent <-table(Athlete_event_whole$Age_over_30, Athlete_event_whole$Sport, Top_7_Team$Medal != "No Medal earned", Top_7_Team$Year >= 1980)
table(Athlete_event_whole$Age_over_30 == "Yes", Athlete_event_whole$Sport)

qt(c(.025, .975), df=5)   # 5 degrees of freedom
qchisq(.95, df=7)        # 7 degrees of freedom 
pnorm(30, mean=72, sd=15.2, lower.tail=FALSE) 

save(athlete_events, file="athlete_events.RData")
save(Athlete_event_whole, file="athlete_event_whole.RData")
save(Athlete_Top_Teams , file="athlete_Top_Teams.RData")
save(Train_Equ, file="Train_Equ.RData")
save(Test_Row, file="Test_Row.RData")
save(Compare_Row_Equest_all, file="Compare_Row_Equest_all.RData")
save(meanfun, file ="meanfun.Rdata")

#linear regression
cor(Train_Equ$Age_over_30, Train_Equ$Medal_value)

age.out <- lm(Age >30 ~Sport == "Equestrianism",
               data=Athlete_event_whole)
summary(age.out)
age.out$residuals
SSE = sum(age.out$residuals^2)
SSE

#linear regression for Training set
e.age.out <- lm(Age >30 ~Medal_value,
              data=Train_Equ)
summary(e.age.out)
e.age.out$residuals
SSE = sum(e.age.out$residuals^2)
SSE

#linear regression adding NOC and age to improve R squared
age.out9 <- lm(Age_over_30 ~ Medal_value + NOC + Age,
                 data=Train_Equ)
summary(age.out9)
age.out9$residuals
SSE = sum(age.out9$residuals^2)
SSE


age.out12 <- lm(Age ~ Sport + Medal_value + Sex + Year,
                data=Athlete_event_whole)
summary(age.out12)

age.out12$residuals
SSE = sum(age.out12$residuals^2)
SSE

#linear regression using glm function for training set
gage.out7 <- glm(Age_over_30 ~ Medal_value,
               data=Train_Equ)
summary(gage.out7)
coef(summary(gage.out7))

EquMatrix = as.matrix(Train_Equ)
EquVector = as.vector((EquMatrix))
str(EquVector)
distances = dist(EquVector, method="euclidean")
clusterathlete = hclust(distances, method = "ward")
plot(clusterathlete)
rect.hclust(clusterathlete, k=3, border="red")
EquClusters = cutree(clusterathlete, k=3)
tapply(Train_Equ$Age, EquClusters, mean)
tapply(Train_Equ$Medal_value, EquClusters, mean)
subset(Train_Equ, Age >= 30)
EquClusters[713]
EquClusters[141]
subset(Train_Equ, Medal_value ==1)
EquClusters[2894]
EquCluster2 = subset(Train_Equ, EquClusters ==2)
EquCluster2$Age[1:25]
EquCluster2$Medal_value[1:25]

suppressWarnings(as.numeric())





#Test validaion
mean(Test_Row$Age)

#Reflecting increase over last 20 years
Test_Row %>%
  group_by(Year) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(percent_over_30))

#Percentage of those over 30 earning a medal through the years
Test_Row %>%
  group_by(Year,Medal_value) %>%
  summarise(
    participants = n(),
    over_30      = sum(Age > 30)
  ) %>%
  mutate(
    percent_over_30 = over_30/participants*100
  ) %>%
  arrange(desc(Year))

#mean age over the years of training set
tapply(Train_Equ$Age, Train_Equ$Year, meanfun)

#Key plots
#plot team and Age and reflect on if earned a medal
ggplot(Test_Row, aes(x=Team, y=Age)) +
  geom_point(aes(color = Medal_value))

#plot to compare sex vs age
ggplot(Test_Row, aes(x=Sex, y=Age)) +
  geom_point(aes(color = Medal_value))

#plot over time - test set
ggplot(Test_Row, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

 
#Line plot over time Test set
ggplot(Test_Row, aes(x=Year, y= Age_over_30)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 

#line plot over Year, Age
ggplot(Test_Row, aes(Year, Age, group=Age_over_30)) +
  geom_line(color= "blue")+
  geom_point() 

#bar chart Test set looking at year by age over 30
ggplot(Test_Row, aes(x = Year, fill = Age_over_30)) +
  geom_bar(position = "dodge")

#line chart to review mean age
ggplot(Test_Row, aes(Year, Age, group=mean(Age))) +
  geom_line(color = "blue") +
  geom_point()

#histogram on Age & medals of test set
ggplot(Test_Row, aes(Age, fill = factor(Medal))) +
  geom_histogram(binwidth = 1)

#bar plot on test set
ggplot(Test_Row, aes (x = Year, fill= factor(Age_over_30))) +
  geom_histogram(binwidth = 1) +
  facet_grid(Medal_value ~ .)

#plot to add color on over 30
ggplot(Test_Row, aes(x=Sport, y=Team, color >= 30(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity

#simple scatterplot
ggplot(Test_Row, aes(Year, Team, color = Age)) +
  geom_point()
ggplot(Test_Row, aes(Year, Age, color = Medal_value)) +
  geom_point(size = 3, alpha = 0.5)


#plot for Test set specifying years and NOC
ggplot(Test_Row, aes(Year, Age, color = NOC), xlim = c(1950, 2016)) +
  geom_point(size = 3, alpha = 0.5)

#plot breakdown by Medal
ggplot(Test_Row, aes(Year, Age)) +
  geom_point() +
  facet_grid(Medal ~.)


#mean age test
ggplot(Test_Row, aes(Year, mean(Age))) +
  geom_point() +
  facet_grid(Medal ~.)

# medal over 30 Test set
ggplot(Test_Row, aes(Team, Age)) +
  geom_point() +
  facet_grid(Medal ~.)

#scatterplot to review Year, Age, Medal
posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Test_Row, aes(x = Year, y = Age, color = Medal)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(.~Team)

#prediction
predictTest = predict(age.out12, newdata=Test_Row)
predictTest
SSE= sum((Test_Row$Age - predictTest)^2)
SST = sum((Test_Row$Age - mean(Train_Equ$Age))^2)
1-SSE/SST

#linear regression


age.out18 <- lm(Age >30 ~Sport == "Rowing",
              data=Athlete_event_whole)
summary(age.out18)
age.out18$residuals
SSE = sum(age.out18$residuals^2)
SSE

#linear regression for Training set
e.age.out20 <- lm(Age >30 ~Medal_value,
                data=Test_Row)
summary(e.age.out20)
e.age.out20$residuals
SSE = sum(e.age.out$residuals^2)
SSE

#linear regression adding NOC and age to improve R squared
age.out21 <- lm(Age_over_30 ~ Medal_value + NOC + Age,
               data=Test_Row)
summary(age.out21)
age.out21$residuals
SSE = sum(age.out21$residuals^2)
SSE


#linear regression using glm function for training set
gage.out22 <- glm(Age_over_30 ~ Medal_value,
                 data=Test_Row)
summary(gage.out22)
coef(summary(gage.out22))

