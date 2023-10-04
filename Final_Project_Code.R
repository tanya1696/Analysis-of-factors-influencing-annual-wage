# Library and Packages
install.packages("dslabs")
install.packages("ggridges")
library(dplyr)
library(ggplot2)

# Importing dataset 
destfile <- "/Users/tanyasharma/Desktop/Statistics and Data Science Books/Statistical Programming/Final Project/glassdoor_data.csv"
df <- read.csv(destfile)
head(df,5)

# Creating a new column
df["Salary"] <- df["BasePay"]+df["Bonus"]
summary(df)

plot(df$PerfEval, df$Bonus)
plot(df$PerfEval, df$BasePay)

gender <- c()
for (i in df$Gender){
  if (i == "Male"){
    gender = c(gender,0)
  }
  else{
    gender = c(gender,1)
  }
}

jobt <- c()
for (i in df$JobTitle){
  if (i == "Graphic Designer"){
    jobt = c(jobt,0)
  } else if (i == "Software Engineer"){
    jobt = c(jobt,1)
  } else if (i == "Warehouse Associater"){
    jobt = c(jobt,2)
  } else if (i == "IT"){
    jobt = c(jobt,3)
  } else if (i == "Sales Associate"){
    jobt = c(jobt,4)
  } else if (i == "Driver"){
    jobt = c(jobt,5)
  } else if (i == "Financial Analyst"){
    jobt = c(jobt,6)
  } else if (i == "Marketing Associate"){
    jobt = c(jobt,7)
  } else if (i == "Data Scientist"){
    jobt = c(jobt,8)
  } else{
    jobt = c(jobt,9)
  }
}

jobtitle <- c()
for (i in df$JobTitle){
  if (i == "Graphic Designer"){
    jobtitle = c(jobtitle,"GD")
  } else if (i == "Software Engineer"){
    jobtitle = c(jobtitle,"SE")
  } else if (i == "Warehouse Associater"){
    jobtitle = c(jobtitle,"WA")
  } else if (i == "IT"){
    jobtitle = c(jobtitle,"IT")
  } else if (i == "Sales Associate"){
    jobtitle = c(jobtitle,"SA")
  } else if (i == "Driver"){
    jobtitle = c(jobtitle,"Driver")
  } else if (i == "Financial Analyst"){
    jobtitle = c(jobtitle,"FA")
  } else if (i == "Marketing Associate"){
    jobtitle = c(jobtitle,"MA")
  } else if (i == "Data Scientist"){
    jobtitle = c(jobtitle,"DS")
  } else{
    jobtitle = c(jobtitle,"Manager")
  }
}


dept <- c()
for (i in df$Dept){
  if (i == "Operations"){
    dept = c(dept,0)
  } else if (i == "Management"){
    dept = c(dept,1)
  } else if (i == "Administration"){
    dept = c(dept,2)
  } else{
    dept = c(dept,3)
  }
}

edu <- c()
for (i in df$Education){
  if (i == "High School"){
    edu = c(edu,12)
  } else if (i == "College"){
    edu = c(edu,16)
  } else if (i == "Masters"){
    edu = c(edu,18)
  } else{
    edu = c(edu,22)
  }
}

unique(df$Seniority)

senior <- c()
for (i in df$Seniority){
  if (i == 1){
    senior = c(senior,0)
  } else if (i == 2){
    senior = c(senior,1)
  } else if (i == 3){
    senior = c(senior,2)
  } else if (i == 4){
    senior = c(senior,3)
  } else{
    senior = c(senior,4)
  }
}
senior

df$Seniority

# df_temp = data.frame(senior,salary)
# 
# p <- df_temp %>%
#   ggplot(aes(senior, salary))+
#   geom_boxplot()
# p

senior <- as.ordered(df$Seniority)
age <- df$Age
salary <- df$Salary
performance <- df$PerfEval
base <- df$BasePay

df_temp = data.frame(base,jobtitle)
pg_plot <- ggplot(df_temp, aes(x = jobtitle, y = base, fill = jobtitle)) +
  geom_boxplot()

pg_plot

# Change the legend labels
pg_plot +
  scale_fill_discrete(labels = c("Driver", "Data Scientist", "Financial Analyst", "Graphic Designer", "IT","Marketing Associate","Manager","Sales Associate","Software Engineer"))




model <- lm(salary~age+gender+jobt+edu+dept+senior+performance)
summary(model)
anova(model)

model2 <- lm(base~gender+jobt+edu+dept+senior+performance)
anova(model2)

par(mfrow=c(2,2))
plot(model)

model2 <- lm(base~age+gender+jobt+edu+dept+senior+performance)
summary(model2)

model_aov <- aov(base~gender+jobt+edu+dept+senior+performance)
summary(model_aov)

temp = multiWayAnova_(base~gender+jobt+edu+dept+senior+performance)
summary(temp)

model3 <- lm(basepay~age+gender+jobt+edu+dept+senior+performance)
summary(model3)

 

my_df <- data.frame(age,gender,jobt,edu,dept,senior,performance,salary)

install.packages("GGally")
library(GGally)
ggpairs(my_df)

install.packages("mctest")
library(mctest)

x = my_df[,1]
y=my_df[,7]
imcdiag(x,salary)


length(dept)
table(df$Gender)
table(df$PerfEval)
table(df$JobTitle,df$Gender)
table(df$Education,df$Gender)
table(df$Dept,df$Gender)
table(df$Seniority,df$Gender)
table(df$JobTitle,df$Dept)

# Trying some plots
p_df <- df %>% ggplot(aes(Age, Salary/1000))+
  xlab("Age")+
  ylab("Salary")
p_df + geom_point(aes(col = Gender))

p_df <- df %>% ggplot(aes(Dept, Salary/1000))+
  xlab("Dept")+
  ylab("Salary")
p_df + geom_point(aes(col = Gender))
 
p_df <- df %>% ggplot(aes(Age, Salary/1000))+
  xlab("Age")+
  ylab("Salary")
p_df + geom_point(aes(col = Education))

df %>% ggplot(aes(Gender))+geom_bar()
df %>% ggplot(aes(Education))+geom_bar()
df %>% ggplot(aes(Dept))+geom_bar()
df %>% ggplot(aes(df$PerfEval))+geom_bar()

df %>% ggplot(aes(BasePay))+geom_histogram(fill="blue", col="black")
df %>% ggplot(aes(log(Age)))+geom_histogram()

p2 <- ggplot(data=df, aes(x=BasePay, group=Gender)) +
  geom_density(adjust=1.5, alpha=.4) 

p2

#Distribution of Salary for females and Males
df %>%
  filter(Gender=="Female") %>%
  ggplot(aes(Salary))+
  geom_histogram(fill="Blue", col="Black")

df %>%
  filter(Gender=="Male") %>%
  ggplot(aes(Salary))+
  geom_histogram(bandwidth=0.5, fill="Blue", col="Black")

filter(df, Gender%in%c("Female","Male")) %>%
  ggplot(aes(Age, Salary, col=Education))+
  geom_point()+
  facet_grid(Education~Gender)

filter(df, Gender%in%c("Female","Male")) %>%
  ggplot(aes(Age, Salary, col=JobTitle))+
  geom_point()+
  facet_grid(JobTitle~Gender)


p <- df %>%
  ggplot(aes(Gender, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Education, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)

# Higher to lower
medians <- reorder(df$Education, -df$BasePay, median)
# medians <- with(chickwts, reorder(feed, -weight, median)) # Equivalent

b = boxplot(df$BasePay ~ medians, las = 2, xlab = "", ylab = "")+
par(mfrow = c(1, 1))
b



p <- df %>%
  ggplot(aes(JobTitle, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Seniority, BasePay))
  #geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Dept, BasePay))
  geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Dept, BasePay, color=Gender))+
  geom_boxplot()
p

p <- df %>%
  ggplot(aes(Education, BasePay, color=Gender))+
  geom_boxplot()
p



p <- df %>%
  ggplot(aes(PerfEval, Bonus))
  #geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(PerfEval, BasePay))
#geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Seniority, Bonus))
#geom_boxplot()
p+geom_point(alpha=0.5)

library(ggridges)
p <- df %>%
  ggplot(aes(Salary, JobTitle))
p+geom_density_ridges()

gender<- c("Male","Female")

jt1 <- df %>%
  filter(Gender=="Male") %>%
  pull(JobTitle)
  
jt2 <- df %>%
  filter(Gender=="Female") %>%
  pull(JobTitle)  

jt <- intersect(jt1,jt2)

#Work on this plot
df %>%
  filter(gender %in% gender, JobTitle %in% jt) %>%
  ggplot(aes(JobTitle, Salary))+
  geom_boxplot()+
  facet_grid(.~gender)

df %>%
  ggplot(aes(Seniority,Salary))+
  geom_jitter(width=0.1, alpha=0.2)

df %>%
  ggplot(aes(JobTitle,Salary))+
  geom_jitter(width=0.1, alpha=0.2)

df %>%
  ggplot(aes(Education,Salary))+
  geom_jitter(width=0.1, alpha=0.2)

# Running a model
gender <- as.factor(df$Gender)
jt <- as.factor(df$JobTitle)
edu <- as.factor(df$Education)
dept <- as.factor(df$Dept)
senior <- as.ordered(df$Seniority)
age <- df$Age
salary <- df$Salary

model <- lm(salary~age+gender+jt+edu+dept+senior)
summary(model)

plot(model, 1)
ggqqplot(residuals(model))




model2 <- lm(salary~gender)
summary(model2)

diagRegressionPlots(model)

install.packages("sm")
library(sm)
sm.density.compare(df$BasePay)

basepay <- log(df$BasePay)

d <- density(df$BasePay,bw = bw.nrd(df$BasePay), adjust = 1, kernel = "gaussian")
plot(d, main="Density plot of Basepay")
polygon(d, col="red", border="blue")

d <- density(log(df$Age),bw = bw.nrd(log(df$Age)), adjust = 1, kernel = "gaussian")
plot(d, main="Density plot of Age")
polygon(d, col="red", border="blue")



