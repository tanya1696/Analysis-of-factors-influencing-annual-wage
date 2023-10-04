# Library and Packages
install.packages("dslabs")
install.packages("ggridges")
library(dplyr)
library(ggplot2)

# Importing dataset 
destfile <- "/Users/tanyasharma/Desktop/Statistics and Data Science Books/Statistical Programming/Final Project/glassdoor_data.csv"
df <- read.csv(destfile)
head(df,5)

age <- df$Age
jobtitle <- df$jobtitle
performance <- as.ordered(df$PerfEval)
senior <- as.ordered(df$Seniority)
base <- df$BasePay

# Bar Graphs for Categorical Variables
df %>% ggplot(aes(Gender))+geom_bar()
df %>% ggplot(aes(Education))+geom_bar()
df %>% ggplot(aes(Dept))+geom_bar()
df %>% ggplot(aes(JobTitle))+geom_bar()
df %>% ggplot(aes(PerfEval))+geom_bar()
df %>% ggplot(aes(Seniority))+geom_bar()

# Exploratory Data Analysis

# Kernel Density Plots
system("R CMD SHLIB kernel_density.c")
dyn.load("kernel_density.so")


den_fn = function(data1,xgrid1,e1){
  c = length(data1)
  d = length(xgrid1)
  fn = .C("kernel_density", x = as.double(data1), n = as.integer(c), g=as.double(xgrid1), m = as.integer(d), bw = as.double(e1), y = double(d))
  fn$y
}

age_sample = seq(min(age), max(age), length.out = 500)
e1 <- bw.nrd(age)

k_estimates <- den_fn(age,age_sample,e1)


# Plotting the estimates
plot.new()
par(mar = c(1, 1, 1, 1))
plot(k_estimates, main="Density plot of Age")
polygon(k_estimates, col="red", border="blue")

base_sample = seq(min(base), max(base), length.out = 500)
e1 <- bw.nrd(base)

k_estimates <- den_fn(base,base_sample,e1)

# Plotting the estimates
plot.new()
par(mar = c(1, 1, 1, 1))
plot(k_estimates, main="Density plot of BasePay")
polygon(k_estimates, col="red", border="blue")

# Boxplots

p <- df %>%
  ggplot(aes(Gender, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Education, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)

p <- df %>%
  ggplot(aes(Dept, BasePay))+
  geom_boxplot()
p+geom_point(alpha=0.5)


base <- df$BasePay
df_temp <- data.frame(base,jobtitle)
pg_plot <- ggplot(df_temp, aes(x = jobtitle, y = base, fill = jobtitle)) +
  geom_boxplot()
# Change the legend labels
pg_plot +
  scale_fill_discrete(labels = c("Driver", "Data Scientist", "Financial Analyst", "Graphic Designer", "IT","Marketing Associate","Manager","Sales Associate","Software Engineer"))

#Plots focussing on Gender Pay Gap

p <- df %>%
  ggplot(aes(Dept, BasePay, color=Gender))+
  geom_boxplot()
p

p <- df %>%
  ggplot(aes(Education, BasePay, color=Gender))+
  geom_boxplot()
p

filter(df, Gender%in%c("Female","Male")) %>%
  ggplot(aes(Age, Salary, col=JobTitle))+
  geom_point()+
  facet_grid(JobTitle~Gender)


# Coding Categorical Variables
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

# Multi-Way Anova
model1 <- lm(base~gender+jobt+edu+dept+senior+performance)
anova(model1)

#Linear Reression
model2 <- lm(salary~age+gender+jobt+edu+dept+senior+performance)
summary(model2)

# Residual Plots
diagRegressionPlots(model)