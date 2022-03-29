setwd("/Users/samueljohngomez/google drive/w203_statistics_for_data_science/lab_02")
getwd()
A = read.csv("anes_pilot_2018.csv")
library(ggplot2)
library(gridExtra)
library(psych)
library(BSDA)
library(DescTools)
library(effsize)
library(moments)
length(A$turnout18)
summary(A$turnout18)
votes_18 = as.data.frame(table(A$turnout18))  
sum(votes_18$Freq[1:3])

votes_16 = as.data.frame(table(A$turnout16))  
sum(votes_16$Freq[1])

# Create the dataframe that will be used for the analysis
# Remove respondents who did not vote in 2018
# Remove respondents who did not respond to either anger or fear emotion
df_analysis=A[A$turnout18 != 4 & A$turnout18 != 5 & A$geangry != -7 & A$geafraid != -7,]
# turnout18 values should include 1, 2, and 3 only
unique(df_analysis$turnout18)
summary(df_analysis$turnout18)
# turnout16 values should include 1, 2, and 3 only
unique(df_analysis$turnout16)
summary(df_analysis$turnout16)
# geangry values should include 1-5 only
unique(df_analysis$geangry)
summary(df_analysis$geangry)
# geafraid values should include 1-5 only
unique(df_analysis$geafraid)
summary(df_analysis$geafraid)

# histogram angry and afraid
layout(matrix(c(1,2),2,1))
hist(df_analysis$geangry, breaks = 25, col="lightblue", border='darkgray', xlim = c(1,5), main = "Angry Histogram") 
hist(df_analysis$geafraid, breaks = 25, col="pink", border='darkgray', xlim = c(1,5), main = "Afraid Histogram")

layout(matrix(c(1,1)))
hist( df_analysis$geafraid, breaks = 25, col="lightblue", border='darkgray', xlim = c(1,5.3), 
      main = "Histrogram of Afraid vs Angry", xlab="Afraid vs. Angry")  # first histogram
hist( df_analysis$geangry+.2, breaks = 25, col="pink", border='darkgray', xlim = c(0,5.3), add=T)

# create dataframes for 2018 and 2016 votrs
# 2018 votes dataframe
df_voters18 = as.data.frame(table(df_analysis$turnout18))  
df_voters18$Var1=c("Definitely voted in person on Nov. 6", "Definitely voted in person before Nov.6", "Definitely voted by mail")
grid.table(df_voters18)
votes_18 = sum(df_voters18$Freq[1:3])
# 2016 votes dataframe
df_voters16 = as.data.frame(table(df_analysis$turnout16))
df_voters16$Var1=c("Definitely voted", "Definitely did not vote", "Not completely sure")  
grid.table(df_voters16)
votes_16 = sum(df_voters16$Freq[1:3])

# perform sign test
SignTest(df_analysis$geangry, df_analysis$geafraid)

# Weight 
df_analysis2=df_analysis[df_analysis$geangry != df_analysis$geafraid,]
df_angry = df_analysis2$geangry
df_afraid = df_analysis2$geafraid
emotion_ben = factor(ifelse(df_angry >df_afraid, "more angry", "more afraid"))
summary(emotion_ben)
binom.test(676, 1024, p=0.5)
SignTest(df_analysis$geafraid, df_analysis$geangry)
  
t.test(df_analysis$geangry, df_analysis$geafraid, paired = TRUE)

wilcox.test(df_analysis$geangry, df_analysis$geafraid, paired=TRUE)
  
CohenD(df_analysis$geangry, df_analysis$geafraid)

cohen.d(df_analysis$geangry, df_analysis$geafraid)






# Trump

summary(A$fttrump)
df_trump = as.data.frame(table(A$fttrump))
df_trump
df_reg = as.data.frame(table(A$reg))
df_reg

df_analysis5 = A[A$reg != 3 & A$reg != -7,]

t_military = as.data.frame(table(A$dem_activduty))
t_military

t_honest = as.data.frame(table(A$honest))
t_honest

t_nonserious = as.data.frame(table(A$nonserious))
t_nonserious

# 1851 respondents remain
df_data = A[A$nonserious==1 & A$honest==5,]
df_data = A[A$nonserious==1,]
df_data = df_data[df_data$honest == 5,]

# 1487 respondents remain
df_data=df_data[df_data$turnout18 != 4 & df_data$turnout18 != 5 & df_data$geangry != -7 & df_data$geafraid != -7,]

df_data2=df_data[df_data$geangry != df_data$geafraid,]
df_angry = df_data2$geangry
df_afraid = df_data2$geafraid
emotion_ben = factor(ifelse(df_angry >df_afraid, "more angry", "more afraid"))
summary(emotion_ben)
binom.test(565, 565+276, p=0.5)

df_antifa=as.data.frame(table(A$ftantifa))
df_antifa

hist(A$ftantifa, breaks = 103, col="lightblue", border='darkgray', xlim=c(1,100),  main = "Angry Histogram")

ggplot(df_antifa$ftantifa, aes(x=c(1,102))) + 
  geom_histogram(bins=10, colour="black", fill="pink") +
  labs(title="Angry Histogram", x="Angry Intensity", y="Count")











































  
