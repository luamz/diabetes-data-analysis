library(dplyr)
library(epiDisplay)
library(readr) 

data <- read_csv("diabetes.csv")

##  Diabetes Distribution in study participants
outcome <- table(data$Outcome)
pie(outcome, labels=c("Without diabetes","With diabetes"),col = rainbow(2), 
    main = "Diabetes Distribution in Study Participants", cex = 0.8,)
positive <- filter(data,Outcome==1)
negative <- filter(data,Outcome==0)

# Age Distribution 
age <- table(data$Age)
pie(age, col = rainbow(60), 
    main = "Age Distribution in Study Participants", cex = 0.6,)

freq_ages <- cut(data$Age, breaks=seq(from = 21, to = 91, by = 10),right=F)
tab1(freq_ages,graph=F)

# Just over half of the patients in the study are between 21 and 30 years old
# and the other half are patients between 31 and 81 years old.
# Therefore, the proportion of young adults in the study is higher
# than that of adults and elders.


## Investigating Glucose Levels

# Glucose histogram from positive patients

histograma_gluc_pos<-hist(positive$Glucose, 20, ylim=c(0,100),col=rainbow(10),
                          main="Glucose Histogram (Positive)", 
                          xlab="Glucose Level", ylab="Frequency")
positives <- cut(positive$Glucose, breaks=seq(from = 0, to = 200, by = 10),right=F)
tab1(positives,graph=F)

# Glucose histogram from negative patients

histograma_gluc_neg<-hist(negative$Glucose, 20, ylim=c(0,100),col=rainbow(10),
                          main="Glucose Histogram (Negative)",
                          xlab="GLucose Level", ylab="Frequency")
negatives <- cut(negative$Glucose, breaks=seq(from = 0, to = 200, by = 10),right=F)
tab1(negatives,graph=F)

# We can see that the majority (94%) of patients with diabetes (positive) 
# have glucose levels between 100 and 199, while the majority (91.8%) 
# of patients without diabetes (negative) have glucose levels
# between 50 and 149 .


## Investigating Insulin Levels

# Glucose histogram from positive patients
histograma_ins_pos<-hist(positive$Insulin, 10, xlim=c(0,800),
                         ylim=c(0,400), 50,col=rainbow(10),
                         main="Insulin Histogram (Positive)",
                         xlab="Insulin Levels", ylab="Frequency")
positives_ins <- cut(positive$Insulin, breaks=seq(from = 0, to = 200, by = 10),right=F)
tab1(positives_ins,graph=F)


# Glucose histogram from negative patients
histograma_ins_neg<-hist(negative$Insulin, 10, xlim=c(0,800),
                         ylim=c(0,400), 50,col=rainbow(10),
                         main="Insulin Histogram (Negative)",
                         xlab="Insulin Levels", ylab="Frequency")

negatives_ins <- cut(negative$Insulin, breaks=seq(from = 0, to = 200, by = 10),right=F)
tab1(negatives_ins,graph=F)



# We can observe that the graphs do not appear to have drastic differences,
# negative patients have most of their insulin levels between 0 and 199 (91.6%),
# whereas positive patients have a slightly lower frequency between 0 and 199 
# (82.5 %), that is, they have a greater distribution of insulin levels 
# between 200 and 800

