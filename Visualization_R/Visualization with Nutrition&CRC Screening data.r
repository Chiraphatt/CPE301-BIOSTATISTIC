setwd("C:/Users/jirap/OneDrive - King Mongkut's University of Technology Thonburi (KMUTT)/year2/T2/CHHD301/1.mid/week2/data assignment")
data <- read.csv("..//data assignment/data1.csv",header = TRUE, as.is = TRUE)

attach(data)

install.packages("scales")# Install and load scales
library("scales")


ftable(gender)
gender_rela_freq <- table(gender)/length(gender)
gender_rela_freq
lbls <- c("Female", "Male")
gender_percentage <- table(gender)/length(gender)*100
gender_percentage
pie(gender_percentage, col=c("pink","skyblue"), main = "Gender")
legend("topright", inset=.05,c("Female","Male"), fill=c("pink","skyblue"), horiz=FALSE)


ftable(food1)
food1_rela_freq <- table(food1)/length(food1)
food1_rela_freq
food1_percentage <- table(food1)/length(food1)*100
food1_percentage
pie(food1_percentage, main = "Proportion of the number of the patient's diet with food1 ")

ftable(food2)
food2_rela_freq <- table(food2)/length(food2)
food2_rela_freq
food2_percentage <- table(food2)/length(food2)*100
food2_percentage
pie(food2_percentage, main = "Proportion of the number of the patient's diet with food2")

ftable(food3)
food3_rela_freq <- table(food3)/length(food3)
food3_rela_freq
food3_percentage <- table(food3)/length(food3)*100
food3_percentage
pie(food3_percentage, main = "Proportion of the number of the patient's diet with food3")

ftable(food4)
food4_rela_freq <- table(food4)/length(food4)
food4_rela_freq
food4_percentage <- table(food4)/length(food4)*100
food4_percentage
pie(food4_percentage, main = "Proportion of the number of the patient's diet with food4")


ftable(food5)
food5_rela_freq <- table(food5)/length(food5)
food5_rela_freq
food5_percentage <- table(food5)/length(food5)*100
food5_percentage
pie(food5_percentage, main = "Proportion of the number of the patient's diet with food5")

ftable(food6)
food6_rela_freq <- table(food6)/length(food6)
food6_rela_freq
food6_percentage <- table(food6)/length(food6)*100
food6_percentage
pie(food6_percentage, main = "Proportion of the number of the patient's diet with food6")

ftable(food7)
food7_rela_freq <- table(food7)/length(food7)
food7_rela_freq
food7_percentage <- table(food7)/length(food7)*100
food7_percentage
pie(food7_percentage, main = "Proportion of the number of the patient's diet with food7")

summary(age)
var(age)
sd(age, na.rm = FALSE)
hist(age, main = "Age Distribution", xlab = "Age (year)")

summary(bmi)
var(bmi)
sd(bmi, na.rm = FALSE)
hist(bmi, main = "Body Mass Index Distribution", xlab = "BMI (Kg/M^2)")

