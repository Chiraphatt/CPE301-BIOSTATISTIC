setwd("C:/Users/jirap/OneDrive - King Mongkut's University of Technology Thonburi (KMUTT)/year2/T2/CHHD301/1.mid/week2/data assignment")
data4 <- read.csv("..//data assignment/data4.csv",header = TRUE, as.is = TRUE)

attach(data4)

install.packages("scales")# Install and load scales
library("scales")

############### catagory data ################

########## fh ##########
#frequency 
fhFreq <- c(fh)
fhfreq <- table(fhFreq)

print(fhfreq)
#relative frequency
fhFreq <- c(fh)
rfreq <- table(fhFreq)/length(fhFreq) 
print(rfreq)
#percentage
fhpercent <- c(rfreq)
percent(fhpercent)

########## dm ##########
#frequency 
dmFreq <- c(dm)
dmfreq <- table(dmFreq)
print(dmfreq)
#relative frequency
dmFreq <- c(dm)
rfreq <- table(dmFreq)/length(dmFreq) 
print(rfreq)
#percentage
dmpercent <- c(rfreq)
percent(dmpercent)

########## smoking ##########
#frequency 
smokingFreq <- c(smoking)
smfreq <- table(smokingFreq)
print(smfreq)
#relative frequency
smokingFreq <- c(smoking)
rfreq <- table(smokingFreq)/length(smokingFreq)
print(rfreq)
#percentage
smokingpercent <- c(rfreq)
percent(smokingpercent)

########## fit ##########
#frequency 
fitFreq <- c(fit)
fitfreq <- table(fitFreq)
print(fitfreq)
#relative frequency
fitFreq <- c(fit)
rfreq <- table(fitFreq)/length(fitFreq)
print(rfreq)
#percentage
fitpercent <- c(rfreq)
percent(fitpercent)

########## coloscopy ##########
#frequency
colosFreq <- c(coloscopy)
cofreq <- table(colosFreq)
print(cofreq)
#relative frequency
colosFreq <- c(coloscopy)
rfreq <- table(colosFreq)/length(colosFreq)
print(rfreq)
#percentage
colospercent <- c(rfreq)
percent(colospercent)

############### numuric data ################

# function ???????????????????????? mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

######## age ##########
ageFreq <- c(age)
agefreq <- table(ageFreq)
print(agefreq)

mean(age) 
median(age)
getmode(age)
range(age,na.rm=TRUE)
sd(age)
var(age)
quantile(age)

######## bmi ##########
mean(bmi) 
median(bmi)
getmode(bmi)
range(bmi,na.rm=TRUE)
sd(bmi)
var(bmi)
quantile(bmi)

######## hematocrit ##########
mean(hematocrit) 
median(hematocrit)
getmode(hematocrit)
range(hematocrit,na.rm=TRUE)
sd(hematocrit)
var(hematocrit)
quantile(hematocrit)

#################################### 4 ########################################


# Pie Chart with fh
par(family = "TH Sarabun New") 
fhdata <- c(fhfreq)
lbls <- c("No", "Yes")
pct <- round(fhdata/sum(fhdata)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(fhdata,labels = lbls, col=c("blue","red"), main="Proportion of the number of families with cancer ")
legend("topright", inset=.05,c("No","Yes"), fill=c("blue","red"), horiz=FALSE) 
# Pie Chart with dm
dmdata <- c(dmfreq)
lbls <- c("No", "Yes")
pct <- round(dmdata/sum(dmdata)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(dmdata,labels = lbls, col= c("skyblue","salmon"), main="Proportion of the number of families with diabetes ")
legend("topright", inset=.05,c("No","Yes"), fill=c("skyblue","salmon"), horiz=FALSE) 
# Pie Chart with smoking
smdata <- c(smfreq)
lbls <- c("No", "Yes")
pct <- round(smdata/sum(smdata)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(smdata,labels = lbls, col= c("steelblue","orangered1"), main="Proportion of the number of people who smoke ")
legend("topright", inset=.05,c("No","Yes"), fill=c("steelblue","orangered1"), horiz=FALSE) 
# Pie Chart with fit
fitdata <- c(fitfreq)
lbls <- c("Negative", "Positive")
pct <- round(fitdata/sum(fitdata)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(fitdata,labels = lbls, col= c("#4DB3E6","#AA4371"), main="Fecal immunochemical test")
legend("topright", inset=.05,c("Negative","Positive"), fill=c("#4DB3E6","#AA4371"), horiz=FALSE) 
# Pie Chart with coloscopy
codata <- c(cofreq)
lbls <- c(" Abnormal", " Colorectal cancer","Normal")
pct <- round(codata/sum(codata)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(codata,labels = lbls, col= c("salmon","red","#4DB3E6"), main="Proportion of diseases caused by laparoscopy ")
legend("bottomright", cex = 0.8,c(" Abnormal", " Colorectal cancer","Normal"), fill=c("salmon","red","#4DB3E6"), horiz=FALSE) 



# Bar Chart with fh
barplot(table(fh), 
        main=" Risk factor : fh",  
        xlab="Test result",  
        ylab="Number of fh",       
        col =c("blue","red"))  
legend("topright", inset=.05,c("No","Yes"), fill=c("blue","red"), horiz=FALSE) 
# Bar Chart with dm
barplot(table(dm), 
        main=" Risk factor : dm",  
        xlab="Test result",  
        ylab="Number of dm",       
        col =c("skyblue","salmon"))  
legend("topright", inset=.05,c("No","Yes"), fill=c("skyblue","salmon"), horiz=FALSE) 
# Bar Chart with smoking
barplot(table(smoking), 
        main=" Risk factor : smoking",  
        xlab="Test result",  
        ylab="Number of smoking",       
        col =c("steelblue","orangered1"))  
legend("topright", inset=.05,c("No","Yes"), fill=c("steelblue","orangered1"), horiz=FALSE) 
# Bar Chart with fit
barplot(table(fit), 
        main=" Risk factor : fit",  
        xlab="Test result",  
        ylab="Number of fit",       
        col =c("#4DB3E6","#AA4371"))  
legend("topright", inset=.05,c("Negative","Positive"), fill=c("#4DB3E6","#AA4371"), horiz=FALSE) 
# Bar Chart with fit
barplot(table(coloscopy), 
        main=" Risk factor : coloscopy",  
        xlab="Test result",  
        ylab="Number of coloscopy",       
        col =c("salmon","red","#4DB3E6"))  
legend("topleft", inset=.05,c(" Abnormal", " Colorectal cancer","Normal"), fill=c("salmon","red","#4DB3E6"), horiz=FALSE) 



# histogram 
attach(data4)
par(mfrow=c(3,1))
hist(age)
hist(bmi)
hist(hematocrit)



