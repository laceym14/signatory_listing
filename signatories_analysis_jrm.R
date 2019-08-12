#BioScience border wall biodiversity signatories
#Started June 2018
#Jennie Miller with Rob Peters, Defenders of Wildlife

#PREPARE WORKSPACE
setwd("C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Border wall/BioScience manuscript/Sign-on materials/Signatories data")

#Install packages
library(plyr)
library(dplyr)
library(ggplot2)

#PREPARE DATA
data = read.csv("signatories 08-01-18.csv", skip = 2, header = T)
colnames(data)= c('Date.time','Type','First.name','Last.name','City','State','Postal.code','Country','Email','Degree','Institution','Position')
data$Country <- as.factor (data$Country)

#test for duplicates
dup <- data.frame(as.numeric(duplicated(data[c('First.name','Last.name')]))) #creates df with binary var for duplicated rows
colnames(dup) <- c("dup") #renames column for simplicity
df2 <- cbind(data,dup) #bind to original df
df3 <- subset(df2, dup == 1) #subsets df using binary var for duplicated`
df3
#if duplicates, remove manually on www.defenders.org/border-wall

dup2 <-data.frame(as.numeric(duplicated(data[c('Last.name','Postal.code','Email')]))) #creates df with binary var for duplicated rows
colnames(dup2) <- c("dup") #renames column for simplicity
df4 <- cbind(data,dup2) #bind to original df
df5 <- subset(df4, dup == 1) #subsets df using binary var for duplicated`
df5

#formatting time
data$Date.time <- strptime(as.character(data$Date.time),"%Y-%m-%d %H:%M")
data$Date <- as.Date(data$Date.time)
data$Time <- format(as.POSIXct(data$Date.time),format = "%H:%M")
#replacing factors
revalue(data$Type, 
        c("A general supporter" = "Supporter", 
          "A scientist with a Masters, PhD, or other terminal degree, or I am working toward a Masters, PhD, or other terminal degree" = "Scientist")) -> data$Type

#ANALYSIS OF DATA
#SUBSET DATA
data1 <- subset(data, Type=='Scientist',select=c(Type:Time))

#SUMMARY
summary(data1$Country)
summary(data1$State)

#signatories by US state
data2 <- subset(data1, Country=='United States',select=c(Type:Time))
state <- as.data.frame(table(data2$State))
colnames(state) <- c("State","Signatories")
state <- state[!(state$Signatories==0),]
write.csv(state,file="Signatories by US state.csv")


#PLOT COUNTRIES HISTOGRAPH
#summary data
country <- as.data.frame(table(data1$Country))
colnames(country) <- c("Country","Signatories")
write.csv(country,file="Signatories by country 07-23-18.csv")

#plot
ggplot(subset(country, Freq>10), aes(x=reorder(Var1,-Freq), y=Freq)) + geom_bar(stat="identity") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, hjust=1), plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1500)) +
  labs(x="Country", y="Signatories",title="Countries with more than 10 signatories")
  

#PLOT CUMULATIVE SIGNATORIES OVER TIME
counts <- data1 %>% group_by(Date) %>% summarize(num_sig=n())
cum_freq <- as.data.frame(counts %>% arrange(Date) %>% mutate(cum_frequency = cumsum(num_sig)))
cum_freq <- acum_freq[cum_freq$Date== "2018-05-30",]

ggplot(cum_freq, aes(x=Date, y=cum_frequency)) +  
  geom_point(size=1, color="black") +
  geom_line(size=.5, color="black") + xlab("Date") + ylab("Number of scientists") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  scale_y_continuous(limits=c(0,3000)) +
  annotate("text", label="Sharing started, Ehrlich tweet", x=as.Date("2018-06-01"), y=5, size=4) +
  annotate("segment", x=as.Date("2018-05-30"), y=50, 
           xend=as.Date("2018-05-30"), yend=280, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="Email to UAM-L", x=as.Date("2018-06-06"), y=500, size=4) +
  annotate("segment", x=as.Date("2018-06-06"), y=575, 
           xend=as.Date("2018-06-06"), yend=1100, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="DiCaprio Foundation tweet", x=as.Date("2018-06-04"), y=1500, size=4) +
  annotate("segment", x=as.Date("2018-06-04"), y=1420, 
           xend=as.Date("2018-06-04"), yend=970, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="Alliance email", x=as.Date("2018-06-08"), y=2280, size=4) +
  annotate("segment", x=as.Date("2018-06-08"), y=2200, 
           xend=as.Date("2018-06-08"), yend=1720, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="MX Acad Sci & PPPBio email", x=as.Date("2018-06-17"), y=1750, size=4) +
  annotate("segment", x=as.Date("2018-06-17"), y=1800, 
           xend=as.Date("2018-06-17"), yend=2310, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="Defenders tweet", x=as.Date("2018-06-12"), y=1400, size=4) +
  annotate("segment", x=as.Date("2018-06-12"), y=1500, 
           xend=as.Date("2018-06-12"), yend=2100, size=0.5,arrow=arrow(length=unit(.2, "cm"))) +
  annotate("text", label="Defenders tweet", x=as.Date("2018-05-31"), y=1100, size=4) +
  annotate("segment", x=as.Date("2018-05-31"), y=1000, 
           xend=as.Date("2018-05-31"), yend=550, size=0.5,arrow=arrow(length=unit(.2, "cm"))) 

  







