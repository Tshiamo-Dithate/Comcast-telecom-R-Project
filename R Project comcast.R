##-----------------------------------------------COMCAST TELECOM COMPLAINTS PROJECT--------------------------------------------------------
setwd("C:\\Users\\Skero kent\\Desktop\\Deviare Programme\\R programming\\R Project")
getwd()

#importing necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


# IMPORTING THE CSV FILE INTO THE R ENVIRONMENT
complaints_data <- read.csv("Comcast Telecom Complaints data.csv")
names(complaints_data)
View(complaints_data)


#DATA EXPLORATION
dim(complaints_data)
nrow(complaints_data)
ncol(complaints_data)


str(complaints_data)
head(complaints_data)
tail(complaints_data)

#checking for any missing values in the data set
is.na(complaints_data)
#no missing values detected

#checking for duplicates in the data set
duplicated(complaints_data)
#no duplicates are found in the data set




#The format of Date column is not same throughout in the data set, we have to make it the same.
complaints_data$Date <- dmy(complaints_data$Date)
View(complaints_data)



#Now we need to provide the complaints on a MONTHLY AND DAILY LEVEL BASIS  and PLOT A TREND CHART for it.
#monthly level basis
complaints_data$Month <- months(complaints_data$Date)
monthly1 <- complaints_data %>% group_by(Month =as.integer(month(Date))) %>% 
  summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))

#A Plot for monthly granularity level
ggplot(data = monthly1,aes(Month,NumOfComplaints,label = NumOfComplaints))+ geom_line()+ 
  geom_point(size = 0.8)+ geom_text()+ scale_x_continuous(breaks = monthly1$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))



daily1 <- complaints_data %>% group_by(Date) %>% summarize(NumOfComplaints=n())

#A Plot for daily granularity level
ggplot(data = daily1,aes(as.POSIXct(Date),NumOfComplaints))+ geom_line()+ 
  geom_point(size = 1)+ scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

##From the trend charts above, we can see that complaints for the month of June are maximum, ***complaints = 1046***.




#A frequency table of the complaint types.

network_tickets <- contains(complaints_data$Customer.Complaint, match = 'network', ignore.case = T)
internet_tickets <- contains(complaints_data$Customer.Complaint,match = 'internet',ignore.case = T)
billing_tickets <- contains(complaints_data$Customer.Complaint,match = 'bill',ignore.case = T)
email_tickets <- contains(complaints_data$Customer.Complaint,match = 'email',ignore.case = T)
charges_ticket <- contains(complaints_data$Customer.Complaint,match = 'charge',ignore.case = T)

complaints_data$ComplaintType[internet_tickets]<- "Internet"
complaints_data$ComplaintType[network_tickets]<- "Network"
complaints_data$ComplaintType[billing_tickets]<- "Billing"
complaints_data$ComplaintType[email_tickets]<- "Email"
complaints_data$ComplaintType[charges_ticket]<- "Charges"

complaints_data$ComplaintType[-c(internet_tickets,network_tickets,
                                 billing_tickets,charges_ticket,email_tickets)] <- "Others"
table(complaints_data$ComplaintType)
##Internet type complaints are maximum as that can be observed from the table above, ***INTERNET = 472***.





#making a new categorical variable for Complaint Status.
open_complaints <- (complaints_data$Status == 'Open' | complaints_data$Status == 'Pending')
closed_complaints <- (complaints_data$Status == 'Closed' | complaints_data$Status == 'Solved')
complaints_data$ComplaintStatus[open_complaints] <- 'Open'
complaints_data$ComplaintStatus[closed_complaints] <- 'Closed'


#A plot of state wise status of complaints in a stacked bar chart.

stack_chart <- table(complaints_data$ComplaintStatus,complaints_data$State)
stack_chart
complaints_data <- group_by(complaints_data,State,ComplaintStatus)
chart_data <- summarise(complaints_data,Count = n())

#Plotting a state wise status of complaints in a stacked bar chart. 
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+ 
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+ theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "red"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "State-Wise Status of Complaints Stacked Bar Chart ",
       x = "States",y = "Number of Tickets",
       fill= "Status")
## From the above chart, we can clearly see that Georgia has maximum complaints.




# Which state has maximum unresolved complaints?
complaints_data %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% 
  summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))
## From the table obtained above we can clearly observe that the State of 
## Georgia has the maximum number of unresolved complaints




# The percentage of resolved complaints.
total <- complaints_data %>% group_by(ComplaintStatus) %>% summarize(NumOfComplaints=n())
total
slices <- total$NumOfComplaints
pct <- round((slices/sum(slices)*100),2)
lbls <- paste(total$ComplaintStatus," ",pct,"%",sep="")

#Plotting a pie chart
pie(slices, labels=lbls)
## The pie chart above clearly displays that there is a total of 76.75% Complaints resolved.





int1<-complaints_data %>% filter(Received.Via=='Internet',ComplaintStatus=='Closed') %>% 
  group_by(Received.Via,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
int2<-complaints_data %>% filter(Received.Via=='Customer Care Call',ComplaintStatus=='Closed') %>% 
  group_by(Received.Via,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
#Percentage of resolved internet Complaints
int1pct<-round(int1$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
int1pct
#Percentage of resolved Customer Care Call Complaints
int2pct<-round(int2$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
int2pct


### The output above shows that of the 76.75% resolved Complaints, 37.9% complaints are Internet type while 38.85% are Customer Care Call type.


