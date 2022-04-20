#Importing and viewing the data
library(readxl)
air_france <- read_excel("/Users/belindadarko/Documents/Skole relatert/MSBAN/Data Science R/Airfrance/Airfrance.xls",
                         sheet = 2)
#Viewing Results
View(air_france)

################################################
# Exploring the data set
################################################

# Displaying the characteristics of each feature
summary(air_france)


#Checking the number of missing values in the table
apply(X = is.na(air_france), MARGIN = 2, FUN = sum) #Bid Strategy has 1224 missing values

#Counting the number of "N/A" values on the whole data frame
#install.packages("stringr")
#library(stringr)

#Counting number of N/A values per vector
sum(str_count(air_france$`Publisher ID`, "N/A"))
sum(str_count(air_france$`Publisher Name`, "N/A"))
sum(str_count(air_france$`Keyword ID`, "N/A"))
sum(str_count(air_france$Keyword, "N/A"))
sum(str_count(air_france$`Match Type`, "N/A")) #Column has a category N/A with 48 observations
sum(str_count(air_france$Campaign, "N/A"))
sum(str_count(air_france$`Keyword Group`, "N/A"))
sum(str_count(air_france$Category, "N/A"))
sum(str_count(air_france$`Keyword Type`, "N/A"))
sum(str_count(air_france$Status, "N/A"))



################################################
# Massaging the data set
################################################
#Publisher name and ID are the same
#Analyzing number of categories
table(air_france$`Publisher Name`)

#Creating new factors with numerical values
summary(air_france$`Publisher Name`) #Verifying type of data
air_france$publisher <- air_france$`Publisher Name` #Creating new column with existing data 
air_france$publisher[air_france$publisher == "Google - Global"] <- 1
air_france$publisher[air_france$publisher == "Google - US"] <- 2
air_france$publisher[air_france$publisher == "MSN - Global"] <- 3
air_france$publisher[air_france$publisher == "MSN - US"] <- 4
air_france$publisher[air_france$publisher == "Overture - Global"] <- 5
air_france$publisher[air_france$publisher == "Overture - US"] <- 6
air_france$publisher[air_france$publisher == "Yahoo - US"] <- 7

#Verifying new column
table(air_france$`Publisher Name`)
table(air_france$publisher)

#Converting publisher into numeric
air_france$publisher <- as.numeric(air_france$publisher)



#Converting Match Type categories
#Number of category
table(air_france$`Match Type`)

# Creating new column
air_france$match_cat <- air_france$`Match Type`

# Reassigning values
air_france$match_cat[air_france$match_cat == "N/A"] <- 0
air_france$match_cat[air_france$match_cat == "Advanced"] <- 1
air_france$match_cat[air_france$match_cat == "Broad"] <- 2
air_france$match_cat[air_france$match_cat == "Exact"] <- 3
air_france$match_cat[air_france$match_cat == "Standard"] <- 4

#Converting to numeric
air_france$match_cat <- as.numeric(air_france$match_cat)


table(air_france$Campaign)
table(air_france$`Keyword Group`)
table(air_france$Category)
table(air_france$`Bid Strategy`)

# Creating new column
 air_france$has_bid_strat <- air_france$`Bid Strategy`

# #Reassigning values
 air_france$has_bid_strat[air_france$has_bid_strat == "Pos 3-6"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 1- 3"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 1-2 Target"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 1 -2 Target"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 1-4 Bid Strategy"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Postiion 1-4 Bid Strategy"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 2-5 Bid Strategy"] <- 1
 air_france$has_bid_strat[air_france$has_bid_strat == "Position 5-10 Bid Strategy"] <- 1
 air_france$has_bid_strat[is.na(air_france$`Bid Strategy`)] <- 0

#Checking results
table(air_france$has_bid_strat)

#Converting vector as numeric
air_france$has_bid_strat<- as.numeric(air_france$has_bid_strat)

# Checking the number of category for remaining character variables
table(air_france$`Keyword Type`)
table(air_france$Status)

#Creating new column
air_france$status_int <- air_france$Status
air_france$status_int[air_france$status_int == "Deactivated"] <- 1
air_france$status_int[air_france$status_int == "Live"] <- 2
air_france$status_int[air_france$status_int == "Paused"] <- 3
air_france$status_int[air_france$status_int == "Sent"] <- 4
air_france$status_int[air_france$status_int == "Unavailable"] <- 5

#Checking results
table(air_france$status_int)

#Converting vector as a numeric
air_france$status_int <- as.numeric(air_france$status_int)


# Chekcking final result
View(air_france)


#Existing Metrics for the campaign
# Average Cost per Click (CPC)
# Engine Click Through Rate (CTR)
# Transaction Conversion Rate (TCR)

# Creating Net Revenue Variable
air_france$net_revenue <- air_france$Amount - air_france$`Total Cost`

#checking results and missing values
View(air_france)
sum(is.na(air_france$net_revenue))


# After a quick scan, we see observation 338 is an outlier that will affect the data
# 1 observation remove out of 4510 will not have any impact on the final result
# Removing observation 338
air_france <- air_france[-338,]

# Checking Result
View(air_france)



################################################
# UDF
################################################
# Normalizing UDF
norm <- function(x){
  normalized <- (x-min(x))/(max(x)-min(x))
  return(normalized)
}#Closing loop

# Return On Advertising UDF (ROA)
roa <- function(a,b,c){
  revenues <- sum(b)
  costs <- sum(c)
  ROA <- (revenues)/costs
  return(ROA)
}#closing loop

################################################
# ROA for each Publisher
################################################
#Yahoo
yahoo_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Yahoo - US")], 
                   b=air_france$Amount[which(air_france$`Publisher Name`=="Yahoo - US")],
                   c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="Yahoo - US")]),2)

#Google US
google_us_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Google - US")], 
                       b=air_france$Amount[which(air_france$`Publisher Name`=="Google - US")],
                       c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="Google - US")]),2)


#Google Global
google_global_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Google - Global")], 
                           b=air_france$Amount[which(air_france$`Publisher Name`=="Google - Global")],
                           c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="Google - Global")]),2)


#MSN Global
msn_global_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="MSN - Global")], 
                               b=air_france$Amount[which(air_france$`Publisher Name`=="MSN - Global")],
                               c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="MSN - Global")]),2)


#MSN US
msn_us_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="MSN - US")], 
                               b=air_france$Amount[which(air_france$`Publisher Name`=="MSN - US")],
                               c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="MSN - US")]),2)



#Overture Global
overture_global_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Overture - Global")], 
                               b=air_france$Amount[which(air_france$`Publisher Name`=="Overture - Global")],
                               c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="Overture - Global")]),2)



#Overture US
overture_us_roa <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Overture - US")], 
                               b=air_france$Amount[which(air_france$`Publisher Name`=="Overture - US")],
                               c=air_france$`Total Cost`[which(air_france$`Publisher Name`=="Overture - US")]),2)


#Implementing everything in data frame
publisher_roa <- data.frame(google_global_roa, google_us_roa, yahoo_roa, overture_global_roa, 
                            overture_us_roa, msn_global_roa, msn_us_roa)

#Transposing 
publisher_roa_transpose = t(publisher_roa)

#Converting to a data frame
publisher_roa_transpose <- as.data.frame(publisher_roa_transpose)

#Renaming V1 column for readability purpose
colnames(publisher_roa_transpose)[colnames(publisher_roa_transpose)=="V1"] <- "ROA"



################################################
# Net Revenue Per Click for Each Publisher
################################################
#ROA udf will for for this new variable 
#Yahoo
yahoo_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Yahoo - US")], 
                       b=air_france$net_revenue[which(air_france$`Publisher Name`=="Yahoo - US")],
                       c=air_france$Clicks[which(air_france$`Publisher Name`=="Yahoo - US")]),2)

#Google US
google_us_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Google - US")], 
                           b=air_france$net_revenue[which(air_france$`Publisher Name`=="Google - US")],
                           c=air_france$Clicks[which(air_france$`Publisher Name`=="Google - US")]),2)


#Google Global
google_global_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Google - Global")], 
                               b=air_france$net_revenue[which(air_france$`Publisher Name`=="Google - Global")],
                               c=air_france$Clicks[which(air_france$`Publisher Name`=="Google - Global")]),2)


#MSN Global
msn_global_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="MSN - Global")], 
                            b=air_france$net_revenue[which(air_france$`Publisher Name`=="MSN - Global")],
                            c=air_france$Clicks[which(air_france$`Publisher Name`=="MSN - Global")]),2)


#MSN US
msn_us_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="MSN - US")], 
                        b=air_france$net_revenue[which(air_france$`Publisher Name`=="MSN - US")],
                        c=air_france$Clicks[which(air_france$`Publisher Name`=="MSN - US")]),2)



#Overture Global
overture_global_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Overture - Global")], 
                                 b=air_france$net_revenue[which(air_france$`Publisher Name`=="Overture - Global")],
                                 c=air_france$Clicks[which(air_france$`Publisher Name`=="Overture - Global")]),2)



#Overture US
overture_us_nrc <- round(roa(a=air_france$`Publisher Name`[which(air_france$`Publisher Name`=="Overture - US")], 
                             b=air_france$net_revenue[which(air_france$`Publisher Name`=="Overture - US")],
                             c=air_france$Clicks[which(air_france$`Publisher Name`=="Overture - US")]),2)


#Implementing everything in data frame
publisher_nrc <- data.frame(google_global_nrc, google_us_nrc, yahoo_nrc, overture_global_nrc, 
                            overture_us_nrc, msn_global_nrc, msn_us_nrc)

#Transposing 
publisher_nrc_transpose = t(publisher_nrc)

#Converting to a data frame
publisher_nrc_transpose <- as.data.frame(publisher_nrc_transpose)

#Renaming V1 column for readability purpose
colnames(publisher_nrc_transpose)[colnames(publisher_nrc_transpose)=="V1"] <- "NRC"



################################################
# Correlation Table
################################################

data.frame(colnames(air_france))

#Select only numerical features based on index
air_france_corr <- air_france[,c(12:23)]

#Display Pearson correlation table 
cor(air_france_corr, air_france$net_revenue, method="pearson")



################################################
# Renaming columns
################################################
#Renaming columns to fit into pivot tables
colnames(air_france)[colnames(air_france)=="net_revenue"] <- "netrevenue"
colnames(air_france)[colnames(air_france)=="Publisher Name"] <- "Publishername"
colnames(air_france)[colnames(air_france)=="Total Volume of Bookings"] <- "Bookings"
colnames(air_france)[colnames(air_france) == "Match Type"] <- "matchtype"
colnames(air_france)[colnames(air_france) == "Search Engine Bid"] <- "SEB"
colnames(air_france)[colnames(air_france) == "Click Charges"] <- "ClickCharges"
colnames(air_france)[colnames(air_france) == "Avg. Cost per Click"] <- "ACPC"
colnames(air_france)[colnames(air_france) == "Total Cost/ Trans."] <- "TCPT"
colnames(air_france)[colnames(air_france) == "Total Cost"] <- "TotalCost"
colnames(air_france)[colnames(air_france) == "Engine Click Thru %"] <- "ECTR"
colnames(air_france)[colnames(air_france) == "Trans. Conv. %"] <- "TCR"




################################################
# Publishers Volume of Bookings
################################################
#install.packages('dplyr')
library(dplyr)

# Campaign performance by amount, impressions, and net revenue
campaign_perf <- air_france%>%
  group_by(Campaign)%>%
  select(Amount, Impressions, Campaign, netrevenue)%>%
  summarise(sum(Amount), sum(Impressions), sum(netrevenue))

#Amount generated from each publisher
 publisher_rev <- air_france%>%
   group_by(Publishername)%>%
   select(netrevenue, Publishername)%>%
  summarise(sum(netrevenue))
 
#Amount generated from each publisher
 publisher_cost <- air_france%>%
   group_by(Publishername)%>%
   select(TCPT, Publishername)%>%
   summarise(mean(TCPT))

#Best match type by net revenue  
match_netrev <- air_france%>%
  group_by(matchtype)%>%
  select(matchtype, netrevenue)%>%
  summarise(sum(netrevenue))

#Best match type by number of bookings
match_booking <- air_france%>%
  group_by(matchtype)%>%
  select(matchtype, Bookings)%>%
  summarise(sum(Bookings))

#Volume of Bookings for each publisher
booking_perf <- air_france%>%
  group_by(Publishername)%>%
  select(Bookings, Publishername)%>%
  summarise(sum(Bookings))

#Net revenue generated from each publisher
nrev_perf <- air_france%>%
  group_by(Publishername)%>%
  select(netrevenue, Publishername)%>%
  summarise(mean(netrevenue))

#Number of impressions for each publisher
impression_perf <- air_france%>%
  group_by(Publishername)%>%
  select(Impressions, Publishername)%>%
  summarise(mean(Impressions))

#Total Cost per transaction, and average cost per click for each publisher
publisher_click <- air_france%>%
  group_by(Publishername)%>%
  select(Publishername, TCPT, ACPC)%>%
  summarise(mean(TCPT), mean(ACPC))

#Average Net Revenue Per Click for each Publisher
netrev_click <- air_france%>%
  group_by(Publishername)%>%
  select(Publishername, netrevenue, Clicks)%>%
  summarise(mean(sum(netrevenue/Clicks)))



################################################
# Normalization of features
################################################
# Normalizing all numeric features
# Clicks
air_france$clicks_norm <- norm(x=air_france$Clicks)

# Clicks charges
air_france$clicks_charge_norm <- norm(x=air_france$ClickCharges)

# Average Cost Per Click
air_france$avg_cpc_norm <- norm(x=air_france$ACPC)

# Impressions
air_france$impressions_norm <- norm(x=air_france$Impressions) #

# Engine Click Thru Rate
air_france$ectr_norm <- norm(x=air_france$ECTR)

# Avg Pos
air_france$avg_pos_norm <- norm(x=air_france$`Avg. Pos.`)

# Trans. COnv. Rate
air_france$trans_conv_norm <- norm(x=air_france$TCR)

# Total cost/trans
air_france$cost_trans_norm <- norm(x=air_france$TCPT)

# Amount
air_france$amount_norm <- norm(x=air_france$Amount)

# Total Cost
air_france$tot_cost_norm <- norm(x=air_france$TotalCost)

# Total Volume of Bookings
air_france$tot_booking_norm <- norm(x=air_france$Bookings)

# Net Revenue Per Click
air_france$netrevclick_norm <- norm(x=air_france$netrevclick)



################################################
# Logistic Regression
################################################
air_france$binom_roa<- c() #assigning empty vector to new object

#Creating udf to create binomial variable 
for(i in 1:nrow(air_france)){
  if(air_france$netrevenue[i]>0){
    air_france$binom_netrevenue[i] <- "1"
  }
  else {air_france$binom_netrevenue[i] <- "0"}
}#Closing udf 

#Converting binomial variable to a numeric
air_france$binom_netrevenue <- as.numeric(air_france$binom_netrevenue)

#Creating training index
train_index <- sample(1:nrow(air_france), size=0.8*nrow(air_france))


air_france_train <- air_france[train_index,] #Train data set
air_france_test <- air_france[-train_index,] #Test data set

#Running logit regression
air_france_logit <- glm(binom_netrevenue ~ Clicks+Impressions+TotalCost+TCPT, 
                        data=air_france_train, family = "binomial")


#Checking result of regression
summary(air_france_logit)



################################################
# Normalized Logistic Regression 
################################################

#Running logit regression
air_france_logit_norm <- glm(binom_netrevenue ~ clicks_norm+impressions_norm+tot_cost_norm+
                               cost_trans_norm, data=air_france_train, family = "binomial")

#Checking results
summary(air_france_logit_norm)



################################################
# Visualization Libraries
################################################
#Importing library
library(ggplot2)

#install.packages("RColorBrewer")                   
library("RColorBrewer")
display.brewer.all(colorblindFriendly = FALSE) 

#install.packages('dplyr')
library(dplyr)

#install.packages('forcats')
library(forcats)

#install.packages('viridis')
library(viridis)

#install.packages('hrbrthemes')
library(hrbrthemes)


############################
# Campaign Net Revenue
############################
#Checking columns name
colnames(campaign_perf)

#Changing names
colnames(campaign_perf)[colnames(campaign_perf)=="sum(netrevenue)"] <- "netrevenue"

#Checking Results
colnames(campaign_perf)

#Calling bar graph
campaign_perf%>%                                             #calling data frame to use
  mutate(Campaign = fct_reorder(Campaign, netrevenue))%>%    #ordering data
  ggplot( aes(x=(netrevenue), y=as.factor(Campaign)))+       #assigning variables to axis
  geom_bar(stat="identity", fill="#f68060")+                 #defining type of bar chart
  ggtitle("Campaign Net Revenue")+                           #graph title
  xlab("Total Net Revenue")+                                 #x axis label
  ylab("Campaign")                                           #y axis label




############################
# Match type Bookings
############################
#Checking columns name
colnames(match_booking)

#Changing columns name
colnames(match_booking)[colnames(match_booking)=="sum(Bookings)"] <- "Bookings"

#Building bar chart
match_booking%>%                                                                 #Data frame to take values from
  mutate(matchtype = fct_reorder(matchtype, Bookings))%>%                        #Ordering values
  ggplot( aes(x=(Bookings), y=as.factor(matchtype)))+geom_bar(stat="identity",
                                                              fill="#f68060")+   #Chart characteristics
  coord_flip()+                                                                  #Reversing order
  ggtitle("Volume of Booking For Each Match Type")+                              #Chart title
  xlab("Volume of Bookings")+                                                    #X axis title
  ylab("Match Type")                                                             #Y axis title



############################
# Match type Net Revenue
############################
#Checking columns name
colnames(match_netrev)

#Changing columns name
colnames(match_netrev)[colnames(match_netrev)=="sum(netrevenue)"] <- "netrevenue"

#Building bar chart
match_netrev%>%                                                                  #Data frame to take values from
  mutate(matchtype = fct_reorder(matchtype, netrevenue))%>%                      #Ordering values
  ggplot( aes(x=(netrevenue), y=as.factor(matchtype)))+geom_bar(stat="identity",
                                                                fill="#f68060")+ #Chart characteristics
  coord_flip()+                                                                  #Reversing order
  ggtitle("Net Revenue For Each Match Type")+                                    #Chart title
  xlab("Net Revenue")+                                                           #X axis title
  ylab("Match Type")                                                             #Y axis title



############################
# Publisher TCPT
############################                                                              
#Checking columns name
colnames(publisher_cost)

#Changing columns name
colnames(publisher_cost)[colnames(publisher_cost)=="mean(TCPT)"] <- "TCPT"

#Checking results
colnames(publisher_cost)

#Building bar chart
publisher_cost%>%                                              #calling data frame to use from
  mutate(Publishername = fct_reorder(Publishername, TCPT))%>%  #ordering data
  ggplot( aes(x=(TCPT), y=as.factor(Publishername)))+          #assigning variables to axis
  geom_bar(stat="identity", fill="#f68060")+                   #defining type of bar chart
  ggtitle("Publisher Avergae Total Cost Per Transaction ")+    #graph title
  xlab("Average Total Cost Per Transaction")+                  #x axis label
  ylab("Publisher")                                            #y axis label



############################
# Publisher Bookings
############################
#Checking columns name
colnames(booking_perf)

#Changing columns name
colnames(booking_perf)[colnames(booking_perf)=="sum(Bookings)"] <- "Bookings"

#Checking results
colnames(booking_perf)

#Pie chart
booking_perf%>%                                                                     #Data frame to take values from 
  mutate(Publishername = fct_reorder(Publishername, Bookings))%>%                   #Order values
  ggplot(aes(x="", y=Bookings, fill=as.factor(Publishername)))+                     #Specification of feature
  geom_bar(stat="identity", width=1)+                                               #Configuration of bar chart to fit with pie chart
  coord_polar("y", start=0)+                                                        #Creating pie chart
  theme_void()+                                                                     #Deleting potential background noise
  geom_text(aes(y=Bookings, label=Bookings),position = position_stack(vjust = .5), 
            color="black", size=4)+                                                 #Position and characteristics of legend
  scale_fill_brewer(palette="RdYlBu")+                                              #Colors of pie chart
  ggtitle("Publishers Total Volume Of Bookings")                                    #Pie chart title

