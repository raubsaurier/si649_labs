
##----------------------------
### set up libraries ### 
##----------------------------
rm(list=ls())
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(readxl)
library(plyr)

##----------------------------
### load the datasets ### 
##----------------------------
 
#setwd("/Users/irena/repos/slvm_va/data_files/")
pollData <-  data.frame(read.csv("/Users/irena/repos/si649_labs/lab5_altair/president_polls.csv",
                                  na.strings = c(""," ", "^\\s+$|^$","NA")))


pollData$created_at_date <- as.Date(pollData$created_at, "%m/%d/%y")
pollData$start_dt <- as.Date(pollData$start_date, "%m/%d/%y")
pollData$end_dt <- as.Date(pollData$end_date, "%m/%d/%y")

# 
# h2hData <- subset(pollData, select=c("question_id", "poll_id", "state", "pct","pollster", 
#                                      "pollster_id","display_name",
#                                       "candidate_party", "candidate_name","created_at", "start_date", "end_date",
#                                       "start_dt", "end_dt"))


h2hData <- pollData %>%
  group_by(question_id, candidate_party) %>%
  mutate(sum_pct = sum(pct),
         trump_net = sum_pct - lag(sum_pct, default = pct[1])) 


h2hData <- h2hData %>%
  group_by(question_id) %>%
  mutate(max.pct = max(pct),
         winner = ifelse(max.pct==pct, "YES", "NO")) 

h2hData.2 <- h2hData %>%
  group_by(question_id) %>%
  mutate(
    dem_pct = sum(pct[candidate_party == 'DEM']) / sum(pct), # pct of vote going to dem 
    dem_lead = sum(pct[candidate_party == 'DEM']) -sum(pct[candidate_party == 'REP']) ## the margin of win/lose for dems
  ) 


summary(filter(h2hData.2, candidate_name == "Andrew Yang") %>%
          group_by(state) %>%
          summarise(mean=mean(pct),
                    sd = sd(pct), 
                    max = max(pct),
                    min=min(pct))
          )


summary(filter(h2hData.2, candidate_name == "Bernard Sanders") %>%
          group_by(state) %>%
          summarise(mean=mean(pct),
                    sd = sd(pct), 
                    max = max(pct),
                    min=min(pct))
)

## how many polls have each DEM candidate been in? 

count_dem_polls <- h2hData %>%
    filter(candidate_party=="DEM") %>%
    group_by(candidate_name) %>%
    summarize(n())

## most polls go to Biden, Sanders, Warren, Buttigieg, and Harris 
## everyone else under 50 


## in how many polls does each candidate beat Trump? 
## just select top 5 dems (Biden, Sanders, Warren, Buttigieg, Harris)

candidate_names <- c("Bernard Sanders", "Donald Trump", "Elizabeth Warren", "Pete Buttigieg", 
               "Joseph R. Biden Jr.", "Amy Klobuchar")

primaryData <- subset(h2hData.2,subset=candidate_name%in%candidate_names)

## in how many polls do candidates beat Trump?
View(primaryData %>%
  filter(dem_lead>0) %>%
  group_by(candidate_name, poll_id,state) %>%
  summarize(n()))


#### in Iowa, did trends 

### who are the most polarizing candidates?
ggplot(primaryData[(primaryData$state%in%c("Iowa","Nevada", "New Hampshire"))&(primaryData$candidate_party=="DEM"),], 
       aes(x=end_dt, y=pct, group=candidate_name,colour=candidate_name)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~state) + 
  labs(title="Democratic Candidates' General Election Favorability \n from polling conducted in IA, NH")



ggplot(primaryData[(primaryData$state=="Iowa")&(primaryData$candidate_party=="DEM"),], aes(x=start_dt, y=pct, group=candidate_name,
                                                                                           colour=candidate_name)) +
  geom_point() +
  geom_line() + 
  labs(title="Democratic Candidates' Head-to-Head % against Trump \n in Iowa over time")



### do different candidates differ by polling method? 
graphData = primaryData[primaryData$candidate_party=="DEM",]

graphData = graphData %>%
  group_by(methodology, candidate_name) %>%
  summarise(mean_pct = mean(pct))


ggplot(graphData,
       aes(x=candidate_name, y=mean_pct, group=methodology,
                                               colour=methodology)) +
  geom_point() + 
  geom_line() +
  labs(title="Democratic Candidates' Results by Polling Method")

## let's look at Bernie Sanders and how he is doing by state: 

bernieData <- primaryData[!is.na(primaryData$state),]

ggplot(bernieData[bernieData$candidate_name=="Bernard Sanders",],
       aes(x=start_dt, y=dem_lead, group=state,colour=state)) +
  geom_point() +
  geom_line() +
  scale_colour_discrete(na.translate = F) + 
  labs(title="Bernie Sanders Head-to-Head Results by State")



## hard to tell exactly what is going on, but it looks like Joe Biden and Pete Buttigieg have the highest
## variations 

### what are the states where the most # of polls are being conducted?

stateData <- data.frame(pollData[,c("state", "poll_id"), with=FALSE])

count_polls <- stateData%>%
  group_by(state) %>%
  count(poll_id)

count_polls <- count_polls %>%
  count(state)

## After NA, Texas has the highest # of polls 19, Michigan, Wisconsin are the next highest # 
## so are NC, Pennsylvania


### when are these polls being conducted? 

polls_over_time <- pollData%>%
  group_by(pollster, start_dt, end_dt) %>%
  count(poll_id) 

ggplot(polls_over_time, aes(x=start_dt, y=n, group=pollster, colour=pollster)) + 
  geom_point() +
  geom_line()+ 
  theme(legend.position = "none")


#### Where does Trump do well? 

trump_by_state <- h2hData.2%>%
  filter(candidate_name=="Donald Trump"&!is.na(state)) %>%
  group_by(state) %>%
  summarize(median_dem_lead = median(dem_lead))

ggplot(polls_over_time, aes(x=start_dt, y=n, group=pollster, colour=pollster)) + 
  geom_point() +
  geom_line()+ 
  theme(legend.position = "none")


plot_usmap(data = trump_by_state, values = "median_dem_lead") + 
  scale_fill_gradient2(
    low = "red", high = "blue",mid="purple",midpoint=0, na.value="grey70",
    name = "Median % Difference", label = scales::comma
  ) + 
  labs(title="Median Percentage Difference in H2H Matchups for Trump") +
  theme(legend.position = "right")


write.csv(trump_by_state, "repos/si649_labs/lab5_altair/state_test.csv", row.names=FALSE)
