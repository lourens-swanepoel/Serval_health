
#========== SERVAL HEALTH PAPER
# Authors 

require(dplyr)
require(ggplot2)
require(tibble)
require(tidyverse)
require(lubridate)
library(wiqid)
library(gmodels)
library(ggpubr)
# Get data

BMI <- read_csv("C:/Users/Swanepoel L/Dropbox/Papers/Students/PhD-Students/Daan Loock/Serval Health/BMI.csv", trim_ws = TRUE)
read.csv("BMI.csv",  header = TRUE)

#---------DROP 2014 data since no pathogen tests was done
SH = BMI %>% 
  filter(Year != "2014")
# We will use BMC as suggested by " Which body condition index is best?" Oikos 123: 111–119, 2014
# They suggested for Fat mass in body the logBM/logBL was best option for both sexes
SH$logW = log(SH$Weight)
SH$logL = log(SH$Length)
SH$BCI = SH$logW/SH$logL
SH$month = as.factor(months(SH$Date))
## Add season to data
library(car)
SH$season<-recode(SH$month,"c('May','June','July','September')='Dry'")
SH$season<-recode(SH$season,"c('December','November','March')='Wet'")

## plot logW to logL
SH %>%
  filter(Age == "Adult") %>%
  ggplot(aes(x = logW, y = logL)) +
  geom_point()+
  geom_smooth(method = "lm", fill = NA)

## plot logW to logL for each sex

SH %>%
  filter(Age == "Adult") %>%
  ggplot(aes(x = logW, y = logL)) +
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method = "lm", fill = NA)

# filter males

SM = SH %>%
  filter(Age == "Adult" & Sex == "M")

# filter for remales
SF = SH %>%
  filter(Age == "Adult" & Sex == "F")

## BCI scores for adults

SH %>%
  filter(Age == "Adult") %>%
  ggplot(aes(x = Sex, y = BCI)) +
  geom_boxplot()

# BCI scores for sex and age classes

SH %>%
  ggplot(aes(x = Sex, y = BCI, fill=Age)) +
  geom_boxplot()+
  theme_classic()

# BMC per month per sex

SH %>%
  mutate(month = month(SH$Date, label = TRUE)) %>% 
  filter(Age == "Adult") %>%
  count(month, Sex, Age)

#===== Fig. 4S nr of positive virusses per animal 

data.frame(expand.grid(Nr=c("0 - Virus", "1-Virus","2-viruses", "3-viruses", "4-viruses", "5-viruses")), 
           Percentage=c(18,30, 15, 15, 20, 2)) %>% 
  ggplot(aes(x = Nr, y = Percentage)) +
  geom_bar(stat='identity', position="dodge", alpha=0.1, color="black",size=1)+
  theme_classic()+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "none")+
  xlab("")

#========================================== RAINFALL DATA Fig 1S ====
## Fig 1S
### Rainfall data

Rain <- read_csv("C:/Users/Swanepoel L/Dropbox/Papers/Students/PhD-Students/Daan Loock/Serval Health/Sasol_Rainfall.csv")

Rain$Date2 <- as.Date(Rain$X1, format = "%y/%m/%d")

## rainfall data - plot for seasons
Rain %>%
  mutate(month = month(Rain$Date2, label = TRUE)) %>%
  group_by(month) %>%
  summarise(Rainfall = sum(Rainfall)) %>% 
  ggplot(aes(x = month, y = Rainfall)) +
  geom_bar(stat = "identity", alpha=0.1, color="black",size=1)

# Rainfall per month to define the seasons
# April to Sep = Dry
# Oct to Mrt = Wet

rain_sum= Rain %>%
  mutate(month = month(Rain$Date2, label = TRUE)) %>%
  group_by(month) %>%
  summarise(Rainfall = sum(Rainfall))

# Nr of captures per month, per sex (not sure if we want to add age as well?)

M_Caps_Rain = SH %>%
  mutate(month = month(SH$Date, label = TRUE)) %>% 
  count(month) %>% 
  left_join(rain_sum, by = "month")

#### Plot on one graph, rainfall as background

ylim.prim <- c(0, 450)   # rainfaill
ylim.sec <- c(0, 30)    # captures

b <- diff(ylim.sec)/diff(ylim.prim)

rain_cap = ggplot(M_Caps_Rain, aes(month, Rainfall)) +
  geom_bar(stat='identity', position="dodge", alpha=0.1, color="black",size=1) +
  geom_point(aes(y = ylim.prim[1]+(n-ylim.sec[1])/b), group = n, color = "black", size = 5, shape = 15) +
  geom_line(aes(y = ylim.prim[1]+(n-ylim.sec[1])/b), group = n, color = "black", size = 2) +
  scale_y_continuous("Precipitation", sec.axis = sec_axis(~((.-ylim.prim[1]) *b  + ylim.sec[1]), name = "Nr of captures"), limits = ylim.prim)+
  theme_classic()

sex_month = SH %>%
  mutate(month = month(SH$Date, label = TRUE)) %>% 
  count(month, Sex, Age) %>% 
  ggplot(aes(x = month, y=n, group = Age)) +
  geom_bar(stat = "identity", aes(fill = Age))+
  scale_fill_manual(values=cbp3)+
  theme_classic()+
  theme(legend.position="top")+
  ylab("Nr of captures")

cbp3<- c("#56B4E9", "#009E73", "#E69F00")
ggarrange(rain_cap,sex_month, ncol = 1, labels = c("A", "B")) ## Fig 1S

SH %>%
  count(season) %>% 
  left_join(rain_sum, by = "month")

#========================================== END Fig 1S ====
#========================================== LINEAR & BCI & SEX models & Fig 2S====
# fit linear model to BCI with sex and season as factors
# filter for adults first

SHA = SH %>%
  filter(Age == "Adult") 

# Fit all models and rank by AIC
require(wiqid)
require(epiR)

mods <- NULL
mods$mod1 = lm(BCI ~ Sex,           data=SHA)   # effect of sex on BCI
mods$mod2 = lm(BCI ~ season,        data=SHA)   # effect of season on BCI
mods$mod3 = lm(BCI ~ Sex + season,  data=SHA)   # effect of sex and season
mods$mod4 = lm(BCI ~ Sex*season,    data=SHA)   # interaction in season and sex
mods$mod4 = lm(BCI ~ 1,             data=SHA)   # Null model, no effect

AICc <- sapply(mods, AICc)
BCI_Table = as.data.frame(AICtable(AICc))

BCI_Table=BCI_Table %>% 
  add_column(Model = c("BCI ~ Sex", "BCI ~ Sex + season", "BCI ~ 1", "BCI ~ Sex*season", "BCI ~ season"), .before = "AICc")
# Large CI overlap between season BCI scores; so drop season and only retain sex
# Only sex had an affect on BCI, with males higher than females
# Plot the result
# only for adults

BCI.plot = SH %>%
  mutate(month = month(SH$Date, label = TRUE)) %>%
  filter(Age == "Adult") %>%
  ggplot(aes(x = Sex, y=BCI, fill=Sex))+
  geom_boxplot()+
  facet_grid(~season)+
  theme_classic()+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "none")

SH %>%
  mutate(month = month(SH$Date, label = TRUE)) %>%
  filter(Age == "Adult") %>%
  group_by(Sex) %>% 
  dplyr::summarize(Mean = mean(BCI, na.rm=TRUE))

library(gridExtra)
library(RGraphics)

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))

AAA <- "AICc model ranking for variables affecting BCI"

p2 = splitTextGrob(AAA)

SummaryTable = tableGrob(BCI_Table)


Fig.2S  = grid.arrange(BCI.plot, SummaryTable, ncol=1)

#========================================== FIG 2 ======================================
### Fig 1 = STUDY AREA- USE GIS TO CREATE MAP 
### For Fig 2 - annual prevelance and mean prevelance
### Need to do estimate for each virus and then combine all
### Prevelance estimates with Wald estimates

## ========== FCV
## FCV - filter out the disease data, then use it in propCI - see propCI for more details
SH %>% 
  filter(FCV == "Positive") %>%
  group_by(Year)  %>% 
  count(FCV)      %>% 
  full_join   (SH %>% 
                 filter(!is.na(FCV)) %>%
                 group_by(Year)      %>% 
                 count(FCV)          %>% 
                 summarize(Totalcap = sum(n)))
### Age test 
SH %>% 
  filter(FCV == "Positive") %>%
  group_by(Age)  %>% 
  count(FCV)      %>% 
  full_join   (SH %>% 
                 filter(!is.na(FCV)) %>%
                 group_by(Age)      %>% 
                 count(FCV)          %>% 
                 summarize(Totalcap = sum(n)))

#   Year FCV          n Totalcap
#1  2015 Positive    11       12
#2  2016 Positive     6       10
#3  2017 Positive     7       20
#4  2018 NA          NA       13

#1 Negative    31
#2 Positive    24 = 55 tested

SH %>% 
  filter(!is.na(FCV)) %>%
  count(FCV)   


FCV.P = as.data.frame(propCI(x = 24, n = 55))                     %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "All", cohort = "All", N = 55)   %>% 
  rbind(as.data.frame(propCI(x = 11, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "2015", cohort = "All", N = 12)) %>% 
  rbind(as.data.frame(propCI(x = 6, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "2016", cohort = "All", N = 10)) %>% 
  rbind(as.data.frame(propCI(x = 7, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "2017", cohort = "All", N = 20)) %>% 
  rbind(as.data.frame(propCI(x = 0, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "2018", cohort = "All", N = 13)) %>% 
  mutate(uci= ifelse(upper > 1.00,1,upper))                       %>% 
  mutate(lci= ifelse(lower < 0,0,lower))
  

FCV.plot = ggplot(FCV.P[-c(1),], aes(x=Year, y=p)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
  width=.2,size=1)+ 
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(aes(yintercept = FCV.P[1,]$p), color="black", linetype="dashed", size=1)+
  ylim(0,1)

### Age 
as.data.frame(propCI(x = 15, n = 35)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "All", cohort = "Adult", N = 35) %>% 
  rbind(as.data.frame(propCI(x = 1, n =3)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "All", cohort = "Juv", N = 3))%>% 
  rbind(as.data.frame(propCI(x = 8, n = 190)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCV", Year = "All", cohort = "Sub", N = 19)) %>% 
  ggplot(aes(x=cohort, y=p))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin=lower, ymax=upper), 
              width=.2,size=1)






## ========== FCoV 

SH %>% 
  filter(FCoV == "Positive") %>%
  group_by(Year)   %>% 
  count(FCoV)      %>% 
  full_join   (SH  %>% 
                 filter(!is.na(FCoV)) %>%
                 group_by(Year)       %>% 
                 count(FCoV)          %>% 
                 summarize(Totalcap = sum(n)))
SH %>% 
  filter(!is.na(FCoV)) %>%
  count(FCoV)   

#   Year FCoV         n Totalcap
#1  2015 Positive    12       12
#2  2016 Positive     9       10
#3  2017 Positive     4       20
#4  2018 Positive    12       13
#1 Negative    18
#2 Positive    37

FCoV.P = as.data.frame(propCI(x = 37, n = 55)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCoV", Year = "All", cohort = "All", N = 55) %>% 
  rbind(as.data.frame(propCI(x = 12, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCoV", Year = "2015", cohort = "All", N = 12))%>% 
  rbind(as.data.frame(propCI(x = 9, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCoV", Year = "2016", cohort = "All", N = 10))%>% 
  rbind(as.data.frame(propCI(x = 4, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCoV", Year = "2017", cohort = "All", N = 20))%>% 
  rbind(as.data.frame(propCI(x = 12, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FCoV", Year = "2018", cohort = "All", N = 13))%>% 
  mutate(uci= ifelse(upper > 1.00,1,upper)) %>% 
  mutate(lci= ifelse(lower < 0,0,lower))

FCoV.plot = ggplot(FCoV.P[-c(1),], aes(x=Year, y=p)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
                width=.2,size=1)+ 
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(aes(yintercept = FCoV.P[1,]$p), color="black", linetype="dashed", size=1)+
  ylim(0,1)

## ========== FHV 
SH %>% 
  filter(FHV == "Positive") %>%
  group_by(Year)  %>% 
  count(FHV)     %>% 
  full_join   (SH %>% 
                 filter(!is.na(FHV)) %>%
                 group_by(Year)      %>% 
                 count(FHV)          %>% 
                 summarize(Totalcap = sum(n)))
SH %>% 
  filter(!is.na(FHV)) %>%
  count(FHV)   

#   Year FHV          n Totalcap
#1  2015 Positive    11       12
#2  2016 Positive     5       10
#3  2017 Positive     8       20
#4  2018 NA          NA       13
#FHV          n
#1 Negative    31
#2 Positive    24 

FHV.P = as.data.frame(propCI(x = 24, n = 55)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FHV", Year = "All", cohort = "All", N = 55) %>% 
  rbind(as.data.frame(propCI(x = 11, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FHV", Year = "2015", cohort = "All", N = 12))%>% 
  rbind(as.data.frame(propCI(x = 5, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FHV", Year = "2016", cohort = "All", N = 10))%>% 
  rbind(as.data.frame(propCI(x = 8, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FHV", Year = "2017", cohort = "All", N = 20))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FHV", Year = "2018", cohort = "All", N = 13)) %>% 
  mutate(uci= ifelse(upper > 1.00,1,upper)) %>% 
  mutate(lci= ifelse(lower < 0,0,lower))


FHV.plot = ggplot(FHV.P[-c(1),], aes(x=Year, y=p)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
  width=.2,size=1)+ 
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(aes(yintercept = FHV.P[1,]$p), color="black", linetype="dashed", size=1)+
  ylim(0,1)


## ========== FPLV

SH %>% 
  filter(FPLV == "Positive") %>%
  group_by(Year)  %>% 
  count(FPLV)     %>% 
  full_join   (SH %>% 
                 filter(!is.na(FPLV)) %>%
                 group_by(Year)       %>% 
                 count(FPLV)          %>% 
                 summarize(Totalcap = sum(n)))
SH %>% 
  filter(!is.na(FPLV)) %>%
  count(FPLV)   

#   Year FPLV         n Totalcap
#1  2015 Positive     9       12
#2  2017 Positive     7       20
#3  2018 Positive     2       13
#4  2016 NA          NA       10
#FPLV         n
#1 Negative    37
#2 Positive    18

FPLV.P = as.data.frame(propCI(x = 18, n = 55)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FPLV", Year = "All", cohort = "All", N = 55) %>% 
  rbind(as.data.frame(propCI(x = 9, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FPLV", Year = "2015", cohort = "All", N = 12))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FPLV", Year = "2016", cohort = "All", N = 10))%>% 
  rbind(as.data.frame(propCI(x = 7, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FPLV", Year = "2017", cohort = "All", N = 20))%>% 
  rbind(as.data.frame(propCI(x = 2, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FPLV", Year = "2018", cohort = "All", N = 13)) %>% 
  mutate(uci= ifelse(upper > 1.00,1,upper)) %>% 
  mutate(lci= ifelse(lower < 0,0,lower))


FPLV.plot = ggplot(FPLV.P[-c(1),], aes(x=Year, y=p)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
  width=.2,size=1)+ 
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(aes(yintercept = FPLV.P[1,]$p), color="black", linetype="dashed", size=1)+
  ylim(0,1)

## ========== FIV 

SH %>% 
  filter(FIV == "Positive") %>%
  group_by(Year)  %>% 
  count(FIV)     %>% 
  full_join   (SH %>% 
                 filter(!is.na(FIV)) %>%
                 group_by(Year)      %>% 
                 count(FIV)          %>% 
                 summarize(Totalcap = sum(n)))
SH %>% 
  filter(!is.na(FIV)) %>%
  count(FIV)   

#Year FIV          n Totalcap
#1  2015 Positive     1       12
#2  2016 Positive     2       10
#3  2017 NA          NA       20
#4  2018 NA          NA       13
#FIV          n
#1 Negative    52
#2 Positive     3

FIV.P = as.data.frame(propCI(x = 3, n = 55)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FIV", Year = "All", cohort = "All", N = 55) %>% 
  rbind(as.data.frame(propCI(x = 1, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FIV", Year = "2015", cohort = "All", N = 12))%>% 
  rbind(as.data.frame(propCI(x = 2, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FIV", Year = "2016", cohort = "All", N = 10))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FIV", Year = "2017", cohort = "All", N = 20))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FIV", Year = "2018", cohort = "All", N = 13)) %>% 
  mutate(uci= ifelse(upper > 1.00,1,upper)) %>% 
  mutate(lci= ifelse(lower < 0,0,lower))


FIV.plot = ggplot(FIV.P[-c(1),], aes(x=Year, y=p)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
  width=.2,size=1)+ 
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(aes(yintercept = FIV.P[1,]$p), color="black", linetype="dashed", size=1)+
  ylim(0,1)

## ========== FLV 

SH %>% 
  filter(FLV == "Positive") %>%
  group_by(Year)  %>% 
  count(FLV)      %>% 
  full_join   (SH %>% 
                 filter(!is.na(FLV)) %>%
                 group_by(Year)      %>% 
                 count(FLV)          %>% 
                 summarize(Totalcap = sum(n)))
SH %>% 
  filter(!is.na(FLV)) %>%
  count(FLV)   

# Groups:   Year [4]
#1  2015 NA       NA       12
#2  2016 NA       NA       10
#3  2017 NA       NA       20
#4  2018 NA       NA       13


FLV.P = as.data.frame(propCI(x = 0, n = 55)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FLV", Year = "All", cohort = "All", N = 55) %>% 
  rbind(as.data.frame(propCI(x = 0, n = 12)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FLV", Year = "2015", cohort = "All", N = 12))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 10)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FLV", Year = "2016", cohort = "All", N = 10))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 20)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FLV", Year = "2017", cohort = "All", N = 20))%>% 
  rbind(as.data.frame(propCI(x = 0, n = 13)) %>% 
  filter(method == "wald") %>% 
  mutate(Disease = "FLV", Year = "2018", cohort = "All", N = 13)) %>% 
  mutate(uci= ifelse(upper > 1.00,1,upper)) %>% 
  mutate(lci= ifelse(lower < 0,0,lower))

prev.all = rbind(FCV.P,FCoV.P,FHV.P,FPLV.P,FIV.P)## combine all data to one DF for plotting

### Fig 2
### First get mean prevelance over the survey period
prev.mean = prev.all %>% 
  filter(Year == "All") %>% 
  mutate_if(is.numeric, round, 2)
## mean prevelance based on all years sampling to get SE - not correct...
prev.all %>% 
  filter(Year !="All") %>% 
  group_by(Disease ) %>%
  summarise(
    MU = mean(p),
    SD = sd(p, na.rm = TRUE))


##====== plot 
# order diseases as ones that cycles, declines and stay similar
prev.all$Disease = factor(prev.all$Disease , levels=c("FCoV","FPLV","FCV","FHV","FIV"))  
prev.mean$Disease = factor(prev.mean$Disease , levels=c("FCoV","FPLV","FCV","FHV","FIV"))  


prev.all %>% 
  filter(Year != "All") %>% 
  ggplot(aes(x=Year, y=p, colour = Year)) +
  geom_point(shape = 19, size = 6) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
  width=0,size=1.2)+ 
  facet_wrap(~Disease, nrow=2, scales = "free")+
  theme(
  strip.text.x = element_text(
  size = 12, color = "black", face = "bold.italic"),
  strip.background = element_rect(
  color="black", fill="#009E73", size=1.5, linetype="solid"))+
  labs(x="",y= "Prevalence", subtitle="", caption="") +
  geom_hline(data = prev.mean,aes(yintercept = p), color="009E73", linetype="solid", size=2)+
  geom_hline(data = prev.mean,aes(yintercept = lower), color="009E73", linetype="dotted", size=2)+
  geom_hline(data = prev.mean,aes(yintercept = upper ), color="009E73", linetype="dotted", size=2)+
  geom_text(data = prev.mean,aes(0,p,label = p), nudge_x = 0.80, nudge_y =0.09, size=6,fontface="bold")+
  geom_hline(yintercept = 0,size = 1, colour = "black")  +
  ylim(-0.09,1)+
  theme (axis.text=element_text(size=20),
  axis.title=element_text(size=20,face="bold"),
  panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(axis.ticks.length = unit(0.19, "cm"))+
  theme(axis.ticks = element_line(size = 3))+
  theme(axis.line = element_line(size = 1, colour = "black"))+
  theme(legend.position = "none") +
  theme(axis.line.x = element_line(size = 1, colour = "white"))


cbp2<- c("#56B4E9", "#009E73") 
cbp4<- c("#56B4E9", "#009E73", "#E69F00", "#D55E00")

#========================================== END FIG 2 =====
#========================================== log linear models ====
## glm logistic regression
devtools::install_github("mdonoghoe/logbin", ref = "reparam",force = TRUE )
library(logbin)
# must log lin model for each pathogen
#===== FCV - logistic regression
lR =  SH %>% 
  filter(!is.na(FCV) & Recapture=="No" & Age !="Juvenile") %>%
  mutate(FCV  = ifelse(FCV  == "Positive", 1, 0))
mod.fcv <- NULL
mod.fcv$fit.logbin1 <- logbin(FCV ~  Sex, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin2 <- logbin(FCV ~  BCI, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin3 <- logbin(FCV ~  season, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin4 <- logbin(FCV ~  Age, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin5 <- logbin(FCV ~  Sex + season, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin6 <- logbin(FCV ~  Sex + BCI, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin7 <- logbin(FCV ~  Age + season, data = lR, trace = 1, method = "em")
mod.fcv$fit.logbin8 <- logbin(FCV ~  Age + Sex, data = lR, trace = 1, method = "em")

AICc.fcv <- sapply(mod.fcv, AICc)
as.data.frame(AICtable(AICc.fcv))

exp(confint(mod.fcv$fit.logbin7))
summary(mod.fcv$fit.logbin3)
exp(coef(mod.fcv$fit.logbin3))

### Change base level to dry season
#SH$season2<-recode(SH$month,"c('May','June','July','September')='Dry'")
#SH$season2<-recode(SH$season2,"c('December','November','March')='aWet'")
#lR.2 = SH %>% 
#  filter(!is.na(FCV) & Recapture=="No" & Age !="Juvenile") %>%
#  mutate(FCV  = ifelse(FCV  == "Positive", 1, 0))

#fit.logbin.test <- logbin(FCV ~  season2, data = lR.2, trace = 1, method = "em")
#exp(coef(fit.logbin.test))

# A logistic regression is said to provide a better fit to the data if it 
# demonstrates an improvement over a model with fewer predictors. 
# This is performed using the likelihood ratio test, which compares 
# the likelihood of the data under the full model against the likelihood 
# of the data under a model with fewer predictors. 
# Removing predictor variables from a model will almost always make 
# the model fit less well (i.e. a model will have a lower log likelihood), 
# but it is necessary to test whether the observed difference in model fit is statistically significant. 
# Given that H0 holds that the reduced model is true, 
# a p-value for the overall model fit statistic that is less than 0.05 would compel 
# us to reject the null hypothesis. It would provide evidence against 
# the reduced model in favor of the current model (more variables model)

anova(mod.fcv$fit.logbin3,mod.fcv$fit.logbin7, test = "LRT")
library(ResourceSelection)
# goodness-of-fit test to consider for the log binomial regression modelis the deciles-of-risk test.
# It is described in Chapter 5 of Hosmer and Lemeshow (2000)

hl <- hoslem.test(mod.fcv$fit.logbin3$y, fitted(mod.fcv$fit.logbin3), g=10)

#This gives p=1, indicating no evidence of poor fit.

## New data to predict
  
new <- data.frame(season=factor(c("Wet","Dry")))

## Set up bootstrap to get proper CI values - we only predict to Season model 
pred.fcv <- predict(mod.fcv$fit.logbin3, type = "response", newdata = new)
n <- 1000 #number of bootstrap resamples
bootpred.fcv <- matrix(ncol = length(pred.fcv), nrow = n)
#loop over n
set.seed(1234)
for (i in seq_len(n)) {
  bootdat.fcv <- lR[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fcv <- logbin(FCV ~  season, data = bootdat.fcv, trace = 1, method = "em")#fit model to bootstrap resample
  bootpred.fcv[i,] <- predict(bootmod.fcv, type = "response", newdata = new) #calculate predictions from this model
}
CI.fcv <- as.data.frame(apply(bootpred.fcv, 2, quantile, probs = c(0.025, 0.975)))
apply(bootpred.fcv, 2, mean) # 0.3714349 (wet) 0.7768394 (dry)
###plot data
FCV.plot.inf  = as.data.frame(CI.fcv) %>% 
  rename(Wet=1, Dry=2) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(season=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl")) %>% 
  spread(CL, value = value) %>% 
  merge(fcv.season %>% 
          rename(Prop = 1) %>% 
          add_column(season= c("Wet", "Dry"))) %>% 
  ggplot(aes(x = season, y=Prop, color = season)) +
  geom_point(size = 8, stat = "identity", shape=19)+
  geom_errorbar(width=0, aes(ymin= lcl, ymax=ucl),size=2)+
  ylim(0,1)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  scale_fill_manual(values=cbp2)+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(axis.ticks.length = unit(0.19, "cm"))+
  theme(axis.ticks = element_line(size = 3))+
  theme(axis.line = element_line(size = 1, colour = "black"))+
  theme(legend.position = "none")

cbp2<- c("#56B4E9", "#009E73") 

# For Fig 5 S - supplementary figure on Sex and Season effect on pathogen infection
# Combine FCV, FCoV & FPLV
# Also support for sex and season  model (mod.fcv$fit.logbin7)
# predict and plot the sex and season model
pred.fcv.2 = predict(mod.fcv$fit.logbin5, type = "response", newdata = new.s) ## Sex and Season

# do bootstrap
n <- 1000#number of bootstrap resamples
bootpred.fcv.2 <- matrix(ncol = length(pred.fcv.2), nrow = n)
#loop over n

for (i in seq_len(n)) {
  bootdat.fcv.2<- lR[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fcv.2 <- logbin(FCV ~  Sex + season, data = bootdat.fcv.2, trace = 1,method = "em")#fit model to bootstrap resample
  bootpred.fcv.2[i,] <- predict(bootmod.fcv.2, type = "response", newdata = new.s) #calculate predictions from this model
}

# get the CI data
CI.fcv.2 <- as.data.frame(apply(bootpred.fcv.2, 2, quantile, probs = c(0.025, 0.975)))

## When plot you can see large overlap in CI for sex in each season - thus 
## better to drop sex and only retain Season in final model
## add this to sup data
FCV.season = CI.fcv.2  %>% 
  rename(MW=1, FW=2,MD=3,FD=4) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(Var=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl","lcl", "ucl","lcl", "ucl")) %>%
  add_column(Sex= c("M", "M","F", "F","M", "M","F", "F")) %>% 
  add_column(season= c("Wet", "Wet","Wet", "Wet","Dry", "Dry","Dry", "Dry")) %>% 
  spread(CL, value = value) %>%
  merge(as.data.frame(apply(bootpred.fcv.2, 2, mean)) %>% 
          rename(Prop = 1) %>% 
          add_column(season= c("Wet", "Wet","Dry","Dry"), Sex=c("M","F","M", "F"))) %>% 
  ggplot(aes(x = Var, y=Prop, color = Sex)) +
  geom_point(size = 5, stat = "identity", shape=19)+
  geom_errorbar(width=.1, aes(ymin= lcl, ymax=ucl),size=1.5)+
  ylim(0,1.2)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  facet_grid(~season, scales="free")+
  scale_x_discrete(labels= c("F","M"))+ 
  ggtitle("FCV")+
  theme(legend.position = "none")


##FCoV
###
### Not really significant effect of sex nor season
### effect RR CI overlapped with Zero, suggesting no effect

lR.fcorv =  SH %>% 
  filter(!is.na(FCORV)& Recapture=="No")  %>%
  mutate(FCORV  = ifelse(FCORV  == "Positive", 1, 0)) 

mod.fcorv   <- NULL
mod.fcorv$fit.logbin1 <- logbin(FCORV ~  Sex, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin2 <- logbin(FCORV ~  BCI, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin3 <- logbin(FCORV ~  season, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin4 <- logbin(FCORV ~  Age, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin5 <- logbin(FCORV ~  Sex + season, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin6 <- logbin(FCORV ~  Sex + BCI, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin7 <- logbin(FCORV ~  Age + season, data = lR.fcorv, trace = 1, method = "em")
mod.fcorv$fit.logbin8 <- logbin(FCORV ~  Age + Sex, data = lR.fcorv, trace = 1, method = "em")

AICc.fcorv <- sapply(mod.fcorv, AICc)
as.data.frame(AICtable(AICc.fcorv))

anova(mod.fcorv$fit.logbin5,mod.fcorv$fit.logbin4, test = "LRT") 

exp(confint(mod.fcorv$fit.logbin2))
summary(mod.fcorv$fit.logbin2)
exp(coef(mod.fcorv$fit.logbin3))

#### Change season code, to get Wet as base value
#lR.fcorv =  SH %>% 
#  filter(!is.na(FCORV)& Recapture=="No")  %>%
#  mutate(FCORV  = ifelse(FCORV  == "Positive", 1, 0)) 

#mod.fcorv.test <- logbin(FCORV ~  season2, data = lR.fcorv, trace = 1, method = "em")
#exp(coef(mod.fcorv.test))

mod.fcorv$fit.logbin2 # BCI model
# predict to BCI values from data
as.data.frame(predict(mod.fcorv$fit.logbin2 , type = "response", newdata = data.frame(BCI=lR.fcorv$BCI))) %>% 
  rename(p=1) %>% 
  cbind(lR.fcorv$BCI) %>% 
  ggplot(aes(x = lR.fcorv$BCI, y=p)) +
  geom_point(size = 5, stat = "identity", shape=19)

## Predict data to plot. Predict not good to produce CI
## Use bootsrapt to get CI

new <- data.frame(season=factor(c("Wet","Dry")))

## Set up bootstrap 
pred.fcorv <- predict(mod.fcorv$fit.logbin4 , type = "response", newdata = new)

n <- 1000 #number of bootstrap resamples
bootpred.fcorv <- matrix(ncol = length(pred.fcorv), nrow = n)

#loop over n
set.seed(1236)
for (i in seq_len(n)) {
  bootdat.fcorv <- lR.fcorv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fcorv <- logbin(FCORV ~  season, data = bootdat.fcorv, trace = 1, method = "em")#fit model to bootstrap resample
  bootpred.fcorv[i,] <- predict(bootmod.fcorv, type = "response", newdata = new) #calculate predictions from this model
}

CI.fcorv<- as.data.frame(apply(bootpred.fcorv, 2, quantile, probs = c(0.025, 0.975)))

## Bootstrap fail to produce 1000 runs in single time, break up into 10 units of 100 each
b1 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b2 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b3 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b4 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b5 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b6 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b7 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b8 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()
b9 = as.data.frame(bootpred.fcorv) %>% 
  drop_na()

bootpred.fcorv.2 = rbind(b1,b2,b3,b4,b5,b6)

CI.fcorv.2 <- as.data.frame(apply(bootpred.fcorv.2, 2, quantile, probs = c(0.025, 0.975)))

FCORV.plot.inf = CI.fcorv.2  %>% 
  rename(Wet=1, Dry=2) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(season=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl")) %>% 
  spread(CL, value = value) %>% 
  merge(fcorv.season %>% 
  rename(Prop = 1) %>% 
  add_column(season= c("Wet", "Dry"))) %>%
  ggplot(aes(x = season, y=Prop, color=season)) +
  geom_point(size = 8, stat = "identity", shape=19)+
  geom_errorbar(width=0, aes(ymin= lcl, ymax=ucl),size=2)+
  ylim(0,1)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  scale_fill_manual(values=cbp2)+
  theme (axis.text=element_text(size=20),
  axis.title=element_text(size=20,face="bold"),
  panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(axis.ticks.length = unit(0.19, "cm"))+
  theme(axis.ticks = element_line(size = 3))+
  theme(axis.line = element_line(size = 1, colour = "black"))+
  theme(legend.position = "none")

cbp2<- c("#56B4E9", "#009E73")

#FCoV
#Also support for sex and season  model
# predict and plot the sex and season model
pred.fcov.2 = predict(mod.fcorv$fit.logbin5, type = "response", newdata = new.s) ## Sex and Season

# do bootstrap
n <- 1000#number of bootstrap resamples
bootpred.fcov.2<- matrix(ncol = length(pred.fcov.2), nrow = n)
#loop over n

for (i in seq_len(n)) {
  bootdat.fcov.2<- lR.fcorv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fcov.2 <- logbin(FCORV ~  Sex + season, data = bootdat.fcov.2, trace = 1,method = "em")#fit model to bootstrap resample
  bootpred.fcov.2[i,] <- predict(bootmod.fcov.2, type = "response", newdata = new.s) #calculate predictions from this model
}

# get the CI data
CI.fcov.2 <- as.data.frame(apply(bootpred.fcov.2, 2, quantile, probs = c(0.025, 0.975)))
## Combine with FCV for Fig. 5S
## When plot you can see large overlap in CI for sex in each season - thus 
## better to drop sex and only retain Season in final model
## add this to sup data
Fcov.season = CI.fcov.2  %>% 
  rename(MW=1, FW=2,MD=3,FD=4) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(Var=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl","lcl", "ucl","lcl", "ucl")) %>%
  add_column(Sex= c("M", "M","F", "F","M", "M","F", "F")) %>% 
  add_column(season= c("Wet", "Wet","Wet", "Wet","Dry", "Dry","Dry", "Dry")) %>% 
  spread(CL, value = value) %>%
  merge(as.data.frame(apply(bootpred.fcov.2, 2, mean)) %>% 
          rename(Prop = 1) %>% 
          add_column(season= c("Wet", "Wet","Dry","Dry"), Sex=c("M","F","M", "F"))) %>% 
  ggplot(aes(x = Var, y=Prop, color = Sex)) +
  geom_point(size = 5, stat = "identity", shape=19)+
  geom_errorbar(width=.1, aes(ymin= lcl, ymax=ucl),size=1.5)+
  ylim(0,1.2)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  facet_grid(~season, scales="free")+
  scale_x_discrete(labels= c("F","M"))+ 
  ggtitle("FCoV")+
  theme(legend.position = "none")


## Some support for BCI Fig. 6S
## predict and plot BCI
##FCoV

mod.fcorv.bci <- logbin(FCORV ~  BCI, data = lR.fcorv, trace = 1, method = "em")

BCI.pred = data.frame(BCI = lR.fcorv$BCI)

BCI.pred.2 = as.data.frame(predict(mod.fcorv$fit.logbin2, type = "response", newdata =BCI.pred ))

# do bootstrap
n <- 1000#number of bootstrap resamples
bootpred.fcov.bci<- matrix(ncol = length(BCI.pred.2), nrow = n)
#loop over n


# to get CI from fitted model - goes over 1, use bootstrap instead
var.x <- diag(model.matrix(mod.fcorv.bci ) %*% vcov(mod.fcorv.bci) %*% t(model.matrix(mod.fcorv.bci)))
lcl.x <- exp(predict(mod.fcorv.bci) - qnorm(0.975) * sqrt(unique(var.x)))
ucl.x <- exp(predict(mod.fcorv.bci) + qnorm(0.975) * sqrt(unique(var.x)))

cbind(BCI.pred,BCI.pred.2,lcl.x,ucl.x)

Fig.6S = cbind(BCI.pred,BCI.pred.2,lcl.x,ucl.x) %>% 
  ggplot(aes(x =  BCI, y=predict(mod.fcorv$fit.logbin2, type = "response", newdata = BCI.pred))) +
  geom_point(size = 5, stat = "identity", shape=19)+
  geom_ribbon(aes(ymin=lcl.x,ymax=ucl.x),alpha=0.5)+
  ylim(0,1.5)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  ggtitle("FCoV")+
  theme(legend.position = "none")


## FHV 
lR.fhv =  SH %>% 
  filter(!is.na(FHV)& Recapture=="No")  %>%
  mutate(FHV  = ifelse(FHV  == "Positive", 1, 0))

mod.fhv  <- NULL
mod.fhv$fit.logbin1 <- logbin(FHV ~  Sex, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin2 <- logbin(FHV ~  BCI, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin3 <- logbin(FHV ~  season, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin4 <- logbin(FHV ~  Age, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin5 <- logbin(FHV ~  Sex + season, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin6 <- logbin(FHV ~  Sex + BCI, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin7 <- logbin(FHV ~  Age + season, data = lR.fhv, trace = 1, method = "em")
mod.fhv$fit.logbin8 <- logbin(FHV ~  Age + Sex, data = lR.fhv, trace = 1, method = "em")


AICc.fhv <- sapply(mod.fhv, AICc)
as.data.frame(AICtable(AICc.fhv))

exp(confint(mod.fhv$fit.logbin3))
summary(mod.fhv$fit.logbin3)
exp(coef(mod.fhv$fit.logbin3))

#### Changeing the order order of season
#mod.fhv.test <- logbin(FHV ~  season2, data = lR.fhv, trace = 1, method = "em")
#exp(coef(mod.fhv.test))

## Set up bootstrap 
pred.fhv <- predict(mod.fhv$fit.logbin3, type = "response", newdata = new)

n <- 1000 #number of bootstrap resamples
bootpred.fhv.coef <- matrix(ncol = length(pred.fhv ), nrow = n)
## Boot to get coefs
#loop over n
set.seed(1237)
for (i in seq_len(n)) {
  bootdat.fhv <- lR.fhv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fhv <- logbin(FHV ~  season, data = bootdat.fhv, trace = 1, method = "em")#fit model to bootstrap resample
  bootpred.fhv.coef [i,] <- exp(coef(bootmod.fhv)) #calculate predictions from this model
}

apply(bootpred.fhv.coef, 2, mean) #0.3467977
as.data.frame(apply(bootpred.fhv, 2, mean)) %>% 
  cbind(new)

#loop over n
set.seed(1236)
for (i in seq_len(n)) {
  bootdat.fhv <- lR.fhv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fhv <- logbin(FHV ~  season, data = bootdat.fhv, trace = 1, method = "em")#fit model to bootstrap resample
  bootpred.fhv[i,] <- predict( bootmod.fhv, type = "response", newdata = new) #calculate predictions from this model
}

CI.fhv <- as.data.frame(apply(bootpred.fhv, 2, quantile, probs = c(0.025, 0.975)))

apply(bootpred.fhv, 2, mean) #0.3418631 0.9999995

### Plot
#season       lcl       ucl      Prop
#1    Dry 0.9999994 0.9999995 0.9999995
#2    Wet 0.1764706 0.4886628 0.3397598

FHV.plot.inf = CI.fhv %>% 
  rename(Wet=1, Dry=2) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(season=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl")) %>% 
  spread(CL, value = value) %>% 
  merge(  as.data.frame(apply(bootpred.fhv, 2, mean)) %>% 
  rename(Prop = 1) %>% 
  add_column(season= c("Wet", "Dry"))) %>% 
  ggplot(aes(x = season, y=Prop, color = season)) +
  geom_point(size = 8, stat = "identity", shape=19)+
  geom_errorbar(width=0, aes(ymin= lcl, ymax=ucl),size=2)+
  ylim(0,1)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  scale_fill_manual(values=cbp2)+
  theme (axis.text=element_text(size=20),
  axis.title=element_text(size=20,face="bold"),
  panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(axis.ticks.length = unit(0.19, "cm"))+
  theme(axis.ticks = element_line(size = 3))+
  theme(axis.line = element_line(size = 1, colour = "black"))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())

##FPLV

lR.fplv =  SH %>% 
  filter(!is.na(FPLV)& Recapture=="No")  %>%
  mutate(FPLV  = ifelse(FPLV  == "Positive", 1, 0)) #%>% 
#group_by(season, Sex) %>% 
#count(FHV) %>%           
#mutate(prop = prop.table(n))

mod.fplv   <- NULL
mod.fplv$fit.logbin1 <- logbin(FPLV ~  Sex, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin2 <- logbin(FPLV ~  BCI, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin3 <- logbin(FPLV ~  season, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin4 <- logbin(FPLV ~  Age, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin5 <- logbin(FPLV ~  Sex + season, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin6 <- logbin(FPLV ~  Sex + BCI, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin7 <- logbin(FPLV ~  Age + season, data = lR.fplv, trace = 1, method = "em")
mod.fplv$fit.logbin8 <- logbin(FPLV ~  Age + Sex, data = lR.fplv, trace = 1, method = "em")


AICc.fplv <- sapply(mod.fplv , AICc)
as.data.frame(AICtable(AICc.fplv))

anova(mod.flpv$fit.logbin1,mod.flpv$fit.logbin5, test = "LRT") #0.05 dropping season had a sig improvment

exp(confint(mod.flpv$fit.logbin5))
summary(mod.flpv$fit.logbin1)
exp(coef(mod.flpv$fit.logbin1))

## Set up bootstrap 
new.sex <- data.frame(Sex=factor(c("M","F")))

#newdata1 <- with(lR.fplv, data.frame(Sex = Sex, season = season))

pred.fplv.2 <- predict(mod.fplv$fit.logbin1, type = "response", newdata = new.sex)

n <- 1000#number of bootstrap resamples
#bootpred.fplv <- matrix(ncol = length(pred.fplv), nrow = n)
bootpred.fplv.2 <- matrix(ncol = length(pred.fplv.2), nrow = n)
#loop over n
for (i in seq_len(n)) {
  bootdat.fplv.2<- lR.fplv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fplv.2 <- logbin(FPLV ~  Sex, data = bootdat.fplv.2, trace = 1,method = "em")#fit model to bootstrap resample
  bootpred.fplv.2[i,] <- predict(bootmod.fplv.2, type = "response", newdata = new.sex) #calculate predictions from this model
}

b16 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()
b17 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()
b18 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()
b19 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()
b20 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()
b15 = as.data.frame(bootpred.fplv.2) %>% 
  drop_na()

bootpred.fplv.3 = rbind(b16,b17,b18,b19,b20)

CI.fplv <- as.data.frame(apply(bootpred.fplv.2, 2, quantile, probs = c(0.025, 0.975)))

FPLV.plot.inf = as.data.frame(CI.fplv.2) %>% 
  rename(M=1, F=2) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(Sex=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl")) %>% 
  spread(CL, value = value) %>% 
  merge(as.data.frame(apply(bootpred.fplv.3, 2, mean)) %>% 
          rename(Prop = 1) %>% 
          add_column(Sex=c("M","F"))) %>% 
  ggplot(aes(x = Sex, y=Prop, color = Sex)) +
  geom_point(size = 8, stat = "identity", shape=19)+
  geom_errorbar(width=0, aes(ymin= lcl, ymax=ucl),size=2)+
  ylim(0,1)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  scale_color_manual(values=cbp4)+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(axis.ticks.length = unit(0.19, "cm"))+
  theme(axis.ticks = element_line(size = 3))+
  theme(axis.line = element_line(size = 1, colour = "black"))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())

cbp2<- c("#56B4E9", "#009E73") 
cbp4<- c("darkgreen", "#9633FF")
cbp4<- c("#56B4E9", "#009E73", "#E69F00", "#D55E00")

## Sex & Season model
## Bootstrap for Sex and Season to show high CI overlap
new.s <- data.frame(expand.grid(Sex=factor(c("M","F")), season=factor(c("Wet","Dry"))))
pred.fplv <- predict(mod.fplv$fit.logbin5, type = "response", newdata = new.s)
for (i in seq_len(n)) {
  bootdat.fplv<- lR.fplv[sample(1:n, replace = TRUE),] #bootstrap resample of data
  bootmod.fplv <- logbin(FPLV ~  Sex + season, data = bootdat.fplv, trace = 1,method = "em")#fit model to bootstrap resample
  bootpred.fplv[i,] <- predict(bootmod.fplv, type = "response", newdata = new.s) #calculate predictions from this model
}

CI.fplv <- as.data.frame(apply(bootpred.fplv, 2, quantile, probs = c(0.025, 0.975), na.rm=TRUE))

## Combine for Fig. 5S
fplv.sex = CI.fplv  %>% 
  rename(MW=1, FW=2,MD=3,FD=4) %>% 
  rownames_to_column(var="CL") %>% 
  gather("CL") %>%
  rename(Var=1) %>% 
  add_column(CL= c("lcl", "ucl","lcl", "ucl","lcl", "ucl","lcl", "ucl")) %>%
  add_column(Sex= c("M", "M","F", "F","M", "M","F", "F")) %>% 
  add_column(season= c("Wet", "Wet","Wet", "Wet","Dry", "Dry","Dry", "Dry")) %>% 
  spread(CL, value = value) %>%
  merge(as.data.frame(apply(bootpred.fplv, 2, mean, na.rm=TRUE)) %>% 
  rename(Prop = 1) %>% 
  add_column(season= c("Wet", "Wet","Dry","Dry"), Sex=c("M","F","M", "F"))) %>% 
  ggplot(aes(x = Var, y=Prop, color = season)) +
  scale_color_manual(values=cbp4)+
  geom_point(size = 5, stat = "identity", shape=19)+
  geom_errorbar(width=.1, aes(ymin= lcl, ymax=ucl),size=1.5)+
  ylim(0,1.2)+
  theme(legend.position="bottom", legend.box = "horizontal")+
  labs(fill='') +
  labs(x="",y= "Proplabity of infection", subtitle="", caption="") +
  theme_classic(base_size =20)+
  facet_grid(~Sex, scales="free")+
  scale_x_discrete(labels= c("Dry","Wet"))+ 
  ggtitle("FPLV")+
  theme(legend.position = "none")

library(ggpubr)

## Plot Fig. 3
ggarrange(FCV.plot.inf,FHV.plot.inf,FCORV.plot.inf,FPLV.plot.inf,
          labels = c("A (FCV)", "B (FHV)", "C (FCoRV)", "D (FPLV) - Sex only"),
          ncol = 2, nrow = 2)

#### Plot suplemtary Fig. 5S for CI overlap FCV, FCoRV, FPLV
ggarrange(FCV.season , Fcov.season, fplv.sex,
          ncol = 2, nrow = 2)
