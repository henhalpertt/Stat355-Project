'
The Features:

AGE(int) : Patient age 

FEMALE(int) : Female / Other - categorical  

RACE(int) : Race [1,...6] - categorical 

APRDRG(int) : Groupings  - categorical (for example, 640 - infants group)

LOS(int) : Length of stay [ probably days] - numerical

TOTCHG(num) : Total Charge to the hospital  - numerical 
'
########################## Packages #########################
install.packages("viridis")  
install.packages("robust")  
library(corrplot)
library(ggplot2)
library(viridis)
library(robust)
library(GGally)
########################## LOAD + Preprocess #########################
setwd('~/Desktop/Stat355/Final-Project/') # please change to your directory. 
data <- read.csv("HospitalCosts.csv")
sum(is.na(data)) # 1 NA entry. 
data <- na.omit(data)
str(data)

########################## Correlations + pairs #########################

corrplot(cor(data), type = "upper", "number") # please run this line separately 

pairs.plot <- ggpairs(data, 
                      upper = list(continuous = wrap("cor", size = 7, color='coral4')),
                      lower = list(continuous = wrap("points",size=2, color='gray27')))+
  theme_bw() 
pairs.plot

##################################################################
##########################   For Report    #######################
##################################################################
require(gridExtra)

####################### Bar Plots ################################

AGE.groups <- seq(0,17)
age.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=AGE)) +
  geom_bar(stat="identity", show.legend = F) +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups) +
  labs(y = "Total Charges") +
  ggtitle('Plot 1') + 
  theme(plot.title = element_text(hjust = 0.5))

age.LOS <- ggplot(data, aes(AGE, LOS, fill=AGE)) +
  geom_bar(stat="identity") +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups) +
  scale_fill_binned(name = "AGE") + 
  ggtitle('Plot 2') + 
  theme(plot.title = element_text(hjust = 0.5))

group.TOTCHG <- ggplot(data, aes(APRDRG, TOTCHG, fill=APRDRG)) +
  geom_bar(stat="identity", show.legend = F) +
  xlim(600,650) + 
  labs(y = "Total Charges", x='Group') +
  ggtitle('Plot 3') + 
  theme(plot.title = element_text(hjust = 0.5))

group.LOS <- ggplot(data, aes(APRDRG, LOS, fill=APRDRG)) +
  geom_bar(stat="identity", show.legend = T) + 
  xlim(600,650) +
  scale_fill_binned(name = "Group") +
  labs(x='Group') +
  ggtitle('Plot 4') + 
  theme(plot.title = element_text(hjust = 0.5))

age.TOTCHG = age.TOTCHG + theme(axis.text=element_text(size=12),
                                axis.title=element_text(size=14,face="bold"))

age.LOS = age.LOS + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold"))


group.TOTCHG = group.TOTCHG + theme(axis.text=element_text(size=12),
                                    axis.title=element_text(size=14,face="bold"))


group.LOS = group.LOS + theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold"))

# Arange plots
grid.arrange( age.TOTCHG, age.LOS,
              group.TOTCHG, group.LOS, ncol = 2)

################### Boxplots ###########################

age.totchg.bp <- ggplot(data, aes(factor(AGE), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) + 
  coord_flip() + 
  theme_minimal() +
  labs(y='Total Charges', x='Age')+
  ggtitle('Plot 3') + 
  theme(plot.title = element_text(hjust = 0.5))


FEMALE.factored <- ifelse(data$FEMALE == 1, 'FEMALE', 'NOT FEMALE' )
totch.female.bp <- ggplot(data, aes(FEMALE.factored, TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) +
  coord_flip() +
  theme_minimal()+
  labs(x='GENDER', y='Total Charges')+
  ggtitle('Plot 1') + 
  theme(plot.title = element_text(hjust = 0.5))


RACE.factored <- as.factor(data$RACE)
race.totchg.bp <- ggplot(data, aes(factor(RACE.factored), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) +
  coord_flip() +
  theme_minimal()+
  labs(x='Race Groups', y='Total Charges')+
  ggtitle('Plot 2') + 
  theme(plot.title = element_text(hjust = 0.5))


totch.female.bp = totch.female.bp + theme(axis.text=element_text(size=12),
                                          axis.title=element_text(size=14,face="bold"))

race.totchg.bp = race.totchg.bp + theme(axis.text=element_text(size=12),
                                        axis.title=element_text(size=14,face="bold"))

age.totchg.bp = age.totchg.bp + theme(axis.text=element_text(size=12),
                                      axis.title=element_text(size=14,face="bold"))
#Align plots
grid.arrange(arrangeGrob(totch.female.bp, race.totchg.bp, nrow=2), age.totchg.bp, nrow=1  )
################################################################################
##########################   Kaggle dataset questions    #######################
################################################################################

'1. We want to find the age category
of people who frequent the hospital and has the maximum expenditure.
'
xtabs(~AGE,data=data)
xtabs(~APRDRG,data=data)
data$TOTCHG
# Total charges by age
AGE.groups <- seq(0,17)
age.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=AGE)) +
  geom_bar(stat="identity") +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups)
age.TOTCHG

age.LOS <- ggplot(data, aes(AGE, LOS, fill=AGE)) +
  geom_bar(stat="identity") +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups)
age.LOS

'2. We Want to find the diagnosis related group
that has maximum hospitalization and expenditure.
'

group.TOTCHG <- ggplot(data, aes(APRDRG, TOTCHG, fill=TOTCHG)) +
  geom_bar(stat="identity" )
group.TOTCHG

group.TOTCHG <- ggplot(data, aes(APRDRG, TOTCHG, fill=TOTCHG)) +
  geom_bar(stat="identity", show.legend = F) +
  xlim(600,650)
group.TOTCHG

group.LOS <- ggplot(data, aes(APRDRG, LOS, fill=LOS)) +
  geom_bar(stat="identity")
group.LOS

group.LOS <- ggplot(data, aes(APRDRG, LOS, fill=LOS)) +
  geom_bar(stat="identity", show.legend = F) + 
  xlim(600,650)
group.LOS

#unique(data$APRDRG)
#xtabs(~APRDRG,data=data)

'3. We want to analyze if
the race of the patient is related to the hospitalization costs.
'

plot(data$RACE, data$TOTCHG, pch=19) # no visible trend 

RACE.groups <- seq(1,6)
RACE.TOTCHG <- ggplot(data, aes(RACE, TOTCHG, fill=RACE)) +
  geom_bar(stat="identity", show.legend = F) +
  scale_x_continuous("RACE", labels = as.character(RACE.groups), breaks = RACE.groups)
RACE.TOTCHG


RACE.factored <- as.factor(data$RACE)
RACE.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=RACE.factored)) +
  geom_bar(stat="identity",position=position_dodge()) 
RACE.TOTCHG


race_binary <-  data$RACE == 1
log_race <-  glm(race_binary ~ data$TOTCHG, family = binomial, data = data)
summary(log_race)
cost_binary = data$TOTCHG >= median(data$TOTCHG) 
log_race2 <-  glm(race_binary ~ cost_binary, family = binomial, data = data)
summary(log_race2)
cost_binary2 <-  data$TOTCHG >= mean(data$TOTCHG) 
log_race3 <-  glm(race_binary ~ cost_binary2, family = binomial, data = data)
summary(log_race3)
# Using logistic regression there does not appear to be any relationship between the patient's race and the cost of their hospitalization.


'4. We want to analyze the severity of the
hospital costs by age and gender for proper allocation of resources.
'

ggplot(data, aes(factor(AGE), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) + 
  coord_flip() + 
  theme_minimal()

FEMALE.factored <- ifelse(data$FEMALE == 1, 'FEMALE', 'NOT FEMALE' )

ggplot(data, aes(factor(FEMALE.factored), TOTCHG)) + 
  geom_boxplot() + 
  coord_flip()


ggplot(data, aes(factor(FEMALE.factored), TOTCHG)) + 
  geom_bar(stat='identity',fill = "#ff6668") +
  coord_flip()

model1 <- lm(TOTCHG ~ AGE + FEMALE, data=data)
summary(model1)


'5. We want to find if the length of stay can be predicted
from age, gender, and race.
'

model2 <- lm(LOS ~ AGE + FEMALE + RACE, data=data)
summary(model2)
# LOS cannot be predicted from AGE FEMALE RACE 

'6.We want to perform a complete analysis
'

model3 <- lm(TOTCHG ~ ., data=data)
summary(model3)

model4 <-  lm(TOTCHG ~ AGE + LOS + APRDRG + FEMALE, data=data)
summary(model4)

#### Final Models ### 
model5 <- lm(TOTCHG ~ AGE + LOS + APRDRG, data=data,)
summary(model5)

model6 <-  lmRob(TOTCHG ~ AGE + LOS + APRDRG,data=data)
summary(model6)

