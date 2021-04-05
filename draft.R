'''
Question we want to answer ? 

AGE(int) : Patient age 

FEMALE(int) : Female / Other - categorical  

RACE(int) : Race [1,...6] - categorical 

APRDRG(int) : ? Groupings maybe - categorical 

LOS(int) : Length of stay [ probably days] - numerical

TOTCHG(num) : Total Charge to the hospital  - numerical 
'''

library(corrplot)
library(ggplot2)
setwd('~/Desktop/Stat355/Final-Project/')
data <- read.csv("HospitalCosts.csv")
## Preprocess ##
sum(is.na(data)) # 1 NA entry. 
data <- na.omit(data)
str(data)

## Corr matrix ## 
data.matrix <- as.matrix(data)
length(data.matrix)
corrplot(cor(data), type = "upper", "number")


'1. wants to find the age category
of people who frequent the hospital and has the maximum expenditure.
'
# Total charges by age
AGE.groups <- seq(0,17)
age.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=TOTCHG)) +
           geom_bar(stat="identity") +
           scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups)
age.TOTCHG

age.LOS <- ggplot(data, aes(AGE, LOS, fill=LOS)) +
  geom_bar(stat="identity") +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups)
age.LOS

'2. Wants to find the diagnosis related group
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

'3.needs to analyze if
the race of the patient is related to the hospitalization costs.
'

plot(data$RACE, data$TOTCHG, pch=19) # no visible trend 


RACE.TOTCHG <- ggplot(data, aes(RACE, TOTCHG, fill=RACE)) +
  geom_bar(stat="identity") 
RACE.TOTCHG


RACE.factored <- as.factor(data$RACE)
RACE.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=RACE.factored)) +
  geom_bar(stat="identity",position=position_dodge()) 
RACE.TOTCHG

'4. has to analyze the severity of the
hospital costs by age and gender for proper allocation of resources.
'
install.packages("viridis")  
library("viridis")


str(data)
pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot(data, aes(factor(AGE), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) + 
  coord_flip() + 
  theme_minimal()

FEMALE.factored <- ifelse(data$FEMALE == 1, 'FEMALE', 'NOT FEMALE' )

ggplot(data, aes(factor(FEMALE.factored), TOTCHG)) + 
  geom_boxplot()

ggplot(data, aes(factor(FEMALE.factored), TOTCHG)) + 
  geom_bar(stat='identity',fill = "#ff6668") +
  coord_flip()

model1 <- lm(TOTCHG ~ AGE + FEMALE, data=data)
summary(model1)

# F-statistic: 6.581 on 2 and 496 DF,  p-value: 0.001511

'5. the agency wants to find if the length of stay can be predicted
from age, gender, and race.
'

model2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data=data)
summary(model2)

'6.To perform a complete analysis, the agency wants to find the variable that
mainly affects the hospital costs.
'

model3 <- lm(TOTCHG ~ ., data=data)
summary(model3)


