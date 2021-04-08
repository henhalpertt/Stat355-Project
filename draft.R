'''
AGE(int) : Patient age 

FEMALE(int) : Female / Other - categorical  

RACE(int) : Race [1,...6] - categorical 

APRDRG(int) : Groupings  - categorical (for example, 640 - infants group)

LOS(int) : Length of stay [ probably days] - numerical

TOTCHG(num) : Total Charge to the hospital  - numerical 
'''
install.packages("viridis")  
install.packages("robust")  
library(corrplot)
library(ggplot2)
library(viridis)
library(robust)

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
?corrplot
'1. wants to find the age category
of people who frequent the hospital and has the maximum expenditure.

imbalanced data: how many babies compared to other age groups ? 
xtabs(~AGE,data=data)s - 306 individuals are babies.
'

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
the race[cat]of the patient is related to the hospitalization costs[num].
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


'4. has to analyze the severity of the
hospital costs by age and gender for proper allocation of resources.
'

str(data)
pal <- wes_palette("Zissou1", 100, type = "continuous")

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


model4 <- lm(TOTCHG ~ AGE + LOS + APRDRG, data=data)
summary(model4)

'AGE - 
TOTCHG - 
LOS - '

'Adjusted R-squared:  0.5479 '


''
##################################################################
##########################   For Report    #######################
##################################################################
require(gridExtra)

AGE.groups <- seq(0,17)
age.TOTCHG <- ggplot(data, aes(AGE, TOTCHG, fill=AGE)) +
  geom_bar(stat="identity", show.legend = F) +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups) +
  labs(y = "Total Charges") +
  age.TOTCHG

age.LOS <- ggplot(data, aes(AGE, LOS, fill=AGE)) +
  geom_bar(stat="identity") +
  scale_x_continuous("AGE", labels = as.character(AGE.groups), breaks = AGE.groups) +
  scale_fill_binned(name = "AGE")
age.LOS

group.TOTCHG <- ggplot(data, aes(APRDRG, TOTCHG, fill=APRDRG)) +
  geom_bar(stat="identity", show.legend = F) +
  xlim(600,650) + 
  labs(y = "Total Charges", x='Group')
group.TOTCHG

group.LOS <- ggplot(data, aes(APRDRG, LOS, fill=APRDRG)) +
  geom_bar(stat="identity", show.legend = T) + 
  xlim(600,650) +
  scale_fill_binned(name = "Group") +
  labs(x='Group')
group.LOS

age.totchg.bp <- ggplot(data, aes(factor(AGE), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) + 
  coord_flip() + 
  theme_minimal() +
  labs(y='Total Charges', x='Age')
age.totchg.bp
grid.arrange(arrangeGrob(age.TOTCHG, age.LOS,
                         group.TOTCHG, group.LOS, ncol = 2),
             age.totchg.bp, nrow=1)


totch.female.bp <- ggplot(data, aes(factor(FEMALE.factored), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) +
  coord_flip() +
  labs(x='', y='Total Charges')
totch.female.bp

RACE.factored <- as.factor(data$RACE)
race.totchg.bp <- ggplot(data, aes(factor(RACE.factored), TOTCHG)) + 
  geom_boxplot(aes(fill ='#ff6668'), show.legend = F) +
  coord_flip() +
  labs(x='Race Groups', y='Total Charges')
race.totchg.bp

grid.arrange(totch.female.bp, race.totchg.bp, nrow=1)

