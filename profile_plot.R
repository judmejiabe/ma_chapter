################################################################################
## What Benefits Drive Membership in Medicare Advantage Plans? (Profile Plot) ##
####           Authors: Jiarui Yu and Juan Diego Mejia Becerra              ####
################################################################################

#Clear memory
rm(list = ls())



#Loads tidyverse
library(tidyverse)

#Loads dataset
Enrollment_Total <- read_excel("Enrollment_Total.xlsx")

#Concatenates Contract, Plan and Segment IDs
Enrollment_Total$Contract_Plan_Segment <- paste0(Enrollment_Total$Contract_ID, 
                                              "-", Enrollment_Total$Plan_ID,
                                              "-", Enrollment_Total$Segment_ID)
#Shifts year column
Enrollment_Total$Year <- Enrollment_Total$Year - 2018

#Generates the profile plot
ggplot(data = Enrollment_Total, 
       aes(x = Year, y = State_Enrollment, color = Contract_ID)) +
  geom_point() +
  facet_wrap(~ Contract_Plan_Segment) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  facet_wrap(~ Contract_Plan_Segment) +
  labs(title = "Changes in Enrollment from 2019 to 2022",
       x = "Year",
       y = "Enrollment",
       color = NULL)

