################################################################################
####   What Benefits Drive Membership in Medicare Advantage Plans? (Code)   ####
####           Authors: Juan Diego Mejia Becerra & Jiarui Yu                ####
################################################################################

### 1. Library & Data Loading

#Clear memory
rm(list = ls())



#Loads Libraries
library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)

source('macros.txt')


#Loads raw data
Benefit_Total <- read_excel("Benefit_Total.xlsx", na = "NA")
Enrollment_Total <- read_excel("Enrollment_Total.xlsx")

#Merges Contract #, Plan # and Segment # as a unique identifier
Benefit_Total$Contract_Plan_Segment <- paste0(Benefit_Total$Contract_ID, 
                                              "-", Benefit_Total$Plan_ID,
                                              "-", Benefit_Total$Segment_ID)
Enrollment_Total$Contract_Plan_Segment <- paste0(Enrollment_Total$Contract_ID, 
                                                "-", Enrollment_Total$Plan_ID,
                                                "-", Enrollment_Total$Segment_ID)


###2. Descriptive Statistics
#The code below manually classifies variables into continuous, dummy and
#categorical
continuous_variables <- c('Premium', 
                          'Annual_Deductible',
                          'In_MOOP',
                          'Out_MOOP',
                          'In_HP_Deductible',
                          'Out_HP_Deductible',
                          'In_Inpatient',
                          'In_Inpatient_numdays',
                          'Out_Inpatient',
                          'Out_Inpatient_numdays',
                          'In_Primary_Min',
                          'In_Primary_Max',
                          'Out_Primary_Min',
                          'Out_Primary_Max',
                          'Out_Primary_Coin',
                          'In_Specialist_Min',
                          'In_Specialist_Max',
                          'Out_Specialist_Min',
                          'Out_Specialist_Max',
                          'Out_Specialist_Coin',
                          'Emergency_Care',
                          'Urgent_Care_Min',
                          'Urgent_Care_Max',
                          'In_Ambulance',
                          'In_Ambulance_Coin',
                          'Out_Ambulance_Min',
                          'Out_Ambulance_Max',
                          'Out_Ambulance_Coin',
                          'In_Vision_Exam',
                          'Out_Vision_Exam',
                          'Out_Vision_Exam_Coin',
                          'In_Hearing_Exam',
                          'Out_Hearing_Exam_Min',
                          'Out_Hearing_Exam_Max',
                          'Out_Hearing_Exam_Coin',
                          'Tier1',
                          'Tier2',
                          'Tier3',
                          'Tier4',
                          'Tier4_Coin',
                          'Tier5_Coin',
                          'OTC_Yearly',
                          '# of preventive Dental Coverage',
                          '# of comprehensive Dental Coverage')

dummy_variables <- c('Drug_Coverage',
                     'In_Inpatient_inf',
                     'Out_Inpatient_inf',
                     '14c1_Health_Education',
                     '14c10_In-Home_Safety_Assessment',
                     '14c11_Personal_Emergency_Response_System',
                     '14c12_Medical_Nutrition_Therapy',
                     '14c13_In-Home_Medication_Reconciliation',
                     '14c14_Re-admission_Prevention',
                     '14c15_Wigs_for_Hair_Loss',
                     '14c16_Weight_Management_Programs',
                     '14c17_Alternative_Therapies',
                     '14c2_Nutritional/Dietary_Benefit',
                     '14c3_Smoking_Tobacco_Counseling',
                     '14c4_Fitness_Benefit',
                     '14c5_Enhanced_Disease_Management',
                     '14c6_Telemonitoring_Services',
                     '14c7_Remote_Access_Technologies',
                     '14c8_Bathroom_Safety_Devices',
                     '14c9_Counseling_Services',
                     '14c18_Therapeutic_Massage',	
                     '14c19_Adult_Day_Health_Services',
                     '14c20_Home-Based_Palliative_Care',
                     '14c21_In-Home_Support_Services',
                     '14c22_Support_for_Caregivers_of_Enrollees')


categorical_variables <- c('Star_Rating')

#The functions below compute several statistics ignoringmissing values
min_ <- function(x)
  return(min(x, na.rm = T))

max_ <- function(x)
  return(max(x, na.rm = T))

q1 <- function(x)
  return(quantile(x, probs = 0.25, na.rm = T))

q3 <- function(x)
    return(quantile(x, probs = 0.75, na.rm = T))

mean_ <- function(x)
  return(mean(x, na.rm = T))

median_ <- function(x)
  return(quantile(x, probs = 0.5, na.rm = T))

sd_ <- function(x)
  return(sd(x, na.rm = T))

iqr <- function(x)
  return(quantile(x, probs = 0.75, na.rm = T) - 
           quantile(x, probs = 0.25, na.rm = T))

count_NAs <- function(x)
  return(sum(is.na(x)))


#Computes some descriptive stats

#Continuous variables
Continuous_Benefit_Descriptive_Stats <-
  Benefit_Total %>% 
    group_by(Year) %>%
    select(all_of(c(continuous_variables, 'Year')))%>%
    summarize_all(list(min = min_, 
                       max = max_,
                       q1 = q1,
                       q3 = q3,
                       mean = mean_,
                       median = median_,
                       sd = sd_,
                       iqr = iqr))

#Dummy variables
Dummy_Benefit_Descriptive_Stats <- 
  Benefit_Total %>% 
    group_by(Year) %>% 
    select(all_of(c(dummy_variables, 'Year'))) %>% 
    summarize_all(list(frequency = mean))

#Categorical Variables
Categorical_Benefit_Cat_Stats <- 
  Benefit_Total %>% 
  select(all_of(c(categorical_variables, 'Year'))) %>%
  mutate(Star_Rating_4.5 = ifelse(Star_Rating == 4.5 & 
                                    ! is.na(Star_Rating), 1, 0)) %>% 
  mutate(Star_Rating_4.0 = ifelse(Star_Rating == 4 & 
                                    ! is.na(Star_Rating), 1, 0)) %>%
  mutate(Star_Rating_New = ifelse(is.na(Star_Rating), 1, 0)) %>%
  mutate(Star_Rating_3.5 = ifelse(Star_Rating == 3.5 & 
                                    ! is.na(Star_Rating), 1, 0)) %>%
  mutate(Star_Rating_3.0 = ifelse(Star_Rating == 3 & 
                                    ! is.na(Star_Rating), 1, 0)) %>%
  group_by(Year) %>%
  select(-c('Star_Rating')) %>%
  summarize_all(list(sum = sum))
  



### 3. Data Normalization

#We transform those variables having min and max into min and range
Normalized_Benefit_Data <-
  Benefit_Total %>% 
  mutate(In_Primary_Range = In_Primary_Max - In_Primary_Min,
         Out_Primary_Range = Out_Primary_Max - Out_Primary_Min,
         In_Specialist_Range = In_Specialist_Max - In_Specialist_Min,
         Out_Specialist_Range = Out_Specialist_Max - Out_Specialist_Min,
         Urgent_Care_Range = Urgent_Care_Max - Urgent_Care_Min,
         Out_Ambulance_Range = Out_Ambulance_Max - Out_Ambulance_Min,
         Out_Hearing_Exam_Range = Out_Hearing_Exam_Max - Out_Hearing_Exam_Min)


#Adds the range to continuous variables
continuous_variables <- c(continuous_variables,
                          'In_Primary_Range',
                          'Out_Primary_Range',
                          'In_Specialist_Range',
                          'Out_Specialist_Range',
                          'Urgent_Care_Range',
                          'Out_Ambulance_Range',
                          'Out_Hearing_Exam_Range')

scale_minmax <- function(x)
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  
Normalized_Benefit_Data <- 
  Normalized_Benefit_Data %>% 
    #minmax normalization for dummy variables
    mutate_at(continuous_variables, scale_minmax) %>%
    #one-hot encoding for Star_Rating
    mutate(Star_Rating_4.5 = ifelse(Star_Rating == 4.5 & 
                                      ! is.na(Star_Rating), 1, 0)) %>% 
    mutate(Star_Rating_4.0 = ifelse(Star_Rating == 4 & 
                                      ! is.na(Star_Rating), 1, 0)) %>%
    mutate(Star_Rating_New = ifelse(is.na(Star_Rating), 1, 0)) %>%
    mutate(Star_Rating_3.5 = ifelse(Star_Rating == 3.5 & 
                                      ! is.na(Star_Rating), 1, 0)) %>%
    mutate(Star_Rating_3.0 = ifelse(Star_Rating == 3 & 
                                      ! is.na(Star_Rating), 1, 0)) %>%
    #Drops the variables below. Explanation:
    #Star_Rating is encoded already,
    #In_Ambulance_Coin are all NAs
    #Out_Ambulance_Coin only has one plan that is not NA
    #Q1 website is not useful for modelling purposes
    #The 14c... variables are identically zero
    #The ..._Max variables are encoded in the corresponding ..._Min variables
    #plus ..._Range
    #Take Star_Rating_New as a comparison baseline
    select(- c('Star_Rating',
               'In_Ambulance_Coin', 
               'Out_Ambulance_Coin', 
               'Q1 website',
               '14c13_In-Home_Medication_Reconciliation',
               '14c14_Re-admission_Prevention',
               '14c17_Alternative_Therapies',
               '14c5_Enhanced_Disease_Management',
               '14c6_Telemonitoring_Services',
               '14c18_Therapeutic_Massage',
               '14c19_Adult_Day_Health_Services',
               '14c20_Home-Based_Palliative_Care',
               '14c21_In-Home_Support_Services',
               '14c22_Support_for_Caregivers_of_Enrollees',
               'In_Primary_Max',
               'Out_Primary_Max',
               'In_Specialist_Max',
               'Out_Specialist_Max',
               'Urgent_Care_Max',
               'Out_Ambulance_Max',
               'Out_Hearing_Exam_Max',
               'Star_Rating_New'))

#NA handling
has_NAs <- function(x)
  return(sum(is.na(x)) > 0)

names_ <- names(Normalized_Benefit_Data)

for(i in 1:dim(Normalized_Benefit_Data)[2]){
  if(has_NAs(Normalized_Benefit_Data[i])){
    #Sets dummy variable for NAs
    Normalized_Benefit_Data[paste0('NA_', names_[i])] <- 
      is.na(Normalized_Benefit_Data[i])
  }
}

Normalized_Benefit_Data <- Normalized_Benefit_Data %>% replace(is.na(.), 0)

#Change names of the NA with more self-explanatory names according to the 
#info. contained in Benefit_Dictionary.xlsx
Normalized_Benefit_Data <-
  Normalized_Benefit_Data %>% 
    rename(No_Out_MOOP = NA_Out_MOOP,
           No_Out_HP_Deductible = NA_Out_HP_Deductible,
           Coin_In_Inpatient_numdays = NA_In_Inpatient_numdays,
           Coin_Out_Primary = NA_Out_Primary_Min,
           Coin__Out_Specialist_Min = NA_Out_Specialist_Min,
           Coin_Out_Ambulance = NA_Out_Ambulance_Min,
           No_In_Vision_Exam = NA_In_Vision_Exam,
           Coin_Out_Hearing_Exam = NA_Out_Hearing_Exam_Min,
           No_OTC = NA_OTC_Yearly) %>%
    mutate(No_Out_Inpatient = NA_Out_Inpatient * NA_Out_Inpatient_Coin,
           Coin_Out_Inpatient = NA_Out_Inpatient - No_Out_Inpatient,
           No_Out_Vision_Exam = NA_Out_Vision_Exam * NA_Out_Vision_Exam_Coin,
           Coin_Out_Vision_Exam = NA_Out_Vision_Exam - No_Out_Vision_Exam,
           Coin_Tier4 = NA_Tier4 - (1 - Drug_Coverage)) %>%
    #The variables below are excluded for the following reason
    #NA_Annual_Deductible means the variable has no drug coverage
    #NA_Out_Inpatient is encoded in No_Out_Inpatient and Coins_Out_Inpatient
    #NA_Out_Inpatient_numdays same
    #NA_Out_Inpatient_Coin same
    #NA_Out_Specialist_Max is encoded in Coins_Specialist_Min
    #NA_Out_Specialist_Coin Same
    #NA_Out_Ambulance_Max is encoded in NA_Out_Ambulance_Max
    #NA_Tier1, NA_Tier2, NA_Tier3, NA_Tier4 and 
    #NA_Tier4_Coin is included in Coin_Tier4
    #NA_Tier5_Coin are encoded in Drug_Coverage
    select(- c(NA_Annual_Deductible,
               NA_Out_Inpatient,
               NA_Out_Inpatient_numdays,
               NA_Out_Inpatient_Coin,
               NA_Out_Primary_Range,
               NA_Out_Primary_Coin,
               NA_Out_Specialist_Range,
               NA_Out_Specialist_Coin,
               NA_Out_Ambulance_Range,
               NA_Out_Vision_Exam,
               NA_Out_Vision_Exam_Coin,
               NA_Out_Hearing_Exam_Range,
               NA_Out_Hearing_Exam_Coin,
               NA_Tier1, 
               NA_Tier2, 
               NA_Tier3,
               NA_Tier4,
               NA_Tier4_Coin,
               NA_Tier5_Coin))

### 4. Merge Enrollment Data

Normalized_Data <- 
  Normalized_Benefit_Data %>% left_join(Enrollment_Total)

### 5. PCR

#### 5.1. PCA
df_for_pca <- Normalized_Benefit_Data %>%
  select(-c(Contract_ID, Plan_ID, Segment_ID, Contract_Plan_Segment)) %>%
  mutate_if(is.logical, as.numeric)

pca_res <- PCA(df_for_pca, scale.unit = T, ncp = 67, graph = F)

eig_val <- get_eigenvalue(pca_res)

eig_val_df <- eig_val %>% as.data.frame()
eig_val_df$no_of_components = 1:dim(eig_val_df)[1]

cumulative_variance_plot <- ggplot(data = eig_val_df, 
                                   aes(x = no_of_components,
                                       y = cumulative.variance.percent)) +
  geom_line(colour = 'steelblue') + geom_point(colour = 'steelblue') +
  labs(title = 'Cumulative Variance Plot', 
       y = 'Cumulative Variance Explained (%)',
       x = 'No. of Principal Components') +
  theme_bw(20)

cumulative_variance_plot

#### 5.2. Regression Step

#Gets the projected observations
ind <- get_pca_ind(pca_res)

#Builds the datastet for pcr
data_for_pcr <- 
  bind_cols(list(ind$coord, select(Normalized_Data, 
                                   c(State_Enrollment, Total_Eligibles))))

data_for_pcr <- data_for_pcr %>% 
  mutate(c_log_mkt_share = log(State_Enrollment / Total_Eligibles) - 
           mean(log(State_Enrollment / Total_Eligibles))) %>%
  select(-c(State_Enrollment, Total_Eligibles))



#Run OLS on the full set of variables
pcr <- lm(c_log_mkt_share ~ -1 + ., data = data_for_pcr)
summary(pcr)

#Influence Plot for the full model
pcr_cooks_d <- cooks.distance(pcr)
plot(pcr_cooks_d, 
     main = 'Influence Plot', 
     ylab = "Cook's Distance", 
     xlab = 'Index', 
     pch = 16)
abline(h = 1)
abline(h = 0.5)

#Leverage plot for the full model
pcr_hat_diag <- hatvalues(pcr)
plot(pcr_hat_diag, 
     main = 'Leverage Plot', 
     ylab = 'Leverage', 
     xlab = 'Index', 
     pch = 16)

#Forward variable selection using the p.value criterion
step_glm(pcr, criterion = 'p.value', direction = 'forward')

#Reduced formula, obtained via forward p.value criterion
reduced_formula <- 
  c_log_mkt_share ~ 0 + Dim.4 + Dim.1 + Dim.47 + Dim.24 + Dim.33 + Dim.9 + 
  Dim.14 + Dim.17 + Dim.13 + Dim.12 + Dim.32 + Dim.11 + Dim.40 + Dim.38 + 
  Dim.8 + Dim.26 + Dim.41 + Dim.10 + Dim.25 + Dim.63 + Dim.42 + Dim.27 + Dim.5 



#Fits the reduced mode. Observations 5 (H3528-14-0 Y2019) and 
#110 (H3528-17-0 Y2022) are eliminated as they're outliers
pcr_2 <- lm(reduced_formula, data = data_for_pcr, subset = -c(5, 110))
summary(pcr_2)

#Saves default graphical settings. To be used latter
.pardefault <- par(no.readonly = F)

#Displays the leverage and influence plots side-by-side in the same panel
par(mfrow = c(1, 2))
pcr_cooks_d_2 <- cooks.distance(pcr_2)
pcr_hat_diag_2 <- hatvalues(pcr_2)
plot(pcr_hat_diag_2, 
     main = 'Leverage Plot', 
     ylab = 'Leverage', 
     xlab = 'Index', 
     pch = 16)

plot(pcr_cooks_d_2, 
     main = 'Influence Plot', 
     ylab = "Cook's Distance", 
     xlab = 'Index', 
     pch = 16)
abline(h = 1)

#resets to default graphical settings
par(.pardefault)

plot(pcr_2$fitted.values, rstudent(pcr_2), 
     main = 'Externally Studentized Residuals vs Fitted Values',
     xlab = 'Fitted', ylab =  'Externally Studentized Residuals', 
     pch = 16)
abline(h = 0)

envelope_glm(pcr_2, rep = 10000)
par(.pardefault)



#### 5.3 Transformation Step
V <- get_pca_var(pca_res)$coord[, c(4,  1,  47,  24,  33, 9,  14, 17, 13,
                                    12, 32, 11,  40,  38, 8,  26, 41, 10,
                                    25, 63, 42,  27,  5)]


beta_pcr <- V %*% as.matrix(coef(pcr_2)) %>% as.data.frame()
arrange(beta_pcr, desc(V1)) %>% View()

