library(tidyverse)
library(randomForest)

# Read the data
df <- read_csv("College_Updated_5_24TH.csv")

# Select columns
dft <- df[c('Pitcher', 'PitcherTeam', 'TaggedPitchType', 'PitchCall', 'PlayResult', 'RelSpeed', 'SpinRate', 'RelHeight', 'RelSide', 'Extension', 'InducedVertBreak', 'HorzBreak', 'PlateLocHeight', 'PlateLocSide')]

# Filter and transform the data
Swings <- dft %>% filter(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))
Whiff <- Swings %>% mutate(whiff=ifelse(PitchCall=="StrikeSwinging", 1, 0))
HB <- Whiff %>% mutate(HorzBreak = abs(HorzBreak))
RS <- HB %>% mutate(RelSide = abs(RelSide))
DiffBreak <- RS %>% mutate(Diffbreak= abs(InducedVertBreak-HorzBreak)) %>% na.omit()
FBDiffBreak <- DiffBreak[DiffBreak$TaggedPitchType %in% c('Fastball', 'Sinker', 'TwoSeamFastBall', 'FourSeamFastBall', 'OneSeamFastBall'),] %>% na.omit()

# Prepare predictor variables and response variable
X_fb <- FBDiffBreak[,c('RelSpeed', 'SpinRate', 'Diffbreak', 'RelHeight', 'RelSide', 'Extension')]
Y_fb <- FBDiffBreak$whiff  # Note: No need to convert this to factor now

# Bind the predictor variables and response variable in a data frame
data_rf <- cbind(X_fb, Y_fb)

# Split the data
set.seed(12345678)
Split= 0.7
N <- nrow(data_rf)
TrainingSize <- round(N*Split)
TrainingCases <- sample(N, TrainingSize)
Training = data_rf[TrainingCases, ]
Test= data_rf[-TrainingCases, ]

# Load necessary library
library(glm2)

# Fit the logistic regression model
logistic_model <- glm(Y_fb ~ ., data = Training, family = binomial())

# Predict probabilities
FBDiffBreak$predicted_whiff <- predict(logistic_model, newdata = X_fb, type = "response")

#find mean of predicted whiff
mean(FBDiffBreak$predicted_whiff, na.rm = TRUE)


#create a new column called stuff+ which is (predicted_whiff / mean of predicted_whiff * 100)
library(dplyr)
mean_predicted_whiff <- mean(FBDiffBreak$predicted_whiff, na.rm = TRUE)


#Calculate stuff+ 
FBDiffBreak <- FBDiffBreak %>%
  mutate(`stuff+` = (predicted_whiff / mean_predicted_whiff) * 100)

#groupby Pitcher
FBDiffBreak_summary <- FBDiffBreak %>%
  group_by(Pitcher) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), count = n())

#Jack Steele Values 
Steele_Jack_Data <- FBDiffBreak_summary %>%
  filter(Pitcher == "Steele, Jack")

#New Dataframe Minimum 100 pitches
FBDiffBreak_summary_over_100 <- FBDiffBreak_summary %>%
  filter(count > 100)



