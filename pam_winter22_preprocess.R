library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)

# Load data from directories
# For some reason we only have original data from Participant 1-10 in the folder.
# So load the data from the RDA file instead!
D <- LoadFromDirectory("data/PAM", event="Game", sample="BlinkLog")
D <- LoadFromDirectory("testdata2/PAM/", event="Game", sample="BlinkLog")
save(D, file = 'data_pam_raw.rda', compress=TRUE)

#load('data_pam_raw.rda')

#############
# Format D
#############
D <- D %>% rename(ConditionLabel = Condition, Condition = i4, Participant = i3)

D <-D %>% mutate(Participant = as.numeric(Participant),
                 Framecount = as.numeric(Framecount))


# FishEvents happen outside Input Windows
# include it to make analysis easier.
# This needs to happen before we fill and filter data outside input windows.
# Technically, these events can be considered part of the windows, afterall.
D <-D %>% mutate(InputWindowOrderFish = InputWindowOrder,
                 InputWindowOrderFish = ifelse(InputWindowOrderFish == "Stopped", NA, InputWindowOrderFish),
                 InputWindowOrderFish = as.numeric(InputWindowOrderFish),
                 InputWindowOrderFish = ifelse(Event == "FishEvent", InputWindowOrderFish-1, InputWindowOrderFish))

# It would be nice to mark input windows with a number indicating whether the input window
# gave fish feedback or not.
D <- D %>% group_by(Participant, Condition, InputWindowOrderFish) %>%
  summarise(fishFeedback = sum(FishEvent == "FishCaught", na.rm=T),
            fishLost = sum(FishEvent == "FishLost", na.rm=T)) %>%
  right_join(D)

posTrial = c("AccInput", "AugSuccess", "AssistSuccess", "ExplicitSham", "OverrideInput")
negTrial = c("RejInput")
neuTrial = c("AssistFail", "MitigateFail")

# Define TrialFeedback, a column denoting what feedback was given during the trial.
D <- D %>% mutate(TrialFeedback = NA,
                  TrialFeedback = ifelse(Event == "GameDecision", "UNDEFINED", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% posTrial, "Reel", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & fishFeedback == 1, "FishCaught", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% negTrial, "Unreel", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & fishLost == 1, "FishLost", TrialFeedback),
                  TrialFeedback = ifelse(Event == "GameDecision" & TrialResult %in% neuTrial, "Stay", TrialFeedback)
)

#cv <- D %>% group_by(Participant, Condition, TrialFeedback) %>% filter(Event == "GameDecision") %>%
#   summarize(n())
# 
#D %>% filter(Participant == 1, Condition == "AS", Event != "Sample") %>%
#  select(Participant, Condition, Timestamp, Event,
#         InputWindowOrder, TrialResult, TrialFeedback,
#         fishFeedback, fishLost) %>% view()

D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  arrange(Timestamp) %>%
  mutate(time_delta = Timestamp - lag(Timestamp),
         time_delta = as.numeric(time_delta),
         time_delta = ifelse(is.na(time_delta), 0, time_delta))



# Filter out data happening before GameRunning event.
# Filter out extra "GameStopped" events.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, isGameOver, isGame) %>% view()
# D %>% filter(Participant == 19, Condition == "AS", Event != "Sample") %>% plot_ly(x=~Timestamp, y=~Framecount)
D = D %>% ungroup() %>% group_by(Participant, Condition) %>% arrange(Timestamp) %>% #filter(Participant == 19, Condition == "AS", Event != "Sample") %>%
  mutate(isGame = ifelse(Event == "GameRunning", 1, 0),
         isGame = cumsum(isGame)) %>%
  filter(isGame == 1) %>%
  mutate(isGameOver = ifelse(Event == "GameStopped", 1,0),
         isGameOver = cumsum(isGameOver),
         isGameOver = ifelse(Event == "GameStopped", isGameOver-1,isGameOver)) %>% #select(isGame, isGameOver, Event, Timestamp) %>%
  filter(isGameOver < 1) # %>% #View()



# Create InputWindowClosedFill - creates an identifier for resting periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowClosedID, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% mutate(InputWindowClosedID = NA,
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", 1, 0),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", cumsum(InputWindowClosedID), InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameStopped", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "GameRunning", -1, InputWindowClosedID),
                 InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Open", -1, InputWindowClosedID),
                 InputWindowClosedFill = ifelse(InputWindowClosedID == 0, NA, InputWindowClosedID)) %>%
  tidyr::fill(InputWindowClosedFill, .direction="down")

# Create InputWindowOrderFilled column - an identifier for open periods.
# Test: D %>% filter(Participant == 6) %>% select(Event, Participant, Condition, InputWindowOrderWithDecision,InputWindowOrderFilled, InputWindow, InputWindowClosedFill) %>% view()
D = D %>% group_by(Participant, Condition) %>% 
  mutate(InputWindowOrder = ifelse(Event == "GameStopped", "-1", InputWindowOrder),
         InputWindowOrder = ifelse(Event == "GameRunning", "-1", InputWindowOrder),
         InputWindowOrderWithDecision = InputWindowOrder,
         InputWindowOrder = ifelse(InputWindow == "Closed", "-1", InputWindowOrder),
         Period = NA,
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", "RestPeriod", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindowClosedID == max(InputWindowClosedID, na.rm=T), "PostGame", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Open", "OpenPeriod", Period),
         Period = ifelse(Event == "GameRunning", "PreGame", Period),
         InputWindowOrderFilled = InputWindowOrder) %>%
  tidyr::fill(InputWindowOrderFilled, .direction="down") %>%
  tidyr::fill(Period, .direction="down")

# InputWindowOrder should be numeric but can contain the value "Stopped"
# if the game was interrupted. Change "Stopped" to NA.
D <-D %>% mutate(InputWindowOrder = as.numeric(InputWindowOrder),
                 InputWindowOrderWithDecision = as.numeric(InputWindowOrderWithDecision),
                 InputWindowOrderFilled = as.numeric(InputWindowOrderFilled))

D = D %>% group_by(Participant, Condition) %>%
  mutate(time_thres = lead(time_delta < 1.0),
         not_same = InputWindowOrderFilled != lead(InputWindowOrderFilled),
         not_na = InputWindowOrderFilled == -1,
         InputWindowOrderFilledSoft = ifelse( InputWindowOrderFilled != lead(InputWindowOrderFilled) &
                                                InputWindowOrderFilled == -1 & 
                                                lead(time_delta) < 1.0, lead(InputWindowOrderFilled), InputWindowOrderFilled),
         InputWindowOrderFilledSoft = ifelse(InputWindowOrderFilled != lag(InputWindowOrderFilled) &
                                               InputWindowOrderFilled == -1 & 
                                               time_delta > 1.0, lag(InputWindowOrderFilled), InputWindowOrderFilledSoft),
         InputWindowOrderFilledSoft = ifelse(InputWindowOrderFilled != lag(InputWindowOrderFilled,2) &
                                               InputWindowOrderFilled == -1 & 
                                               time_delta+lag(time_delta) > 1.0, lag(InputWindowOrderFilled,2), InputWindowOrderFilledSoft),
         InputWindowOrderFilledSoft = ifelse( InputWindowOrderFilled != lead(InputWindowOrderFilled,2) &
                                                InputWindowOrderFilled == -1 & 
                                                lead(time_delta)+lead(time_delta,2) < 1.0, lead(InputWindowOrderFilled,2), InputWindowOrderFilledSoft))

#############
# Filter out invalid conditions
#############

valid_conditions = c("AS", "NO", "IO", "MF")

D = D %>% filter(Condition %in% valid_conditions)

############
# Load Likert Data
#############

# Fill empty data for missing participants

# Load data from Google Sheets
L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1f27a167SGqvY_k1g2M8sopxzuDERl3y8qkYYheKkers/edit#gid=237988817')

# Mutate Easiest/Hardest
L = L %>% mutate(Easiest = ifelse(Easiest == "Yes",1,0),
                 Easiest = ifelse(is.na(Easiest),0,Easiest),
                 Hardest = ifelse(Hardest == "Yes",1,0),
                 Hardest = ifelse(is.na(Hardest),0,Hardest))

D <- D %>% left_join(L, by=c('Condition' = 'Condition', 'Participant' = 'Participant'))



#############
# Save to RDA
#############
save(D, file = 'data_pam.rda', compress=TRUE)
