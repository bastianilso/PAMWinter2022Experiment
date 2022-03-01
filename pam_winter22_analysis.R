library(plotly)
library(tidyverse)
library(lme4)
library(MuMIn)
options("digits.secs"=6)
options(max.print=1000)

source("utils/visutils.R")
source("utils/calcutils.R")

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_pam.rda')

#############
# Summaries
#############

St <- D %>% group_by(Participant, Condition) %>%
  summarise(rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            posTrial = sum(TrialResult == "OverrideInput" | TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "ExplicitSham" | TrialResult == "AssistSuccess", na.rm=T),
            assistInput = sum(TrialResult %in% c("AssistSuccess", "AugSuccess"), na.rm=T),
            explicitSham = sum(TrialResult %in% c("ExplicitSham", "OverrideInput"), na.rm=T),
            mitigateFail = sum(TrialResult %in% c("AssistFail","MitigateFail"), na.rm=T),
            totalTrials2 = sum(!is.na(TrialResult), na.rm=T),
            totalTrials = rejInput+accInput+assistInput+explicitSham+mitigateFail,
            #fishCaught = sum(FishEvent == "FishCaught", na.rm=T),
            #fishCaught2 = sum(Event == "GameDecision" & fishFeedback == 1, na.rm=T),
            fishCaught = sum(TrialFeedback == "FishCaught", na.rm=T),
            fishReel = sum(TrialFeedback == "Reel", na.rm=T),
            fishStay = sum(TrialFeedback == "Stay", na.rm=T),
            fishUnreel = sum(TrialFeedback == "Unreel", na.rm=T),
            fishLost = sum(TrialFeedback == "FishLost", na.rm=T),
            notFishCaught = sum(Event == "GameDecision" & lead(Event) != "FishEvent"),
            reel = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & !(TrialResult %in% c("RejInput", "AssistFail", "MitigateFail"))),
            escape = sum(Event == "GameDecision" & lead(Event) != "FishEvent" & (TrialResult %in% c("RejInput"))),
            trial_rate_accept = (accInput + assistInput) / totalTrials,
            trial_rate_reject = rejInput / totalTrials,
            trial_rate_assist = assistInput / totalTrials,
            trial_rate_sham = explicitSham / totalTrials,
            trial_rate_mitigate = mitigateFail / totalTrials,
            trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
            pam_rate = 0,
            pam_rate = unique(ifelse(Condition == "AS", trial_rate_assist, pam_rate)),
            pam_rate = unique(ifelse(Condition == "IO", trial_rate_sham, pam_rate)),
            pam_rate = unique(ifelse(Condition == "MF", trial_rate_mitigate, pam_rate)),
            time_total = sum(time_delta),
            PercNormalized = unique(PercNormalized),
            Gender = unique(Gender),
            Age = unique(Age),
            FrustNormalized = unique(FrustNormalized),
            Order = unique(Order),
            Hardest = unique(Hardest),
            Easiest = unique(Easiest))

St = St %>% filter(Participant %in% c(5,6))


fig_p <- fig %>%
  add_trace(x=St$trial_rate_accept, y=St$FrustNormalized, color=St$Condition,
            type='scattergl', mode='markers', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  add_trace(x=St$trial_rate_accept, y=St$FrustNormalized, text=St$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11)),
         yaxis=list(range=c(-0.1,1.1)))
fig_p

fig_p <- fig %>%
  add_trace(x=St$trial_rate_assist, y=St$PercNormalized, color=St$Condition,
            type='scattergl', mode='markers', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  add_trace(x=St$trial_rate_assist, y=St$PercNormalized, text=St$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11), title="Fish Lost"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
fig_p
