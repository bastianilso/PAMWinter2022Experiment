library(plotly)
library(tidyverse)
library(lme4)
library(MuMIn)
options("digits.secs"=6)
options(max.print=1000)

source("utils/visutils.R")
source("utils/clmcalcutils.R")

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_pam.rda')

posTrialLabels = c("OverrideInput","AccInput","AugSuccess","ExplicitSham","AssistSuccess")
posTrialLabels_user = c("AccInput","AugSuccess","AssistSuccess")

#############
# Summaries
#############

# excluded participants - they had 0% recognition in some conditions
excluded = c(7, 11, 15) 
D <- D %>% filter(!Participant %in% excluded)

St <- D %>% group_by(Participant, Condition) %>% 
  summarise(rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            posTrial = sum(TrialResult %in% posTrialLabels, na.rm=T),
            triggerTrial = sum(TrialResult == "AccInput" | TrialResult == "AugSuccess" | TrialResult == "AssistSuccess", na.rm=T),
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
            trial_rate_accept = ifelse(is.nan(trial_rate_accept), 0, trial_rate_accept),
            trial_rate_reject = rejInput / totalTrials,
            trial_rate_reject = ifelse(is.nan(trial_rate_reject), 0, trial_rate_reject),
            trial_rate_assist = assistInput / totalTrials,
            trial_rate_assist = ifelse(is.nan(trial_rate_assist), 0, trial_rate_assist),
            trial_rate_sham = explicitSham / totalTrials,
            trial_rate_sham = ifelse(is.nan(trial_rate_sham), 0, trial_rate_sham),
            trial_rate_mitigate = mitigateFail / totalTrials,
            trial_rate_mitigate = ifelse(is.nan(trial_rate_mitigate), 0, trial_rate_mitigate),
            trial_rate_positive = (accInput+assistInput+explicitSham) / (totalTrials-mitigateFail),
            trial_rate_positive = ifelse(is.nan(trial_rate_positive), 0, trial_rate_positive),
            pam_rate = 0,
            pam_rate = unique(ifelse(Condition == "AS", trial_rate_assist, pam_rate)),
            pam_rate = unique(ifelse(Condition == "IO", trial_rate_sham, pam_rate)),
            pam_rate = unique(ifelse(Condition == "MF", trial_rate_mitigate, pam_rate)),
            time_total = sum(time_delta),
            PercNormalized = unique(PercNormalized),
            PercNormalized.f = unique(PercNormalized.f),
            PerceivedControlEpisode = unique(PerceivedControlEpisode),
            Gender = unique(Gender),
            Age = unique(Age),
            FrustNormalized = unique(FrustNormalized),
            FrustNormalized.f = unique(FrustNormalized.f),
            FrustrationEpisode = unique(FrustrationEpisode),
            Fatigue = unique(Fatigue),
            bci_experience = unique(BCIExp),
            Order = unique(Order),
            Hardest = unique(Hardest),
            Easiest = unique(Easiest),
            PerceivedPerformance = unique(PerceivedPerformance),
            Participant.f = unique(Participant.f),
            ConditionLabel = unique(ConditionLabel),
            Gender.f = unique(Gender.f),
            Condition.f = unique(Condition.f),
            Order.f = unique(Order.f),
            Perc.f = unique(Perc.f),
            Frust.f = unique(Frust.f))
            #accRecogRate should be calculated from the actual measured blinks vs total trials.
            #accRecogRate = posTrial / sum(TrialGoal == "OverrideInput" | TrialGoal == "AccInput" | TrialGoal == "AugSuccess" | TrialGoal == "ExplicitSham" | TrialGoal == "AssistSuccess", na.rm=T),
            #accRecogRate = ifelse(is.nan(accRecogRate), 0, accRecogRate))
            
            # Blink Recognition: How many blinks were recognized in practice.
            # Blink Conv. Rate: How many blinks resulted in positive trial outcomes.
            # blinkTrial= no. of trials which had blinks /  posTrial = positive trial outcomes
      


St = St %>% mutate(ConditionLabel = ifelse(ConditionLabel == "AugmentedSucces","Aug.\n Success",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "MitigatedFailure","Mit.\n Failure",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "OverrideInput","Overr.\n Input",ConditionLabel),
                   ConditionLabel = ifelse(ConditionLabel == "Control","Ref.",ConditionLabel))

# Some participants have NaN in their results

# Blink counts
St <- D %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod"),
  ) %>% right_join(St)


# Summary of Input Windows. Filter out negative period orders, focus only on actual input windows.
Si <- D %>% ungroup() %>% filter(PeriodOrder != "-1") %>% group_by(Participant, Condition, PeriodOrder) %>%
  summarize(TrialFeedback = paste(unique(TrialFeedbackWindow), collapse=" "),
            TrialResult = paste(unique(TrialResultWindow), collapse=" "),
            blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta))

# Each Input Window, what happened (MI?), TrialResult for the InputWindow, ..
#D %>% ungroup() %>% filter(Period %in% c("OpenPeriod","DecisionPeriod")) %>% group_by(Condition) %>%
#  filter(Participant == 1) %>% select(Event, TrialFeedbackWindow, TrialResultWindow, Condition, PeriodOrder,InputWindow) %>% view()

# Check what caused different outcomes in the input windows.
#Si %>% group_by(Condition, TrialResult) %>% summarize(blink_recog = sum(blink_recog_window == 1),
#                                           no_recog = sum(blink_recog_window == 0)) %>% view()

# Mark the affected Input Windows
#Si %>% group_by(Condition, TrialResult) %>% mutate(invalid_col = ifelse(blink_recog_window == 0 & TrialResult == "AccInput", T,NA)) %>% view()
#D %>% filter(Condition == AS"", Participant == 1, InputWindowOrderWithRest == 5) %>% select(Timestamp, Event, Period, TrialResult) %>% view()


# Group by input window. Count the number of attempts in each window.
St = Si %>% group_by(Participant, Condition) %>%
  summarize(blink_recog_trial = sum(blink_recog_window > 0),
            blink_conv_trial = sum(blink_recog_window > 0 & TrialResult %in% posTrialLabels_user),
            blink_recog_window = sum(blink_recog_window),
            blink_recog_window_count = sum(blink_recog_window_count),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)






#D %>% ungroup() %>% filter(Participant == 15, Period %in% c("OpenPeriod")) %>% group_by(Participant, Condition, InputWindowOrderFilledSoft) %>% view()


St <- St %>% mutate(time_window = ifelse(is.na(time_window), 0, time_window),
             blink_recog_window = ifelse(is.na(blink_recog_window), 0, blink_recog_window),
             blink_conv_trial = ifelse(is.na(blink_conv_trial), 0, blink_conv_trial),
             blink_recog_trial = ifelse(is.na(blink_recog_trial),0,blink_recog_trial),
             time_window_min = ifelse(is.na(time_window_min),0,time_window_min))
  
# Si = Summary of Input Windows
# Feedback Delay
# Fabricated delay
Si <- D %>% filter(PeriodOrder != -1) %>% group_by(Participant, Condition, PeriodOrder) %>%
  filter(Event == "EyeOpening") %>%
  summarize(last_attempt_stamp = max(Timestamp)) %>% right_join(Si)

Si <- D %>% ungroup() %>% group_by(Participant, Condition, PeriodOrder) %>%
  filter(Event == "GameDecision") %>%
  summarize(feedback_stamp = max(Timestamp)) %>% right_join(Si)

Si <- Si %>% filter(PeriodOrder != -1) %>%
  mutate(feedback_delay = feedback_stamp - last_attempt_stamp,
         feedback_delay = as.numeric(feedback_delay),
         feedback_delay = ifelse(is.na(feedback_delay), 0, feedback_delay))

#fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=Si, type='scatter')

St <- Si %>% group_by(Participant, Condition) %>%
  summarize(mean_delay = mean(feedback_delay)) %>% right_join(St) %>%
  mutate(mean_delay = ifelse(is.na(mean_delay), 0, mean_delay))


St <- St %>% group_by(Participant, Condition) %>%
  summarize(rate_total_blink = triggerTrial / blinks_total) %>%
  mutate(rate_total_blink = ifelse(rate_total_blink == Inf, 0, rate_total_blink),
         rate_total_blink = ifelse(is.na(rate_total_blink), 0, rate_total_blink)) %>% right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_feedback = posTrial/ totalTrials,
         rate_feedback = ifelse(is.nan(rate_feedback), 0, rate_feedback),
         session = 1,
         session = cumsum(session))

# Calculate blink recognition and blink conversion rate
St <- St %>% mutate(
  # blink recognition: in how many trials were blinks recognized out of 20
  blink_recog = blink_recog_trial / totalTrials,
  # blink conv. rate: how many blinks triggered positive feedback out of total blink attempts.
  blink_conv_rate = blink_conv_trial / blink_recog_window_count
  )

St <- St %>% mutate(
  rate_help = 0,
  rate_help = ifelse(Condition == "AS", assistInput, rate_help),
  rate_help = ifelse(Condition == "IO", explicitSham, rate_help),
  rate_help = ifelse(Condition == "MF", mitigateFail, rate_help),
  rate_help = rate_help / totalTrials
)

Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(Gender = unique(Gender),
            Age = unique(Age),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_feedback = mean(rate_feedback),
            mean_rate_blink = mean(blink_conv_rate),
            mean_blink_recog = mean(blink_recog)
            )

Sc <- St %>% group_by(Condition) %>% 
  summarize(
    PercNormalized_SD = sd(PercNormalized),
    Perc.f_SD = sd(as.numeric(Perc.f)),
    FrustNormalized_SD = sd(FrustNormalized),
    Frust.f_SD = sd(as.numeric(Frust.f)),
    rate_feedback_SD = sd(rate_feedback),
    rate_blink_SD = sd(blink_conv_rate),
    rate_help_SD = sd(rate_help),
    blink_recog_SD = sd(blink_recog),
    fishCaught_SD = sd(fishCaught),
    fishLost_SD = sd(fishLost),
    fishReel_SD = sd(fishReel),
    fishUnreel_SD = sd(fishUnreel),
    time_total_SD = sd(time_total),
    PercNormalized = mean(PercNormalized),
    Perc.f = mean(as.numeric(Perc.f)),
    FrustNormalized = mean(FrustNormalized),
    Frust.f = mean(as.numeric(Frust.f)),
    rate_feedback = mean(rate_feedback),
    rate_blink = mean(blink_conv_rate),
    blink_recog = mean(blink_recog),
    fishCaught = mean(fishCaught),
    fishLost = mean(fishLost),
    fishReel = mean(fishReel),
    fishUnreel = mean(fishUnreel),
    rate_help = mean(rate_help),
    time_total = mean(time_total),
    Hardest = sum(Hardest),
    Easiest = sum(Easiest)
  )


Sc_old <- St %>% ungroup() %>% group_by(Condition) %>%
  summarize(
    mean_perceived_performance = mean(PerceivedPerformance, na.rm=T),
    sd_perceived_performance = sd(PerceivedPerformance, na.rm=T),
    mean_pam_rate = mean(pam_rate),
    sd_pam_rate = sd(pam_rate),
    mean_perc = mean(PercNormalized),
    mean_percEp = mean(PerceivedControlEpisode),
    sd_perc = sd(PercNormalized),
    mean_frust = mean(FrustNormalized),
    mean_frustEp = mean(FrustrationEpisode),
    sd_frust = sd(FrustNormalized),
    mean_feedback = mean(trial_rate_positive),
    sd_feedback = sd(trial_rate_positive),
    mean_hardest = sum(ifelse(Hardest,0,1)),
    mean_easiest = sum(ifelse(Easiest,0,1)),
    mean_fishCaught = mean(fishCaught),
    mean_fishLost = mean(fishLost),
    fishCaught_lower = min(fishCaught),
    fishCaught_upper = max(fishCaught),
  )


#save(St, file = 'pamSurrogate.rda', compress=TRUE)

#############
# ICC3 Scores
#############
Sicc <- tibble(
  PercNormalized = St %>% ungroup() %>% select(Participant, PercNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  FrustNormalized = St %>% ungroup() %>% select(Participant, FrustNormalized, Condition) %>%
    pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
    ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  Hardest = St %>% ungroup() %>% select(Participant, Hardest, Condition) %>%
    pivot_wider(names_from = Participant, values_from = Hardest) %>%
    replace_na(list(`12` = 0)) %>% ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
  Easiest = St %>% ungroup() %>% select(Participant, Easiest, Condition) %>%
    pivot_wider(names_from = Participant, values_from = Easiest) %>%
    replace_na(list(`12` = 0)) %>% ungroup() %>% select(-Condition) %>% 
    psych::ICC(.) %>% unlist(.) %>% .[["results.ICC3"]],
) %>% 
  select(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, Hardest, Easiest) %>%
  pivot_longer(cols=everything(), names_to = "Variables", values_to="ICC3")


#############
# Latex Table: Group Level Features
#############
cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1,1.5,2.2),
             lv_percf = c(0.1, 2.3,4.5,5.8,7.8,8.8,9.8),
             lv_frust = rev(lv_perc),
             lv_frustf = rev(lv_percf),
             lv_rate = c(-0.1, 0.35, 0.55, 0.85, 1.1, 1.5, 2.2),
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),
             lv_reel = c(0,3,6,9,12,15,18),
             lv_unreel = rev(c(0,3,6,9,12,15,18)),
             lv_fish = c(0,2,4,6,10,12,14),
             lv_diff = rev(c(0,2,4,6,10,12,14)),
             lv_lost = c(8,4,2,1,0,-1,-2),
             lv_time = c(115,130,145,160,175,190,210),
             lv_icc = c(-0.1,0.5,0.73,0.75,0.77,0.90,1.00),
             lv_help = c(-0.1,0.15,0.25,0.35,0.45,0.90,1.00))

Sc_table <- Sc %>% ungroup() %>% group_by(Condition) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    percf_c = t_color(Perc.f, cri$lv_percf, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    frustf_c = t_color(Frust.f, cri$lv_frustf, cri$colors),
    rate_c = t_color(rate_blink, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    rate_acc_c = t_color(blink_recog, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    reel_c = t_color(fishReel, cri$lv_reel, cri$colors),
    unreel_c = t_color(fishUnreel, cri$lv_unreel, cri$colors),
    time_c = t_color(time_total, cri$lv_time, cri$colors),
    help_c = t_color(rate_help, cri$lv_help, cri$colors),
    hardest_c = t_color(Hardest, cri$lv_diff, cri$colors),
    easiest_c = t_color(Easiest, cri$lv_diff, cri$colors),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    Perc.f = format(round(Perc.f,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    Frust.f = format(round(Frust.f,2), nsmall = 2),
    fishCaught = format(round(fishCaught,2), nsmall = 2),
    fishLost = format(round(fishLost,2), nsmall = 2),
    fishReel = format(round(fishReel,2), nsmall = 2),
    fishUnreel = format(round(fishUnreel,2), nsmall = 2),
    #blink_recog = format(round(blink_recog,2), nsmall = 2),
    time_total = format(round(time_total,0), nsmall = 0),
    PercNormalized_SD = format(round(PercNormalized_SD,2), nsmall = 2),
    Perc.f_SD = format(round(Perc.f_SD,2), nsmall = 2),
    Frust.f_SD = format(round(Frust.f_SD,2), nsmall = 2),
    FrustNormalized_SD = format(round(FrustNormalized_SD,2), nsmall = 2),
    rate_feedback_SD = format(round(rate_feedback_SD,2), nsmall = 2),
    rate_blink_SD = format(round(rate_blink_SD,2), nsmall = 2),
    rate_help_SD = format(round(rate_help_SD,2), nsmall = 2),
    blink_recog_SD = format(round(blink_recog_SD,2), nsmall = 2),
    fishCaught_SD = format(round(fishCaught_SD,2), nsmall = 2),
    fishLost_SD = format(round(fishLost_SD,2), nsmall = 2),
    fishReel_SD = format(round(fishReel_SD,2), nsmall = 2),
    fishUnreel_SD = format(round(fishUnreel_SD,2), nsmall = 2),
    time_total_SD = format(round(time_total_SD,0), nsmall = 0),
    rate_blink = paste0(format(round(rate_blink * 100,0), nsmall = 0),"\\%"),
    rate_help = paste0(format(round(rate_help * 100,0), nsmall = 0),"\\%"),
    blink_recog = paste0(format(round(blink_recog * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized, " (", PercNormalized_SD, ")"),
    Perc.f = paste0("\\cellcolor{", percf_c, "}", Perc.f, " (", Perc.f_SD, ")"),
    Frust.f = paste0("\\cellcolor{", frustf_c, "}", Frust.f, " (", Frust.f_SD, ")"),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized, " (", FrustNormalized_SD, ")"),
    rate_blink = paste0("\\cellcolor{", rate_c, "}", rate_blink, " (", rate_blink_SD,")"),
    rate_help = paste0("\\cellcolor{", help_c, "}", rate_help, " (", rate_help_SD,")"),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback,  " (", rate_feedback_SD, ")"),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught, " (", fishCaught_SD, ")"),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost, " (", fishLost_SD, ")"),
    fishReel = paste0("\\cellcolor{", reel_c, "}", fishReel, " (", fishReel_SD, ")"),
    fishUnreel = paste0("\\cellcolor{", unreel_c, "}", fishUnreel, " (", fishUnreel_SD, ")"),
    blink_recog = paste0("\\cellcolor{", rate_acc_c, "}", blink_recog, " (", blink_recog_SD, ")"),
    time_total = paste0("\\cellcolor{", time_c, "}", time_total, "s (", time_total_SD, "s)"),
    Hardest = paste0("\\cellcolor{", hardest_c, "}", Hardest),
    Easiest = paste0("\\cellcolor{", easiest_c, "}", Easiest),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, feedback_c = NULL, easiest_c = NULL, hardest_c = NULL, time_c = NULL,
    lost_c = NULL, fish_c = NULL, rate_acc_c = NULL, irritation_c = NULL, pacing_c = NULL, likedhelp_c = NULL, muchhelp_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  select(`Perc. Control` = Perc.f, `Frustration` = Frust.f, Hardest, Easiest, `Blink Recognition` = blink_recog, `Blink Conv. Rate` = rate_blink,
         `Pos. Feedback` = rate_feedback, `Help Feedback` = rate_help, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         `Fish Reel` = fishReel, `Fish Unreel` = fishUnreel, Duration = time_total) %>%
  select(-c(ends_with("_SD"))) %>%
  pivot_longer(cols=-c(Condition), names_to = "Variables") %>%
  pivot_wider(names_from = Condition, values_from = value)

# Add ICC Scores
Sc_table <- Sc_table %>% left_join(Sicc) %>% 
  mutate(ICC3 = ifelse(is.na(ICC3),"-",format(round(ICC3,2), nsmall = 2)))


paste(colnames(Sc_table), collapse=" & ")
writeLines(paste(Sc_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")

St_icc <- St %>% filter(Condition %in% c("AS","IO","MF")) %>% ungroup %>%
  select(Participant, HowMuchHelpNormalized, Condition) %>%
  pivot_wider(names_from = Participant, values_from = HowMuchHelpNormalized) %>%
  ungroup() %>% select(-Condition)

test <- psych::ICC(St_icc)

#St %>% filter(Condition %in% c("AS","IO","MF"))


D_icc <- St %>% ungroup %>%
  select(Participant, FrustNormalized, GameTitle, Condition) %>%
  pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
  ungroup() %>% select(-Condition, -GameTitle)

test <- psych::ICC(D_icc)



#############
# Latex Table: Condition-Wise
#############

Sc_table <- Sc_old %>% mutate(Condition = factor(Condition, levels=c("NO","AS","IO","MF"))) %>% arrange(Condition) %>%
  group_by(Condition) %>% select(Condition, mean_percEp, mean_frustEp, mean_feedback, mean_fishCaught, mean_fishLost) %>% 
  pivot_longer(cols=-c(Condition), names_to = "Variable") %>%
  mutate(value = format(round(value,2), nsmall = 2)) %>%
  pivot_wider(names_from = Condition, values_from = value) %>% 
  mutate(across(everything(), ~ str_replace_all(.x, c("mean_fishCaught" = "Fish Caught",
                                             "mean_fishLost" = "Fish Lost",
                                             "mean_fishUnreel" = "Fish Unreel",
                                             "mean_feedback" = "Pos. Feedback",
                                             "mean_pam_rate" = "PAM Rate",
                                             "mean_fishReel" = "Fish Reel",
                                             "mean_rate_blink" = "Blink Conv. Rate",
                                             "mean_bci_experience" = "BCI Experience",
                                             "mean_percEp" = "Perc. Control",
                                             "mean_frustEp" = "Frustration")))) %>%
  rename(` ` = Variable, `Input Overr.` = IO,`Aug. Success` = AS,`Normal (No help)` = NO,`Mit. Failure` = MF) %>% view()
paste(colnames(Sc_table), collapse=" & ")
message(paste(Sc_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\\ "))

#############
# Latex Table: Participants
#############
cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1,1.5,2.2),
             lv_frust = rev(lv_perc),
             lv_rate = c(-0.1, 0.35, 0.55, 0.85, 1.1, 1.5, 2.2),
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),
             lv_fish = c(0,2,4,6,10,12,14),
             lv_lost = c(8,4,2,1,0,-1,-2))

St_table <- St %>% group_by(Condition) %>% select(Participant, PercNormalized, FrustNormalized, blink_recog_trial, rate_feedback,
                                                  fishCaught, fishLost) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    rate_c = t_color(blink_recog_trial, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    blink_recog_trial = paste0(format(round(blink_recog_trial * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized),
    blink_recog_trial = paste0("\\cellcolor{", rate_c, "}", blink_recog_trial),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, rate_acc_c = NULL, feedback_c = NULL,
    lost_c = NULL, fish_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  rename(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `Blink Recognition` = blink_recog_trial,
         `Pos. Feedback` = rate_feedback, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         ) %>%
  pivot_longer(cols=-c(Participant, Condition), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)

St_table <- St_table %>% group_by(Condition) %>%
  group_modify(~ add_row(Variable=paste("\\underline{",.y,"}"),.before=0, .x)) %>%
  ungroup() %>% replace(is.na(.)," ") %>%
  select(-Condition)

paste(colnames(St_table), collapse=" & ")
writeLines(paste(St_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")

#############
# Cumulative Link Mixed Models: Updated
#############

### H1/2: No variables significantly predicted/affected participants' perceived control and frustration.
clmms = list(predictors = c("Frust.f","Perc.f"),
             random = c("bci_experience.f","Fatigue.f","Gender.f"),
             fixed = c("rate_feedback","blink_conv_rate", "Condition.f", "Gender.f", "Order.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost"),
             null = c("Participant.f"),
             threshold = 0.05, # increased to 0.1 to show models which were close to significant.
             df = St)

table = g_clmm_table(clmms)

glme_table <- table %>% 
  mutate(p = `$\\chi^2$`,
         `Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         `$\\chi^2$` = ifelse(p < 0.05, paste0(`$\\chi^2$`,"*")),
         across(everything(), ~ str_replace_all(.x, c("Order.f" = "Condition Order",
                                                      "Frust.f" = "Frustration",
                                                      "Perc.f" = "Perc. Control",
                                                      "Condition.f" = "Condition",
                                                      "Participant.f" = "Participant",
                                                      "fishCaught" = "Fish Caught",
                                                      "fishLost" = "Fish Lost",
                                                      "fishUnreel" = "Fish Unreel",
                                                      "rate_feedback" = "Pos. Feedback",
                                                      "pam_rate" = "PAM Rate",
                                                      "fishReel" = "Fish Reel",
                                                      "blink_conv_rate" = "Blink Conv. Rate",
                                                      "bci_experience" = "BCI Experience")))
  ) %>%
  select(Predicted, `Fixed Effect`, AIC, ML, LR, `$\\chi^2$`) %>%
  arrange(desc(Predicted),AIC)
message(paste(colnames(glme_table), collapse=" & "))
message(paste(glme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


# Perc.f Null model
model.null = clm(Perc.f ~ 1 + (1|Participant), data=St)

# Perc.f - Fish reel model
model.condition = clmm(Perc.f ~ 1 + (1|Participant) + fishReel, data=St)

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

# Perc.f Condition model
St$Condition.f = factor(St$Condition.f, levels=c("IO","AS","MF","NO"))
model.condition = clmm(Perc.f ~ 1 + (1|Participant) + Condition.f, data=St)
anova(model.condition, model.null)
modelcond.summary = summary(model.condition)
modelcond.summary

# Frust.f Null model
model.null = clm(Frust.f ~ 1 + (1|Participant), data=St)

# Frust.f - Fish reel model
model.condition = clmm(Frust.f ~ 1 + (1|Participant) + fishReel, data=St)

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

# Frust.f - Fish lost model
model.condition = clmm(Frust.f ~ 1 + (1|Participant) + fishLost, data=St)

anova(model.condition, model.null)

modelcond.summary = summary(model.condition)
modelcond.summary

# Frust.f Condition model
St$Condition.f = factor(St$Condition.f, levels=c("IO","AS","MF","NO"))
model.condition = clmm(Frust.f ~ 1 + (1|Participant) + Condition.f, data=St)
anova(model.condition, model.null)
modelcond.summary = summary(model.condition)
modelcond.summary



#############
# Cumulative Mixed Models: Old
#############


clmms = list(predictors = c("FrustNormalized.f", "PercNormalized.f"),
             random = c("bci_experience"),
             fixed = c("rate_blink","rate_feedback", "Condition.f", "Gender.f",
                       "pam_rate", "fishCaught","fishReel","fishUnreel", "fishLost"),
             null = c("Participant.f"),
             threshold = 0.05,
             df = St)

table = g_clmm_table(clmms)

glme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("fishCaught" = "Fish Caught",
                                                      "fishLost" = "Fish Lost",
                                                      "fishUnreel" = "Fish Unreel",
                                                      "rate_feedback" = "Pos. Feedback",
                                                      "pam_rate" = "PAM Rate",
                                                      "fishReel" = "Fish Reel",
                                                      "rate_blink" = "Blink Conv. Rate",
                                                      "bci_experience" = "BCI Experience",
                                                      "PercNormalized" = "Perceived Control",
                                                      "FrustNormalized" = "Frustration")))
  ) %>%
  select(Predicted, `Fixed Effect`, AIC, ML, LR, `$\\chi^2$`)
message(paste(colnames(lme_table), collapse=" & "))
message(paste(lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

St$Condition.f = factor(St$Condition.f, levels=c("IO","MF","AS","NO"))
model.null = clm(PercNormalized.f ~ 1 + (1|Participant), data=St)
model = clmm(PercNormalized.f ~ 1 + Condition.f + (1|Participant), data=St)

model = clmm(FrustNormalized.f ~ 1 + fishLost + (1|Participant), data=St)
model = clmm(FrustNormalized.f ~ 1 + fishCaught + (1|Participant), data=St)



anova(model, model.null)

#############
# Linear Mixed Models
#############
# Test:
# Some patients report fatigue. Check whether Order has influence.
# Most patients report Number of Fish. Check FishLost / FishCaught
# 

# Testing variables:
# Perc.Control, Frustration, Preference, Dispreference

lmes = list(predictors = c("PercNormalized", "FrustNormalized", "Hardest","Easiest"),
            random = c("Fatigue", "bci_experience"),
            fixed = c("rate_blink","rate_feedback","Gender", "Condition", 
                      "pam_rate", "fishCaught","fishLost","fishReel","fishUnreel"),
            null = c("Participant"),
            threshold = 0.05,
            df = St)

table = g_lme_table(lmes)

lme_table <- table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Random Intercept` = "Participant",
         `$\\chi^2$` = format(round(`$\\chi^2$`,3), nsmall = 3),
         `$\\chi^2$` = ifelse(`$\\chi^2$` == "0.000", "$<$0.001", `$\\chi^2$`),
         across(everything(), ~ str_replace_all(.x, c("fishCaught" = "Fish Caught",
                                                      "fishLost" = "Fish Lost",
                                                      "fishUnreel" = "Fish Unreel",
                                                      "rate_feedback" = "Pos. Feedback",
                                                      "pam_rate" = "PAM Rate",
                                                      "fishReel" = "Fish Reel",
                                                      "rate_blink" = "Blink Conv. Rate",
                                                      "bci_experience" = "BCI Experience",
                                                      "PercNormalized" = "Perceived Control",
                                                      "FrustNormalized" = "Frustration")))
  ) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)
message(paste(colnames(lme_table), collapse=" & "))
message(paste(lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

#############
# Violin Plots of Main Measurements
#############
fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
            y=~n_clip(jitter(PercNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
#  add_trace(data=St %>% ungroup() %>% arrange(Participant,Condition),x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")),
            #y=~n_clip(jitter(PercNormalized,amount=.02)), type='scatter',mode='lines') %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“I felt I was in control of the \n fisherman reeling in the fish.”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ",  dtick=0.167, tickformat = ".2", tickfont=list(size=15), zeroline=F))
fig_c
orca(fig_c, "fig/S1_condition_percNormalized_violin.pdf", width=320, height=370)


fig %>% add_trace(data=St %>% arrange(Participant,Condition),x=~factor(ConditionLabel, levels=c("Aug.\n Success","Overr.\n Input","Mit.\n Failure","Ref.")),
                  y=~n_clip(jitter(PercNormalized,amount=.02)), name=~Participant, hovertext=~Participant, type='scatter',mode='markers+lines')

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
            y=~n_clip(jitter(FrustNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),title=list(font=list(size=15),xanchor="center",xref="paper",
                                                  text="“How much frustration did you \n feel in this condition?”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ", zeroline=F, tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", zeroline=F, dtick=0.167, tickformat = ".2", tickfont=list(size=15), showticklabels=T))
fig_c
orca(fig_c, "fig/S1_condition_frustNormalized_violin.pdf", width=320, height=370)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input")),
            y=~n_clip(jitter(HowMuchHelpNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4,color=I('black')),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“How much did you feel \n the game helped you?”"),showlegend=F,
         xaxis=list(range=c(-0.45,2.55), title=" ", tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", tickfont=list(size=15), zeroline=F, showticklabels=F))
fig_c
orca(fig_c, "fig/condition_howMuchHelp_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input")),
            y=~n_clip(jitter(LikedHelpNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="\n“I liked how the game helped me.”"), showlegend=F,
         xaxis=list(range=c(-0.45,2.55), title=" ", tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ",tickfont=list(size=15), zeroline=F, showticklabels=FALSE))
fig_c
orca(fig_c, "fig/condition_LikedHelp_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")),
            y=~n_clip(jitter(PacingNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“I felt the pacing of the game was”\n (slow/fast)"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", tickfont=list(size=15), zeroline=F,showticklabels=F))
fig_c
orca(fig_c, "fig/condition_pacingNormalized_violin.pdf", width=275, height=325)

fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), y=~n_clip(jitter(IrritationNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.45, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“How irritated did you feel\n in this condition?”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", tickfont=list(size=15), zeroline=F,showticklabels=FALSE))
fig_c
orca(fig_c, "fig/condition_irritationNormalized_violin.pdf", width=275, height=325)





#############
# Perceived Control to Pos. Feedback
#############
fig_c <- fig %>%
  add_trace(data=St, x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")), 
            y=~n_clip(jitter(PercNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.65, meanline=list(visible=T,width=4),
            symbol=I('o'),marker=list(size=10,line=list(width=1.5)),
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.09, color=I('darkgray')) %>%
  #  add_trace(data=St %>% ungroup() %>% arrange(Participant,Condition),x=~factor(ConditionLabel, levels=c("Aug.\n Success","Mit.\n Failure","Overr.\n Input","Ref.")),
  #y=~n_clip(jitter(PercNormalized,amount=.02)), type='scatter',mode='lines') %>%
  layout(margin=list(l=0,r=0,t=55,b=0), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text="“I felt I was in control of the \n fisherman reeling in the fish.”"), showlegend=F,
         xaxis=list(range=c(-0.45,3.55), title=" ",tickfont=list(size=15)),
         yaxis=list(range=c(-0.02,1.02), title=" ", tickfont=list(size=15), zeroline=F))
fig_c
orca(fig_c, "fig/S1_condition_percNormalized_violin.pdf", width=275, height=325)


fig_p <- fig %>%
  add_trace(data=St, x=n_clip(jitter(St$rate_feedback,amount=.05)), y=n_clip(jitter(St$PercNormalized,amount=.02)), color=I('white'), opacity=.4,
            type='scattergl', mode='markers', marker=list(size=10,line=list(width=1.5, 
            color=I('black'))), symbol=~Condition, hoverinfo="text", 
            text=paste(St$Participant, St$Condition),
            symbols=list('circle', 'triangle-up', 'square', 'x')) %>%
  layout(margin=list(l=50,r=0,t=0,b=10), title=list(font=list(size=15),xanchor="center",xref="paper",
                                                   text=" "),
         xaxis=list(range=c(-0.05,1.05), title="Positive Feedback",tickfont=list(size=15), zeroline=F),
         yaxis=list(range=c(-0.05,1.05), title="Perceived Control", tickfont=list(size=15), zeroline=F),
         legend=list(x=0.80,y=0.05))
fig_p


#############
# Level of Control to Perceived Control
#############

PercLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"PercNormalized", "trial_rate_positive"),
                 "AS" = p_lin(St %>% filter(Condition == "AS"),"PercNormalized", "trial_rate_positive"),
                 "MF" = p_lin(St %>% filter(Condition == "MF"),"PercNormalized", "trial_rate_positive"),
                 "IO" = p_lin(St %>% filter(Condition == "IO"),"PercNormalized", "trial_rate_positive"))

FrustLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"FrustNormalized", "trial_rate_positive"),
                  "AS" = p_lin(St %>% filter(Condition == "AS"), "FrustNormalized", "trial_rate_positive"),
                  "MF" = p_lin(St %>% filter(Condition == "MF"),"FrustNormalized", "trial_rate_positive"),
                  "IO" = p_lin(St %>% filter(Condition == "IO"),"FrustNormalized", "trial_rate_positive"))

CombLine <- list("NO" = p_lin(St %>% filter(Condition == "NO"),"FrustNormalized", "PercNormalized"),
                 "AS" = p_lin(St %>% filter(Condition == "AS"), "FrustNormalized", "PercNormalized"),
                 "MF" = p_lin(St %>% filter(Condition == "MF"),"FrustNormalized", "PercNormalized"),
                 "IO" = p_lin(St %>% filter(Condition == "IO"),"FrustNormalized", "PercNormalized"))

# Perceived Control to Condition

fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=PercLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(PercNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=PercLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/patients_perc_control_pos_feedback.pdf", width=1150, height=350)


fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=FrustLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=FrustLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~trial_rate_positive, y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=FrustLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Frustration"), xaxis=list(title="Positive Feedback"))
fig_c
orca(fig_c, "fig/patients_frust_control_pos_feedback.pdf", width=1150, height=350)


fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=CombLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=CombLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    add_trace(data=CombLine[[cond]], x=~x, y=~y, color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Frustration', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Frustration"), xaxis=list(title="Perceived Control"))
fig_c
orca(fig_c, "fig/patients_frust_perc_control.pdf", width=1150, height=350)

figf <- fig %>%
  add_trace(name= "NO", data = St %>% filter(Condition == "NO"), x=~trial_rate_positive, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("black"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["Pure"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "AS", data = St %>% filter(Condition == "AS"), x=~trial_rate_positive, text=S_vis$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("blue"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["AS"]], x=~x, y=~y, color=I('blue'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "MF", data = St %>% filter(Condition == "MF"), x=~trial_rate_positive, text=S_vis$Condition, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("red"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["MF"]], x=~x, y=~y, color=I('red'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(name= "IO", data = St %>% filter(Condition == "IO"), x=~trial_rate_positive, text=S_vis$Condition,  y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("orange"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["IO"]], x=~x, y=~y, color=I('orange'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Control"))
figf


fig_p <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$PercNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  add_trace(data=PercLine[["Pure"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))

fig_f <- fig %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, color=S_vis$Condition,
            type='scattergl', mode='markers', marker=list(size=20)) %>%
  add_trace(x=S_vis$trial_rate_accept, y=S_vis$FrustNormalized, text=S_vis$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,1.1), title="Control"),
         yaxis=list(range=c(-0.1,1.1), title="Frustration"))

fig_p
fig_f


vis <- subplot(fig_f, fig_p) %>% layout(showlegend=FALSE)
vis


#############
# Visualize Lost Fish / Caught Fish
#############

fig_p <- fig %>%
  add_trace(x=jitter(St$fishCaught,amount=.05), y=jitter(St$PercNormalized,amount=.02), color=St$Condition, opacity=.6,
            type='scattergl', mode='markers+text', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  layout(xaxis=list(range=c(-0.1,11)),
         yaxis=list(range=c(-0.1,1.1)))
fig_p

fig_p <- fig %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, color=St$Condition,
            type='scattergl', mode='markers', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  add_trace(x=St$fishCaught, y=St$PercNormalized, text=St$Condition,
            type='scattergl', mode='text') %>%
  layout(xaxis=list(range=c(-0.1,11), title="Fish Caught"),
         yaxis=list(range=c(-0.1,1.1), title="Perc.Control"))
fig_p