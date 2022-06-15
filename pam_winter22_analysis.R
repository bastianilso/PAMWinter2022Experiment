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
            Fatigue = unique(Fatigue),
            bci_experience = unique(BCIExp),
            Order = unique(Order),
            Hardest = unique(Hardest),
            Easiest = unique(Easiest),
            accRecogRate = posTrial / sum(TrialGoal == "OverrideInput" | TrialGoal == "AccInput" | TrialGoal == "AugSuccess" | TrialGoal == "ExplicitSham" | TrialGoal == "AssistSuccess", na.rm=T))

# Blink counts
St <- D %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
  ) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(Participant, Condition, InputWindowOrderFilledSoft) %>%
  summarize(blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilledSoft > -1) %>% ungroup() %>% group_by(Participant, Condition) %>%
  summarize(blink_recog_trial = sum(blink_recog_window > 0),
            blink_recog_window = sum(blink_recog_window),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)

# Si = Summary of Input Windows
# Feedback Delay
# Fabricated delay
Si <- D %>% ungroup() %>% group_by(Participant, Condition, InputWindowOrderFilled) %>%
  filter(Event == "EyeOpening") %>%
  summarize(last_attempt_stamp = max(Timestamp)) %>% ungroup()

Si <- D %>% ungroup() %>% group_by(Participant, Condition, InputWindowOrderWithDecision) %>%
  filter(Event == "GameDecision") %>%
  summarize(feedback_stamp = max(Timestamp)) %>%
  mutate(InputWindowOrderFilled = InputWindowOrderWithDecision) %>% ungroup() %>% right_join(Si)

Si <- Si %>% filter(!InputWindowOrderFilled == -1) %>%
  mutate(feedback_delay = feedback_stamp - last_attempt_stamp,
         feedback_delay = as.numeric(feedback_delay))

fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=Si, type='scatter')

St <- Si %>% group_by(Participant, Condition) %>%
  summarize(mean_delay = mean(feedback_delay)) %>% right_join(St) %>%
  mutate(mean_delay = ifelse(is.na(mean_delay), 0, mean_delay))


St <- St %>% group_by(Participant, Condition) %>%
  summarize(rate_blink = posTrial / blinks_total) %>%
  mutate(rate_blink = ifelse(rate_blink == Inf, 0, rate_blink)) %>% right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_feedback = posTrial/ totalTrials,
         session = 1,
         session = cumsum(session))

Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(Gender = unique(Gender),
            Age = unique(Age),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_feedback = mean(rate_feedback),
            mean_rate_blink = mean(rate_blink),
            mean_blink_recog = mean(accRecogRate)
            )
save(St, file = 'pamSurrogate.rda', compress=TRUE)

#############
# Latex Table: Participants
#############
cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1,1.5,2.2),
             lv_frust = rev(lv_perc),
             lv_rate = c(-0.1, 0.35, 0.55, 0.85, 1.1, 1.5, 2.2),
             colors = c("g0","g1", "g2", "g3", "g4","g4","g4"),
             lv_fish = c(0,2,4,6,10,12,14),
             lv_lost = c(8,4,2,1,0,-1,-2))

St_table <- St %>% group_by(Condition) %>% select(Participant, PercNormalized, FrustNormalized, rate_blink, accRecogRate, rate_feedback,
                                                  fishCaught, fishLost) %>%
  mutate(
    Condition = ifelse (Condition == "MF", "Mit. Failure", Condition),
    Condition = ifelse (Condition == "NO", "Ref. Condition", Condition),
    Condition = ifelse (Condition == "AS", "Aug. Success", Condition),
    Condition = ifelse (Condition == "IO", "Input Override", Condition),
    perc_c = t_color(PercNormalized, cri$lv_perc, cri$colors),
    frust_c = t_color(FrustNormalized, cri$lv_frust, cri$colors),
    rate_c = t_color(rate_blink, cri$lv_rate, cri$colors),
    rate_acc_c = t_color(accRecogRate, cri$lv_rate, cri$colors),
    feedback_c = t_color(rate_feedback, cri$lv_rate, cri$colors),
    fish_c = t_color(fishCaught, cri$lv_fish, cri$colors),
    lost_c = t_color(fishLost, cri$lv_lost, cri$colors),
    PercNormalized = format(round(PercNormalized,2), nsmall = 2),
    FrustNormalized = format(round(FrustNormalized,2), nsmall = 2),
    rate_blink = paste0(format(round(rate_blink * 100,0), nsmall = 0),"\\%"),
    rate_feedback = paste0(format(round(rate_feedback * 100, 0), nsmall = 0), "\\%"),
    accRecogRate =  paste0(format(round(accRecogRate * 100,0), nsmall = 0),"\\%"),
    PercNormalized = paste0("\\cellcolor{", perc_c, "}", PercNormalized),
    FrustNormalized = paste0("\\cellcolor{", frust_c, "}", FrustNormalized),
    rate_blink = paste0("\\cellcolor{", rate_c, "}", rate_blink),
    accRecogRate = paste0("\\cellcolor{", rate_acc_c, "}", accRecogRate),
    rate_feedback = paste0("\\cellcolor{", feedback_c, "}", rate_feedback),
    fishCaught = paste0("\\cellcolor{", fish_c, "}", fishCaught),
    fishLost = paste0("\\cellcolor{", lost_c, "}", fishLost),
    perc_c = NULL, frust_c = NULL, rate_c = NULL, rate_acc_c = NULL, feedback_c = NULL,
    lost_c = NULL, fish_c = NULL,
    across(everything(), as.character)) %>% arrange(Condition) %>%
  rename(`Perc. Control` = PercNormalized, `Frustration` = FrustNormalized, `Blink Conv. Rate` = rate_blink,
         `Pos. Feedback` = rate_feedback, `Fish Caught` = fishCaught, `Fish Lost` = fishLost,
         `Blink Recognition` = accRecogRate,) %>%
  pivot_longer(cols=-c(Participant, Condition), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)

St_table <- St_table %>% group_by(Condition) %>%
  group_modify(~ add_row(Variable=paste("\\underline{",.y,"}"),.before=0, .x)) %>%
  ungroup() %>% replace(is.na(.)," ") %>%
  select(-Condition)

paste(colnames(St_table), collapse=" & ")
writeLines(paste(St_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "), "table.txt")




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
# Perceived Control to Real level of control
#############

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