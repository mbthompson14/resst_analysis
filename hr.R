library(dplyr)
library(ggplot2)
library(hausekeep)
library(rstatix)
source('utils.R')
`%!in%` = Negate(`%in%`)


hr = read.csv("heartrate/HRdata.csv") %>%
  as_tibble()

hr = hr %>% filter(!is.na(BPM_average)) %>% filter(Beats_valid_percent>=90) %>% filter(Activity_recorded_percent>=90)

hr = hr %>%
  filter(Activity=="Q2" | 
           Activity=="Q3" |
           Activity=="state_prepmainmath" |
           Activity=="Q4" |
           Activity=="Q5" |
           Activity=="reminder_1" |
           Activity=="Q6" |
           Activity=="Q7" |
           Activity=="reminder_2" |
           Activity=="Q8") %>%
  mutate(Activity = recode(Activity,
                           "Q2"="1",
                           "Q3"="2",
                           "state_prepmainmath"="3",
                           "Q4"="4",
                           "Q5"="5",
                           "reminder_1"="6",
                           "Q6"="7",
                           "Q7"="8",
                           "reminder_2"="9",
                           "Q8"="a")) %>%
  mutate(task_order = ifelse(Sequence=="A",0,
                             ifelse(Sequence=="B",0,
                                    ifelse(Sequence=="C",1,
                                           ifelse(Sequence=="D",1,
                                                  NA))))) %>%
  mutate(rep = ifelse(Condition=="TSST","regular",
                      ifelse(Condition=="Color","regular",
                             ifelse(Condition=="Singing","musical",
                                    ifelse(Condition=="Music","musical",
                                           NA)))))

#   mutate(Activity = recode(Activity,
#                            "Q2"="Baseline 1",
#                            "Q3"="Baseline 2",
#                            "state_prepmainmath"="State Induction",
#                            "Q4"="Post-induction",
#                            "Q5"="Pre-reminder 1",
#                            "reminder_1"="Reminder 1",
#                            "Q6"="Post-reminder 1",
#                            "Q7"="Pre-reminder 2",
#                            "reminder_2"="Reminder 2",
#                            "Q8"="Post-reminder 2"))

# hr$Activity = factor(hr$Activity, levels = c("Baseline 1",
#                                              "Baseline 2",
#                                              "State Induction",
#                                              "Post-induction",
#                                              "Pre-reminder 1",
#                                              "Reminder 1",
#                                              "Post-reminder 1",
#                                              "Pre-reminder 2",
#                                              "Reminder 2",
#                                              "Post-reminder 2"))

# hr$Activity = factor(hr$Activity, levels = c("Q2",
#                                              "Q3",
#                                              "state_prepmainmath",
#                                              "Q4",
#                                              "Q5",
#                                              "reminder_1",
#                                              "Q6",
#                                              "Q7",
#                                              "reminder_2",
#                                              "Q8"))

hr_temp = hr

hr$Participant = as.factor(hr$Participant)
hr$Session = as.factor(hr$Session)
hr$Gender = as.factor(hr$Gender)
hr$Sequence = as.factor(hr$Sequence)
hr$Drug = as.factor(hr$Drug)
hr$Condition = as.factor(hr$Condition)
hr$Stress = as.factor(hr$Stress)
hr$Status = as.factor(hr$Status)
hr$Activity = as.factor(hr$Activity)
hr$task_order = as.factor(hr$task_order)
hr$rep = as.factor(hr$rep)




hr_placebo = hr %>%
  filter(Drug == "Placebo")

hr_bsln_cor = hr_placebo %>%
  ddply(.(Participant, Session), transform, bsln_corr = BPM_average - BPM_average[2]) %>%
  filter(Participant %!in% c('E045','E064'))


hr_bsln_cor %>%
  filter(ifelse(Activity == 2 & bsln_corr !=0,T,F))

# By stress

hr_summary = summarySEwithin2(data=hr_placebo, measurevar = "BPM_average",
                      withinvars = c("Stress", "Activity"),
                      idvar = "Participant", na.rm = T) %>%
  mutate(lower = BPM_average-ci, upper = BPM_average+ci)


hr_summary %>%
  ggplot(aes(x = Activity, y = BPM_average, color = Stress, group = Stress)) +
  geom_line() +
  geom_ribbon(aes(x=Activity, y=BPM_average, ymin=lower, ymax=upper, color = Stress, fill = Stress), alpha = 0.2) +
  scale_color_manual(name ="", labels=c("Stress" = "Stress", "Non-stress" = "Control"), values=c("Stress"="#F8766D", "Non-stress"="#00BFC4"))+
  scale_fill_manual(name ="", labels=c("Stress" = "Stress", "Non-stress" = "Control"), values=c("Stress"="#F8766D", "Non-stress"="#00BFC4"))+
  theme_bw()+
  labs(title = "Heart Rate", x = "", y = "Average BPM") +
  scale_x_discrete(labels = c("Baseline 1", "Baseline 2", "State Induction", "Post-induction",
                              "Pre-reminder 1", "Reminder 1", "Post-reminder 1", "Pre-reminder 2",
                              "Reminder 2", "Post-reminder 2")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"), plot.title = element_text(size=16, face = "bold"),
        legend.title=element_text(size=10), legend.text=element_text(size=9))+
  theme(plot.title=element_text(hjust=0),
        legend.position="top",
        legend.justification="left")


# By condition

hr_summary_2 = summarySEwithin2(data=hr_placebo, measurevar = "BPM_average",
                              withinvars = c("Condition", "Activity"),
                              idvar = "Participant", na.rm = T) %>%
  mutate(lower = BPM_average-ci, upper = BPM_average+ci)


hr_summary_2 %>%
  ggplot(aes(x = Activity, y = BPM_average, color = Condition, group = Condition)) +
  geom_line() +
  geom_ribbon(aes(x=Activity, y=BPM_average, ymin=lower, ymax=upper, color = Condition, fill = Condition), alpha = 0.2) +
  scale_color_manual(name ="", labels=c("TSST" = "Speech", "Singing" = "Singing", "Color" = "Color Control", "Music" = "Music Control"), 
                     values=c("TSST"="#F8766D", "Singing" = "#C77CFF", "Color"="#00BFC4", "Music" = "#7CAE00"))+
  scale_fill_manual(name ="", labels=c("TSST" = "Speech", "Singing" = "Singing", "Color" = "Color Control", "Music" = "Music Control"),
                    values=c("TSST"="#F8766D", "Singing" = "#C77CFF", "Color"="#00BFC4", "Music" = "#7CAE00"))+
  theme_bw()+
  labs(title = "Heart Rate", x = "", y = "Average BPM") +
  scale_x_discrete(labels = c("Baseline 1", "Baseline 2", "State Induction", "Post-induction",
                              "Pre-reminder 1", "Reminder 1", "Post-reminder 1", "Pre-reminder 2",
                              "Reminder 2", "Post-reminder 2")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"), plot.title = element_text(size=16, face = "bold"),
        legend.title=element_text(size=10), legend.text=element_text(size=9))+
  theme(plot.title=element_text(hjust=0),
        legend.position="top",
        legend.justification="left")

# by condition, basline corrected

hr_summary_2 = summarySEwithin2(data=hr_bsln_cor, measurevar = "bsln_corr",
                                withinvars = c("Condition", "Activity"),
                                idvar = "Participant", na.rm = T) %>%
  mutate(lower = bsln_corr-ci, upper = bsln_corr+ci)


hr_summary_2 %>%
  ggplot(aes(x = Activity, y = bsln_corr, color = Condition, group = Condition)) +
  geom_line() +
  geom_ribbon(aes(x=Activity, y=bsln_corr, ymin=lower, ymax=upper, color = Condition, fill = Condition), alpha = 0.2) +
  scale_color_manual(name ="", labels=c("TSST" = "Speech", "Singing" = "Singing", "Color" = "Color Control", "Music" = "Music Control"), 
                     values=c("TSST"="#F8766D", "Singing" = "#C77CFF", "Color"="#00BFC4", "Music" = "#7CAE00"))+
  scale_fill_manual(name ="", labels=c("TSST" = "Speech", "Singing" = "Singing", "Color" = "Color Control", "Music" = "Music Control"),
                    values=c("TSST"="#F8766D", "Singing" = "#C77CFF", "Color"="#00BFC4", "Music" = "#7CAE00"))+
  theme_bw()+
  labs(title = "Baseline-corrected Heart Rate", x = "", y = "Average BPM\n(baseline-corrected)") +
  scale_x_discrete(labels = c("Baseline 1", "Baseline 2", "State Induction", "Post-induction",
                              "Pre-reminder 1", "Reminder 1", "Post-reminder 1", "Pre-reminder 2",
                              "Reminder 2", "Post-reminder 2")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"), plot.title = element_text(size=16, face = "bold"),
        legend.title=element_text(size=10), legend.text=element_text(size=9))+
  theme(plot.title=element_text(hjust=0),
        legend.position="top",
        legend.justification="left") +
  coord_cartesian(ylim = c(-10, 30))


# Model 1

hr_model_1 = hr %>%
  filter(Activity %in% c(2,3,4), Participant %!in% c("E019","E029"))

hr_model_1 %>%
  anova_test(dv = BPM_average, wid = Participant, within = c(Condition, Activity), between = task_order) %>%
  get_anova_table()


# outliers = hr_model_1 %>%
#   identify_outliers(BPM_average) %>%
#   filter(is.extreme == T)
# 
# wo_outliers = anti_join(hr_model_1, outliers, by = c('Participant', 'Session', 'Activity'))
# 
# wo_outliers %>%
#   anova_test(dv = BPM_average, wid = Participant, within = c(Condition, Activity), between = task_order) %>%
#   get_anova_table()
# 
# one.way <- wo_outliers %>%
#   group_by(Activity) %>%
#   anova_test(dv = BPM_average, wid = Participant, within = c(Condition)) %>%
#   get_anova_table() %>%
#   adjust_pvalue(method = "hochberg")
# one.way

one.way <- hr_model_1 %>%
  group_by(Activity) %>%
  anova_test(dv = BPM_average, wid = Participant, within = c(Condition)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "hochberg")
one.way

pwc <- hr_model_1 %>%
  group_by(Activity) %>%
  pairwise_t_test(
    BPM_average ~ Condition, paired = F,
    p.adjust.method = "hochberg"
  )
pwc %>%
  filter(Activity %in% c(3,4))

#get t stat and df

hr_m1_tsst = hr_model_1 %>%
  filter(Activity == 3)
result = pairwise.t.test.with.t.and.df(hr_m1_tsst$BPM_average, hr_m1_tsst$Condition, p.adjust.method = "hochberg")
result
result[[5]] #t stat
result[[6]] #df

hr_m1_q4 = hr_model_1 %>%
  filter(Activity == 4)
result = pairwise.t.test.with.t.and.df(hr_m1_q4$BPM_average, hr_m1_q4$Condition, p.adjust.method = "hochberg")
result
result[[5]] #t stat
result[[6]] #df

# Model 2

hr_model_2 = hr_placebo %>%
  filter(Activity %in% c(5,6,7,8,9,'a'), Participant %!in% c("E019","E029"))

hr_model_2 %>%
  anova_test(dv = BPM_average, wid = Participant, within = c(Stress, Activity), between = c(task_order,rep)) %>%
  get_anova_table()

# outliers = hr_model_2 %>%
#   identify_outliers(BPM_average) %>%
#   filter(is.extreme == T)
# 
# wo_outliers = anti_join(hr_model_2, outliers, by = c('Participant', 'Session', 'Activity'))
# 
# wo_outliers %>%
#   anova_test(dv = BPM_average, wid = Participant, within = c(Stress, Activity), between = c(task_order,rep)) %>%
#   get_anova_table()

# two.way <- hr_model_2 %>%
#   group_by(rep) %>%
#   anova_test(dv = BPM_average, wid = Participant, within = c(Stress,Activity)) %>%
#   get_anova_table() %>%
#   adjust_pvalue(method = "hochberg")
# two.way
# 
# one.way <- hr_model_2 %>%
#   group_by(rep,Activity) %>%
#   anova_test(dv = BPM_average, wid = Participant, within = c(Stress)) %>%
#   get_anova_table() %>%
#   adjust_pvalue(method = "hochberg")
# one.way

pwc <- hr_model_2 %>%
  pairwise_t_test(
    BPM_average ~ Stress, paired = F,
    p.adjust.method = "hochberg"
  )
pwc

pwc <- hr_model_2 %>%
  pairwise_t_test(
    BPM_average ~ Activity, paired = F,
    p.adjust.method = "hochberg"
  )
pwc

result = pairwise.t.test.with.t.and.df(hr_model_2$BPM_average, hr_model_2$Stress, p.adjust.method = "hochberg")
result
result[[5]] #t stat
result[[6]] #df

hr_model_1 %>%
  summarySEwithin2(measurevar = 'BPM_average', withinvars = c('Condition','Activity'),
                   idvar = 'Participant') %>%
  filter(Activity == 3)

hr_model_2 %>%
  summarySEwithin2(measurevar = 'BPM_average', withinvars = c('Stress'),
                   idvar = 'Participant')




# diagnostics

check_outliers = hr_model_1 %>%
  group_by(Participant, Condition, Activity) %>%
  identify_outliers(BPM_average)

model  <- lm(BPM_average ~ Condition*Activity, data = hr_model_1)

emmeans(model, specs = ~ Activity | Condition)

# Create a QQ plot of residuals
ggqqplot(residuals(model)) +
  labs(title = "Q-Q plot - Model 1 - Average BPM")

model %>%
  ggplot(aes(x=fitted(model), y=resid(model))) +
  geom_point() +
  geom_hline(yintercept = 0)+
  labs(title = "Residuals - Model 1 - Average BPM", x = "Fitted", y = "Residuals") +
  theme_bw()


model  <- lm(BPM_average ~ Stress*Activity, data = hr_model_2)

emmeans(model, specs = ~ Stress)

# Create a QQ plot of residuals
ggqqplot(residuals(model)) +
  labs(title = "Q-Q plot - Model 2 - Average BPM")

model %>%
  ggplot(aes(x=fitted(model), y=resid(model))) +
  geom_point() +
  geom_hline(yintercept = 0)+
  labs(title = "Residuals - Model 2 - Average BPM", x = "Fitted", y = "Residuals") +
  theme_bw()



# model = aov(BPM_average ~ Condition*Activity*task_order+Error(Participant/(Condition*Activity)), data = hr)
# 
# model.pr = proj(model)
# hr_placebo$resi = model.pr[[3]][, "Residuals"]
# hr_placebo$fit = fitted.aovlist(model)
# 
# ggqqplot(hr_placebo$resi) +
#   labs(title = "Q-Q plot - Model 1 - Average BPM")
# 
# hr_model_1 %>%
#   ggplot(aes(x=fit, y=resi)) +
#   geom_point() +
#   geom_hline(yintercept = 0)+
#   labs(title = "Residuals - Model 1 - Average BPM", x = "Fitted", y = "Residuals") +
#   theme_bw()




