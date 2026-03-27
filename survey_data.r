## Read in survey data from excel files

#install.packages("tidyverse")
#install.packages("GGAlly")
#packageVersion("ggplot2")  # should show 4.0.0 or 4.0.1

library(readxl)
library(tidyverse)
library(GGally)
# Read in the survey data

pre_survey <- read_excel("./Data/Survey Data/Air Quality Survey Sheet.xlsx", 
                sheet = "Final Pre Intervention")

post_survey <- read_excel("./Data/Survey Data/Air Quality Survey Sheet.xlsx", 
                sheet = "Final Post Intervention")

## convert all dbl columns to integer in pre_survey

names_survey <- names(pre_survey)

pre_survey <- pre_survey |> 
  mutate(across(where(is.double), as.integer)) |> 
  mutate(across(all_of(names_survey), as.character)) |>
  filter(!is.na(`HOUSE  #`)) |> # remove any rows where Q1 is NA (these are incomplete surveys)
  mutate(Survey = 'Pre_Intervention')

post_survey <- post_survey |> 
  mutate(across(where(is.double), as.integer)) |>
  mutate(across(all_of(names_survey), as.character)) |> 
  filter(!is.na(`HOUSE  #`)) |> # remove any rows where Q1 is NA (these are incomplete surveys)
  mutate(Survey = 'Post_Intervention')

survey <- bind_rows(pre_survey, post_survey) |> 
          rename(HouseID = `HOUSE  #`) |> 
          pivot_longer(cols = starts_with("Q"), 
                       names_to = "QuestionID", 
                       values_to = "Response")  

survey_numbers <- survey |>
          filter(!str_detect(QuestionID, "Other")) |>
          mutate(QuestionID = parse_number(QuestionID)) |> ## grab the number from QuestionID
          mutate(QuestionID = paste0("Q",str_pad(as.integer(QuestionID),width=2,side="left",pad="0")))

survey_others <- survey |>
          filter(str_detect(QuestionID, "Other")) |>
          mutate(QuestionID = str_replace(QuestionID," ","_")) ## replace space with underscore

survey <- bind_rows(survey_numbers, survey_others) |>
           arrange(Survey, HouseID, QuestionID)

## Read in answer key
## Note I deleted some of the empty columns from the Data dictionary sheet before importing the data
## I also added a column name A8 to column K

answer_key <- read_excel("./Data/Survey Data/Air Quality Survey Sheet.xlsx", 
                         sheet = "Data Dictionary") |> 
  rename(QuestionID = "Question #")  |> 
  mutate(QuestionID = paste0("Q",str_pad(as.integer(QuestionID),width=2,side="left",pad="0"))) 

names(answer_key)

unique(answer_key$Construct)
## Add pollutant column

answer_key <- answer_key |> 
  mutate(Pollutant = case_when(
    str_detect(Construct, "PM2.5") ~ "PM2.5",
    str_detect(Construct, "O3") ~ "O3",
    TRUE ~ "Both")) |> 
  mutate(Construct_general = case_when(
    str_detect(Construct, "Knowledge") ~ "Knowledge",
    str_detect(Construct, "Perceived Susceptibility") ~ "Perceived Susceptibility",
    str_detect(Construct, "Perceived Severity") ~ "Perceived Severity",
    str_detect(Construct, "Behavior") ~ "Behavior",
    str_detect(Construct, "Demographic") ~ "Demographic",
    TRUE ~ "Other"
  ))

## Join the survey data with the answer key to get question text and answer choices

## Knowledge questions
Q_K <- c("Q01","Q02","Q03","Q04","Q05","Q06","Q07","Q08") 

survey_grade <- survey |>
  left_join(answer_key, by = "QuestionID") |>
  filter(QuestionID %in% Q_K)|>
  rename(Correct_A = `Correct A`) |> 
  mutate(Correct_responses = str_count(Correct_A, "," ) + 1) |>
  mutate(Response_1 = str_split_fixed(Response, ",", 5)[,1]) |>
  mutate(Response_2 = str_split_fixed(Response, ",", 5)[,2]) |>
  mutate(Response_3 = str_split_fixed(Response, ",", 5)[,3]) |>
  mutate(Response_4 = str_split_fixed(Response, ",", 5)[,4]) |>
  mutate(Response_5 = str_split_fixed(Response, ",", 5)[,5]) |> 
  mutate(Grade_1 = ifelse(str_detect(Correct_A, Response_1), 1,-1)) |>
  mutate(Grade_2 = ifelse(str_detect(Correct_A, Response_2),1,-1)) |>
  mutate(Grade_3 = ifelse(str_detect(Correct_A, Response_3),1,-1)) |>
  mutate(Grade_4 = ifelse(str_detect(Correct_A, Response_4),1,-1)) |>
  mutate(Grade_5 = ifelse(str_detect(Correct_A, Response_5),1,-1)) |>
  mutate(Grade = rowSums(across(starts_with("Grade_"))/Correct_responses,na.rm=TRUE)) |> ## give points for correct answers, subtract points for incorrect answers, 
## then normalize by the number of correct responses
  mutate(Grade = ifelse(QuestionID ==1 & Response =="5",0,Grade)) |> ## special case for Q1 where "5" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==2 & Response =="7",0,Grade)) |> ## special case for Q1 where "7" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==3 & Response =="5",0,Grade)) |> ## special case for Q1 where "5" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==4 & Response =="6",0,Grade)) |> ## special case for Q1 where "6" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==5 & Response =="5",0,Grade)) |> ## special case for Q1 where "5" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==6 & Response =="7",0,Grade)) |> ## special case for Q1 where "7" means "I don't know", which should be graded as 0
  mutate(Grade = ifelse(QuestionID ==7 & Response =="5",0,Grade)) |> ## special case for Q1 where "5" means "I don't know", which should be graded as 0
  mutate(Grade_Final = case_when(
  Grade == 1 ~ 'Correct',
  Grade > 0 & Grade < 1 ~ 'Partially Correct',
  TRUE ~ 'Incorrect'))

names(survey_grade)  

## bind with other survey data
## What are the numerical questions?
Q_SS <- c("Q09","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20",
          "Q21","Q22","Q23","Q24","Q25","Q29")

survey_scores <- survey |> 
              filter(!(QuestionID %in% Q_K)) |> ## non-knowledge questions
              left_join(answer_key, by = "QuestionID") |>
              bind_rows(survey_grade) |> 
              mutate(Score = case_when(
                QuestionID %in% Q_K ~ as.numeric(Grade),  ## for knowledge questions, use the grade as the score
                QuestionID %in% Q_SS ~ as.numeric(Response), ## for SS questions, use the response as the score
                TRUE ~ NA)) |> ## for the other questions leave it as blank
              mutate(Score_mod = case_when(
                  QuestionID %in% c("Q09","Q10","Q17","Q18") ~ 6 - Score, ## reverse score so that higher score consistently means more concern for air quality
                  QuestionID %in% c("Q25","Q29") ~ 3 - Score, ## reverse score so that higher score means taking action
                  TRUE ~ Score)) |> # This makes it consistent with increased knowledge of air quality and increased concern (not necessarily better, but consistent)
              select(HouseID, Survey, QuestionID, Construct, Construct_general, Pollutant, Question, 
                     Correct_A, Response, Grade, Score, Score_mod) |> 
              arrange(HouseID, Survey, QuestionID)


## Create database with original survey answers and air quality data
## read in air quality data

aq_visits <- read_csv("./Processed Data//EvapCooler.summary.csv")

names(aq_visits)
## Calculate average I/O for each home by house,season, and number
aq_home_summary <- aq_visits |> 
                  select(House.Number,season,ac.type,`I/O_PM`,`I/O_O3`) |>
                  group_by(House.Number,season,ac.type) |> 
                  summarize(mean_IO_PM = mean(`I/O_PM`, na.rm=TRUE),
                            mean_IO_O3 = mean(`I/O_O3`, na.rm=TRUE),
                            n_PM = sum(!is.na(`I/O_PM`)),
                            n_O3 = sum(!is.na(`I/O_O3`))) |> 
                  ungroup()
                
#Add rankings for I/O by season and actype
aq_home_summary <- aq_home_summary |> 
                    rename(HouseID = House.Number) |> 
                    group_by(season,ac.type) |> 
                    mutate(PM_rank_season_actype = dense_rank(mean_IO_PM),
                              O3_rank_season_actype = dense_rank(mean_IO_O3)) |>
                    ungroup() |> 
                    group_by(season) |> ## now add rankings just by season and pollutant
                    mutate(PM_rank_season = dense_rank(mean_IO_PM),
                              O3_rank_season = dense_rank(mean_IO_O3)) |>
                    ungroup() |> 
                    arrange(season,ac.type,PM_rank_season_actype)

# Make aq_home_summary wide for easier joining
aq_home_summary_wide <- aq_home_summary |> 
                    pivot_wider(names_from = season,
                                values_from = c(mean_IO_PM, mean_IO_O3,n_PM, n_O3,
                                                PM_rank_season_actype, PM_rank_season,
                                                O3_rank_season_actype,O3_rank_season)) |> 
                    select(-c(mean_IO_O3_Winter,n_O3_Winter,O3_rank_season_actype_Winter,O3_rank_season_Winter)) ## remove O3 winter columns because we didn't analyze O3 in the winter

names(aq_home_summary_wide)
# Join with survey data

survey_aq <- survey_scores |> 
              left_join(aq_home_summary_wide, by = "HouseID")


## Thought-- I could add pollutant as a column, and have more generic IO ratios.... and merge them to questions that are also PM or O3 specific
## However, not all the questions are PM or O3 specific.... so maybe it doesn't make sense....

## export survey_aq data
write_csv(survey_aq, "./Processed Data/survey_aq_data.csv")

## Analyze survey score changes pre- and post-intervention

names(survey_scores)

## Take the average of Q15 and Q23, and Q16 and Q24 (they were intended to be differnet questions (the first about PM2.5 and the second about O3)
## But we accidently used the same question about ozone for both. So we will average the two responses for each question to get a more accurate response.

survey_scores_Q23 <- survey_scores |> 
                          filter(QuestionID %in% c("Q15","Q23")) |> 
                          group_by(HouseID, Survey, Construct, Construct_general, Pollutant) |>
                          summarize(Score = mean(Score, na.rm=TRUE),
                                    Score_mod = mean(Score_mod, na.rm=TRUE)) |>
                          ungroup() |>
                          mutate(QuestionID = "Q23") 

survey_scores_Q24 <- survey_scores |> 
                          filter(QuestionID %in% c("Q16","Q24")) |> 
                          group_by(HouseID, Survey, Construct, Construct_general, Pollutant) |>
                          summarize(Score = mean(Score, na.rm=TRUE),
                                    Score_mod = mean(Score_mod, na.rm=TRUE)) |>
                          ungroup() |>
                          mutate(QuestionID = "Q24") 

survey_diff <- survey_scores |> 
  filter(QuestionID %in% c(Q_K, Q_SS)) |> ## only knowledge and score questions
  filter(!(QuestionID %in% c("Q15","Q16","Q23","Q24"))) |> ## remove Q15, Q16, Q23, Q24 since we are replacing them with the averaged versions
  bind_rows(survey_scores_Q23, survey_scores_Q24) |> ## bring in the averaged versions
  mutate(Score = ifelse(QuestionID %in% c('Q15','Q16'), NA,Score)) |> ## remove Q15 and Q16 responses
  mutate(Score_mod = ifelse(QuestionID %in% c('Q15','Q16'), NA,Score_mod)) |> ## remove Q15 and Q16 responses
  select(HouseID, Construct, Construct_general, Pollutant, Survey, QuestionID, Score, Score_mod) |>
  pivot_wider(names_from = Survey, values_from = c(Score, Score_mod)) |> 
  mutate(Score_Change = Score_Post_Intervention - Score_Pre_Intervention,
        Score_mod_Change = Score_mod_Post_Intervention - Score_mod_Pre_Intervention) |> 
  left_join(aq_home_summary_wide, by = "HouseID")

## export data

write_csv(survey_diff, "./Processed Data/survey_score_changes.csv")


## calculate average score change by question
avg_score_change <- survey_diff |> 
  group_by(QuestionID) |> 
  summarize(mean = mean(Score_Change, na.rm=TRUE),
            sd = sd(Score_Change, na.rm=TRUE),
            n = sum(!is.na(Score_Change))) |> 
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound) 

## calculate average score_mod change by question
avg_score_mod_change <- survey_diff |> 
  group_by(QuestionID) |> 
  summarize(mean = mean(Score_mod_Change, na.rm=TRUE),
            sd = sd(Score_mod_Change, na.rm=TRUE),
            n = sum(!is.na(Score_mod_Change))) |> 
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound) 

## plot score changes by question

ggplot(survey_diff, aes(x=QuestionID, y=Score_Change)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.5) +
  geom_point(data=avg_score_change, aes(x=QuestionID, y=mean), 
             color="red", size=3) +
  theme_minimal() +
  labs(title="Change in Survey Scores by Question",
       x="Question ID",
       y="Score Change (Post - Pre)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(survey_diff, aes(x=QuestionID, y=Score_mod_Change)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.5) +
  geom_point(data=avg_score_change, aes(x=QuestionID, y=mean), 
             color="red", size=3) +
  theme_minimal() +
  labs(title="Change in Survey Scores by Question",
       x="Question ID",
       y="Score Change (Post - Pre)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=avg_score_change,aes(x=QuestionID, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  #expand_limits(y=c(-1,1))+
  #scale_y_continuous(breaks = seq(0,1,.2))+
    labs(title="Change in Survey Scores by Question",
       x="Question ID",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))

ggplot(data=avg_score_mod_change,aes(x=QuestionID, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  #expand_limits(y=c(-1,1))+
  #scale_y_continuous(breaks = seq(0,1,.2))+
    labs(title="Change in Survey Scores by Question",
       x="Question ID",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))


## scatter plot matrix of score changes

names(survey_diff)

survey_diff_wide <- survey_diff |> 
  select(HouseID, QuestionID, Score_Change) |> 
  pivot_wider(names_from = QuestionID, values_from = Score_Change)
names(survey_diff_wide)
ggpairs(survey_diff_wide, columns = 2:ncol(survey_diff_wide),
        title = "Scatter Plot Matrix of Survey Score Changes")

## Calculate sum of score changes by construct

survey_diff_construct <- survey_diff |> 
  group_by(HouseID, Construct, Construct_general, Pollutant) |> 
  summarize(Total_Score_Change = sum(Score_Change, na.rm=TRUE),
            Total_Score_mod_Change = sum(Score_mod_Change, na.rm=TRUE)) |> 
  ungroup() 

## calculate average score_mod change by question across houses
total_construct_score_mod_change <- survey_diff_construct |> 
  group_by(Construct) |> 
  summarize(mean = mean(Total_Score_mod_Change, na.rm=TRUE),
            sd = sd(Total_Score_mod_Change, na.rm=TRUE),
            n = sum(!is.na(Total_Score_mod_Change))) |> 
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound) 


## calculate average score_mod change by question across houses
total_construct_score_mod_change_pollutant <- survey_diff_construct |> 
  group_by(Construct_general, Pollutant) |> 
  summarize(mean = mean(Total_Score_mod_Change, na.rm=TRUE),
            sd = sd(Total_Score_mod_Change, na.rm=TRUE),
            n = sum(!is.na(Total_Score_mod_Change))) |> 
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound) 

## plot results
ggplot(data=total_construct_score_mod_change,aes(x=Construct, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  labs(title="Change in Survey Scores by Construct",
       x="Construct",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10, angle=45, hjust=1),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))

## plot results
ggplot(data=total_construct_score_mod_change_pollutant,aes(x=Construct_general, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  facet_grid(.~Pollutant, scales='free_x', space='free_x')+
  labs(title="Change in Survey Scores by Construct",
       x="Construct",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10, angle=45, hjust=1),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))

## 


## Calculate sum of score changes by construct, but not pollutant

survey_diff_construct_only <- survey_diff |> 
  group_by(HouseID, Construct_general) |> 
  summarize(Total_Score_Change = sum(Score_Change, na.rm=TRUE),
            Total_Score_mod_Change = sum(Score_mod_Change, na.rm=TRUE)) |> 
  ungroup() 


## calculate average score_mod change by question across houses
total_construct_only_score_mod_change <- survey_diff_construct_only |> 
  group_by(Construct_general) |> 
  summarize(mean = mean(Total_Score_mod_Change, na.rm=TRUE),
            sd = sd(Total_Score_mod_Change, na.rm=TRUE),
            n = sum(!is.na(Total_Score_mod_Change))) |> 
  mutate(tcrit = qt(.975,df=(n-1))) %>% ## two-sided 
  mutate(bound = tcrit*sd/sqrt(n)) %>%
  mutate(lower.95 = mean-bound) %>%
  mutate(upper.95 = mean+bound) 

## plot results
ggplot(data=total_construct_only_score_mod_change,aes(x=Construct_general, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  labs(title="Change in Survey Scores by Construct",
       x="Construct",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10, angle=45, hjust=1),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))

## plot results
ggplot(data=total_construct_score_mod_change_pollutant,aes(x=Construct_general, y= mean))+
  geom_col()+
  geom_errorbar(aes(ymin=lower.95,ymax=upper.95,width=0.25))+
  theme_bw()+
  facet_grid(.~Pollutant, scales='free_x', space='free_x')+
  labs(title="Change in Survey Scores by Construct",
       x="Construct",
       y="Score Change (Post - Pre)") +
  #scale_fill_brewer(palette = 'Paired')+
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(size=10),axis.text.x = element_text(size=10, angle=45, hjust=1),
        axis.title = element_text(size = 12),plot.title = element_text(size = 12,hjust=0.5),
        plot.margin= margin(t=1,r=1,b=0,l=1))

## 