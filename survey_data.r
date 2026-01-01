## Read in survey data from excel files

library(readxl)
library(tidyverse)

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

## Read in answer key
## Note I deleted some of the empty columns from the Data dictionary sheet before importing the data
## I also added a column name A8 to column K
names(answer_key)

answer_key <- read_excel("./Data/Survey Data/Air Quality Survey Sheet.xlsx", 
                         sheet = "Data Dictionary") |> 
  rename(QuestionID = "Question #")  |> 
  mutate(QuestionID = paste0("Q",as.integer(QuestionID)))


## Join the survey data with the answer key to get question text and answer choices

survey_grade <- survey |>
  left_join(answer_key, by = "QuestionID") |>
  filter(QuestionID %in% c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")) |>
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
## What are the severity and susceptibility questions?
Q_SS <- c("Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20",
          "Q21","Q22","Q23","Q24","Q25")

survey_scores <- survey |> 
              filter(QuestionID %in% Q_SS) |> ## SS questions
              left_join(answer_key, by = "QuestionID") |>
              bind_rows(survey_grade) |> 
              mutate(Score = ifelse(is.na(Grade), as.numeric(Response), as.numeric(Grade))) |> ## combine grades and SS responses into one column
              select(HouseID, Survey, QuestionID, Construct, Question, 
                     Correct_A, Score) |> 
              arrange(HouseID, Survey, QuestionID)

## Before and After change in scores

names(survey_scores)

survey_diff <- survey_scores |> 
  pivot_wider(names_from = Survey, values_from = Score) |> 
  mutate(Score_Change = Post_Intervention - Pre_Intervention)


