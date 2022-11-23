library(tidyverse)
library(magrittr)
library(bigrquery)
library(caret)

con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
hypertension <- tbl(con, "course3_data.hypertension_goldstandard")
hypertension

diagnoses_icd <- tbl(con, "mimic3_demo.DIAGNOSES_ICD")
### DON'T RE-RUN AGAIN
training <- hypertension %>% 
  collect() %>% 
  sample_n(80)

write.csv(training,"\\training.csv", row.names = FALSE)
###

training_htn = read.csv('\\training.csv')

getStats <- function(df, ...){
  df %>%
    select_(.dots = lazyeval::lazy_dots(...)) %>%
    mutate_all(funs(factor(., levels = c(1,0)))) %>% 
    table() %>% 
    confusionMatrix()
}

###
### DON'T RE-RUN AGAIN
icd_4011 <- diagnoses_icd %>% 
  filter(ICD9_CODE == "4011") %>% 
  distinct(SUBJECT_ID) %>% 
  mutate(icd_4011 = 1)

write.csv(icd_4011,'icd_4011.csv',row.names = FALSE)
###

icd_4011 = read_csv('icd_4011.csv')
training_htn %>% 
  left_join(icd_4011) %>% 
  mutate(icd_4011 = coalesce(icd_4011, 0)) %>% 
  collect() %>% 
  getStats(icd_4011, HYPERTENSION)

###
icd_4010 <- diagnoses_icd %>% 
  filter(ICD9_CODE == "4010") %>% 
  distinct(SUBJECT_ID) %>% 
  mutate(icd_4010 = 1)

write.csv(icd_4010,'icd_4010.csv',row.names = FALSE)
###
icd_4010 = read_csv('icd_4010.csv')
training_htn %>% 
  left_join(icd_4010) %>% 
  mutate(icd_4010 = coalesce(icd_4010, 0)) %>% 
  collect() %>% 
  getStats(icd_4010, HYPERTENSION)
###

icd_4019 <- diagnoses_icd %>% 
  filter(ICD9_CODE == "4019") %>% 
  distinct(SUBJECT_ID) %>% 
  mutate(icd_4019 = 1)

write.csv(icd_4019,'icd_4019.csv',row.names = FALSE)
###
icd_4019 = read_csv('icd_4019.csv')
training_htn %>% 
  left_join(icd_4019) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0)) %>% 
  collect() %>% 
  getStats(icd_4019, HYPERTENSION)
###


### TESIING DIAGNOSIS OF PATIENTS BASED ON SYS AND DIA BP
### WE NEED TO INCLUDE PATIENTS WITH SYSTOLIC ONLY THEN DIASTOLIC ONLY THEN EITHER ONE OF THEM AND SEE THE RESULT
chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
sys <- chartevents %>%
  filter(ITEMID %in% c(220179,51,455,220050,225309)) %>% 
  mutate(sys_htn = case_when(as.numeric(VALUE) >= 140 ~ 1,
                             TRUE ~0)) %>%
  group_by(SUBJECT_ID) %>%
  summarise(sys_htn_count = sum(sys_htn, na.rm = TRUE)) %>% 
  mutate(sys_htn_min2 = case_when(sys_htn_count >= 2 ~ 1,
                                      TRUE ~ 0)) %>% 
  select(SUBJECT_ID, sys_htn_min2)
  
sys

write.csv(sys,'sys.csv',row.names = FALSE)
###
dia <- chartevents %>%
  filter(ITEMID %in% c(220180,8368,8441,220051,225310)) %>% 
  mutate(dia_htn = case_when(as.numeric(VALUE) >= 90 ~ 1,
                             TRUE ~0)) %>%
  group_by(SUBJECT_ID) %>%
  summarise(dia_htn_count = sum(dia_htn, na.rm = TRUE)) %>% 
  mutate(dia_htn_min2 = case_when(dia_htn_count >= 2 ~ 1,
                                  TRUE ~ 0)) %>% 
  select(SUBJECT_ID, dia_htn_min2)

dia

write.csv(dia,'dia.csv',row.names = FALSE)
###
### testing sys only
sys = read_csv('sys.csv')

training_htn %>% 
  left_join(sys) %>% 
  mutate(sys_htn_min2 = coalesce(sys_htn_min2, 0)) %>% 
  collect() %>% 
  getStats(sys_htn_min2, HYPERTENSION)

dia = read_csv('dia.csv')

training_htn %>% 
  left_join(dia) %>% 
  mutate(dia_htn_min2 = coalesce(dia_htn_min2, 0)) %>% 
  collect() %>% 
  getStats(dia_htn_min2, HYPERTENSION)
###
###combination icd_4019 or sys_htn_min2

training_htn %>% 
  left_join(icd_4019) %>% 
  left_join(sys) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0),
         sys_htn_min2 = coalesce(sys_htn_min2, 0)) %>% 
  mutate(icd_4019_or_sys_htn_min2 = case_when(icd_4019 == 1 |
                                                sys_htn_min2 == 1 ~ 1,
                                               TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_4019_or_sys_htn_min2, HYPERTENSION)

###combination icd_4019 and sys_htn_min2
training_htn %>% 
  left_join(icd_4019) %>% 
  left_join(sys) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0),
         sys_htn_min2 = coalesce(sys_htn_min2, 0)) %>% 
  mutate(icd_4019_and_sys_htn_min2 = case_when(icd_4019 == 1 &&
                                                sys_htn_min2 == 1 ~ 1,
                                              TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_4019_and_sys_htn_min2, HYPERTENSION)


###nested combination
training_htn %>% 
  left_join(icd_4019) %>% 
  left_join(sys) %>% 
  left_join(dia) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0),
         sys_htn_min2 = coalesce(sys_htn_min2, 0),
         dia_htn_min2 = coalesce(dia_htn_min2, 0)) %>% 
  mutate(icd_4019_or_sys_dia_htn_min2 = case_when(icd_4019 == 1 |
                                                 sys_htn_min2 == 1 | sys_htn_min2 == 1  ~ 1,
                                               TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_4019_or_sys_dia_htn_min2, HYPERTENSION)

###combination icd or dia
training_htn %>% 
  left_join(icd_4019) %>% 
  left_join(dia) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0),
         dia_htn_min2 = coalesce(dia_htn_min2, 0)) %>% 
  mutate(icd_4019_or_dia_htn_min2 = case_when(icd_4019 == 1 |
                                                dia_htn_min2 == 1 ~ 1,
                                              TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_4019_or_dia_htn_min2, HYPERTENSION)

###combination icd and dia
training_htn %>% 
  left_join(icd_4019) %>% 
  left_join(dia) %>% 
  mutate(icd_4019 = coalesce(icd_4019, 0),
         dia_htn_min2 = coalesce(dia_htn_min2, 0)) %>% 
  mutate(icd_4019_and_dia_htn_min2 = case_when(icd_4019 == 1 &&
                                                dia_htn_min2 == 1 ~ 1,
                                              TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_4019_and_dia_htn_min2, HYPERTENSION)