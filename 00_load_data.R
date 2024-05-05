
# Load packages for analysis --------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here, tidyverse, lubridate, ggplot2, ggpubr, cowplot, zoo, lme4, car, MuMIn
)


# Load data and data cleaning-------------------------------------------------------------------------------------------------------------

demographics <- read.csv(here("SDS RWE Data Challenge Dataset/SDS RWE Data Challenge Dataset", 
                      "demographics.csv"))

demographics <- demographics |> 
  mutate(date_of_birth = as.Date(date_of_birth, format = "%Y-%m-%d")) |> 
  mutate(gender = case_when(
    gender == "f" ~ "Female",
    gender == "m" ~ "Male",
    .default = as.character(gender)
  )) |>
  mutate(resident_status = case_when(
    resident_status == "Singapore citizen" ~ "Singaporean",
    .default = as.character(resident_status)
  )) |>
  mutate(race = case_when(
    race == "India" ~ "Indian",
    race == "chinese" ~ "Chinese",
    .default = as.character(race)
  ))




clinical_data <- read.csv(here("SDS RWE Data Challenge Dataset/SDS RWE Data Challenge Dataset", 
                              "clinical_data.csv")) #repeated id
clinical_data <- clinical_data |> 
  mutate(date_of_admission = as.Date(date_of_admission, format = "%d/%m/%y"),
         date_of_discharge = as.Date(date_of_discharge, format = "%d/%m/%y")) |>
  mutate(stay_duration = as.numeric(difftime(date_of_discharge, date_of_admission, units = "days"))) |>
  mutate(medical_history_hbp = case_when(
    medical_history_hbp == "No" ~ "0",
    medical_history_hbp == "Yes" ~ "1",
    .default = as.character(medical_history_hbp)
  ))



bill_id <- read.csv(here("SDS RWE Data Challenge Dataset/SDS RWE Data Challenge Dataset", 
                         "bill_id.csv")) #repeated id, 4 to 16 bills per patient
bill_id <- bill_id |> 
  mutate(date_of_admission = as.Date(date_of_admission, format = "%Y-%m-%d")) 


bill_amount <- read.csv(here("SDS RWE Data Challenge Dataset/SDS RWE Data Challenge Dataset", 
                         "bill_amount.csv"))

# Final dataset -------------------------------------------------------------------------------------------------------------


data <- demographics |> full_join(clinical_data, 
                                  by=c("patient_id" = "id"), 
                                  relationship = "one-to-many") |>
  full_join(bill_id, 
            by=c("patient_id", "date_of_admission"), 
            relationship = "one-to-many") |>
  full_join(bill_amount, by=c("bill_id"), 
            relationship = "one-to-one") |>
  mutate(age = year(date_of_admission) - year(date_of_birth)) |>
  mutate(BMI = weight/(height/100)^2)


bill_each <- data |> group_by(patient_id) |> 
  summarise(sum_amount = sum(amount)) |> ungroup() 

data <- data |> select(-c(bill_id, amount)) |> distinct() |> 
  left_join(bill_each, by =c("patient_id"))




#Visual ------------------------------------------------------------------------------
temp <- data |> select(contains("medical")|contains("trt")|contains("symptom"))

count_result <- lapply(temp, table)
count_df <- do.call(rbind, lapply(count_result, as.data.frame))
count_df$variable <- rep(names(count_result), sapply(count_result, length))

des <- ggplot(count_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Category", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

ggsave("des.pdf", des, width = 8, height = 8)


duration <- data |> ggplot() + geom_point(aes(x= stay_duration, y = sum_amount)) +
  labs(x = "Stay Duration", y = "Amount") + theme_bw()

ggsave("duration.pdf", duration, width = 4, height = 4)


