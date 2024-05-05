
data <- data |>
  mutate(log_amount = log(sum_amount, 10)) |>
  mutate_at(vars(contains("medical") | contains("trt") | contains("symptom")), as.factor)

#mixed effect models ------------------------------------------------------------
m1 <- lmer(log_amount ~ medical_history_dia + medical_history_hbp +
             medical_history_ren + medical_history_anx + 
             medical_history_mood + trt_anx + trt_con + trt_adt + trt_the + trt_oth + 
             symptom_1 + symptom_2 + symptom_3 + symptom_4 + symptom_5 + 
             cgis_adm + cgis_dis + gaf_lv + scale(stay_duration) +  scale(age) + scale(BMI) +
             (1|gender)+(1|race)+(1|resident_status)+(1|patient_id), 
           data = data)

summary(m1)
vif(m1)
r.squaredGLMM(m1)


m1.1 <- update(m1,~. - (1|patient_id))
r.squaredGLMM(m1.1)

# extract random effect-------------------------------------------------------------------------------------------------------------
random_effects_df <- as.data.frame(ranef(m1))
race_effect <- ggplot(random_effects_df |> filter(grpvar == "race"), 
                      aes(x = grp, y = condval)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = condval-1.96* condsd, ymax = condval+1.96* condsd),
                width = 0.2, color = "blue") +
  labs(x = "Group", y = "Random Effect") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

gender_effect <- ggplot(random_effects_df %>% filter(grpvar == "gender"), aes(x = grp, y = condval)) +
  geom_point(color = "purple") +
  geom_errorbar(aes(ymin = condval-1.96* condsd, ymax = condval+1.96* condsd),
                width = 0.2, color = "purple") +
  labs(x = "Group", y = "Random Effect") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

res_status_effect <- ggplot(random_effects_df |> filter(grpvar == "resident_status"), aes(x = grp, y = condval)) +
  geom_point(color = "brown") +
  geom_errorbar(aes(ymin = condval-1.96* condsd, ymax = condval+1.96* condsd),
                width = 0.2, color = "brown") +
  labs(x = "Group", y = "Random Effect") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )


# extract fixed effect-------------------------------------------------------------------------------------------------------------
fixed_effects <- fixef(m1)

se <- sqrt(diag(vcov(m1)))

forest_data <- data.frame(
  Variables = names(fixed_effects),
  Estimate = fixed_effects,
  lower = fixed_effects - 1.96 * se,
  upper = fixed_effects + 1.96 * se
)

forest_data$Variables <- factor(forest_data$Variables, 
                                levels = forest_data$Variables[order(forest_data$Estimate)])


forest_plot <- ggplot(forest_data |> filter(! Variables == "(Intercept)"), aes(x = Estimate, y = Variables)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Fixed effect", y = "Predictors") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
    )



# all result-------------------------------------------------------------------------------------------------------------

combined_plot <- ggarrange(forest_plot, 
                           plot_grid(race_effect, gender_effect,
                           res_status_effect, ncol = 3, nrow = 2,
                           rel_heights = c(3, 1)), nrow = 2, ncol = 1)

combined_plot

ggsave("combined_plot.pdf", combined_plot, width = 8, height = 8)
