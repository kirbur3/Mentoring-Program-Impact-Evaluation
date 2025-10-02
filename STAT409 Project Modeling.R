#Tommy Hoang-Tien's Modeling Code

# Prediction Function

###use this to make a matrix that plots prediction of the typical student###

library(dplyr)

# subject is our 5 categories
# social studies was apart of other due to low representative in semester 2

f.predmat <- function(subject, gap) {
  subj <- paste0(toupper(substr(subject, 1, 1)),
                 tolower(substr(subject, 2, nchar(subject))))
  max_week <- 20L
  
  # building the base of the dataframe
  base_df <- tibble(
    Week            = 1L:max_week,
    Course.Category = subj
  )
  pred_ment <- base_df %>%
    mutate(
      Mentor.Course     = "Yes",
      Grade.Year        = 10L,
      Period            = 1L,
      Semester          = "S1",
      Total.Days.Absent = mean(df8_clean$Total.Days.Absent, na.rm = TRUE),
      Student.ID        = 1L
    )
  pred_not <- base_df %>%
    mutate(
      Mentor.Course     = "No",
      Grade.Year        = 10L,
      Period            = 1L,
      Semester          = "S1",
      Total.Days.Absent = mean(df8_clean$Total.Days.Absent, na.rm = TRUE),
      Student.ID        = 1L
    )
  

  mentoring_weeks <- seq(2L, max_week, by = gap)
  pred_ment$`Running.Total..Class.` <- cumsum(pred_ment$Week %in% mentoring_weeks)
  pred_not$`Running.Total..Class.`   <- 0
  
  # 3) Combine & label
  combined <- bind_rows(
    pred_ment %>% mutate(Group = "Mentored"),
    pred_not  %>% mutate(Group = "Not Mentored")
  )
  
  # fills in dataframe and rearranges dataframe to fit d8_clean
  # d8_clean is a filtered dataset of original that only consists of mentored.students 
  missing_cols <- setdiff(names(df8_clean), names(combined))
  for (col in missing_cols) combined[[col]] <- NA
  combined <- combined[, c(names(df8_clean), "Group")]
  
  combined <- combined %>%
    mutate(
      Course.Category = factor(Course.Category, levels = levels(df8_clean$Course.Category)),
      Mentor.Course   = as.character(Mentor.Course),
      Grade.Year      = as.integer(Grade.Year),
      Period          = as.integer(Period),
      Semester        = factor(Semester, levels = levels(df8_clean$Semester)),
      `Running.Total..Class.` = as.numeric(`Running.Total..Class.`),
      Total.Days.Absent       = as.numeric(Total.Days.Absent),
      Student.ID = if (is.factor(df8_clean$Student.ID)) {
        factor(as.character(Student.ID), levels = levels(df8_clean$Student.ID))
      } else {
        as.integer(Student.ID)
      },
      Week.sem = as.numeric(Week)
    )
  
  # predicting with standard error 
  pr <- predict(
    gam_model_grades,
    newdata = combined,
    type   = "response",
    se.fit = TRUE,
    exclude = c("s(Student.ID)", "s(Student.ID,Week.sem)")
  )
  combined <- combined %>%
    mutate(
      fit   = pr$fit,
      se    = pr$se.fit,
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  
  
  
  # extract baseline per group (Week == 1)
  baseline_df <- combined %>%
    filter(Week == 1) %>%
    transmute(Group, baseline = fit)
  
  # left join baseline and compute adjusted fits
  combined <- combined %>%
    left_join(baseline_df, by = "Group")
  
  # pull out the mentored baseline value
  ment_ref <- baseline_df$baseline[baseline_df$Group == "Mentored"]
  
  # final adjustments to the matrix & carry 'gap' through
  # gap is the measurement of how spaced in between mentoring visits are 
  combined <- combined %>%
    mutate(
      ref       = ment_ref,
      fit_adj   = fit   - baseline + ref,
      lower_adj = lower - baseline + ref,
      upper_adj = upper - baseline + ref,
      gap       = gap
    )
  
  # returning what we need for the plot 
  combined %>%
    select(Week, Course.Category, Group, fit_adj, lower_adj, upper_adj, gap)
}







# Plotting Function

function(pred_df, ymin = -1.25, ymax = 0.25) {
  # pred_df needs: 
  # Week, Course.Category(this is subject), Group, fit_adj, lower_adj, upper_adj, gap
  df <- pred_df %>%
    mutate(
      is_mentored_point = Group == "Mentored" &
        Week >= 2 &
        ((Week - 2) %% gap == 0),
      Course.Category = droplevels(Course.Category),
      gap = factor(gap, 
                   levels = sort(unique(gap)),
                   labels = paste0("gap = ", sort(unique(gap)), " wk"))
    )
  
  ggplot(df, aes(x = Week, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = lower_adj, ymax = upper_adj),
                alpha = 0.2, color = NA, na.rm = TRUE) +
    geom_line(aes(y = lower_adj), linewidth = 0.4, na.rm = TRUE) +
    geom_line(aes(y = upper_adj), linewidth = 0.4, na.rm = TRUE) +
    geom_line(aes(y = fit_adj), linewidth = 1.2, na.rm = TRUE) +
    geom_point(
      data = filter(df, is_mentored_point),
      aes(x = Week, y = fit_adj),
      shape = 4, size = 2.2, stroke = 1,
      color = "red", na.rm = TRUE
    ) +
    # rows = subject, cols = gap
    facet_grid(Course.Category ~ gap) +
    scale_x_continuous(breaks = seq(2, 20, by = 2)) +
    scale_color_manual(
      name   = "Course Type",
      values = c(Mentored = "#1A9850", `Not Mentored` = "#377EB8"),
      labels = c("Mentored", "Not Mentored")
    ) +
    scale_fill_manual(
      name   = "Course Type",
      values = c(Mentored = "#66BD63", `Not Mentored` = "#377EB833"),
      labels = c("Mentored", "Not Mentored")
    ) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(
      title    = "Predicted Z-Score Trajectories of Mentored Students(Grades)",
      subtitle = "(Red X marks mentoring sessions)",
      x        = "Week",
      y        = "Predicted Z Score(Grades)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_line(linewidth = 0.3, colour = "gray90"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(hjust = 0.5, size = 7),
      strip.text       = element_text(face = "bold")
    )
}





# Single Geometry Plot


# Step 1: Filter and label mentoring/attendance
class_data <- df5 %>%
  filter(Course.Code == "MAT522 / 12",
         Staff.Names == "ROCHELLE AITON",
         Period == 2,
         Semester == "S1") %>%
  mutate(
    Mentored = trimws(Mentored.Student) == "Yes",
    Attended_Mentoring_Week = Mentored & trimws(Attended) == "Yes"
  )

# Step 2: GPA stats from non-mentored students
nonmentored_gpa_stats <- class_data %>%
  filter(!Mentored) %>%
  group_by(Week) %>%
  summarise(
    nonment_avg = mean(Grade.Perc, na.rm = TRUE),
    nonment_sd  = sd(Grade.Perc, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Compute Z-scores
all_students <- class_data %>%
  left_join(nonmentored_gpa_stats, by = "Week") %>%
  mutate(Z_Score = (Grade.Perc - nonment_avg) / nonment_sd)

# Step 4: Compute IQR (25th–75th percentile) of non-mentored Z-scores
nonmentored_z_summary <- all_students %>%
  filter(!Mentored) %>%
  group_by(Week) %>%
  summarise(
    z_lower = quantile(Z_Score, 0.25, na.rm = TRUE),
    z_upper = quantile(Z_Score, 0.75, na.rm = TRUE),
    gpa_label = round(mean(Grade.Perc, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Step 5: Identify students who attended at least once
attended_at_least_once <- all_students %>%
  group_by(Student.ID) %>%
  summarise(attended_any = any(Attended_Mentoring_Week), .groups = "drop")

# Step 6: Merge that back into the data
all_students <- left_join(all_students, attended_at_least_once, by = "Student.ID")

# Step 7: Add legend group for unmentored dots
all_students <- all_students %>%
  mutate(legend_group = case_when(
    !Mentored ~ "Unmentored",
    Attended_Mentoring_Week ~ "Attended",
    Mentored & attended_any ~ "Mentored",
    TRUE ~ NA_character_
  ))

# Step 8: Final plot
ggplot() +
  geom_ribbon(data = nonmentored_z_summary,
              aes(x = Week, ymin = z_lower, ymax = z_upper),
              fill = "grey80", alpha = 0.25) +
  
  geom_text(data = nonmentored_z_summary,
            aes(x = Week, y = 0, label = gpa_label),
            angle = 90, vjust = -0.5, size = 3, color = "black") +
  
  # Unmentored student grey dots with legend
  geom_point(data = all_students %>% filter(!is.na(legend_group) & legend_group == "Unmentored"),
             aes(x = Week, y = Z_Score, color = legend_group, shape = legend_group),
             size = 1.2, alpha = 0.6) +
  
  # Mentored trend lines (blue) with legend
  geom_line(data = all_students %>% filter(Mentored & attended_any),
            aes(x = Week, y = Z_Score, group = Student.ID, color = "Mentored"),
            linewidth = 1, alpha = 0.7) +
  
  # Red X for mentoring session attended
  geom_point(data = all_students %>% filter(Attended_Mentoring_Week),
             aes(x = Week, y = Z_Score, color = "Attended", shape = "Attended"),
             size = 3) +
  
  # Blue X for mentored students who missed
  geom_point(data = all_students %>% filter(Mentored & !Attended_Mentoring_Week & attended_any),
             aes(x = Week, y = Z_Score, color = "Mentored", shape = "Mentored"),
             size = 3) +
  
  # Grey dots for mentored students who never attended (not in legend)
  geom_point(data = all_students %>% filter(Mentored & !attended_any),
             aes(x = Week, y = Z_Score),
             color = "darkgrey", size = 1.2, alpha = 0.6) +
  
  labs(title = "Z-Score Comparison within a Class (Mentored vs. Unmentored)",
       x = "Week", y = "Z-Score (Standardized GPA)",
       color = "Legend", shape = "Legend") +
  
  scale_color_manual(values = c(
    "Mentored" = "#70A9D1",         # Blue for trend lines and blue X
    "Attended" = "red",             # Red for attended X
    "Unmentored" = "darkgrey"       # Grey dots for unmentored students
  )) +
  scale_shape_manual(values = c(
    "Mentored" = 4,
    "Attended" = 4,
    "Unmentored" = 16
  )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(min(all_students$Week, na.rm = TRUE),
                                  max(all_students$Week, na.rm = TRUE), by = 1))







# GAM Modeling


df8_clean <- df8 %>%
  # forcing column of our subjects 
  mutate(
    Course.Category = case_when(
      Subject %in% c("Math", "Science", "English", "Business") ~ Subject,
      TRUE ~ "Other"
    ),
    Course.Category = factor(
      Course.Category,
      levels = c("Other", "Math", "Science", "English", "Business")
    )
  ) %>%
  # Create Week.sem and updated Semester factor (“S1” for Week 1–20, “S2” for Week ≥21)
  mutate(
    Week.sem = if_else(Week <= 20, Week, Week - 20),
    Semester = if_else(Week >= 21, "S2", "S1"),
    Semester = factor(Semester, levels = c("S1", "S2"))
  ) %>%
  # remove nas
  filter(!is.na(Running.Total..Class.)) %>%
  filter(trimws(Mentored.Student) == "Yes") %>%
  droplevels()

# this can be removed 
# if we remove the single 9 grader the n i got was 10116 obs of 30 variables(columns)
df8_clean <- df8_clean %>%
  filter(Grade.Year != 9)

# gam model with smooths and coefficients 
gam_model_grades <- gam(
  Z.Score ~ 
    Mentor.Course * Grade.Year +
    Mentor.Course * Course.Category +
    s(Week.sem, k = 15) +     # smooth within‐semester
    Period +
    Semester +                # now a factor ("S1"/"S2")
    s(Running.Total..Class., k = 6) +
    s(Total.Days.Absent,      k = 6) +
    s(Student.ID, Week.sem, bs = "re") +  # random slope by student over Week.sem
    s(Student.ID, bs = "re"),
  data   = df8_clean,
  method = "REML")

summary(gam_model_grades)




