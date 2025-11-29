
#### Function to make Exploratory Data Analysis Plots ####
eda_hatits_f <- function(data){

  # Global Set and Libraries
  library(patchwork)
  set_theme(theme_minimal())

  #### Univariate Analysis ####

  ## Test Exam Scores Distribution
  u_exam <-
    ggplot(data = clean_habits,aes(x = exam_score))+
    geom_histogram(colour = "black",fill = "lightblue",bins = 30)
  u_exam_2 <-
    ggplot(data = clean_habits,aes(x = exam_score))+
    geom_boxplot(colour = "black",fill = "lightblue")

  # Combine the plot #
  final_exam_plot <- (u_exam + u_exam_2)

  ## Study time Distribution
  u_study <-
    ggplot(data = clean_habits,aes(x = study_hours_per_day))+
    geom_histogram(colour = "black",fill = "lightblue",bins = 30)
  u_exam_2 <-
    ggplot(data = clean_habits,aes(x = study_hours_per_day))+
    geom_boxplot(colour = "black",fill = "lightblue")

  # Combine the plot #
  final_study_plot <- (u_study + u_exam_2)

  ## Age Distribution
  u_age <-
    ggplot(data = clean_habits,aes(x = age))+
    geom_density(colour = "black",fill = "lightblue")

  ## Internet Quality Distribution ##
  u_internet <-
    ggplot(data = clean_habits,aes(x = internet_quality))+
    geom_bar(color = "black",fill = "lightblue")


  #### Bivariate Analysis ####

  #### R Extracurricular_participation ~ Part Time Job ####
  p_1 <-
    ggplot(data = clean_habits,aes(x = extracurricular_participation,fill = part_time_job))+
    geom_bar()+
    scale_fill_viridis_d(option = "B",begin = 0.1,end = 0.5)+
    labs(
      title = "Extracurricular_participation ~ Part Time Job",
      x = "Extracurricular_participation",
      y = "Count",
      fill = "Part Time Job"
    )+
    scale_x_discrete(labels = c("Don't Attend", "Attend"))

  # Stat Table
  t_1 <-
    clean_habits %>%
    group_by(extracurricular_participation, part_time_job) %>%
    summarise(count = n(),.groups = "drop") %>%
    group_by(extracurricular_participation) %>%
    mutate(
      total = sum(count),
      rate = count / total,
      percentage = rate * 100
    ) %>%
    ungroup()

  #### R Extracurricular_participation ~ Parental Education ####
  p_2 <-
    ggplot(data = clean_habits,aes(x = fct_infreq(parental_education_level),fill = extracurricular_participation))+
    geom_bar()+
    scale_fill_viridis_d(option = "B",begin = 0.1,end = 0.5)+
    labs(
      title = " Extracurricular_participation ~ Parental Education",
      x = "Parental Education",
      y = "Count",
      fill = "Extracurricular_participation"
    )

  # Stat Table
  t_2 <-
    clean_habits %>%
    count(extracurricular_participation, parental_education_level) %>%
    group_by(extracurricular_participation) %>%
    mutate(
      total = sum(n),
      rate = n / total,
      percentage = rate * 100
    ) %>%
    ungroup()

  #### R Part time Job ~ Study time ####
  t_3 <-
    clean_habits %>%
    group_by(parental_education_level) %>%
    summarise(
      mean_study_time = mean(study_hours_per_day),
      meadian_study_time = median(study_hours_per_day)
      )

  #### R Part time job ~ Age ####
  p_4 <-
    clean_habits %>%
    count(age,part_time_job) %>%
    group_by(age) %>%
    mutate(
      percent_job = n / sum(n) * 100
    ) %>%
    ggplot(aes(x = age,y = percent_job,fill = part_time_job))+
    geom_col(position = "dodge")+
    scale_fill_viridis_d(option = "B",begin = 0.1,end = 0.5)+
    labs(
        title = " Age ~ Part Time Job Participation",
        x = "Age",
        y = "Part Time Job Participation %",
        fill = "Part Time Job Participation"
    )

  #### R Study Time ~ Test Score ####
  lm_1 <- lm(exam_score ~ study_hours_per_day,data = clean_habits)
  alpha_1 <- lm_1$coefficients[1]
  beta_1 <- lm_1$coefficients[2]

  p_5 <-
    ggplot(data = clean_habits, aes(x = study_hours_per_day, y = exam_score)) +
    geom_point(aes(color = exam_score)) +
    geom_smooth(method = "lm", se = FALSE,color = "black") +
    scale_color_viridis_c(begin = 0.2, end = 0.5) +
    labs(
      title = "Test Score ~ Study Time",
      subtitle = paste0("Formula:","y = ",round(alpha_1,digits = 2),"+",round(beta_1,digits = 2),"* Study Time"),
      x = "Study Time",
      y = "Test Score"
    )

  #### R Sleep Test Score ####
  p_6 <-
    ggplot(data = clean_habits,aes(x = sleep_hours,y = exam_score,color = exam_score))+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE,color = "black") +
    scale_color_viridis_c(begin = 0.2, end = 0.5) +
    labs(
      title = "Test Score ~ Sleep",
      x = "Sleep Time",
      y = "Test Score"
    )

  #### R Mental Health Exam Score ####
  p_7 <-
    ggplot(data = clean_habits,aes(exam_score))+
    geom_boxplot()+
    facet_wrap(~mental_health_rating)+
    labs(
      title = "Exam score by Levels of Mental Health",
      x = "Exam Scores"
    )

  #### R Netflix Hours and internet_quality ####
  p_8 <-
    clean_habits %>%
    group_by(internet_quality)%>%
    summarise(
      mean_netflix = mean(netflix_hours),
      quantile_95 = quantile(netflix_hours,0.95),
      quantile_05 = quantile(netflix_hours,0.05)
    ) %>%
    ggplot(aes(x  = internet_quality,y = mean_netflix))+
    geom_point()


  # Return Plots
  return(list(
    univariate = list(
      exam_scores = final_exam_plot,
      study_time = final_study_plot
    ),
    bivariate = list(
      extracurricular_participation_part_time_job = p_1,
      extracurricular_participation_parental_education = p_2,
      test_score_study_time = p_5,
      test_score_sleep = p_6,
      test_score_mental_health = p_7
    )
  ))
}
