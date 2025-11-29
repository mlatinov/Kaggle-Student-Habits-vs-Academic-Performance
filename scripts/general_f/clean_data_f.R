
#### Function to Clean the data ####
clean_data_f <- function(data){

  data %>%
    # Encode as 0 and 1 gender part_time_job extracurricular_participation
    mutate(
      gender = ifelse(gender == "Female", yes = "0", no = "1"), # Female = 0 Male = 1
      part_time_job = ifelse(part_time_job == "No", yes = "0", no = "1"), # 0 = No Job 1 = Job
      extracurricular_participation =
        ifelse(extracurricular_participation == "No", yes = "0", no = "1") # 0 = No 1 = Yes
      ) %>%
    # Change in parental_education_level level High School to High_School
    mutate(
      parental_education_level = ifelse(
        str_detect(string = parental_education_level, pattern = "High School"),
        yes = "High_School",
        no = parental_education_level
        )
    ) %>%
    # Change the Variables Classes from Char to Factors
    mutate(across(.cols = where(is.character),.fns = as.factor))
}
