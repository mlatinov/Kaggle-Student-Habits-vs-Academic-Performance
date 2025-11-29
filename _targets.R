
#### Global Libraries ####
library(tidyverse)
library(targets)

## Source Functions ##
tar_source(files = "scripts/")

## Pipeline ##
list(

  # Load the Dataset
  tar_target(
    name = student_habits_data,
    command = read.csv("data/student_habits_performance.csv")
  ),

  # Clean the Data
  tar_target(
    name = clean_habits,
    command = clean_data_f(student_habits_data)
  ),

  # Exploratory Data analysis
  tar_target(
    name = eda_habits,
    command = eda_hatits_f(clean_habits)
  ),

  # Generate DAG
  tar_target(
    name = habits_dag,
    command = generate_dag()
  ),

  # Create a Scientific Generative Model
  tar_target(
    name = generative_habit,
    command = generate_habits_f()
  )






)
