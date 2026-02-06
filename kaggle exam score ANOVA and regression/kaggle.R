library(readr)
library(dplyr)

###Visualization with ggplot 
dataset <- read_csv("Exam_Score_Prediction.csv")

glimpse(dataset) #shows "vectors"
dim(dataset) #shows nrows and ncol 
str(dataset) #shows the type of variables 
summary(dataset) #shows statistics quantiles

head(dataset) #shows titles and first rows

#Let's begin by taking a look at the data 
#which are the variables? 
dataset |>
  select(where(is.numeric)) |>
  names()

library(ggplot2)

ggplot(dataset, aes(x = study_hours, y = exam_score)) +
  geom_point(alpha = 0.3, color = "gray30", size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Study hours",
    y = "Exam score",
    title = "Exam score vs study hours"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = study_hours, y = exam_score, color = gender)) +
  geom_point(alpha = 0.6, size = 1)+
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x = "Study hours",
    y = "Exam score",
    title = "Exam scores vs Study hours"
    ) +
  theme_minimal()


ggplot(dataset, aes(x = sleep_hours, y = exam_score)) +
  geom_point(alpha = 0.3, color = "gray30", size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Sleep hours",
    y = "Exam score",
    title = "Exam score vs sleep hours"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = class_attendance, y = exam_score)) + 
  geom_point(alpha = 0.3, color = "gray30", size = 1) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    x = "Class Attendance",
    y = "Exam score",
    title = "Exam score vs class attendance"
  ) + 
  theme_minimal()
  

# same chart, different colors for gender and course 
# first, we need to create a factor 

dataset <- dataset |>
  mutate(
    gender = factor(gender),
    course = factor(course)
  )
# also works: dataset$gender <- factor(dataset$gender)

dataset <- dataset |> 
  mutate(
    study_method = factor(study_method)
  )

dataset <- dataset |> 
  mutate(
    sleep_quality = factor(sleep_quality)
  )

#then we plot them with different colors 
ggplot(dataset, aes(x = study_hours, y = exam_score, color = gender)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x = "Study hours",
    y = "Exam score", 
    title = "Exam score vs study hours"
  ) +
  theme_minimal()

ggplot(dataset, aes(x=class_attendance, y = exam_score, color = gender)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    x = "Class attendance", 
    y = "Exam score", 
    title = "Exam score vs class attendance" 
  ) +
  theme_minimal()

ggplot(dataset, aes(x = study_hours, y = exam_score, color = gender)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    x = "Study hours",
    y = "Exam score",
    title = "Exam score vs study hours by gender"
  ) + 
  theme_minimal() 

#taking a look of what is inside the variables: study_method and 
table(dataset$study_method)
table(dataset$sleep_quality)
table(dataset$course)
table(dataset$internet_access)
table(dataset$facility_rating)
table(dataset$exam_difficulty)

# Let's visualize :) 
ggplot(dataset, aes(x = study_method)) + 
  geom_bar(fill = "blue") +
  labs(
    x = "Study method", 
    y = "Number of students",
    title = "Distribution of study methods"
  ) + 
  theme_minimal()

ggplot(dataset, aes(x = sleep_quality)) + 
  geom_bar(fill = "blue") + 
  labs(
    x = "Sleep quality",
    y = "number of students",
    title = "Sleep quality frequency"
  ) + 
  theme_minimal()


dataset$sleep_quality <- factor(
  dataset$sleep_quality,
  levels = c("poor", "average", "good")
)

ggplot(dataset, aes(x = sleep_quality)) + 
  geom_bar(fill = "blue") +
  labs(
    x = "Sleep quality",
    y = "Number of students",
    title = "Distribution of Students"
  ) + 
  theme_minimal()

ggplot(dataset, aes(x = study_method, y = exam_score)) + 
  geom_boxplot(fill = "grey80", color = "grey20") + 
  labs(
    x = "Study method", 
    y = "Exam score",
    title = "Exam score by study method"
  ) + 
  theme_minimal()

dataset$exam_difficulty <- factor(
  dataset$exam_difficulty,
  levels = c("easy", "moderate", "hard")
)  

ggplot(dataset, aes(x = exam_difficulty, y = exam_score)) + 
  geom_boxplot(fill = "grey80", color = "grey20") + 
  labs(
    x = "Exam difficulty",
    y = "Number of students",
    title = "Distribution of students"
  ) +
  theme_minimal()

dataset$facility_rating <- factor(
  dataset$facility_rating, 
  levels = c("low", "medium", "high")
)

ggplot(dataset, aes(x = facility_rating, y = exam_score)) + 
  geom_boxplot(fill = "grey80", color = "grey20") + 
  labs(
    x = "Facility rating",
    y = "Exam score",
    title = "Distribution of Exam score per facility rating"
  ) + 
  theme_minimal()


dataset$internet_access <- factor(dataset$internet_access)

ggplot(dataset, aes(x = exam_difficulty, y = exam_score)) + 
  geom_boxplot(fill = "grey90", color = "grey20") + 
  labs(
    x = "Exam difficulty",
    y = "Exam score",
    title = "Exam score vs exam difficulty"
  ) + 
  theme_minimal()
