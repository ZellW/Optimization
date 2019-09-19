# BUSINESS SCIENCE - LEARNING LAB 15 ----
# CASE 1 - What Minimum Exam Grade Do I Need To Get To Pass The Course?

# 1.0 LIBRARIES ----

# Solver Backend
library(ROI)
library(ROI.plugin.glpk)

# Tidy Linear Programming
library(ompr)
library(ompr.roi)

# Core
library(tidyverse)

# Timing
library(tictoc)

# 2.0 DATA ----

exam_grades_tbl <- tibble(
    exam   = c("Quiz #1", "Midterm", "Quiz #2", "Final"),
    grade  = c(50, 65, 70, NA),
    weight = c(0.15, 0.25, 0.15, 0.45)
)

exam_grades_tbl

# 3.0 CONSTRAINTS -----

final_grade_min  <- 0
final_grade_max  <- 100 
course_grade_max <- 70


# 4.0 OMPR OPTIMIZATION MODEL ----

sum_prod_exams_1_3 <- sum(exam_grades_tbl$grade[1:3] * exam_grades_tbl$weight[1:3]) 

tic()
model <- MIPModel() %>%
    add_variable(final_grade, type = "continuous", lb = final_grade_min) %>%
    set_objective(final_grade, "min") %>%
    add_constraint(final_grade * exam_grades_tbl$weight[4] + sum_prod_exams_1_3 == course_grade_max) %>% 
    solve_model(with_ROI(solver = "glpk")) 
toc()

model

ompr::get_solution(model, final_grade) %>% enframe()



# APPENDIX - ROI VERSION ----

model_lp <- OP(
    objective   = L_objective(L = 1, names = "final_grade"),
    constraints = rbind(
        L_constraint(L = exam_grades_tbl$weight[4], dir = "==", course_grade_max - sum_prod_exams_1_3),
        L_constraint(L = 1, "<=", 100),
        L_constraint(L = 1, ">=", 0)
        ), 
    maximum = FALSE
)

sol <- ROI_solve(model_lp, "glpk")

solution(sol)


