# main.R

# Load required packages (optional, depending on your script dependencies)

# 1. Load and process 9th grade student data
source("code/1_nine_grade_students.R")

# 2. Load and process school-level data
#source("code/2_schools.R")

# 3. Create panel data for 9th grade
source("code/3_panel_nine_grade.R")

source("code/quintis.R")
# 4. Apply Callaway and Sant’Anna DiD estimator
source("code/callaway_santanna.R")

# # 5. Analyze school dropout (evasão) using Lepes data
# source("lepes_evasao.R")
# 
# # 6. Analyze proficiency using Lepes data
# source("lepes_proficiencia.R")

cat("All scripts executed successfully.\n")
