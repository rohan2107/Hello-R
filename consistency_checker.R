# Change this to TRUE if you want to force a reinstall of SCEMChecker
# (Useful to get the latest version, as this is still a work in development)
reinstall_SCEMchecker <- FALSE

# ========== SETUP ==========
if(!("remotes") %in% rownames(installed.packages())){
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

if(reinstall_SCEMchecker | !("SCEMChecker") %in% rownames(installed.packages())){
  remotes::install_github("fcampelo/SCEMChecker",
                          dependencies = c("Imports"),
                          force = TRUE)
}

library(SCEMChecker)


# ========== SET CHECKING PARAMETERS ==========

# Path to the Checkfile (the template Rmd file)
checkfile_path <- "C:/Users/rohta/OneDrive/Desktop/MSc Data Science/SCEM - Statistical Computing and Empirical Methods/Labs/Hello-R/Week 5/W05_checkfile.Rmd"

# Path to your submission file (change as needed)
submission_path <- "C:/Users/rohta/OneDrive/Desktop/MSc Data Science/SCEM - Statistical Computing and Empirical Methods/Labs/Hello-R/Week 5/Assignment5.Rmd"

# Change lab number to the one you want to check - UPDATED FOR W03
lab_number <- 5


# ========== RUN CHECKER ==========

# 'mycheck' will contain a data frame with the details of the check
mycheck <- consistency_checker(checkfile_path, submission_path, template_number = lab_number)

# print(mycheck) # <--- uncomment to inspect the raw data frame

# This prints a summary of any issues encountered
# If there are no issues, it shows a blank list
summary(mycheck)
