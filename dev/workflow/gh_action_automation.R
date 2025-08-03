library(tidyverse)
library(googlesheets4)

# The data is publicly available so we don't need to authenticate
gs4_deauth()

survey_data <- read_sheet("https://docs.google.com/spreadsheets/d/13kfPtyQP1xmL4Rn6rfJHgJcAblfH7pxS5RvdmGe6BHg/edit?usp=sharing")

survey_data %>%
  write_rds("survey_data.rds")


# check the explaination in the quarto doc!

# gs4 authentication
gs4_auth()
# this opens a web-browser to let you sign-in with google

# after auth, try to read
link <- "https://docs.google.com/spreadsheets/d/1pdykU23HqTm2qv0pLw6uFuO5jHkdlRv1zpkyvETrcno/edit?gid=785448353#gid=785448353"
sheet <- "Open Positions"
mydata <- read_sheet(ss = link, sheet = sheet)

?read_sheet
