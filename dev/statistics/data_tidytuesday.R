patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

patient_risk_profiles |> colnames()
patient_risk_profiles$`Aspirin exposures in prior year`
patient_risk_profiles$`predicted risk of Dementia` |> round(2)
scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')
scurvy
scurvy$gum_rot_d6
rladies_chapters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')
rladies_chapters
squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')
squirrel_data
summary(squirrel_data)
squirrel_data$`Other Activities`
winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
winners


medicaldata::polyps$baseline |> hist()
medicaldata::polyps$number3m |> hist()
medicaldata::polyps$number12m |> hist()


ct <- medicaldata::covid_testing

medicaldata::blood_storage

medicaldata::cytomegalovirus |> colnames()
medicaldata::esoph_ca

?medicaldata::strep_tb


