# use liggetid data for a few methods 
library(dplyr)
library(ggplot2)
liggetid <- readxl::read_excel("data/liggetid.xlsx")
liggetid
colnames(liggetid)


dlos <- select(liggetid, 
               c('innaar', 'alder',  'kjoenn', 
                 'kom_fra', 'slag', 'liggetid'))

# a bit processing ----
class(dlos$slag)
summary(dlos)
table(dlos$innaar, dlos$kjoenn)

colnames(dlos) <- c('admission_year', 'age', 'sex',
                    'admission_from', 'stroke', 'los')

dlos$sex <- case_when(dlos$sex == 'mann' ~ 'male', 
                      dlos$sex == 'kvinne' ~ 'female')

# kom_fra: 1 = home, 2 = div.medicine, 3 = div.surgery
# 4. other division, 5 = other hospital, 6 = nursing home

dlos$admission_from <- case_when(
  dlos$admission_from == '1' ~ 'home', 
  dlos$admission_from == '2' ~ 'div_medicine', 
  dlos$admission_from == '3' ~ 'div_surgery', 
  dlos$admission_from == '4' ~ 'div_other', 
  dlos$admission_from == '5' ~ 'other_hospital' ,
  dlos$admission_from == '6' ~ 'nursing_home'
)

# saveRDS(dlos,  file = 'data/los.rds')
los <- readRDS("~/Documents/GitHub/data-apothecary-notes/data/los.rds")
# table(dlos$admission_from)
head(los)

# initial analysis 
# https://ocbe-uio.github.io/teaching_mf9130e/lab/lab_eda_part2.html

# 7 years (1981 - 1987)
# 1981 - 1983, abbove 250days
# 1984 - 1987, less 
# should probably remove 1981, female only
# 6 types of admission 

sum(is.na(dlos$slag)) # 85 missing

# los ~ age, sex, admission, year, slag

boxplot(liggetid ~ slag, data = liggetid)


# eda ----
# remove NA
los <- filter(los, !is.na(sex) & !is.na(stroke) & !is.na(admission_from))

# code admission from with text
# unique(los$admission_from)
los$admission_from <- factor(los$admission_from, 
                            levels = c('home', 'div_surgery', 
                                       'div_medicine', 'div_other', 
                                       'other_hospital', 'nursing_home'), 
                            labels = c('home', 'div_surgery', 
                                       'div_medicine', 'div_other', 
                                       'other_hospital', 'nursing_home'))

# code admission year with text
los$admission_year <- factor(los$admission_year,
                            levels = c(1981:1987),
                            labels = as.character(1981:1987))

los$stroke <- factor(los$stroke, 
                          levels = c(0, 1), 
                          labels = c('no','yes'))

# p1 ----
plt_scat2 <- ggplot(data = los, 
                    mapping = aes(x = age, y = los, shape = sex, color = sex))
plt_scat2 <- plt_scat2 + geom_point(size = 2, alpha = 0.7)
# customize
plt_scat2 <- plt_scat2 + labs(
  x = 'Age', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay versus age'
)
plt_scat2 <- plt_scat2 + theme_bw() # make white background
# change text size
plt_scat2 <- plt_scat2 + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15)
)
# change color
plt_scat2 <- plt_scat2 + scale_color_brewer(palette = 'Set1')
plt_scat2


# p2 ----
library(ggridges)
plt_ridge <- ggplot(data = los, 
                    mapping = aes(x = los, y = admission_year, fill = sex))
plt_ridge <- plt_ridge + geom_density_ridges(alpha = 0.6) 
plt_ridge <- plt_ridge + theme_ridges()
plt_ridge <- plt_ridge + labs(
  x = 'Length of hosptial stay (days)', 
  y = 'Admission year', 
  title = 'Length of stay in each year, for each gender'
)
# change color
plt_ridge <- plt_ridge + scale_fill_brewer(palette = 'Set1')
plt_ridge


# p3 ----
plt_box <- ggplot(data = los, 
                  mapping = aes(x = admission_year, y = los, fill = sex))
plt_box <- plt_box + geom_boxplot(outlier.size = 1)
# plt_box <- plt_box + facet_wrap( ~ sex)
plt_box <- plt_box + coord_flip()

# customize
plt_box <- plt_box + theme_bw() # make white background
plt_box <- plt_box + labs(
  x = 'Admission year', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay in each year, both men and women'
)
plt_box <- plt_box + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  strip.text = element_text(size = 12)
)

plt_box <- plt_box + scale_fill_brewer(palette = 'Set1')
plt_box 



# p4 ----
plt_box2 <- ggplot(data = los, 
                   mapping = aes(x = admission_year, y = los, fill = sex))
plt_box2 <- plt_box2 + geom_boxplot(outlier.size = 0.8)
plt_box2 <- plt_box2 + facet_wrap( ~ admission_from)


# customize
plt_box2 <- plt_box2 + theme_bw() # make white background
plt_box2 <- plt_box2 + labs(
  x = 'Admission year', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay in each year, each type of admission'
)
plt_box2 <- plt_box2 + theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  strip.text = element_text(size = 12), 
  axis.text.x = element_text(angle = 45) # more readable
)

plt_box2 <- plt_box2 + scale_fill_brewer(palette = 'Set1')
plt_box2 

