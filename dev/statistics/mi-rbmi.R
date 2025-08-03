# test package: rbmi 
# https://insightsengineering.github.io/rbmi/main/articles/quickstart.html

d <- rbmi::antidepressant_data
d <- data.frame(d)
xtabs(~d$PATIENT) # some 4, some less

# outcome: HAMD17 (depression rating scale)

d$HAMDTL17

head(d)

# each person have different baseline
dat <- expand_locf(
  d,
  PATIENT = levels(d$PATIENT), # expand by PATIENT and VISIT 
  VISIT = levels(d$VISIT),
  vars = c("BASVAL", "THERAPY"), # fill with LOCF BASVAL and THERAPY
  group = c("PATIENT"),
  order = c("PATIENT", "VISIT")
)


nrow(d)
nrow(dat) # added 80 rows

xtabs(~dat$PATIENT) # filled in the incomplete 4 records



