library(data.table)
library(magrittr)
library(ggplot2)

# raw data from ssb ----


d <- fread("https://data.ssb.no/api/v0/dataset/932937.csv?lang=en")
d
d <- d[sex=="0 Both sexes"]

# select age groups
d <- d[
  age %in% c(
    "F00-04 0-4 years",
    "F05-09 5-9 years",
    "F10-14 10-14 years",
    "F15-19 15-19 years",
    "F20-24 20-24 years",
    "F25-29 25-29 years",
    "F30-34 30-34 years",
    "F35-39 35-39 years",
    "F40-44 40-44 years",
    "F45-49 45-49 years",
    "F50-54 50-54 years",
    "F55-59 55-59 years"
  )
]
xtabs(~d$sex)
xtabs(~d$age)

d_raw <- copy(d)
colnames(d_raw)[5] <- 'deaths'
head(d_raw)

d_raw[, isoyear := as.numeric(stringr::str_extract(week, "^[0-9][0-9][0-9][0-9]"))]
d_raw


d_raw[isoyear == '2023' & age == 'F10-14 10-14 years']$deaths |> sum()

d_moregroups <- d_raw[isoyear %in% 2005:2023, .(
  deaths_n = sum(deaths)
), keyby=.(
  age, isoyear
)]

d_moregroups
xtabs(~d_moregroups$age) # 19 per group

q <- ggplot(d_moregroups, aes(x = isoyear, y = deaths_n, color = age))
q <- q + geom_line()
q + facet_wrap(~age, ncol = 4, scales = 'free')
q
?facet_grid
# exploratory smaller age groups


plot(d[age == 'F15-19 15-19 years']$`12954: Deaths per week. Preliminary figures, by sex, age, week and contents`, 
     type = 'l')



# make bigger age groups
d[, agex := fcase(
  age=="F00-04 0-4 years", "000_019",
  age=="F05-09 5-9 years", "000_019",
  age=="F10-14 10-14 years", "000_019",
  age=="F15-19 15-19 years", "000_019",
  age=="F20-24 20-24 years", "020_039",
  age=="F25-29 25-29 years", "020_039",
  age=="F30-34 30-34 years", "020_039",
  age=="F35-39 35-39 years", "020_039",
  age=="F40-44 40-44 years", "040_059",
  age=="F45-49 45-49 years", "040_059",
  age=="F50-54 50-54 years", "040_059",
  age=="F55-59 55-59 years",  "040_059"
)]

xtabs(~d$agex)

d[, isoyear := as.numeric(stringr::str_extract(week, "^[0-9][0-9][0-9][0-9]"))]
xtabs(~d$isoyear)

d <- d[isoyear %in% 2005:2023, .(
  deaths_n = sum(`12954: Deaths per week. Preliminary figures, by sex, age, week and contents`)
), keyby=.(
  agex, isoyear
)]

d
setnames(d, "agex", "age")
d


# processed data ----
# compare with the data 

x1 <- readxl::read_excel("./data/Døde 0-59 år 2000-2023 til Wilthil NRK.xlsx", range ="B3:Z6" ) %>% 
  tidyr::pivot_longer(!`...1`) %>% 
  setDT()

setnames(x1, c("pretty_age", "year", "deaths_n"))
x1
x2 <- copy(x1)

# total
x2[, pretty_age := "0-59 år"]
x2 <- x2[, .(deaths_n = sum(deaths_n)), keyby=.(pretty_age, year)]
x2
master <- rbindlist(list(x1, x2))


master[, age := fcase(
  pretty_age=="0-19 år", "000_019",
  pretty_age=="20-39 år", "020_039",
  pretty_age=="40-59 år",  "040_059",
  pretty_age=="0-59 år",  "000_059"
)]

master[, year := as.numeric(year)]
master


q <- ggplot(master, aes(x = year, y = deaths_n, color = age))
q <- q + geom_line()
q

master[
  csdata::nor_population_by_age_cats(list(0:19, 20:39, 40:59, 0:59))[location_code=="nation_nor"],
  on = c("age", "year==calyear"),
  pop_jan1_n := pop_jan1_n
]
master[, deaths_vs_pop_per_100k := 100000 * deaths_n / pop_jan1_n]

# save a copy for myself 
# mycopy <- copy(master)
# mycopy[,pretty_age := NULL]
# saveRDS(mycopy, file = 'data/mortality_2000_2023.rds')


# looks like more 40-59 than the global (0-59)
q <- ggplot(master, aes(x = year, y = deaths_vs_pop_per_100k, color = age))
q <- q + geom_line()
q



# analysis ----

p <- plnr::Plan$new()
p$add_data("data", direct = master)
p$add_argset_from_list(plnr::expand_list(
  baseline=c("2000-2019", "2005-2019", "2010-2019", "2012-2019"),
  age=c("000_019", "020_039", "040_059", "000_059")
))

action_fn <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(1)
  }
  
  pd <- data$data[age == argset$age]
  
  if(argset$baseline == "2000-2019"){
    pd <- pd[year >= 2000]
  } else if(argset$baseline == "2005-2019"){
    pd <- pd[year >= 2005]
  } else if(argset$baseline == "2010-2019"){
    pd <- pd[year >= 2010]
  } else if(argset$baseline == "2012-2019"){
    pd <- pd[year >= 2012]
  } else if(argset$baseline == "2015-2019"){
    pd <- pd[year >= 2015]
  }
  
  set.seed(10)
  fit <- rstanarm::stan_glm(
    deaths_vs_pop_per_100k ~ year, data = pd[year<=2019], 
    family = gaussian,
    iter = 20000,
    chains = 8,
    refresh = 0
  )
  pred <- rstanarm::posterior_predict(fit, pd)
  fin <- apply(pred, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as.data.frame()
  names(fin) <- c("expected_deaths_per_100k_p025", "expected_deaths_per_100k_p50", "expected_deaths_per_100k_p975")
  fin$baseline <- argset$baseline
  
  return(cbind(pd, fin))
}
p$apply_action_fn_to_all_argsets(action_fn)

raw <- p$run_all()


# present results -----
# 16 analyses

d <- rbindlist(raw)
d

d[, pretty_age := factor(pretty_age, levels = c("0-59 år", "0-19 år", "20-39 år", "40-59 år"))]

# multiply by pop
d[, expected_deaths_n_p025 := round(pop_jan1_n * expected_deaths_per_100k_p025/100000)]
d[, expected_deaths_n_p50 := round(pop_jan1_n * expected_deaths_per_100k_p50/100000)]
d[, expected_deaths_n_p975 := round(pop_jan1_n * expected_deaths_per_100k_p975/100000)]

d[, excess_deaths_n_p025 := deaths_n - expected_deaths_n_p025]
d[, excess_deaths_n_p50 := deaths_n - expected_deaths_n_p50]
d[, excess_deaths_n_p975 := deaths_n - expected_deaths_n_p975]

# as a percentage
d[, excess_deaths_pr100_p025 := 100*(excess_deaths_n_p025 / expected_deaths_n_p025)]
d[, excess_deaths_pr100_p50 := 100*(excess_deaths_n_p50 / expected_deaths_n_p50)]
d[, excess_deaths_pr100_p975 := 100*(excess_deaths_n_p975 / expected_deaths_n_p975)]

d[, alert := fcase(
  deaths_vs_pop_per_100k > expected_deaths_per_100k_p975, "Høyere enn forventet",
  default = "Forventet"
)]

d[, type := fcase(
  year <= 2019, paste0("Baseline (", baseline,")"),
  default = "Pandemiårene (2020-2023)"
)]

d$baseline |> unique()
b <- "2000-2019"

d[baseline == '2000-2019' & age == '000_019', 
  .(deaths_n, expected_deaths_n_p50,
    deaths_vs_pop_per_100k, expected_deaths_per_100k_p50)]

d[baseline == '2000-2019' & age == '020_039', 
  .(deaths_n, expected_deaths_n_p50,
    deaths_vs_pop_per_100k, expected_deaths_per_100k_p50)]

d[baseline == '2000-2019' & age == '040_059', 
  .(deaths_n, expected_deaths_n_p50,
    deaths_vs_pop_per_100k, expected_deaths_per_100k_p50)]


for(b in unique(d$baseline)){
  if(b=="2000-2019"){
    spacing <- 1
  } else {
    spacing <- 1
  }
  q <- ggplot(d[baseline==b & age=="000_059"], aes(x = year))
  q <- q + geom_ribbon(mapping = aes(ymin = expected_deaths_per_100k_p025, ymax = expected_deaths_per_100k_p975), alpha = 0.3)
  q <- q + geom_line(mapping = aes(y = expected_deaths_per_100k_p50, lty = type), size = 1)
  q <- q + geom_point(mapping = aes(y = deaths_vs_pop_per_100k, color = alert), size = 5)
  q <- q + geom_vline(xintercept = 2019.5, color = "red", lty = 2)
  #q <- q + facet_wrap(~pretty_age, ncol = 2, scales = "free_y")
  q <- q + expand_limits(y=0)
  q <- q + scale_y_continuous("Dødsfall per 100 000 innbyggere\n", expand = expansion(mult = c(0, 0.1)))
  q <- q + scale_x_continuous("\nÅr", breaks = seq(2000, 2023, spacing))
  q <- q + scale_linetype_discrete(NULL)
  q <- q + scale_color_brewer(NULL, palette = "Set1", direction = -1)
  q <- q + theme_gray(20)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
  q <- q + theme(legend.box = "horizontal", legend.margin = margin(2, 2, 2, 2))
  q <- q + labs(caption = paste0("\nDet grå området representerer 95% kredibilitetsintervall basert på ", b,"."))
  q
  ggsave(paste0("figure_",b,".jpg"), q, width = 5000, height = 3000, units = "px")
  
  to_save <- d[baseline==b & age=="000_059",.(year, age, deaths_vs_pop_per_100k, expected_deaths_per_100k_p025, expected_deaths_per_100k_p50, expected_deaths_per_100k_p975)]
  setorder(to_save, year, age)
  to_save
  
  writexl::write_xlsx(to_save, paste0("results_",b,".xlsx"))
}

q <- ggplot(d[year==2023], aes(color = baseline, x=pretty_age, y = excess_deaths_pr100_p50, ymin = excess_deaths_pr100_p025, ymax = excess_deaths_pr100_p975))
q <- q + geom_errorbar(lwd = 2, width = 0.25, position = position_dodge(width = 0.5))
q <- q + geom_point(size = 5, position = position_dodge(width = 0.5))
q <- q + expand_limits(y=0)
q <- q + scale_y_continuous("Overdødelighet (%) i 2023", expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 200, 20), labels = csstyle::format_num_as_nor_perc_0)
q <- q + scale_x_discrete("Alder")
q <- q + scale_color_brewer("Baseline", palette = "Set1", direction = -1)
q <- q + theme_gray(16)
q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
q <- q + theme(legend.box = "vertical", legend.margin = margin(2, 2, 2, 2))
q <- q + labs(caption = "\nUsikkerhetsstolper representerer 95% kredibilitetsintervall.")
q
csstyle::save_a4(q, "overdødelighet_prosent.jpg", scaling_factor = 1)


q <- ggplot(d[year==2023], aes(color = baseline, x=pretty_age, y = excess_deaths_n_p50, ymin = excess_deaths_n_p025, ymax = excess_deaths_n_p975))
q <- q + geom_errorbar(lwd = 2, width = 0.25, position = position_dodge(width = 0.5))
q <- q + geom_point(size = 5, position = position_dodge(width = 0.5))
q <- q + expand_limits(y=0)
q <- q + scale_y_continuous("Overdødelighet (antall) i 2023", expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 2000, 100))
q <- q + scale_x_discrete("Alder")
q <- q + scale_color_brewer("Baseline", palette = "Set1", direction = -1)
q <- q + theme_gray(16)
q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
q <- q + theme(legend.box = "vertical", legend.margin = margin(2, 2, 2, 2))
q <- q + labs(caption = "\nUsikkerhetsstolper representerer 95% kredibilitetsintervall.")
q
csstyle::save_a4(q, "overdødelighet_antall.jpg", scaling_factor = 1)





# nrk ----

b <- "2000-2019"
spacing <- 1

q <- ggplot(d[baseline==b & age!="000_059"], aes(x = year))
q <- q + geom_ribbon(mapping = aes(ymin = expected_deaths_per_100k_p025, ymax = expected_deaths_per_100k_p975), alpha = 0.3)
q <- q + geom_line(mapping = aes(y = expected_deaths_per_100k_p50, lty = type), size = 1)
q <- q + geom_point(mapping = aes(y = deaths_vs_pop_per_100k, color = alert), size = 5)
q <- q + geom_vline(xintercept = 2019.5, color = "red", lty = 2)
q <- q + facet_wrap(~pretty_age, ncol = 1, scales = "free_y")
q <- q + expand_limits(y=0)
q <- q + scale_y_continuous("Dødsfall per 100 000 innbyggere\n", expand = expansion(mult = c(0, 0.1)))
q <- q + scale_x_continuous("\nÅr", breaks = seq(2000, 2023, spacing))
q <- q + scale_linetype_discrete(NULL)
q <- q + scale_color_brewer(NULL, palette = "Set1", direction = -1)
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
q <- q + theme(legend.box = "vertical", legend.margin = margin(2, 2, 2, 2))
q <- q + labs(caption = paste0("\nDet grå området representerer 95% prediksjonsintervall basert på ", b,"."))
q
ggsave(paste0("dødsfall_med_baseline_",b,".jpg"), q, width = 3000, height = 5000, units = "px")

