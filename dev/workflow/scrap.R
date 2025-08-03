# install.packages('rvest')

library(rvest)
library(magrittr)
library(stringr)
library(purrr)
stringr::str_c()

website <- "https://www.berlin.de/polizei/polizeimeldungen/archiv/" %>% 
  read_html()

web_ema <- read_html("https://www.ema.europa.eu/en/medicines/field_ema_web_categories%253Aname_field/Human/ema_group_types/ema_medicine")

web_ema %>% html_elements(css = "div.textile a[title^='Link']")


links <- website %>% 
  html_elements(css = "div.textile a[title^='Link']") %>% 
  html_attr(name = "href") %>% 
  str_c("https://www.berlin.de", .)
links



max_pages <- links %>% 
  purrr::map(~ {
    Sys.sleep(2)
    read_html(.x)
  }) %>%
  purrr::map(html_element, css = "li.pager-item.last > a") %>% 
  purrr::map(html_text) %>% 
  as.numeric()
max_pages 

pag_links <- character()
# pagination
for (i in 1:length(links)) {
  for (j in 1:max_pages[i]) {
    pag_links <- append(pag_links, values = str_c(links[i], "?page_at_1_0=", j))
  }
}

# if explicit, i can track the progress
pages <- pag_links %>% 
  map(~ {
    Sys.sleep(2)
    read_html(.x)
  })



