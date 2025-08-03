# learn how to use gt package to make tables 

library(tidyverse)

# prep penguin data ----

penguins <- palmerpenguins::penguins |> filter(!is.na(sex))
penguins

penguin_counts <- penguins |> 
  mutate(year = as.character(year)) |> 
  group_by(species, island, sex, year) |> 
  summarise(n = n(), .groups = 'drop')
penguin_counts


penguin_counts_wider <- penguin_counts |> 
  pivot_wider(
    names_from = c(species, sex),
    values_from = n
  ) |> 
  # Make missing numbers (NAs) into zero
  mutate(across(.cols = -(1:2), .fns = ~replace_na(., replace = 0))) |> 
  arrange(island, year) 
penguin_counts_wider

# install.packages('gt')
library(gt)

# most basic table -----
gt(penguin_counts_wider)




# change colname ----

penguin_counts_wider |> 
  gt() |> 
  cols_label(
    island = 'Island',
    year = 'Year',
    Adelie_female = 'Adelie (female)',
    Adelie_male = 'Adelie (male)',
    Chinstrap_female = 'Chinstrap (female)',
    Chinstrap_male = 'Chinstrap (male)',
    Gentoo_female = 'Gentoo (female)',
    Gentoo_male = 'Gentoo (male)',
  )

# what if no pipe?
t1 <- gt(penguin_counts_wider)
t1
cols_label(t1, island = 'Island') # would have to assign to an obj


# spanner ----
# this is an overarching title 
pt_nospanner <- penguin_counts_wider |> 
  gt() |> 
  cols_label(
    island = 'Island',
    year = 'Year',
    Adelie_female = 'Adelie (female)',
    Adelie_male = 'Adelie (male)',
    Chinstrap_female = 'Chinstrap (female)',
    Chinstrap_male = 'Chinstrap (male)',
    Gentoo_female = 'Gentoo (female)',
    Gentoo_male = 'Gentoo (male)',
  )
pt_nospanner

# label added as a markdown bolded text
# option 1: specify col id
tab_spanner(pt_nospanner, label = md('**Adelie**'), 
            columns = 3:4)

# option 2: specify col names
# colname in the df, not modified table
tab_spanner(pt_nospanner, label = md('**Chinstrap**'), 
            columns = c('Chinstrap_female', 'Chinstrap_male'))
# pt_nospanner$`_data`

# rexp
tab_spanner(pt_nospanner, label = md('**Gentoo**'), 
            columns = contains('Gentoo'))



# add header ----
tab_header(pt_nospanner, 
           title = 'Penguins in the Palmer Archipelago', 
           subtitle = 'Data comes from {palmerpenguins} R package')


# grouping ----
# group based on one column
# penguin_counts_wider
gt(penguin_counts_wider, 
   groupname_col = 'island')

# also treat year as row name
gt(penguin_counts_wider, 
   groupname_col = 'island', 
   rowname_col = 'year')


# remove missing ----

gt(penguin_counts_wider, 
   groupname_col = 'island', 
   rowname_col = 'year') |> 
  sub_zero(zero_text = '-')



# add color ----
# install.packages('gtExtras')
library(gtExtras)

# color from mpg to disp
basic_use <- mtcars %>%
  head(15) %>%
  gt() %>%
  gt_color_rows(mpg:disp)

basic_use

# change palette to one that paletteer recognizes
change_pal <- mtcars %>%
  head(15) %>%
  gt() %>%
  gt_color_rows(mpg:disp, palette = "ggsci::blue_material")
change_pal

# change palette to raw values
vector_pal <- mtcars %>%
  head(15) %>%
  gt() %>%
  gt_color_rows(
    mpg:disp, palette = c("white", "green"))
vector_pal
# could also use palette = c("#ffffff", "##00FF00")

# use discrete instead of continuous palette
discrete_pal <- mtcars %>%
  head(15) %>%
  gt() %>%
  gt_color_rows(
    cyl, pal_type = "discrete",
    palette = "ggthemes::colorblind", 
    domain = range(mtcars$cyl) # from 4 to 8
  )


# use discrete and manually define range
range_pal <- mtcars %>%
  dplyr::select(gear, mpg:hp) %>%
  head(15) %>%
  gt() %>%
  gt_color_rows(
    gear, 
    pal_type = "discrete", 
    direction = -1,
    palette = "colorblindr::OkabeIto_black", 
    domain = c(3,4,5))
range_pal


# overall theme ----

gt_theme_guardian(range_pal)

opt_stylize(range_pal, style = 6, color = 'pink')


# modify specific cells ----
basic_use
tab_style_body(basic_use, 
               fn = function(x) between(x, 100, 200), 
               style = cell_text(color = 'blue', weight = 'bold'))

# is there a way to only selec one col?
tab_style(basic_use, 
          locations = cells_body(columns = hp, 
                                 rows = hp>100),
          style = cell_text(color = 'blue', weight = 'bold')) |> 
  gt_plt_bar(column = hp, keep_column = T, width = 35)



# plot with gt ----
# install.packages('svglite')

mtcars %>%
  dplyr::select(cyl:wt, mpg) %>% 
  head() %>%
  gt() %>%
  gt_plt_bar(column = mpg, keep_column = TRUE, width = 35)








