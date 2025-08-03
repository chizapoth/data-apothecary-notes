# install.packages('pointblank')
library(pointblank)
pointblank::small_table

# Create the agent with `create_agent()`; the `tbl` is given to the agent
agent_1 <- 
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "Workshop agent No. 1",
  )

# Printing the `agent` will print the report with the default options
agent_1

# no interrogation yet
agent_1 <-
  agent_1 %>%
  col_vals_gte(columns = d, value = 0) %>%
  col_vals_in_set(columns = f, set = c("low", "mid", "high")) %>%
  col_is_logical(columns = e) %>%
  col_is_numeric(columns = d) %>%
  col_is_character(columns = c(b, f)) %>%
  rows_distinct()

agent_1

# inteerrogate
agent_1 <- agent_1 %>% interrogate()

agent_1

# validation with action level ----

# Create an `action_levels` object with the namesake function.
al <- 
  action_levels(
    warn_at = 0.15,
    stop_at = 0.25,
    notify_at = 0.35
  )

# This can be printed for inspection
al

agent_2 <-
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "Workshop agent No. 2",
    actions = al
  ) %>%
  col_is_posix(columns = date_time) %>%
  col_vals_lt(columns = a, value = 7) %>%
  col_vals_regex(columns = b, regex = "^[0-9]-[a-w]{3}-[2-9]{3}$") %>%
  col_vals_between(columns = d, left = 0, right = 4000) %>%
  col_is_logical(columns = e) %>%
  col_is_character(columns = c(b, f)) %>%
  col_vals_lt(columns = d, value = 9600) %>%
  col_vals_in_set(columns = f, set = c("low", "mid")) %>%
  rows_distinct() %>%
  interrogate()

agent_2













