# this script is solely for testing purposes
library(tidyverse)
library(DBI)

the_data <- read_csv("data_big_mp.csv", guess_max = 5000)

con <- dbConnect(RSQLite::SQLite(), dbname = "data.db")
dbListTables(con)
dbWriteTable(con, "the_data", the_data)

db_table <- tbl(con, "data")
db_table

#die parameter die zu ändern wären sind jetzt gap1, gap2, stdpercent, und T
# Tc und corr ist erstmal alles gleich
# 
# stdpercent ist leider momentan auch mal alles 0, aber das ist alles was drin ist bei einem 100 MB csv file
# 
# q gegen bias, 
# samegap1 gegen samegap1_bias
# samegap2 gegen samegap2_bias
# 
# bias ist immer x


db_table %>% 
  summarise(max_gap1 = max(gap1),
            min_gap1 = min(gap1),
            max_gap2 = max(gap2),
            min_gap2 = min(gap2),
            max_std = max(stdpercent),
            min_std = min(stdpercent),
            max_t = max(T),
            min_t = min(T))


sample <- db_table %>% 
  filter(gap1 == 1,
         gap2 == 1) %>% 
  collect()

sample %>% 
  select(contains("bias"), q, samegap1, samegap2) %>%
  pivot_longer(contains("bias"), names_to = "x_name", values_to = "x_val") %>% 
  pivot_longer(q:samegap2, names_to = "y_name", values_to = "y_val") %>% 
  distinct() %>% 
  ggplot(aes(x_val, y_val, colour = y_name)) +
  geom_jitter() +
  labs(x = "bias")


DBI::dbDisconnect(con)


