# setup
suppressPackageStartupMessages(library("dplyr"))
packageVersion("dplyr")
# library(dplyr)
# quo is a tidy eval concept for quoting
grp_var <-quo(Species)
voi <- quo(Sepal.Length)
# use !! another tidy eval concept to unquote
dmp <- iris %>%
  select(!! grp_var, !! voi) %>% 
  group_by(!! grp_var) %>%
  summarise(Median_Value = median( !! voi ), Count = n())
dmp
