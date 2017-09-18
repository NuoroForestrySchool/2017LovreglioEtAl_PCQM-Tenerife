library(tidyverse, warn.conflicts = FALSE, verbose = FALSE, quietly = TRUE )

dbname <- "2017LovreglioEtAl_PCQM-Tenerife.sqlite"
DB <- src_sqlite(path = dbname)
Transects <- as_tibble(tbl(DB, "Transects"))
Transects <- Transects %>% left_join(as_tibble(tbl(DB, "Trees_dbh")))

# source("http://math.hws.edu/pcqm/pcqm.txt") 
# original source, documented in https://arxiv.org/abs/1010.3303
#                 pdf in https://arxiv.org/pdf/1010.3303.pdf
source("Mitchell/pcqm.txt")

# importance.val() is most interesting but accepts no vacancies!
Transects %>% 
  mutate(tr_point=paste(Id_transect, Id_point, sep="-")) %>%
  filter(Id_layer == 'L1') %>%
  select(tr_point, Id_quadrant, Species, Dist, dbh) %>%
  importance.val() %>% print()

# test density.est()  output, that has different structures depending on 'method'
Transects %>% 
  filter(Id_transect == 'A' & Id_layer == 'L2') %>%
  select(Id_transect, Id_point, Id_quadrant, Dist) %>%
  spread(key=Id_quadrant, value=Dist) %>%
  select(3:6) %>%
  density.est() -> a
str(a)
print(a)
data.frame(dens = a[['estimate']], npts = a[['parameter']][1],
           nvpts = a[['parameter']][2], conf.int.l = a[['conf.int']][1], conf.int.u = a[['conf.int']][2], row.names = NULL)

# testing the use of nest() -- not finished
compute_density <- function(x) {
  x %>%
  select(Id_point, Id_quadrant, Dist) %>%
  spread(key=Id_quadrant, value=Dist) %>%
  select(`1`, `2`, `3`, `4`) %>%
  density.est() -> z
  z$estimate
}
t <- Transects %>% 
  group_by(Id_transect, Id_layer) %>%
  nest()
map(t$data, compute_density)

# proceding with do() instead of nest()
compute_density <- function(x) {
  x %>%
    ungroup() %>%
    select(Id_transect, Id_point, Id_quadrant, Dist) %>%
    spread(key=Id_quadrant, value=Dist) %>%
    select(`1`, `2`, `3`, `4`) %>%
    density.est() -> a
  dens <- a[['estimate']][1]
  npts <- a[['parameter']][1]
  nvpts <- a[['parameter']][2]
  conf.int <- NULL
  conf.int <- if(is.null(a[['conf.int']])) rep(NA, 3) else c(attr(a$conf.int, "conf.level"), a[['conf.int']])
  data.frame(dens, npts, nvpts, conf.int.conf.level = conf.int[1], conf.int.l = conf.int[2], conf.int.u = conf.int[3], row.names = NULL)
#  data.frame(dens, npts, nvpts, row.names = NULL)
}
z <- NULL
Transects %>% 
  group_by(Id_transect, Id_layer) %>%
  do(compute_density(.)) -> z
print(z)
