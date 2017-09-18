library(tidyverse, warn.conflicts = FALSE, verbose = FALSE, quietly = TRUE )
library(rlang)
library(magrittr)
library(Rmisc)
# source("http://math.hws.edu/pcqm/pcqm.txt") 
# original source, documented in https://arxiv.org/abs/1010.3303
#                 pdf in https://arxiv.org/pdf/1010.3303.pdf
source("Mitchell/pcqm.txt")


dbname <- "2017LovreglioEtAl_PCQM-Tenerife.sqlite"
DB <- src_sqlite(path = dbname)
Transects <- as_tibble(tbl(DB, "Transects")) %>% 
  left_join(as_tibble(tbl(DB, "Interventions"))) %>% 
  left_join(as_tibble(tbl(DB, "VertStr"))) %>% 
  left_join(as_tibble(tbl(DB, "Species"))) %>% 
  left_join(as_tibble(tbl(DB, "Trees_dbh"))) %>% 
  mutate(
    AntiErosionIntervention = factor(
      paste(Id_transect,AntiErosionIntervention, sep="-")),
    VertLayer = factor(layer),
    Sp = Species,
    Species = factor(Species_name),
    Point = factor(paste0(.$Id_transect,.$Id_point)),
    Quarter = factor(Id_quadrant)
  )
Transects$AntiErosionIntervention <- 
  factor(Transects$AntiErosionIntervention, 
         levels = rev(sort(levels(Transects$AntiErosionIntervention))))

compute_density <- function(x) {
  a <- x %>%
    ungroup() %>%
    select(Point, Quarter, Dist) %>%
    spread(key=Quarter, value=Dist) %>%
    select(`1`, `2`, `3`, `4`) %>%
    density.est()
  dens <- a[['estimate']][1]
  npts <- a[['parameter']][1]
  nvpts <- a[['parameter']][2]
  conf.int <- NULL
  conf.int <- if(is.null(a[['conf.int']])) rep(NA, 3) else c(attr(a$conf.int, "conf.level"), a[['conf.int']])
  ## add also Method!!
  data.frame(dens, npts, nvpts, conf.int.conf.level = conf.int[1], conf.int.l = conf.int[2], conf.int.u = conf.int[3], row.names = NULL)
}

### Processing PCQM within each subset of observations
#     e.g., grouped by:  A) LayerPoolingAllTransects or b) LayerInTreatment

###  A) Subset = Layer (pooling thogheter transects with different interventions)
sbs_var <- quo(VertLayer)
cover_var <- quo(CrownArea)
cover_lbl <- "crown area [m^2]"
Points <- Transects %>% 
  mutate(Point = paste0(Id_transect, Id_point),
         CrownArea = CrownDiam1*CrownDiam2*pi/4) %>%
  select( !! sbs_var, Point, Quarter = Id_quadrant, Species, Dist, !! cover_var)

## 1 - Density estimate (accounting for vacant quarters)
DensBySbs <- Points %>% group_by(!! sbs_var) %>% do(compute_density(.))
# includes, for each subset:  nvpts = n. of points, nvpts = n. points with no vacant Quarters, CI

## 2 - Compute relative and absolute 'Density partition by species', 
#   (if vacant Quarters are computed as 0 frequency, the partition can be incomplete, sum<tot)

# SHOW: n. of quarters where Species <i> occurs, by subset and Point
Points %$% table(Species, Point, eval_tidy(sbs_var, .))
# n. of quarters where Species <i> occurs
nQwSpByS <- Points %>% 
  group_by(!! sbs_var, Species) %>% 
  dplyr::summarise(nQwSp = n())

# SHOW n. of Q. with Obs by subset
Points %>% group_by(!! sbs_var, Point) %>% dplyr::summarise(nQ = n())%>% 
  ungroup() %>% select(!! sbs_var, Point, nQ) %>% spread(Point, nQ) %>% as.data.frame()
# n. of Q. with Obs by Layer  (Vacancies complement = nPtsByL*4 - vacancies)
nQwObsByS <- Points %>% group_by(!! sbs_var) %>% dplyr::summarise(nQ = n())

# Relative and absolute Density Partition(*) by Species
PCQMtab <- DensBySbs %>% select(!! sbs_var, dens) %>% 
  left_join(nQwSpByS) %>% left_join(nQwObsByS) %>%
  mutate(relDensBySp = 100 * nQwSp / nQ, densPartBySp = dens * relDensBySp) %>%
  select(!! sbs_var,Species, relDensBySp, densPartBySp)

## 3 - Compute relative and absolute 'species frequency spread' 
# "The absolute frequency of a species is the 
#      percentage of sample points at which a species occurs."

# SHOW: points where Species <i> occurs
Points %$% table(Species, Point, eval_tidy(sbs_var, .))>0

# freq = n. of points where Species <i> occurs
rawSpFrq <- Points %>% 
  distinct(!! sbs_var, Point, Species) %>% 
  group_by(!! sbs_var, Species) %>% 
  dplyr::summarise(nPwSp = n())
SubSets <- Points %>% 
  distinct(!! sbs_var, Point) %>% 
  group_by(!! sbs_var) %>% 
  dplyr::summarise(nPts = n())
PCQMtab <- PCQMtab %>%
  left_join(rawSpFrq) %>%
  left_join(SubSets) %>%
  mutate(absFrq = 100 * nPwSp / nPts)

# "To normalize for the fact that the absolute frequencies sum to more than 100%, 
#       the relative frequency  is computed"

sumOFabsFreqByS <- PCQMtab %>%
  group_by(!! sbs_var) %>%
  dplyr::summarise(SumOfFrq = sum(absFrq))

PCQMtab <- PCQMtab %>%
  left_join(sumOFabsFreqByS) %>%
  mutate(relFrq = 100 * absFrq / SumOfFrq) %>%
  select(-nPwSp,  -nPts,  -absFrq, -SumOfFrq)

###---

## 3 - Compute cover
Dominance <- Points %>% 
  group_by(!! sbs_var, Species) %>%
  dplyr::summarise(meanCover = mean(!! cover_var))

PCQMtab <- PCQMtab %>%
  left_join(Dominance) %>%
  mutate(Dominance = meanCover * densPartBySp)

DominanceBySbs <- PCQMtab %>%
  group_by(!! sbs_var) %>%
  dplyr::summarise(sumDom = sum( Dominance))

PCQMtab <- PCQMtab %>%
  left_join(DominanceBySbs) %>%
  mutate(rDominance = 100* Dominance / sumDom) %>%
  select(-sumDom)



ggplot(Transects, aes(VertLayer, Height)) + 
  geom_boxplot(aes(colour = AntiErosionIntervention)) + 
  guides(col = guide_legend(reverse=TRUE)) +
  scale_y_log10() + 
  labs(x="Vertical layer", y="Height [m]", title="Distribution of vegetation heigth") +
  coord_flip()
ggplot(Transects, aes(CrownDiam1, CrownDiam2)) + 
  geom_point(aes(shape = AntiErosionIntervention, 
                 col = VertLayer)) + 
  guides(col = guide_legend(reverse=TRUE)) +
  guides(shape = guide_legend(reverse=TRUE)) +
  labs(title="Crown dimensions")

Transects$AntiErosionIntervention <- 
  factor(Transects$AntiErosionIntervention, 
         levels = (sort(levels(Transects$AntiErosionIntervention))))
ggplot(Transects, aes(CrownDiam1, CrownDiam2)) + 
  geom_point(aes(col = VertLayer)) + 
  guides(col = guide_legend(reverse=TRUE)) +
  facet_wrap(~AntiErosionIntervention) +
  scale_x_log10() + scale_y_log10() +
  labs(title="Crown dimensions: cross diameters")

Transects <- Transects %>% mutate(CrownArea = CrownDiam1*CrownDiam2*pi/4)
ggplot(Transects, aes(Height, CrownArea)) +
  geom_point(aes(col = VertLayer)) + 
  guides(col = guide_legend(reverse=TRUE)) +
  facet_wrap(~AntiErosionIntervention) +
  scale_x_log10() + scale_y_log10() +
  labs(title="Crown dimensions: area ~ tree.height")

