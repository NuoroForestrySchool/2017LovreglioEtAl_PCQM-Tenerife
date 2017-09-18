library(tidyverse, warn.conflicts = FALSE, verbose = FALSE, quietly = TRUE )
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
CoverLbl <- "crown area [m^2]"
PointsBySubset <- Transects %>% 
  mutate(SubSetK = Id_layer, Point = paste0(Id_transect, Id_point),
         Cover = CrownDiam1*CrownDiam2*pi/4) %>%
  select(SubSetK, Point, Quarter = Id_quadrant, Species, Dist, Cover)

## 1 - Density estimate (accounting for vacant quarters)
DensByS <- PointsBySubset %>% group_by(SubSetK) %>% do(compute_density(.))
# includes, for each subset:  nvpts = n. of points, nvpts = n. points with no vacant Quarters, CI

## 2 - Compute relative and absolute 'Density partition by species', 
#   (if vacant Quarters are computed as 0 frequency, the partition can be incomplete, sum<tot)

# SHOW: n. of quarters where Species <i> occurs, by subset and Point
PointsBySubset %$% table(Species, Point, SubSetK)
# n. of quarters where Species <i> occurs
nQwSpByS <- PointsBySubset %>% group_by(SubSetK, Species) %>% summarise(nQwSp = n())

# SHOW n. of Q. with Obs by subset
PointsBySubset %>% group_by(SubSetK, Point) %>% summarise(nQ = n())%>% 
  ungroup() %>% select(SubSetK, Point, nQ) %>% spread(Point, nQ) %>% as.data.frame()
# n. of Q. with Obs by Layer  (Vacancies complement = nPtsByL*4 - vacancies)
nQwObsByS <- PointsBySubset %>% group_by(SubSetK) %>% summarise(nQ = n())

# Relative and absolute Density Partition(*) by Species
PCQMtab <- DensByS %>% select(SubSetK, dens) %>% 
  left_join(nQwSpByS) %>% left_join(nQwObsByS) %>%
  mutate(relDensBySp = nQwSp/nQ, densPartBySp = dens*relDensBySp) %>%
  select(SubSetK,Species, relDensBySp, densPartBySp)

## 3 - Compute cover
Dominance <- PointsBySubset %>% 
  group_by(SubSetK) %>%
#   summarise(meanCover = mean(.$Cover))  # doesn't compute by group!
  do(data.frame(meanCover = mean(.$Cover)))
PCQMtab <- PCQMtab %>%
  left_join(Dominance) %>%
  mutate(Dominance = meanCover * densPartBySp)


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

