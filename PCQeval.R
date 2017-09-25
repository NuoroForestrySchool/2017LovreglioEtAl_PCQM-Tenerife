library(Rmisc)
# 'tidyverse' carica 'dplyr', se viene prima di 'Rmisc', genera questo avviso:
#### --------------------------------------------------------------------
####   You have loaded plyr after dplyr - this is likely to cause problems.
#### If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
####   library(plyr); library(dplyr)
#### --------------------------------------------------------------------
library(tidyverse, warn.conflicts = FALSE, verbose = FALSE, quietly = TRUE )
library(rlang)
library(magrittr)
# source("http://math.hws.edu/pcqm/pcqm.txt") 
# original source, documented in https://arxiv.org/abs/1010.3303
#                 pdf in https://arxiv.org/pdf/1010.3303.pdf


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

source("F_PCQstats.R")
PCQtabByLyTr <- Transects %>%
  mutate(Cov = CrownDiam1 * CrownDiam2 * pi/4) %>%
  group_by(Id_layer, Id_transect) %>%
  do(PCQstats(., Cover=Cov))

PCQtabByLy <- Transects %>%
  mutate(Cov = CrownDiam1 * CrownDiam2 * pi/4) %>%
  group_by(Id_layer) %>%
  do(PCQstats(., Cover=Cov))

PCQtabByLyTr %>%
  group_by(Id_layer, Species) %>%
  summarise(l_dens = mean(densPartBySp)) %>%
  full_join(PCQtabByLy[,c(1,2,5)]) %>%
  mutate(r= l_dens / densPartBySp)

PCQtabByLyTr %>%
  group_by(Id_transect, Id_layer, Species) %>%
  summarise(densBySp = mean(densPartBySp)) %>%
  group_by(Id_transect, Id_layer) %>%
  summarise(dens = sum(densBySp)) %>%
  select(Id_transect, Id_layer, dens) %>%
  spread(Id_layer, dens)
