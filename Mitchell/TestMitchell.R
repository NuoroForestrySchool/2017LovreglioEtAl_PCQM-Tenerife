rm(list=ls())
library(tidyverse)
source("http://math.hws.edu/pcqm/pcqm.txt")
print(PauoaFlats <- read_csv("http://math.hws.edu/pcqm/PauoaFlats.csv"), n=5)
## Sample.Pt Qtr.No Species Distance.m DBH.cm
## 1 1 1 Psidium guajava 0.7 5.5
## 2 1 2 Acacia koa 1.6 42.5
## 3 1 3 Metrosideros collina 3.5 17.0
## 4 1 4 Metrosideros tremuloides 2.0 25.0
## 5 2 1 Psidium guajava 1.1 4.0
importance.val(PauoaFlats)
## Number of sample points: n = 5
## Overall Absolute Density per Hectare (Cottam & Curtis): 3156.17
##
## Rel Density R Cover R Freq Importance R Import Abs Density
## Acacia koa 30.00 78.54 30.77 139.31 46.44 946.85
## Psidium guajava 45.00 1.89 38.46 85.35 28.45 1420.28
## Metrosideros collina 20.00 13.88 23.08 56.96 18.99 631.23
## Metrosideros tremuloides 5.00 5.69 7.69 18.38 6.13 157.81

print(lamington <- read_csv("http://math.hws.edu/pcqm/lamington.csv"), n=3)
density.est(lamington)
##
## Pollard's estimate of density using 4 sectors per point
##
## data: lamington
## No. of sample pts: n = 15
## 95 percent confidence interval:
## 1676.98 2787.47
## sample estimates:
## density per hectare
## 2160.95

