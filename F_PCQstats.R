PCQstats <- function(Transect, Point=Point, Quarter=Quarter, Dist=Dist, 
                     Species=Species, Cover=Cover) {
### interface towards Mitchell's 'density.est', complete PCQ calculations
    ## {programming reference, just above: http://dplyr.tidyverse.org/articles/programming.html#different-input-variable }
  ## 0 - Density estimate (accounting for vacant quarters)
  Cover <- enquo(Cover)
  cols <- levels(as.factor(Transect$Quarter))
  a <- Transect %>%
    ungroup() %>%
    select(Point, Quarter, Dist) %>%
    spread(key=Quarter, value=Dist) %>%
    select(cols) %>%
    density.est()                ### hart of the function!! Mitchell source
  dens <- a[['estimate']][1]
  method <- a$method
  npts <- a[['parameter']][1]
  nvpts <- a[['parameter']][2]
  conf.int <- NULL
  conf.int <- if(is.null(a[['conf.int']])) rep(NA, 3) else 
    c(attr(a$conf.int, "conf.level"), a[['conf.int']])
  
  ## 1 - Compute relative and absolute 'Density partition by species', 
  #   (if vacant Quarters are computed as 0 frequency, the partition can be incomplete, sum<tot)
  # n. of quarters where Species <i> occurs
  PCQstats <- Transect %>% 
    group_by(Species) %>% 
    dplyr::summarise(nQwSp = n())
  # n. of Q. with Obs by Layer  (Vacancies complement = nPtsByL*4 - vacancies)
  nQwObs <- sum(PCQstats$nQwSp)
  # Relative and absolute Density Partition(*) by Species
  PCQstats <-PCQstats %>%
    mutate(relDensBySp = 100 * nQwSp / nQwObs, 
           densPartBySp = dens * relDensBySp / 100)
  
  ## 2 - Compute cover
  Dominance <- Transect %>% 
    group_by(Species) %>%
    dplyr::summarise(meanCover = mean(!! Cover))
  PCQstats <- PCQstats %>%
    full_join(Dominance) %>%
    mutate(Dominance = meanCover * densPartBySp)
  sumDom <- PCQstats %$%  sum( Dominance)
  PCQstats <- PCQstats %>%
    full_join(Dominance) %>%
    mutate(rDominance = 100* Dominance / sumDom)
  
  ## 3 - Compute relative and absolute 'species frequency spread' 
  # "The absolute frequency of a species is the 
  #      percentage of sample points at which a species occurs."
  # freq = n. of points where Species <i> occurs
  rawSpFrq <- Transect %>% 
    distinct(Point, Species) %>% 
    group_by(Species) %>% 
    dplyr::summarise(nPwSp = n())
  # <npts> computed by <density.est()>
  PCQstats <-PCQstats %>%
    full_join(rawSpFrq) %>%
    mutate(absFrq = 100 * nPwSp / npts)
  # "To normalize for the fact that the absolute frequencies sum to more than 100%, 
  #       the relative frequency  is computed"
  sumOFabsFreq <- PCQstats %$% sum(absFrq)
  PCQstats <- PCQstats %>%
    mutate(relFrq = 100 * absFrq / sumOFabsFreq)
  attributes(PCQstats)$density.est.details <- data.frame(dens, npts, nvpts, conf.int.conf.level = conf.int[1], conf.int.l = conf.int[2], conf.int.u = conf.int[3], method, row.names = NULL)
#  attributes(PCQstats)$density.est.method <- method
  return(PCQstats)
}

# debugging extras
if(F) {
aa <- Transects %>%
  mutate(Cov = CrownDiam1 * CrownDiam2 * pi/4) %>%
  filter(Id_layer == 'L1') %>%
  PCQstats(Cover=Cov)
attributes(aa)

debugonce(PCQstats)
}