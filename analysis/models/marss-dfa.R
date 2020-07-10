# remotes::install_github("fate-ewi/bayesdfa")
# remotes::install_github("nwfsc-timeseries/MARSS")
# install.packages("broom")
# library(bayesdfa)
library(MARSS)
library(dplyr)
library(tidyr)
library(broom)

# remotes::install_github("NOAA-EDAB/ecodata")
data("nefsc_survey_disaggregated", package = "ecodata")

year_n <- length(unique(nefsc_survey_disaggregated$Time))

bio <- nefsc_survey_disaggregated %>%
  # dplyr::filter(Time >= 1990) %>%
  dplyr::mutate(comname = gsub(" ", "_", comname)) %>%
  group_by(EPU, Season, comname) %>%
  dplyr::mutate(kg_tow = scale(kg.per.tow, center = TRUE, scale = TRUE)) %>%
  dplyr::select(EPU, Time, Season, comname, kg_tow)

## Work on this -- right now it is taking presence per year, should be a cutoff of stations per year
bio_n <- bio %>%
  group_by(EPU, Season, comname) %>%
  summarize(count = sum(!is.na(kg_tow)),
            prop = count/year_n) %>%
  filter(prop >= .99)
#
# bio_n_table <- bio_n %>%
#   group_by(EPU, Season) %>%
#   summarize()

epu <- "GB"
season <- "fall"

marssLoop("GB", "fall")
marssLoop("GB", "spring")


marssLoop <- function(epu, season){

  mod_path <- here::here("analysis/models/")

  sp_list <- bio_n %>%
    filter(EPU == epu,
           Season == season) %>%
    pull(comname)

  bio_wide <- bio %>%
    filter(EPU == epu,
           Season == season,
           comname %in% sp_list) %>%
    tidyr::pivot_wider(names_from = Time, values_from = kg_tow) %>%
    ungroup() %>%
    select(-EPU, -Season) %>%
    as.data.frame()

  row.names(bio_wide) <- bio_wide$comname
  bio_wide <- bio_wide[,-1]

  # set new control params
  cntl_list = list(minit = 200,
                   maxit = 10000,
                   allow.degen = FALSE)

  # set up forms of R matrices
  levels_R = c("diagonal and equal",
               "diagonal and unequal",
               "equalvarcov",
               "unconstrained")
  N_ts = 10

  model_data = data.frame()
  # fit lots of models & store results
  # NOTE: this will take a long time to run!
  for(R in levels_R) {
    for(m in 1:(N_ts)) {
      dfa_model = list(A = "zero",
                       R = R,
                       m = m)

      kemz = MARSS(as.matrix(bio_wide),
                   model = dfa_model,
                   control = cntl_list,
                   form = "dfa",
                   z.score = TRUE)

      model_data = rbind(model_data,
                         data.frame(R = R,
                                    m = m,
                                    logLik = kemz$logLik,
                                    K = kemz$num.params,
                                    AICc = kemz$AICc,
                                    stringsAsFactors = FALSE))
      assign(paste("kemz", m, R, sep="_"), kemz)
      saveRDS(kemz, sprintf("%skemz_%s_%s_%s_%s.rds", mod_path, epu, season, m, R))
    } # end m loop
  } # end R loop
  saveRDS(model_data, sprintf("%s%s_%s-model_list.rds", mod_path, epu, season))
  return(model_data)
}



