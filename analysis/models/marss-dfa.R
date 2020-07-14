# remotes::install_github("fate-ewi/bayesdfa")
# remotes::install_github("nwfsc-timeseries/MARSS")
# install.packages("broom")
# library(bayesdfa)
library(MARSS)
library(dplyr)
library(tidyr)
library(broom)
library(doParallel)

dfaResultsClass <- function(EPU = NULL,
                            Season = NULL,
                            sp_list = NULL,
                            cov_names = NULL,
                            R = NULL,
                            m = NULL,
                            logLik = NULL,
                            K = NULL,
                            AICc = NULL,
                            runtime = NULL)
{
  me <- list(
    EPU = EPU,
    Season = Season,
    sp_list = sp_list,
    cov_names = cov_names,
    R = R,
    m = m,
    logLik = logLik,
    K = K,
    AICc = AICc,
    runtime = runtime
  )

  ## Set the name for the class
  class(me) <- append(class(me), "dfaResultsClass")
  return(me)
}



## Load survey data
nefsc_survey_disaggregated <- ecodata::nefsc_survey_disaggregated

##
bio <- nefsc_survey_disaggregated %>%
  dplyr::filter(Time >= 1982,
                Time <= 2018) %>%
  dplyr::mutate(comname = gsub(" ", "_", comname)) %>%
  group_by(EPU, Season, comname) %>%
  dplyr::mutate(kg_tow = scale(kg.per.tow, center = TRUE, scale = TRUE)) %>%
  dplyr::select(EPU, Time, Season, comname, kg_tow)

year_n <- length(unique(bio$Time))

## Work on this -- right now it is taking presence per year, should be a cutoff of stations per year
bio_n <- bio %>%
  group_by(EPU, Season, comname) %>%
  summarize(count = sum(!is.na(kg_tow)),
            prop = count/year_n) %>%
  filter(prop >= .95)

## Covariates
nao <- ecodata::nao %>%
  dplyr::select(-EPU,
                -Units,
                -Var,
                nao = Value) %>%
  expand_grid(EPU = c("GOM", "GB", "MAB"))

cold_pool <- ecodata::cold_pool %>%
  dplyr::select(-Units, -Source,
                cold_pool = Value)

surface_temp <- ecodata::seasonal_oisst_anom %>%
  dplyr::mutate(Season = gsub("? .*", "", Var),
                Season = paste0(Season, "_SST")) %>%
  dplyr::select(-Units,
         -Var) %>%
  tidyr::pivot_wider(names_from = Season, values_from = Value) %>%
  dplyr::select(-winter_SST,
                -summer_SST)

bottom_temp <- ecodata::bottom_temp %>%
  dplyr::filter(grepl("anomaly", Var),
                EPU %in% c("GOM", "GB", "MAB")) %>%
  dplyr::select(-Units) %>%
  dplyr::mutate(Var = ifelse(grepl("bottom", Var),
                             "bottom_temp",
                             "surface_temp")) %>%
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
  dplyr::select(-surface_temp)

com_dat <- ecodata::comdat %>%
  dplyr::filter(EPU %in% c("GOM", "GB", "MAB")) %>%
  group_by(EPU, Time) %>%
  summarize(total_landings = sum(Value, na.rm = TRUE))

cov_dat <- com_dat %>%
  left_join(bottom_temp,  by = c("EPU", "Time")) %>%
  left_join(surface_temp, by = c("EPU", "Time")) %>%
  left_join(cold_pool,  by = c("EPU", "Time")) %>%
  left_join(nao,  by = c("EPU", "Time")) %>%
  dplyr::filter(Time >= 1982) %>%
  group_by(EPU) %>%
  dplyr::mutate(bottom_temp = ifelse(is.na(bottom_temp),
                                     mean(bottom_temp, na.rm = TRUE),
                                     bottom_temp),
                spring_SST = ifelse(is.na(spring_SST),
                                     mean(spring_SST, na.rm = TRUE),
                                    spring_SST),
                fall_SST = ifelse(is.na(fall_SST),
                                    mean(fall_SST, na.rm = TRUE),
                                  fall_SST),
                cold_pool = ifelse(is.na(cold_pool),
                                      mean(cold_pool, na.rm = TRUE),
                                      cold_pool),
                cold_pool = ifelse(is.nan(cold_pool),
                                   NA,
                                   cold_pool)) #%>%

## Creates a data.frame of available covariate/season/EPU combination
cov_list <- cov_dat %>%
  dplyr::select(-Time) %>%
  expand_grid(Season = c("fall", "spring")) %>%
  tidyr::pivot_longer(-c(EPU, Season)) %>%
  dplyr::mutate(value = case_when(Season == "spring" & name == "fall_SST" ~ NA_real_ ,
                                   Season == "fall" & name == "spring_SST" ~ NA_real_,
                                   TRUE ~ value)) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value) %>%
  distinct(.keep_all = TRUE)

## Organize all covariate data
cov_mat <- expand_grid(EPU = c("GB", "GOM", "MAB"),
                      Season = c("fall", "spring")) %>%
  left_join(cov_dat, by = "EPU") %>%
  dplyr::mutate(fall_SST = ifelse(Season == "fall",
                                  fall_SST,
                                  NA),
                spring_SST = ifelse(Season == "spring",
                                  spring_SST,
                                  NA))

# set up forms of R matrices
levels_R = c("diagonal and equal",
             "diagonal and unequal",
             # "equalvarcov",
             "unconstrained")
N_ts = 6

mod_path <- here::here("analysis/models/")

## Data frame of models to run
mod_list <- cov_list %>%
  group_by(EPU, Season) %>%
  summarize(cov_regex = c(NA, unlist(purrr::map(1:n(), function(x) apply(combn(name, x), 2, paste, collapse='|'))))) %>%
  expand_grid(R = levels_R, m = 1:N_ts) %>%
  dplyr::mutate(cov_name = gsub("\\|", "-", cov_regex),
                file_name = sprintf("%s%s-%s-%s-%s-%s.rds", mod_path, EPU, Season, R, m, cov_name))

##

detectedCores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(detectedCores)
doParallel::registerDoParallel(cores = cl)

model_out <- foreach(i = 1:nrow(mod_list),
# model_out <- foreach(i = c(1, 19, 109, 208),
                     .combine = rbind,
                     .packages = c("MARSS")) %dopar% {

  # For i in nrow(mod_list)...
  mod_list_i <- mod_list[i,]

  epu <- mod_list_i$EPU
  season <- mod_list_i$Season
  cov_names <- unlist(strsplit(mod_list_i$cov_name, "-"))

  dfa_model = list(A = "zero",
                   R = mod_list_i$R,
                   m = mod_list_i$m)

  if(!is.na(cov_names)){
  cov_mat <- cov_dat %>%
    dplyr::filter(EPU == epu) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::one_of(cov_names)) %>%
    t()
}

  sp_list <- bio_n %>%
    dplyr::filter(EPU == epu,
                  Season == season) %>%
    dplyr::pull(comname)

  bio_wide <- bio %>%
    dplyr::filter(EPU == epu,
           Season == season,
           comname %in% sp_list) %>%
    tidyr::pivot_wider(names_from = Time, values_from = kg_tow) %>%
    dplyr::ungroup() %>%
    dplyr::select(-EPU, -Season) %>%
    as.data.frame()

  row.names(bio_wide) <- bio_wide$comname
  bio_wide <- as.matrix(bio_wide[,-1])

  ## set new control params
  cntl_list = list(minit = 200,
                   maxit = 10000,
                   conv.test.slope.tol = 0.1,
                   allow.degen = FALSE)
  start <- proc.time()
  ##
  result <- dfaResultsClass()

  if(!is.na(cov_names)){
  kemz = MARSS::MARSS(bio_wide,
                      model = dfa_model,
                      control = cntl_list,
                      form = "dfa",
                      covariates = cov_mat,
                      z.score = FALSE)
  result$cov_names = cov_names
  }
  if(is.na(cov_names)){
    kemz = MARSS::MARSS(bio_wide,
                        model = dfa_model,
                        control = cntl_list,
                        form = "dfa",
                        # covariates = cov_mat,
                        z.score = FALSE)

    result$cov_names = NA
  }

  saveRDS(kemz, mod_list_i$file_name)

  result$EPU = epu
  result$Season = season
  result$sp_list = sp_list
  result$R = dfa_model$R
  result$m = dfa_model$m
  result$logLik = kemz$logLik
  result$K = kemz$num.params
  result$AICc = kemz$AICc
  result$runtime = proc.time() - start

  mod_list_i <- NULL
  kemz <- NULL
  return(result)
}
stopCluster(cl)

saveRDS(model_out, paste0(mod_path, "model_out.rds"))

# tt <- readRDS("/net/home5/slarge/projects/soe-synthesis/analysis/models/GB-fall-diagonal and unequal-4-total_landings-bottom_temp-fall_SST.rds")

