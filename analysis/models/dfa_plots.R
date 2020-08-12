library(dplyr)
library(purrr)
library(MARSS)
library(tidyr)
library(ggplot2)
library(ggrepel)

mod_path <- here::here("analysis/models")
all_out <- tibble(file_path = list.files(mod_path))


mod_list <- all_out %>%
  # head(20) %>%
  filter(!grepl("^kemz.*|model_out.*|marss-dfa.*|GB_fall-model_list.*", file_path)) %>%
  mutate(out = gsub(".rds", "", file_path),
         file_path = here::here("analysis/models", file_path)) %>%
  separate(col = out, sep = "-", into = c("EPU", "Season", "R", "m",
                                          "covariateA", "covariateB", "covariateC", "covariateD"),
           fill = "right",
           remove = TRUE) %>%
  mutate(covariateA = ifelse(covariateA == "NA",
                             NA,
                             covariateA))

sp_guild <- ecodata::nefsc_survey_disaggregated %>%
  mutate(comname = gsub(" ", "_", comname)) %>%
  select(comname, `Feeding guild`) %>%
  filter(comname %in% sp_names) %>%
  distinct(.keep_all = TRUE)

mod_out <- mod_list %>%
  filter(EPU == "GB", Season == "fall") %>%
  mutate(AICc = purrr::map(file_path, function(x) readRDS(x)$AICc)) %>%
  unnest(AICc) %>%
  arrange(AICc)


mod_plot <- mod_out %>%
  filter(R != "unconstrained") %>%
  mutate(delta_AICc = AICc - min(AICc),
         wt = exp(-0.5*delta_AICc),
         Ak_wt = wt/sum(wt),
         Ak_wt_cum = cumsum(Ak_wt),
         n_covariate = rowSums(!is.na(select(., starts_with("cov"))))) %>%
  pivot_longer(cols = starts_with("cov"), names_to = "all_cov") %>%
  select(-all_cov) %>%
  mutate(value = ifelse(n_covariate == 0,
                        "none", value),
         best_model = ifelse(AICc == min(AICc),
                             "Best Model",
                             NA)) %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(value))

epu = unique(mod_plot$EPU)
season = unique(mod_plot$Season)

ggplot() +
  geom_point(data = mod_plot %>%  filter(n_covariate <= 1),
             aes(x = m, y = delta_AICc, shape = R, color = value)) +
  geom_point(data = mod_plot %>%  filter(n_covariate >= 2),
             aes(x = m, y = delta_AICc, shape = R, color = value), position = position_jitter(w = 0.4, h = 0.0)) +
  geom_label_repel(
    data = mod_plot %>% filter(!is.na(best_model)), aes(x = m, y = delta_AICc), label = "Best Model",
    force = 100,
  ) +
  facet_wrap(~n_covariate) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "DFA model selection",
       subtitle = sprintf("%s in the %s", epu, season),
       color = "Covariate",
       y = "\u0394 AICc") +
  theme_bw()


best_mod <- mod_plot %>%
  filter(!is.na(best_model)) %>%
  pull(file_path) %>%
  readRDS()

## Trends
d <- tidy(best_mod, type="states")
ggplot(data = d) +
  geom_line(aes(t, estimate)) +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  facet_grid(~term) +
  labs(x = "Time Step",
       y = "Val") +
  theme_bw()

## Loadings

## get the estimated ZZ
Z_est <- coef(best_mod, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
colnames(Z_rot) <- paste0("Trend_", 1:ncol(Z_rot))

load_plot <- data.frame(Z_rot) %>%
  mutate(comname = sp_names) %>%
  pivot_longer(-comname, names_to = "Trend", values_to = "val") %>%
  left_join(sp_guild, by = "comname") %>%
  mutate(val = ifelse(abs(val) <= 0.05,
                      0,
                      val),
         comname = as.factor(comname))

guild_pal <- RColorBrewer::brewer.pal(length(unique(load_plot$`Feeding guild`)), name = "Dark2")

ggplot(data = load_plot, aes(x = comname, y = 0, xend = comname, yend = val, color = `Feeding guild`)) +
  geom_point() +
  geom_segment()+
  facet_wrap(~Trend) +
  coord_flip() +
  scale_color_manual(values = guild_pal) +
  scale_x_discrete(limits = rev(levels(load_plot$comname))) +
  theme(axis.text.y = element_text(angle = 0, hjust = 0)) +
  theme_bw()


mod_fits <- get_dfa_fits(best_mod)

sp_names <- rownames(best_mod$call$data)

ex <- data.frame(sp_names,
                 mod_fits$ex, stringsAsFactors = FALSE)
colnames(ex) = c("comname", colnames(best_mod$call$data))
ex <- ex %>%
  pivot_longer(-comname, names_to = "Time", values_to = "est")

up <- data.frame(sp_names,
                 mod_fits$up, stringsAsFactors = FALSE)
colnames(up) = c("comname", colnames(best_mod$call$data))
up <- up %>%
  pivot_longer(-comname, names_to = "Time", values_to = "up")

lo <- data.frame(sp_names,
                 mod_fits$lo, stringsAsFactors = FALSE)
colnames(lo) = c("comname", colnames(best_mod$call$data))
lo <- lo %>%
  pivot_longer(-comname, names_to = "Time", values_to = "lo")

raw <- data.frame(best_mod$marss$data) %>%
  tibble::rownames_to_column(var = "comname") %>%
  pivot_longer(-comname, names_to = "Time", values_to = "raw") %>%
  mutate(Time = gsub("X", "", Time))

mod_fits <- ex %>%
  left_join(up, by = c("comname", "Time")) %>%
  left_join(lo, by = c("comname", "Time")) %>%
  left_join(raw, by = c("comname", "Time")) %>%
  mutate(Time = as.numeric(Time))


ggplot(data = mod_fits, aes(x = Time, y = est, ymin = lo, ymax = up)) +
  geom_ribbon(color = "grey60", alpha = 0.1) +
  geom_line() +
  geom_point(data = mod_fits, aes(x = Time, y = raw), color = "blue", alpha = .5) +
  facet_wrap(~comname) +
  theme_bw()


get_dfa_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*%
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}



