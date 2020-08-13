
## Helper function to extract DFA fits ##
get_dfa_fits <- function(MLEobj, dd = NULL, alpha = 0.05, return_tidy = TRUE) {
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
  fits$raw <- MLEobj$marss$data
  if(return_tidy){
    colnames(fits[["ex"]]) <-  colnames(MLEobj$call$data)
    colnames(fits[["up"]]) <-  colnames(MLEobj$call$data)
    colnames(fits[["lo"]]) <-  colnames(MLEobj$call$data)
    colnames(fits[["raw"]]) <-  colnames(MLEobj$call$data)

    fits <- purrr::map_df(fits, ~ as.data.frame(.x), .id = "id") %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(comname =  rownames(MLEobj$call$data)) %>%
      tidyr::pivot_longer(-c(comname, id), names_to = "Time", values_to = "val") %>%
      tidyr::pivot_wider(names_from = id, values_from = val) %>%
      dplyr::mutate(Time = as.numeric(Time))
    return(fits)
  }
  if(!return_tidy){
    return(fits)
  }
}



matrix_ci <- function(MLEobjCI, var = c("up", "lo", "raw")) {

  # MLEobjCI <- MARSSparamCIs(MLEobj, ...)
  ## Needs to be after MARSSparamCI

  up = coef(MLEobjCI, what = "par.upCI")$Z
  lo = coef(MLEobjCI, what = "par.lowCI")$Z
  raw = coef(MLEobjCI, what = "par")$Z
  ci_list <- list(up = up, lo = lo, raw = raw)

  f = MLEobjCI$marss$fixed$Z
  d = MLEobjCI$marss$free$Z

  dims = attr(MLEobjCI$marss, "model.dims")$Z
  par_names <- rownames(MLEobjCI$call$data)

  delem = d
  attr(delem, "dim") = attr(delem, "dim")[1:2]

  felem = f
  attr(felem, "dim") = attr(felem, "dim")[1:2]
  x = 1
  par_mat <- lapply(1:3, function(x) matrix(felem + delem %*% ci_list[[x]],
                                            dims[1], dims[2]))
  names(par_mat) <- names(ci_list)

  return(par_mat[[var]])
  # purrr::map_df(par_mat, ~ as.data.frame(.x), .id = "id") %>%
  #   dplyr::group_by(id) %>%
  #   dplyr::mutate(comname =  par_names) %>%
  #   tidyr::pivot_longer(-c(comname, id), names_to = "trend", values_to = "val") %>%
  #   tidyr::pivot_wider(names_from = id, values_from = val) %>%
  #   dplyr::mutate(trend = gsub("V", "Trend_", trend))

}
