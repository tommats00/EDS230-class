#' highflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param high_flow_months which to use default (August 8)
#' @param wts (vector of 4 for annual_min_err, annual_min_corr, high_month_cor, high_month_err)
#' @param max_err_annual_min
#' @param max_err_high_month
#' @return annual_min_err, annual_min_corr, high_month_cor, high_month_err


compute_highflowmetrics_all <- function(m, o, month, day, year, wy, high_flow_months = 5,
                                       max_err_annual_max = NULL, max_err_high_month = NULL, wts = c(0.25, 0.25, 0.25, 0.25)) {
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  # first lets get minimum yearly values
  
  tmp <- flow %>%
    group_by(wy) %>%
    dplyr::summarize(maxo = max(o), maxm = max(m))
  
  annual_max_err <- mean(tmp$maxm - tmp$maxo)
  
  annual_max_cor <- cor(tmp$maxm, tmp$maxo)
  
  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_annual_max)) {
    max_err_annual_max <- 0.5 * mean(tmp$maxo)
  }
  
  # now lets get monthly values
  tmp <- flow %>%
    group_by(month, year) %>%
    dplyr::summarize(model = sum(m), obs = sum(o))
  # now extract august
  high <- subset(tmp, month %in% high_flow_months)
  high_month_err <- mean(high$model - high$obs)
  high_month_cor <- cor(high$model, high$obs)
  
  # if user doesn't specify maximum errors use 50% of mean observed values
  if (is.null(max_err_high_month)) {
    max_err_high_month <- 0.5 * mean(high$obs)
  }
  
  # transform errors to be between 0 and 1
  annual_max_err_trans <- max(0, (1 - abs(annual_max_err / max_err_annual_max)))
  high_month_err_trans <- max(0, (1 - abs(high_month_err / max_err_high_month)))
  
  # apply weight (normalize in case they don't sum to 1)
  wts <- wts / sum(wts)
  
  combined <- wts[1] * annual_max_err_trans + wts[2] * annual_max_cor +
    wts[3] * high_month_cor + wts[4] * high_month_err_trans
  
  return(list(
    annual_max_err = annual_max_err, annual_max_cor = annual_max_cor, high_month_err = high_month_err,
    high_month_cor = high_month_cor, combined = combined
  ))
}