
#' Weighted mean with jackknife, returns single obs. unprocessed.
#' 
#' @param x values to average
#' @param w weights
#' @return jackknife mean estimate
#' @export
jackknife_weighted_mean <- function(x, w) {
  o <- vector(mode = 'numeric', length = length(x))
  if (length(x) == 1)
    return(x)
  for ( i in 1:length(x)) {
    o[i] <- sum(x[-i] * w[-i])/sum(w[-i])
  }
  return(mean(o))
}

#' Weighted sd with jackknife.
#' 
#' @param x values to average
#' @param w weights
#' @return jackknife sd estimate.
#' @export
jackknife_weighted_sd <- function(x, w) {
  o <- vector(mode = 'numeric', length = length(x))
  k <- length(x)
  r <- jackknife_weighted_mean(x, w)
  for ( i in 1:length(x)) {
    ri = k * r - (k - 1) * jackknife_weighted_mean(x[-i], w[-i])
    o[i] = (ri - r)^2
  }
  o <- sqrt(1/(k*(k-1)) * sum(o))
  bad <- is.na(o) | is.nan(o)
  return(o)
}



#' Produce standard DHS weighted estimates and intervals
#' for proportions.
#' 
#' @param data data frame containing required columns.
#' @param yes unquoted column name containing count of 'yes' 
#'        responses.
#' @param count unquoted column name containing count of 
#'        people asked the question.
#' @param weight survey weight
#' @param grouping character vector of column names used for grouping
#' @return data frame with columns for `estimate`, `sd`, `lb`, and
#'         `ub`, corresponding to the jackknife mean, 
#'         standard deviation, mean - 2 * sd, and mean + 2 * sd
#' @export
jackknife_estimates <- function(data, yes, count, weight,
  grouping = NULL                                
) {  
  yes = rlang::enquo(yes)
  count = rlang::enquo(count)
  weight = rlang::enquo(weight)
  if (!is.null(grouping)) 
    data = data %>% dplyr::group_by(.dots = grouping)
  data = data %>% dplyr::summarise(
    estimate = jackknife_weighted_mean((!!yes)/(!!count), (!!weight)),
    sd = jackknife_weighted_sd((!!yes)/(!!count), (!!weight)),
    lb = estimate - 2 * sd, ub = estimate + 2 * sd, 
    weight = jackknife_weighted_mean((!!weight), (!!weight))
  )
  return(data)
}


