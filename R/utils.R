#' Convert price to tick
#'
#' Calculates the tick index corresponding to a given price using
#' the standard base 1.0001 logarithmic formula of Uniswap V3.
#'
#' @param p Numeric. The price to convert.
#'
#' @return A numeric value representing the tick (unrounded).
#' @export
#'
#' @examples
#' price_to_tick(1.15)
price_to_tick <- function(p) {
  log(p) / log(1.0001)
}


#' Convert tick to price
#'
#' Calculates the exact price corresponding to a specific tick index.
#'
#' @param i Integer or Numeric. The tick index [-887272 ; 887272].
#'
#' @return A numeric value representing the price.
#' @export
#'
#' @examples
#' tick_to_price(1397)
tick_to_price <- function(i) {
  1.0001^i
}


#' Get bounding prices based on fee tier
#'
#' Given a target price and a pool fee tier, this function determines
#' the applicable tick spacing and returns the prices corresponding
#' to the valid ticks immediately below and above the given price.
#'
#' @param p Numeric. The current or target price.
#' @param fee Numeric or Character. The pool fee tier
#'   (accepted values: 100, 500, 3000, 10000).
#'
#' @return A numeric vector of length 2 containing the lower bound price
#'   and the upper bound price.
#' @export
#'
#' @examples
#' price_to_ticks(p = 1500, fee = 500)
#' price_to_ticks(p = 1500, fee = 3000)
price_to_ticks <- function(p, fee) {
  fee_dict <- list("100" = 1, "500" = 10, "3000" = 60, "10000" = 200)
  tick_spacing <- fee_dict[[as.character(fee)]]
  tick_start <- price_to_tick(p) |> floor()

  return(c(
    tick_to_price(tick_start),
    tick_to_price(tick_start + tick_spacing)
  ))
}
