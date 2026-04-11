#' @title Optimal Swap Calculation for Uniswap V3
#'
#' @description Calculates the exact amount of tokens to swap to reach a perfect
#' asset ratio before adding liquidity to a Uniswap V3 pool (or similar)
#' based on a defined price range.
#'
#' @param price_x_usd Numeric. Price of token X in USD.
#' @param price_y_usd Numeric. Price of token Y in USD.
#' @param Pa_ratio Numeric. Lower bound of the range (expressed as amount of Y per 1 X).
#' @param Pb_ratio Numeric. Upper bound of the range (expressed as amount of Y per 1 X).
#' @param x_init Numeric. Initial amount of token X in the wallet.
#' @param y_init Numeric. Initial amount of token Y in the wallet.
#'
#' @return A list containing the details of the action to be performed:
#' \itemize{
#'   \item \code{action} : The type of swap to execute (e.g., "Swap X for Y").
#'   \item \code{amount} : The amount of tokens to swap.
#'   \item \code{amount_usd} : The USD value of the swap.
#'   \item \code{token_in} : The token to sell.
#'   \item \code{token_out} : The token to buy.
#' }
#'
#' @export
#'
#' @examples
#' # Example usage with ETH (X) and USDC (Y)
#' res <- lp_optimal_swap(
#'   price_x_usd = 3000,
#'   price_y_usd = 1,
#'   Pa_ratio = 2800,
#'   Pb_ratio = 3200,
#'   x_init = 1,
#'   y_init = 3000
#' )
lp_optimal_swap <- function(price_x_usd, price_y_usd, Pa_ratio, Pb_ratio, x_init, y_init) {

  # Relative price P used by the Uniswap pool (How many Y for 1 X)
  P <- price_x_usd / price_y_usd

  # Calculate the total wallet value in USD
  total_value_usd <- (x_init * price_x_usd) + (y_init * price_y_usd)

  cat(sprintf("=== Optimal Swap Analysis ===\n"))
  cat(sprintf("Price X: $%.2f | Price Y: $%.2f\n", price_x_usd, price_y_usd))
  cat(sprintf("Pool relative price (Y/X): %.4f\n", P))
  cat(sprintf("Pool Range (Y/X): [%.4f, %.4f]\n", Pa_ratio, Pb_ratio))
  cat(sprintf("Initial wallet: %.4f X and %.4f Y (Total Value: $%.2f)\n", x_init, y_init, total_value_usd))

  if (P <= Pa_ratio) {
    # Below the range: the pool is composed entirely of token X
    amount_usd <- y_init * price_y_usd
    cat(sprintf("-> Action: Swap Y for X\n"))
    cat(sprintf("-> Amount to Swap: %.4f Y (Value: $%.2f)\n\n", y_init, amount_usd))
    return(list(action="Swap Y for X", amount=y_init, amount_usd=amount_usd, token_in="Y", token_out="X"))

  } else if (P >= Pb_ratio) {
    # Above the range: the pool is composed entirely of token Y
    amount_usd <- x_init * price_x_usd
    cat(sprintf("-> Action: Swap X for Y\n"))
    cat(sprintf("-> Amount to Swap: %.4f X (Value: $%.2f)\n\n", x_init, amount_usd))
    return(list(action="Swap X for Y", amount=x_init, amount_usd=amount_usd, token_in="X", token_out="Y"))
  }

  # Within the range: calculating the required ratio
  x_req_factor <- (1 / sqrt(P) - 1 / sqrt(Pb_ratio))
  y_req_factor <- (sqrt(P) - sqrt(Pa_ratio))

  # Ratio R = Y_required / X_required
  R <- y_req_factor / x_req_factor

  # Formula to find the amount delta_x of token X to swap for Y
  # (or vice-versa if negative) to achieve the perfect ratio R
  delta_x <- (R * x_init - y_init) / (P + R)

  if (delta_x > 0) {
    amount_usd <- delta_x * price_x_usd
    cat(sprintf("-> Action: Swap X for Y\n"))
    cat(sprintf("-> Amount to Swap: %.4f X (Value: $%.2f)\n\n", delta_x, amount_usd))
    return(list(action="Swap X for Y", amount=delta_x, amount_usd=amount_usd, token_in="X", token_out="Y"))

  } else if (delta_x < 0) {
    delta_y <- abs(delta_x) * P
    amount_usd <- delta_y * price_y_usd
    cat(sprintf("-> Action: Swap Y for X\n"))
    cat(sprintf("-> Amount to Swap: %.4f Y (Value: $%.2f)\n\n", delta_y, amount_usd))
    return(list(action="Swap Y for X", amount=delta_y, amount_usd=amount_usd, token_in="Y", token_out="X"))

  } else {
    cat("-> Action: No swap required, perfect ratio.\n\n")
    return(list(action="None", amount=0, amount_usd=0, token_in="None", token_out="None"))
  }
}
