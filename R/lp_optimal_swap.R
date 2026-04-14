#' @title Optimal Swap Calculation for Uniswap V3
#'
#' @description Calculates the exact amount of tokens to swap to reach a perfect
#' asset ratio before adding liquidity to a Uniswap V3 pool (or similar)
#' based on a defined price range.
#'
#' @param price_x_usd Numeric. Price of token X in USD.
#' @param price_y_usd Numeric. Price of token Y in USD.
#' @param min_range Numeric. Lower bound of the range (expressed as amount of Y per 1 X).
#' @param max_range Numeric. Upper bound of the range (expressed as amount of Y per 1 X).
#' @param x_init Numeric. Initial amount of token X in the wallet.
#' @param y_init Numeric. Initial amount of token Y in the wallet.
#' @param usd_init  Numeric. Initial amount of USD to invest (Optional).
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
#' @examples
#' lp_optimal_swap(
#'   price_x_usd = 3553,
#'   price_y_usd = 1.1656,
#'   min_range = 2800,
#'   max_range = 3200,
#'   x_init = 1,
#'   y_init = 2000,
#'   usd_init = 0)
#'
#' lp_optimal_swap(
#'   price_x_usd = 3553,
#'   price_y_usd = 1.1656,
#'   min_range = 2800,
#'   max_range = 3200,
#'   x_init = 0,
#'   y_init = 0,
#'   usd_init = 5884.20)
#'
#' @export
lp_optimal_swap <- function(price_x_usd, price_y_usd, min_range, max_range, x_init = 0, y_init = 0, usd_init = 0) {

  # Sanity check
  if (min_range >= max_range) {
    stop(sprintf("Wrong Pool Ranges (Y/X): [%.4f, %.4f]\n", min_range, max_range))
  }

  if (x_init == 0 && y_init == 0 && usd_init == 0) {
    stop("You must provide an initial balance greater than 0 for either x_init, y_init, or usd_init.")
  }

  # Relative price P used by the Uniswap pool (How many Y for 1 X)
  P <- price_x_usd / price_y_usd

  # Calculate the total wallet value in USD
  total_usd_value <- (x_init * price_x_usd) + (y_init * price_y_usd) + usd_init

  cat(sprintf("=== Optimal Swap Analysis ===\n"))
  cat(sprintf("Price X: $%.6f | Price Y: $%.6f\n", price_x_usd, price_y_usd))
  cat(sprintf("Pool relative price (X/Y): %.8f\n", P))
  cat(sprintf("Pool Range (X/Y): [%.4f, %.4f]\n", min_range, max_range))
  cat(sprintf("Initial wallet: %.4f X, %.4f Y, $%.2f (Total Value: $%.2f)\n", x_init, y_init, usd_init, total_usd_value))

  # stop if the current price is outside the range
  if (P < min_range) {
    stop("Current price is below the range: the pool will only consist of token X.\n")}
  if (P > max_range) {
    stop("Current price is above the range: the pool will only consist of token Y.\n")}

  # Inside the range: calculate the required ratio R (Y / X)
  x_req_factor <- (1 / sqrt(P) - 1 / sqrt(max_range))
  y_req_factor <- (sqrt(P) - sqrt(min_range))
  R <- y_req_factor / x_req_factor

  # Solving the equation:
  # x_target * price_x_usd + y_target * price_y_usd = total_usd_value
  # and y_target = R * x_target
  # x_target * price_x_usd + (R * x_target) * price_y_usd = total_usd_value
  x_target <- total_usd_value / (price_x_usd + R * price_y_usd)
  y_target <- R * x_target

  # Differences compared to current inventory
  diff_x <- x_target - x_init
  diff_y <- y_target - y_init

  # --- Formatting the response depending on whether usd_init is used or not ---

  if (usd_init == 0) {
    # Classic use case: Direct swap between X and Y
    action <- "None"
    amount_to_swap <- 0
    amount_usd <- 0
    token_in <- "None"
    token_out <- "None"
    amount_in <- "None"

    if (diff_x > 1e-8 && diff_y < -1e-8) {
      action <- "Swap Y for X"
      amount_to_swap <- abs(diff_y)
      amount_usd <- amount_to_swap * price_y_usd
      token_in <- "Y"
      token_out <- "X"
      amount_in <- amount_usd / price_x_usd
    } else if (diff_x < -1e-8 && diff_y > 1e-8) {
      action <- "Swap X for Y"
      amount_to_swap <- abs(diff_x)
      amount_usd <- amount_to_swap * price_x_usd
      token_in <- "X"
      token_out <- "Y"
      amount_in <- amount_usd / price_y_usd
    }

    if (action != "None") {
      cat(sprintf("-> Action: %s\n", action))
      cat(sprintf("-> Amount to Swap: %.8f %s (Value: $%.4f) for %.8f %s\n\n", amount_to_swap, token_in, amount_usd, amount_in, token_out))
    } else {
      cat("-> Action: No Swap needed, ratio is perfect.\n\n")
    }

    return(list(
      action = action,
      amount = amount_to_swap,
      amount_usd = amount_usd,
      token_in = token_in,
      token_out = token_out,
      target_x = x_target,
      target_y = y_target
    ))

  } else {
    # Mixed use case: Buying/Selling using USD
    cat(sprintf("-> Required Target Portfolio: %.4f X and %.4f Y\n", x_target, y_target))
    cat(sprintf("-> Actions to perform (from USD balance):\n"))

    if (diff_x > 0) {
      cat(sprintf("   - Buy %.4f X (Cost: $%.2f)\n", diff_x, diff_x * price_x_usd))
    } else if (diff_x < 0) {
      cat(sprintf("   - Sell %.4f X (Gain: $%.2f)\n", abs(diff_x), abs(diff_x) * price_x_usd))
    }

    if (diff_y > 0) {
      cat(sprintf("   - Buy %.4f Y (Cost: $%.2f)\n", diff_y, diff_y * price_y_usd))
    } else if (diff_y < 0) {
      cat(sprintf("   - Sell %.4f Y (Gain: $%.2f)\n", abs(diff_y), abs(diff_y) * price_y_usd))
    }
    cat("\n")

    return(list(
      action = "Rebalance with USD",
      target_x = x_target,
      target_y = y_target,
      diff_x = diff_x,
      diff_y = diff_y
    ))
  }
}
