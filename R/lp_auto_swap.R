#' @title Automated Optimal Swap with Oku API
#'
#' @description Fetches the real-time USD prices of the tokens in a specified
#' Uniswap V3 pool using the Oku (Icarus) API, and then calculates the optimal swap.
#'
#' @param pool_address Character. The contract address of the liquidity pool.
#' @param chain Character. The blockchain name as used by Oku (e.g., "gnosis", "arbitrum", "optimism").
#' @param min_range Numeric. Lower bound of the range (expressed as amount of token1 per token0).
#' @param max_range Numeric. Upper bound of the range (expressed as amount of token1 per token0).
#' @param x_init Numeric. Initial amount of token0 (X) in the wallet.
#' @param y_init Numeric. Initial amount of token1 (Y) in the wallet.
#' @param usd_init Numeric. Initial amount of USD to invest.
#'
#' @importFrom httr POST add_headers content_type_json status_code content
#' @importFrom jsonlite toJSON
#'
#' @examples
#' oku_optimal_swap(
#'  pool_address = "0xe8a249626d3f3b876b887c30a3355513cb3fa9e4",
#'  chain = "gnosis",
#'  min_range = 2000,
#'  max_range = 2500,
#'  x_init = 0,
#'  y_init = 893.91,
#'  usd_init = 0)
#'
#' @export
oku_optimal_swap <- function(pool_address, chain, min_range, max_range, x_init = 0, y_init = 0, usd_init = 0) {

  # Building the Oku API URL based on the string
  url <- sprintf("https://omni.icarus.tools/%s/cush/searchPoolsByAddress", chain)

  # Creating the JSON payload
  payload <- list(
    params = list(
      pool_address,
      list(
        result_size = 2,
        sort_by = "tvl_usd",
        sort_order = FALSE
      )
    )
  )

  # Request POST
  response <- httr::POST(
    url,
    httr::add_headers(Accept = "application/json"),
    httr::content_type_json(),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )

  if (httr::status_code(response) == 200) {
    parsed_data <- httr::content(response, as = "parsed", type = "application/json")

    # Check that the pool exists
    if (length(parsed_data$result$pools) == 0) {
      stop("Error: No pool found for this address on this chain.")
    }

    pool <- parsed_data$result$pools[[1]]

    cat(sprintf("\n=== Data retrieved via Oku (%s) ===\n", toupper(chain)))
    cat(sprintf("Pool  : %s / %s\n", pool$t0_symbol, pool$t1_symbol))
    cat(sprintf("Price (x) %s : $%.6f\n", pool$t0_symbol, pool$t0_price_usd))
    cat(sprintf("Price (y) %s : $%.6f\n", pool$t1_symbol, pool$t1_price_usd))
    cat(sprintf("Last price : %f\n", pool$last_price))
    cat("=========================================\n\n")

    # Call of function lp_optimal_swap
    result <- lp_optimal_swap(
      price_x_usd = pool$t0_price_usd,
      price_y_usd = pool$t1_price_usd,
      min_range   = min_range,
      max_range   = max_range,
      x_init      = x_init,
      y_init      = y_init,
      usd_init    = usd_init
    )

    return(result)

  } else {
    stop(sprintf("HTTP error %s occurred while calling the Oku API", httr::status_code(response)))
  }
}


