# "bot" refers to a telegram bot which can be found here: https://cran.r-project.org/web/packages/telegram/README.html
# If you have this library then errors and warnings can be sent to your phone and if not then error and warnings will just print out to the console

library(httr)
source("list.R")


use_token <- function(token) {
    token <<- token
    return(TRUE)
}


order_open <- function() {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/order/open.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Order Open: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Order Open: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Orders Open: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }
    
    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)
    
}


user_details <- function() {
    result <- NULL
    while(is.null(result)) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/user/details.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("User Details: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("User Details: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("User Details: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }
    
    return_vector <- list(data$response$username,
                          data$response$email,
                          as.numeric(data$response$balance),
                          as.numeric(data$response$deposit_unconfirmed),
                          data$response$date_created,
                          data$error_message,
                          data$warning_message)
    
    return(return_vector)
    
}


user_overview <- function() {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/user/overview.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("User Overview: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("User Overview: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("User Overview: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }
    
    return_vector <- list(data$response$username,
                          data$response$email,
                          as.numeric(data$response$balance),
                          as.numeric(data$response$deposit_unconfirmed),
                          data$response$date_created,
                          as.numeric(data$response$orders_worth),
                          as.numeric(data$response$positions_worth),
                          as.numeric(data$response$net_worth),
                          data$response$orders_open,
                          data$response$positions_open,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


user_bitcoin_deposit_address <- function() {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/user/bitcoin_deposit_address.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("User Bitcoin Deposit Address: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("User Bitcoin Deposit Address: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("User Bitcoin Deposit Address: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response$bitcoin_deposit_address,
                          data$response$two_factor_authentication,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


user_transaction_log <- function(offset = 0, limit = 20, date_start = "1970-01-01T12:00:00Z", date_end = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/user/transaction_log.php", query = list(token=token, offset=offset, limit=limit, date_start=date_start, date_end = date_end)), as = "parsed")
            if (data$error) {
              message_out <- paste0("User Transaction Log: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("User Transaction Log: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("User Transaction Log: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


user_quota_status <- function() {
  result <- NULL
  while(is.null(result)) {
    tryCatch({
      data <- content(GET("https://1broker.com/api/v2/user/quota_status.php", query = list(token=token)), as = "parsed")
      if (data$error) {
        message_out <- paste0("User Quota Status: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("User Quota Status: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("User Quota Status: Error: ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return_vector <- list(as.numeric(data$response$cpu_time_left),
                        data$error_message,
                        data$warning_message)
  
  return(return_vector)
  
}


order_create <- function(symbol, margin, direction, leverage = 1, order_type = "market", order_type_parameter, stop_loss, take_profit, shared = "false") {
    tryCatch({order_type_parameter}, error = function(e) {order_type_parameter <<- FALSE})
    tryCatch({stop_loss}, error = function(e) {stop_loss <<- FALSE})
    tryCatch({take_profit}, error = function(e) {take_profit <<- FALSE})

    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            if (order_type_parameter != FALSE && stop_loss != FALSE && take_profit != FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, order_type_parameter=order_type_parameter, stop_loss=stop_loss, take_profit=take_profit, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter == FALSE && stop_loss != FALSE  && take_profit != FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, stop_loss=stop_loss, take_profit=take_profit, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter != FALSE && stop_loss == FALSE && take_profit != FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, order_type_parameter=order_type_parameter, take_profit=take_profit, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter != FALSE && stop_loss != FALSE  && take_profit == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, order_type_parameter=order_type_parameter, stop_loss=stop_loss, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter == FALSE && stop_loss == FALSE && take_profit != FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, take_profit=take_profit, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter == FALSE && stop_loss != FALSE  && take_profit == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, stop_loss=stop_loss, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter != FALSE && stop_loss == FALSE && take_profit == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, order_type_parameter=order_type_parameter, referral_id=3981, shared=shared)), as = "parsed")
            }
            if (order_type_parameter == FALSE && stop_loss == FALSE && take_profit == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/order/create.php", query = list(token=token, symbol=symbol, margin=margin, direction=direction, leverage=leverage, order_type=order_type, order_type_parameter=order_type_parameter, referral_id=3981, shared=shared)), as = "parsed")
            }
          
            if (data$error) {
              message_out <- paste0("Order Create: Error: ", symbol, ": ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) } 
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Order Create: Warning: ", symbol, ": ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
            cat("Order", data$response$order_id, "created\n")
        },
        error = function(e) {
            cat("Order Create: Error: ", symbol, ": ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response$order_id,
                          data$response$symbol,
                          as.numeric(data$response$margin),
                          as.numeric(data$response$leverage),
                          data$response$direction,
                          data$response$order_type,
                          as.numeric(data$response$order_type_parameter),
                          as.numeric(data$response$stop_loss),
                          as.numeric(data$response$take_profit),
                          data$response$shared,
                          data$response$copy_of,
                          data$response$date_created,
                          data$error_message,
                          data$warning_message
                          )
    return(return_vector)

}


order_cancel <- function(order_id) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/order/cancel.php", query = list(token=token, order_id=order_id)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Order Cancel: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Order Cancel: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Order Cancel: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return(TRUE)

}


position_open <- function() {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/position/open.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Position Open: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Position Open: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Position Open: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }


    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


position_history <- function(offset = 0, limit = 20, date_start = "1970-01-01T12:00:00Z", date_end = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/position/history.php", query = list(token=token, offset=offset, limit=limit, date_start=date_start, date_end = date_end)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Position History: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Position History: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Position History: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


position_edit <- function(position_id, stop_loss, take_profit, trailing_stop_loss) {
    tryCatch({stop_loss}, error = function(e) {stop_loss <<- FALSE})
    tryCatch({take_profit}, error = function(e) {take_profit <<- FALSE})
    tryCatch({trailing_stop_loss}, error = function(e) {trailing_stop_loss <<- FALSE})

    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            if (stop_loss != FALSE && take_profit != FALSE && trailing_stop_loss == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, stop_loss=stop_loss, take_profit=take_profit)), as = "parsed")
            }
            if (stop_loss == FALSE && take_profit != FALSE && trailing_stop_loss == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, take_profit=take_profit)), as = "parsed")
            }
            if (stop_loss != FALSE  && take_profit == FALSE && trailing_stop_loss == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, stop_loss=stop_loss)), as = "parsed")
            }
            if (stop_loss == FALSE  && take_profit == FALSE && trailing_stop_loss == FALSE) {
                data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id)), as = "parsed")
            }
          
          if (stop_loss != FALSE && take_profit != FALSE && trailing_stop_loss != FALSE) {
            data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, stop_loss=stop_loss, take_profit=take_profit, trailing_stop_loss=trailing_stop_loss)), as = "parsed")
          }
          if (stop_loss == FALSE && take_profit != FALSE && trailing_stop_loss != FALSE) {
            data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, take_profit=take_profit, trailing_stop_loss=trailing_stop_loss)), as = "parsed")
          }
          if (stop_loss != FALSE  && take_profit == FALSE && trailing_stop_loss != FALSE) {
            data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, stop_loss=stop_loss, trailing_stop_loss=trailing_stop_loss)), as = "parsed")
          }
          if (stop_loss == FALSE  && take_profit == FALSE && trailing_stop_loss != FALSE) {
            data <- content(GET("https://1broker.com/api/v2/position/edit.php", query = list(token=token, position_id=position_id, trailing_stop_loss=trailing_stop_loss)), as = "parsed")
          }
          
          if (data$error) {
            message_out <- paste0("Position Edit: Error: ", data$error_message, "\n")
            if (exists('bot')) { bot$sendMessage(message_out) }
            cat(message_out)
          }
          if (data$warning) {
            message_out <- paste0("Position Edit: Warning: ", data$warning_message, "\n")
            if (exists('bot')) { bot$sendMessage(message_out) }
            cat(message_out)
          }
          result <- TRUE
        },
        error = function(e) {
            cat("Position Edit: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response$position_id,
                          as.numeric(data$response$stop_loss),
                          as.numeric(data$response$take_profit),
                          data$response$trailing_stop_loss,
                          data$error_message,
                          data$warning_message
                          )
    return(return_vector)

}


position_close <- function(position_id) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/position/close.php", query = list(token=token, position_id=position_id)), as = "parsed")
            print(GET("https://1broker.com/api/v2/position/close.php", query = list(token=token, position_id=position_id)))
            print(data)
            if (data$error) {
              message_out <- paste0("Position Close: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Position Close: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Position Close: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return(TRUE)

}


position_close_cancel <- function(position_id) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/position/close_cancel.php", query = list(token=token, position_id=position_id)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Position Close Cancel: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Position Close Cancel: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Position Close Cancel: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return(TRUE)

}


position_shared_get <- function(position_id) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/position/shared/get.php", query = list(token=token, position_id=position_id)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Position Shared Get: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Position Shared Get: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Position Shared Get: Error: ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return_vector <- list(data$response$symbol,
                        data$response$direction,
                        as.numeric(data$response$position_id),
                        data$response$username,
                        data$response$profile_image_url,
                        as.numeric(data$response$user_id),
                        as.numeric(data$response$leverage),
                        data$response$date_created,
                        as.numeric(data$response$entry_price),
                        data$response$is_open,
                        data$response$date_closed,
                        as.numeric(data$response$exit_price),
                        as.numeric(data$response$profit_loss_percent),
                        as.numeric(data$response$stop_loss),
                        as.numeric(data$response$take_profit),
                        data$response$trailing_stop_loss,
                        data$response$comments,
                        data$error_message,
                        data$warning_message)
  return(return_vector)
  
}
  

market_categories <- function() {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/market/categories.php", query = list(token=token)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Market Categories: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Market Categories: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Market Categories: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    #return(data$response)
    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


market_list <- function(category = "Index") {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/market/list.php", query = list(token=token, category=category)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Market List: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Market List: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Market List: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


market_details <- function(symbol="BTCUSD") {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/market/details.php", query = list(token=token, symbol=symbol)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Market Details: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Market Details: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Market Details: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response$symbol,
                          data$response$name,
                          data$response$description,
                          data$response$category,
                          data$response$type,
                          as.numeric(data$response$maximum_leverage),
                          as.numeric(data$response$maximum_amount),
                          as.numeric(data$response$overnight_charge_long_percent),
                          as.numeric(data$response$overnight_charge_short_percent),
                          as.numeric(data$response$decimals),
                          data$response$market_hours$timezone,
                          data$response$market_hours$open,
                          data$response$market_hours$close,
                          data$response$market_hours$daily_break_start,
                          data$response$market_hours$daily_break_stop,
                          data$error_message,
                          data$warning_message)
    return(return_vector)
    
}


market_quotes <- function(symbols="BTCUSD") {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/market/quotes.php", query = list(token=token, symbols=symbols)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Market Quotes: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Market Quotes: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Market Quotes: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }
    
    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


market_bars <- function(symbol, resolution = 86400, date_start = "1970-01-01T12:00:00Z", date_end = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), limit=NULL ) {
    result <- NULL
    while( is.null(result) ) {
        tryCatch({
            data <- content(GET("https://1broker.com/api/v2/market/bars.php", query = list(token=token, symbol=symbol, resolution=resolution, date_start=date_start, date_end = date_end, limit=limit)), as = "parsed")
            if (data$error) {
              message_out <- paste0("Market Bars: Error: ", data$error_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            if (data$warning) {
              message_out <- paste0("Market Bars: Warning: ", data$warning_message, "\n")
              if (exists('bot')) { bot$sendMessage(message_out) }
              cat(message_out)
            }
            result <- TRUE
        },
        error = function(e) {
            cat("Market Bars: Error: ", as.character(e), "\n")
            Sys.sleep(60)
            result <- NULL
        })
    }

    return_vector <- list(data$response,
                          data$error_message,
                          data$warning_message)
    return(return_vector)

}


get_all_quotes <- function() {
    symbols = ""
    categories <- market_categories()
    for (category in categories) {
        list[market] <- market_list(category=category)
        for (i in 1:length(market)) {
            symbols <- paste0(symbols, market[i][[1]]$symbol, sep=",")
            if (i %% 20 == 0 || i == length(market)) {
                symbols <- substr(symbols, 1, nchar(symbols)-1)
                list[quotes] <- market_quotes(symbols=symbols)
                for (j in 1: length(quotes)) {
                    cat(quotes[j][[1]]$symbol, ":", (as.numeric(quotes[j][[1]]$bid) + as.numeric(quotes[j][[1]]$ask)) / 2, "\n")
                }
                symbols = ""
                Sys.sleep(1)
            }
        }
    }
}


social_profile_statistics <- function(user_id=1) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/social/profile_statistics.php", query = list(token=token, user_id=user_id)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Social Profile Statistics: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Social Profile Statistics: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Social Profile Statistics: Error: ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return_vector <- list(data$response$user_id,
                        data$response$username,
                        data$response$profile_image_url,
                        data$response$date_created,
                        data$response$profile_about_me_html,
                        data$response$profile_about_me_raw,
                        data$response$own_profile_hidden,
                        as.numeric(data$response$risk_score),
                        as.numeric(data$response$maximum_profit_this_month),
                        as.numeric(data$response$maximum_loss_this_month),
                        as.numeric(data$response$copy_trade_reward_total),
                        as.numeric(data$response$copier_count),
                        as.numeric(data$response$copy_margin_per_trade),
                        data$response$performance,
                        data$response$copier_count_history,
                        data$response$date_cached,
                        as.numeric(data$response$average_holding_time_seconds),
                        as.numeric(data$response$trades_last_7_days),
                        as.numeric(data$response$trades_last_12_months),
                        data$response$market_category_share,
                        data$error_message,
                        data$warning_message)
  return(return_vector)
  
}


social_profile_trades <- function(user_id=1, offset = 0, limit = 20) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/social/profile_trades.php", query = list(token=token, user_id=user_id, offset = offset, limit = limit)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Social Profile Trades: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Social Profile Trades: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Social Profile Trades: Error(2): ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return_vector <- list(as.numeric(data$response$user_id),
                        data$response$username,
                        data$response$profile_image_url,
                        data$response$date_created,
                        data$response$profile_about_me_html,
                        data$response$profile_about_me_raw,
                        data$response$own_profile_hidden,
                        as.numeric(data$response$risk_score),
                        as.numeric(data$response$maximum_profit_this_month),
                        as.numeric(data$response$maximum_loss_this_month),
                        data$response$trading_ideas_open,
                        data$response$trading_ideas_closed,
                        data$error_message,
                        data$warning_message)
  return(return_vector)
  
}


social_profile_trades <- function(user_id=1, offset = 0, limit = 20) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/social/profile_trades.php", query = list(token=token, user_id=user_id, offset = offset, limit = limit)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Social Profile Trades: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Social Profile Trades: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Social Profile Trades: Error(2): ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return_vector <- list(as.numeric(data$response$user_id),
                        data$response$username,
                        data$response$profile_image_url,
                        data$response$date_created,
                        data$response$profile_about_me_html,
                        data$response$profile_about_me_raw,
                        data$response$own_profile_hidden,
                        as.numeric(data$response$risk_score),
                        as.numeric(data$response$maximum_profit_this_month),
                        as.numeric(data$response$maximum_loss_this_month),
                        data$response$trading_ideas_open,
                        data$response$trading_ideas_closed,
                        data$error_message,
                        data$warning_message)
  return(return_vector)
  
}


copy_trader_create <- function(user_id=3981, margin_per_trade = 0.25, limit_trades_daily = 20) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/copy_trader/create.php", query = list(token=token, user_id=user_id, margin_per_trade = margin_per_trade, limit_trades_daily = limit_trades_daily)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Copy Trader Create: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Copy Trader Create: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Copy Trader Create: Error(2): ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return(TRUE)
  
}


copy_trader_edit <- function(user_id=3981, margin_per_trade = 0.25, limit_trades_daily = 20) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/copy_trader/edit.php", query = list(token=token, user_id=user_id, margin_per_trade = margin_per_trade, limit_trades_daily = limit_trades_daily)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Copy Trader Edit: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Copy Trader Edit: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Copy Trader Edit: Error(2): ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return(TRUE)
  
}

copy_trader_delete <- function(user_id=3981) {
  result <- NULL
  while( is.null(result) ) {
    tryCatch({  
      data <- content(GET("https://1broker.com/api/v2/copy_trader/delete.php", query = list(token=token, user_id=user_id)), as = "parsed")
      if (data$error) {
        message_out <- paste0("Copy Trader Delete: Error: ", data$error_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      if (data$warning) {
        message_out <- paste0("Copy Trader Delete: Warning: ", data$warning_message, "\n")
        if (exists('bot')) { bot$sendMessage(message_out) }
        cat(message_out)
      }
      result <- TRUE
    },
    error = function(e) {
      cat("Copy Trader Delete: Error(2): ", as.character(e), "\n")
      Sys.sleep(60)
      result <- NULL
    })
  }
  
  return(TRUE)
  
}

