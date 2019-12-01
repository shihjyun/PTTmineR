#' @importFrom glue glue

ptt_cnd_handler <- function(error.type, miner.env = NULL, ...){

  get(error.type)(miner.env = NULL, ...)

}

err_unknow <- function(miner.env){
  miner_env <- miner.env
  par_env <- caller_env(5)
  miner_env$.error_log$error_url <- unlist(c(miner_env$.error_log$error_url, par_env$tmp_post_result[["error_url"]]))
}


err_date_inval <- function(miner.env){
  # message
  par_env <- find_env("cnd_break")
  par_env$cnd_break <- TRUE
}

err_final_page <- function(miner.env){
  par_env <- find_env("cnd_break")
  par_env$cnd_break <- TRUE
  cli_alert_info("This is the last page of seaching!", .envir = par_env)
}


abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue("{msg}; not {not}.")
  }

  abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not
  )
}
