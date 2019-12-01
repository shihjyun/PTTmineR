

export_dt <- function(obj.name, miner.env){

  miner_env <- miner.env
  export_dt_set <- miner_env$self$result_dt
  env_poke(.GlobalEnv, obj.name, export_dt_set)

}

export_tbl <- function(obj.name, miner.env){

  miner_env <- miner.env
  post_info_tbl <- as_tibble(miner_env$self$result_dt$post_info_dt)
  post_comment_tbl <- as_tibble(miner_env$self$result_dt$post_comment_dt)
  export_tbl_set <- list(post_info_tbl = post_info_tbl,
                         post_comment_tbl = post_comment_tbl)
  env_poke(.GlobalEnv, obj.name, export_tbl_set)

}

export_nested_tbl <- function(obj.name, miner.env){

  miner_env <- miner.env
  post_info_tbl <- as_tibble(miner_env$self$result_dt$post_info_dt)
  post_comment_tbl <- as_tibble(miner_env$self$result_dt$post_comment_dt) %>%
    group_nest(post_id)
  export_nested_tbl <- left_join(post_info_tbl, post_comment_tbl, by = "post_id")
  env_poke(.GlobalEnv, obj.name, export_nested_tbl)

}
