

generate_url <- function(board = NULL, keyword = NULL, author = NULL,
                         recommend = NULL, id = NULL, miner.env){

  miner_env <- miner.env
  miner_env$.mutable_obj$target_board <- board # set up pttminer
  domain_url <- miner_env$.helper_obj$ptt_long_url # get from pttminer

  if (is.null(c(keyword, author, recommend, id))) {

    target_url <- str_c(domain_url,"/", board, "/")

  } else if(!is.null(id)) {

    target_url <- str_c(domain_url,"/", board, "/", id, ".html")

  } else {

    par_author <- ifelse(is.null(author), "", str_c(" author:", author))
    par_recommend <- ifelse(is.null(recommend), "", str_c(" recommend:", recommend))
    target_url <- str_c(domain_url,"/", board, "/search?q=", keyword, par_author, par_recommend)

  }

  return(target_url)

}
