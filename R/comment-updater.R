
update_comment <- function(target.board, post.id, miner.env) {

  miner_env <- miner.env
  post_url <- generate_url(board = target.board, id = post.id, miner.env = miner_env)
  post_id <- post.id
  target_board <- miner_env$.mutable_obj$target_board # get from pttminer
  f2_sep_term <- miner_env$.helper_obj$f2_sep_term # get from pttminer
  min_date <- "1970-01-01"

  post_dom <- GET(post_url, set_cookies(`over18` = 1L))
  if (post_dom$status_code == 404){
    post_comment_dt <- list("post_comment_dt" = NULL)
    return(post_comment_dt)
  }

  post_dom <- post_dom %>%
    content(as = "parsed", encoding = "UTF-8")

  post_info_dt <- get_post_info(post.dom = post_dom)
  post_comment_dt <- list("post_comment_dt" = get_post_comment(post.dom = post_dom))

  return(post_comment_dt)
}
