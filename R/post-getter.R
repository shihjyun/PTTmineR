#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_split str_extract str_replace_all str_sub
#' @importFrom httr GET content



get_post_info <- function(post.dom) {
  par_env <- caller_env()
  post_dom <- post.dom

  # extract post's meta data
  meta_value <-
    html_nodes(post_dom, ".article-meta-value") %>% html_text()
  # resolve the problem that there isn't have board tag in meta_value
  if (any(grepl(par_env$target_board, meta_value))) {
    meta_value <- meta_value[-(grep(par_env$target_board, meta_value))]
  }

  # get post author id from meta_value
  post_author <- str_split(meta_value[[1]], pattern = " ")[[1]][[1]]

  # get post title from meta_value
  post_cat_title <- meta_value[[2]]

  post_category <- str_extract(post_cat_title, "\\[.*\\]")[[1]] %>%
    str_replace_all("\\[|\\]| ", "")

  post_title <-
    str_replace_all(post_cat_title, "\\[.*\\] |\\[.*\\]", "")

  # get post date-time from meta_value
  post_date <- str_sub(meta_value[[3]], start = 5L) %>%
    convert_time(convert.type = "post")

  # check date is within the range of user seting
  if (post_date < par_env$min_date) {
    par_env$date_invalid <- TRUE
  }

  # get post IP from post_dom
  f2_set <- post_dom %>%
    html_nodes(".f2") %>%
    html_text()

  f2_pos <- min(grep(par_env$f2_sep_term, f2_set, fixed = TRUE))

  # handle another ip format problem eg.'Gossiping/M.1574519359.A.7F4'
  if (is.infinite(f2_pos)) {
    f2_pos <-
      min(grep(str_c("\u203b \u7de8\u8f2f: ", post_author), f2_set, fixed = TRUE))
  }

  post_ip_country <- f2_set[[f2_pos]] %>%
    str_extract(
      "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+ \\(.*\\)|\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+ .*\\)"
    ) %>%
    str_replace_all("\\(|\\)", "") %>%
    str_split(" ")

  post_ip <- post_ip_country[[1]][1]

  post_country <- post_ip_country[[1]][2]

  # get post content from post_dom (between date-time & f2_set)
  adj_post_dom <- post_dom %>%
    html_nodes(xpath = '//*[@id="main-content"]/text() | //*[@id="main-content"]/div |
               //*[@id="main-content"]/span | //*[@id="main-content"]/a')

  start_pos <-
    max(grep('<span class="article-meta-value">', adj_post_dom , fixed = TRUE))
  end_pos <- min(grep("\u203b \u767c\u4fe1\u7ad9:|\u6587\u7ae0\u7db2\u5740:|\u203b \u7de8\u8f2f:", adj_post_dom))

  post_content <- adj_post_dom[(start_pos + 1):(end_pos - 1)] %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")


  post_info <- data.table(
    "post_id" = par_env$post_id,
    "post_author" = post_author,
    "post_category" = post_category,
    "post_title" = post_title,
    "post_date_time" = post_date,
    "post_ip" = post_ip,
    "post_country" = post_country,
    "post_content" = post_content,
    "post_board" = par_env$target_board
  )


  return(post_info)

}



get_post_comment <- function(post.dom) {
  par_env <- caller_env()
  post_dom <- post.dom

  adj_post_dom <- post_dom %>%
    html_nodes(".f2, .push")


  f2_pos <-
    min(grep(par_env$f2_sep_term, adj_post_dom, fixed = TRUE))

  # handle another ip format problem eg.'Gossiping/M.1574519359.A.7F4'
  if (is.infinite(f2_pos)) {
    f2_pos <-
      min(grep(str_c("\u203b \u7de8\u8f2f: ", par_env$post_author), adj_post_dom, fixed = TRUE))
  }

  start_pos <- f2_pos
  adj_post_dom <- adj_post_dom[-(1:start_pos)]


  push_type <- adj_post_dom %>%
    html_nodes(".push-tag") %>%
    html_text(trim = TRUE)

  push_id <- adj_post_dom %>%
    html_nodes(".push-userid") %>%
    html_text(trim = TRUE)

  push_content <- adj_post_dom %>%
    html_nodes(".push-content") %>%
    html_text(trim = TRUE) %>%
    str_sub(start = 3L)

  push_ipdatetime_set <- adj_post_dom %>%
    html_nodes(".push-ipdatetime") %>%
    html_text(trim = TRUE)

  push_ip <-
    str_extract(push_ipdatetime_set, "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+")

  # post_info_list is super scope variable
  push_date_time <-
    str_extract(push_ipdatetime_set, "[0-9]+/[0-9]+ [0-9]+:[0-9]+") %>%
    convert_time(convert.type = "comment",
                 post.date = par_env$post_info_dt[["post_date_time"]]) %>%
    modify_year()


  # Resolve the problem that some post would repost other push comment
  # from the otherside, so need to find the real push comment tag below
  # the separate tag.

  post_comment_info <- data.table(
    "post_id" = par_env$post_id,
    "push_type" = push_type,
    "push_id" = push_id,
    "push_content" = push_content,
    "push_ip" = push_ip,
    "push_date_time" = push_date_time
  )

  return(post_comment_info)

}


get_post_dt <-
  function(post.id,
           min.date = min_date ,
           miner.env = miner_env$private) {
    date_invalid <- FALSE
    error_occur <- FALSE
    min_date <- min.date
    miner_env <- miner.env
    post_id <- post.id
    target_board <-
      miner_env$.mutable_obj$target_board # get from pttminer
    f2_sep_term <-
      miner_env$.helper_obj$f2_sep_term # get from pttminer

    post_url <-
      generate_url(board = target_board,
                   id = post_id,
                   miner.env = miner_env)


    tryCatch({
      post_dom <- GET(post_url, set_cookies(`over18` = 1L)) %>%
        content(as = "parsed", encoding = "UTF-8")

      post_info_dt <-
        get_post_info(post.dom = post_dom)  # potencial error
      post_comment_dt <- get_post_comment(post.dom = post_dom)

      result_list <- list("post_info_dt" = post_info_dt,
                          "post_comment_dt" = post_comment_dt)
    },
    error = function(cnd) {
      # message here
      error_occur <<- TRUE
    })

    # error/date reporter
    if (error_occur) {
      result_list <-  list(
        "post_info_dt" = NULL,
        "post_comment_dt" = NULL,
        "error_url" = post_url,
        "error_type" = "err_unknow"
      )
    } else if (date_invalid) {
      result_list <-  list(
        "post_info_dt" = NULL,
        "post_comment_dt" = NULL,
        "error_type" = "err_date_inval"
      )
    }


    return(result_list)
  }
