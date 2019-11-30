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
      min(grep(str_c("※ 編輯: ", post_author), f2_set, fixed = TRUE))
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
  end_pos <- min(grep("※ 發信站:|文章網址:|※ 編輯:", adj_post_dom))

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
