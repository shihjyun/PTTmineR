#'
#' @importFrom httr modify_url set_cookies
#' @importFrom stringr str_detect str_match
#' @importFrom rvest html_attr
#' @importFrom furrr future_pmap
#' @importFrom purrr transpose walk
#' @importFrom data.table set
#' @importFrom stats na.omit

get_all_posts <- function(index.page.url, last.n.page, min.date, miner.env) {

  # preparation
  cnd_break <- FALSE
  miner_env <- miner.env
  target_board <- miner_env$private$.mutable_obj$target_board  # get from pttminer
  ptt_url <- miner_env$private$.helper_obj$ptt_short_url  # get from pttminer
  ifelse(is.null(min.date), {min_date <- as.POSIXct("1970-01-01")}, {min_date <- as.POSIXct(min.date)})
  ifelse(is.null(last.n.page), {for_count_max <- 1:1e5}, {for_count_max <- 1:last.n.page})
  index_page_url <- modify_url(index.page.url)


  # the interator to get post id
  for (i in for_count_max) {

    miner_env$private$.spinner$mine_monkey$spin(template = "{spin}PTTmineR mining from ptt on your setting ...")
    index_req <- GET(index_page_url, set_cookies(`over18` = 1L))
    if (index_req$status_code == 404) {
      abort("PTTmineR can't enter into the index page. Please check if your settings are appropriate.",
            "error_404") # handler:error_404
    }

    index_page <- content(index_req, as = 'parsed', encoding = "UTF-8")

    main_container <- index_page %>%
      html_nodes(css = '.r-ent .title ,.r-list-sep')

    # extract & replace non-announcement elements from main_container if this is the true inedex page
    if (str_detect(index_page_url, str_c(target_board, "/", "$"))) {
      r_list_sep <- miner_env$private$.helper_obj$r_list_sep # get from pttminer
      end_pos <- min(which(grepl(r_list_sep,as.character(main_container), fixed = TRUE))) - 1L
      if (!is.infinite(end_pos)) {
        main_container <- main_container[1L:end_pos]
      }
    }

    add_post_id <- main_container %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      str_match(str_c(target_board, "/(.*?).html"))

    add_post_id <- rev(add_post_id[,2])

    if (identical(add_post_id, character(0))) {
      # no result searching
      cnd_break <- TRUE
      miner_env$private$.spinner$mine_monkey$finsh()
      cli_alert_danger("There're no result on your search! pls try again!")
      break
    } else {
      # check duplicated post id
      add_post_id <- add_post_id[!(add_post_id %in% miner_env$self$result_dt$post_info_dt$post_id)]
      if (identical(add_post_id, character(0))) next
    }


    # parallel excution
    tmp_post_result <- future_pmap(list(post.id = add_post_id),
                                   ~ get_post_dt(post.id = .x,
                                                 miner.env = miner_env$private,
                                                 min.date = min_date)) %>%
      transpose()



    # row bind each posts' information
    add_post_info_dt <- rbindlist(tmp_post_result[["post_info_dt"]])
    add_post_comment_dt <- rbindlist(tmp_post_result[["post_comment_dt"]])

    # row bind each pages' to PTTmineR by reference
    miner_env$self$result_dt$post_info_dt <- rbindlist(list(miner_env$self$result_dt$post_info_dt, add_post_info_dt))
    miner_env$self$result_dt$post_comment_dt <- rbindlist(list(miner_env$self$result_dt$post_comment_dt, add_post_comment_dt))

    # get last page url
    index_page_url <- index_page %>%
      html_nodes('a[class="btn wide"], a[class="btn wide disabled"]') %>%
      html_attr('href') %>%
      `[[`(2)

    index_page_url <- str_c(ptt_url, index_page_url)

    if (is.na(index_page_url)) {
      ptt_cnd_handler(error.type = "err_final_page")
      break
    }


    # extract error_url & store into miner's error log
    error_set <- unlist(tmp_post_result[["error_type"]])

    if (!is.null(error_set)) {
      walk(unique(error_set), ~ptt_cnd_handler(error.type = .x, miner.env = miner_env$private))
    }

    if (cnd_break) break

  }

  if (!cnd_break) {
    miner_env$private$.spinner$mine_monkey$spin("{spin}PTTmineR mining from ptt on your setting ... DONE")
  }


  if ((miner_env$self$result_dt$post_info_dt$post_id == "dummy") & (nrow(miner_env$self$result_dt$post_info_dt)>1)) {
    # remove the first dummy row !
    set(miner_env$self$result_dt$post_comment_dt, 1L, 1L, NA)
    miner_env$self$result_dt$post_comment_dt <- na.omit(miner_env$self$result_dt$post_comment_dt, cols="post_id")

    # remove the first dummy row !
    set(miner_env$self$result_dt$post_info_dt, 1L, 1L, NA)
    miner_env$self$result_dt$post_info_dt <- na.omit(miner_env$self$result_dt$post_info_dt, cols="post_id")
  }

  # remove NA comment
  miner_env$self$result_dt$post_comment_dt <- na.omit(miner_env$self$result_dt$post_comment_dt, cols="push_type")

}
