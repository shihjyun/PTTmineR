
#' @title
#' Crawl the selected board post and store into PTTmineR.
#'
#' @description
#' You can use following arguments to get/filter the
#' PTT post data you want. Note that you need at least
#' choose which board you want to crawl.
#'
#' @param ptt.miner a R6 class object uses `PTTmineR$new()` to create.
#' @param board a string. PTT board you want to crawl.
#' @param keyword a string. The keyword you want to search on selected board.
#' @param author a string. The post author you want to search on selected board.
#' @param recommend a number. The number of net recommend you want to filter.
#' @param min.date a date format string. The farthest date you want to set.
#' @param last.n.page a number. The number of page you wnat to crawl.
#'
#' @return
#' - post_info_dt: the data.table type post's information.
#' - post_comment_dt: the data.table type post's comments.
#' The result is store in miner_object$result_dt, but my
#' suggestion is don't modify the data in miner_object, use
#' `export_ptt()` to get and analysis your data.
#'
#' @examples
#'
#' # assume that rookie_miner is an object using `PTTmineR$new()` to create
#' # get all Gossiping posts
#' \dontrun{rookie_miner %>%
#'     mine_ptt(board = "Gossiping")}
#'
#' # get all Gossiping posts to filter by keyword 'youtuber'
#' \dontrun{rookie_miner %>%
#'     mine_ptt(board = "Gossiping", keyword = "youtuber")}
#'
#' # get all Gossiping posts to filter by keyword 'youtuber' and
#' # net recommend nuber 10.
#' \dontrun{rookie_miner %>%
#'     mine_ptt(board = "Gossiping", recommend = 10)}
#'
#' # if you want to do multiple crawling task on one eval,
#' # you can use multiple `%>%` :
#' \dontrun{rookie_miner %>%
#'     mine_ptt(board = "Gossiping", recommend = 10) %>%
#'     mine_ptt(board = "Soft_job", keyword = "python")}
#'
#' # or use `purrr::pwalk()`:
#' \dontrun{board_list <- c("Gossiping", "Soft_job", "Beauty")
#' pwalk(board_list, ~mine_ptt(board = .x, recommend = 10))}
#' # why `pwalk()`? because all PTTmineR's functions are
#' # side-effect funciton. The data will return to rookie_miner.
#'
#'
#'
#' @importFrom rlang abort
#' @importFrom magrittr %>%
#' @importFrom prettyunits pretty_bytes
#' @importFrom lobstr obj_size
#'
#' @export
#' @md
mine_ptt <- function(ptt.miner,
           board = NULL,
           keyword = NULL,
           author = NULL,
           recommend = NULL,
           min.date = NULL,
           last.n.page = NULL) {

    # check all arguments are correct type
    if (is.character(board) | is.null(board)){NULL} else {
      abort_bad_argument("board", must = "be character", not = board)
    }

    if (is.character(keyword) | is.null(keyword)){NULL} else {
      abort_bad_argument("keyword", must = "be character", not = keyword)
    }

    if (is.character(author) | is.null(author)){NULL} else {
      abort_bad_argument("author", must = "be character", not = author)
    }

    if (is.numeric(recommend) | is.null(recommend)){NULL} else {
      abort_bad_argument("recommend", must = "be numeric", not = recommend)
    }

    if (is.character(min.date) | is.null(min.date)){NULL} else {
      abort_bad_argument("min.date", must = "be character", not = min.date)
    }

    if (is.numeric(last.n.page) | is.null(last.n.page)){NULL} else {
      abort_bad_argument("last.n.page", must = "be numeric", not = last.n.page)
    }

    # check arguments are reasonable
    if (!is.null(last.n.page) & is.numeric(last.n.page)){
      if (last.n.page < 1) {
        abort_bad_argument("last.n.page", must = "greater than one")
      }
    }

    if (is.null(board)){
      abort_bad_argument("board", must = "be set")
    }

    if (!is.null(min.date)) {
      tryCatch(
        as.POSIXct(min.date),
        error = function(cnd) {
          abort("min.date is not in a standard unambiguous format")
        }
      )
    }

    # actually ptt.miner is lhs in pipe
    chain_env <- find_env("chain_parts")

    if (!identical(chain_env, .GlobalEnv)) {
      # the situation that user is using pipe
      if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
        abort("The class of ptt.miner is not 'PTTmineR'!")
      }

      root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__
    } else {
      # the situation that user is not using pipe
      root_miner_env <- ptt.miner$.__enclos_env__
    }

    origin_set <- getOption("warn")
    options(warn = -1)


    generate_url(
      board = board,
      keyword = keyword,
      author = author,
      recommend = recommend,
      miner.env = root_miner_env$private
    ) %>%
      get_all_posts(last.n.page = last.n.page, min.date = min.date, miner.env = root_miner_env)


    root_miner_env$private$.meta_obj$last_crawl_date <- Sys.time()
    root_miner_env$private$.meta_obj$total_posts <-
      nrow(root_miner_env$self$result_dt$post_info_dt)
    root_miner_env$private$.meta_obj$total_comments <-
      nrow(root_miner_env$self$result_dt$post_comment_dt)
    root_miner_env$private$.meta_obj$corpus_size <-
      prettyunits::pretty_bytes(as.numeric(lobstr::obj_size(root_miner_env)))

    on.exit({
      options(warn = origin_set)
    }, add = TRUE)

  }


#' @title
#' Update the post comments have been crawled.
#'
#' @param ptt.miner a R6 class object uses `PTTmineR$new()` to create.
#' @param update.post.id the charactar vector post id you want to update.
#'
#' @examples
#' # get the post id set from rookie_miner
#' \dontrun{update_id <- rookie_miner$result_dt$post_info_dt$post_id}
#'
#' # update the post comments from selected post id
#' \dontrun{rookie_miner %>%
#'     update_ptt(update.post.id = update_id)}
#' @importFrom furrr future_pmap
#' @importFrom data.table rbindlist
#' @export
update_ptt <- function(ptt.miner, update.post.id = character()) {
  # check arguments are correct type
  if (!is.character(update.post.id))
    abort_bad_argument("update.post.id", must = "be character", not = update.post.id)

  update_post_id <- update.post.id

  origin_set <- getOption("warn")
  options(warn = -1)

  # actually ptt.miner is lhs in pipe
  chain_env <- find_env("chain_parts")

  if (!identical(chain_env, .GlobalEnv)) {
    # the situation that user is using pipe
    if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
      abort("The class of ptt.miner is not 'PTTmineR'!")
    }

    root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__
  } else {
    # the situation that user is not using pipe
    root_miner_env <- ptt.miner$.__enclos_env__
  }

  # check update_post_id are within ptt.miner
  update_pos <-
    (root_miner_env$self$result_dt$post_info_dt$post_id) %in% update_post_id
  if (any(!(update_post_id %in% root_miner_env$self$result_dt$post_info_dt$post_id))) {
    abort(
      "The posts' id you selected is not within in PTTmineR. Please check if the posts'
          id you selected are stored in PTTmineR."
    )
  }

  root_miner_env$private$.spinner$update_runner$spin(template = "{spin}PTTmineR updating the posts you choose ...")

  target_board_set <-
    root_miner_env$self$result_dt$post_info_dt[update_pos]$post_board


  update_com_set <-
    future_pmap(
      list(target_board_set, update_post_id),
      ~ update_comment(
        target.board = .x,
        post.id = .y,
        miner.env = root_miner_env$private
      )
    ) %>%
    transpose()

  update_com_set <- rbindlist(update_com_set[["post_comment_dt"]])

  # remove the original post_id (this is not the best efficient way to remove row)
  root_miner_env$self$result_dt$post_comment_dt <-
    root_miner_env$self$result_dt$post_comment_dt[!(root_miner_env$self$result_dt$post_comment_dt$post_id %in% update_post_id)]
  # bind the updated comment into ptt.miner comment data.table
  root_miner_env$self$result_dt$post_comment_dt <-
    rbindlist(list(
      root_miner_env$self$result_dt$post_comment_dt,
      update_com_set
    ))

  root_miner_env$self$result_dt$post_comment_dt <- na.omit(root_miner_env$self$result_dt$post_comment_dt, cols="push_type")

  origin_set <- getOption("warn")
  options(warn = -1)

  root_miner_env$private$.meta_obj$last_crawl_date <-
    Sys.time()
  root_miner_env$private$.meta_obj$total_comments <-
    nrow(root_miner_env$self$result_dt$post_comment_dt)
  root_miner_env$private$.meta_obj$corpus_size <-
    prettyunits::pretty_bytes(lobstr::obj_size(root_miner_env))

  root_miner_env$private$.spinner$update_runner$spin(template = "{spin}PTTmineR updating the posts you chose ... DONE")
}


#' @title
#' Export data store in miner_object for you to analyze.
#'
#' @description
#' There're three export types you can choose:
#' - dt: the type of data.table
#' - tbl: the type of tibble (like data.frame but more better)
#' - nested_tbl: the type of nested tibble (just return one table)
#'
#' @param ptt.miner a R6 class object uses `PTTmineR$new()` to create.
#' @param export.type a string. the three export types in description part.
#' @param obj.name a string. the global object name you want to name.
#'
#' @return
#' `export_ptt()` is a side-effect function also. After you excute
#' this function, you will get the object name 'obj.name' in your
#' global environment.
#'
#' @examples
#' # export data into global environment.
#' \dontrun{rookie_miner %>%
#'     export_ptt(export.type = "dt", obj.name = "my_ptt_data")
#'     }
#'
#' # then you can call the object you name in the global environment.
#' \dontrun{my_ptt_data}
#'
#' @import rlang
#' @importFrom stringr str_c
#' @export
#' @md

export_ptt <- function(ptt.miner, export.type = character(), obj.name = character()) {

  if (!(export.type %in% c("dt", "tbl", "nested_tbl"))) {
    abort("The selected type is not acceptable.")
  }

  # actually ptt.miner is lhs in pipe
  chain_env <- find_env("chain_parts")

  if (!identical(chain_env, .GlobalEnv)) {
    # the situation that user is using pipe
    if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
      abort("The class of ptt.miner is not 'PTTmineR'!")
    }

    root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__
  } else {
    # the situation that user is not using pipe
    root_miner_env <- ptt.miner$.__enclos_env__
  }


  full_export_type <- str_c("export_", export.type)

  get(full_export_type)(obj.name, miner.env = root_miner_env)

  cli_alert_success("PTTmineR export corpus to {export.type} successfully!")

}
