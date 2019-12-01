
mine_ptt <-
  function(ptt.miner,
           board = NULL,
           keyword = NULL,
           author = NULL,
           recommend = NULL,
           min.date = NULL,
           last.n.page = NULL) {
    # check arguments are correct type
    if (!is.character(board))
      abort_bad_argument("board", must = "be character", not = board)

    if (!is.character(keyword))
      abort_bad_argument("keyword", must = "be character", not = keyword)

    if (!is.character(author))
      abort_bad_argument("author", must = "be character", not = author)

    if (!is.numeric(recommend))
      abort_bad_argument("recommend", must = "be numeric", not = recommend)

    if (!is.character(min.date))
      abort_bad_argument("min.date", must = "be character", not = min.date)

    if (!is.numeric(last.n.page))
      abort_bad_argument("last.n.page", must = "be numeric", not = last.n.page)

    # check arguments are reasonable
    if (last.n.page < 1)
      abort_bad_argument("last.n.page", must = "greater than one")

    if (is.null(board))
      abort_bad_argument("board", must = "be set")

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

    if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
      abort("The class of ptt.miner is not 'PTTmineR'!")
    }

    origin_set <- getOption("warn")
    options(warn = -1)


    # let all functions can access miner's scope
    root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__


    generate_url(
      board = board,
      keyword = keyword,
      author = author,
      recommend = recommend,
      id = id,
      miner.env = root_miner_env$private
    ) %>%
      get_all_posts(last.n.page = last.n.page, min.date, miner.env = root_miner_env)


    root_miner_env$private$.meta_obj$last_crawl_date <- Sys.Date()
    root_miner_env$private$.meta_obj$total_posts <-
      nrow(root_miner_env$self$result_dt$post_info_dt)
    root_miner_env$private$.meta_obj$total_comments <-
      nrow(root_miner_env$self$result_dt$post_comment_dt)
    root_miner_env$private$.meta_obj$corpus_size <-
      prettyunits::pretty_bytes(lobstr::obj_size(root_miner_env))

    on.exit({
      options(warn = origin_set)
    }, add = TRUE)

  }


update_ptt <- function(ptt.miner, update.post.id) {
  # check arguments are correct type
  if (!is.character(update.post.id))
    abort_bad_argument("update.post.id", must = "be character", not = update.post.id)

  update_post_id <- update.post.id

  # actually ptt.miner is lhs in pipe
  chain_env <- find_env("chain_parts")

  if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
    abort("The class of ptt.miner is not 'PTTmineR'!")
  }

  root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__

  # check update_post_id are within ptt.miner
  update_pos <-
    (root_miner_env$self$result_dt$post_info_dt$post_id) %in% update_post_id
  if (any(!update_pos)) {
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
    root_miner_env$self$result_dt$post_comment_dt[!(post_id %in% update_post_id)]
  # bind the updated comment into ptt.miner comment data.table
  root_miner_env$self$result_dt$post_comment_dt <-
    rbindlist(list(
      root_miner_env$self$result_dt$post_comment_dt,
      update_com_set
    ))
  root_miner_env$private$.spinner$update_runner$spin(template = "{spin}PTTmineR updating the posts you chose ... DONE")
}


export_ptt <- function(ptt.miner, export.type, obj.name) {
  chain_env <- find_env("chain_parts")
  # actually ptt.miner is lhs in pipe

  if (!any(class(chain_env$env$`_lhs`) %in% "PTTmineR")) {
    abort("The class of ptt.miner is not 'PTTmineR'!")
  }

  if (!(export.type %in% c("dt", "tbl", "nested_tbl"))) {
    abort("The selected type is not acceptable.")
  }

  root_miner_env <- chain_env$env$`_lhs`$.__enclos_env__
  full_export_type <- str_c("export_", export.type)

  get(full_export_type)(obj.name, miner.env = root_miner_env)

  cli_alert_success("PTTmineR export corpus to {export.type} successfully!")

}
