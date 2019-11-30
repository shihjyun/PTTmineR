PTTmineR <- R6::R6Class(
  classname = "PTTmineR",
  lock_objects = FALSE,
  public = list(
    initialize = function(task.name) {

      if (!is.character(task.name)) {
        abort_bad_argument("task.name", must = "be character", not = task.name)
      }

      private$.meta_obj$task_name <- task.name

      self$result_dt$post_info_dt <-
        data.table(
          "post_id" = "dummy",
          "post_author" = "dummy",
          "post_category" = "dummy",
          "post_title" = "dummy",
          "post_date_time" = as.POSIXct("1970-01-01"),
          "post_ip" = "dummy",
          "post_country" = "dummy",
          "post_content" = "dummy",
          "post_board" = "dummy"
        )

      self$result_dt$post_comment_dt <-
        data.table(
          "post_id" = "dummy",
          "push_type" = "dummy",
          "push_id" = "dummy",
          "push_content" = "dummy",
          "push_ip" = "dummy",
          "push_date_time" = as.POSIXct("1970-01-01")
        )



    },
    result_dt = list(post_info_dt = NULL,
                     post_comment_dt = NULL),
    print = function(){
      cli_rule(center = " * PTTMINER * ")
      cli_li("task name: {private$.meta_obj$task_name}")
      cli_li("total posts: {private$.meta_obj$total_posts}")
      cli_li("total comments: {private$.meta_obj$total_comments}")
      cli_li("miner's size: {private$.meta_obj$corpus_size}")
      cli_li("last crawling date: {private$.meta_obj$last_crawl_date}")
    }

  ),
  private = list(
    .meta_obj = list(
      task_name = NULL,
      total_posts = NA,
      total_comments = NA,
      last_crawl_date = NA,
      corpus_size = NA
    ),
    .helper_obj = list(
      f2_sep_term = "※ 發信站: 批踢踢實業坊(ptt.cc)",
      r_list_sep = "<div class=\"r-list-sep\"></div>",
      ptt_short_url = "https://www.ptt.cc",
      ptt_long_url = "https://www.ptt.cc/bbs"
    ),
    .mutable_obj = list(target_board = NULL),
    .error_log = list(error_occur = FALSE,
                      error_url = NULL),
    .spinner = list(mine_monkey = make_spinner(which = "monkey"),
                    update_runner = make_spinner(which = "runner"))
  )
)
