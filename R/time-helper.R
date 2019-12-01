#
# There's the problem that the comments' date-time of each post doesn't have the year tag,
# so the parser needs to customize to resolve this problem.
#
# There're some following rules:
# 1. The post's year is the benchmark of the comment year.
# 2. The n+1th comment date-time is grater than/same as the nth comment date time.
# 3. If n+1th comment date-time is lesser than the nth comment date-time, the post year must plus 1.
#'
#' @importFrom lubridate year

convert_time <- function(date.time.char, convert.type, post.date) {
  convert_format <- list(post = "%b %e %H:%M:%S %Y",
                         comment = "%Y %m/%d %H:%M")
  convert_type <- convert_format[[convert.type]]

  origin_set <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'C')

  if (missing(post.date)) {
    stopifnot(convert.type == "post")
    convert_result <-
      strptime(date.time.char, convert_type, tz = "Asia/Taipei") %>%
      as.POSIXct()

  } else {
    base_year <- lubridate::year(post.date)
    convert_result <-
      strptime(str_c(base_year, " ", date.time.char), convert_type, tz = "Asia/Taipei") %>%
      as.POSIXct()
  }

  on.exit(Sys.setlocale('LC_TIME', origin_set), add = TRUE)
  return(convert_result)

}


modify_year <- function(raw.time.vec) {
  diff_pos <-
    which((raw.time.vec[2:length(raw.time.vec)] - raw.time.vec[1:length(raw.time.vec) -
                                                                 1]) < 0)

  if (identical(diff_pos, integer(0))) {
    return(raw.time.vec)

  } else {
    for (i in seq_along(diff_pos)) {
      if (i == max(seq_along(diff_pos))) {
        lubridate::year(raw.time.vec[(diff_pos[i] + 1):length(raw.time.vec)]) <-
          lubridate::year(raw.time.vec[diff_pos[i]]) + 1

      } else {
        lubridate::year(raw.time.vec[(diff_pos[i] + 1):diff_pos[i + 1]]) <-
          lubridate::year(raw.time.vec[diff_pos[i]]) + 1

      }
    }
    return(raw.time.vec)
  }

}
