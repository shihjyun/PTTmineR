
find_env <- function(var.name) {
  i <- 1
  while(!(var.name %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  parent.frame(i)
}

cli_miner_create <- function(){
  cli_par()
  cli_alert_success("Miner is created successfully!")
  cli_end()
  cli_text("Note that you don't need to assign the result back to a variable when you do the following actions.
           {.pkg PTTmineR}'s functions are modified by reference.")
  cli_par()
  cli_li(c(
    " Crawl the data from {.url www.ptt.cc}: {.code mine_ptt()}",
    " Update the selected post already store in the database: {.code update_ptt()}",
    " Analysis the data crawl from {.url www.ptt.cc}: {.code export_ptt()}",
    " Parallel scraping: {.code futrue::plan(multiprocess(workers = n))}"
  ))
  cli_end()
}
