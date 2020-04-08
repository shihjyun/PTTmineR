
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PTTmineR <img src="man/figures/logo.svg" align="right" alt="" width="120" />

簡單、快速地爬取 PTT 上的資料，讓使用者更能專注在分析工作上！

## Why `PTTmineR` ?

  - 友善使用者的語意化使用方式
  - 整合多種 PTT 文章搜尋方式
  - 內部高效率的資料處理( `data.table` )
  - 支持平行運算( `future` )

## Installation

安裝最新的 `PTTmineR` 開發版本：

``` r
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("shihjyun/PTTmineR")
```

## Getting started

### 載入 `PTTmineR` 套件

``` r
library(PTTmineR)
```

### 使用 `PTTmineR$new` 創建第一個 miner

你可以幫這支 miner 所要執行的爬蟲任務取一個簡單的名稱

``` r
rookie_miner <- PTTmineR$new(task.name = "Mr. Meeseeks")
# inspect your miner's metadata
rookie_miner
#> *** PTTMINER ***
#> ● task name: Mr. Meeseeks
#> ● total posts: NA
#> ● total comments: NA
#> ● miner's size: NA
#> ● last crawling date time: NA
```

接下來的任何操作都不需要再用到 `<-` 或是 `=` 綁定名字到物件上(除非你需要再創建新的 miner 物件)， 原因是在
`PTTmineR` 套件的設計上有 **Reference Semantics** 的特性，所以接下來所操作的 function 都會是
side-effect function，對這部分不理解的話並不會影響套件的使用，有興趣的話可以參考之後的技術文件會更深入說明，
而現在你只需要透過接下來的範例知道爬到的資料最後都到了哪裡就好了！

### 使用 `mine_ptt()` 獲得資料

在 `mine_ptt()` function 中除了 `ptt.miner` 需輸入剛剛創建 miner 變數外，也可以輸入一下參數：

  - `board` : 字串，必填，你想要爬的板 e.g. Gossiping, Beauty, HatePolitics
  - `keyword` : 字串，選填，在板中透過標題關鍵字搜尋
  - `author` : 字串，選填，在板中透過文章作者搜尋
  - `recommend` : 數字，選填，在板中透過文章淨推文搜尋
  - `min.date` : 字串，選填，爬到什麼時間點為止 e.g. `2018-01-01`,
    `2019-11-01 15:00:01`
  - `last.n.page` : 數字，從最前面一頁開始爬多少頁

以上提到的各種搜尋條件都可以混搭使用！

``` r
# You can ...
mine_ptt(ptt.miner = rookie_miner,
         board = "Gossiping",
         last.n.page = 10)
# or ...(Using `%>%` is more semantic !!)
rookie_miner %>% 
  mine_ptt(board = "Gossiping",
           last.n.page = 10)

#> 🙉 PTTmineR mining from ptt on your setting ... DONE
  
```

雖然 `PTTmineR` 的 function 都是 side-effect function，但一樣可以支持 `%>%` 進行語意化操作：

「rookie\_miner 從 Gossiping 板中爬最新的十頁文章回來！」

``` r
rookie_miner

#> *** PTTMINER ***
#> * task name: Mr. Meeseeks
#> * total posts: 192
#> * total comments: 6487
#> * miner's size: 1.52 MB
#> * last crawling date time: 2019-12-02 15:45:03
```

### 使用 `export_ptt()` 輸出爬下來的資料

除了 `ptt.miner` 要放入 miner 物件外，`export.type` 參數目前接受三種資料輸出格式：

  - `"dt"` : `data.table` 格式，習慣 `data.tabla` 操作的使用者可以使用
  - `"tbl"` : `tibble` 格式，習慣 `tidyverse` 操作的使用者可以使用
  - `"nested_tbl"` : 巢狀 `tibble` 格式，只有一張表，各篇文章的推文內容會以單個 column 的形式儲存

而最後的 `obj.name` 要填入最後要回傳到全域環境的物件名稱

``` r
rookie_miner %>% 
  export_ptt(export.type = "tbl",
             obj.name = "tbl_result")

colnames(tbl_result$post_info_tbl)

#> [1] "post_id"        "post_author"    "post_category"  "post_title"     
#> [5] "post_date_time" "post_ip"        "post_country"   "post_content"  
#> [9] "post_board"  

colnames(tbl_result$post_comment_tbl)

#> [1] "post_id"        "push_type"      "push_id"        "push_content"   
#> [5] "push_ip"        "push_date_time"
```

一般來說在自己定義的物件名稱中會得到兩張表：

  - `post_info_tbl` : 文章基本資料
  - `post_comment_tbl` : 推文的基本資料

而以上所顯示的欄位就是現階段 `PTTmineR` 能夠爬取的資料

### 使用 `update_ptt()` 更新已經爬的文章推文

我們知道推文是會動態增加的，所以我們要分析時有可能會想知道之前爬過的文章有沒有新的推文產生，盡量保持資料完整性

``` r
rookie_miner

#> *** PTTMINER ***
#> * task name: Mr. Meeseeks
#> * total posts: 192
#> * total comments: 6487
#> * miner's size: 1.52 MB
#> * last crawling date time: 2019-12-02 15:45:03

update_id <- rookie_miner$result_dt$post_info_dt$post_id

rookie_miner %>% 
  update_ptt(update.post.id = update_id)

rookie_miner

#> *** PTTMINER ***
#> * task name: Mr. Meeseeks
#> * total posts: 192
#> * total comments: 8406
#> * miner's size: 1.89 MB
#> * last crawling date time: 2019-12-02 16:52:23
```

### 使用 `plan(multiprocess)` 進行平行爬取

如果沒有做特別設定的話，R 都是以單執行緒來進行爬取作業，如果要做大量的爬取，可能會非常花時間， 所以 `PTTmineR`
使用平行爬取來解決這個問題，要使用平行爬取非常簡單，只需執行：

``` r
plan(multiprocess(workers = 8, gc = TRUE)) # from `future` package
```

`worker`代表著想要使用的執行續數量可以依照配備的狀況作調整，而`gc`建議都設定為`TRUE`，它可以在爬行時幫大家有規律地啟動 R
的記憶體釋放機制，釋放已經沒有用處的記憶體

從以下測試(8 cores)可以感受到平行爬取的高效率，但實際情況還是會跟設備等級/網路速度有關，
使用者可以依照自己主機/網路的情形來決定要不要進行平行爬取，待爬取任務結束後建議可以使用`plan(sequential)`
將沒有在工作中的 R 執行緒的記憶體釋放掉，關於平行化我會在技術文件中談論更多

``` r
single_miner <- PTTmineR$new(task.name = "lonely miner")
multiple_miners <- PTTmineR$new(task.name = "unity is strength")


plan(sequential)
tictoc::tic()
single_miner %>% 
  mine_ptt(board = "Gossiping", last.n.page = 10)
tictoc::toc()

#> 🙈 PTTmineR mining from ptt on your setting ... DONE
#> 82.83 sec elapsed sec elapsed

plan(multiprocess(workers = 8, gc = TRUE))
tictoc::tic()
multiple_miners %>% 
  mine_ptt(board = "Gossiping", last.n.page = 10)
tictoc::toc()

#> 🙈 PTTmineR mining from ptt on your setting ... DONE
#> 24.42 sec elapsed
```

(小提醒：可以觀察🐵變換表情的頻率來約略知道爬取的速度)

如果想要保存整個 miner 也十分簡單：

``` r
readr::write_rds(rookie_miner, "rookie_miner.RDS")

rookie_miner <- readRDS("rookie_miner.RDS")
```

基本上可以把 miner 物件想成是一個小型資料庫，使用者可以針對各個爬取主題輕鬆進行管理

## Questions

### Q : 一些推文的 ip/時間 有 `NA` 的情形是？

  - 有些板並沒有顯示推文 ip
  - 有些文章作者在回文編輯時會不小心動到其他推文的格式

目前無法處理這種情況

### Q : 推文沒顯示年分，你的年份怎麼來的？

這部分先可以參考 `PTTmineR`
時間處理函數的[註解](https://github.com/shihjyun/PTTmineR/blob/master/R/time-helper.R)，
目前可以簡單解決推文跨年份的問題，但還是有極小機率判斷錯誤，之後會於其他技術文件中說明

### Q : 有爬不了的文章嗎？

有的！像是[這種](https://www.ptt.cc/bbs/Gossiping/M.1574656085.A.287.html)整個沒有基本資料的文章，基本上這種文章
`PTTmineR`會記錄在錯誤區然後跳過。目前包括新舊文章，應該可以處理 99% 的狀況，之後的技術文件會說明 目前已經處理了那些狀況

### Q : 會加入分析的功能嗎？

不會，`PTTmineR`的工作就是專注在爬取資料

### Q : 真正的資料在哪裡？

其實爬下來的資料都是存放在 miner 物件中，但會有額外 `export_ptt()` 的設計是希望使用者盡量不要去修改到 miner
物件中的原始資料

``` r
rookie_miner$result_dt
```

因為目前還是開發版本，難免會有不完美的地方，有一些東西(測試、文件)我還會補得更齊全， 如果發現 bug 或是有其他相關問題建議，歡迎在
issue 中跟我說，中英都很歡迎！
