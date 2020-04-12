# 準備
## 既存のオブジェクトを削除
rm(list = ls())

## パッケージの読み込み
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)

## 開始時間の記録
start.time <- Sys.time()

## エンドポイントの読み込み
source("endpoint.r", encoding = "utf-8")

## テキストの読み込み
text <- scan("text.txt", what = character(), fileEncoding = "utf-8") %>% paste0(collapse = "")
paste0("Text length: ", nchar(text)) %>% print()

# COTOHAによる処理
## アクセストークンの取得
config <- read_json("config.json") # リクエストボディのロード
token <- POST(url.token, body = config, encode = "json", content_type = "application/json", verbose()) # リクエストをPOST
access.token <- content(token) %>% magrittr::extract2("access_token") # レスポンスからアクセストークンを取得


## 固有表現抽出
status <- 500 # ステータスコードの初期化
while (status == 500) { # ステータスコードが500の間はリトライ
    body.ne <- list(sentence = text) # リクエストボディを作成
    result.ne.tmp <- POST(url.ne, body = body.ne, encode = "json", content_type = "application/json;charset=UTF-8", add_headers(Authorization = paste("Bearer", access.token)), verbose()) # リクエストをポスト
    status <- status_code(result.ne.tmp) # レスポンスのステータスコードを取得
    Sys.sleep(20) # 20秒遅延
    if (as.numeric(difftime(Sys.time(), start.time, units = "mins")) > 3) {break()} # 総経過時間が3分を超えていれば処理を中断
}

result.ne <- result.ne.tmp %>% content() %>% magrittr::extract2("result") %>% bind_rows() %>% bind_rows() # レスポンスをtibbleにまとめる

## キーワード抽出
status <- 500 # ステータスコードを初期化
while (status == 500) { # ステータスコードが500の間はリトライ
    body.key <- list(document = text, type = "default", max_keyword_num = 100) # リクエストボディを作成
    result.key.tmp <- POST(url.keyword, body = body.key, encode = "json", content_type = "application/json;charset=UTF-8", add_headers(Authorization = paste("Bearer", access.token)), verbose()) # リクエストをポスト
    status <- status_code(result.key.tmp) # ステータスコードを取得
    Sys.sleep(20) # 20秒遅延
    if (as.numeric(difftime(Sys.time(), start.time, units = "mins")) > 3) {break()} # 総経過時間が3分を超えていれば処理を中断
}

result.key <- result.key.tmp %>% content() %>% magrittr::extract2("result") %>% bind_rows() %>% bind_rows() # レスポンスをtibbleにまとめる


# 結果の統合
full.list.tmp <- inner_join(result.ne, result.key, by = "form", keep = TRUE) %>% distinct(form) # 共通部分を残して結合し、重複を削除
if (nrow(full.list.tmp) == 0) { # キーワード抽出がうまく行かなかった場合の処理
    full.list <- result.ne %>% distinct(form) # 固有表現の結果のみを使用
}

full.list <- full.list.tmp %>% mutate(num = row_number(), index = as.character(formatC(row_number(), width = 6, flag = "0"))) # 行番号と行のインデックスを取得


# 問題文と解答の作成
quiz.tmp <- NULL # オブジェクトを初期化
for (j in full.list$num) { # 該当する単語をインデックスコードに置換
    if (j == 1) {
        quiz.tmp <- text %>% str_replace_all(pattern = full.list$form[j], replacement = full.list$index[j])
    } else {
        quiz.tmp <- quiz.tmp %>% str_replace_all(pattern = full.list$form[j], replacement = full.list$index[j])
    }
}

order.list <- str_extract_all(quiz.tmp, pattern="[0-9]{6}") %>% unlist() %>% unique() %>% tibble(index =., order = 1:length(.)) # 問題文のなかでの順番を取得

quiz <- NULL # オブジェクトを初期化
for (k in 1:nrow(order.list)) { # インデックスコードを番号に置換
    if (k == 1) {
        quiz <- quiz.tmp %>% str_replace_all(pattern = order.list$index[k], replacement = paste0("（　", order.list$order[k], "　）"))
    } else {
        quiz <- quiz %>% str_replace_all(pattern = order.list$index[k], replacement = paste0("（　", order.list$order[k], "　）"))
    }
}

## ファイルネームの設定
file.name <- start.time %>% format( "%Y-%m-%d_%H_%M" )

## 問題文の保存
quiz %>% write(paste0("quiz_", file.name, ".txt"))

## 解答の作成と保存
answer <- inner_join(full.list, order.list, by = "index") %>% arrange(order) %>% transmute(`番号` = order, `答え` = form) # 番号と解答をひもづけ
answer %>% write_csv(paste0("answer_", file.name, ".txt")) # 解答を保存