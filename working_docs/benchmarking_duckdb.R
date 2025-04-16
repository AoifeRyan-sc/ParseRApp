# should maybe save this somewhere
# me testing timnings - not sure I should bother with this? ----
# start <- Sys.time()
# 
# tmp_duckplyr <- tmp %>% as_duckdb_tibble()
# test <- tmp_duckplyr %>%
#   mutate(clean_text = Message,
#          clean_text = tm::removeWords(clean_text, tm::stopwords((kind = "SMART")))) 
# 
# timer_duckplyr <- Sys.time() - start
# 
# start <- Sys.time()
# 
# test <- tmp %>%
#   mutate(clean_text = Message,
#          clean_text = tm::removeWords(clean_text, tm::stopwords((kind = "SMART")))) 
# timer_dplyr <- Sys.time() - start
# 
# 
# start <- Sys.time()
# # tmp_duckplyr <- tmp %>% as_duckdb_tibble()
# test <- tbl(con, "test_df") %>%
#   mutate(sentiment = tolower(Sentiment)) |> 
#   summarise(
#     n = n(),
#     median_rt = median(`Sender Followers Count`, na.rm = TRUE),
#     mean_rt = mean(`Sender Followers Count`, na.rm = TRUE),
#     max_rt = max(`Sender Followers Count`, na.rm = TRUE),
#     .by = SenderScreenName
#   ) |> 
#   arrange(desc(median_rt)) |> 
#   filter(n > 10)
# 
# timer_duckplyr <- Sys.time() - start
# 
# start <- Sys.time()
# 
# test <- tmp %>%
#   mutate(sentiment = tolower(Sentiment)) |> 
#   summarise(
#     n = n(),
#     median_rt = median(`Sender Followers Count`, na.rm = TRUE),
#     mean_rt = mean(`Sender Followers Count`, na.rm = TRUE),
#     max_rt = max(`Sender Followers Count`, na.rm = TRUE),
#     .by = SenderScreenName
#   ) |> 
#   arrange(desc(median_rt)) |> 
#   filter(n > 10)
# timer_dplyr <- Sys.time() - start
# 
# tmp_5_db <- as_duckdb_tibble(tmp_5)
# con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
# 
# duckdb::dbWriteTable(
#   conn = con,
#   name = "test_df_5",
#   tmp_5
# )
#  
# tbl_test <- tbl(con, "test_df_5")
# 
start <- Sys.time()
test <-   tbl_test %>%
  mutate(clean_text = Message) %>%
  # tmp_5_db %>%
  # as_duckdb_tibble() %>% # tbl(con, "test_df") %>% # tmp_duckplyr <- tmp %>% as_duckdb_tibble()
  ParseR::clean_text(text_var = clean_text, in_parallel = F)
df_no_stops <- test %>%
  collect() %>%
  mutate(clean_text = tm::removeWords(clean_text, tm::stopwords("SMART"))) 

duckdb::dbWriteTable( # need to move so much of this to a designated r function
  conn = con,
  name = "test_df_5",
  df_no_stops,
  overwrite = T
)
tbl_test <- dplyr::tbl(con, "test_df_5")
timer_duckplyr <- Sys.time() - start

start <- Sys.time()
# test <- tmp_2 %>%
#   ParseR::clean_text(text_var = Message, in_parallel = F)
# timer_dplyr <- Sys.time() - start
# 
# print(timer_dplyr)
# print(timer_duckplyr)

bigram <- test %>%
  collect() %>%
  count_ngram_app(text_var = clean_text, top_n = 25, min_freq = 5)






