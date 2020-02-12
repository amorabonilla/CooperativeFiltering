library(jsonlite)
winners <- fromJSON("/Volumes/ImacUMA2019/Google\ Drive/Mi\ unidad/yelp-dataset/yelp_academic_dataset_user.json",flatten=TRUE)

library(RJSONIO) 
tip <- stream_in(file("yelp_academic_dataset_tip.json"))
checkin <- stream_in(file("yelp_academic_dataset_checkin.json"))
user <- stream_in(file("yelp_academic_dataset_user.json"))
review <- stream_in(file("yelp_academic_dataset_review.json"))
business <- stream_in(file("yelp_academic_dataset_business.json")) 

Lines <- readLines("/Volumes/ImacUMA2019/Google\ Drive/Mi\ unidad/yelp-dataset/yelp_academic_dataset_user.json") 
business <- as.data.frame(t(sapply(Lines, fromJSON)))


yelp <- stream_in(file("yelp_academic_dataset_business.json"))
head(yelp)
str(yelp)
yelp_flat <- flatten(yelp)
str(yelp_flat)

library(tibble)
yelp_tbl <- as_data_frame(yelp_flat)
yelp_tbl

yelp_tbl %>% mutate(categories = as.character(categories)) %>% select(categories)


yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attribute"))


library(stringr)
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant"))


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  mutate(categories = as.character(categories)) %>% select(categories)


library(tidyr)
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories)

yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories) %>%
  count(categories)

yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories) %>%
  count(categories) %>%
  arrange(desc(n))


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(categories) %>%
  arrange(desc(n))


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  arrange(desc(n))


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  group_by(state) %>%
  top_n(1, n)


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  filter(n > 10) %>%
  group_by(state) %>%
  top_n(1, n)









