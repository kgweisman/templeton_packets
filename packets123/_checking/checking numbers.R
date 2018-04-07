# from "data munging" -----

p1v1_encoded %>% distinct(p1v1_ctry, p1v1_subj) %>% count(p1v1_ctry)
p1v2_encoded %>% distinct(p1v2_ctry, p1v2_subj) %>% count(p1v2_ctry)
p2v1_encoded %>% distinct(p2v1_ctry, p2v1_subj) %>% count(p2v1_ctry)
p2v2_encoded %>% distinct(p2v2_ctry, p2v2_subj) %>% count(p2v2_ctry)
p3v1_encoded %>% distinct(p3v1_ctry, p3v1_subj) %>% count(p3v1_ctry)
p3v2_encoded %>% distinct(p3v2_ctry, p3v2_subj) %>% count(p3v2_ctry)


p1v1_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)
p1v2_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)
p2v1_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)
p2v2_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)
p3v1_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)
p3v2_tidy %>% filter(scale == "ctry") %>% distinct(response, subj) %>% count(response)

d_merge %>% filter(scale == "ctry") %>% distinct(packet, version, subj, response) %>% count(response, packet, version)

d_all %>% distinct(ctry, packet, version, subj) %>% count(ctry, packet, version)

# from d_all directly
d_all <- read.csv("//Users/kweisman/Documents/Research (Stanford)/Projects/Templeton Grant/DATA WRANGLING/templeton_packets/packets123/packets123_data.csv")

d_all_long <- d_all %>%
  select(-c(X, batc, entr, date, file, starts_with("wher", recr, whoc),
            ends_with("_num"))) %>%
  distinct() %>%
  gather(question, response, -c(subj, packet, version, ctry)) %>%
  filter(!is.na(response)) %>%
  mutate(attn = (grepl("attn", question))) %>%
  distinct()

d_all_long %>%
  filter(attn) %>% 
  count(ctry, packet, version, subj, response) %>% 
  count(ctry, packet, version, response) %>%
  group_by(ctry, packet, version) %>%
  mutate(total = sum(nn)) %>%
  spread(response, nn) %>%
  mutate(fail = ifelse(is.na(fail), 0, fail)) %>%
  mutate(prop_fail = round(fail/total, 2))

# from "finalizing" -----
d_tidy1 %>% distinct(ctry, packet, version, subj) %>% count(ctry, packet, version)

d_tidy5 %>%
  left_join(d_tidy1 %>% distinct(ctry, subj)) %>%
  distinct(ctry, packet, version, subj) %>% count(ctry, packet, version)

d_tidy6 %>%
  left_join(d_tidy1 %>% distinct(ctry, subj)) %>%
  distinct(ctry, packet, version, subj) %>% count(ctry, packet, version)
# HERE'S THE BIG PROBLEM!!
