# test range of subscales

# ... before "finalizing" -----
d_all <- read.csv("//Users/kweisman/Documents/Research (Stanford)/Projects/Templeton Grant/DATA WRANGLING/templeton_packets/packets123/packets123_data.csv")[-1] %>%
  select(ctry, subj, ends_with("_num")) %>%
  gather(question, response, ends_with("_num")) %>%
  mutate(question = gsub("_num", "", question))

# p1, absorption: should be 0 to 1
d_all %>% 
  filter(grepl("exwl_", question)) %>% 
  count(response) # 0, 1

# p1, daily spiritual events #1-14: should be 0 to 5
d_all %>% 
  filter(grepl("dse_", question), 
         !grepl("15", question), !grepl("16", question)) %>% 
  count(response)

# p1, daily spiritual events #15-16: should be 0 to 3
d_all %>% 
  filter(grepl("dse_", question),
         grepl("15", question) | grepl("16", question)) %>% 
  count(response)

# p1, spiritual events: should be 0 to 4
d_all %>% 
  filter(grepl("spev_", question)) %>% 
  count(response)

# p2, sensation seeking: should be -2 to 2
d_all %>% 
  filter(question %in% c("sen_01", "sen_02", "sen_03", "sen_04", "sen_05",
                         "sen_06", "sen_07", "sen_08", "sen_09", "sen_10",
                         "sen_11", "sen_12", "sen_13", "sen_14")) %>%
  count(response)

# p2, body awareness: should be -2 to 2
d_all %>% 
  filter(question %in% c("sen_15", "sen_16", "sen_17", "sen_18", "sen_19",
                         "sen_20", "sen_21", "sen_22", "sen_23", "sen_24",
                         "sen_25", "sen_26", "sen_27", "sen_28", "sen_29",
                         "sen_30", "sen_31", "sen_32", "sen_33")) %>%
  count(response)

# p2, trait metamood: should be -2 to 2
d_all %>% 
  filter(grepl("sen2_", question)) %>% 
  count(response)

# p2, hallucination: should be 0 to 3
d_all %>% 
  filter(grepl("her2_", question)) %>% 
  count(response)

# p2 & p3, VISQ: should be -2 to 2
d_all %>% 
  filter(grepl("invo_", question)) %>% 
  count(response)

# p3, hearing events: should be 0 to 1
d_all %>% 
  filter(grepl("her_", question)) %>% 
  count(response)

# p3, encoding style: should be 0 to 5
d_all %>% 
  filter(grepl("enco_", question)) %>% 
  count(response)

# p3, mind metaphors: should be -2 to 2
d_all %>% 
  filter(grepl("meta_", question)) %>% 
  count(response)

# p3, metacognition: should be -2 to 2
d_all %>% 
  filter(grepl("tat_", question)) %>% 
  count(response)

# p3, dualism: should be 0 to 1
d_all %>% 
  filter(grepl("minw_", question)) %>% 
  count(response)


# ... after "finalizing" -----
d_long <- read_csv("//Users/kweisman/Documents/Research (Stanford)/Projects/Templeton Grant/DATA WRANGLING/templeton_packets/packets123/packets123_data_byquestion_long.csv") %>%
  mutate(ctry = factor(ctry, 
                       levels = c("us", "ghana", "thailand", "china", "vanuatu")))

# p1, absorption: should be 0 to 1
d_long %>% 
  filter(grepl("exwl_", question)) %>% 
  count(response) # 0, 1

# p1, daily spiritual events #1-14: should be 0 to 5
d_long %>% 
  filter(grepl("dse_", question), 
         !grepl("15", question), !grepl("16", question)) %>% 
  count(response)

# p1, daily spiritual events #15-16: should be 0 to 3
d_long %>% 
  filter(grepl("dse_", question),
         grepl("15", question) | grepl("16", question)) %>% 
  count(response)

# p1, spiritual events: should be 0 to 4
d_long %>% 
  filter(grepl("spev_", question)) %>% 
  count(response)

# p2, sensation seeking: should be -2 to 2
d_long %>% 
  filter(question %in% c("sen_01", "sen_02", "sen_03", "sen_04", "sen_05",
                         "sen_06", "sen_07", "sen_08", "sen_09", "sen_10",
                         "sen_11", "sen_12", "sen_13", "sen_14")) %>%
  count(response)

# p2, body awareness: should be -2 to 2
d_long %>% 
  filter(question %in% c("sen_15", "sen_16", "sen_17", "sen_18", "sen_19",
                         "sen_20", "sen_21", "sen_22", "sen_23", "sen_24",
                         "sen_25", "sen_26", "sen_27", "sen_28", "sen_29",
                         "sen_30", "sen_31", "sen_32", "sen_33")) %>%
  count(response)

# p2, trait metamood: should be -2 to 2
d_long %>% 
  filter(grepl("sen2_", question)) %>% 
  count(response)

# p2, hallucination: should be 0 to 3
d_long %>% 
  filter(grepl("her2_", question)) %>% 
  count(response)

# p2 & p3, VISQ: should be -2 to 2
d_long %>% 
  filter(grepl("invo_", question)) %>% 
  count(response)

# p3, hearing events: should be 0 to 1
d_long %>% 
  filter(grepl("her_", question)) %>% 
  count(response)

# p3, encoding style: should be 0 to 5
d_long %>% 
  filter(grepl("enco_", question)) %>% 
  count(response)

# p3, mind metaphors: should be -2 to 2
d_long %>% 
  filter(grepl("meta_", question)) %>% 
  count(response) # PROBLEM XXXXXXXXXX

# p3, metacognition: should be -2 to 2
d_long %>% 
  filter(grepl("tat_", question)) %>% 
  count(response)

# p3, dualism: should be 0 to 1
d_long %>% 
  filter(grepl("minw_", question)) %>% 
  count(response)
