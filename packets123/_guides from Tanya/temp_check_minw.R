from_d_merge <- d_merge %>% 
  filter(grepl("minw", scale), 
         !grepl("attn", question_label_universal), 
         !is.na(response), 
         subj %in% d_long$subj) %>%
  distinct(subj, question_label_universal, response) %>%
  count(question_label_universal, response) %>% 
  spread(response, n) %>%
  rename("question" = "question_label_universal", 
         "MERGE_agree" = "i agree",
         "MERGE_disagree" = "i do not agree",
         "MERGE_missing" = "missing data") %>%
  select(question, MERGE_agree, MERGE_disagree) %>%
  data.frame()

from_d_long <- d_long %>% 
  filter(grepl("minw", question), 
         !grepl("attn", question), 
         !is.na(response)) %>% 
  count(question, response) %>% 
  spread(response, n) %>%
  select(question, "1", "0") %>%
  rename("LONG_agree" = "1",
         "LONG_disagree" = "0") %>%
  data.frame()

compare <- full_join(from_d_merge, from_d_long) %>%
  left_join(question_key %>% 
              rename(question = question_label_universal) %>%
              select(question, byhand_coding)) %>%
  mutate(COMP_agree = (MERGE_agree - LONG_agree),
         COMP_disagree = (MERGE_disagree - LONG_disagree))
View(compare)

from_d_merge2 <- d_merge %>% 
  filter(grepl("minw", scale), 
         !grepl("attn", question_label_universal), 
         !is.na(response), 
         subj %in% d_long$subj) %>%
  distinct(subj, question_label_universal, response) %>%
  data.frame() %>%
  left_join(question_key %>% 
              rename(rev = byhand_coding) %>%
              select(byhand_subscale, question_label_universal, rev)) %>%
  left_join(d_all %>% 
              mutate(subj = as.character(subj)) %>% 
              distinct(subj, ctry)) %>%
  distinct(subj, ctry, byhand_subscale, question_label_universal, rev, response) %>%
  mutate(response_cat = factor(response, 
                               levels = c("i do not agree", "i agree")),
         response_num = as.numeric(response_cat) - 1,
         response_num_rev = ifelse(rev == 1, 
                                   response_num, 
                                   abs(response_num - 1)),
         ctry = factor(ctry, 
                       levels = c("us", "ghana", "thailand", 
                                  "china", "vanuatu")))

from_d_merge3 <- from_d_merge2 %>%
  group_by(subj, ctry, byhand_subscale) %>%
  summarise(score = sum(response_num_rev))

ggplot(from_d_merge3, 
       aes(x = score, fill = ctry)) +
  facet_grid(ctry ~ byhand_subscale) +
  geom_histogram(binwidth = 1) +
  geom_vline(data = from_d_merge3 %>% 
               group_by(ctry, byhand_subscale) %>% 
               summarise(med = median(score, na.rm = T)),
             aes(xintercept = med),
             color = "black", lty = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

ggplot(from_d_merge2 %>% 
         filter(!is.na(response_num_rev)) %>%
         left_join(question_key %>% distinct(question_label_universal, question_text)) %>%
         distinct(),
       aes(x = reorder(question_text, as.numeric(byhand_subscale)),
           alpha = factor(response_num_rev, 
                          labels = c("materialist", "dualist")), 
           fill = ctry)) +
  facet_grid(~ ctry) +
  geom_bar(position = position_fill()) +
  geom_vline(xintercept = 5.5, size = 1) +
  geom_vline(xintercept = 11.5, size = 1) +
  geom_vline(xintercept = 16.5, size = 1) +
  geom_vline(xintercept = 24.5, size = 1) +
  geom_hline(yintercept = 0.5, lty = 2, color = "darkgray") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 1, 0.5)) +
  theme_bw() +
  coord_flip() +
  labs(y = "proportion", x = "question (by subscale)", fill = "ctry",
       alpha = "response")

