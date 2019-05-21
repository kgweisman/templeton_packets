temp <- d0 %>%
  select(p7_ctry, p7_subj, contains("_dse_"), contains("_se_"),
         -c(contains("total"), contains("check"), ends_with("_cat"))) %>%
  distinct() %>%
  gather(question, response, -c(p7_ctry, p7_subj)) %>%
  mutate(scale = case_when(grepl("_dse_", question) ~ "dse",
                           grepl("_se_", question) ~ "se",
                           TRUE ~ NA_character_),
         # rescale everything to 0-1
         response = case_when(scale == "dse" ~ response/5,
                              scale == "se" ~ response/4)) %>%
  group_by(scale, question, p7_ctry) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  ungroup() %>%
  spread(p7_ctry, mean)

temp2 <- bind_rows(temp %>% 
                     gather(site2, mean2, -c(scale, question, US)) %>%
                     mutate(site1 = "US") %>%
                     rename(mean1 = US) %>%
                     select(scale, question, site1, mean1, site2, mean2),
                   temp %>% 
                     gather(site2, mean2, -c(scale, question, Ghana)) %>%
                     mutate(site1 = "Ghana") %>%
                     rename(mean1 = Ghana) %>%
                     select(scale, question, site1, mean1, site2, mean2),
                   temp %>% 
                     gather(site2, mean2, -c(scale, question, Thailand)) %>%
                     mutate(site1 = "Thailand") %>%
                     rename(mean1 = Thailand) %>%
                     select(scale, question, site1, mean1, site2, mean2),
                   temp %>% 
                     gather(site2, mean2, -c(scale, question, China)) %>%
                     mutate(site1 = "China") %>%
                     rename(mean1 = China) %>%
                     select(scale, question, site1, mean1, site2, mean2),
                   temp %>% 
                     gather(site2, mean2, -c(scale, question, Vanuatu)) %>%
                     mutate(site1 = "Vanuatu") %>%
                     rename(mean1 = Vanuatu) %>%
                     select(scale, question, site1, mean1, site2, mean2)) %>%
  mutate_at(vars(site1, site2),
            funs(factor(., levels = c("US", "Ghana", "Thailand", 
                                      "China", "Vanuatu"))))


# all questions, DSE & SE -----

temp_cor_all_res <- temp %>%
  select(-c(scale, question)) %>%
  corr.test(method = "spearman")

ggplot(temp2, aes(x = mean1, y = mean2)) +
  facet_grid(site1 ~ site2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(data = temp_cor_all_res$ci %>%
              data.frame() %>%
              rownames_to_column("pair") %>%
              mutate(site1 = gsub("-.*$", "", pair),
                     site2 = gsub("^.*-", "", pair)) %>%
              mutate_at(vars(site1, site2),
                        funs(case_when(. %in% c("US", "Ghana", "China") ~ .,
                                       . == "Thlnd" ~ "Thailand",
                                       . == "Vanut" ~ "Vanuatu"))) %>%
              mutate_at(vars(site1, site2),
                        funs(factor(., levels = c("US", "Ghana", "Thailand", 
                                                  "China", "Vanuatu")))),
            aes(x = 0.5, y = 1.1, 
                label = paste0("r = ", 
                               format(round(r, 2), nsmall = 2),
                               ", p = ",
                               format(round(p, 3), nsmall = 3))),
            size = 3) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.25)) +
  labs(title = "Packet 7: Correlations across sites in mean responses to questions on DSE & SE scales", 
       subtitle = "Correlations (r) are Spearman rank correlations, after rescaling all response scales to be in a common range (0-1)",
       x = "Mean response (Site 1)", y = "Mean response (Site 2)")


# DSE only -----

temp_cor_dse_res <- temp %>%
  filter(scale == "dse") %>%
  select(-c(scale, question)) %>%
  corr.test(method = "spearman")

ggplot(temp2 %>% filter(scale == "dse"), aes(x = mean1, y = mean2)) +
  facet_grid(site1 ~ site2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(data = temp_cor_dse_res$ci %>%
              data.frame() %>%
              rownames_to_column("pair") %>%
              mutate(site1 = gsub("-.*$", "", pair),
                     site2 = gsub("^.*-", "", pair)) %>%
              mutate_at(vars(site1, site2),
                        funs(case_when(. %in% c("US", "Ghana", "China") ~ .,
                                       . == "Thlnd" ~ "Thailand",
                                       . == "Vanut" ~ "Vanuatu"))) %>%
              mutate_at(vars(site1, site2),
                        funs(factor(., levels = c("US", "Ghana", "Thailand", 
                                                  "China", "Vanuatu")))),
            aes(x = 0.5, y = 1.1, 
                label = paste0("r = ", 
                               format(round(r, 2), nsmall = 2),
                               ", p = ",
                               format(round(p, 3), nsmall = 3))),
            size = 3) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.25)) +
  labs(title = "Packet 7: Correlations across sites in mean responses to questions on DSE scale only", 
       subtitle = "Correlations (r) are Spearman rank correlations, after rescaling all response scales to be in a common range (0-1)",
       x = "Mean response (Site 1)", y = "Mean response (Site 2)")


# SE only -----

temp_cor_se_res <- temp %>%
  filter(scale == "se") %>%
  select(-c(scale, question)) %>%
  corr.test(method = "spearman")

ggplot(temp2 %>% filter(scale == "se"), aes(x = mean1, y = mean2)) +
  facet_grid(site1 ~ site2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(data = temp_cor_se_res$ci %>%
              data.frame() %>%
              rownames_to_column("pair") %>%
              mutate(site1 = gsub("-.*$", "", pair),
                     site2 = gsub("^.*-", "", pair)) %>%
              mutate_at(vars(site1, site2),
                        funs(case_when(. %in% c("US", "Ghana", "China") ~ .,
                                       . == "Thlnd" ~ "Thailand",
                                       . == "Vanut" ~ "Vanuatu"))) %>%
              mutate_at(vars(site1, site2),
                        funs(factor(., levels = c("US", "Ghana", "Thailand", 
                                                  "China", "Vanuatu")))),
            aes(x = 0.5, y = 1.1, 
                label = paste0("r = ", 
                               format(round(r, 2), nsmall = 2),
                               ", p = ",
                               format(round(p, 3), nsmall = 3))),
            size = 3) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.25)) +
  labs(title = "Packet 7: Correlations across sites in mean responses to questions on SE scale only", 
       subtitle = "Correlations (r) are Spearman rank correlations, after rescaling all response scales to be in a common range (0-1)",
       x = "Mean response (Site 1)", y = "Mean response (Site 2)")

