# load packages
library(tidyverse)
library(langcog)
library(psych)
library(readxl)
library(cowplot)
library(lme4)
library(lmerTest)
library(kableExtra)
library(lubridate)

# set theme
theme_set(theme_bw())

# load in raw data
d_raw <- read_xlsx("./data from Nikki/Packet 7_CODED_March 18, 2019.xlsx",
                   sheet = 1)

# make variable key
var_key <- d_raw[1,] %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("question") %>%
  rename(question_text = ".") %>%
  mutate(question_text = gsub("‚Äô", "'", question_text),
         question_text = gsub("‚Äî", " - ", question_text),
         question_text = gsub("‚Äú", "'", question_text),
         question_text = gsub("‚Äù", "'", question_text),
         question_text = gsub("\n", " ", question_text),
         question_text = gsub("\r", " ", question_text),
         question_text = gsub("  ", " ", question_text),
         question_text = gsub("  ", " ", question_text),
         question_text = gsub("  ", " ", question_text)) %>%
  mutate(order = 1:nrow(.),
         scale = gsub("p7_", "", question),
         scale = gsub("_.*$", "", scale),
         scale = ifelse(scale %in% c("entr", "2day", "ver", "batc", "resample",
                                     "ctry", "subj", "file", "recr", "wher", 
                                     "whoc"), "data_entry", scale)) %>%
  select(order, scale, question, question_text) %>%
  arrange(order)

# check for duplicates
duplicates <- d_raw %>%
  count(p7_ctry, p7_subj) %>%
  filter(n > 1)

# eliminate duplicates
d00 <- full_join(d_raw %>%
                   filter((!p7_subj %in% duplicates$p7_subj)),
                 d_raw %>% 
                   filter(p7_subj %in% duplicates$p7_subj & p7_batc == 1))

# eliminate first sample from ghana (poorly collected)
d00 <- d00 %>%
  filter(!(p7_resample == "1" & p7_ctry == "4"))

# # check for duplicates
# d00 %>%
#   count(p7_ctry, p7_subj) %>%
#   filter(n > 1)

# # check sample size by site
# d00 %>% 
#   distinct(p7_ctry, p7_subj) %>%
#   count(p7_ctry)

# recode variables
d0 <- d00[-1,] %>% # get rid of question text
  mutate_at(vars(starts_with("p7_abs_"), starts_with("p7_dse_"),
                 starts_with("p7_se_"), starts_with("p7_wob_"),
                 starts_with("p7_unev_"), starts_with("p7_exsen_"),
                 starts_with("p7_hthk_"), starts_with("p7_por_"),
                 starts_with("p7_mm_"), p7_dem_age, p7_dem_ses,
                 p7_dem_how.sprt.relg), 
            funs(as.numeric)) %>%
  mutate_at(vars(p7_dem_holy.tung.gif),
            funs(recode_factor(., "0" = "no", "1" = "yes"))) %>%
  mutate_at(vars(p7_abs_check, p7_dse_check, p7_se_check, p7_unev_check, 
                 p7_exsen_check, p7_por_check, p7_mm_check),
            funs(recode_factor(., "0" = "pass", "1" = "fail"))) %>%
  mutate_at(vars(p7_abs_child.exp:p7_abs_sunset),
            funs("cat" = recode(., "0" = F, "1" = T))) %>%
  mutate_at(vars(p7_dse_god.prescn:p7_dse_accept.wrong),
            funs("cat" = recode_factor(., 
                                       "0" = "never",
                                       "1" = "once in a while",
                                       "2" = "some days",
                                       "3" = "most days",
                                       "4" = "every day",
                                       "5" = "many times a day"))) %>%
  mutate_at(vars(p7_se_voice.out:p7_se_own.healing),
            funs("cat" = recode_factor(., 
                                       "0" = "never",
                                       "1" = "once",
                                       "2" = "several times",
                                       "3" = "fairly often",
                                       "4" = "very often"))) %>%
  mutate_at(vars(p7_wob_set.mind_reverse:p7_wob_future.on.me_reverse),
            funs("cat" = recode_factor(., 
                                       "-3" = "strongly disagree",
                                       "-2" = "somewhat disagree",
                                       "-1" = "a little disagree",
                                       "0" = "neither agree nor disagree",
                                       "1" = "a little agree",
                                       "2" = "somewhat agree",
                                       "3" = "strongly agree"))) %>%
  mutate_at(vars(p7_wob_little.change:p7_wob_pushed.around),
            funs("cat" = recode_factor(., 
                                       "3" = "strongly disagree",
                                       "2" = "somewhat disagree",
                                       "1" = "a little disagree",
                                       "0" = "neither agree nor disagree",
                                       "-1" = "a little agree",
                                       "-2" = "somewhat agree",
                                       "-3" = "strongly agree"))) %>%
  mutate_at(vars(p7_unev_voice.aloud:p7_unev_shadows),
            funs("cat" = recode_factor(., 
                                       "0" = "never",
                                       "1" = "sometimes",
                                       "2" = "often",
                                       "3" = "almost always"))) %>%
  mutate_at(vars(p7_exsen_esp.exists:p7_exsen_send.msg),
            funs("cat" = recode_factor(., 
                                       "0" = "has not had the experience (no)",
                                       "1" = "has had the experience (yes)"))) %>%
  mutate_at(vars(p7_hthk_complex:p7_hthk_responsblt, p7_hthk_long.hrs,
                 p7_hthk_way.to.top:p7_hthk_new.soltions,
                 p7_hthk_puzzles:p7_hthk_intel.task, p7_hthk_not.personal),
            funs("cat" = recode_factor(., 
                                       "-2" = "extremely not like me",
                                       "-1" = "somewhat not like me",
                                       "0" = "not sure",
                                       "1" = "somewhat like me",
                                       "2" = "extremely like me"))) %>%
  mutate_at(vars(p7_hthk_not.fun:p7_hthk_avoid.think, 
                 p7_hthk_hrd.hav.to:p7_hthk_lil.thought, p7_hthk_not.exciting,
                 p7_hthk_mental.effrt:p7_hthk_job.done),
            funs("cat" = recode_factor(., 
                                       "2" = "extremely not like me",
                                       "1" = "somewhat not like me",
                                       "0" = "not sure",
                                       "-1" = "somewhat like me",
                                       "-2" = "extremely like me"))) %>%
  mutate_at(vars(p7_por_thgs.hrt, p7_por_wifi.thgs:p7_por_angr.cntrl,
                 p7_por_sprt.envy:p7_por_read.thgs, p7_por_stre.spoil,
                 p7_por_conslt.unseen:p7_por_spkn.curse,
                 p7_por_curse.sick:p7_por_fall.in.lov,
                 p7_por_thgs.heal:p7_por_visualization),
            funs("cat" = recode_factor(., 
                                       "0" = "it does not happen",
                                       "1" = "it might happen",
                                       "2" = "it definitely happens"))) %>%
  mutate_at(vars(p7_por_thgs.hurt_a:p7_por_thgs.hurt_c,
                 p7_por_angr.cntrl_a:p7_por_angr.cntrl_c,
                 p7_por_read.thgs_a:p7_por_read.thgs_c,
                 p7_por_stre.spoil_a:p7_por_stre.spoil_c,
                 p7_por_spkn.curse_a:p7_por_spkn.curse_c,
                 p7_por_fall.in.lov_a:p7_por_fall.in.lov_c),
            funs("cat" = recode_factor(., 
                                       "0" = "no",
                                       "1" = "a little",
                                       "2" = "a lot"))) %>%
  mutate_at(vars(p7_mm_ang_feel.hurt:p7_mm_sprt.thgs.hurt),
            funs("cat" = recode_factor(., 
                                       "0" = "never",
                                       "1" = "rarely",
                                       "2" = "often",
                                       "3" = "very often"))) %>%
  mutate(p7_ctry = recode_factor(p7_ctry,
                                 "1" = "US",
                                 "4" = "Ghana",
                                 "3" = "Thailand",
                                 "2" = "China",
                                 "5" = "Vanuatu"),
         p7_subj = factor(p7_subj),
         p7_dem_sex = recode_factor(p7_dem_sex, 
                                    "1" = "male", 
                                    "2" = "female", 
                                    "3" = "other"),
         p7_dem_rur.urb = recode_factor(p7_dem_rur.urb,
                                        "1" = "rural",
                                        "2" = "urban"),
         p7_dem_affrd.basics = recode_factor(p7_dem_affrd.basics,
                                             "1" = "no",
                                             "0" = "yes"),
         p7_dem_ses_cat = recode_factor(p7_dem_ses,
                                        "-2" = "much poorer",
                                        "-1" = "a little poorer",
                                        "0" = "about the same",
                                        "1" = "a little richer",
                                        "2" = "much richer")) %>%
  select(-c(p7_entr:p7_resample, p7_file:p7_whoc)) %>%
  distinct()

# count anyone who failed any attention checks
# exclude anyone who failed any attention checks
# d_fail <- d0 %>%
#   filter(p7_abs_check == "fail" | p7_dse_check == "fail" | 
#            p7_se_check == "fail" | p7_unev_check == "fail" | 
#            p7_exsen_check == "fail" | p7_por_check == "fail" |
#            p7_mm_check == "fail")
# d_fail %>% count(p7_ctry)

# # count attention check failures
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("check")) %>%
#   gather(scale, attention_check, contains("check")) %>%
#   count(p7_ctry, scale, attention_check) %>%
#   spread(attention_check, n) %>%
#   mutate(scale = gsub("p7_", "", scale),
#          scale = gsub("_.*$", "", scale))

# exclude anyone who failed any attention checks
d <- d0 %>%
  filter(p7_abs_check != "fail", p7_dse_check != "fail", p7_se_check != "fail",
         p7_unev_check != "fail", p7_exsen_check != "fail",
         p7_por_check != "fail", p7_mm_check != "fail")

# # check country codes
# d %>% 
#   distinct(p7_ctry, p7_subj) %>%
#   mutate(p7_subj_firstdig = substr(p7_subj, 1, 1)) %>%
#   count(p7_ctry, p7_subj_firstdig)

# # check for missing questions: absorption
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("abs")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: porosity
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("por")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# CORRECT FOR MISSING QUESTION IN THAILAND: porosity
d0 <- d0 %>%
  mutate(p7_por_total = case_when(p7_ctry == "Thailand" ~ 
                                    p7_por_total * 32/30,
                                  TRUE ~ p7_por_total))

# # check for missing questions: martha story
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("mm")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # # check for missing questions: spiritual events
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("_se_")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: dse
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("dse")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: extrasensory
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("exsen")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: unusual events
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("unev")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: ways of being
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("wob")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# # check for missing questions: hard thinking
# d0 %>% 
#   select(p7_ctry, p7_subj, contains("hthk")) %>% 
#   select(-ends_with("_cat"), -contains("total"), -contains('check')) %>%
#   gather(question, response, -c(p7_ctry, p7_subj)) %>%
#   filter(!is.na(response)) %>%
#   count(p7_ctry, question) %>%
#   complete(p7_ctry, nesting(question), fill = list(n = 0)) %>%
#   filter(n < 20)

# standardize, collapsing across participants
d_std <- d0 %>% # including all participants
  select(p7_ctry, p7_subj, contains("total")) %>%
  distinct() %>%
  gather(scale, score, contains("total")) %>%
  mutate(scale = gsub("p7_", "", scale),
         scale = gsub("_.*$", "", scale)) %>%
  group_by(scale) %>%
  mutate(score_std = scale(score, scale = T)) %>%
  ungroup() %>%
  select(-score) %>%
  spread(scale, score_std)

# standardize, collapsing across participants
d_std_bysite <- d0 %>% # including all participants
  select(p7_ctry, p7_subj, contains("total")) %>%
  distinct() %>%
  gather(scale, score, contains("total")) %>%
  mutate(scale = gsub("p7_", "", scale),
         scale = gsub("_.*$", "", scale)) %>%
  group_by(p7_ctry, scale) %>% # group by site
  mutate(score_std = scale(score, scale = T)) %>%
  ungroup() %>%
  select(-score) %>%
  spread(scale, score_std)
