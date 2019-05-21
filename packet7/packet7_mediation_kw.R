library(mediation)

# POROSITY (DULIN) -----

## DSE -----

### porosity mediated by absorption -----

# por --> abs --> dse
b_dse <- lm(abs ~ por, d_std)
c_dse <- lm(dse ~ por + abs, d_std)
contcont_dse <- mediate(b_dse, c_dse, sims = 1000, 
                        treat = "por", mediator = "abs")
summary(contcont_dse) # NO MEDIATION (0%)
# plot(contcont_dse)

### absorption mediated by porosity -----

# abs --> por --> dse
b_dse2 <- lm(por ~ abs, d_std)
c_dse2 <- lm(dse ~ por + abs, d_std)
contcont_dse2 <- mediate(b_dse2, c_dse2, sims = 1000, 
                         treat = "abs", mediator = "por")
summary(contcont_dse2) # YES MEDIATION (91%)
# plot(contcont_dse2)


## SE -----

### porosity mediated by absorption -----

# por --> abs --> se
b_se <- lm(abs ~ por, d_std)
c_se <- lm(se ~ por + abs, d_std)
contcont_se <- mediate(b_se, c_se, sims = 1000, 
                       treat = "por", mediator = "abs")
summary(contcont_se) # YES MEDIATION (4%)
# plot(contcont_se)

### absorption mediated by porosity -----

# abs --> por --> se
b_se2 <- lm(por ~ abs, d_std)
c_se2 <- lm(se ~ por + abs, d_std)
contcont_se2 <- mediate(b_se2, c_se2, sims = 1000, 
                        treat = "abs", mediator = "por")
summary(contcont_se2) # YES MEDIATION (48%)
# plot(contcont_se2)


# MARTHA STORY -----

## DSE -----

### porosity mediated by absorption -----

# mm --> abs --> dse
b_dse <- lm(abs ~ mm, d_std)
c_dse <- lm(dse ~ mm + abs, d_std)
contcont_dse <- mediate(b_dse, c_dse, sims = 1000, 
                        treat = "mm", mediator = "abs")
summary(contcont_dse) # NO MEDIATION (2%)
# plot(contcont_dse)

### absorption mediated by porosity -----

# abs --> mm --> dse
b_dse2 <- lm(mm ~ abs, d_std)
c_dse2 <- lm(dse ~ mm + abs, d_std)
contcont_dse2 <- mediate(b_dse2, c_dse2, sims = 1000, 
                         treat = "abs", mediator = "mm")
summary(contcont_dse2) # YES MEDIATION (73%)
# plot(contcont_dse2)


## SE -----

### porosity mediated by absorption -----

# mm --> abs --> se
b_se <- lm(abs ~ mm, d_std)
c_se <- lm(se ~ mm + abs, d_std)
contcont_se <- mediate(b_se, c_se, sims = 1000, 
                       treat = "mm", mediator = "abs")
summary(contcont_se) # YES MEDIATION (5%)
# plot(contcont_se)

### absorption mediated by porosity -----

# abs --> mm --> se
b_se2 <- lm(mm ~ abs, d_std)
c_se2 <- lm(se ~ mm + abs, d_std)
contcont_se2 <- mediate(b_se2, c_se2, sims = 1000, 
                        treat = "abs", mediator = "mm")
summary(contcont_se2) # YES MEDIATION (47%)
# plot(contcont_se2)



# SCRAPS -----

temp1 <- lm(dse ~ abs + por, d_std)
summary(temp1)

temp2 <- lm(dse ~ abs * por, d_std)
summary(temp2)

temp3 <- lm(se ~ abs + por, d_std)
summary(temp3)

temp4 <- lm(se ~ abs * por, d_std)
summary(temp4)

temp5 <- lm(dse ~ abs + mm, d_std)
summary(temp5)

temp6 <- lm(dse ~ abs * mm, d_std)
summary(temp6)

temp7 <- lm(se ~ abs + mm, d_std)
summary(temp7)

temp8 <- lm(se ~ abs * mm, d_std)
summary(temp8)
