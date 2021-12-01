data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

library(psych)
library(tidyverse)
library(car) 
library(lmtest) 
library(sandwich) 
library(boot)
library(lmboot) 
 

View(data_sample_1)

data_sample_1 %>%
  summary()

data_sample_1 %>%
  ggplot() + aes(x = pain) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = age) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = pain_cat) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = mindfulness) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = STAI_trait) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = cortisol_serum) + geom_histogram()
data_sample_1 %>%
  ggplot() + aes(x = cortisol_saliva) + geom_histogram()

#excluding 
data_sample_1_true1 <- data_sample_1 %>%
  slice(-c(88, 34))

data_sample_1_true <- data_sample_1_true1 %>% 
  mutate(sex = recode(sex, "female" = "0", "male" = "1"))



model1 <- lm(pain ~ age + sex, data = data_sample_1_true)
summary(model1)


model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_true)
summary(model2)




data_sample_1_true %>%
  ggplot() + aes(x = cortisol_saliva, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = age, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = sex, y = pain) + geom_point()

data_sample_1_true %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point()



data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = cortisol_saliva, y = pain, label = rownum) +
  geom_point() + geom_text()

data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) +
  geom_point() + geom_text()


data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_point() + geom_text()

data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) +
  geom_point() + geom_text()

data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_point() + geom_text()


data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = sex, y = pain, label = rownum) +
  geom_point() + geom_text()


data_sample_1_true %>%
  mutate(rownum = row.names(data_sample_1_true)) %>%
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) +
  geom_point() + geom_text()



data_sample_1_true %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() +
  geom_smooth(method = "lm")


model2 %>%
  plot(which = 5)

model2 %>%
  plot(which = 4)

data_sample_1_true %>%
  slice(c(85, 46, 73))


model2 %>%
  plot(which = 2)

residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))


model2 %>%
  residualPlots()

model2 %>%
  plot(which = 3)

model2 %>%
  ncvTest()

model2 %>%
  bptest()

model2 %>%
  vif()

data_sample_1_true %>%
  select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum,cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

summary(model2)



model2_new <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_true)
summary(model2_new)

model2_new %>%
  plot(which = 5)

model2_new %>%
  plot(which = 4)

model2_new %>%
  plot(which = 2)

residuals_model2_new = enframe(residuals(model2_new))
residuals_model2_new %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2_new))


model2_new %>%
  residualPlots()

model2_new %>%
  plot(which = 3)

model2_new %>%
  ncvTest()

model2_new %>%
  bptest()

model2_new %>%
  vif()

data_sample_1_true %>%
  select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum) %>%
  pairs.panels(col = "red", lm = T)



summary(model1)$adj.r.squared

summary(model2_new)$adj.r.squared


AIC(model1)

AIC(model2_new)


Anova(model1, model2_new)

