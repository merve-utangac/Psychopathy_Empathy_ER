
# Data Analyzses

```{r}
library(tidyverse)
library(dplyr)
library(visdat)
library(xlsx)
library(Hmisc) 
library(MASS)
library(car) 
library(olsrr) 
library(purrr)
library(ggcorrplot)
library(lavaan)
library(ggpubr)
library(performance) 
library(ggplot2)
library(readxl) 
library(corrtable)
library(afex)
library(rstatix)
library(scales)
library(apaTables)
library(emmeans)
library(rempsyc)
library(olsrr)
```

## Age and Gender

MET

```{r}

ggplot(final_data, aes(x = Age, y = MET_SUM_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Age, y = MET_MEAN_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 


ggplot(final_data, aes(x = Gender, y = MET_SUM_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(final_data, aes(x = Gender, y = MET_MEAN_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 


```

QCAE

```{r}

ggplot(final_data, aes(x = Age, y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Age, y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Gender, y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Gender, y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 
```

PPTS

```{r}

ggplot(final_data, aes(x = Age, y = PPTS_SUM )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Gender, y = PPTS_SUM )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 
```

ER

```{r}

ggplot(final_data, aes(x = Age, y = ER_SUM_ALL)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Gender, y = ER_SUM_ALL )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 
```

### T-TEST

#### MET

```{r}
t.test(final_data$MET_SUM_CE ~ final_data$Gender, var.equal = TRUE) 
t.test(final_data$MET_MEAN_AE ~ final_data$Gender, var.equal = TRUE) 


t.test(final_data$MET_SUM_CE, final_data$Age) 
t.test(final_data$MET_MEAN_AE, final_data$Age)

```

#### QCAE

```{r}
t.test(final_data$QCAE_CE ~ final_data$Gender, var.equal = TRUE) 
t.test(final_data$QCAE_AE ~ final_data$Gender, var.equal = TRUE) 

t.test(final_data$QCAE_CE, final_data$Age)
t.test(final_data$QCAE_AE, final_data$Age)

```

####  ER

```{r}
t.test(final_data$ER_SUM_ALL ~ final_data$Gender, var.equal = TRUE) 

t.test(final_data$ER_SUM_ALL, final_data$Age)
```

#### PPTS

```{r}

t.test(final_data$PPTS_SUM ~ final_data$Gender, var.equal = TRUE) 


t.test(final_data$PPTS_SUM, final_data$Age) 
```

## 1. MET and PPTS

The variables needed will be selected.

```{r}
MET_PPTS <- final_data %>%
  dplyr::select(Affective_Responsiveness_PPTS,Cognitive_Responsiveness_PPTS,
                Interpersonal_Manipulation_PPTS,Egocentricity_PPTS, MET_SUM_CE, MET_MEAN_AE, Age)

summary(final_data$PPTS_SUM)
sd(final_data$PPTS_SUM)

```

### Visualization

```{r}
ggplot(final_data, aes(x = PPTS_SUM, y = MET_SUM_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = PPTS_SUM, y = MET_MEAN_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 



ggplot(final_data, aes(x = Age, y = MET_SUM_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 





```

### Assumptions of correlation

#### 1.Identifying outliers

```{r}
MET_PPTS  %>% 
  identify_outliers(MET_MEAN_AE) 

MET_PPTS  %>% 
  identify_outliers(MET_SUM_CE) 

final_data  %>% 
  identify_outliers(PPTS_SUM) 


```

#### 2.Examining the assumptition of normality

QQ-plot

```{r}

ggqqplot(MET_PPTS$MET_MEAN_AE)

ggqqplot(MET_PPTS$MET_SUM_CE)

ggqqplot(final_data$PPTS_SUM)

```

Shapiro-Wilk test

```{r}
MET_PPTS %>% shapiro_test(MET_MEAN_AE,MET_SUM_CE, Egocentricity_PPTS, Interpersonal_Manipulation_PPTS, Cognitive_Responsiveness_PPTS, Affective_Responsiveness_PPTS, Age)
```

### Correlation

```{r}
mean(MET_PPTS$MET_SUM_CE)
sd(MET_PPTS$MET_SUM_CE)

mean(MET_PPTS$MET_MEAN_AE)
sd(MET_PPTS$MET_MEAN_AE)
```

cor.met.ppts

```{r}
gen.met.ae <- cor.test(x = final_data$PPTS_SUM, y = final_data$MET_MEAN_AE, method = 'spearman')

gen.met.ae

gen.met.ce <- cor.test(x = final_data$PPTS_SUM, y = final_data$MET_SUM_CE, method = 'spearman')
gen.met.ce


# r values
met_matrix <- cor(MET_PPTS, method = "spearman")

# p values

cor.met.ppts <- rcorr(as.matrix(MET_PPTS), type = "spearman")
```

#### **Benjamini-Hochberg procedure**

```{r}

met.ppts.pvalues<-c( 0.0000,0.0000,0.0162, 0.0000,0.0000,0.0000, 0.4249, 0.388, 0.0690, 0.0481, 0.0000, 0.0063, 0.0007, 0.0000,0.8253, 0.7092, 0.0077, 0.0072, 0.0153, 0.0000, 0.0005 )


fdrs.met.ppts <-p.adjust(met.ppts.pvalues, method="BH")

print(fdrs.met.ppts)

```

### Regression

Regression for all subscales of PPTS and MET AE

```{r}

#overall of ppts

model0 <- lm(PPTS_SUM ~ 1, data = final_data)
model1 <- lm(PPTS_SUM~ MET_MEAN_AE + Gender + Age, data = final_data)

stepwise_1 <- step(model0, scope = list(upper = model1),
                           direction = "both")

check_model(stepwise_1)
durbinWatsonTest(stepwise_1)
vif(stepwise_1)
confint(stepwise_1)

summary(stepwise_1)

#subscales of ppts

model2 <- lm(MET_MEAN_AE ~ 1, data = final_data)
model3 <- lm(MET_MEAN_AE ~ Egocentricity_PPTS + Affective_Responsiveness_PPTS + Interpersonal_Manipulation_PPTS + Cognitive_Responsiveness_PPTS + Gender + Age, data = final_data)

stepwise_2 <- step(model2, scope = list(upper = model3),
                           direction = "both")


check_model(stepwise_2)
durbinWatsonTest(stepwise_2)
vif(stepwise_2)
confint(stepwise_2)

summary(stepwise_2)

#report
install.packages("report")
library(report)
report(stepwise_2)
```


```{durbinWatsonTest(model3)}
vif(model3)

```

Affective empathy = 734,015 - 014.947 (Affective Responsivness) - 042.785 (Gender) + 001.405 (Age) + residual

## 2. QCAE and PPTS

The variables will be selected

```{r}

QCAE_PPTS <- final_data %>%
  dplyr::select(Affective_Responsiveness_PPTS,Cognitive_Responsiveness_PPTS,
                Interpersonal_Manipulation_PPTS,Egocentricity_PPTS, QCAE_CE, QCAE_AE, Age)

```

#### Visualization

```{r}

ggplot(final_data, aes(x = PPTS_SUM, y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = PPTS_SUM, y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 
```

### Assumptions of correlation

#### 1.Identifying outliers

```{r}
QCAE_PPTS  %>% 
  identify_outliers(QCAE_AE)


QCAE_PPTS  %>% 
  identify_outliers(QCAE_CE)

```

#### 2.Examining the assumptition of normality

QQ-plot

```{r}
ggqqplot(QCAE_PPTS$QCAE_AE)

ggqqplot(QCAE_PPTS$QCAE_CE)

```

Shapiro-Wilk test

```{r}
QCAE_PPTS %>% shapiro_test(QCAE_AE,QCAE_CE)
```

### Correlation

```{r}
mean(QCAE_PPTS$QCAE_CE)
sd(QCAE_PPTS$QCAE_CE)

mean(QCAE_PPTS$QCAE_AE)
sd(QCAE_PPTS$QCAE_AE)
```

cor.qcae.ppts

```{r}

ppts.qcae.ae <- cor.test(x = final_data$PPTS_SUM, y = final_data$QCAE_AE, method = 'spearman')
ppts.qcae.ae

ppts.qcae.ce <- cor.test(x = final_data$PPTS_SUM, y = final_data$QCAE_CE, method = 'spearman')
ppts.qcae.ce



# r values
qcae_matrix <- cor(QCAE_PPTS, method = "spearman")

# p values
cor.qcae.ppts <- rcorr(as.matrix(QCAE_PPTS), type = "spearman")

```

#### **Benjamini-Hochberg procedure**

```{r}

qcae.ppts.pvalues<-c(0.0000, 0.0000, 0.0162, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.1275, 0.0000, 0.0000, 0.0000, 0.0628, 0.0006, 0.0000, 0.7092, 0.0077, 0.0072, 0.0153, 0.0093, 0.0247)

fdrs.qcae.ppts <-p.adjust(qcae.ppts.pvalues, method="BH")

print(fdrs.qcae.ppts)

```

### Regression

```{r}
#overall of ppts

model4 <- lm(PPTS_SUM ~ 1, data = final_data)
model5 <- lm(PPTS_SUM ~ QCAE_CE + Gender + Age, data = final_data)

stepwise_3 <- step(model4, scope = list(upper = model5),
                           direction = "both")

check_model(stepwise_3)
durbinWatsonTest(stepwise_3)
vif(stepwise_3)
confint(stepwise_3)

summary(stepwise_3)


---------
  
model6 <- lm( PPTS_SUM ~ 1, data = final_data)
model7 <- lm( PPTS_SUM~ QCAE_AE + Gender + Age, data = final_data)

stepwise_4 <- step(model6, scope = list(upper = model7),
                           direction = "both")

check_model(stepwise_4)
durbinWatsonTest(stepwise_4)
vif(stepwise_4)
confint(stepwise_4)

summary(stepwise_4)
  

#subscales of ppts

model8 <- lm(QCAE_AE ~ 1, data = final_data)
model9 <- lm(QCAE_AE ~ Egocentricity_PPTS + Affective_Responsiveness_PPTS + Cognitive_Responsiveness_PPTS + Gender + Age, data = final_data)

stepwise_5 <- step(model8, scope = list(upper = model9),
                           direction = "both")


check_model(stepwise_5)
durbinWatsonTest(stepwise_5)
vif(stepwise_5)
confint(stepwise_5)

summary(stepwise_5)


---------------
  
model10 <- lm(QCAE_CE ~ 1, data = final_data)
model11 <- lm(QCAE_CE ~ Egocentricity_PPTS + Affective_Responsiveness_PPTS + Cognitive_Responsiveness_PPTS + Gender + Age, data = final_data)

stepwise_6 <- step(model10, scope = list(upper = model11),
                           direction = "both")

check_model(stepwise_6)
durbinWatsonTest(stepwise_6)
vif(stepwise_6)
confint(stepwise_6)

summary(stepwise_6)
report(model_f)

```

## 3. General ER Anova

Creating data set

```{r}

ER_ANOVA_1 <- ER_Emotions_Longer %>%
  full_join(ER_RT) %>%
  mutate(ER_Response= factor(ER_Response)) %>%
  subset(ID != 8609541 & ID != 9120041 & ID != 9147329 & ID != 9048138 & ID != 9120031)

vis_miss(ER_ANOVA_1)

ER_ANOVA<- replace(ER_ANOVA_1,is.na(ER_ANOVA_1),0)

vis_miss(ER_ANOVA)
```

### Summarizing data

```{r}

sum_er <- ER_ANOVA %>% 
group_by(ER_Response) %>%
summarise(mean_sum = mean(ER_SUM), sd_sum = sd (ER_SUM),
            mean_rt = mean(ER_RT), sd_rt = sd (ER_RT))


```

### Visualization

```{r}

#Note: Error bars show 95% confidence intervals

accuracy_plot <- ER_ANOVA %>%
  ggplot(aes(x = fct_reorder(ER_Response, ER_SUM), y = ER_SUM, colour = ER_Response)) +
  scale_y_continuous(labels = scales::percent_format (scale = 6.65)) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Facial Emotion Expressions", y = "Accuracy") +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 25, size = 18, hjust = 1),
    axis.text.y = element_text(size = 10, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 12.5), size = 20),
    axis.title.y = element_text(margin = margin(r = 9), size = 20)
  )
accuracy_plot 

# Save the plot as a high-resolution image
ggsave("accuracy_plot.png", accuracy_plot)

```

```{r}

intensity_all <- ER_Intensity_ANOVA %>%
    filter(ER_Intensity != "Neutral",
           ER_Response != "Neutral" )

intensity_graph <-
  intensity_all %>% 
  ggplot(aes(x =  fct_reorder(ER_Response, ER_SUM), y = ER_SUM, group = ER_Intensity, colour = ER_Intensity)) +
  guides(colour = FALSE) +
  scale_y_continuous(breaks = seq(0, 16, by = 1))  +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Facial Emotion Expressions", y = "Accuracy") +
  guides(color = guide_legend(title = "Intensity Level")) +
  theme(legend.position = c(.99, .30),
        legend.justification = c("right", "top"),
    text = element_text(size = 10),
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 11), size = 10),
    axis.title.y = element_text(margin = margin(r = 11), size = 10))

intensity_graph
```

### Model

accuracy and emotion

```{r}

ac_emotions <- aov_4(ER_SUM ~ ER_Response (1 + ER_Response | ID), data = ER_ANOVA)

summary(ac_emotions)
anova(ac_emotions)

emmeans(ac_emotions, pairwise ~ ER_Response, adjust = "Bonferroni")

```

```{r}
intensity.model <- aov_4(ER_SUM ~ ER_Response * ER_Intensity + (1 + ER_Response * ER_Intensity |ID), data = intensity_all, na.rm = TRUE)
anova(intensity.model)


emmeans(intensity.model, pairwise ~ ER_Response * ER_Intensity , adjust = "none")
```

#### 

## 4. MET and ER

The variables will be selected

```{r}
MET_ER <- final_data %>%
  dplyr::select(MET_SUM_CE, MET_MEAN_AE, Embarrassment, 
                Sadness, Pride, Contempt, Happiness, Disgust, Anger, Surprise, Fear, 
                Neutral)

MET_ER_RT <- MET_ER %>%
  cross_join(ER_RT_cor)


```

### Visualization

```{r}
ggplot(final_data, aes(x = ER_SUM_ALL, y = MET_SUM_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 


ggplot(final_data, aes(x = ER_SUM_ALL, y = MET_MEAN_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 




```

#### 1.Identifying outliers

```{r}
final_data  %>% 
  identify_outliers(ER_SUM_ALL)

```

#### 2.Examining the assumptition of normality

```{r}

ggqqplot(final_data$ER_SUM_ALL)


```

```{r}

MET_ER %>% shapiro_test(MET_SUM_CE, MET_MEAN_AE, Embarrassment, 
                Sadness, Pride, Contempt, Happiness, Disgust, Anger, Surprise, Fear, 
                Neutral,Age)

final_data %>% shapiro_test(ER_SUM_ALL)


ER_RT_cor %>% shapiro_test(ER_RT)
```

### Correlation

```{r}

gen.ae.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$MET_MEAN_AE, method = 'spearman')
gen.ae.er

gen.ce.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$MET_SUM_CE, method = 'spearman')
gen.ce.er

# r and p values

cor.met.er <- rcorr(as.matrix(MET_ER), type = "spearman")
cor.met.er 
```

#### Benjamini Hocberg 

```{r}

met.er.pvalues<-c(0.8253,         
0.0003,     0.6787,                     
0.3092,     0.3500,      0.0006,             
0.0003,     0.8865,      0.0000,        0.3901,        
0.1175,     0.8504,      0.0412,        0.1384,  0.0108,           
0.9184,     0.1233,      0.5911,        0.2132,  0.0018, 0.5759,              
0.5300,     0.9420,      0.2638,        0.9886,  0.4421, 0.0242,   0.2070,           
0.9129,     0.9446,      0.0039,        0.0002,  0.1172, 0.0673,   0.0267,    0.0173,          
0.1301,     0.4701,      0.1300,        0.0053,  0.9975, 0.2830,   0.0891,    0.0333,  0.0127,
0.1597,     0.8630,      0.0000,        0.0095,  0.0912, 0.0514,   0.8448,    0.3145,  0.0000, 0.0567,     0.1662,      0.2962,        0.7872,  0.5517, 0.9593,   0.0040,    0.0213,  0.1983, 0.4112,     0.3360,      0.9430    )


fdrs.met.er <-p.adjust(met.er.pvalues, method="BH")

print(fdrs.met.er)

```

### Regression

```{r}

model.1 <- lm(MET_SUM_CE ~ 1, data = final_data)

model.2 <- lm(MET_SUM_CE ~ ER_SUM_ALL + Gender + Age, data = final_data)

stepwise.new <- step(model.1, scope = list(upper = model.2),
                           direction = "both")
  
  
check_model(stepwise.new )
durbinWatsonTest(stepwise.new)
vif(stepwise.new)
confint(stepwise_7)

summary(stepwise.new)
  
--------------

model12 <- lm(MET_SUM_CE ~ 1, data = final_data)
model13 <- lm(MET_SUM_CE ~ Embarrassment + Pride + Gender + Age, data = final_data)

stepwise_7 <- step(model12, scope = list(upper = model13),
                           direction = "both")

check_model(stepwise_7)
durbinWatsonTest(stepwise_7)
vif(stepwise_7)
confint(stepwise_7)

summary(stepwise_7)

```

## 5. QCAR and ER

Selecting variables

```{r}

QCAE_ER <- final_data %>%
  dplyr::select(QCAE_CE, QCAE_AE,Embarrassment, 
                Sadness, Pride, Contempt, Happiness, Disgust, Anger, Surprise, Fear, 
                Neutral)
```

#### Visualization

```{r}
ggplot(final_data, aes(x = ER_SUM_ALL, y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = ER_SUM_ALL, y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

```

#### Correlation


```{r}

# age and er correlation

age.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$Age, method = 'spearman')
age.er

# qcae and er correlation


qcae.ae.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$QCAE_AE, method = 'spearman')
qcae.ae.er

qcae.ce.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$QCAE_CE, method = 'spearman')
qcae.ce.er

# r and p values

cor.qcae.er <- rcorr(as.matrix(QCAE_ER), type = "spearman")
cor.qcae.er
```

### 

## 6. PPTS and ER

Selecting Variables

```{r}

PPTS_ER <- final_data %>%
  dplyr::select(Embarrassment, PPTS_SUM, Sadness, Pride, Contempt, Happiness, Disgust, Anger, Surprise, Fear, Neutral, Age)


Intensity.PPTS <- ER_Intensity %>%
  full_join(ER_Emotions_Wider) %>%
  full_join(PPTS)

```

#### Visualization

```{r}
ggplot(final_data, aes(x = ER_SUM_ALL, y = PPTS_SUM )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Interpersonal_Manipulation_PPTS, y = ER_SUM_ALL)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 


ggplot(final_data, aes(x = Egocentricity_PPTS, y = ER_SUM_ALL)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Cognitive_Responsiveness_PPTS, y = ER_SUM_ALL )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = Affective_Responsiveness_PPTS, y = ER_SUM_ALL )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

```

#### Correlation

```{r}
ppts.er <- cor.test(x = final_data$ER_SUM_ALL, y = final_data$PPTS_SUM, method = 'spearman')
ppts.er



cor.test(x = Intensity.PPTS$HIGH_ER_SUM, y = Intensity.PPTS$PPTS_SUM, method = 'spearman')


# r and p values

cor.ppts.er <- rcorr(as.matrix(PPTS_ER), type = "spearman")
cor.ppts.er 
```

## 7. MET and QCAE

#### Visualization

```{r}
ggplot(final_data, aes(x = MET_SUM_CE, y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = MET_MEAN_AE , y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))


ggplot(final_data, aes(x = MET_SUM_CE, y = QCAE_AE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

ggplot(final_data, aes(x = MET_MEAN_AE  , y = QCAE_CE )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

```

#### Correlation

```{r}
met.qcae.ae <- cor.test(x = final_data$MET_MEAN_AE, y = final_data$QCAE_AE, method = 'spearman')
met.qcae.ae 

met.qcae.ce <- cor.test(x = final_data$MET_SUM_CE, y = final_data$QCAE_CE, method = 'spearman')
met.qcae.ce 


cor.test(x = final_data$MET_SUM_CE, y = final_data$QCAE_AE, method = 'spearman')

cor.test(x = final_data$MET_MEAN_AE, y = final_data$QCAE_CE, method = 'spearman')



```

#### **Regression**

```{r}
model14 <- lm(MET_SUM_CE ~ 1, data = final_data)
model15 <- lm(MET_SUM_CE ~ QCAE_CE + Gender + Age, data = final_data)

stepwise_8 <- step(model14, scope = list(upper = model15),
                           direction = "both")

check_model(model15)
durbinWatsonTest(stepwise_8)
vif(stepwise_8)
confint(stepwise_8)

summary(stepwise_8)


------------------------

model16 <- lm(MET_MEAN_AE ~ 1, data = final_data)
model17 <- lm(MET_MEAN_AE ~ QCAE_AE + Gender + Age, data = final_data)

stepwise_9 <- step(model16, scope = list(upper = model17),
                           direction = "both")

check_model(stepwise_9)
durbinWatsonTest(stepwise_9)
vif(stepwise_9)
confint(stepwise_9)

summary(stepwise_9)

------------------------

model18 <- lm(MET_MEAN_AE ~ 1, data = final_data)
model19 <- lm(MET_MEAN_AE ~ QCAE_CE + Gender + Age, data = final_data)

stepwise_10 <- step(model18, scope = list(upper = model19),
                           direction = "both")

check_model(stepwise_10)
durbinWatsonTest(stepwise_10)
vif(stepwise_10)
confint(stepwise_10)

summary(stepwise_10)







```

