---
title: "Data2002 Module Report 2"
author: '470518795'
date: "20/09/2019"
output:
  html_document:
    code_folding: hide
    css: https://use.fontawesome.com/releases/v5.2.0/css/all.css
    fig_caption: yes
    number_sections: no
    self_contained: yes
    theme: flatly
    toc: yes
    toc_depth: 2
    toc_float: yes
  pdf_document:
    toc: yes
    toc_depth: '2'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library("tidyverse")
library("lubridate")
library("kableExtra")
library("forcats")
library("readr")
library("dplyr")
library("janitor")
library("ggplot2")
remotes::install_github("ropenscilabs/gendercodeR",force = TRUE)
library("gendercodeR")
```

<style>
h1.title {
  font-size: 44px;
}
blockquote {
    border-left:none;
    font-size: 1em;
    display: block;
    margin-top: .25em;
    margin-left: 20px;
}
.table{
    margin-bottom:0px;
}
h1 {
    color:	#e64626;
    font-size: 32px;
}
h2 {
    font-size: 22px;
}
h3 {
    font-size: 21px;
}
h4 {
    font-size: 17px;
}
.list-group-item.active, .list-group-item.active:hover, .list-group-item.active:focus{
    background-color:#e64626;
}
h2, h3, h4, h5, h6 {
    color: black;
}
h1, h2, h3{
font-weight:bold;
}
a { 
    color: #0148A4; 
} 
body { 
    color: #424242; 
}
</style>

# Introduction
The survey conducted by Dr.Garth is about the different characteristics as well as social and academic activities of student enrolled in data20X2 in 2019 semester 2 at University of Sydney. The data from this survey is collected from 110 enrolled students.


# Importing data
```{r import data,message=FALSE}
mydata=readr::read_csv("data/DATA2X02 class survey.csv",na=c(""," "))
```

# Data cleaning
```{r}
clean_data<-janitor::clean_names(mydata)
# Using the function to clean the column names
# however, there are some columns which are quite long, it can be cleaned further.
new_colnames<-c("survey_time","user_name","gender","home_postcode","previous_statistics_courses",
                "number_of_clubs_attended","last_time_went_to_dentist","study_time","social_media",
                "number_of_siblings","pet","live_with_parents","hours_of_exercise_per_week","eye_color","work_time","favourite_season","shoe_size","height","frequency_of_floss_teeth","with_glasses","dominant_hand","steak_cooked_preference")
colnames(mydata)<-new_colnames
colnames(mydata)
```

## Eye color
```{r warning=FALSE,message=FALSE}
mydata=mydata %>% mutate(eye_color=stringr::str_to_title(eye_color),
                         eye_color=case_when(eye_color=="Balack"~"Black",eye_color=="Hazelnut"~"Hazel",TRUE~eye_color))
mydata = mydata %>% 
  mutate(
    eye_color = forcats::fct_lump_min(eye_color, min = 2)
  )
mydata %>% ggplot(aes(x = eye_color)) + geom_bar(fill = c("black", 
    "blue", "brown2", "brown4", "green3", "darkgoldenrod3", "red")) + 
    labs(title = "eye_color", y = "Count", 
        x = "Eye color") + theme_linedraw() + coord_flip()
mydata %>% tabyl(eye_color) %>% adorn_pct_formatting() %>% kable()

```

## Gender
```{r}
recoded_gender=recode_gender(gender=mydata$gender)
mydata$gender=recoded_gender
```

## Social media
```{r}
mydata$social_media=mydata$social_media %>% tolower()
mydata$social_media=forcats::fct_recode(mydata$social_media,
                                        "facebook"="facebook messenger",
                                        "facebook"="fb",
                                        "instagram"="ig",
                                        "none"="i never use social media.")
mydata$social_media=forcats::fct_lump(mydata$social_media,n=7)
mydata %>% tabyl(social_media) %>% adorn_pct_formatting()%>%kable()
```


## Height
```{r}
mydata = mydata %>% mutate(
  height =case_when(
    height<2.2 ~ height*100,
    height<4 ~ NA_real_,
    height<8 ~height *30.48,
    height>250 ~NA_real_,
    TRUE ~ height)
)
```

## Exercise
```{r}
mydata$hours_of_exercise_per_week<-as.numeric(mydata$hours_of_exercise_per_week)
mydata=mydata %>%mutate(
  hours_of_exercise_per_week=case_when(
    hours_of_exercise_per_week>7*24 ~ NA_real_,
    TRUE ~ hours_of_exercise_per_week
  )
)
as.numeric(mydata$hours_of_exercise_per_week)
```

## Save the cleaned data
```{r}
readr::write_csv(mydata,"mydata_cleaned.csv")
```


# Data analysis
## 1: Is this a random sample of DATA2002 students?

This survey is not random sample.  Student who completed this survey is actively interact with lecturer, however, not all response to this survey. Therefore, samples only can represent a part of active DATA2X02 students, it cannot represent the whole population. This survey suffers from selection bias. Those who do not respond survey is inactive comparing with those who complete it, certain group(inactive students) are under-represented. The probability of completing survey is not the same across the students, therefore the sample is not random. 


## 2: What are the potential biases in this data generation?

There might be selection bias, people answering this survey is likely to be more active. Some inactive students do not answer the survey. Therefore, the sample does not acurately represents the population.

There also likely to exists measurement bias. For example, if it is hard to identify whether student studying 30 hours per week or 30 min if he just type 30 in the survey. Also , there are some people who study more than 144 hours one week, which is unrealistic. This is because some people does not take this survey seriously. Therefore, there exists measurement bias in the dataset.

Another measurement bias would be on study time, student may tend to overestimate their study time since they may feel inappropriate to study less than certain amount of time. Since we can not detect the truth and there is no penalty or award for eliciting the real time of study. Therefore, this dataset is not incentive compatible.

## 3: Which variables are most likely to be subjected to this bias?

In this survey, there are several biases, and the free-response variables are most likely to be subjected to this bias because the answers of these questions often have unrelated or even impossible answers, such as exercise time, study time. For example, it is impossible for a person to study more than 144 hours per week. These kind of response of those variables could even be outliers there, which would definitely lead to bias in this survey.

 Besides, the variable that did not clarify the standard measurement units are also most likely to be subjected to this bias, such as height(it could be measured in cm or m), study hours(it could be measured in minutes or hours), shoe size (it is different in different country). For example, the height, someone use cm as measurement unit (for example, 178) and someone use m as measurement unit here (for example, 1.89).

## 4: Is there any evidence to suggest that there’s a difference in the amount of exercise done by people who eat red meat compared to people who don’t eat red meat?


```{r}
mydata$steak_cooked_preference=fct_collapse(mydata$steak_cooked_preference,
                                            eaters=c("Blue","Rare","Medium-rare","Medium","Medium-well done","Well done"),
                                            non_eaters=c("I don't eat red meat"))
exercise = mydata %>% filter(steak_cooked_preference=="eaters"||steak_cooked_preference=="non-eaters")%>% select(steak_cooked_preference,hours_of_exercise_per_week)

ggplot(subset(exercise,!is.na(steak_cooked_preference)),aes(x=steak_cooked_preference,y=hours_of_exercise_per_week))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "The hours of exercise per week for red meat eaters and non-eaters",
       x="Red meat eater?",
       y="Hours exercise per week")

```


This is a two sample t-test.

Firstly, looking at the pooled two sample t-test, the assumption here is there have equal variance. By calculation, it can be seen that the variance differences among the sample is approximately 1. By test of equal variance assumption, p-value is much more than 0.05. Therefore, we have sufficient evidence to assume equal variance.
```{r Check for equal variance assumption}

eaters=exercise %>% filter(steak_cooked_preference=="eaters",as.numeric(hours_of_exercise_per_week)) %>% drop_na
sd_eaters=sd(eaters$hours_of_exercise_per_week)
non_eaters=exercise %>% filter(steak_cooked_preference=="non_eaters",as.numeric(hours_of_exercise_per_week)) %>% drop_na
sd_noneaters=sd(non_eaters$hours_of_exercise_per_week)
c((sd_eaters)^2,(sd_noneaters)^2)
var.test(eaters$hours_of_exercise_per_week,non_eaters$hours_of_exercise_per_week)

```

Secondly, it is assumed that the distribution of these data is normal. This assumption cen be checked by drawing QQ-plot to see wehther it is approximate straight line. By looking at two diagram, it is obvious that the sample is quite fit with theoretical line. The only problem is that we have only few samples of non-eaters. Therefore, it is reasonable to assume hours of exercise is normally distributed.

```{r}
par(mfrow=c(1,2))
qqnorm(eaters$hours_of_exercise_per_week)
qqline(eaters$hours_of_exercise_per_week)
qqnorm(non_eaters$hours_of_exercise_per_week)
qqline(non_eaters$hours_of_exercise_per_week)
```

It is suggested that the pooled two sample t-test should be used in this analysis since the equal variance assumption is statisfied.

1： Hypothesis: $H_0: u_{eaters} = p_{non-eaters}$ vs  $H_1: u_{eaters}\neq u_{non-eaters}$
Null hypothesis: There is no difference in hours of exercise per week between red meat eaters and non-eaters
Alternative: The hours of exercise between these two groups are different.

2： Assumption: $X_1, ..., X_{n_x}$ are iid with $N(\mu_X,\sigma^2)$, $Y_1, ..., Y_{n_x}$ are iid with $N(\mu_Y,\sigma^2)$ and $X_i$ are independent of $Y_i$

3： Test statistic: $T=\frac{\bar{X} - \bar{Y}}{S_p\sqrt{\frac{1}{n_x} + \frac{1}{n_y}}}$, where $S_p^2=\frac{(n_x-1)S_{x}^2+(n_y-1)S_{y}^2}{n_x+n_y-2}$. Under $H_0$, $T\sim t_{n_x+n_y-2}$

4:  Observed test statistic: $t_0=.01050989$

5:  p-value: $p=2P(T\geq t_0) = 0.9916391$ .

6:  Decision: Since the p-value is approximately 0.99, this is not significant at 1% significance level, therefore, we do not reject null hypthothesis that there is no difference on hours of exercise per week between red meat eaters and non-eaters.


```{r}
n1=nrow(eaters)
n2=nrow(non_eaters)
n1
n2
Sp=sqrt(((n1-1)*(sd_eaters)^2+(n2-1)*(sd_noneaters)^2)/(n1+n2-2))
t_0=(mean(eaters$hours_of_exercise_per_week)-mean(non_eaters$hours_of_exercise_per_week))/(Sp*sqrt(1/n1+1/n2))
t_0
(p=2*pt(t_0,df=n1+n2-2,lower.tail = FALSE))
t.test(eaters$hours_of_exercise_per_week,non_eaters$hours_of_exercise_per_week,alternative="two.sided",var.equal = TRUE)

```

As mentioned, the only concern is that for non-eaters, there are only few samples. It is doubt that the normality assumption is hold. To make our analysis more robust, we can use Wilcoxon rank-sum test which relaxes on normality assumption.




Wilcoxon rank-sum test


1: Hypothesis: $H_0: \mu_x = \mu_y$ vs $H_1: \mu_x \neq \mu_y$

2: Assumption: $X_1, ..., X_{n_x}$ and $Y_1, ..., Y_{n_x}$ are iid and follow the same distribution but differ by a shift.

3: Test statistics: $W = R_1+R_2+...+R_{n_x}$. Under $H_0$, $W \sim WRS(100, 7)$ distribution

4: Observed test statistic: $w=r_1+r_2+...+r_n_x=304.5$

5: P-value: 
$E(W)=\frac{100(107+1)}{2}=5400$

$Var(W)=\frac{100*7}{107(107-1)}(\sum_{i=1}^{N}(r^2_i)-\frac{107(107+1)^2}{4})$

$p\approx 2P(Z\geq |\frac{W-E(W)}{\sqrt{Var(W)}}|) =0.5638$

6: Decision: As the p-value is much more than 0.05, there is insufficient evidence to reject $H_0$.

.
```{r}
exercise$r=rank(exercise$hours_of_exercise_per_week)
exercise_sum = exercise %>%
  drop_na%>%
  dplyr::group_by(steak_cooked_preference) %>% 
  dplyr::summarise(
  w = sum(r),
  xbar = mean(hours_of_exercise_per_week),
  s = sd(hours_of_exercise_per_week),
  n = n()
)
exercise_sum
na = exercise_sum$n[exercise_sum$steak_cooked_preference == "eaters"]
nb = exercise_sum$n[exercise_sum$steak_cooked_preference == "non_eaters"]
N = na + nb # total number of observations
na
nb
c(na, nb, N)
w = exercise_sum$w[exercise_sum$steak_cooked_preference== "eaters"]
EW = na * (N + 1)/2
c(w, EW)
sumsqrank = sum(exercise$r^2)
g = N * (N + 1)^2/4
varW = na * nb * (sumsqrank - g)/(N * (N - 1))
t0 = (w - EW)/sqrt(varW)
t0
wilcox.test(hours_of_exercise_per_week~steak_cooked_preference,data = exercise,correct = FALSE,alternative = "two.sided")
```


5: Is there any evidence that the weekly exercise time of students who participate in more than 3 univeristy clubs is different to those who don’t?
```{r}

```

6: Is there evidence that students who live with their parents study more hours per week than students who don’t live with their parents?
```{r}

```

7: What other questions could you ask? These might also be questions we learnt to answer in Module 1 regarding the relationship between categorical variables - that’s OK, you might use the group work discussion from Lab 1C to help think of other questions to ask.

Is handedness independent of wearing glasses?

```{r}
handedglasseschi <- mydata %>%
  select(with_glasses, dominant_hand) %>%
  drop_na() %>%
  group_by(with_glasses, dominant_hand) %>%
  count() %>%
  spread(key = dominant_hand, value = n) %>%
  column_to_rownames(var = "with_glasses")
handedglasseschi

n <- sum(handedglasseschi)
r <- rowSums(handedglasseschi)
c <- colSums(handedglasseschi)
e <- r %*% t(c) / n
paste("The expected cell count assumption is", valid <- all(e >= 5))
#It can be seen that the assumption needed for chi-squared test does not hold, therefore, it is recommended to use Fisher exact test.
fisher.test(handedglasseschi)
```
The p-value is 0.7818 which is quite large comparing to 0.05. Therefore, we have sufficient evidence to claim that wearing glasses is independent of handedness.


```{r}
ggplot(data = mydata %>%drop_na() ,
       aes(x = dominant_hand, fill = with_glasses) )+
  geom_bar(position = "fill") +
  labs(x = "Handedness", 
       y = "Proportion", 
       fill = "wearing_glasses",
       title = "Distribution of handedness and gender",
       caption = "Source: Data2002 survey") +
  theme_bw()
```
It can be seen that while there were roughly the same amount of left-handed wearing glasses and without glasses, the right-handed subpopulation tended to be skewed towards wearing glasses and the left-handed subpopulation tended to be skewed towards without glasses.


Is there any evidence that there is an difference on hours of exercise per week between male and female?
```{r}
gender = mydata %>% filter(mydata$gender=="male"||mydata$gender=="female")%>% select(gender,hours_of_exercise_per_week)
ggplot(subset(gender,!is.na(hours_of_exercise_per_week)),aes(x=gender,y=hours_of_exercise_per_week))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "The hours of exercise per week for red meat eaters and non-eaters",
       x="Red meat eater?",
       y="Hours exercise per week")

```

This is a two sample t-test.

Firstly, looking at the pooled two sample t-test, the assumption here is there have equal variance. By calculation, it can be seen that the variance differences among the sample is approximately 1. By test of equal variance assumption, p-value is much more than 0.05. Therefore, we have sufficient evidence to assume equal variance.
```{r  equal variance assumption}
male=gender %>% filter(gender=="male",as.numeric(hours_of_exercise_per_week)) %>% drop_na

sd_male=sd(male$hours_of_exercise_per_week)
female=gender %>% filter(gender=="female",as.numeric(hours_of_exercise_per_week)) %>% drop_na
sd_female=sd(female$hours_of_exercise_per_week)
c((sd_male)^2,(sd_female)^2)
var.test(male$hours_of_exercise_per_week,female$hours_of_exercise_per_week)

```

Secondly, it is assumed that the distribution of these data is normal. This assumption cen be checked by drawing QQ-plot to see wehther it is approximate straight line. By looking at two diagram, it is obvious that the sample is quite fit with theoretical line. The only problem is that we have only few samples of non-eaters. Therefore, it is reasonable to assume hours of exercise is normally distributed.

```{r}
par(mfrow=c(1,2))
qqnorm(male$hours_of_exercise_per_week)
qqline(male$hours_of_exercise_per_week)
qqnorm(female$hours_of_exercise_per_week)
qqline(female$hours_of_exercise_per_week)
```


As mentioned, the only concern is that for non-eaters, there are only few samples. It is doubt that the normality assumption is hold. To make our analysis more robust, we can use Wilcoxon rank-sum test which relaxes on normality assumption.

Wilcoxon rank-sum test


1: Hypothesis: $H_0: \mu_x = \mu_y$ vs $H_1: \mu_x \neq \mu_y$

2: Assumption: $X_1, ..., X_{n_x}$ and $Y_1, ..., Y_{n_x}$ are iid and follow the same distribution but differ by a shift.

3: Test statistics: $W = R_1+R_2+...+R_{n_x}$. Under $H_0$, $W \sim WRS(100, 7)$ distribution

4: Observed test statistic: $w=r_1+r_2+...+r_n_x=304.5$

5: P-value: 
$E(W)=\frac{100(107+1)}{2}=5400$

$Var(W)=\frac{100*7}{107(107-1)}(\sum_{i=1}^{N}(r^2_i)-\frac{107(107+1)^2}{4})$

$p\approx 2P(Z\geq |\frac{W-E(W)}{\sqrt{Var(W)}}|) =0.5638$

6: Decision: As the p-value is much more than 0.05, there is insufficient evidence to reject $H_0$.

.
```{r}
gender$r=rank(gender$hours_of_exercise_per_week)
gender_sum = gender %>%
  drop_na%>%
  dplyr::group_by(gender) %>% 
  dplyr::summarise(
  w = sum(r),
  xbar = mean(hours_of_exercise_per_week),
  s = sd(hours_of_exercise_per_week),
  n = n()
)
gender_sum

na = gender_sum$n[gender_sum$gender== "male"]
nb = gender_sum$n[gender_sum$gender == "male"]
N = na + nb # total number of observations
na
nb
c(na, nb, N)
w = gender_sum$w[gender_sum$gender== "male"]
w
EW = na * (N + 1)/2
c(w, EW)
sumsqrank = sum(gender$r^2)
g = N * (N + 1)^2/4
varW = na * nb * (sumsqrank - g)/(N * (N - 1))
varW

wilcox.test(male$hours_of_exercise_per_week,female$hours_of_exercise_per_week,correct = FALSE,alternative = "two.sided")
```
