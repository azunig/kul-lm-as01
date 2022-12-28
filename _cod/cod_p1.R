library(tidyverse)
library(gridExtra)
library(multcomp)
library(car)
library(corrplot)


#getwd()
fol_main <- '/Users/aronvz/Documents/github/kul-lm-as01/'
fol_src  <- '_src/'
fol_out  <- '_out/'
fil_raw  <- 'salary.gender.txt'
setwd(fol_main)

salary.gender.txt <- read.table(paste0(fol_src,fil_raw), 
                                header=TRUE, 
                                skip = 27,
                                quote="\"", sep=",")

summary(salary.gender.txt)
#Salary-Gender-Education 

salary.gender.txt01 <- 
  transform(salary.gender.txt,
            f.gender = as.factor(Gender), 
            f.education = as.factor(Education))

summary(salary.gender.txt01)
#Salary-Gender-Education __ f.gender-f.education

levels(salary.gender.txt01$f.gender)
levels(salary.gender.txt01$f.education)

#Exploratory Data Analysis

# Distribution of response variable - Salary
p_ht1 <- ggplot(salary.gender.txt01, aes(Salary)) + 
  geom_histogram(colour = 'green', fill = 'blue', bins=30) + 
  ggtitle('Distribution of Response variable') + 
  xlab('Response variable - Salary') + 
  theme_bw()

# Distribution of explanatory variable - f.gender
p_br1 <- ggplot(salary.gender.txt01, aes(f.gender)) + 
  geom_bar(colour = 'blue', fill = NA) + 
  ggtitle('Distribution of Gender') + 
  xlab('Explanatory variable - Gender') + 
  theme_bw()

# Distribution of explanatory variable - f.education
p_br2 <- ggplot(salary.gender.txt01, aes(f.education)) + 
  geom_bar(colour = 'red', fill = NA) + 
  ggtitle('Distribution of Education') + 
  xlab('Explanatory variable - Education') + 
  theme_bw()

grid.arrange(p_ht1, p_br1, p_br2, ncol = 3)

# Marginal distributions of Salary Gender-Education 
# Salary-Gender-Education 
box1 <- ggplot(salary.gender.txt01, aes(f.gender, Salary)) + 
  geom_boxplot(aes(colour = Gender), size = 0.8) + 
  geom_jitter(aes(colour = Gender), size = 0.7) +
  xlab('Gender') + ylab('Salary') + 
  ggtitle('Distribution of Salary - Gender') + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13))

box2 <- ggplot(salary.gender.txt01, aes(f.education, Salary)) + 
  geom_boxplot(aes(colour = Salary), size = 0.8, outlier.colour = 'black') + 
  geom_jitter(aes(colour = Salary), size = 0.7) +
  xlab('Education') + ylab('Salary') + 
  ggtitle('Distribution of Salary - Education') + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13))
grid.arrange(box1, box2, ncol = 2)

# Distribution groups - Same scale
box3 <- ggplot(salary.gender.txt01, aes(Salary, f.gender)) + 
  geom_boxplot(colour = rep(c('red', 'blue'), 2), ) + 
  facet_wrap(~ f.education, nrow = 2) + 
  xlab('Salary') + ylab('Gender') + 
  ggtitle('Distribution of Salary - Gender') + 
  theme_bw() +
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        strip.text = element_text(size = 13))

box4 <- ggplot(salary.gender.txt01, aes(Salary, f.gender)) + 
  geom_boxplot(colour = rep(c('red', 'blue'), 2), ) + 
  facet_wrap(~ f.education, nrow = 2, scale = 'free') + 
  xlab('Salary') + ylab('Gender') + 
  ggtitle('Distribution of Salary - Gender') + 
  theme_bw() +
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        strip.text = element_text(size = 13))

grid.arrange(box3, box4, ncol = 2)


# Mean of response variable 
table_unbal_mean <- with(salary.gender.txt01, 
                         tapply(Salary, list(f.gender, f.education), 
                                mean))
table_unbal_mean %>% round(2) %>% as.table()

# Frequency table of response variable by workload and salary
table_unbal_freq <- with(salary.gender.txt01, 
                         table(f.gender, f.education))
table_unbal_freq

# Data in long format
data_longer <- table_unbal_mean %>% as.data.frame()
data_longer$`f.gender` <- row.names(data_longer)
data_longer <- data_longer %>% 
  pivot_longer(cols = c(Degree, `No degree`),
               names_to = 'degree',
               values_to = 'mean_y')

# Interaction plot
int1 <- ggplot(data_longer, aes(degree, mean_y)) + 
  geom_line(aes(group = f.gender, color = f.gender), size = 1) + 
  xlab('Gender') + 
  ylab('Mean value of Salary') + 
  ggtitle('Interaction Plot') + theme_bw() + 
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 13))

int2 <- ggplot(data_longer, aes(f.gender, mean_y)) + 
  geom_line(aes(group = degree, color = degree), size = 1) + 
  xlab('Salary') + 
  ylab('Mean value of Salary') + 
  ggtitle('Interaction Plot') + theme_bw() +   
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 13))

grid.arrange(int1, int2, ncol = 2)

## https://rpubs.com/mhanauer/300976

# Balanced data
result.balance <- aov(lm(Salary~f.gender*f.education, data = salary.gender.txt01))
summary(result.balance)

# the p-value of gender is 0.271 (no significant), which indicates that the levels of gender are not associated with significant different salaries.
# the p-value of education is < 2e-16 (significant), which indicates that the levels of educations are associated with significant different salaries.
# the p-value for the interaction between gender*education is 0.537 (no significant), which indicates that the relationships between gender and education does not depends on the salary.

# Unbalanced data
#result.Unbalance <- Anova(lm(Salary~f.gender*f.education, data = salary.gender.txt01), type="III")
#result.Unbalance

# the p-value of gender is 0.00258 (significant), which indicates that the levels of gender are associated with significant different salaries.
# the p-value of education is 3.707e-15 (significant), which indicates that the levels of educations are associated with significant different salaries.
# the p-value for the interaction between gender*education is 0.537 (no significant), which indicates that the relationships between gender and education does not depends on the salary.