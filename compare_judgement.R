library(sirt)
library(tidyverse)

## simulate answers
answers <- tibble(student = paste0("s", 101:148))
answers <- answers %>% 
  mutate(truescore = rnorm(length(answers$student)))
answer_lkp <- answers$truescore
names(answer_lkp) <- answers$student
## look at the total number of combinations of student pairs
pot_comb <- combn(answers$student, 2) %>% as.data.frame() %>% t() 
colnames(pot_comb) <- c("s1", "s2")
pot_comb <- pot_comb %>% 
  as_tibble()

# simulate comparisons
## select a judge for each pair; need 12.5 per student if each pair compared once
## will occassionally get same student
needed <- nrow(pot_comb)
needed_each <- needed/length(answers$student)
judges <- rep(answers$student, round(needed_each,0) + 1)
judges <- judges[1:needed]
pot_comb <- pot_comb %>% 
  mutate(judge = rev(judges))

## count number of times each student is marked; each is marked 47 times
table(c(pot_comb$s1, pot_comb$s2))
# count number of times each student marks self
pot_comb <- pot_comb %>% 
  mutate(mark_self = judge == s1 | judge == s2)
# once or mat most twice
table(pot_comb$judge[pot_comb$mark_self])
## count number of times each student marks another student

# simualte judgement of comparisons
## calculated based on simualated (known) true score
pot_comb <- pot_comb %>% 
  mutate(judgement = case_when(
    answer_lkp[s1] > answer_lkp[s2] ~ 1,
    answer_lkp[s2] > answer_lkp[s1] ~ 0,
    TRUE ~ 0.5
  ))

pot_comb_frmt <- pot_comb %>% 
  select(s1, s2, result = judgement) %>% 
  as.data.frame()


mod1 <- btm(pot_comb_frmt, judge = pot_comb$judge, maxit = 400, fix.eta = 0, ignore.ties = TRUE)
summary(mod1)

## able to recover original true ranking based on truescore
all(mod1$effects$individual ==  answers %>% arrange(desc(truescore)) %>% pull(student))




