# https://docs.google.com/spreadsheets/d/1Gm3jTIHs2TGMXoz84_DyuTBd3auGY3iJ3jzjQNExl5Q/edit#gid=33209479
df <- read.csv("~/Desktop/poll1/data/R - Shiny User Survey (Responses) - Form Responses 1.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(dplyr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

alphaOnly <- function(x) gsub("[^[:alnum:] ]", "", x)

clean<-function(col){
  return(trim(tolower(alphaOnly(unlist(strsplit(col, split=","))))))
}

# Length of Use

table(df$How.long.have.you.been.using.R.)
ggplot(df, aes(x=How.long.have.you.been.using.R.)) + geom_bar()

# Role

table(clean(df$In.what.role.do.you.use.R.))

table(roleList(df$In.what.role.do.you.use.R.)) %>% as.data.frame() %>%
  arrange(desc(Freq)) %>% View()


ggplot(df, aes(x=In.what.role.do.you.use.R.)) + geom_bar() + coord_flip() 

df %>% group_by(In.what.role.do.you.use.R.) %>% filter(n() > 10) %>% 
  ggplot(aes(x=In.what.role.do.you.use.R., fill=osx)) + 
  geom_bar() + coord_flip() + facet_wrap(~ linux) 

# Platforms
table(clean(df$On.what.platform.s..do.you.run.R.))

ggplot(df, aes(x=On.what.platform.s..do.you.run.R.)) + 
  geom_bar() + 
  coord_flip()

ggplot(df, aes(x=Have.you.created.a.Shiny.Web.Application., 
               fill = On.what.platform.s..do.you.run.R. )) + 
  geom_bar()

# Web Tech
table(df$What.technologies.related.to.web.development.have.you.used.) %>% 
  as.data.frame() %>% unique() %>% View()

# Features to add
stopWords <- c('to', 'the', 'and', 'a', 'of', 'for', 'in', 'with', 'i', 'app', 'be', '', 'on', 'that', 'would', 'dont', 'can', 'data', 'it', 'know', 'like')
wordList = function (x){
  unlist(strsplit(clean(x), split=' '))
}


words <- wordList(df$What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications.) %>% 
  table() %>% as.data.frame() %>% arrange(desc(Freq)) 
  
colnames(words) <-c('word', 'freq')

words %>% filter(!word %in% stopWords) %>% View()

# Get Ideas, and filter
words %>% filter(grepl('auth',word)) %>% View()


# For context...                 
df %>% filter(grepl('auth|password', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()
df %>% filter(grepl('interactive', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()
df %>% filter(grepl('debug', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()
## ggvis, ggplot etc.
df %>% filter(grepl(' gg', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()
# This that could be easier
df %>% filter(grepl('eas', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()

df %>% filter(grepl('free', What.feature.s..would.you.like.to.see.fixed.or.added.to.Shiny.Applications., ignore.case = TRUE))  %>% View()


  


# Modify the data frame:
# ...Split out platforms

df <- df %>% mutate(
  windows=grepl('Windows',On.what.platform.s..do.you.run.R.),
  osx=grepl('Mac / OSX',On.what.platform.s..do.you.run.R.),
  linux=grepl('Linux',On.what.platform.s..do.you.run.R.)
)

df <- df %>% mutate(
  html=grepl('HTML',What.technologies.related.to.web.development.have.you.used.),
  javascript=grepl('JavaScript',What.technologies.related.to.web.development.have.you.used.),
  css=grepl('CSS',What.technologies.related.to.web.development.have.you.used.),
  serverside=grepl('Server Side Technologies',What.technologies.related.to.web.development.have.you.used.),
  no_web_tech=grepl('None',What.technologies.related.to.web.development.have.you.used.)
)



df %>% mutate(ds=In.what.role.do.you.use.R.=='University Student' )


# Questions
# What operating system is associated with folks who do or don't use web tech
df %>% group_by(no_web_tech) %>% select(windows, osx, linux) %>% 
  summarize(n(), sum(windows), sum(linux), sum(osx))

df %>% group_by(serverside) %>% select(windows, osx, linux) %>% 
  summarize(n(), sum(windows), sum(linux), sum(osx))


