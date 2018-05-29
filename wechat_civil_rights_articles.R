###########################
#### WeChat    ####
#### January 29th 2018 ####
#### unny Shao    ####
###########################


# install.packages("tmcn")
# install.packages("tm")
# install.packages("NLP")
# install.packages("jiebaR")
# install.packages("jiebaRD")
library(devtools)
# slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
# install_url(slam_url)
# install_github("psolymos/pbapply")
library(pbapply)
library(tmcn)
library(NLP)
library(slam)
library(tm)
library(jiebaRD)
library(jiebaR)
library(quanteda)
library(topicmodels)
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library(rvest)
library(tidyverse)
library(readxl)

#I wasn't sure if the associating of words would be a good way to generate topics in Chinese
#So I firstly try to do that just based on the article titles

#Set WD
setwd("~/Dropbox/ucr/2018spring/POSC207/text_analysis")
#Read in WeChat data (collected using ifonbox)
Data<-readxl::read_xlsx("~/Dropbox/ucr/2018spring/POSC207/text_analysis/civil_rights_articles.xlsx")
#break each sentense into segment of words (meaningful group of characters)
Data$title <- segmentCN(Data$title, 
                        package = c("jiebaR", "Rwordseg"),
                        nature = F,
                        nosymbol = T, 
                        useStopDic = F, 
                        returnType = c("vector", "tm"))
#turn this back to character
Data$title <- as.character(Data$title)
#turn into Chinese Character term matrix
dta <- createDTM(DataSub$words, language = c("zh", "en"),
          tokenize = NULL, removePunctuation = TRUE,
          removeNumbers = TRUE, removeStopwords = TRUE)
#turn it back to matrix
trim_dfm <- as.dfm(dta)
trim_mx_dfm <- as.matrix(trim_dfm)

#create topic (5)
k <- 5
alpha <- 1/k
burnin <- 1000
iter <- 1000
keep <- 50
thin <- 500
seed <- 335050

#not sure the difference between Gibbs and VEM here
fitted <- LDA(trim_mx_dfm, k = k, method = "Gibbs",
              control = list(alpha=alpha, burnin = burnin,
                             iter = iter, keep = keep, seed=seed) )

get_terms(fitted, k=10)
#This actually make a lot of sense

############################################################################################
############################################################################################
#topic 1 this is most of the breaking news, major death or events, usually involve China or Chinese
#keywords: people, china, breaking news, major death

#topic 2 This is about Trump, immigration, America, California and custom
#keywords: trump, president, immigration, American, California, major, Entering the boarder

#topic 3 This is about Asian American ethnic data disaggregation
#keywords: support, data disaggregation, America, Asian American, Chinese American, no, trump, rights

#topic 4 This is about Chinese American, foreign Chinese students
#keywords: Chinese American, fellow Chinese, Country, Student, Chinese F1 students

#topic 5 This is about Muslim, religion, extremeism
#keywords: America, China, Muslim, blend-in, religion, extremeism
############################################################################################
############################################################################################


#Now I want to get all the articles using the URL I got from each article and run the same topic modeling with full text
#Set WD
setwd("~/Dropbox/ucr/2018spring/POSC207/text_analysis")
#Read in URLS:
Data<-readxl::read_xlsx("~/Dropbox/ucr/2018spring/POSC207/text_analysis/civil_rights_articles.xlsx")
DataSub<-Data[1:364, ]
#text of each article by going into each link
#wrap it around a pblapply so I get a progress bar (just don't want to wait in the dark)
op <- pboptions(type = "timer")
system.time(text<-pblapply(as.character(DataSub$url),function(i)Whatever <-read_html(i) %>%
               html_nodes("span") %>%
               html_text()))
#Read in chinese:
df_utf8_ch <- lapply(text, as.character)
#Add to existing dataframe:
DataSub$text<-as.data.frame(noquote(df_utf8_ch))
head(DataSub$text)
#set the column with scrapped text as character
DataSub$text <- as.character(DataSub$text)
#seperate sentense into words (meaningful groups of Chinese characters)
DataSub$words <- segmentCN(DataSub$title, package = c("jiebaR", "Rwordseg"), nature = F,
          nosymbol = T, useStopDic = F, returnType = c("vector", "tm"))
DataSub$words <- as.character(DataSub$words)
#save this step into a Rdata just in case R crashes
save(DataSub, file="civil_rights_100.Rdata")



#turn into Chinese Character term matrix
dta <- createDTM(DataSub$words, language = c("zh"),
          tokenize = NULL, removePunctuation = TRUE,
          removeNumbers = TRUE, removeStopwords = TRUE)




#turn it into dfm
trim_dfm <- as.dfm(dta)
topfeatures(trim_dfm, n=20)
#a lot of words actually does not make sense

#remove words
remove <- c(" “ ", ",", "国家", "主义", "发声", "做", "发", "认为",  "做", "平台", "事件", 
            "表示", "朗", "n", "r", "civil", "rights", "年", "普", "美国", "the", "and", 
            "1", "2", "长", "加", "月",  "华人", "都", "更", "人", "不", "号", "交流", 
            "说", "上", "投稿", "后", "没有", "信息", "公众", "关注", "出来", "阅读", 
            "没有", "读者", "中", "还会", "很", "二维码", "原文", "支持", "年", "不是", 
            "还会", "工作", "获得", "为主", "华", "声音", "微", "一个", "right", "a", 
            "com", ".com", "图", "两",  "里", "we",  "川", "信", "主", "of", "to", 
            "已经",  "civilrights", "civilright", "read", "more", "提示", "长", 
            "下图", "微信", "扫描")

# turn back into a matrix and remove those words
trim_mx_dfm <- as.matrix(trim_dfm)
trim_mx_dfm <- trim_mx_dfm[,!colnames(trim_mx_dfm) %in% remove]
trim_dfm <- as.dfm(trim_mx_dfm)
topfeatures(trim_dfm, n=20)

#create topic
k <- 5
alpha <- 1/k
burnin <- 1000
iter <- 1000
keep <- 50
thin <- 500
seed <- 335050


fitted <- LDA(trim_mx_dfm, k = k, method = "Gibbs",
              control = list(alpha=alpha, burnin = burnin,
                             iter = iter, keep = keep, seed=seed) )
topic <- get_terms(fitted, k=10)
get_terms(fitted, k=5)
#at this step, there are still a lot of words that does not make sense
#basically I realize that I need to remove as many verb and adj as possible to figure out the topic

#generate another list of remove words
remove <- c("\"", "信号", "our", "政府", "加州", "编", "媒体",
  "新", "想", "拉", "日", "全球", "in", "朋友",
  "码", "元", "奥", "10", "听到", "马", "私人", "种族", "请",
  "非常", "戳", "http", "想", "公司", "3", "介绍", "留意",
  "需要", "之后", "纽约", "饶", "联邦", "家", "合作", "知道", 
  "及时", "化", "继续", "世界", "朋友", "及时", 
  "场", "制造", "维", "可能", "希望", "相关", "文", 
  "霾", "重大", "万",  "问题", "6", "c", "州", 
  "现在", "斯", "will", "msa", "ourcivilright", 
  "文章", "群", "现在", "进行", "目前", 
  "资本", "维", "今天", "买", "裔", "全球", "相信", 
  "看到", "人民", "安全", "所谓", "5", "5", "不能", "国", 
  "媒体", "outlook", "重要", "方面", "接受", "必须",  
  "email", "曾经", "名", "城市", "保险", "be", "州", 
  "is", "写", "参与", "不同",  "称", "这种", "洛杉矶", 
  "应该", "直接", "c",  "for", "时间", "钱", "this", 
  "问", "很多", "视频",  "直接", "3", "吃", "老",  "觉得", 
  "几乎", "4", "出", "真正", "功能", "11", "镇", "is", "主流", "成", 
  "i", "可能", "c", "位", "空气", "美元", "反", "正确", "历史",
  "正在", "决定", "关系", "性", "生活", "正确", "不会", "20", 
  "雾", "成为", "一位", "宝",  "克", "帮助", "本文", "分子", 
  "独立", "过去", "群体",  "很多",  "提供",  "洛杉矶", "工人",  
  "高", "not", "一年", "拉", "猛",  "化", "性", "问题", "投", 
  "america", "第一", "应该", "成为", "背景", "一次",  
  "黄金", "几个", "令人", "at", "提到", "发现","当时", "努力", 
  "网友", "世界", "as", "12", "真的", "要求", "维", "日", "可能", "码", "化", "3",
  "our", "很多", "现在", "新", "信号", "介绍", "想", "世界", "听到", "重大",
  "私人", "文", "c", "及时", "知道", "功能", "留言", "5", "斯", "万", "直接", 
  "will", "编", "饶", "拉", "没", "特别", "事情", "点", "美", "that", "发生", 
  "it", "发布", "事", "机会", "得到", "赏", "you", "卡", "进入", "先生", "最后", "看看", "u0001f44a", ".",
  "7", "太", "能够", "原创", "8", "www", "找")

# Remove those words
trim_mx_dfm <- trim_mx_dfm[,!colnames(trim_mx_dfm) %in% remove]
trim_dfm <- as.dfm(trim_mx_dfm)
#save this version as an Rdata file
save(trim_mx_dfm, file="trim_mx_dfm.Rdata")

fitted <- LDA(trim_mx_dfm, k = k, method = "Gibbs",
              control = list(alpha=alpha, burnin = burnin,
                             iter = iter, keep = keep, seed=seed) )
topic <- get_terms(fitted, k=10)
get_terms(fitted, k=5)

#this time it makes more sense
#I actually also tried to sort into 10 topics and 15 topics, and it seems that sorting into
#5 topics makes most of the sense here

############################################################################################
############################################################################################
#topic 1 this is about Asian American Interests and Islam
#keywords: politics, democracy, Asian American, Interest, investigation, president, Islam

#topic 2 This is about discrimination, Chinese students when taking about illegal immigration
#keywords: Against, Student, Majority, illegal, protection, discrimination, refugees

#topic 3 Thi is a topic about party, election and terrorist attacks (mainly involving cars)
#keywords: party, election, terror, car, segregation, president, university, volunteers, reporter

#topic 4 This is a topic about party election with news on immigration
#keywords: party, society , professor, elected, news, immigration, report, facts

#topic 5 This is a topic about muslim, immigration and welfare system
#keywords: Muslim, immigrants, China, donation, Islam, White people, welfare, religion, president
############################################################################################
############################################################################################
