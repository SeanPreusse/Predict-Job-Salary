# COMP5310 Project Stage 1 – Sean Preusse, spre7600
# Project: Classification Model to Predict Advertised Job Salary

setwd("C:/Users/sean/Desktop/project_workspace/abs_data")
setwd("H:/Desktop/project_workspace")

# Load requried Packages

library(gdata)
library(lubridate)
library(gsubfn)
library(splitstackshape)
library(sqldf)
library(dplyr)
library(stringr)

# INDEED Processing

date_extracted <- as.POSIXct("2016-04-14 7:30", format="%Y-%m-%d  %H:%M")

## Indeed Data Cleaning AUS


indeed_au <- read.csv('H:/Desktop/project_workspace/indeed/au.csv')
indeed_au$date_extracted <- date_extracted
indeed_au <- indeed_au[names(indeed_au) %in% c("date_extracted", "job_description", "job_title_with_link", "job_title_with_link._text", "published", "company", "location", "salary")]
names(indeed_au) <- c("url", "job_title", "published", "job_description", "company", "location", "salary", "date_extracted")
#names(indeed_au) <- c("url", "job_description", "company", "location", "job_title", "published", "salary", "date_extracted")


indeed_au <- sqldf("select *
                   ,CASE WHEN published like '%day%' THEN '86400'
                   WHEN published like '%hours%' THEN '3600'
                   WHEN published like '%minute%' THEN '60' END AS published_rate
                   ,CASE WHEN salary like '%year%' THEN 'Annual'
                   WHEN salary like '%week%' THEN 'Weekly'
                   WHEN salary like '%Month%' THEN 'Monthly'
                   WHEN salary like '%Day%' THEN 'Daily'
                   WHEN salary like '%fortnight%' THEN 'Fortnightly'
                   WHEN salary like '%hour%' THEN 'Hourly' END AS pay_rate
                   ,CASE WHEN salary like '%year%' THEN 1
                   WHEN salary like '%week%' THEN 52
                   WHEN salary like '%Month%' THEN 12
                   WHEN salary like '%Day%' THEN 244
                   WHEN salary like '%fortnight%' THEN 24
                   WHEN salary like '%hour%' THEN '1976' END AS pay_rate_modify
                   from indeed_au;")


indeed_au$published_num <- substr(indeed_au$published, 1, 2)
indeed_au$published_num2 = as.numeric(indeed_au$published_num) * as.numeric(indeed_au$published_rate)


indeed_au$published_date <- date_extracted - as.numeric(indeed_au$published_num2)


### Salary Fix
indeed_au$salary1 <- gsub(' an hour','',indeed_au$salary)
indeed_au$salary2 <- gsub(' a week','',indeed_au$salary1)
indeed_au$salary3 <- gsub(' a year','',indeed_au$salary2)
indeed_au$salary4 <- gsub(' a day','',indeed_au$salary3)
indeed_au$salary5 <- gsub(' a month','',indeed_au$salary4)
indeed_au$salary6 <- gsub('$','',indeed_au$salary5, fixed = TRUE)
indeed_au$salary7 <- gsub(',','',indeed_au$salary6)
indeed_au$salary8 <- gsub(' ','',indeed_au$salary7)
indeed_au$salary9 <- gsub('Â','',indeed_au$salary8, fixed = TRUE)
indeed_au <- cSplit(indeed_au, "salary9", sep="-")
indeed_au$salary9_1 <- as.numeric(indeed_au$salary9_1)
indeed_au$salary9_2 <- as.numeric(indeed_au$salary9_2)
indeed_au$min_salary <- indeed_au$salary9_1 * as.numeric(indeed_au$pay_rate_modify)
indeed_au$max_salary <- indeed_au$salary9_2 * as.numeric(indeed_au$pay_rate_modify)
indeed_au$average_salary <-ifelse(is.na(indeed_au$max_salary), indeed_au$min_salary, (indeed_au$min_salary + indeed_au$max_salary) / 2 )

indeed_au$source <- "au"
indeed_au$salary <- NULL
indeed_au$pay_rate <- NULL
indeed_au$pay_rate_modify <- NULL
indeed_au$salary1 <- NULL
indeed_au$salary2 <- NULL
indeed_au$salary3 <- NULL
indeed_au$salary4 <- NULL
indeed_au$salary5 <- NULL
indeed_au$salary6 <- NULL
indeed_au$salary7 <- NULL
indeed_au$salary8 <- NULL
indeed_au$salary9_1 <- NULL
indeed_au$salary9_2 <- NULL
indeed_au$min_salary <- NULL
indeed_au$max_salary <- NULL
indeed_au$published <- NULL
indeed_au$published_rate <- NULL
indeed_au$published_num <- NULL
indeed_au$published_num2 <- NULL


indeed_au <- indeed_au %>% distinct(url, job_title, job_description)

indeed_au$avg_sal_aud <- indeed_au$average_salary




## Indeed Data Cleaning UK

indeed_uk <- read.csv('H:/Desktop/project_workspace/indeed/uk.csv')
indeed_uk$date_extracted <- date_extracted
indeed_uk <- indeed_uk[names(indeed_uk) %in% c("date_extracted", "job_description","job_title_with_link", "job_title_with_link._text", "published", "company", "location", "salary")]
names(indeed_uk) <- c("url", "job_title", "published", "salary", "job_description", "company", "location", "date_extracted")
#names(indeed_uk) <- c("url", "job_description", "company", "location", "job_title", "published", "salary", "date_extracted")





indeed_uk <- sqldf("select *
                   ,CASE WHEN published like '%day%' THEN '86400'
                   WHEN published like '%hours%' THEN '3600'
                   WHEN published like '%minute%' THEN '60' END AS published_rate
                   ,CASE WHEN salary like '%year%' THEN 'Annual'
                   WHEN salary like '%week%' THEN 'Weekly'
                   WHEN salary like '%Month%' THEN 'Monthly'
                   WHEN salary like '%Day%' THEN 'Daily'
                   WHEN salary like '%fortnight%' THEN 'Fortnightly'
                   WHEN salary like '%hour%' THEN 'Hourly' END AS pay_rate
                   ,CASE WHEN salary like '%year%' THEN 1
                   WHEN salary like '%week%' THEN 52
                   WHEN salary like '%Month%' THEN 12
                   WHEN salary like '%Day%' THEN 244
                   WHEN salary like '%fortnight%' THEN 24
                   WHEN salary like '%hour%' THEN '1976' END AS pay_rate_modify
                   from indeed_uk;")


indeed_uk$published_num <- substr(indeed_uk$published, 1, 2)
indeed_uk$published_num2 = as.numeric(indeed_uk$published_num) * as.numeric(indeed_uk$published_rate)
indeed_uk$published_date <- date_extracted - as.numeric(indeed_uk$published_num2)


### Salary Fix
indeed_uk$salary1 <- gsub(' an hour','',indeed_uk$salary)
indeed_uk$salary2 <- gsub(' a week','',indeed_uk$salary1)
indeed_uk$salary3 <- gsub(' a year','',indeed_uk$salary2)
indeed_uk$salary4 <- gsub(' a day','',indeed_uk$salary3)
indeed_uk$salary5 <- gsub(' a month','',indeed_uk$salary4)
indeed_uk$salary6 <- gsub('£','',indeed_uk$salary5, fixed = TRUE)
indeed_uk$salary7 <- gsub(',','',indeed_uk$salary6)
indeed_uk$salary8 <- gsub(' ','',indeed_uk$salary7)
indeed_uk$salary9 <- gsub('Â','',indeed_uk$salary8, fixed = TRUE)
indeed_uk <- cSplit(indeed_uk, "salary9", sep="-")
indeed_uk$salary9_1 <- as.numeric(indeed_uk$salary9_1)
indeed_uk$salary9_2 <- as.numeric(indeed_uk$salary9_2)
indeed_uk$min_salary <- indeed_uk$salary9_1 * as.numeric(indeed_uk$pay_rate_modify)
indeed_uk$max_salary <- indeed_uk$salary9_2 * as.numeric(indeed_uk$pay_rate_modify)
indeed_uk$average_salary <-ifelse(is.na(indeed_uk$max_salary), indeed_uk$min_salary, (indeed_uk$min_salary + indeed_uk$max_salary) / 2 )

indeed_uk$source <- "uk"
indeed_uk$salary <- NULL
indeed_uk$pay_rate <- NULL
indeed_uk$pay_rate_modify <- NULL
indeed_uk$salary1 <- NULL
indeed_uk$salary2 <- NULL
indeed_uk$salary3 <- NULL
indeed_uk$salary4 <- NULL
indeed_uk$salary5 <- NULL
indeed_uk$salary6 <- NULL
indeed_uk$salary7 <- NULL
indeed_uk$salary8 <- NULL
indeed_uk$salary9_1 <- NULL
indeed_uk$salary9_2 <- NULL
indeed_uk$min_salary <- NULL
indeed_uk$max_salary <- NULL
indeed_uk$published <- NULL
indeed_uk$published_rate <- NULL
indeed_uk$published_num <- NULL
indeed_uk$published_num2 <- NULL


indeed_uk <- indeed_uk %>% distinct(url, job_title, job_description)

indeed_uk$avg_sal_aud <- indeed_uk$average_salary / .54




### Indeed Data Cleaning us

indeed_us <- read.csv('H:/Desktop/project_workspace/indeed/us.csv')
indeed_us$date_extracted <- date_extracted

indeed_us <- indeed_us[names(indeed_us) %in% c("job_title_with_link", "job_title_with_link._title", "job_description", "company", "location", "published", "salary", "date_extracted")]
names(indeed_us) <- c("url", "job_title", "job_description", "company", "location", "published", "salary", "date_extracted")

                                               



indeed_us <- sqldf("select *
                   ,CASE WHEN published like '%day%' THEN '86400'
                   WHEN published like '%hours%' THEN '3600'
                   WHEN published like '%minute%' THEN '60' END AS published_rate
                   ,CASE WHEN salary like '%year%' THEN 'Annual'
                   WHEN salary like '%week%' THEN 'Weekly'
                   WHEN salary like '%Month%' THEN 'Monthly'
                   WHEN salary like '%Day%' THEN 'Daily'
                   WHEN salary like '%fortnight%' THEN 'Fortnightly'
                   WHEN salary like '%hour%' THEN 'Hourly' END AS pay_rate
                   ,CASE WHEN salary like '%year%' THEN 1
                   WHEN salary like '%week%' THEN 52
                   WHEN salary like '%Month%' THEN 12
                   WHEN salary like '%Day%' THEN 244
                   WHEN salary like '%fortnight%' THEN 24
                   WHEN salary like '%hour%' THEN '1976' END AS pay_rate_modify
                   from indeed_us;")


indeed_us$published_num <- substr(indeed_us$published, 1, 2)
indeed_us$published_num2 = as.numeric(indeed_us$published_num) * as.numeric(indeed_us$published_rate)
indeed_us$published_date <- date_extracted - as.numeric(indeed_us$published_num2)



### Salary Fix
indeed_us$salary1 <- gsub(' an hour','',indeed_us$salary)
indeed_us$salary2 <- gsub(' a week','',indeed_us$salary1)
indeed_us$salary3 <- gsub(' a year','',indeed_us$salary2)
indeed_us$salary4 <- gsub(' a day','',indeed_us$salary3)
indeed_us$salary5 <- gsub(' a month','',indeed_us$salary4)
indeed_us$salary6 <- gsub('$','',indeed_us$salary5, fixed = TRUE)
indeed_us$salary7 <- gsub(',','',indeed_us$salary6)
indeed_us$salary8 <- gsub(' ','',indeed_us$salary7)
indeed_us$salary9 <- gsub('Â','',indeed_us$salary8, fixed = TRUE)
indeed_us <- cSplit(indeed_us, "salary9", sep="-")
indeed_us$salary9_1 <- as.numeric(indeed_us$salary9_1)
indeed_us$salary9_2 <- as.numeric(indeed_us$salary9_2)
indeed_us$min_salary <- indeed_us$salary9_1 * as.numeric(indeed_us$pay_rate_modify)
indeed_us$max_salary <- indeed_us$salary9_2 * as.numeric(indeed_us$pay_rate_modify)
indeed_us$average_salary <-ifelse(is.na(indeed_us$max_salary), indeed_us$min_salary, (indeed_us$min_salary + indeed_us$max_salary) / 2 )



indeed_us$source <- "us"
indeed_us$salary <- NULL
indeed_us$pay_rate <- NULL
indeed_us$pay_rate_modify <- NULL
indeed_us$salary1 <- NULL
indeed_us$salary2 <- NULL
indeed_us$salary3 <- NULL
indeed_us$salary4 <- NULL
indeed_us$salary5 <- NULL
indeed_us$salary6 <- NULL
indeed_us$salary7 <- NULL
indeed_us$salary8 <- NULL
indeed_us$salary9_1 <- NULL
indeed_us$salary9_2 <- NULL
indeed_us$min_salary <- NULL
indeed_us$max_salary <- NULL
indeed_us$published <- NULL
indeed_us$published_rate <- NULL
indeed_us$published_num <- NULL
indeed_us$published_num2 <- NULL


indeed_us <- indeed_us %>% distinct(url, job_title, job_description)

indeed_us$avg_sal_aud <- indeed_us$average_salary / .77





# Merging all the results


indeed <- rbind(indeed_au, indeed_uk, indeed_us)
indeed$job_description <- str_replace_all(indeed$job_description, "[[:punct:]]", " ")
indeed$job_title <- str_replace_all(indeed$job_title, "[[:punct:]]", " ")
setwd("H:/Desktop/project_workspace/indeed/")
#save(indeed, file = "indeed_201604014.RData")


### Merge master and cleaned


load("indeed_201604013.RData")
load("indeed_master.RData")
indeed_final <- rbind(indeed, indeed_old)
indeed_final <- indeed_final %>% distinct(url, job_title, job_description)
#save(indeed_final, file = "indeed_master.RData")


write.csv(indeed_final, "indeed_final.csv", sep=",",row.names=F)



### Custom Markup for AU File

indeed_final2 <- sqldf("select *
                   ,CASE WHEN location like '%NSW%' and source = 'au' THEN 'NSW'
                   WHEN location like '%VIC%' and source = 'au' THEN 'VIC'
                   WHEN location like '%QLD%' and source = 'au' THEN 'QLD'
                   WHEN location like '%TAS%' and source = 'au' THEN 'TAS'
                   WHEN location like '%WA%' and source = 'au' THEN 'WA'
                   WHEN location like '%ACT%' and source = 'au' THEN 'ACT'
                   WHEN location like '%SA%' and source = 'au' THEN 'SA'
                   WHEN location like '%NT%' and source = 'au' THEN 'NT'
                   ELSE 'AUS' END AS state
                   ,CASE WHEN avg_sal_aud < 50000 THEN '1'
                   WHEN avg_sal_aud >= 50000 and avg_sal_aud < 100000 THEN '2'
                   WHEN avg_sal_aud >= 50000 and avg_sal_aud < 150000 THEN '3'
                   WHEN avg_sal_aud >= 50000 and avg_sal_aud < 200000 THEN '4'
                   WHEN avg_sal_aud >= 200000 and avg_sal_aud < 250000 THEN '5'
                   WHEN avg_sal_aud >= 250000 THEN '6' 
                   ELSE 'NA' END AS pay_rank
                   ,CASE WHEN job_title like '%Business Development Manager%' THEN 'Business Development Manager'
                   WHEN job_title like '%Client Relationship Manager%' THEN 'Client Relationship Manager'
                   WHEN job_title like '%Advanced Financial Planner%' THEN 'Advanced Financial Planner'
                   WHEN job_title like '%Senior Financial Planner%' THEN 'Senior Financial Planner'
                   WHEN job_title like '%Customer Service Manager%' THEN 'Customer Service Manager'
                   WHEN job_title like '%Customer Service Officer%' THEN 'Customer Service Officer'
                   WHEN job_title like '%Client Service Manager%' THEN 'Client Service Manager'
                   WHEN job_title like '%Customer Relationship%' THEN 'Customer Relationship'
                   WHEN job_title like '%Home Finance Manager%' THEN 'Home Finance Manager'
                   WHEN job_title like '%Home Lending Manager%' THEN 'Home Lending Manager'
                   WHEN job_title like '%Relationship Manager%' THEN 'Relationship Manager'
                   WHEN job_title like '%Financial Consultant%' THEN 'Financial Consultant'
                   WHEN job_title like '%Executive Assistant%' THEN 'Executive Assistant'
                   WHEN job_title like '%Customer Relations%' THEN 'Customer Relations'
                   WHEN job_title like '%Personal Assistant%' THEN 'Personal Assistant'
                   WHEN job_title like '%Financial Planner%' THEN 'Financial Planner'
                   WHEN job_title like '%Account Executive%' THEN 'Account Executive'
                   WHEN job_title like '%Executive Manager%' THEN 'Executive Manager'
                   WHEN job_title like '%Solution Designer%' THEN 'Solution Designer'
                   WHEN job_title like '%Business Analyst%' THEN 'Business Analyst'
                   WHEN job_title like '%Business Manager%' THEN 'Business Manager'
                   WHEN job_title like '%Practice Manager%' THEN 'Practice Manager'
                   WHEN job_title like '%Regional Manager%' THEN 'Regional Manager'
                   WHEN job_title like '%Finance Manager%' THEN 'Finance Manager'
                   WHEN job_title like '%Account Manager%' THEN 'Account Manager'
                   WHEN job_title like '%Account Officer%' THEN 'Account Officer'
                   WHEN job_title like '%Product Manager%' THEN 'Product Manager'
                   WHEN job_title like '%General Manager%' THEN 'General Manager'
                   WHEN job_title like '%Data Scientist%' THEN 'Data Scientist'
                   WHEN job_title like '%Change Manager%' THEN 'Change Manager'
                   WHEN job_title like '%Private Banker%' THEN 'Private Banker'
                   WHEN job_title like '%Branch Manager%' THEN 'Branch Manager'
                   WHEN job_title like '%Senior Manager%' THEN 'Senior Manager'
                   WHEN job_title like '%Data Insights%' THEN 'Data Insights'
                   WHEN job_title like '%State Manager%' THEN 'State Manager'
                   WHEN job_title like '%Bank Manager%' THEN 'Bank Manager'
                   WHEN job_title like '%Paraplanner%' THEN 'Paraplanner'
                   WHEN job_title like '%Sales Coach%' THEN 'Sales Coach'
                   WHEN job_title like '%Team Leader%' THEN 'Team Leader'
                   WHEN job_title like '%Programmer%' THEN 'Programmer'
                   WHEN job_title like '%HR Manager%' THEN 'HR Manager'
                   WHEN job_title like '%Paralegal%' THEN 'Paralegal'
                   WHEN job_title like '%Strategy%' THEN 'Strategy'
                   WHEN job_title like '%Analyst%' THEN 'Analyst'
                   WHEN job_title like '%Manager%' THEN 'Manager'
                   WHEN job_title like '%Head of%' THEN 'Head of'
                   WHEN job_title like '%Counsel%' THEN 'Counsel'
                   WHEN job_title like '%Auditor%' THEN 'Auditor'
                   WHEN job_title like '%Banker%' THEN 'Banker'
                   WHEN job_title like '%Lawyer%' THEN 'Lawyer'
                   ELSE 'Other' END AS job_role
                   from indeed_final;")


# Density and Skewness.

plot(density(indeed_final2$avg_sal_aud, na.rm=T))

library(e1071) 

skewness(indeed_final2$avg_sal_aud, na.rm=T)



### Exploration step1
library(ggraptR)
ggraptR()
write.table(indeed_final2, 'explore.csv', sep=",", row.names=F)



# Market and Economic Data - Future Project Material

## ABS-RBA-YAHOO DATA FIX

### Daily
asx_market <- read.csv('http://real-chart.finance.yahoo.com/table.csv?s=%5EAXJO&d=3&e=10&f=2016&g=d&a=10&b=23&c=1992&ignore=.csv')

### Monthly
lf_all <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202001.xls&6202.0&Time%20Series%20Spreadsheet&36C607D77EAB8B5DCA257F7800134E50&0&Feb%202016&17.03.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
lf_youth <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&6202013.xls&6202.0&Time%20Series%20Spreadsheet&F458213E7A60E111CA257F7800135E08&0&Feb%202016&17.03.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
currency_prior2010 <- read.xls('http://www.rba.gov.au/statistics/tables/xls-hist/f11hist-1969-2009.xls', sheet=1, verbose=FALSE, pattern='Series ID')
currency_post2010 <- read.xls('http://www.rba.gov.au/statistics/tables/xls-hist/f11hist.xls', sheet=1, verbose=FALSE, pattern='Series ID')
interest_rate <- read.xls('http://www.rba.gov.au/statistics/tables/xls/f01hist.xls', sheet=1, verbose=FALSE, pattern='Series ID')
trade_balance <- read.xls('http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&536801.xls&5368.0&Time%20Series%20Spreadsheet&945D4F8166156AB7CA257F8B001467A8&0&Feb%202016&05.04.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')



### Quarterly
cpi_all <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&640101.xls&6401.0&Time%20Series%20Spreadsheet&E1274D6D73D7DF1CCA257F45000D8123&0&Dec%202015&27.01.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
gdp_aggregate <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206001_key_aggregates.xls&5206.0&Time%20Series%20Spreadsheet&DF7CCAB68D1F6CC2CA257F6900110BD9&0&Dec%202015&02.03.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
gdp_industry <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&5206006_industry_gva.xls&5206.0&Time%20Series%20Spreadsheet&F911992FCF3DC0A5CA257F6900110FC0&0&Dec%202015&02.03.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
job_vacancy_all <- read.xls('http://www.abs.gov.au/ausstats/meisubs.nsf/log?openagent&6354001.xls&6354.0&Time%20Series%20Spreadsheet&78C32079A7EFA9B6CA257F860016AE20&0&Feb%202016&31.03.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
wage_price_index_all <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&634501.xls&6345.0&Time%20Series%20Spreadsheet&FD3E68284BBBF059CA257F620013B4E5&0&Dec%202015&24.02.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')
hourly_rate_all <- read.xls('http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&634501.xls&6345.0&Time%20Series%20Spreadsheet&FD3E68284BBBF059CA257F620013B4E5&0&Dec%202015&24.02.2016&Latest', sheet=2, verbose=FALSE, pattern='Series ID')

### Get google trend data - keywords = vacancy, job, unemployment, mycareer, seek, data scientist
### python version - https://github.com/GeneralMills/pytrends
setwd("C:/Users/sean/Desktop/project_workspace/trend_data/")
library(gtrendsR)

gconnect('user', 'pw')
words <-c('Vacancy','Unemployment', 'MyCareer', 'Seek', 'Jobs','Data Science','Future', 'Customer Relations', 'Financial Planner', 'Advanced Financial Planner', 'Senior Financial Planner', 'Business Analyst', 'Analyst', 'Banker', 'Paraplanner', 'Data Scientist', 'Programmer', 'Change Manager', 'Private Banker', 'HR Manager', 'Finance Manager', 'Business Manager', 'Practice Manager', 'Client Relationship Manager', 'Client Service Manager', 'Customer Service Manager', 'Customer Relationship', 'Data Insights', 'Strategy') 


words_2 <- words[notin 8]
word <- words[22]
trends<- gtrends(word, geo="AU")
write.csv(trends$trend, paste(word, ".csv"),row.names=F)

exclude <- c(8,10,11,19,20)


### Converting Date Ranges

asx_market$Date <- as.Date(asx_market$Date)
cpi_all$Series.ID <- as.Date(paste("01",cpi_all$Series.ID,sep="-"),"%d-%b-%Y")
currency_prior2010$Series.ID <- as.Date(currency_prior2010$Series.ID)
currency_post2010$Series.ID <- as.Date(currency_post2010$Series.ID)
gdp_aggregate$Series.ID <- as.Date(paste("01",gdp_aggregate$Series.ID,sep="-"),"%d-%b-%Y")
gdp_industry$Series.ID <- as.Date(paste("01",gdp_industry$Series.ID,sep="-"),"%d-%b-%Y")
hourly_rate_all$Series.ID <- as.Date(paste("01",hourly_rate_all$Series.ID,sep="-"),"%d-%b-%Y")
interest_rate$Series.ID <- as.Date(paste("01",interest_rate$Series.ID,sep="-"),"%d-%b-%Y")
job_vacancy_all$Series.ID <- as.Date(paste("01",job_vacancy_all$Series.ID,sep="-"),"%d-%b-%Y")
lf_all$Series.ID <- as.Date(paste("01",lf_all$Series.ID,sep="-"),"%d-%b-%Y")
lf_youth$Series.ID <- as.Date(paste("01",lf_youth$Series.ID,sep="-"),"%d-%b-%Y")
trade_balance$Series.ID <- as.Date(paste("01",trade_balance$Series.ID,sep="-"),"%d-%b-%Y")
wage_price_index_all$Series.ID <- as.Date(paste("01",wage_price_index_all$Series.ID,sep="-"),"%d-%b-%Y")



### 22 trend, 13 abs, 1 indeed

### Min/MaxDate Ranges

summary(asx_market$Date)
summary(cpi_all$Series.ID)
summary(currency_prior2010$Series.ID)
summary(currency_post2010$Series.ID)
summary(gdp_aggregate$Series.ID)
summary(gdp_industry$Series.ID)
summary(hourly_rate_all$Series.ID)
summary(interest_rate$Series.ID)
summary(job_vacancy_all$Series.ID)
summary(lf_all$Series.ID)
summary(lf_youth$Series.ID)
summary(trade_balance$Series.ID)
summary(wage_price_index_all$Series.ID)


### Subsetting Data
asx_market <- asx_market[names(asx_market) %in% c("Date", "Adj.Close")]
names(asx_market) <- c("date", "asx_value")

lf_all <- lf_all[names(lf_all) %in% c("Series.ID", "A84423043C", "A84423041X", "A84423042A", "A84423046K", "A84423050A", "A84423051C")]
names(lf_all) <- c("date", "employed_persons", "employed_ft_persons", "employed_pt_persons", "unemployment_persons", "unemployment_rate", "lf_participation_rate")

cpi_all <- cpi_all[names(cpi_all) %in% c("Series.ID", "A2325846C", "A2325847F")]
names(cpi_all) <- c("date", "cpi", "inflation_rate")

currency_post2010 <- currency_post2010[names(currency_post2010) %in% c("Series.ID", "FXRUSD")]
currency_prior2010 <- currency_prior2010[names(currency_prior2010) %in% c("Series.ID", "FXRUSD")]
currency <- rbind(currency_post2010, currency_prior2010)
names(currency) <- c("date", "currency_usd")
rm(currency_post2010, currency_prior2010)

gdp_aggregate <- gdp_aggregate[names(gdp_aggregate) %in% c("Series.ID", "A2304322X", "A2304402X", "A2304412C")]
names(gdp_aggregate) <- c("date", "gdp", "gdp_constant_price", "annual_gross_nat_income")

gdp_industry <- gdp_industry[names(gdp_industry) %in% c("Series.ID", "A2716160J", "A2716163R", "A2716166W", "A2716175X", "A2716179J", "A2716183X", "A2716188K", "A2716189L", "A2716192A")]
names(gdp_industry) <- c("date", "gdp_from_agriculture", "gdp_from_mining", "gdp_from_manufacturing", "gdp_from_utilities", "gdp_from_construction", "gdp_from_transport", "gdp_from_it", "gdp_from_finance", "gdp_from_publicadmin")

hourly_rate_all <- hourly_rate_all[names(hourly_rate_all) %in% c("Series.ID", "A83895308K")]
names(hourly_rate_all) <- c("date", "hourly_rate_rate")

interest_rate <- interest_rate[names(interest_rate) %in% c("Series.ID", "FIRMMCRT")]
names(interest_rate) <- c("date", "interest_rate")

job_vacancy_all <- job_vacancy_all[names(job_vacancy_all) %in% c("Series.ID", "A590698F")]
names(job_vacancy_all) <- c("date", "job_vacancies")

lf_youth <- lf_youth[names(lf_youth) %in% c("Series.ID", "A84424185C", "A84424188K")]
names(lf_youth) <- c("date", "lf_youth_unemployment_rate", "lf_employment_topopulation_ratio")

trade_balance <- trade_balance[names(trade_balance) %in% c("Series.ID", "A2718585A")]
names(trade_balance) <- c("date", "trade_balance")

wage_price_index_all <- wage_price_index_all[names(wage_price_index_all) %in% c("Series.ID", "A2603039T")]
names(wage_price_index_all) <- c("date", "wage_price_index")





