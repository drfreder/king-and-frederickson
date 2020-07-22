The Pandemic Penalty: COVID-19’s Gendered Impact on Academic
Productivity
================
Megan Frederickson and Molly King
July 17, 2020

We quantified how the COVID-19 pandemic is affecting the gender
breakdown of preprint submissions to [arXiv](https://arxiv.org/) and
[bioRxiv](https://www.biorxiv.org/), two preprint servers that together
cover many STEM fields.

We used these packages:

``` r
#Load packages
library(tidyverse) #includes ggplot2, dplyr, readr, stringr
library(knitr)
library(cowplot)
library(gender)
library(aRxiv)
#install.packages("devtools") 
#devtools::install_github("nicholasmfraser/rbiorxiv") 
library(rbiorxiv) 
library(lubridate)
library(anytime)
library(car)
library(rcrossref)
library(lme4)
```

## arXiv submissions

We scraped submission data from arXiv, a preprint server for physics,
math, computer science, statistics, and other quantitative disciplines.
We used the aRxiv package to scrape the data, see:

Karthik, R. and K. Broman (2019). aRxiv: Interface to the arXiv API. R
package version 0.5.19. <https://CRAN.R-project.org/package=aRxiv>

We began by scraping all records for March 15-April 15, 2020, during the
COVID-19 pandemic, and for the same date range in 2019. Then, we
expanded to scrape all the data for Jan 1, 2020 to June 30, 2020,
inclusive. We scraped the data in batches, as recommended in the aRxiv
package tutorial. For brevity, we are not reproducing the code here, but
it is available in the [R markdown
file](https://github.com/drfreder/king-and-frederickson/master/README.Rmd)
included in this repo.

Next, we assigned gender to author names using the gender package, see:

Mullen, L. (2019). gender: Predict Gender from Names Using Historical
Data. R package version 0.5.3, <https://github.com/ropensci/gender>.

This package returns the probability that a first name belongs to a
woman or a man by comparing the name to names in a database; we used the
U.S. Social Security Administration baby names database.

Please note: this is a brute force method of predicting gender, and it
has many limitations, as discussed by the package authors on their
GitHub repo and included links. By using this method, we are not
assuming that individuals are correctly gendered in the resulting
dataset, but merely that it provides insight into gender’s effects in
aggregate across the population of preprint authors.

This code takes a while to run, so it is not run when knitting this
markdown document.

``` r
#Not run
#First combine data for year-by-year comparison
df.2020 <- read.csv("Data/arxiv_2020_data.csv") #Read in data
df.2019 <- read.csv("Data/arxiv_2019_data.csv")
df.full <- rbind(df.2019, df.2020) #Combine in one dataframe

#Next combine data for early 2020 comparison
df.early2020 <- read.csv("Data/arxiv_early2020_data.csv")
df.update <- read.csv("Data/arxiv_update2020_data.csv")
df.update.2 <- read.csv("Data/arxiv_update2020_2_data.csv")
df.mayjune2020 <- read.csv("Data/arxiv_updateMayJune2020_data.csv")
df.all2020 <- rbind(df.2020, df.early2020, df.update, df.update.2, df.mayjune2020) #Full 2020 data

split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)} #Function to split strings of author names
last.author <- function(x){gsub(".*\\|", "", as.character(x))} #Function to extract last author
first.author <- function(x){gsub("\\|.*", "", as.character(x))} #Function to extract first author

#For the year-over-year dataset
df.full$split.names <- lapply(df.full$authors, split.names) #Apply functions
df.full$first.author <- lapply(df.full$authors, first.author)
df.full$last.author <- lapply(df.full$authors, last.author)

all_first_names <- word(unlist(df.full$split.names),1) #Make a list of all author first names
#install.packages("genderdata", repos = "http://packages.ropensci.org") #In case you need the gender data package
gender <- gender(all_first_names, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)]) #Keep only unique names

#This loop is an inelegant way of counting the number of men and women authors for each paper
tmp <- NULL
for(i in 1:length(df.full$authors)){
  tmp <- as.data.frame(word(unlist(df.full$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.full$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.full$female.n[i] <-  sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

#Predict first author gender (includes sole authors)
df.full$first.author.first.name <- word(df.full$first.author, 1)
gender <- gender(df.full$first.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.full$first.author.gender <- getgender[df.full$first.author.first.name]

#Predict last author gender (omits sole authors)
df.full$author.n <- str_count(df.full$authors, pattern = "\\|")+1 #Count author number
df.full$last.author.first.name <- ifelse(df.full$author.n > 1, word(df.full$last.author, 1), NA)
gender <- gender(df.full$last.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.full$last.author.gender <- getgender[df.full$last.author.first.name]

#Count middle authors for each gender
df.full$female.mid.authors.n <- df.full$female.n - ifelse(df.full$first.author.gender == "female", 1, 0) - ifelse(df.full$last.author.gender == "female", 1, 0)
df.full$male.mid.authors.n <- df.full$male.n - ifelse(df.full$first.author.gender == "male", 1, 0) - ifelse(df.full$last.author.gender == "male", 1, 0)

df.full.output <- as.data.frame(apply(df.full,2,as.character)) 
write.csv(df.full.output, "Data/arxiv_full_gender.csv") #Save data

#Same for the early 2020 dataset
df.all2020$split.names <- lapply(df.all2020$authors, split.names)
df.all2020$first.author <- lapply(df.all2020$authors, first.author)
df.all2020$last.author <- lapply(df.all2020$authors, last.author)

tmp <- NULL
all_first_names <- word(unlist(df.all2020$split.names),1)
gender <- gender(all_first_names, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])

for(i in 1:length(df.all2020$authors)){
  tmp <- as.data.frame(word(unlist(df.all2020$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.all2020$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.all2020$female.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

df.all2020$first.author.first.name <- word(df.all2020$first.author, 1)
gender <- gender(df.all2020$first.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.all2020$first.author.gender <- getgender[df.all2020$first.author.first.name]

df.all2020$author.n <- str_count(df.all2020$authors, pattern = "\\|")+1 #Count author number
df.all2020$last.author.first.name <- ifelse(df.all2020$author.n > 1, word(df.all2020$last.author, 1), NA)
gender <- gender(df.all2020$last.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.all2020$last.author.gender <- getgender[df.all2020$last.author.first.name]

df.all2020$female.mid.authors.n <- df.all2020$female.n - ifelse(df.all2020$first.author.gender == "female", 1, 0) - ifelse(df.all2020$last.author.gender == "female", 1, 0)
df.all2020$male.mid.authors.n <- df.all2020$male.n - ifelse(df.all2020$first.author.gender == "male", 1, 0) - ifelse(df.all2020$last.author.gender == "male", 1, 0)

df.all2020.output <- as.data.frame(apply(df.all2020,2,as.character))
write.csv(df.all2020.output, "Data/arxiv_all2020_gender.csv") #Save data
```

Next, we calculated some summary statistics for the arXiv dataset we
assembled.

``` r
df.full <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/arxiv_full_gender.csv") #Read in data
df.full <- df.full[!duplicated(df.full), ] #Remove duplicates, if any
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year

df.all2020 <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/arxiv_all2020_gender.csv") #Read in data
df.all2020 <- df.all2020[!duplicated(df.all2020), ] #Remove duplicated rows, if any
df.all2020$year <- as.factor(year(as.Date(df.all2020$submitted))) #Extract year

all.arxiv <- rbind(df.full, df.all2020) #Combine datasets
all.arxiv <- all.arxiv[!duplicated(all.arxiv), ] #Remove duplicates
total.preprints <- length(all.arxiv$id) #Total number of preprints
total.authors <- sum(all.arxiv$author.n) #Total number of authors
total.authors.with.gender <- sum(all.arxiv$male.n+all.arxiv$female.n) #Total number of authors with gender inferred
per.gender <- round((total.authors.with.gender/total.authors)*100, 1) #Percent of authors with gender

year.arxiv.preprints <- length(df.full$"id") #Total number of preprints for year-over-year comparison
year.arxiv.authors <- sum(df.full[, "male.n"]+df.full[, "female.n"]) #Authors with gender inferred for year-over-year comparison
```

There are 114632 preprints in the full arXiv dataset, with a total of
549512 non-unique authors. We inferred the gender of 266133 authors, or
48.4%, with the rest omitted from subsequent analyses. This a lower
success rate for predicting author gender for the arXiv dataset than for
the bioRxiv dataset (see below), reflecting the fact that arXiv
preprints are more likely to list large consortia as authors (e.g., CMS
Collaboration), have authors who provide only first initials, or have
authors who have names not in the U.S. Social Security names database.

For just the comparison of March 15-April 15, 2020 with the same dates
in 2019, there are 28711 arXiv preprints with 67309 authors for whom we
inferred
gender.

### Comparing arXiv preprint authors between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many men versus women authors of preprints were there in Mar/Apr
2020, compared to the same dates last year? Note: this is not the number
of unique authors; it includes authors who submitted multiple
preprints.

``` r
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year
all <- as.data.frame(ungroup(df.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all.long <- gather(all, Gender, number, Female:Male) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
fontsize = 10

#Make figure comparing 2020 to 2019
p1 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(9000,29000), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="arXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p1 
```

![](README_files/figure-gfm/Visualize%20arXiv%20year-over-year%20data-1.png)<!-- -->

arXiv preprint submissions are up overall, but the number of men authors
is currently growing faster than the number of women authors. Comparing
preprint submissions in late March and early April 2020 to the same
dates in 2019, the number of men authors has grown more than the number
of women authors, both as a percent change and in absolute
terms.

### Comparing single-authored arXiv preprints between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many arXiv preprints were authored by a single woman versus a single
man in Mar/Apr, 2020, compared to the same dates last
year?

``` r
sole.authors <- as.data.frame(ungroup(subset(df.full, author.n == 1) %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
sole.long <- gather(sole.authors, Gender, number, Male:Female) #Make wide data long
sole.authors.t <- as.data.frame(t(sole.authors[,-1])) #Transpose
colnames(sole.authors.t) <- c("2019", "2020") #Fix column names
sole.authors.t$per.dif.1920 <- ((sole.authors.t$`2020`-sole.authors.t$`2019`)/(sole.authors.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure for single-authored preprints
p2 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(270,1350), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "sole authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p2
```

![](README_files/figure-gfm/Sole%20authors-1.png)<!-- -->

Single-authored arXiv submissions are also up overall, but again the
number of men authors is currently growing faster than the number of
women authors, both as a percent change and in absolute terms.

### Comparing arXiv preprint submissions by author position

What if we break it down further by author position, so first, middle,
or
last?

#### First authors

``` r
first.authors <- subset(df.full, !is.na(first.author.gender)) %>% group_by(year,first.author.gender) %>% summarize(n=n()) #Summarize by year
first.authors$per.dif.1920 <- c(first.authors[3,3]/first.authors[1,3],first.authors[4,3]/first.authors[2,3] ,first.authors[3,3]/first.authors[1,3], first.authors[4,3]/first.authors[2,3])
first.authors$per.dif.1920 <- (as.numeric(first.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p3 <- ggplot(data=first.authors, aes(fill=as.factor(year), y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(2300,7800), label = paste0("+", round(first.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "first authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p3
```

![](README_files/figure-gfm/arXiv%20year-over-year%20first%20authors-1.png)<!-- -->

The number of men first authors has grown only very slightly faster than
the number of women first
authors.

#### Last authors

``` r
last.authors <- subset(df.full, !is.na(last.author.gender)) %>% group_by(year,last.author.gender) %>% summarize(n=n()) #Summarize by year
last.authors$per.dif.1920 <- c(last.authors[3,3]/last.authors[1,3],last.authors[4,3]/last.authors[2,3] ,last.authors[3,3]/last.authors[1,3], last.authors[4,3]/last.authors[2,3])
last.authors$per.dif.1920 <- (as.numeric(last.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p4 <- ggplot(data=last.authors, aes(fill=as.factor(year), y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(1900,7050), label = paste0("+", round(last.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "last authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p4
```

![](README_files/figure-gfm/arXiv%20last%20authors%20year-over-year-1.png)<!-- -->

The number of men last authors has grown substantially year-over-year,
but the number of women last authors is almost unchanged from
2019.

#### Middle authors

``` r
middle <- as.data.frame(ungroup(df.full %>% group_by(year) %>% summarize(Female = sum(female.mid.authors.n, na.rm=TRUE), Male = sum(male.mid.authors.n, na.rm=TRUE)))) #Summarize by year
middle.long <- gather(middle, Gender, number, Female:Male) #Make wide data long
middle.t <- as.data.frame(t(middle[,-1])) #Transpose
colnames(middle.t) <- c("2019", "2020") #Fix column names
middle.t$per.dif.1920 <- ((middle.t$`2020`-middle.t$`2019`)/(middle.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
fontsize = 10

#Make figure comparing 2020 to 2019
p5 <- ggplot(data=middle.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(2700,7900), label = c(paste0(round(middle.t$per.dif.1920[1], 1), "%"), paste0("+", round(middle.t$per.dif.1920[2], 1), "%")))+labs(title="arXiv", subtitle="middle authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p5
```

![](README_files/figure-gfm/arXiv%20middle%20authors%20year-over-year-1.png)<!-- -->

The number of women middle authors actually declined from Mar/Apr 2019
to Mar/Apr 2020, while the number of men middle authors rose
somewhat.

### Comparing arXiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Next, we looked back over the months leading up to widespread
stay-at-home orders and school and childcare closures that North
Americans experienced beginning in late March or early April, 2020.
These measures were implemented to different degrees and on different
dates in different parts of the world, but we assumed their effects
would be most pronounced (globally) starting in March, 2020 and
thereafter.

``` r
start.date <- as.Date("2020-03-01") #Month WHO declared COVID-19 a pandemic
df.all2020$COVID <- ifelse(as.Date(df.all2020$submitted) < start.date, "Jan. 1-Feb. 29, 2020", ifelse(as.Date(df.all2020$submitted) < "2020-05-01", "Mar. 1-Apr. 30, 2020", "May 1-Jun. 30, 2020")) #Classify dates as COVID or not
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(COVID) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.long <- gather(arxiv, gender, n, female.n:male.n) #Make wide data long
arxiv.long$gender <- as.factor(arxiv.long$gender) #Make sure gender is a factor
levels(arxiv.long$gender) <- c("Female", "Male") #Capitalize genders
arxiv.long$per <- NA
for (i in 1:length(arxiv.long$n)) {
  arxiv.long$per[i] <- ifelse(i != 1 & i != 4, (arxiv.long$n[i]/arxiv.long$n[i-1]-1)*100, NA)
}
bump = 3000 #Set for figure annotation
arxiv.long$y <- arxiv.long$n +bump
arxiv.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation
m.labels=c("Jan. 1 - Feb. 29, 2020", "Mar. 1 - Apr. 30, 2020", "May 1 - Jun. 30, 2020")
colours2 = c("#deebf7", "#9ecae1", "#3182bd") 

#Make figure
p6 <- ggplot(data=arxiv.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p6
```

![](README_files/figure-gfm/Early%202020%20arXiv%20all%20authors%20analysis-1.png)<!-- -->

``` r
#Model
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)
arxiv.long$day_of_week <- wday(arxiv.long$`as.Date(submitted)`, label=TRUE)
arxiv.long$day <- as.numeric(arxiv.long$`as.Date(submitted)`)-18261
arxiv.long$day_of_week <- factor(arxiv.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm1 <- lm(n~day*gender+day_of_week, data=arxiv.long)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * gender + day_of_week, data = arxiv.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -431.16  -87.70  -22.66  124.10  348.10 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -81.8769    26.5496  -3.084   0.0022 ** 
    ## day                0.4833     0.1901   2.542   0.0114 *  
    ## gendermale.n     504.4862    28.3612  17.788  < 2e-16 ***
    ## day_of_weekMon   418.5358    26.4206  15.841  < 2e-16 ***
    ## day_of_weekTue   418.3600    26.4216  15.834  < 2e-16 ***
    ## day_of_weekWed   360.5685    26.4257  13.645  < 2e-16 ***
    ## day_of_weekThu   373.9696    26.4233  14.153  < 2e-16 ***
    ## day_of_weekFri   295.4862    26.4216  11.184  < 2e-16 ***
    ## day_of_weekSat   -25.9781    26.4206  -0.983   0.3262    
    ## day:gendermale.n   1.5002     0.2688   5.581 4.76e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 134.7 on 354 degrees of freedom
    ## Multiple R-squared:  0.8885, Adjusted R-squared:  0.8857 
    ## F-statistic: 313.5 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm1, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##               Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)   172605   1   9.5106  0.002203 ** 
    ## day           117277   1   6.4620  0.011446 *  
    ## gender       5742431   1 316.4094 < 2.2e-16 ***
    ## day_of_week 11642519   6 106.9176 < 2.2e-16 ***
    ## day:gender    565342   1  31.1505 4.757e-08 ***
    ## Residuals    6424652 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)

#Alternate figure format
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)
colnames(arxiv.long) <- c("week", "gender", "n")
who <- "2020-03-11"
arxiv.long$COVID <- ifelse(as.Date(arxiv.long$week) < who, "no", "yes")

p7 <- ggplot(data=subset(arxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+labs(title="arXiv", subtitle="all authors")+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")
p7
```

![](README_files/figure-gfm/Early%202020%20arXiv%20all%20authors%20analysis-2.png)<!-- -->

Again, during the pandemic, the number of male authors has grown faster
than the number of female authors, both in absolute terms and as a
percent change.

Compared to January and February, 2020, arXiv preprints in May and June,
2020 had over 10000 more male authors, while female authors increased by
about 3200 over the same
period.

### Comparing single-authored arXiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Again, what about for sole authorships? How does early 2020 compare to
during the
pandemic?

``` r
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n == 1) %>% group_by(COVID) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n) #Make wide data long
arxiv.sole.long$gender <- as.factor(arxiv.sole.long$gender) #Make sure gender is a factor
arxiv.sole.long$per <- NA
for (i in 1:length(arxiv.sole.long$n)) {
  arxiv.sole.long$per[i] <- ifelse(i != 1 & i != 4, (arxiv.sole.long$n[i]/arxiv.sole.long$n[i-1]-1)*100, NA)
}
bump = 150 #Set for figure annotation
arxiv.sole.long$y <- arxiv.sole.long$n +bump
arxiv.sole.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation

#Make figure
p8 <- ggplot(data=arxiv.sole.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="sole authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p8
```

![](README_files/figure-gfm/Early%202020%20arXiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)
arxiv.sole.long$day_of_week <- wday(arxiv.sole.long$`as.Date(submitted)`, label=TRUE)
arxiv.sole.long$day <- as.numeric(arxiv.sole.long$`as.Date(submitted)`)-18261
arxiv.sole.long$day_of_week <- factor(arxiv.sole.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm2 <- lm(n~`as.Date(submitted)`*gender+day_of_week, data=arxiv.sole.long)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = n ~ `as.Date(submitted)` * gender + day_of_week, 
    ##     data = arxiv.sole.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -21.241  -5.013  -0.695   5.122  40.264 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       -3.270e+01  1.897e+02  -0.172 0.863221    
    ## `as.Date(submitted)`               1.748e-03  1.034e-02   0.169 0.865758    
    ## gendermale.n                      -8.538e+02  2.682e+02  -3.184 0.001581 ** 
    ## day_of_weekMon                     1.445e+01  1.436e+00  10.065  < 2e-16 ***
    ## day_of_weekTue                     1.112e+01  1.436e+00   7.744 1.03e-13 ***
    ## day_of_weekWed                     1.037e+01  1.436e+00   7.221 3.17e-12 ***
    ## day_of_weekThu                     1.085e+01  1.436e+00   7.552 3.68e-13 ***
    ## day_of_weekFri                     5.648e+00  1.436e+00   3.933 0.000101 ***
    ## day_of_weekSat                    -3.570e+00  1.436e+00  -2.486 0.013379 *  
    ## `as.Date(submitted)`:gendermale.n  4.833e-02  1.461e-02   3.308 0.001037 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.323 on 354 degrees of freedom
    ## Multiple R-squared:  0.8583, Adjusted R-squared:  0.8547 
    ## F-statistic: 238.3 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm2, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                              Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)                     1.6   1  0.0297  0.863221    
    ## `as.Date(submitted)`            1.5   1  0.0286  0.865758    
    ## gender                        543.6   1 10.1377  0.001581 ** 
    ## day_of_week                 13587.1   6 42.2299 < 2.2e-16 ***
    ## `as.Date(submitted)`:gender   586.7   1 10.9410  0.001037 ** 
    ## Residuals                   18982.7 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)
colnames(arxiv.sole.long) <- c("week", "gender", "n")

p9 <- ggplot(data=subset(arxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="sole authors")
p9
```

![](README_files/figure-gfm/Early%202020%20arXiv%20sole%20author%20analysis-2.png)<!-- -->

Again, there is a bigger uptick in male than female sole-authorships
during the pandemic, and the difference is larger than in the full
dataset, which is dominated by multi-authored
papers.

### First authors

``` r
arxiv.first <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(COVID, first.author.gender) %>% summarize(n=n()))) #Summarize by month
arxiv.first$per <- NA
for (i in 1:length(arxiv.first$n)) {
  arxiv.first$per[i] <- ifelse(i != 1 & i != 2, (arxiv.first$n[i]/arxiv.first$n[i-2]-1)*100, NA)
}
bump = 1000 #Set for figure annotation
arxiv.first$y <- arxiv.first$n +bump
arxiv.first$x <- c(0.4, 1.4, 0.87, 1.87, 1.17,2.17) #X coordinates for figure text annotation

#Make figure
p10 <- ggplot(data=arxiv.first, aes(fill=COVID, y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="first authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p10
```

![](README_files/figure-gfm/Early%202020%20arXiv%20first%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.first <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(as.Date(submitted), first.author.gender) %>% summarize(n=n())))
arxiv.first$day_of_week <- wday(arxiv.first$`as.Date(submitted)`, label=TRUE)
arxiv.first$day <- as.numeric(arxiv.first$`as.Date(submitted)`)-18261
arxiv.first$day_of_week <- factor(arxiv.first$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm3 <- lm(n~day*first.author.gender+day_of_week, data=arxiv.first)
summary(lm3)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * first.author.gender + day_of_week, data = arxiv.first)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -134.533  -22.758   -4.974   30.228   98.729 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -17.50580    6.89068  -2.541   0.0115 *  
    ## day                           0.11822    0.04935   2.396   0.0171 *  
    ## first.author.gendermale     142.02762    7.36086  19.295  < 2e-16 ***
    ## day_of_weekMon              105.70923    6.85719  15.416  < 2e-16 ***
    ## day_of_weekTue              102.16847    6.85745  14.899  < 2e-16 ***
    ## day_of_weekWed               90.54768    6.85852  13.202  < 2e-16 ***
    ## day_of_weekThu               92.10307    6.85790  13.430  < 2e-16 ***
    ## day_of_weekFri               71.35076    6.85745  10.405  < 2e-16 ***
    ## day_of_weekSat               -9.90154    6.85719  -1.444   0.1496    
    ## day:first.author.gendermale   0.34510    0.06976   4.947 1.17e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 34.96 on 354 degrees of freedom
    ## Multiple R-squared:  0.8927, Adjusted R-squared:   0.89 
    ## F-statistic: 327.3 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm3, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                         Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)               7890   1   6.4542   0.01150 *  
    ## day                       7015   1   5.7385   0.01712 *  
    ## first.author.gender     455138   1 372.2962 < 2.2e-16 ***
    ## day_of_week             743614   6 101.3776 < 2.2e-16 ***
    ## day:first.author.gender  29915   1  24.4697 1.169e-06 ***
    ## Residuals               432770 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
arxiv.first <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1)), first.author.gender) %>% summarize(n=n())))
colnames(arxiv.first) <- c("week", "gender", "n")

p11 <- ggplot(data=subset(arxiv.first, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="first authors")
p11
```

![](README_files/figure-gfm/Early%202020%20arXiv%20first%20author%20analysis-2.png)<!-- -->

### Last authors

``` r
arxiv.last <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(COVID, last.author.gender) %>% summarize(n=n()))) #Summarize by month
arxiv.last$per <- NA
for (i in 1:length(arxiv.last$n)) {
  arxiv.last$per[i] <- ifelse(i != 1 & i != 2, (arxiv.last$n[i]/arxiv.last$n[i-2]-1)*100, NA)
}
bump = 1000 #Set for figure annotation
arxiv.last$y <- arxiv.last$n +bump
arxiv.last$x <- c(0.4, 1.4, 0.87, 1.87, 1.17,2.17) #X coordinates for figure text annotation

#Make figure
p12 <- ggplot(data=arxiv.last, aes(fill=COVID, y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="last authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p12
```

![](README_files/figure-gfm/Early%202020%20arXiv%20last%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.last <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(as.Date(submitted), last.author.gender) %>% summarize(n=n())))
arxiv.last$day_of_week <- wday(arxiv.last$`as.Date(submitted)`, label=TRUE)
arxiv.last$day <- as.numeric(arxiv.last$`as.Date(submitted)`)-18261
arxiv.last$day_of_week <- factor(arxiv.last$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm4 <- lm(n~day*last.author.gender+day_of_week, data=arxiv.last)
summary(lm4)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * last.author.gender + day_of_week, data = arxiv.last)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -122.050  -21.759   -5.517   31.188   84.025 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -21.07383    6.44638  -3.269  0.00118 ** 
    ## day                          0.09792    0.04617   2.121  0.03461 *  
    ## last.author.gendermale     126.99387    6.88625  18.442  < 2e-16 ***
    ## day_of_weekMon              92.10312    6.41505  14.357  < 2e-16 ***
    ## day_of_weekTue              92.37931    6.41530  14.400  < 2e-16 ***
    ## day_of_weekWed              81.66445    6.41630  12.728  < 2e-16 ***
    ## day_of_weekThu              83.24834    6.41572  12.976  < 2e-16 ***
    ## day_of_weekFri              65.33222    6.41530  10.184  < 2e-16 ***
    ## day_of_weekSat              -5.04543    6.41505  -0.786  0.43210    
    ## day:last.author.gendermale   0.36715    0.06527   5.625 3.76e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 32.71 on 354 degrees of freedom
    ## Multiple R-squared:  0.8888, Adjusted R-squared:  0.886 
    ## F-statistic: 314.5 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm4, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                        Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)             11434   1  10.6870  0.001185 ** 
    ## day                      4814   1   4.4989  0.034611 *  
    ## last.author.gender     363884   1 340.0949 < 2.2e-16 ***
    ## day_of_week            568515   6  88.5581 < 2.2e-16 ***
    ## day:last.author.gender  33859   1  31.6453 3.764e-08 ***
    ## Residuals              378761 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
arxiv.last <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1)), last.author.gender) %>% summarize(n=n())))
colnames(arxiv.last) <- c("week", "gender", "n")

p13 <- ggplot(data=subset(arxiv.last, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="last authors")
p13
```

![](README_files/figure-gfm/Early%202020%20arXiv%20last%20author%20analysis-2.png)<!-- -->

### Middle authors

``` r
arxiv.middle <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(COVID) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE)))) #Summarize by month
arxiv.middle.long <- gather(arxiv.middle, gender, n, female.n:male.n) #Make wide data long
arxiv.middle.long$gender <- as.factor(arxiv.middle.long$gender) #Make sure gender is a factor
levels(arxiv.middle.long$gender) <- c("Female", "Male") #Capitalize genders
arxiv.middle.long$per <- NA
for (i in 1:length(arxiv.middle.long$n)) {
  arxiv.middle.long$per[i] <- ifelse(i != 1 & i != 4, (arxiv.middle.long$n[i]/arxiv.middle.long$n[i-1]-1)*100, NA)
}
bump = 3000 #Set for figure annotation
arxiv.middle.long$y <- arxiv.middle.long$n +bump
arxiv.middle.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation
m.labels=c("Jan. 1 - Feb. 29, 2020", "Mar. 1 - Apr. 30, 2020", "May 1 - Jun. 30, 2020")
colours2 = c("#deebf7", "#9ecae1", "#3182bd") 

#Make figure
p14 <- ggplot(data=arxiv.middle.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="middle authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p14
```

![](README_files/figure-gfm/Early%202020%20arXiv%20middle%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.middle <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
arxiv.middle.long <- gather(arxiv.middle, gender, n, female.n:male.n)
arxiv.middle.long$day_of_week <- wday(arxiv.middle.long$`as.Date(submitted)`, label=TRUE)
arxiv.middle.long$day <- as.numeric(arxiv.middle.long$`as.Date(submitted)`)-18261
arxiv.middle.long$day_of_week <- factor(arxiv.middle.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm5 <- lm(n~day*gender+day_of_week, data=arxiv.middle.long)
summary(lm5)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * gender + day_of_week, data = arxiv.middle.long)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -114.028  -33.390   -7.703   39.251  160.037 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -38.86133    9.50183  -4.090 5.35e-05 ***
    ## day                0.13939    0.06805   2.048   0.0413 *  
    ## gendermale.n     132.40544   10.15018  13.045  < 2e-16 ***
    ## day_of_weekMon   136.64007    9.45565  14.451  < 2e-16 ***
    ## day_of_weekTue   144.83784    9.45601  15.317  < 2e-16 ***
    ## day_of_weekWed   124.68971    9.45748  13.184  < 2e-16 ***
    ## day_of_weekThu   125.31055    9.45663  13.251  < 2e-16 ***
    ## day_of_weekFri   114.08524    9.45601  12.065  < 2e-16 ***
    ## day_of_weekSat    -1.58238    9.45565  -0.167   0.8672    
    ## day:gendermale.n   0.44107    0.09620   4.585 6.31e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 48.21 on 354 degrees of freedom
    ## Multiple R-squared:  0.8354, Adjusted R-squared:  0.8312 
    ## F-statistic: 199.7 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm5, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##              Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)   38883   1  16.7271 5.347e-05 ***
    ## day            9754   1   4.1959   0.04126 *  
    ## gender       395557   1 170.1627 < 2.2e-16 ***
    ## day_of_week 1282952   6  91.9845 < 2.2e-16 ***
    ## day:gender    48867   1  21.0217 6.307e-06 ***
    ## Residuals    822901 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)

#Alternate figure format
arxiv.middle <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
arxiv.middle.long <- gather(arxiv.middle, gender, n, female.n:male.n)
colnames(arxiv.middle.long) <- c("week", "gender", "n")
arxiv.middle.long$COVID <- ifelse(as.Date(arxiv.middle.long$week) < who, "no", "yes")

p15 <- ggplot(data=subset(arxiv.middle.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+labs(title="arXiv", subtitle="middle authors")+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")
p15
```

![](README_files/figure-gfm/Early%202020%20arXiv%20middle%20author%20analysis-2.png)<!-- -->

### Omnibus figures

``` r
#Year over year
p16 <- plot_grid(p1, p2, p3, p4, p5, align = 'v', axis='l')
p16
```

![](README_files/figure-gfm/Combine%20visualizations%20for%20omnibus%20figures-1.png)<!-- -->

``` r
save_plot("year-over-year_arxiv.png", p16, base_height=8, base_width=8, dpi=600)

p17 <- plot_grid(p6, p8, p10, p12, p14, align='v', axis='l')
p17
```

![](README_files/figure-gfm/Combine%20visualizations%20for%20omnibus%20figures-2.png)<!-- -->

``` r
p18 <- plot_grid(p7, p9, p11, p13, p15, align='v', axis='l')
p18
```

![](README_files/figure-gfm/Combine%20visualizations%20for%20omnibus%20figures-3.png)<!-- -->

``` r
save_plot("early2020_v1_arxiv.png", p17, base_height=8, base_width=8, dpi=600)
save_plot("early2020_v2_arxiv.png", p18, base_height=8, base_width=14, dpi=600)
```

## bioRxiv submissions

Next, we scraped submission data from bioRxiv, the main preprint server
for biology. We used the rbiorxiv package, see:

Fraser, N (2020). rbiorxiv. R package,
<https://github.com/nicholasmfraser/rbiorxiv>

We scraped the same date ranges as for the arXiv analysis, above.

``` r
#Not run
#Get all submissions between Jan 1, 2020 and April 22, 2020
df.b.2020 <- biorxiv_content(from = "2020-01-01", to = "2020-04-22", limit = "*", format = "df")
#Get all submissions for March 15 to April 15, 2019
df.b.2019 <- biorxiv_content(from = "2019-03-15", to = "2019-04-15", limit = "*", format = "df")

#Update with April 22 to April 30, 2020 data
df.b.2020.update <- biorxiv_content(from = "2020-04-22", to = "2020-04-30", limit = "*", format = "df")

write.csv(df.b.2020, "Data/biorxiv_2020_data.csv")
write.csv(df.b.2019, "Data/biorxiv_2019_data.csv")
write.csv(df.b.2020.update, "Data/biorxiv_2020_update_data.csv")

#Update with May 1 to June 30, 2020 data
df.b.2020.may <- biorxiv_content(from = "2020-05-01", to = "2020-05-15", limit = "*", format = "df")
df.b.2020.may2 <- biorxiv_content(from = "2020-05-16", to = "2020-05-31", limit = "*", format = "df")
df.b.2020.june <- biorxiv_content(from = "2020-06-01", to = "2020-06-15", limit = "*", format = "df")
df.b.2020.june2 <- biorxiv_content(from = "2020-06-16", to = "2020-06-30", limit = "*", format = "df")
df.b.2020.mayjune <- rbind(df.b.2020.may, df.b.2020.may2, df.b.2020.june, df.b.2020.june2)
write.csv(df.b.2020.mayjune, "Data/biorxiv_2020_MayJune_data.csv")
```

Note that the bioRxiv API only returns first names for corresponding
authors, and not for all authors, so we used a work-around to overcome
this issue and get first names for all authors. Briefly, we used the
package rcrossref to extract full citations (with first names of all
authors) from all bioRxiv dois:

Scott Chamberlain, Hao Zhu, Najko Jahn, Carl Boettiger and Karthik Ram
(2020). rcrossref: Client for Various ‘CrossRef’ ‘APIs’. R package
version 1.0.0. <https://CRAN.R-project.org/package=rcrossref>

(Thanks to @palolili23 for suggesting rcrossref as a work-around\!)

``` r
#Not run
df.b.2019 <- read.csv("Data/biorxiv_2019_data.csv")
df.b.all2020 <- read.csv("Data/biorxiv_2020_data.csv")
df.b.2020.update <- read.csv("Data/biorxiv_2020_update_data.csv")
df.b.2020.mayjune <- read.csv("Data/biorxiv_2020_MayJune_data.csv")

#Get first names for 2019 data
for (i in 1:length(df.b.2019$doi)){
 tmp_doi <- df.b.2019$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.2019$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.2019$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.2019$authors_full[i] <- tmp_authors
 }, error=function(e){})
}
write.csv(df.b.2019, "Data/biorxiv_2019_data_first.names.csv")

#Get first names for updated 2020 data
for (i in 1:length(df.b.2020.update$doi)){
 tmp_doi <- df.b.2020.update$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.2020.update$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.2020.update$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.2020.update$authors_full[i] <- tmp_authors
 }, error=function(e){})
}

write.csv(df.b.2020.update, "Data/biorxiv_2020_update_data_first.names.csv")

#Get first names for 2020 data
for (i in 1:5000){
 tmp_doi <- df.b.all2020$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.all2020$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.all2020$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.all2020$authors_full[i] <- tmp_authors
 }, error=function(e){})
}

for (i in 5001:10000){
 tmp_doi <- df.b.all2020$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.all2020$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.all2020$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.all2020$authors_full[i] <- tmp_authors
 }, error=function(e){})
}

for (i in 10000:length(df.b.all2020$doi)){
 tmp_doi <- df.b.all2020$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.all2020$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.all2020$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.all2020$authors_full[i] <- tmp_authors
 }, error=function(e){})
}
write.csv(df.b.all2020, "Data/biorxiv_2020_data_first.names.csv")

#Get first names for May/June 2020 data    
for (i in 1:length(df.b.2020.mayjune$doi)){
 tmp_doi <- df.b.2020.mayjune$doi[i] 
 tryCatch({
    if (is.na(tmp_doi)) next()
    df.b.2020.mayjune$bib[i] <- cr_cn(dois=tmp_doi)
    tmp_authors <- sub(".*\n\tauthor = \\{", "", df.b.2020.mayjune$bib[i])
    tmp_authors <- sub("\\},.*", "", tmp_authors)
    df.b.2020.mayjune$authors_full[i] <- tmp_authors
 }, error=function(e){})
}

write.csv(df.b.2020.mayjune, "Data/biorxiv_2020_MayJune_data_first.names.csv")
```

We inferred the gender of bioRxiv preprint authors, as above.

``` r
#Not run
df.b.2019 <- read.csv("Data/biorxiv_2019_data_first.names.csv")
df.b.all2020 <- read.csv("Data/biorxiv_2020_data_first.names.csv")
df.b.2020.update <- read.csv("Data/biorxiv_2020_update_data_first.names.csv")
df.b.2020.mayjune <- read.csv("Data/biorxiv_2020_MayJune_data_first.names.csv")

df.b.full <- rbind(df.b.2019, subset(df.b.all2020, as.Date(date) >= "2020-03-15" & as.Date(date) <= "2020-04-15")) #Make year comparison, subsetting 2020 data to just March 15 to April 15
df.b.all2020 <- rbind(df.b.all2020, df.b.2020.update, df.b.2020.mayjune) #Make early 2020 dataset

df.b.full$year <- as.factor(year(as.Date(df.b.full$date))) #Extract year
df.b.all2020$year <- as.factor(year(as.Date(df.b.all2020$date)))

split.b.names <- function(x){strsplit(as.character(x), " and ", fixed=TRUE)} #Function to split strings of author names
last.b.author <- function(x){gsub(".*\\band\\b", "", as.character(x))} #Function to extract last author
first.b.author <- function(x){gsub("\\band\\b.*", "", as.character(x))} #Function to extract first author

#For the year-over-year dataset
df.b.full$split.names <- lapply(df.b.full$authors_full, split.b.names) #Apply functions
df.b.full$first.author <- lapply(df.b.full$authors_full, first.b.author)
df.b.full$last.author <- lapply(df.b.full$authors_full, last.b.author)

all_first_names <- word(unlist(df.b.full$split.names),1) #Make a list of all author first names
gender <- gender(all_first_names, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)]) #Keep only unique names

tmp <- NULL
for(i in 1:length(df.b.full$authors_full)){
  tmp <- as.data.frame(word(unlist(df.b.full$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.b.full$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.b.full$female.n[i] <-  sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

#Predict first author gender (includes sole authors)
df.b.full$first.author.first.name <- word(df.b.full$first.author, 1)
gender <- gender(df.b.full$first.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.b.full$first.author.gender <- getgender[df.b.full$first.author.first.name]

#Predict last author gender (omits sole authors)
df.b.full$author.n <- str_count(df.b.full$authors, pattern = "\\;")+1 #Count author number
df.b.full$last.author.first.name <- ifelse(df.b.full$author.n > 1, word(trimws(as.character(df.b.full$last.author, 1))), NA)
gender <- gender(df.b.full$last.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.b.full$last.author.gender <- getgender[df.b.full$last.author.first.name]

#Count middle authors for each gender
df.b.full$female.mid.authors.n <- df.b.full$female.n - ifelse(df.b.full$first.author.gender == "female", 1, 0) - ifelse(df.b.full$last.author.gender == "female", 1, 0)
df.b.full$male.mid.authors.n <- df.b.full$male.n - ifelse(df.b.full$first.author.gender == "male", 1, 0) - ifelse(df.b.full$last.author.gender == "male", 1, 0)

df.b.full <- df.b.full[!duplicated(df.b.full),] #Remove duplicated rows, if any
df.b.full.output <- as.data.frame(apply(df.b.full,2,as.character)) 
write.csv(df.b.full.output, "Data/biorxiv_full_authors_gender.csv") #Save data

#For the 2020 dataset
df.b.all2020$split.names <- lapply(df.b.all2020$authors_full, split.b.names) #Apply function
df.b.all2020$first.author <- lapply(df.b.all2020$authors_full, first.b.author)
df.b.all2020$last.author <- lapply(df.b.all2020$authors_full, last.b.author)

all_first_names <- word(unlist(df.b.all2020$split.names),1) #Make a list of all author first names
gender <- gender(all_first_names, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)]) #Keep only unique names

tmp <- NULL
for(i in 1:length(df.b.all2020$authors_full)){
  tmp <- as.data.frame(word(unlist(df.b.all2020$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.b.all2020$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.b.all2020$female.n[i] <-  sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

#Predict first author gender (includes sole authors)
df.b.all2020$first.author.first.name <- word(df.b.all2020$first.author, 1)
gender <- gender(df.b.all2020$first.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.b.all2020$first.author.gender <- getgender[df.b.all2020$first.author.first.name]

#Predict last author gender (omits sole authors)
df.b.all2020$author.n <- str_count(df.b.all2020$authors, pattern = "\\;")+1 #Count author number
df.b.all2020$last.author.first.name <- ifelse(df.b.all2020$author.n > 1, word(trimws(as.character(df.b.all2020$last.author, 1))), NA)
gender <- gender(df.b.all2020$last.author.first.name, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)])
getgender <- gender$gender
names(getgender) <- gender$name
df.b.all2020$last.author.gender <- getgender[df.b.all2020$last.author.first.name]

#Count middle authors for each gender
df.b.all2020$female.mid.authors.n <- df.b.all2020$female.n - ifelse(df.b.all2020$first.author.gender == "female", 1, 0) - ifelse(df.b.all2020$last.author.gender == "female", 1, 0)
df.b.all2020$male.mid.authors.n <- df.b.all2020$male.n - ifelse(df.b.all2020$first.author.gender == "male", 1, 0) - ifelse(df.b.all2020$last.author.gender == "male", 1, 0)

df.b.all2020 <- df.b.all2020[!duplicated(df.b.all2020),] #Remove duplicated rows, if any
df.b.all2020.output <- as.data.frame(apply(df.b.all2020,2,as.character)) 
write.csv(df.b.all2020.output, "Data/biorxiv_full_authors_2020_gender.csv") #Save data
```

Next we calculated some summary statistics for the bioRxiv dataset.

``` r
df.b.full <- read.csv("Data/biorxiv_full_authors_gender.csv")
df.b.all2020 <- read.csv("Data/biorxiv_full_authors_2020_gender.csv")

all.biorxiv <- rbind(df.b.all2020, df.b.full) #Combine datasets
all.biorxiv <- all.biorxiv[!duplicated(all.biorxiv), ] #Remove duplicates
total.b.preprints <- length(all.biorxiv$doi) #Total number of preprints
#all.biorxiv$author.n <- str_count(all.biorxiv$authors, pattern = "\\;")+1 #Count author number
all.biorxiv$authors.with.gender <- all.biorxiv$female.n+all.biorxiv$male.n  
total.b.authors <- sum(all.biorxiv$author.n) #Total number of authors
total.b.authors.with.gender <- sum(all.biorxiv$male.n+all.biorxiv$female.n) #Total number of authors with gender inferred
per.b.gender <- round((total.b.authors.with.gender/total.b.authors)*100, 1) #Percent of authors with gender

year.biorxiv.preprints <- length(df.b.full[, "doi"]) #Preprints for just year-over-year comparison
year.biorxiv.authors <- sum(df.b.full[, "male.n"]+df.b.full[, "female.n"]) #Preprint authors with gender for year-over-year comparison

#How many corresponding authors are first versus last authors?
all.biorxiv$cor <- paste0(word(all.biorxiv$author_corresponding, 1), " ", word(all.biorxiv$author_corresponding, -1))
all.biorxiv$last <- paste0(word(trimws(all.biorxiv$last.author), 1), " ", word(all.biorxiv$last.author, -1))
all.biorxiv$cor_is_last <- ifelse(all.biorxiv$author.n > 1, ifelse(as.character(all.biorxiv$cor) == as.character(all.biorxiv$last), TRUE, FALSE), TRUE)
length(which(all.biorxiv$cor_is_last))
```

    ## [1] 20045

``` r
all.biorxiv$first <- paste0(word(trimws(as.character(all.biorxiv$first.author)), 1), " ", word(trimws(all.biorxiv$first.author), -1))
all.biorxiv$cor_is_first <- ifelse(all.biorxiv$author.n > 1, ifelse(as.character(all.biorxiv$cor) == as.character(all.biorxiv$first), TRUE, FALSE), TRUE)
length(which(all.biorxiv$cor_is_first))
```

    ## [1] 9136

``` r
all.biorxiv$cor_is_mid <- all.biorxiv$cor_is_last == FALSE & all.biorxiv$cor_is_first == FALSE

per.cor.last <- length(which(all.biorxiv$cor_is_last))/(length(which(all.biorxiv$cor_is_last))+length(which(all.biorxiv$cor_is_first)))
```

There are 34492 preprints in the full bioRxiv dataset, each with a
single corresponding author. I inferred the gender of 195204
corresponding authors, or 75.5%, with the rest omitted from subsequent
analyses.

For just the comparison of March 15-April 15, 2020 with the same dates
in 2019, there are 7818 bioRxiv preprints with 43125 corresponding
authors for whom I inferred
gender.

### Comparing bioRxiv preprint authors between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many male and female authors were there on bioRxiv preprints between
Mar/Apr 2019 and 2020?

``` r
df.b.full$year <- as.factor(year(as.Date(df.b.full$date))) #Extract year
all <- as.data.frame(ungroup(df.b.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all.long <- gather(all, Gender, number, Female:Male) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
fontsize = 10

#Make figure comparing 2020 to 2019
p19 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(11000,17000), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="biorXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p19
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202019%20versus%202020%20data-1.png)<!-- -->

### Comparing single-authored biorXiv preprints between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many bioRxiv preprints were authored by a single woman versus a
single man in Mar/Apr, 2020, compared to the same dates last year?

``` r
df.b.full$author.n <- str_count(df.b.full$authors, pattern = "\\;")+1
sole.authors <- as.data.frame(ungroup(subset(df.b.full, author.n == 1) %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
sole.long <- gather(sole.authors, Gender, number, Male:Female) #Make wide data long
sole.authors.t <- as.data.frame(t(sole.authors[,-1])) #Transpose
colnames(sole.authors.t) <- c("2019", "2020") #Fix column names
sole.authors.t$per.dif.1920 <- ((sole.authors.t$`2020`-sole.authors.t$`2019`)/(sole.authors.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure for single-authored preprints
p20 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(28,90), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="biorXiv", subtitle = "sole authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p20
```

![](README_files/figure-gfm/bioRxiv%20sole%20authors-1.png)<!-- -->

### Comparing bioRxiv preprint submissions by author position

What if we break it down further by author position, so first, middle,
or
last?

#### First authors

``` r
first.authors <- subset(df.b.full, !is.na(first.author.gender)) %>% group_by(year,first.author.gender) %>% summarize(n=n()) #Summarize by year
first.authors$per.dif.1920 <- c(first.authors[3,3]/first.authors[1,3],first.authors[4,3]/first.authors[2,3] ,first.authors[3,3]/first.authors[1,3], first.authors[4,3]/first.authors[2,3])
first.authors$per.dif.1920 <- (as.numeric(first.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p21 <- ggplot(data=first.authors, aes(fill=as.factor(year), y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(1450,2000), label = paste0("+", round(first.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="bioRxiv", subtitle = "first authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p21
```

![](README_files/figure-gfm/bioRxiv%20year-over-year%20first%20authors-1.png)<!-- -->

#### Last authors

``` r
last.authors <- subset(df.b.full, !is.na(last.author.gender)) %>% group_by(year,last.author.gender) %>% summarize(n=n()) #Summarize by year
last.authors$per.dif.1920 <- c(last.authors[3,3]/last.authors[1,3],last.authors[4,3]/last.authors[2,3] ,last.authors[3,3]/last.authors[1,3], last.authors[4,3]/last.authors[2,3])
last.authors$per.dif.1920 <- (as.numeric(last.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p22 <- ggplot(data=last.authors, aes(fill=as.factor(year), y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(950,2500), label = paste0("+", round(last.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="bioRxiv", subtitle = "last authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p22
```

![](README_files/figure-gfm/bioRxiv%20last%20authors%20year-over-year-1.png)<!-- -->

#### Middle authors

``` r
middle <- as.data.frame(ungroup(df.b.full %>% group_by(year) %>% summarize(Female = sum(female.mid.authors.n, na.rm=TRUE), Male = sum(male.mid.authors.n, na.rm=TRUE)))) #Summarize by year
middle.long <- gather(middle, Gender, number, Female:Male) #Make wide data long
middle.t <- as.data.frame(t(middle[,-1])) #Transpose
colnames(middle.t) <- c("2019", "2020") #Fix column names
middle.t$per.dif.1920 <- ((middle.t$`2020`-middle.t$`2019`)/(middle.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
fontsize = 10

#Make figure comparing 2020 to 2019
p23 <- ggplot(data=middle.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(5200,7500), label = c(paste0("+", round(middle.t$per.dif.1920[1], 1), "%"), paste0("+", round(middle.t$per.dif.1920[2], 1), "%")))+labs(title="bioRxiv", subtitle="middle authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p23
```

![](README_files/figure-gfm/bioRxiv%20middle%20authors%20year-over-year-1.png)<!-- -->

### Comparing bioRxiv preprint submissions in the months before and during COVID-19 pandemic, by gender

As for arXiv submissions, we also compared bioRxiv submissions across
months for early
2020.

``` r
start.date <- as.Date("2020-03-01") #Month WHO declared COVID-19 a pandemic
df.b.all2020$COVID <- ifelse(as.Date(df.b.all2020$date) < start.date, "Jan. 1-Feb. 29, 2020", ifelse(as.Date(df.b.all2020$date) < "2020-05-01", "Mar. 1-Apr. 30, 2020", "May 1-Jun. 30, 2020")) #Classify dates as COVID or not
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(COVID) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n) #Make wide data long
biorxiv.long$gender <- as.factor(biorxiv.long$gender) #Make sure gender is a factor
levels(biorxiv.long$gender) <- c("Female", "Male") #Capitalize genders
biorxiv.long$per <- NA
for (i in 1:length(biorxiv.long$n)) {
  biorxiv.long$per[i] <- ifelse(i != 1 & i != 4, (biorxiv.long$n[i]/biorxiv.long$n[i-1]-1)*100, NA)
}
bump = 3000 #Set for figure annotation
biorxiv.long$y <- biorxiv.long$n +bump
biorxiv.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation
m.labels=c("Jan. 1 - Feb. 29, 2020", "Mar. 1 - Apr. 30, 2020", "May 1 - Jun. 30, 2020")
colours2 = c("#deebf7", "#9ecae1", "#3182bd") 

#Make figure
p24 <- ggplot(data=biorxiv.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("biorxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="biorxiv", subtitle="all authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p24
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-1.png)<!-- -->

``` r
#Model
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)
biorxiv.long$day_of_week <- wday(biorxiv.long$`as.Date(date)`, label=TRUE)
biorxiv.long$day <- as.numeric(biorxiv.long$`as.Date(date)`)-18261
biorxiv.long$day_of_week <- factor(biorxiv.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm6 <- lm(n~day*gender+day_of_week, data=biorxiv.long)
summary(lm6)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * gender + day_of_week, data = biorxiv.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -387.50 -100.51  -11.26   92.13  598.72 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       58.3001    29.9915   1.944 0.052701 .  
    ## day                1.0475     0.2148   4.877 1.63e-06 ***
    ## gendermale.n     143.9971    32.0380   4.495 9.46e-06 ***
    ## day_of_weekMon   111.4020    29.8457   3.733 0.000221 ***
    ## day_of_weekTue   215.1117    29.8469   7.207 3.47e-12 ***
    ## day_of_weekWed   221.2575    29.8515   7.412 9.25e-13 ***
    ## day_of_weekThu   280.4094    29.8488   9.394  < 2e-16 ***
    ## day_of_weekFri   241.2345    29.8469   8.082 1.02e-14 ***
    ## day_of_weekSat    92.0019    29.8457   3.083 0.002213 ** 
    ## day:gendermale.n   0.5626     0.3036   1.853 0.064726 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 152.2 on 354 degrees of freedom
    ## Multiple R-squared:  0.5055, Adjusted R-squared:  0.4929 
    ## F-statistic:  40.2 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm6, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##              Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)   87512   1  3.7787   0.05270 .  
    ## day          550794   1 23.7827 1.632e-06 ***
    ## gender       467848   1 20.2012 9.456e-06 ***
    ## day_of_week 3130880   6 22.5314 < 2.2e-16 ***
    ## day:gender    79513   1  3.4333   0.06473 .  
    ## Residuals   8198420 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)

#Alternate figure format
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)
colnames(biorxiv.long) <- c("week", "gender", "n")
colnames(biorxiv) <- c("week", "female.n", "male.n")

p25 <- ggplot(data=subset(biorxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="all authors")
p25
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-2.png)<!-- -->

### Comparing single-authored bioRxiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Again, what about for sole authorships? How does early 2020 compare to
during the
pandemic?

``` r
df.b.all2020$author.n <- str_count(df.b.all2020$authors, pattern = "\\;")+1
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n == 1) %>% group_by(COVID) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n) #Make wide data long
biorxiv.sole.long$gender <- as.factor(biorxiv.sole.long$gender) #Make sure gender is a factor
biorxiv.sole.long$per <- NA
for (i in 1:length(biorxiv.sole.long$n)) {
  biorxiv.sole.long$per[i] <- ifelse(i != 1 & i != 4, (biorxiv.sole.long$n[i]/biorxiv.sole.long$n[i-1]-1)*100, NA)
}
bump = 20 #Set for figure annotation
biorxiv.sole.long$y <- biorxiv.sole.long$n +bump
biorxiv.sole.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation

#Make figure
p26 <- ggplot(data=biorxiv.sole.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("biorxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="biorxiv", subtitle="sole authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p26
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)
biorxiv.sole.long$day_of_week <- wday(biorxiv.sole.long$`as.Date(date)`, label=TRUE)
biorxiv.sole.long$day <- as.numeric(biorxiv.sole.long$`as.Date(date)`)-18261
biorxiv.sole.long$day_of_week <- factor(biorxiv.sole.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm7 <- lm(n~day*gender+day_of_week, data=biorxiv.sole.long)
summary(lm7)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * gender + day_of_week, data = biorxiv.sole.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7749 -1.3950 -0.6377  0.6049 14.6655 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       0.514264   0.528365   0.973  0.33111   
    ## day               0.003794   0.003739   1.015  0.31092   
    ## gendermale.n      1.782578   0.563680   3.162  0.00171 **
    ## day_of_weekMon    0.313793   0.517461   0.606  0.54466   
    ## day_of_weekTue    0.772085   0.512687   1.506  0.13303   
    ## day_of_weekWed    0.703094   0.522624   1.345  0.17945   
    ## day_of_weekThu    0.394452   0.512732   0.769  0.44226   
    ## day_of_weekFri   -0.014636   0.512696  -0.029  0.97724   
    ## day_of_weekSat    0.335833   0.548808   0.612  0.54100   
    ## day:gendermale.n  0.002897   0.005274   0.549  0.58318   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.533 on 330 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1407 
    ## F-statistic: 7.167 on 9 and 330 DF,  p-value: 1.653e-09

``` r
Anova(lm7, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##              Sum Sq  Df F value  Pr(>F)   
    ## (Intercept)    6.08   1  0.9473 0.33111   
    ## day            6.61   1  1.0299 0.31092   
    ## gender        64.16   1 10.0007 0.00171 **
    ## day_of_week   27.93   6  0.7257 0.62921   
    ## day:gender     1.94   1  0.3017 0.58318   
    ## Residuals   2116.97 330                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)

#Alternate figure format
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)
colnames(biorxiv.sole.long) <- c("week", "gender", "n")

p27 <- ggplot(data=subset(biorxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="sole authors")
p27
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20sole%20author%20analysis-2.png)<!-- -->

### First authors

``` r
biorxiv.first <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(COVID, first.author.gender) %>% summarize(n=n()))) #Summarize by month
biorxiv.first$per <- NA
for (i in 1:length(biorxiv.first$n)) {
  biorxiv.first$per[i] <- ifelse(i != 1 & i != 2, (biorxiv.first$n[i]/biorxiv.first$n[i-2]-1)*100, NA)
}
bump = 400 #Set for figure annotation
biorxiv.first$y <- biorxiv.first$n +bump
biorxiv.first$x <- c(0.4, 1.4, 0.87, 1.87, 1.17,2.17) #X coordinates for figure text annotation

#Make figure
p28 <- ggplot(data=biorxiv.first, aes(fill=COVID, y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="bioRxiv", subtitle="first authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p28
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20first%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.first <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(as.Date(date), first.author.gender) %>% summarize(n=n())))
biorxiv.first$day_of_week <- wday(biorxiv.first$`as.Date(date)`, label=TRUE)
biorxiv.first$day <- as.numeric(biorxiv.first$`as.Date(date)`)-18261
biorxiv.first$day_of_week <- factor(biorxiv.first$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm8 <- lm(n~day*first.author.gender+day_of_week, data=biorxiv.first)
summary(lm8)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * first.author.gender + day_of_week, data = biorxiv.first)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.085 -13.316  -1.193  12.471  74.261 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 10.86265    3.97566   2.732 0.006605 ** 
    ## day                          0.12093    0.02847   4.247 2.77e-05 ***
    ## first.author.gendermale     14.04062    4.24694   3.306 0.001043 ** 
    ## day_of_weekMon              15.04863    3.95634   3.804 0.000168 ***
    ## day_of_weekTue              28.86649    3.95649   7.296 1.96e-12 ***
    ## day_of_weekWed              27.95932    3.95711   7.066 8.54e-12 ***
    ## day_of_weekThu              37.10411    3.95675   9.377  < 2e-16 ***
    ## day_of_weekFri              31.21043    3.95649   7.888 3.85e-14 ***
    ## day_of_weekSat              11.35522    3.95634   2.870 0.004350 ** 
    ## day:first.author.gendermale  0.04549    0.04025   1.130 0.259139    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.17 on 354 degrees of freedom
    ## Multiple R-squared:  0.4203, Adjusted R-squared:  0.4055 
    ## F-statistic: 28.51 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm8, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                         Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)               3038   1  7.4654  0.006605 ** 
    ## day                       7341   1 18.0395 2.769e-05 ***
    ## first.author.gender       4448   1 10.9300  0.001043 ** 
    ## day_of_week              54087   6 22.1508 < 2.2e-16 ***
    ## day:first.author.gender    520   1  1.2775  0.259139    
    ## Residuals               144063 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
biorxiv.first <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1)), first.author.gender) %>% summarize(n=n())))
colnames(biorxiv.first) <- c("week", "gender", "n")

p29 <- ggplot(data=subset(biorxiv.first, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="first authors")
p29
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20first%20author%20analysis-2.png)<!-- -->

### Last authors

``` r
biorxiv.last <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(COVID, last.author.gender) %>% summarize(n=n()))) #Summarize by month
biorxiv.last$per <- NA
for (i in 1:length(biorxiv.last$n)) {
  biorxiv.last$per[i] <- ifelse(i != 1 & i != 2, (biorxiv.last$n[i]/biorxiv.last$n[i-2]-1)*100, NA)
}
bump = 400 #Set for figure annotation
biorxiv.last$y <- biorxiv.last$n +bump
biorxiv.last$x <- c(0.4, 1.4, 0.87, 1.87, 1.17,2.17) #X coordinates for figure text annotation

#Make figure
p30 <- ggplot(data=biorxiv.last, aes(fill=COVID, y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("bioRxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="biorxiv", subtitle="last authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p30
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.last <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(as.Date(date), last.author.gender) %>% summarize(n=n())))
biorxiv.last$day_of_week <- wday(biorxiv.last$`as.Date(date)`, label=TRUE)
biorxiv.last$day <- as.numeric(biorxiv.last$`as.Date(date)`)-18261
biorxiv.last$day_of_week <- factor(biorxiv.last$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm9 <- lm(n~day*last.author.gender+day_of_week, data=biorxiv.last)
summary(lm9)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * last.author.gender + day_of_week, data = biorxiv.last)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -66.550 -12.888  -0.579  11.687  80.590 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.36825    4.30636  -0.086 0.931902    
    ## day                         0.07643    0.03084   2.478 0.013669 *  
    ## last.author.gendermale     35.78131    4.60020   7.778 8.12e-14 ***
    ## day_of_weekMon             14.40534    4.28543   3.361 0.000860 ***
    ## day_of_weekTue             28.94529    4.28560   6.754 5.91e-11 ***
    ## day_of_weekWed             27.74404    4.28626   6.473 3.21e-10 ***
    ## day_of_weekThu             36.99553    4.28587   8.632  < 2e-16 ***
    ## day_of_weekFri             31.80471    4.28560   7.421 8.70e-13 ***
    ## day_of_weekSat             11.11389    4.28543   2.593 0.009897 ** 
    ## day:last.author.gendermale  0.15185    0.04360   3.483 0.000558 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.85 on 354 degrees of freedom
    ## Multiple R-squared:  0.6454, Adjusted R-squared:  0.6364 
    ## F-statistic:  71.6 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm9, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                        Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)                 3   1  0.0073 0.9319017    
    ## day                      2932   1  6.1416 0.0136690 *  
    ## last.author.gender      28887   1 60.5005 8.124e-14 ***
    ## day_of_week             55165   6 19.2557 < 2.2e-16 ***
    ## day:last.author.gender   5792   1 12.1306 0.0005581 ***
    ## Residuals              169026 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
biorxiv.last <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1)), last.author.gender) %>% summarize(n=n())))
colnames(biorxiv.last) <- c("week", "gender", "n")

p31 <- ggplot(data=subset(biorxiv.last, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="last authors")
p31
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-2.png)<!-- -->

### Middle authors

``` r
biorxiv.middle <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(COVID) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE)))) #Summarize by month
biorxiv.middle.long <- gather(biorxiv.middle, gender, n, female.n:male.n) #Make wide data long
biorxiv.middle.long$gender <- as.factor(biorxiv.middle.long$gender) #Make sure gender is a factor
levels(biorxiv.middle.long$gender) <- c("Female", "Male") #Capitalize genders
biorxiv.middle.long$per <- NA
for (i in 1:length(biorxiv.middle.long$n)) {
  biorxiv.middle.long$per[i] <- ifelse(i != 1 & i != 4, (biorxiv.middle.long$n[i]/biorxiv.middle.long$n[i-1]-1)*100, NA)
}
bump = 1200 #Set for figure annotation
biorxiv.middle.long$y <- biorxiv.middle.long$n +bump
biorxiv.middle.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation
m.labels=c("Jan. 1 - Feb. 29, 2020", "Mar. 1 - Apr. 30, 2020", "May 1 - Jun. 30, 2020")
colours2 = c("#deebf7", "#9ecae1", "#3182bd") 

#Make figure
p32 <- ggplot(data=biorxiv.middle.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("biorxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="bioRxiv", subtitle="middle authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p32
```

![](README_files/figure-gfm/Early%202020%20biorxiv%20middle%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.middle <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
biorxiv.middle.long <- gather(biorxiv.middle, gender, n, female.n:male.n)
biorxiv.middle.long$day_of_week <- wday(biorxiv.middle.long$`as.Date(date)`, label=TRUE)
biorxiv.middle.long$day <- as.numeric(biorxiv.middle.long$`as.Date(date)`)-18261
biorxiv.middle.long$day_of_week <- factor(biorxiv.middle.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm10 <- lm(n~day*gender+day_of_week, data=biorxiv.middle.long)
summary(lm10)
```

    ## 
    ## Call:
    ## lm(formula = n ~ day * gender + day_of_week, data = biorxiv.middle.long)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -194.344  -48.451   -8.246   43.556  303.799 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       25.9728    14.6702   1.770 0.077515 .  
    ## day                0.5703     0.1051   5.428 1.06e-07 ***
    ## gendermale.n      56.3116    15.6713   3.593 0.000373 ***
    ## day_of_weekMon    45.0862    14.5989   3.088 0.002172 ** 
    ## day_of_weekTue    92.6532    14.5995   6.346 6.75e-10 ***
    ## day_of_weekWed   115.0013    14.6018   7.876 4.19e-14 ***
    ## day_of_weekThu   134.9914    14.6005   9.246  < 2e-16 ***
    ## day_of_weekFri   118.5776    14.5995   8.122 7.71e-15 ***
    ## day_of_weekSat    46.3753    14.5989   3.177 0.001621 ** 
    ## day:gendermale.n   0.2254     0.1485   1.518 0.129945    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 74.44 on 354 degrees of freedom
    ## Multiple R-squared:  0.4723, Adjusted R-squared:  0.4589 
    ## F-statistic:  35.2 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm10, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##              Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)   17369   1  3.1345 0.0775147 .  
    ## day          163270   1 29.4646 1.059e-07 ***
    ## gender        71547   1 12.9118 0.0003727 ***
    ## day_of_week  760890   6 22.8858 < 2.2e-16 ***
    ## day:gender    12766   1  2.3039 0.1299454    
    ## Residuals   1961590 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)

#Alternate figure format
biorxiv.middle <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
biorxiv.middle.long <- gather(biorxiv.middle, gender, n, female.n:male.n)
colnames(biorxiv.middle.long) <- c("week", "gender", "n")
biorxiv.middle.long$COVID <- ifelse(as.Date(biorxiv.middle.long$week) < who, "no", "yes")

p33 <- ggplot(data=subset(biorxiv.middle.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+labs(title="biorxiv", subtitle="middle authors")+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")
p33
```

![](README_files/figure-gfm/Early%202020%20biorxiv%20middle%20author%20analysis-2.png)<!-- -->

### Omnibus figures

``` r
#Year over year
p34 <- plot_grid(p19, p20, p21, p22, p23, align = 'v', axis='l')
p34
```

![](README_files/figure-gfm/Combine%20biorxiv%20visualizations%20for%20omnibus%20figures-1.png)<!-- -->

``` r
save_plot("year-over-year_biorxiv.png", p34, base_height=8, base_width=8, dpi=600)

p35 <- plot_grid(p24, p26, p28, p30, p32, align='v', axis='l')
p35
```

![](README_files/figure-gfm/Combine%20biorxiv%20visualizations%20for%20omnibus%20figures-2.png)<!-- -->

``` r
p36 <- plot_grid(p25, p27, p29, p31, p33, align='v', axis='l')
p36
```

![](README_files/figure-gfm/Combine%20biorxiv%20visualizations%20for%20omnibus%20figures-3.png)<!-- -->

``` r
save_plot("early2020_biorxiv_v1.png", p35, base_height=8, base_width=8, dpi=600)
save_plot("early2020_biorxiv_v2.png", p36, base_height=8, base_width=14, dpi=600)
```

``` r
#p9 <- plot_grid(p6, p8, nrow=2, align = 'v', axis='l')
#p9

#save_plot("figure.png", p9, base_height=8, base_width=8, dpi=600)
```

## Tracking individual authors over time

``` r
#split.names.unlist <- function(x){as.data.frame(unlist(strsplit(as.character(x), "|", fixed=TRUE)))} #Function to split strings of author names

#arxiv.authors.2020 <- lapply(df.all2020$authors, split.names.unlist)
#arxiv.authors.2020 <- as.data.frame(unlist(arxiv.authors.2020))
#colnames(arxiv.authors.2020) <- c("authors")
#count.arxiv.authors.2020 <- count(arxiv.authors.2020, authors)
```
