The Pandemic Penalty: COVID-19’s gendered impact on academic
productivity
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

We predicted the genders of preprint authors, and summarized the data as
the number of men and women authors of each preprint, regardless of
author order. This code takes a while to run, so it is not run when
knitting this markdown document.

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

#For the year-over-year dataset
df.full$split.names <- lapply(df.full$authors, split.names) #Apply function

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

df.full.output <- as.data.frame(apply(df.full,2,as.character)) 
write.csv(df.full.output, "Data/arxiv_full_gender.csv") #Save data

#Same for the early 2020 dataset
df.all2020$split.names <- lapply(df.all2020$authors, split.names)

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

df.all2020.output <- as.data.frame(apply(df.all2020,2,as.character))
write.csv(df.all2020.output, "Data/arxiv_all2020_gender.csv") #Save data
```

Next, we calculated some summary statistics for the arXiv dataset we
assembled.

``` r
df.full <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/arxiv_full_gender.csv") #Read in data
df.full <- df.full[!duplicated(df.full), ] #Remove duplicates, if any
df.full$author.n <- str_count(df.full$authors, pattern = "\\|")+1 #Count author number
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year

df.all2020 <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/arxiv_all2020_gender.csv") #Read in data
df.all2020 <- df.all2020[!duplicated(df.all2020), ] #Remove duplicated rows, if any
df.all2020$author.n <- str_count(df.all2020$authors, pattern = "\\|")+1 #Count author number
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
women authors, both as a percent change and in absolute
terms.

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
p3 <- ggplot(data=arxiv.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p3
```

![](README_files/figure-gfm/Early%202020%20arXiv%20analysis-1.png)<!-- -->

``` r
#Model
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)

lm1 <- lm(n~`as.Date(submitted)`*gender, data=arxiv.long)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = n ~ `as.Date(submitted)` * gender, data = arxiv.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -664.51 -101.82   34.77  128.31  459.20 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       -8.614e+03  5.801e+03  -1.485 0.138427    
    ## `as.Date(submitted)`               4.816e-01  3.161e-01   1.524 0.128431    
    ## gendermale.n                      -2.689e+04  8.203e+03  -3.278 0.001147 ** 
    ## `as.Date(submitted)`:gendermale.n  1.500e+00  4.470e-01   3.356 0.000874 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 224 on 360 degrees of freedom
    ## Multiple R-squared:  0.6865, Adjusted R-squared:  0.6839 
    ## F-statistic: 262.8 on 3 and 360 DF,  p-value: < 2.2e-16

``` r
Anova(lm1, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                               Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)                   110668   1  2.2051 0.1384270    
    ## `as.Date(submitted)`          116535   1  2.3220 0.1284309    
    ## gender                        539290   1 10.7457 0.0011470 ** 
    ## `as.Date(submitted)`:gender   565342   1 11.2648 0.0008741 ***
    ## Residuals                   18067171 360                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)
colnames(arxiv.long) <- c("week", "gender", "n")
who <- "2020-03-11"
arxiv.long$COVID <- ifelse(as.Date(arxiv.long$week) < who, "no", "yes")

p4 <- ggplot(data=subset(arxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+labs(title="arXiv", subtitle="all authors")+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")
p4
```

![](README_files/figure-gfm/Early%202020%20arXiv%20analysis-2.png)<!-- -->

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
p5 <- ggplot(data=arxiv.sole.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="sole authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p5
```

![](README_files/figure-gfm/Early%202020%20arXiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)

lm2 <- lm(n~`as.Date(submitted)`*gender, data=arxiv.sole.long)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = n ~ `as.Date(submitted)` * gender, data = arxiv.sole.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.234  -3.438  -0.076   4.571  33.286 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                       -2.934e+01  2.463e+02  -0.119   0.9052  
    ## `as.Date(submitted)`               1.946e-03  1.342e-02   0.145   0.8848  
    ## gendermale.n                      -8.538e+02  3.483e+02  -2.451   0.0147 *
    ## `as.Date(submitted)`:gendermale.n  4.833e-02  1.898e-02   2.547   0.0113 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.512 on 360 degrees of freedom
    ## Multiple R-squared:  0.7569, Adjusted R-squared:  0.7549 
    ## F-statistic: 373.7 on 3 and 360 DF,  p-value: < 2.2e-16

``` r
Anova(lm2, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                             Sum Sq  Df F value  Pr(>F)  
    ## (Intercept)                      1   1  0.0142 0.90523  
    ## `as.Date(submitted)`             2   1  0.0210 0.88480  
    ## gender                         544   1  6.0087 0.01471 *
    ## `as.Date(submitted)`:gender    587   1  6.4848 0.01130 *
    ## Residuals                    32570 360                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)
colnames(arxiv.sole.long) <- c("week", "gender", "n")

p6 <- ggplot(data=subset(arxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="sole authors")
p6
```

![](README_files/figure-gfm/Early%202020%20arXiv%20sole%20author%20analysis-2.png)<!-- -->

Again, there is a bigger uptick in male than female sole-authorships
during the pandemic, and the difference is larger than in the full
dataset, which is dominated by multi-authored papers.

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

#For the year-over-year dataset
df.b.full$split.names <- lapply(df.b.full$authors_full, split.b.names) #Apply function

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

df.b.full <- df.b.full[!duplicated(df.b.full),] #Remove duplicated rows, if any
df.b.full.output <- as.data.frame(apply(df.b.full,2,as.character)) 
write.csv(df.b.full.output, "Data/biorxiv_full_authors_gender.csv") #Save data

#For the 2020 dataset
df.b.all2020$split.names <- lapply(df.b.all2020$authors_full, split.b.names) #Apply function

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
all.biorxiv$author.n <- str_count(all.biorxiv$authors, pattern = "\\;")+1 #Count author number
all.biorxiv$authors.with.gender <- all.biorxiv$female.n+all.biorxiv$male.n  
total.b.authors <- sum(all.biorxiv$author.n) #Total number of authors
total.b.authors.with.gender <- sum(all.biorxiv$male.n+all.biorxiv$female.n) #Total number of authors with gender inferred
per.b.gender <- round((total.b.authors.with.gender/total.b.authors)*100, 1) #Percent of authors with gender

year.biorxiv.preprints <- length(df.b.full[, "doi"]) #Preprints for just year-over-year comparison
year.biorxiv.authors <- sum(df.b.full[, "male.n"]+df.b.full[, "female.n"]) #Preprint authors with gender for year-over-year comparison
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

How many male and female corresponding authors were there on bioRxiv
preprints between Mar/Apr 2019 and 2020?

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
p7 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(11000,17000), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="biorXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p7
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
p8 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(28,90), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="biorXiv", subtitle = "sole authors")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p8
```

![](README_files/figure-gfm/bioRxiv%20sole%20authors-1.png)<!-- -->

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
p9 <- ggplot(data=biorxiv.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("biorxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label= ifelse(is.na(per), "", paste0("+", round(per), "%"))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="biorxiv", subtitle="all authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p9
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-1.png)<!-- -->

``` r
#Model
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)

lm3 <- lm(n~`as.Date(date)`*gender, data=biorxiv.long)
summary(lm3)
```

    ## 
    ## Call:
    ## lm(formula = n ~ `as.Date(date)` * gender, data = biorxiv.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -440.55 -108.55   -7.13  110.66  648.98 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -1.834e+04  4.593e+03  -3.992 7.95e-05 ***
    ## `as.Date(date)`               1.016e+00  2.503e-01   4.061 5.99e-05 ***
    ## gendermale.n                 -1.013e+04  6.496e+03  -1.559    0.120    
    ## `as.Date(date)`:gendermale.n  5.626e-01  3.540e-01   1.590    0.113    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 177.4 on 360 degrees of freedom
    ## Multiple R-squared:  0.3166, Adjusted R-squared:  0.3109 
    ## F-statistic:  55.6 on 3 and 360 DF,  p-value: < 2.2e-16

``` r
Anova(lm3, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                          Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)              501456   1 15.9343 7.950e-05 ***
    ## `as.Date(date)`          519119   1 16.4955 5.988e-05 ***
    ## gender                    76531   1  2.4318    0.1198    
    ## `as.Date(date)`:gender    79513   1  2.5266    0.1128    
    ## Residuals              11329299 360                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Alternate figure format
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)
colnames(biorxiv.long) <- c("week", "gender", "n")
colnames(biorxiv) <- c("week", "female.n", "male.n")
biorxiv$per_female <- biorxiv$female.n/(biorxiv$female.n+biorxiv$male.n)

p10 <- ggplot(data=subset(biorxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="all authors")
p10
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
bump = 150 #Set for figure annotation
biorxiv.sole.long$y <- biorxiv.sole.long$n +bump
biorxiv.sole.long$x <- c(0.4,0.87,1.17,1.4,1.87,2.17) #X coordinates for figure text annotation

#Make figure
p11 <- ggplot(data=biorxiv.sole.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("biorxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y,label=ifelse(is.na(per), "", ifelse(per < 0, paste0(round(per,digits=1), "%"), paste0("+", round(per,digits=1), "%")))))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="biorxiv", subtitle="sole authors")+guides(fill=guide_legend(nrow=3))+scale_x_discrete(labels=c("Women", "Men"))
p11
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)

lm4 <- lm(n~`as.Date(date)`*gender, data=biorxiv.sole.long)
summary(lm4)
```

    ## 
    ## Call:
    ## lm(formula = n ~ `as.Date(date)` * gender, data = biorxiv.sole.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8248 -1.3701 -0.8028  0.6761 15.0089 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                  -68.999839  68.287182  -1.010    0.313
    ## `as.Date(date)`                0.003826   0.003720   1.028    0.304
    ## gendermale.n                 -51.123738  96.572659  -0.529    0.597
    ## `as.Date(date)`:gendermale.n   0.002897   0.005262   0.551    0.582
    ## 
    ## Residual standard error: 2.527 on 336 degrees of freedom
    ## Multiple R-squared:  0.1525, Adjusted R-squared:  0.1449 
    ## F-statistic: 20.15 on 3 and 336 DF,  p-value: 4.96e-12

``` r
Anova(lm4, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: n
    ##                         Sum Sq  Df F value Pr(>F)
    ## (Intercept)               6.52   1  1.0210 0.3130
    ## `as.Date(date)`           6.75   1  1.0577 0.3045
    ## gender                    1.79   1  0.2802 0.5969
    ## `as.Date(date)`:gender    1.94   1  0.3032 0.5822
    ## Residuals              2144.90 336

``` r
#Alternate figure format
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)
colnames(biorxiv.sole.long) <- c("week", "gender", "n")

p12 <- ggplot(data=subset(biorxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=week, y=n, color=gender))+geom_point()+geom_smooth(method="lm")+ylab("Authors (no.)")+xlab("Date")+theme_cowplot()+scale_color_discrete(name="Gender", labels=c("Women", "Men"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="bioRxiv", subtitle="sole authors")
p12
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20sole%20author%20analysis-2.png)<!-- -->

``` r
#p9 <- plot_grid(p6, p8, nrow=2, align = 'v', axis='l')
#p9 #Omnibus figure

#p8 #Updated figure

#save_plot("figure.png", p9, base_height=8, base_width=8, dpi=600)
#save_plot("updated_figure.png", p8, base_height=4, base_width=8, dpi=600)
```

## Tracking individual authors over time

``` r
#split.names.unlist <- function(x){as.data.frame(unlist(strsplit(as.character(x), "|", fixed=TRUE)))} #Function to split strings of author names

#arxiv.authors.2020 <- lapply(df.all2020$authors, split.names.unlist)
#arxiv.authors.2020 <- as.data.frame(unlist(arxiv.authors.2020))
#colnames(arxiv.authors.2020) <- c("authors")
#count.arxiv.authors.2020 <- count(arxiv.authors.2020, authors)
```
