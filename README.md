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

### Comparing arXiv preprint authorships between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many men versus women authorships of preprints were there in Mar/Apr
2020, compared to the same dates last year? Note: this is not the number
of unique authors; it includes authors who submitted multiple preprints.
Thus, 1 “authorship” equals 1 author on 1
paper.

``` r
all <- as.data.frame(ungroup(df.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all.long <- gather(all, Gender, number, Female:Male) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
fontsize = 10

#Make figure comparing 2020 to 2019
p1 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(9000,29000), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="arXiv", subtitle="all authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p1 
```

![](README_files/figure-gfm/Visualize%20arXiv%20year-over-year%20data-1.png)<!-- -->

arXiv preprint submissions are up overall, but the number of men
authorships is currently growing faster than the number of women
authorships. Comparing preprint submissions in late March and early
April 2020 to the same dates in 2019, the number of men authorships has
grown more than the number of women authorships, both as a percent
change and in absolute
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
p2 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(270,1350), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "sole authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p2
```

![](README_files/figure-gfm/Sole%20authors-1.png)<!-- -->

Single-authored arXiv submissions are also up overall, but again the
number of men authorhips is currently growing faster than the number of
women authorships, both as a percent change and in absolute
terms.

### Comparing arXiv preprint submissions by authorship position between Mar/Apr 2019 and Mar/Apr 2020, by gender

What if we break it down further by author position, so first, middle,
or last? First up, first authorships. Note that this includes first
authorships of multi-authored papers as well as sole
authorships.

##### First authorships

``` r
first.authors <- subset(df.full, !is.na(first.author.gender)) %>% group_by(year,first.author.gender) %>% summarize(n=n()) #Summarize by year
first.authors$per.dif.1920 <- c(first.authors[3,3]/first.authors[1,3],first.authors[4,3]/first.authors[2,3] ,first.authors[3,3]/first.authors[1,3], first.authors[4,3]/first.authors[2,3])
first.authors$per.dif.1920 <- (as.numeric(first.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p3 <- ggplot(data=first.authors, aes(fill=as.factor(year), y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(2300,7800), label = paste0("+", round(first.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "first authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p3
```

![](README_files/figure-gfm/arXiv%20year-over-year%20first%20authors-1.png)<!-- -->

The number of men first authorships has grown only very slightly faster
than the number of women first authorships, year-over-year.

##### Last authorships

What about last, or “senior,” authorships of multi-authored
papers?

``` r
last.authors <- subset(df.full, !is.na(last.author.gender)) %>% group_by(year,last.author.gender) %>% summarize(n=n()) #Summarize by year
last.authors$per.dif.1920 <- c(last.authors[3,3]/last.authors[1,3],last.authors[4,3]/last.authors[2,3] ,last.authors[3,3]/last.authors[1,3], last.authors[4,3]/last.authors[2,3])
last.authors$per.dif.1920 <- (as.numeric(last.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p4 <- ggplot(data=last.authors, aes(fill=as.factor(year), y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(1900,7050), label = paste0("+", round(last.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "last authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p4
```

![](README_files/figure-gfm/arXiv%20last%20authors%20year-over-year-1.png)<!-- -->

The number of men last authorships has grown substantially
year-over-year, but the number of women last authorships is almost
unchanged from 2019.

##### Middle authorships

And finally, middle authorships, or all authorships on multi-authored
papers that are not in the first or last
position.

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
p5 <- ggplot(data=middle.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(2700,8000), label = c(paste0(round(middle.t$per.dif.1920[1], 1), "%"), paste0("+", round(middle.t$per.dif.1920[2], 1), "%")))+labs(title="arXiv", subtitle="middle authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p5
```

![](README_files/figure-gfm/arXiv%20middle%20authors%20year-over-year-1.png)<!-- -->

The number of women middle authorships actually declined from Mar/Apr
2019 to Mar/Apr 2020, while the number of men middle authorships rose
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
#Aggregate data by week for figure
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)
colnames(arxiv.long) <- c("week", "gender", "n")
who <- "2020-03-11" #Date the WHO declared COVID-19 a pandemic, for reference
arxiv.long$gender <- factor(arxiv.long$gender, rev(levels(as.factor(arxiv.long$gender))))
colours2 = c("#7fbf7b","#af8dc3") #Set colours

p6 <- ggplot(data=subset(arxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender, shape=gender))+geom_point(size=2)+geom_smooth(method="lm", se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+labs(title="arXiv", subtitle="all authorships")+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p6
```

![](README_files/figure-gfm/Early%202020%20arXiv%20all%20authors%20analysis-1.png)<!-- -->

``` r
#Aggregate data by day for model
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.long <- gather(arxiv, gender, n, female.n:male.n)
arxiv.long$day_of_week <- wday(arxiv.long$`as.Date(submitted)`, label=TRUE)
arxiv.long$date <- as.numeric(arxiv.long$`as.Date(submitted)`)-18261
arxiv.long$day_of_week <- factor(arxiv.long$day_of_week, ordered = FALSE)

#Fit mdoel
lm1 <- lm(sqrt(n)~date*gender+day_of_week, data=arxiv.long)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * gender + day_of_week, data = arxiv.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1336 -1.2236 -0.0177  1.3412  5.3027 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        7.321762   0.406104  18.029  < 2e-16 ***
    ## date               0.016425   0.002908   5.648 3.34e-08 ***
    ## gendermale.n      12.490985   0.433814  28.793  < 2e-16 ***
    ## day_of_weekMon     9.200601   0.404130  22.766  < 2e-16 ***
    ## day_of_weekTue     9.220602   0.404145  22.815  < 2e-16 ***
    ## day_of_weekWed     8.101160   0.404208  20.042  < 2e-16 ***
    ## day_of_weekThu     8.398549   0.404172  20.780  < 2e-16 ***
    ## day_of_weekFri     6.914817   0.404145  17.110  < 2e-16 ***
    ## day_of_weekSat    -0.776936   0.404130  -1.922   0.0553 .  
    ## date:gendermale.n  0.018168   0.004112   4.419 1.32e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.061 on 354 degrees of freedom
    ## Multiple R-squared:  0.943,  Adjusted R-squared:  0.9415 
    ## F-statistic: 650.5 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm1, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##             Sum Sq  Df F value    Pr(>F)    
    ## (Intercept) 1380.3   1 325.056 < 2.2e-16 ***
    ## date         135.4   1  31.895 3.345e-08 ***
    ## gender      3520.4   1 829.062 < 2.2e-16 ***
    ## day_of_week 5897.9   6 231.495 < 2.2e-16 ***
    ## date:gender   82.9   1  19.526 1.321e-05 ***
    ## Residuals   1503.2 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm1)
```

The number of male authorships is growing faster than the number of
female authorships during the
pandemic.

### Comparing single-authored arXiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Again, what about for sole authorships? How does early 2020 compare to
during the
pandemic?

``` r
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)
colnames(arxiv.sole.long) <- c("week", "gender", "n")
arxiv.sole.long$gender <- factor(arxiv.sole.long$gender, rev(levels(as.factor(arxiv.sole.long$gender))))

p7 <- ggplot(data=subset(arxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender, shape=gender))+geom_point(size=2)+geom_smooth(method="lm", se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+labs(title="arXiv", subtitle="sole authorships")+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p7
```

![](README_files/figure-gfm/Early%202020%20arXiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.sole <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.sole.long <- gather(arxiv.sole, gender, n, female.n:male.n)
arxiv.sole.long$day_of_week <- wday(arxiv.sole.long$`as.Date(submitted)`, label=TRUE)
arxiv.sole.long$date <- as.numeric(arxiv.sole.long$`as.Date(submitted)`)-18261
arxiv.sole.long$day_of_week <- factor(arxiv.sole.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm2 <- lm(sqrt(n)~date*gender+day_of_week, data=arxiv.sole.long)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * gender + day_of_week, data = arxiv.sole.long)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.97095 -0.39740  0.00302  0.44899  3.05314 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.6977986  0.1251246  13.569  < 2e-16 ***
    ## date               0.0005422  0.0008961   0.605  0.54554    
    ## gendermale.n       3.4338482  0.1336624  25.690  < 2e-16 ***
    ## day_of_weekMon     1.3774077  0.1245164  11.062  < 2e-16 ***
    ## day_of_weekTue     1.1130060  0.1245213   8.938  < 2e-16 ***
    ## day_of_weekWed     1.0710357  0.1245406   8.600 2.62e-16 ***
    ## day_of_weekThu     1.1343656  0.1245293   9.109  < 2e-16 ***
    ## day_of_weekFri     0.6221684  0.1245213   4.996 9.19e-07 ***
    ## day_of_weekSat    -0.4714561  0.1245164  -3.786  0.00018 ***
    ## date:gendermale.n  0.0034644  0.0012668   2.735  0.00656 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6349 on 354 degrees of freedom
    ## Multiple R-squared:  0.9096, Adjusted R-squared:  0.9073 
    ## F-statistic: 395.6 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm2, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##              Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)  74.217   1 184.1140 < 2.2e-16 ***
    ## date          0.148   1   0.3661  0.545538    
    ## gender      266.048   1 659.9998 < 2.2e-16 ***
    ## day_of_week 146.835   6  60.7105 < 2.2e-16 ***
    ## date:gender   3.015   1   7.4788  0.006557 ** 
    ## Residuals   142.698 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm2)
```

Again, the number of preprints single-authored by men is growing faster
than the number of preprints single-authored by
women.

### Comparing arXiv preprint submissions by authorship position in the months before and during COVID-19 pandemic, by gender

##### First authorships

``` r
arxiv.first <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1)), first.author.gender) %>% summarize(n=n())))
colnames(arxiv.first) <- c("week", "gender", "n")
arxiv.first$gender <- factor(arxiv.first$gender, rev(levels(as.factor(arxiv.first$gender))))

p8 <- ggplot(data=subset(arxiv.first, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm", se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="first authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p8
```

![](README_files/figure-gfm/Early%202020%20arXiv%20first%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.first <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(as.Date(submitted), first.author.gender) %>% summarize(n=n())))
arxiv.first$day_of_week <- wday(arxiv.first$`as.Date(submitted)`, label=TRUE)
arxiv.first$date <- as.numeric(arxiv.first$`as.Date(submitted)`)-18261
arxiv.first$day_of_week <- factor(arxiv.first$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm3 <- lm(sqrt(n)~date*first.author.gender+day_of_week, data=arxiv.first)
summary(lm3)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * first.author.gender + day_of_week, 
    ##     data = arxiv.first)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6488 -0.6026  0.0192  0.7279  4.1469 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   3.973201   0.213432  18.616  < 2e-16 ***
    ## date                          0.007930   0.001529   5.188 3.59e-07 ***
    ## first.author.gendermale       6.763264   0.227995  29.664  < 2e-16 ***
    ## day_of_weekMon                4.445954   0.212394  20.933  < 2e-16 ***
    ## day_of_weekTue                4.317420   0.212403  20.327  < 2e-16 ***
    ## day_of_weekWed                3.896587   0.212436  18.342  < 2e-16 ***
    ## day_of_weekThu                3.970088   0.212416  18.690  < 2e-16 ***
    ## day_of_weekFri                3.221395   0.212403  15.166  < 2e-16 ***
    ## day_of_weekSat               -0.548568   0.212394  -2.583 0.010202 *  
    ## date:first.author.gendermale  0.007791   0.002161   3.606 0.000356 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.083 on 354 degrees of freedom
    ## Multiple R-squared:  0.9411, Adjusted R-squared:  0.9396 
    ## F-statistic: 628.5 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm3, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##                           Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)               406.45   1 346.548 < 2.2e-16 ***
    ## date                       31.57   1  26.913 3.591e-07 ***
    ## first.author.gender      1032.07   1 879.958 < 2.2e-16 ***
    ## day_of_week              1393.66   6 198.042 < 2.2e-16 ***
    ## date:first.author.gender   15.25   1  13.000 0.0003562 ***
    ## Residuals                 415.19 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm3)
```

Men first authorships are growing faster than women first
authorships.

##### Last authors

``` r
arxiv.last <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1)), last.author.gender) %>% summarize(n=n())))
colnames(arxiv.last) <- c("week", "gender", "n")
arxiv.last$gender <- factor(arxiv.last$gender, rev(levels(as.factor(arxiv.last$gender))))

p9 <- ggplot(data=subset(arxiv.last, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm", se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date("2020-03-11")), linetype="dashed")+labs(title="arXiv", subtitle="last authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p9
```

![](README_files/figure-gfm/Early%202020%20arXiv%20last%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.last <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(as.Date(submitted), last.author.gender) %>% summarize(n=n())))
arxiv.last$day_of_week <- wday(arxiv.last$`as.Date(submitted)`, label=TRUE)
arxiv.last$date <- as.numeric(arxiv.last$`as.Date(submitted)`)-18261
arxiv.last$day_of_week <- factor(arxiv.last$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm4 <- lm(sqrt(n)~date*last.author.gender+day_of_week, data=arxiv.last)
summary(lm4)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * last.author.gender + day_of_week, 
    ##     data = arxiv.last)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5259 -0.6473 -0.0091  0.7045  2.5549 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3.371402   0.206114  16.357  < 2e-16 ***
    ## date                         0.007393   0.001476   5.008 8.69e-07 ***
    ## last.author.gendermale       6.584824   0.220178  29.907  < 2e-16 ***
    ## day_of_weekMon               4.138431   0.205112  20.176  < 2e-16 ***
    ## day_of_weekTue               4.096691   0.205120  19.972  < 2e-16 ***
    ## day_of_weekWed               3.677064   0.205152  17.924  < 2e-16 ***
    ## day_of_weekThu               3.794111   0.205134  18.496  < 2e-16 ***
    ## day_of_weekFri               3.075272   0.205120  14.993  < 2e-16 ***
    ## day_of_weekSat              -0.354833   0.205112  -1.730   0.0845 .  
    ## date:last.author.gendermale  0.009278   0.002087   4.446 1.17e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.046 on 354 degrees of freedom
    ## Multiple R-squared:  0.9428, Adjusted R-squared:  0.9414 
    ## F-statistic: 648.6 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm4, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##                          Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)              292.65   1 267.550 < 2.2e-16 ***
    ## date                      27.43   1  25.081 8.689e-07 ***
    ## last.author.gender       978.33   1 894.415 < 2.2e-16 ***
    ## day_of_week             1190.92   6 181.462 < 2.2e-16 ***
    ## date:last.author.gender   21.62   1  19.770 1.171e-05 ***
    ## Residuals                387.21 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm4)
```

Men last authorships are growing faster than women last
authorships.

### Middle authors

``` r
arxiv.middle <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(round_date(as.Date(submitted), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
arxiv.middle.long <- gather(arxiv.middle, gender, n, female.n:male.n)
colnames(arxiv.middle.long) <- c("week", "gender", "n")
arxiv.middle.long$gender <- factor(arxiv.middle.long$gender, rev(levels(as.factor(arxiv.middle.long$gender))))

p10 <- ggplot(data=subset(arxiv.middle.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender, shape=gender))+geom_point(size=2)+geom_smooth(method="lm", se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+labs(title="arXiv", subtitle="middle authorships")+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p10
```

![](README_files/figure-gfm/Early%202020%20arXiv%20middle%20author%20analysis-1.png)<!-- -->

``` r
#Model
arxiv.middle <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-06-30") %>% group_by(as.Date(submitted)) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
arxiv.middle.long <- gather(arxiv.middle, gender, n, female.n:male.n)
arxiv.middle.long$day_of_week <- wday(arxiv.middle.long$`as.Date(submitted)`, label=TRUE)
arxiv.middle.long$day <- as.numeric(arxiv.middle.long$`as.Date(submitted)`)-18261
arxiv.middle.long$day_of_week <- factor(arxiv.middle.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm5 <- lm(sqrt(n)~day*gender+day_of_week, data=arxiv.middle.long)
summary(lm5)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ day * gender + day_of_week, data = arxiv.middle.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8947 -0.9626  0.0034  0.9037  4.0132 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       2.913955   0.285741  10.198  < 2e-16 ***
    ## day               0.008940   0.002046   4.369 1.64e-05 ***
    ## gendermale.n      6.174141   0.305239  20.227  < 2e-16 ***
    ## day_of_weekMon    6.014098   0.284352  21.150  < 2e-16 ***
    ## day_of_weekTue    6.291759   0.284363  22.126  < 2e-16 ***
    ## day_of_weekWed    5.598725   0.284408  19.686  < 2e-16 ***
    ## day_of_weekThu    5.586159   0.284382  19.643  < 2e-16 ***
    ## day_of_weekFri    5.289113   0.284363  18.600  < 2e-16 ***
    ## day_of_weekSat   -0.116213   0.284352  -0.409 0.683012    
    ## day:gendermale.n  0.010543   0.002893   3.644 0.000308 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.45 on 354 degrees of freedom
    ## Multiple R-squared:  0.9087, Adjusted R-squared:  0.9064 
    ## F-statistic: 391.6 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm5, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##              Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)  218.62   1 103.997 < 2.2e-16 ***
    ## day           40.12   1  19.087 1.643e-05 ***
    ## gender       860.10   1 409.142 < 2.2e-16 ***
    ## day_of_week 2543.96   6 201.690 < 2.2e-16 ***
    ## day:gender    27.92   1  13.281 0.0003083 ***
    ## Residuals    744.18 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm5)
```

Men middle authorships are growing faster than women middle authorships.

### Omnibus figures

``` r
#Year over year
p11 <- plot_grid(p1, p2, p3, p4, p5, align = 'v', axis='l')
p11
```

![](README_files/figure-gfm/Combine%20visualizations%20for%20omnibus%20figures-1.png)<!-- -->

``` r
save_plot("year-over-year_arxiv.png", p11, base_height=8, base_width=8, dpi=600)

p12 <- plot_grid(p6, p7, p8, p9, p10, align='v', axis='l')
p12
```

![](README_files/figure-gfm/Combine%20visualizations%20for%20omnibus%20figures-2.png)<!-- -->

``` r
save_plot("early2020_rxiv.png", p12, base_height=8, base_width=14, dpi=600)
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

Next we calculated some summary statistics for the bioRxiv
dataset.

``` r
df.b.full <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/biorxiv_full_authors_gender.csv")
df.b.all2020 <- read.csv("~/Dropbox/Megan2020/Pandemic Penalty/biorxiv_full_authors_2020_gender.csv")

all.biorxiv <- rbind(df.b.all2020, df.b.full) #Combine datasets
all.biorxiv <- all.biorxiv[!duplicated(all.biorxiv), ] #Remove duplicates
total.b.preprints <- length(all.biorxiv$doi) #Total number of preprints
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
authors for whom I inferred gender.

For the sake of interest, 70% of bioRxiv corresponding authors are last
authors.

### Comparing bioRxiv preprint authorships between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many men and women authorships were there on bioRxiv preprints
between Mar/Apr 2019 and
2020?

``` r
all <- as.data.frame(ungroup(df.b.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all.long <- gather(all, Gender, number, Female:Male) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure comparing 2020 to 2019
p13 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(10500,16500), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="biorXiv", subtitle="all authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p13
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202019%20versus%202020%20data-1.png)<!-- -->

Overall, for bioRxiv preprints, women authorships actually increased
more than men authorships year-over-year, as a percent change. (This is
consistent with a long-standing trend in which women were narrowing the
gender gap.) In absolute terms, there was nonetheless a bigger increase
in men than women authorships: between Mar/Apr 2019 and Mar/Apr 2020,
there was an increase of 2669 women authorships and 4168 men
authorships.

### Comparing single-authored biorXiv preprints between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many bioRxiv preprints were authored by a single woman versus a
single man in Mar/Apr, 2020, compared to the same dates last
year?

``` r
sole.authors <- as.data.frame(ungroup(subset(df.b.full, author.n == 1) %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
sole.long <- gather(sole.authors, Gender, number, Male:Female) #Make wide data long
sole.authors.t <- as.data.frame(t(sole.authors[,-1])) #Transpose
colnames(sole.authors.t) <- c("2019", "2020") #Fix column names
sole.authors.t$per.dif.1920 <- ((sole.authors.t$`2020`-sole.authors.t$`2019`)/(sole.authors.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure for single-authored preprints
p14 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(28,90), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="biorXiv", subtitle = "sole authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p14
```

![](README_files/figure-gfm/bioRxiv%20sole%20authors-1.png)<!-- -->

The number of sole-authored preprints increased a little more for men
than women.

### Comparing bioRxiv preprint submissions by author position

What if we break it down further by author position, so first, middle,
or last, as for arXiv
preprints?

##### First authors

``` r
first.authors <- subset(df.b.full, !is.na(first.author.gender)) %>% group_by(year,first.author.gender) %>% summarize(n=n()) #Summarize by year
first.authors$per.dif.1920 <- c(first.authors[3,3]/first.authors[1,3],first.authors[4,3]/first.authors[2,3] ,first.authors[3,3]/first.authors[1,3], first.authors[4,3]/first.authors[2,3])
first.authors$per.dif.1920 <- (as.numeric(first.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p15 <- ggplot(data=first.authors, aes(fill=as.factor(year), y=n, x=first.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(1450,2000), label = paste0("+", round(first.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="bioRxiv", subtitle = "first authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p15
```

![](README_files/figure-gfm/bioRxiv%20year-over-year%20first%20authors-1.png)<!-- -->

Both as a percent change and an absolute change year-over-year, there
was a larger increase in the number of women first authorships than the
number of men first authorships. In absolute terms, there were an
additional 359 women first authorships and 299 men first authorships in
Mar/Apr 2020, compared to Mar/Apr
2019.

##### Last authors

``` r
last.authors <- subset(df.b.full, !is.na(last.author.gender)) %>% group_by(year,last.author.gender) %>% summarize(n=n()) #Summarize by year
last.authors$per.dif.1920 <- c(last.authors[3,3]/last.authors[1,3],last.authors[4,3]/last.authors[2,3] ,last.authors[3,3]/last.authors[1,3], last.authors[4,3]/last.authors[2,3])
last.authors$per.dif.1920 <- (as.numeric(last.authors$per.dif.1920)-1)*100 #Calculate percent change, 2020 over 2019

p16 <- ggplot(data=last.authors, aes(fill=as.factor(year), y=n, x=last.author.gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(950,2550), label = paste0("+", round(last.authors$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="bioRxiv", subtitle = "last authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p16
```

![](README_files/figure-gfm/bioRxiv%20last%20authors%20year-over-year-1.png)<!-- -->

In contrast to first authorships, growth in the number of men last
authorships outpaced growth in the number of women first authorships
between Mar/Apr 2019 and Mar/Apr
2020.

#### Middle authors

``` r
middle <- as.data.frame(ungroup(df.b.full %>% group_by(year) %>% summarize(Female = sum(female.mid.authors.n, na.rm=TRUE), Male = sum(male.mid.authors.n, na.rm=TRUE)))) #Summarize by year
middle.long <- gather(middle, Gender, number, Female:Male) #Make wide data long
middle.t <- as.data.frame(t(middle[,-1])) #Transpose
colnames(middle.t) <- c("2019", "2020") #Fix column names
middle.t$per.dif.1920 <- ((middle.t$`2020`-middle.t$`2019`)/(middle.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure comparing 2020 to 2019
p17 <- ggplot(data=middle.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authorships (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(5200,7500), label = c(paste0("+", round(middle.t$per.dif.1920[1], 1), "%"), paste0("+", round(middle.t$per.dif.1920[2], 1), "%")))+labs(title="bioRxiv", subtitle="middle authorships")+guides(fill=guide_legend(nrow=2))+scale_x_discrete(labels=c("Women", "Men"))
p17
```

![](README_files/figure-gfm/bioRxiv%20middle%20authors%20year-over-year-1.png)<!-- -->

There was also more growth in the number of men than women middle
authorships in Mar/Apr 2020, compared to Mar/Apr
2019.

### Comparing bioRxiv preprint submissions in the months before and during COVID-19 pandemic, by gender

As for arXiv submissions, we also compared bioRxiv submissions across
months for early
2020.

``` r
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)
colnames(biorxiv.long) <- c("week", "gender", "n")
biorxiv.long$gender <- factor(biorxiv.long$gender, rev(levels(as.factor(biorxiv.long$gender))))

p18 <- ggplot(data=subset(biorxiv.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm",se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+labs(title="bioRxiv", subtitle="all authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p18
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-1.png)<!-- -->

``` r
#Model
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.long <- gather(biorxiv, gender, n, female.n:male.n)
biorxiv.long$day_of_week <- wday(biorxiv.long$`as.Date(date)`, label=TRUE)
biorxiv.long$date <- as.numeric(biorxiv.long$`as.Date(date)`)-18261
biorxiv.long$day_of_week <- factor(biorxiv.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm6 <- lm(sqrt(n)~date*gender+day_of_week, data=biorxiv.long)
summary(lm6)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * gender + day_of_week, data = biorxiv.long)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.8665  -2.3926  -0.2277   2.6462  10.0422 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       9.678381   0.754728  12.824  < 2e-16 ***
    ## date              0.032820   0.005405   6.072 3.26e-09 ***
    ## gendermale.n      4.071557   0.806226   5.050 7.08e-07 ***
    ## day_of_weekMon    3.599393   0.751059   4.792 2.43e-06 ***
    ## day_of_weekTue    6.110795   0.751089   8.136 7.00e-15 ***
    ## day_of_weekWed    6.076765   0.751205   8.089 9.67e-15 ***
    ## day_of_weekThu    7.659684   0.751137  10.197  < 2e-16 ***
    ## day_of_weekFri    6.753371   0.751089   8.991  < 2e-16 ***
    ## day_of_weekSat    2.439773   0.751059   3.248  0.00127 ** 
    ## date:gendermale.n 0.006951   0.007641   0.910  0.36358    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.83 on 354 degrees of freedom
    ## Multiple R-squared:  0.5211, Adjusted R-squared:  0.509 
    ## F-statistic: 42.81 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm6, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##             Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept) 2411.8   1 164.4466 < 2.2e-16 ***
    ## date         540.7   1  36.8702 3.255e-09 ***
    ## gender       374.0   1  25.5039 7.083e-07 ***
    ## day_of_week 2353.0   6  26.7404 < 2.2e-16 ***
    ## date:gender   12.1   1   0.8276    0.3636    
    ## Residuals   5191.8 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm6)
```

### Comparing single-authored bioRxiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Again, what about for sole authorships? How does early 2020 compare to
during the
pandemic?

``` r
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n==1) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE))))
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)
colnames(biorxiv.sole.long) <- c("week", "gender", "n")
biorxiv.sole.long$gender <- factor(biorxiv.sole.long$gender, rev(levels(as.factor(biorxiv.sole.long$gender))))

p19 <- ggplot(data=subset(biorxiv.sole.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm",se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+labs(title="bioRxiv", subtitle="sole authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p19
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20sole%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.sole <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & author.n == 1) %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
biorxiv.sole.long <- gather(biorxiv.sole, gender, n, female.n:male.n)
biorxiv.sole.long$day_of_week <- wday(biorxiv.sole.long$`as.Date(date)`, label=TRUE)
biorxiv.sole.long$date <- as.numeric(biorxiv.sole.long$`as.Date(date)`)-18261
biorxiv.sole.long$day_of_week <- factor(biorxiv.sole.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm7 <- lm(sqrt(n)~date*gender+day_of_week, data=biorxiv.sole.long)
summary(lm7)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * gender + day_of_week, data = biorxiv.sole.long)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.77650 -0.61178 -0.00889  0.44511  3.10963 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.4719848  0.1658976   2.845  0.00472 ** 
    ## date              0.0013884  0.0011738   1.183  0.23774    
    ## gendermale.n      0.8063142  0.1769858   4.556 7.36e-06 ***
    ## day_of_weekMon    0.1038179  0.1624737   0.639  0.52328    
    ## day_of_weekTue    0.2953790  0.1609750   1.835  0.06741 .  
    ## day_of_weekWed    0.2560264  0.1640950   1.560  0.11966    
    ## day_of_weekThu    0.2761130  0.1609890   1.715  0.08726 .  
    ## day_of_weekFri    0.0684905  0.1609776   0.425  0.67077    
    ## day_of_weekSat    0.0525647  0.1723163   0.305  0.76052    
    ## date:gendermale.n 0.0007547  0.0016561   0.456  0.64890    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7953 on 330 degrees of freedom
    ## Multiple R-squared:  0.2577, Adjusted R-squared:  0.2374 
    ## F-statistic: 12.73 on 9 and 330 DF,  p-value: < 2.2e-16

``` r
Anova(lm7, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##              Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)   5.119   1  8.0942  0.004718 ** 
    ## date          0.885   1  1.3990  0.237744    
    ## gender       13.126   1 20.7554 7.355e-06 ***
    ## day_of_week   4.318   6  1.1380  0.339825    
    ## date:gender   0.131   1  0.2077  0.648905    
    ## Residuals   208.702 330                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm7)
```

##### First authorships

``` r
biorxiv.first <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1)), first.author.gender) %>% summarize(n=n())))
colnames(biorxiv.first) <- c("week", "gender", "n")
biorxiv.first$gender <- factor(biorxiv.first$gender, rev(levels(as.factor(biorxiv.first$gender))))

p20 <- ggplot(data=subset(biorxiv.first, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm",se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+labs(title="bioRxiv", subtitle="first authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p20
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20first%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.first <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(first.author.gender)) %>% group_by(as.Date(date), first.author.gender) %>% summarize(n=n())))
biorxiv.first$day_of_week <- wday(biorxiv.first$`as.Date(date)`, label=TRUE)
biorxiv.first$date <- as.numeric(biorxiv.first$`as.Date(date)`)-18261
biorxiv.first$day_of_week <- factor(biorxiv.first$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm8 <- lm(sqrt(n)~date*first.author.gender+day_of_week, data=biorxiv.first)
summary(lm8)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * first.author.gender + day_of_week, 
    ##     data = biorxiv.first)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8109 -0.9156  0.0210  1.0121  4.2198 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3.687138   0.285781  12.902  < 2e-16 ***
    ## date                         0.010583   0.002047   5.171 3.91e-07 ***
    ## first.author.gendermale      1.124532   0.305281   3.684 0.000266 ***
    ## day_of_weekMon               1.362123   0.284392   4.790 2.46e-06 ***
    ## day_of_weekTue               2.315623   0.284403   8.142 6.71e-15 ***
    ## day_of_weekWed               2.214889   0.284447   7.787 7.67e-14 ***
    ## day_of_weekThu               2.857271   0.284422  10.046  < 2e-16 ***
    ## day_of_weekFri               2.489683   0.284403   8.754  < 2e-16 ***
    ## day_of_weekSat               0.865025   0.284392   3.042 0.002528 ** 
    ## date:first.author.gendermale 0.001195   0.002893   0.413 0.679836    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.45 on 354 degrees of freedom
    ## Multiple R-squared:  0.4406, Adjusted R-squared:  0.4263 
    ## F-statistic: 30.97 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm8, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##                          Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)              350.03   1 166.4607 < 2.2e-16 ***
    ## date                      56.22   1  26.7377 3.907e-07 ***
    ## first.author.gender       28.53   1  13.5689 0.0002659 ***
    ## day_of_week              327.65   6  25.9694 < 2.2e-16 ***
    ## date:first.author.gender   0.36   1   0.1706 0.6798357    
    ## Residuals                744.39 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm8)
```

##### Last authorships

``` r
biorxiv.last <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1)), last.author.gender) %>% summarize(n=n())))
colnames(biorxiv.last) <- c("week", "gender", "n")
biorxiv.last$gender <- factor(biorxiv.last$gender, rev(levels(as.factor(biorxiv.last$gender))))

p21 <- ggplot(data=subset(biorxiv.last, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week), y=n, color=gender))+geom_point(size=2)+geom_smooth(method="lm",se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+theme_cowplot()+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+labs(title="bioRxiv", subtitle="last authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p21
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.last <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30" & !is.na(last.author.gender)) %>% group_by(as.Date(date), last.author.gender) %>% summarize(n=n())))
biorxiv.last$day_of_week <- wday(biorxiv.last$`as.Date(date)`, label=TRUE)
biorxiv.last$date <- as.numeric(biorxiv.last$`as.Date(date)`)-18261
biorxiv.last$day_of_week <- factor(biorxiv.last$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm9 <- lm(sqrt(n)~date*last.author.gender+day_of_week, data=biorxiv.last)
summary(lm9)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * last.author.gender + day_of_week, 
    ##     data = biorxiv.last)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9963 -0.8734  0.0531  0.9117  4.3512 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 2.690751   0.285375   9.429  < 2e-16 ***
    ## date                        0.008066   0.002044   3.947 9.56e-05 ***
    ## last.author.gendermale      2.832759   0.304847   9.292  < 2e-16 ***
    ## day_of_weekMon              1.305701   0.283988   4.598 5.95e-06 ***
    ## day_of_weekTue              2.270761   0.283999   7.996 1.85e-14 ***
    ## day_of_weekWed              2.150997   0.284043   7.573 3.21e-13 ***
    ## day_of_weekThu              2.784022   0.284018   9.802  < 2e-16 ***
    ## day_of_weekFri              2.489159   0.283999   8.765  < 2e-16 ***
    ## day_of_weekSat              0.854343   0.283988   3.008  0.00281 ** 
    ## date:last.author.gendermale 0.006611   0.002889   2.288  0.02272 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 354 degrees of freedom
    ## Multiple R-squared:  0.6729, Adjusted R-squared:  0.6646 
    ## F-statistic: 80.91 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm9, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##                         Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)             186.41   1 88.9026 < 2.2e-16 ***
    ## date                     32.66   1 15.5763 9.559e-05 ***
    ## last.author.gender      181.06   1 86.3483 < 2.2e-16 ***
    ## day_of_week             316.47   6 25.1549 < 2.2e-16 ***
    ## date:last.author.gender  10.98   1  5.2355   0.02272 *  
    ## Residuals               742.28 354                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(lm9)
```

![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-2.png)<!-- -->![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-3.png)<!-- -->![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-4.png)<!-- -->![](README_files/figure-gfm/Early%202020%20bioRxiv%20last%20author%20analysis-5.png)<!-- -->

### Middle authorships

``` r
biorxiv.middle <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(round_date(as.Date(date), unit="weeks", week_start = getOption("lubridate.week.start", 1))) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
biorxiv.middle.long <- gather(biorxiv.middle, gender, n, female.n:male.n)
colnames(biorxiv.middle.long) <- c("week", "gender", "n")
biorxiv.middle.long$gender <- factor(biorxiv.middle.long$gender, rev(levels(as.factor(biorxiv.middle.long$gender))))

p22 <- ggplot(data=subset(biorxiv.middle.long, as.Date(week) >= "2020-01-01" & as.Date(week) <= "2020-06-28"), aes(x=as.Date(week),y=n, color=gender))+geom_point(size=2)+theme_cowplot()+geom_smooth(method="lm",se=FALSE)+ylab("Authorships (no.)")+xlab("Date")+scale_color_manual(name="Gender", labels=c("Men", "Women"), values=colours2)+scale_shape_discrete(name="Gender", labels=c("Men", "Women"))+geom_vline(aes(xintercept=as.Date(who)), linetype="dashed")+labs(title="bioRxiv", subtitle="middle authorships")+scale_x_date(date_labels = "%b %Y")+theme(axis.text.x=element_text(angle=60, hjust=1))
p22
```

![](README_files/figure-gfm/Early%202020%20biorxiv%20middle%20author%20analysis-1.png)<!-- -->

``` r
#Model
biorxiv.middle <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-06-30") %>% group_by(as.Date(date)) %>% summarize(female.n=sum(female.mid.authors.n, na.rm=TRUE), male.n=sum(male.mid.authors.n, na.rm=TRUE))))
biorxiv.middle.long <- gather(biorxiv.middle, gender, n, female.n:male.n)
biorxiv.middle.long$day_of_week <- wday(biorxiv.middle.long$`as.Date(date)`, label=TRUE)
biorxiv.middle.long$date <- as.numeric(biorxiv.middle.long$`as.Date(date)`)-18261
biorxiv.middle.long$day_of_week <- factor(biorxiv.middle.long$day_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), ordered = FALSE)

lm10 <- lm(sqrt(n)~date*gender+day_of_week, data=biorxiv.middle.long)
summary(lm10)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(n) ~ date * gender + day_of_week, data = biorxiv.middle.long)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.9442 -1.7159 -0.1169  1.7756  7.5880 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       6.656234   0.534307  12.458  < 2e-16 ***
    ## date              0.025121   0.003827   6.565 1.86e-10 ***
    ## gendermale.n      2.347885   0.570765   4.114 4.85e-05 ***
    ## day_of_weekMon    2.157687   0.531710   4.058 6.09e-05 ***
    ## day_of_weekTue    3.820173   0.531731   7.184 4.02e-12 ***
    ## day_of_weekWed    4.478879   0.531813   8.422 9.37e-16 ***
    ## day_of_weekThu    5.299959   0.531765   9.967  < 2e-16 ***
    ## day_of_weekFri    4.730181   0.531731   8.896  < 2e-16 ***
    ## day_of_weekSat    1.679724   0.531710   3.159  0.00172 ** 
    ## date:gendermale.n 0.003849   0.005410   0.711  0.47726    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.711 on 354 degrees of freedom
    ## Multiple R-squared:  0.493,  Adjusted R-squared:  0.4801 
    ## F-statistic: 38.25 on 9 and 354 DF,  p-value: < 2.2e-16

``` r
Anova(lm10, type=3)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: sqrt(n)
    ##              Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept) 1140.74   1 155.1943 < 2.2e-16 ***
    ## date         316.78   1  43.0974 1.856e-10 ***
    ## gender       124.38   1  16.9215 4.849e-05 ***
    ## day_of_week 1164.41   6  26.4025 < 2.2e-16 ***
    ## date:gender    3.72   1   0.5062    0.4773    
    ## Residuals   2602.05 354                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot(lm10)
```

### Omnibus figures

``` r
#Year over year
p23 <- plot_grid(p13,p14,p15,p16,p17, align = 'v', axis='l')
p23
```

![](README_files/figure-gfm/Combine%20biorxiv%20visualizations%20for%20omnibus%20figures-1.png)<!-- -->

``` r
save_plot("year-over-year_biorxiv.png", p23, base_height=8, base_width=8, dpi=600)

p24 <- plot_grid(p18, p19, p20, p21, p22, align='v', axis='l')
p24
```

![](README_files/figure-gfm/Combine%20biorxiv%20visualizations%20for%20omnibus%20figures-2.png)<!-- -->

``` r
save_plot("early2020_biorxiv.png", p24, base_height=8, base_width=14, dpi=600)
```
