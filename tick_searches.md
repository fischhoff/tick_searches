tick\_searches
================
Ilya
5/13/2018

#### Objective: Predict Lyme disease risk at temporal and spatial scales useful for decision-makers (public health agencies, healthcare providers, members of the public).

#### When we go outdoors, we run a risk of coming into contact with ticks that could give us disease-causing pathogens. In the eastern U.S., blacklegged ticks (Ixodes scapularis) transmit the bacterium Borrelia burgdorferi, which causes Lyme disease; in the West Coast, the Western blacklegged tick (Ixodes pacificus) is the vector for Lyme disease. Both of these ticks also transmit pathogens causing other diseases in people and pets. Tick activity varies depending on time of year, weather, wildlife host abundance, and other factors. Knowing the level of risk in our area at a particular time can help us decide what outdoor activities to pursue and what precautions to take (such as checking oneself for ticks after going into tick habitat, use of repellent).

#### Lyme disease risk arises from two interacting causes: first, the abundance of infected host-seeking blacklegged ticks; and second, human behavior that brings us into proximity of ticks. The abundance of infected ticks seeking hosts, also referred to as entomological risk, is influenced by the wildlife host community in a particular place, landscape characteristics (e.g., forest cover, geographic location), and weather. We are exposed to ticks through our work and recreation. Our risk further depends on the extent to which we apply tick-bite prevention methods.

#### Internet search data offer a source of real-time information on people's encounters with ticks. If we know people in our area are searching more for ticks, this could be a sign that tick activity is high and we need to be more vigilant against ticks. Volume of searches related to ecological factors that affect risk (e.g. "chipmunk") or activities that expose people to ticks (e.g., "hiking) or that reduce risk (e.g. "repellent), may also be useful in predicting risk. To evaluate whether internet search data provides a useful measure of disease risk, here we use Google trends data from 2004 to 2016 to predict Lyme disease incidence in those years.

##### install packages

    ## 
    ## Attaching package: 'geojsonio'

    ## The following object is masked from 'package:base':
    ## 
    ##     pretty

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following object is masked from 'package:raster':
    ## 
    ##     shift

    ## 
    ## Attaching package: 'devtools'

    ## The following object is masked from 'package:geojsonio':
    ## 
    ##     lint

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:raster':
    ## 
    ##     intersect, select, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: survival

    ## Loading required package: lattice

    ## Loading required package: splines

    ## Loading required package: parallel

    ## Loaded gbm 2.1.3

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:survival':
    ## 
    ##     cluster

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     ggsave

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:raster':
    ## 
    ##     extract

    ## Loading required package: rgenoud

    ## ##  rgenoud (Version 5.8-2.0, Build Date: 2018-04-03)
    ## ##  See http://sekhon.berkeley.edu/rgenoud for additional documentation.
    ## ##  Please cite software as:
    ## ##   Walter Mebane, Jr. and Jasjeet S. Sekhon. 2011.
    ## ##   ``Genetic Optimization Using Derivatives: The rgenoud package for R.''
    ## ##   Journal of Statistical Software, 42(11): 1-26. 
    ## ##

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following objects are masked from 'package:raster':
    ## 
    ##     area, select

    ## 
    ## ##  anchors (Version 3.0-8, Build Date: 2014-02-24)
    ## ##  See http://wand.stanford.edu/anchors for additional documentation and support.

    ## Downloading GitHub repo PMassicotte/gtrendsR@master
    ## from URL https://api.github.com/repos/PMassicotte/gtrendsR/zipball/master

    ## Installing gtrendsR

    ## '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
    ##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
    ##   '/private/var/folders/0d/qm_pqljx11s_ddc42g1_yscr0000gn/T/Rtmp7nspDD/devtools74d41150494/PMassicotte-gtrendsR-a7ceb95'  \
    ##   --library='/Library/Frameworks/R.framework/Versions/3.4/Resources/library'  \
    ##   --install-tests

    ## 

    ## phantomjs has been installed to /Users/fischhoff/Library/Application Support/PhantomJS

###### read in geojson file with nielsen dmas, plot map of dma with color based on percentage with cable (to check these data make sense)

``` r
#https://rstudio.github.io/leaflet/json.html
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#nielsen DMA
dma <- geojsonio::geojson_read("https://rawgit.com/simzou/nielsen-dma/master/nielsentopo.json",
  what = "sp")

#make map of dmas to make sure they look okay
pal <- colorNumeric("viridis", NULL)

#commenting this out because it does not display well in github_document
dmaMap <- leaflet(dma) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(log10(cableperc)),
    label = ~paste0(name, ": ", formatC(cableperc, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(cableperc), opacity = 1.0,
    labFormat = labelFormat(transform = function(x) round(10^x)))
dmaMap
```

![](tick_searches_files/figure-markdown_github/dma-1.png)

``` r
#sort dma by dma1
dma.df = as.data.frame(dma)
dma.df$dma1 = as.character(dma.df$dma1)
dma.df$state = substrRight(dma.df$dma1, 2)

dma.df = dma.df[order(dma.df$state, dma.df$dma1),]
save(dma.df, file ="dma.df.Rdata")
write.csv(dma.df, file = "dma.csv")
```

test case getting google trends data
====================================

``` r
devtools::install_github("PMassicotte/gtrendsR", branch = "low-search-volume") #use version for getting low search volume regions
```

    ## Skipping install of 'gtrendsR' from a github remote, the SHA1 (a7ceb958) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(gtrendsR) 

time = "2004-01-01 2004-12-31"
gt <- gtrends(keyword = "tick bite", geo = c("US"), time = time, category = 0, hl = "en-US", low_search_volume = TRUE)

gt_dma <- gt$interest_by_dma
unique(gt_dma$hits)
```

    ##  [1] 100  85  54  44  41  40  37  36  34  33  23  21  20  18  17  16  15
    ## [18]  14  12  11  10   9   8   7   6   5   4   3   2   1  NA

``` r
gt_no_low <- gtrends(keyword = "tick bite", geo = c("US"), time = time, category = 0, hl = "en-US", low_search_volume = FALSE)
gt_no_low_dma<- gt_no_low$interest_by_dma
unique(gt_no_low_dma$hits)
```

    ##  [1] 100  85  54  44  41  40  37  36  34  33  23  21  20  18  17  16  15
    ## [18]  14  12  11  10   9   8   7   6   5   4   3   2   1  NA

get google trends data -- annual
================================

##### We used the term "tick bite", and the top 10 related queries for that term. We excluded queries related to tick species other than the blacklegged tick (Ixodes scapularis), and related to tick encounters with pets. We also excluded queries such as "what does tick bite look like", that contain within it other search terms already included (in this case, "tick bite"). We also included several search terms related to ecological factors (e.g. "deer") and behavioral factors (e.g. "repellent") that may mediate risk. Google Trends values are from 0 to 100. Values reflect the popularity of a search term, relative to the popularity of other search terms within a geographical area.

``` r
#make function for substring
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

years = seq(from = 2004, to = 2016, by = 1)
#sequence of years lagged by one year, to be used for terms like chipmunk that should be lagged that way
year.lag.1 = seq(from = 2003, to = 2015, by =1)
#sequence of years lagged by two years
year.lag.2 = seq(from = 2002, to = 2014, by =1)

#make data.frame of start and end day of each year
start.month.day = rep("01-01",length(years))
end.month.day = rep("12-31", length(years))
start.days = paste(as.character(years), start.month.day, sep="-")
end.days = paste(as.character(years), end.month.day, sep="-")
lag0=rep(0, length(end.days))
days_df = data.frame(year = years,
                     start.days = start.days,
                     end.days = end.days,
                     lag = lag0)

start.days.lag1 = paste(as.character(year.lag.1), start.month.day, sep="-")
end.days.lag1 = paste(as.character(year.lag.1), end.month.day, sep="-")
lag1=rep(1, length(end.days.lag1))
days_tmp = data.frame(year = year.lag.1,
                      start.days = start.days.lag1,
                      end.days = end.days.lag1,
                      lag = lag1)
days_df = rbind(days_df, days_tmp)

start.days.lag2 = paste(as.character(year.lag.2), start.month.day, sep="-")
end.days.lag2 = paste(as.character(year.lag.2), end.month.day, sep="-")
lag2=rep(2, length(end.days.lag2))
days_tmp = data.frame(year = year.lag.2,
                      start.days = start.days.lag2,
                      end.days = end.days.lag2,
                      lag = lag2)
days_df = rbind(days_df, days_tmp)

terms = c("garden", "mowing", "tick", "hiking", "hunting", "deet", "repellent", "deer tick", "tick bites", "ticks", "chipmunk", "mouse trap",
           "deer", "acorns", "mouse")
terms_current_df_field = c("garden", "mowing","tick", "hiking", "hunting", "deet","repellent","deer.tick", "tick bites", "ticks", "chipmunk", "mouse.trap", "deer", "acorns", "mouse")
lag_seq = c(rep(0,10) ,1,1,2,2,1)

#test set, commenting out for now
# terms = c("deet")
# terms_current_df_field = c("deet")
# lag_seq = c(0)

out = NULL
a = 1
b =1

#confirm that values of NA are likely 0
#     tmp <- gtrends(keyword = "deet", geo = c("US"), time = paste(year_subset$start.days, year_subset$end.days, sep = " "), category = 0, hl = "en-US",
#     low_search_volume = TRUE)
# tmp$interest_by_dma$hits
# tmp$interest_by_dma$location
#get test search results
a = 1
year_subset= subset(days_df, lag == 0 & year == years[a])

for (a in 1:length(years)){
#for (a in c(1:2)){#test set
  print(a)
    #initialize a data.frame with one search term
    year_subset= subset(days_df, lag == 0 & year == years[a])
    gt <- gtrends(keyword = "tick bite", geo = c("US"), time = paste(year_subset$start.days, year_subset$end.days, sep = " "), category = 0, hl = "en-US",
    low_search_volume = TRUE)
    #get last two characters for the state
    gt$interest_by_dma$state = substrRight(gt$interest_by_dma$location, 2)
    gt$interest_by_dma$year = years[a]
    gt = gt$interest_by_dma
    gt = gt[order(gt$location),]

    gt$tick.bite = gt$hits
    gt$tick.bite[gt$tick.bite == "<1"]=  1#replace any "<1"
    gt$tick.bite[is.na(gt$tick.bite)]=  0#replace any NA with 0
    gt$tick.bite = as.numeric(as.character(gt$tick.bite))
    gt = gt[,c("location","tick.bite", "state", "year")]

  for (b in 1:length(terms)){#begin for loop through terms
    lag_tmp = lag_seq[b]
    #year
    year_subset = subset(days_df, lag == lag_tmp & year == (years[a]-lag_tmp))#find the year in question for this term and relevant time lag
    #only proceed if 2004 or later, else no google trends data
    if (year_subset$year >=2004){
      #now can proceed
            #deer tick
        gt.tmp <- gtrends(keyword = terms[b], geo = c("US"), time = paste(year_subset$start.days, year_subset$end.days, sep = " "), category = 0, hl = "en-US",
                           low_search_volume = TRUE)
        gt.tmp$interest_by_dma$state = substrRight(gt.tmp$interest_by_dma$location, 2)
        gt.tmp$interest_by_dma$year = years[a]
        gt.tmp = gt.tmp$interest_by_dma
        gt.tmp = gt.tmp[order(gt.tmp$location),]
        gt.tmp$hits[gt.tmp$hits=="<1"]= 1#replace any "<1"
        gt.tmp$hits[is.na(gt.tmp$hits)]=  0#replace any NA with 0
        gt.tmp$hits = as.numeric(as.character(gt.tmp$hits))
        gt.tmp[,c(terms_current_df_field[b])]= gt.tmp$hits
        gt.tmp = gt.tmp[,c("location",terms_current_df_field[b], "state", "year")]
        #now add to gt
        #gt.tmp$location == gt$location
        gt[,c(terms_current_df_field[b])] = gt.tmp[,c(terms_current_df_field[b])]
        #rm(gt.tmp)
    } 
    else if (year_subset$year <2004){
      print("else")
      gt[,c(terms_current_df_field[b])]=NA
    }
  }#end for loop through terms
  out = rbind(out, gt)
}#end for loop through years
```

    ## [1] 1
    ## [1] "else"
    ## [1] "else"
    ## [1] "else"
    ## [1] "else"
    ## [1] "else"
    ## [1] 2
    ## [1] "else"
    ## [1] "else"
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13

``` r
gt = out
save(gt, file = "gt.Rdata")

#output one year/keyword subset for assigning google dma names to dma names in json file (they are slightly different)
dma.tab = subset(gt, year ==2004) 
dma.tab = dma.tab[order(dma.tab$state),]
write.csv(dma.tab, file = "dma.tab.google.csv")
```

##### assign google dma names to dma-county file (do this part in excel, pseudo code here)

``` r
#next: 
#open dma.csv and  dma.tab.google.csv in excel, 
#copy in google trends dma names into dma.csv, 
#align the two dma name sets, fixing state abbreviations, 
#and save dma.csv as dma.google.assigned.by.hand2.csv
```

##### read dma json file back in after assigning google trends dma names to it and fixing state abbreviations

``` r
load("dma.df.Rdata")
dma.assigned = read.csv( "dma.google.assigned.by.hand2.csv")
dma.assigned = subset(dma.assigned, !is.na(id))
names(dma.assigned)[names(dma.assigned)=="location"]="google.dma"
dma.assigned = dma.assigned[order(dma.assigned$id),]
dma.df = dma.df[order(dma.df$id),]
#check these are all true as error check nothing odd happened
dma.assigned$state = as.character(dma.assigned$state)

inds = which(dma.df$dma1 !=dma.assigned$dma1)
inds#should be empty
```

    ## integer(0)

``` r
dma.ggl <- dma.assigned
keep.col = c("latitude","dma","dma1", "longitude", "state", "google.dma")
dma.ggl = dma.ggl[,keep.col]

save(dma.ggl, file = "dma.ggl.Rdata")
```

##### now assign google dmas to counties

``` r
require(sp)
load("dma.ggl.Rdata")

#https://catalog.data.gov/dataset/us-counties/resource/cc1b2e44-d5a4-4c26-82c1-39b0da37bfb8
counties.test <- shapefile("tl_2016_us_county.shp")
counties = counties.test
#confirm there is a projetion in counties
projection(counties)#there is none, but we're not going to use it
```

    ## [1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

``` r
#pal <- colorNumeric("viridis", NULL)

#make map of counties to make sure they look like counties -- commented out to make markdown smaller
# counties_map <-  leaflet(counties) %>%
#   addTiles() %>%
#   addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#     fillColor = ~pal((as.numeric(STATEFP))),
#     label = ~paste0(NAME, ": ", formatC(as.numeric(STATEFP), big.mark = ","))) %>%
#   addLegend(pal = pal, values = ~(as.numeric(STATEFP)), opacity = 1.0,
#     labFormat = labelFormat(transform = function(x) round(10^x)))
# counties_map

#read dma in again as polygons (need it to be polygons, not data.frame)
dma <- geojsonio::geojson_read("https://rawgit.com/simzou/nielsen-dma/master/nielsentopo.json",
  what = "sp")

#tried assigning a projection to dma, but it seemed to result in holes in polygons
# dma_project <- projection(dma, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#then assign google dma name
#make version of dma.ggl w/ only relevant columns
dma.ggl.sm = dma.ggl[,c("dma1","google.dma")]
#merge dma with dma.ggl.sm, assigning google dma name
#source: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html#spatial-non-spatial
#doing this with merge messes it up w/ respect to holes in polygons, so instead use for loop
# dma = merge(dma,dma.ggl.sm, by.x = "dma1", by.y = "dma1")
dma$dma.google = NA
udma = unique(dma$dma1)
a = 1
for (a in 1:length(udma)){
  ind = which(dma$dma1==udma[a])
  ind.google = which(as.character(dma.ggl.sm$dma1)==udma[a])
  dma$dma.google[ind]= as.character(dma.ggl.sm$google.dma[ind.google])
}

#counties has a projection; dma does not; assigning no projection to counties
projection(counties) <- projection(dma)#package raster

#this does not work if identical CRS(x,y) is not true
counties$dma.ggl <- over( counties, dma)$dma.google#name of dma

#read in fps codes, in order to assign abbreviated and long state name to counties
fps = fread("https://www2.census.gov/geo/docs/reference/state.txt")
fps = fps[,c("STATE", "STUSAB", "STATE_NAME")]
names(fps)[names(fps)=="STATE"]="STATEFP"

setdiff(names(fps), names(counties))
```

    ## [1] "STUSAB"     "STATE_NAME"

``` r
counties$STATEFP = as.numeric(as.character(counties$STATEFP))
dim(counties)
```

    ## [1] 3233   18

``` r
counties = merge(counties,fps, by.x = "STATEFP", by.y = "STATEFP")

dim(counties)#confirm no counties are lost
```

    ## [1] 3233   20

``` r
save(counties, file = "counties.Rdata")

#used this below to check for holes, and found them in dma when it had a projection assigned (which resulted in function over failing)
# require(devtools)
# install_github("eblondel/cleangeo")
# require(cleangeo)
# report <- clgeo_CollectionReport(counties)
# summary <- clgeo_SummaryReport(report)
# issues <- report[report$valid == FALSE,]
# 
# report <- clgeo_CollectionReport(dma)
# summary <- clgeo_SummaryReport(report)
# issues <- report[report$valid == FALSE,]
```

##### read in CDC Lyme data and reshape

``` r
#website: https://www.cdc.gov/lyme/stats/index.html
L = read.csv("https://www.cdc.gov/lyme/resources/ld-Case-Counts-by-County-00-16.csv")
L = reshape(L, direction = "long", varying = 5:21, sep = "")
dim(L)
```

    ## [1] 54281     7

``` r
save(L, file = "L.Rdata")
```

##### read in census data from SEER.

``` r
library(data.table)
#source: https://seer.cancer.gov/popdata/download.html
#dictionary for data: https://seer.cancer.gov/popdata/popdic.html
A = read.table("us.1990_2016.19ages.adjusted.txt", skip = 1)
A=as.data.frame(A)
A$V1 = as.character(A$V1)
A$year = substr(A$V1, 1, 4)
A$state = substr(A$V1, 5, 6)
A$county_fps = substr(A$V1, 9, 11)
A$population = substr(A$V1, 19, 26)
save(A, file = "A.Rdata")
# 
```

fix population in census data
=============================

``` r
library(data.table)
load("A.Rdata") 
A$population.2 =  as.numeric(substr(A$population,regexpr("[^0]",A$population),nchar(A$population)))
 A$population = A$population.2
 #make field with state followed by county_fps -- make as numeric to get rid of leading 0s so it matches up with fps next
A$state_countyfps = paste(A$state, as.numeric(as.character(A$county_fps)))
 length(unique(A$state_countyfps))
```

    ## [1] 3155

``` r
 keep.col = c("V1", "year", "state", "county_fps", "population", "state_countyfps")
A = A[, keep.col]

# #county fps
 fps = fread("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt")
 #assign meaningful names
 names(fps)= c("state", "statefp", "county_fps", "county", "classfp")
 fps = data.frame(fps)
 keep.col = c("state", "county", "county_fps", "statefp")
 fps = fps[, keep.col]
 fps$state_countyfps = paste(fps$state, fps$county_fps)
 #commenting this out because there each county_fps may be used in multiple states -- not unique nationally 
 #A$county_fps=as.numeric(as.character(A$county_fps))

 test = setdiff(A$state_countyfps, fps$state_countyfps )
 A.test = subset(A, state_countyfps %in% test)
 unique(A.test$state)#includes AK, AZ, CO, HI, VA, KR
```

    ## [1] "AK" "AZ" "CO" "HI" "VA" "KR"

``` r
 #A.test$county_fps = as.numeric(as.character(A.test$county_fps))
 #to merge need to add county field
 #A.test$county = NA#14 counties in A don't have match in fps 
 #A.test$statefp = NA
 
 A$county_fps=as.numeric(as.character(A$county_fps))#removing trailing ones means can merge by this field (together with state_countyfps and state)
 #A.test2 = merge(A.test, fps, by = intersect(names(A.test), names(fps)))
  A2 = merge(A, fps)#need to merge by this field or else some state_countyfps are lost
  length(unique(A2$state_countyfps))
```

    ## [1] 3141

``` r
#
 #A3 = rbind(A2, A.test)
# A = A3
  A3 = A2
 A3$year = as.numeric(A3$year)
 A3$population =  as.numeric(substr(A3$population,regexpr("[^0]",A3$population),nchar(A3$population)))
# A2 = A
 save(A3, file = "A3.Rdata")
```

##### read in Census data and summarize by year

``` r
load("A3.Rdata")
A = A3
keep = c("state", "population", "county", "year", "county_fps", "state_countyfps", "statefp")
A1 = A[,keep]

#now use county_state as group_by variable (as alternative to state_countyfps that is easier to interpret)
A1$county_state = paste(A1$county, A1$state, sep = "_")
A2 <- A1 %>%
  group_by(county_state, year) %>%
  summarize(population = sum(population),
            county = county[1], 
            state = state[1], 
            state_countyfps = state_countyfps[1],
            county_fps=county_fps[1],
            statefp = statefp[1])
length(unique(A2$state_countyfps))
```

    ## [1] 3141

``` r
census = A2
save(census, file = "census.Rdata")
```

##### read back in Census data, reshape long, assign full names of states

``` r
load("census.Rdata")
#names(census)[names(census)=="state"]="state_name"
names(census)[names(census)=="county"]="county_name"
census = subset(census, year >=2004)

#read in file of state abbreviations
states = read.csv("50_us_states_all_data.csv")
states = states[,c(2:3)]
colnames(states) =c("state_name", "state")

intersect(names(census), names(states))
```

    ## [1] "state"

``` r
#add state abbreviation to census data
census1 = merge(census, states, by = "state")
#loses a few rows
#check a few rows
head(census1)
```

    ##   state                 county_state year population
    ## 1    AK       North Slope Borough_AK 2016       9606
    ## 2    AK       Bristol Bay Borough_AK 2016        898
    ## 3    AK Matanuska-Susitna Borough_AK 2016     104365
    ## 4    AK    Anchorage Municipality_AK 2013     301143
    ## 5    AK    Anchorage Municipality_AK 2011     296397
    ## 6    AK    Anchorage Municipality_AK 2010     293415
    ##                 county_name state_countyfps county_fps statefp state_name
    ## 1       North Slope Borough          AK 185        185       2     Alaska
    ## 2       Bristol Bay Borough           AK 60         60       2     Alaska
    ## 3 Matanuska-Susitna Borough          AK 170        170       2     Alaska
    ## 4    Anchorage Municipality           AK 20         20       2     Alaska
    ## 5    Anchorage Municipality           AK 20         20       2     Alaska
    ## 6    Anchorage Municipality           AK 20         20       2     Alaska

``` r
tail(census1)
```

    ##       state       county_state year population     county_name
    ## 39894    WY Washakie County_WY 2005       8022 Washakie County
    ## 39895    WY  Natrona County_WY 2005      69922  Natrona County
    ## 39896    WY  Natrona County_WY 2006      70806  Natrona County
    ## 39897    WY   Weston County_WY 2015       7230   Weston County
    ## 39898    WY   Carbon County_WY 2004      15236   Carbon County
    ## 39899    WY  Natrona County_WY 2014      81432  Natrona County
    ##       state_countyfps county_fps statefp state_name
    ## 39894           WY 43         43      56    Wyoming
    ## 39895           WY 25         25      56    Wyoming
    ## 39896           WY 25         25      56    Wyoming
    ## 39897           WY 45         45      56    Wyoming
    ## 39898            WY 7          7      56    Wyoming
    ## 39899           WY 25         25      56    Wyoming

``` r
length(unique(census$year))
```

    ## [1] 13

``` r
save(census1, file = "census1.Rdata")
```

##### combine census and LD data

``` r
load("census1.Rdata")
census = census1
load("L.Rdata")
#make county_name and county_state
names(L)[names(L)=="Ctyname"]="county_name"
names(L)[names(L)=="Stname"]="state_name"
names(L)[names(L)=="time"]="year"
L$county_state =paste(L$county_name, L$state_name, sep = " ")
L = L[, c("county_name", "county_state", "state_name", "year", "Cases")]

test  = subset(census, is.na(population))#should be empty
#census = census[, c("state_name", "county_name", "year", "population", "state", "county_state")]
census$county_state=paste(census$county_name, census$state_name, sep = " ")

length(setdiff(L$county_state,census$county_state))
```

    ## [1] 125

``` r
length(setdiff(census$county_state,L$county_state))
```

    ## [1] 5

``` r
#seems to be missing Alabama, but that's okay as it's way South so doesn't have much Lyme 
L1 = merge(census, L, by = intersect(names(census), names(L)))
L = L1
L$incidence = 100000*L$Cases/L$population
length(unique(L$year))
```

    ## [1] 13

``` r
length(unique(L$state_countyfps))
```

    ## [1] 3068

``` r
Lcensus = L
save(Lcensus, file = "Lcensus.Rdata") 
```

##### assign DMA and google trends to Lyme disease data

``` r
require(sp)
load("Lcensus.Rdata")#Lyme
L = Lcensus
L$county_state=tolower(L$county_state)
load("gt.Rdata")
load("counties.Rdata")
names(counties)[names(counties)=="NAMELSAD"]="county_name"
names(counties)[names(counties)=="STUSAB"]="state"
counties$county_state =tolower(paste(counties$county_name, counties$STATE_NAME, sep = " "))
keep.col = c("dma.ggl", "county_state", "STATEFP"      ,"COUNTYFP", "county_name", "INTPTLAT", "INTPTLON","state"     ,  "STATE_NAME"    )
counties = counties[,keep.col]
counties.df = data.frame(counties)
names(counties.df)=tolower(names(counties.df))
counties.df = counties.df[,c("dma.ggl", "county_state")]
#lose some AL counties here
dim(L)
```

    ## [1] 39844    11

``` r
#make version that is numeric
L$state_county_fp = paste(L$statefp, L$county_fps)
L =L[,c("county_state", "year", "Cases", "county_name", "state_name", "state", "population", "incidence", "state_county_fp")]
Lc = merge(L,counties.df,by = intersect(names(L), names(counties.df)))
dim(Lc)#none are lost
```

    ## [1] 39844    10

``` r
#assign google trends to L
names(gt)[names(gt)=="location"]="dma.ggl"

# gt = gt[,c("dma.ggl", "deet", "year")]

gt = gt[,c("dma.ggl", "tick.bite"      , "year"      , "garden"    , "mowing"    , "tick",       "hiking", "hunting"  ,"deet"       ,"repellent",  "deer.tick" , "tick bites", "ticks"   ,   "chipmunk",   "mouse.trap", "deer","acorns", "mouse")]
L1 = merge(Lc, gt, by = c("dma.ggl", "year"))

#L1 = subset(L1, !is.na(incidence))
save(L1, file = "L1.Rdata")
```

##### summarize by county across time for plotting - not for use in data analysis

``` r
load("L1.Rdata")
L1$tick.bites = L1[,20]
names(L1)
```

    ##  [1] "dma.ggl"         "year"            "county_state"   
    ##  [4] "Cases"           "county_name"     "state_name"     
    ##  [7] "state"           "population"      "incidence"      
    ## [10] "state_county_fp" "tick.bite"       "garden"         
    ## [13] "mowing"          "tick"            "hiking"         
    ## [16] "hunting"         "deet"            "repellent"      
    ## [19] "deer.tick"       "tick bites"      "ticks"          
    ## [22] "chipmunk"        "mouse.trap"      "deer"           
    ## [25] "acorns"          "mouse"           "tick.bites"

``` r
# terms_current_df_field = c("garden", "mowing","tick", "hiking", "hunting", "deet","repellent","deer.tick", "tick bites", "ticks", "chipmunk", "mouse.trap", "deer", "acorns")

L1$tick.bite=as.numeric(as.character(L1$tick.bite))
library(dplyr)
Lc<-L1 %>%
  group_by(county_state) %>%
  summarize(population = mean(population[!is.na(population)]),
            Cases = sum(Cases),
            incidence = mean(incidence[!is.na(incidence)]),
            garden = mean(garden[!is.na(garden)]),
            mowing = mean(mowing[!is.na(mowing)]),
            hunting = mean(hunting[!is.na(hunting)]),
            tick = mean(tick[!is.na(tick)]),
            deer.tick = mean(deer.tick[!is.na(deer.tick)]),
            tick.bite = mean(tick.bite[!is.na(tick.bite)]),
            tick.bites=mean(tick.bites[!is.na(tick.bites)]),
            ticks = mean(ticks[!is.na(ticks)]), 
            chipmunk = mean(chipmunk[!is.na(chipmunk)]),
            mouse.trap = mean(mouse.trap[!is.na(mouse.trap)]),
            mouse = mean(mouse[!is.na(mouse)]),
            deer = mean(deer[!is.na(deer)]),
            acorns = mean(acorns[!is.na(acorns)]),
            repellent = mean(repellent[!is.na(repellent)]),
            hiking = mean(hiking[!is.na(hiking)]),
            deet = mean(deet[!is.na(deet)]),
            state = state[1],
            county_name = county_name[1],
            state_county_fp = state_county_fp[1],
            #county_fps = county_fps[1],
            #statefp = statefp[1],
            dma.ggl = dma.ggl[1]
            )
#summary(L1$incidence.dma)
#L <- L1
save(Lc, file = "Lc.Rdata")


tmp = subset(Lc, county_name %in% "Dutchess County")
```

##### read in shapefile of counties and make map of incidence by county

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("viridis", NULL)
#commenting this out because it does not display well in github_document
M<- leaflet(outC) %>%
  addTiles() %>%
  setView(lat = 39.5, lng=-98.5, zoom =4) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$incidence),
    #label with county name 
    label = ~paste0(county_name, ": ", formatC(outC$incidence, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$incidence, opacity = 1.0,
     labFormat = labelFormat(transform = function(x) round(10^x)))

mapshot(M, file = "incidence.png")
M
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-5-1.png)

##### read in shapefile of counties and make map of incidence\_rescaled (from 0 to 1) by county -- to see if rescaling data makes patterns more apparent

``` r
load("Lc.Rdata")
L = Lc
#retitle column to make it more obvious how to link to shapefile
#names(L)[names(L)=="state_countyfps"]="state_county_fp"
#L$state_county_fp = paste(L$statefp, L$county_fps)
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(incidence))
outC$incidence_rescaled =rescale_mid(outC$incidence, to = c(0,1), mid=median(outC$incidence))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("viridis", NULL)
#commenting this out because it does not display well in github_document
inc_resc_map<- leaflet(outC) %>%
  addTiles() %>%
    setView(lat = 39.5, lng=-98.5, zoom =4) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$incidence_rescaled),
    #label with county name 
    label = ~paste0(county_name, ": ", formatC(outC$incidence_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$incidence_rescaled, opacity = 1.0,
     labFormat = labelFormat(transform = function(x) round(10^x)))
inc_resc_map
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#mapshot(N, file = "incidence_rescaled.png")
```

##### read in shapefile of counties and make map of tick.bite by county, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$tick.bite_rescaled =rescale_mid(outC$tick.bite, to = c(0,1), mid=median(outC$tick.bite))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$tick.bite_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$tick.bite_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$tick.bite_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-7-1.png)

##### read in shapefile of counties and make map of hunting\_rescaled by dma, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$hunting_rescaled =rescale_mid(outC$hunting, to = c(0,1), mid=median(outC$hunting))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$hunting_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$hunting_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$hunting_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/hunting-1.png)

##### read in shapefile of counties and make map of hiking\_rescaled by dma, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$hiking_rescaled =rescale_mid(outC$hiking, to = c(0,1), mid=median(outC$hiking))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$hiking_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$hiking_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$hiking_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/hiking-1.png)

##### read in shapefile of counties and make map of deer\_rescaled by dma, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$deer_rescaled =rescale_mid(outC$deer, to = c(0,1), mid=median(outC$deer))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$deer_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$deer_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$deer_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/deer-1.png)

##### read in shapefile of counties and make map of mouse.trap\_rescaled by dma, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$mouse.trap_rescaled =rescale_mid(outC$mouse.trap, to = c(0,1), mid=median(outC$mouse.trap, na.rm=TRUE))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$mouse.trap_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$mouse.trap_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$mouse.trap_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/mouse_trap-1.png)

##### read in shapefile of counties and make map of deet\_rescaled by dma, together with outlines of counties by dma

``` r
load("Lc.Rdata")
L = Lc
#L$state_county_fp = paste(L$statefp, L$county_fps)
#L$COUNTYFP = as.numeric(as.character(L$countyfp))
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
head(C$STATEFP)
```

    ## [1] "01" "01" "01" "01" "01" "01"

``` r
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
outC <- append_data(C, L, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 197 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
outC = subset(outC, !is.na(tick.bite))
outC = subset(outC, tick.bite!="NaN")

outC$deet_rescaled =rescale_mid(outC$deet, to = c(0,1), mid=median(outC$deet, na.rm=TRUE))
test = under_coverage()
check = subset(L, state_county_fp %in% test$value)
check2 = subset(C, state_county_fp %in% test$value)
unique(check$county_state)
```

    ## character(0)

``` r
#make map of dmas to make sure they look okay
pal <- colorNumeric("Blues", NULL)
pal_dma <- colorNumeric("YlOrRd", NULL)
#commenting this out because it does not display well in github_document
leaflet(outC) %>%
  addTiles() %>%
      setView(lat = 39.5, lng=-98.5, zoom =4) %>%

    #add polygons for dma.ggl 
  addPolygons(stroke = TRUE, smoothFactor=0.3, opacity = 1, weight = 1,
              fill=FALSE,color = ~pal_dma(as.numeric(as.factor(outC$dma.ggl))))%>%

  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    fillColor = ~pal(outC$deet_rescaled),
    #label with county name
    label = ~paste0(county_name, ": ", formatC(outC$deet_rescaled, big.mark = ","))) %>%
   addLegend(pal = pal, values = outC$deet_rescaled, opacity = 0.5,
     labFormat = labelFormat(transform = function(x) round(10^x)))
```

![](tick_searches_files/figure-markdown_github/deet-1.png)

read in L1 data, do linear regression
=====================================

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
library(ggplot2)
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
require(MuMIn)
```

    ## Loading required package: MuMIn

``` r
load("L1.Rdata")
summary(L1)
```

    ##    dma.ggl               year      county_state           Cases         
    ##  Length:39468       Min.   :2004   Length:39468       Min.   :   0.000  
    ##  Class :character   1st Qu.:2007   Class :character   1st Qu.:   0.000  
    ##  Mode  :character   Median :2010   Mode  :character   Median :   0.000  
    ##                     Mean   :2010                      Mean   :   9.784  
    ##                     3rd Qu.:2013                      3rd Qu.:   1.000  
    ##                     Max.   :2016                      Max.   :1398.000  
    ##                                                                         
    ##  county_name           state_name       state             population      
    ##  Length:39468       Texas   : 3302   Length:39468       Min.   :      55  
    ##  Class :character   Georgia : 2067   Class :character   1st Qu.:   11052  
    ##  Mode  :character   Virginia: 1716   Mode  :character   Median :   25648  
    ##                     Kentucky: 1560                      Mean   :   99115  
    ##                     Missouri: 1495                      3rd Qu.:   65986  
    ##                     Kansas  : 1365                      Max.   :10137915  
    ##                     (Other) :27963                                        
    ##    incidence       state_county_fp      tick.bite          garden      
    ##  Min.   :  0.000   Length:39468       Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:  0.000   Class :character   1st Qu.:  1.00   1st Qu.: 31.00  
    ##  Median :  0.000   Mode  :character   Median : 12.00   Median : 36.00  
    ##  Mean   :  8.870                      Mean   : 15.96   Mean   : 37.72  
    ##  3rd Qu.:  0.831                      3rd Qu.: 24.00   3rd Qu.: 43.00  
    ##  Max.   :923.276                      Max.   :100.00   Max.   :100.00  
    ##                                                                        
    ##      mowing           tick            hiking          hunting      
    ##  Min.   :  0.0   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 10.0   1st Qu.: 20.00   1st Qu.: 16.00   1st Qu.: 16.00  
    ##  Median : 21.0   Median : 30.00   Median : 22.00   Median : 26.00  
    ##  Mean   : 24.1   Mean   : 30.69   Mean   : 25.63   Mean   : 29.89  
    ##  3rd Qu.: 35.0   3rd Qu.: 40.00   3rd Qu.: 30.00   3rd Qu.: 39.00  
    ##  Max.   :100.0   Max.   :100.00   Max.   :100.00   Max.   :100.00  
    ##                                                                    
    ##       deet          repellent       deer.tick        tick bites    
    ##  Min.   :  0.00   Min.   :  0.0   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:  0.00   1st Qu.: 11.0   1st Qu.:  0.00   1st Qu.:  0.00  
    ##  Median : 16.00   Median : 25.0   Median :  8.00   Median :  8.00  
    ##  Mean   : 17.81   Mean   : 27.2   Mean   : 12.67   Mean   : 14.75  
    ##  3rd Qu.: 27.00   3rd Qu.: 42.0   3rd Qu.: 17.00   3rd Qu.: 23.00  
    ##  Max.   :100.00   Max.   :100.0   Max.   :100.00   Max.   :100.00  
    ##                                   NA's   :1473                     
    ##      ticks           chipmunk        mouse.trap          deer       
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 15.00   1st Qu.: 12.00   1st Qu.:  0.00   1st Qu.: 19.00  
    ##  Median : 23.00   Median : 23.00   Median : 24.00   Median : 28.00  
    ##  Mean   : 23.97   Mean   : 22.45   Mean   : 26.06   Mean   : 31.11  
    ##  3rd Qu.: 32.00   3rd Qu.: 30.00   3rd Qu.: 43.00   3rd Qu.: 40.00  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :100.00  
    ##                   NA's   :3036     NA's   :3036     NA's   :6072    
    ##      acorns           mouse       
    ##  Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:  2.00   1st Qu.: 36.00  
    ##  Median : 12.00   Median : 55.00  
    ##  Mean   : 16.22   Mean   : 50.77  
    ##  3rd Qu.: 24.00   3rd Qu.: 63.00  
    ##  Max.   :100.00   Max.   :100.00  
    ##  NA's   :6072     NA's   :3036

``` r
L1$tick.bites = L1[,20]
m1 = lmer(incidence ~ tick.bite+tick+ticks +tick.bites+(1|dma.ggl), data = L1)
summary(m1)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: incidence ~ tick.bite + tick + ticks + tick.bites + (1 | dma.ggl)
    ##    Data: L1
    ## 
    ## REML criterion at convergence: 373573.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2514 -0.1055 -0.0226  0.0580 30.2024 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  dma.ggl  (Intercept) 500.6    22.37   
    ##  Residual             739.2    27.19   
    ## Number of obs: 39468, groups:  dma.ggl, 183
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept) 3.492e+00  1.707e+00 1.972e+02   2.046   0.0421 *  
    ## tick.bite   1.359e-01  1.253e-02 3.942e+04  10.843  < 2e-16 ***
    ## tick        4.285e-02  1.087e-02 3.936e+04   3.943 8.06e-05 ***
    ## ticks       8.122e-02  1.185e-02 3.941e+04   6.851 7.44e-12 ***
    ## tick.bites  6.864e-02  1.173e-02 3.932e+04   5.852 4.90e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr) tick.bt tick   ticks 
    ## tick.bite   0.030                      
    ## tick       -0.131 -0.401               
    ## ticks      -0.114 -0.277  -0.080       
    ## tick.bites -0.024 -0.174  -0.103 -0.131

``` r
m2 = lmer(incidence ~ tick.bite+tick+ticks +tick.bites+garden+mowing+hiking+hunting+deer+repellent+deer.tick+chipmunk+mouse.trap+deer+ acorns+ mouse+ (1|dma.ggl), data = L1)

r.squaredGLMM(m1)#conditional is fixed plus random effects
```

    ##        R2m        R2c 
    ## 0.01467259 0.41251991

``` r
r.squaredGLMM(m2)
```

    ##        R2m        R2c 
    ## 0.01792293 0.41760589

``` r
model.sel(m1, m2)
```

    ## Warning in model.sel.default(m1, m2): models are not all fitted to the same
    ## data

    ## Model selection table 
    ##    (Int)    tick tick.bit tick.bts    tcks     acr     chp       der
    ## m2 3.657 0.02323   0.1053  0.05514 0.06004 0.02074 0.02066 -0.003457
    ## m1 3.492 0.04285   0.1359  0.06864 0.08122                          
    ##    der.tick     grd    hkn     hnt     mos   mos.trp     mwn      rpl df
    ## m2  0.08663 -0.1107 0.1062 0.08356 0.01285 -0.006529 0.01002 -0.06794 18
    ## m1                                                                     7
    ##       logLik     AICc    delta weight
    ## m2 -152377.9 304791.8     0.00      1
    ## m1 -186787.0 373587.9 68796.09      0
    ## Models ranked by AICc(x) 
    ## Random terms (all models): 
    ## '1 | dma.ggl'

``` r
L1$incidence.plot = L1$incidence+0.00001
plot <- ggplot(data = L1, aes(x = tick.bite, y= incidence.plot))+
  #geom_point(alpha = 0.5, aes(color = factor(dma.ggl)))
  geom_point(alpha = 0.5)+
  geom_smooth()+
  scale_y_log10()
plot
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](tick_searches_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
library(rsq)
#rsq(m1)  
```

reshape for keras for scalar regression
=======================================

##### read in shapefile of counties and add to it L1 data, then rasterize

``` r
require(raster)
library(rgdal)
```

    ## rgdal: version: 1.3-3, (SVN revision 759)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
    ##  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/sf/gdal
    ##  GDAL binary built with GEOS: FALSE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/sf/proj
    ##  Linking to sp version: 1.2-7

``` r
C <- shapefile("/Users/fischhoff/app_wild/tick_searches/age_codes/cb_2017_us_county_500k.shp")
C$COUNTYFP=as.numeric(as.character(C$COUNTYFP))
C$STATEFP=as.numeric(as.character(C$STATEFP))
C$state_county_fp = paste(C$STATEFP, C$COUNTYFP)
load("L1.Rdata")

L = L1
L$dma.ggl = as.numeric(factor(L$dma.ggl))
L$state = as.numeric(factor(L$state))
pred_name_inds = c(1,2, 7:26)
names= names(L)[pred_name_inds]
L = replace.value( L, names=names, from=NA, to=0)

DP =createDataPartition(L$incidence, p = 0.8)
Train = L[DP$Resample1,]
train_data = Train[,pred_name_inds]
train_targets= Train[,c("year", "incidence", "state_county_fp")]#needs to be reshaped
Test = L[-DP$Resample1,]
test_data = Test[,pred_name_inds]
test_targets= Test[,c("year", "incidence", "state_county_fp")]

Train = data.frame(Train)
uyear = unique(train_data$year)
nimage = 180

a = 1
L_yr = subset(train_data, year == uyear[a])
outC <- append_data(C, L_yr, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
```

    ## Under coverage: 785 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

``` r
pred_names = names(outC[c(13:31)])

#matrix for train data

#4D tensor with shape: (batch, channels, rows, cols) if data_format is #"channels_first"
train_data_mat = array(0, dim = c(length(uyear), length(pred_names), nimage, nimage))
a = 1
b = 10
for (a in 1:length(uyear)){#for loop through years
  print(a)
      L_yr = subset(train_data, year == uyear[a])
    outC <- append_data(C, L_yr, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
  for (b in 1:length(pred_names)){#for loop through predictors
    print(b)
    r <- raster(ncol=nimage, nrow=nimage)
  extent(r) <- extent(outC)
  rp <- rasterize(outC, r, pred_names[b])
  rp_mat = raster::as.array(rp)
  train_data_mat[a, b, ,]=rp_mat
  }
}
```

    ## [1] 1

    ## Under coverage: 785 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 2

    ## Under coverage: 809 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 3

    ## Under coverage: 779 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 4

    ## Under coverage: 813 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 5

    ## Under coverage: 839 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 6

    ## Under coverage: 807 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 7

    ## Under coverage: 782 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 8

    ## Under coverage: 781 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 9

    ## Under coverage: 794 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 10

    ## Under coverage: 838 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 11

    ## Under coverage: 799 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 12

    ## Under coverage: 827 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19
    ## [1] 13

    ## Under coverage: 801 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18
    ## [1] 19

``` r
train_targets_mat = array(0, dim = c(length(uyear), 1, nimage, nimage))

for (a in 1:length(uyear)){#for loop through years
      L_yr = subset(train_targets, year == uyear[a])
    outC <- append_data(C, L_yr, key.shp = "state_county_fp", key.data = "state_county_fp",
                    ignore.duplicates = TRUE)
#  for (b in 1:length(pred_names)){#for loop through predictors
    r <- raster(ncol=nimage, nrow=nimage)
  extent(r) <- extent(outC)
  rp <- rasterize(outC, r, "incidence")
  rp_mat = raster::as.array(rp)
  train_targets_mat[a, 1, , ]=rp_mat
#  }
}
```

    ## Under coverage: 785 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 809 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 779 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 813 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 839 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 807 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 782 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 781 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 794 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 838 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 799 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 827 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

    ## Under coverage: 801 out of 3233 shape features did not get appended data. Run under_coverage() to get the corresponding feature id numbers and key values.

##### summarize by dma -- test with one variable

``` r
load("L1.Rdata")
#L1$tick.bites = L1[,20]
L = L1
library(dplyr)
L1<-L %>%
  group_by(dma.ggl, year) %>%
  summarize(pop.dma = sum(population),
            Cases.dma = sum(Cases),
            incidence.dma = 100000*Cases.dma/pop.dma,
            # garden = garden[1],
            # mowing = mowing[1],
            # hunting = hunting[1],
            # tick = tick[1],
            # deer.tick = deer.tick[1],
            # tick.bite = tick.bite[1],
            # tick.bites=tick.bites[1],
            # ticks = ticks[1], 
            # chipmunk = chipmunk[1],
            # mouse.trap = mouse.trap[1],
            #mouse = mean(mouse[!is.na(mouse)]),
            # deer =deer[1],
            # acorns = acorns[1],
            # repellent = repellent[1],
            # hiking = hiking[1],
            deet = deet[1],
            state = state[1])
summary(L1$incidence.dma)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0000   0.0000   0.4248   7.8521   1.6906 268.9252

``` r
#L <- L1
L2 = L1
save(L2, file = "L2.Rdata")
summary(L2)
```

    ##    dma.ggl               year         pop.dma           Cases.dma     
    ##  Length:2379        Min.   :2004   Min.   :    1385   Min.   :   0.0  
    ##  Class :character   1st Qu.:2007   1st Qu.:  323586   1st Qu.:   0.0  
    ##  Mode  :character   Median :2010   Median :  794328   Median :   4.0  
    ##                     Mean   :2010   Mean   : 1644339   Mean   : 162.3  
    ##                     3rd Qu.:2013   3rd Qu.: 1903494   3rd Qu.:  20.0  
    ##                     Max.   :2016   Max.   :21955677   Max.   :8238.0  
    ##  incidence.dma           deet          state          
    ##  Min.   :  0.0000   Min.   :  0.0   Length:2379       
    ##  1st Qu.:  0.0000   1st Qu.:  0.0   Class :character  
    ##  Median :  0.4248   Median : 13.0   Mode  :character  
    ##  Mean   :  7.8521   Mean   : 16.6                     
    ##  3rd Qu.:  1.6906   3rd Qu.: 27.0                     
    ##  Max.   :268.9252   Max.   :100.0

##### summarize by dma

``` r
load("L1.Rdata")
L1$tick.bites = L1[,20]
L = L1
library(dplyr)
L1<-L %>%
  group_by(dma.ggl, year) %>%
  summarize(pop.dma = sum(population),
            Cases.dma = sum(Cases),
            incidence.dma = 100000*Cases.dma/pop.dma,
            garden = garden[1],
            mowing = mowing[1],
            hunting = hunting[1],
            tick = tick[1],
            deer.tick = deer.tick[1],
            tick.bite = tick.bite[1],
            tick.bites=tick.bites[1],
            ticks = ticks[1], 
            chipmunk = chipmunk[1],
            mouse.trap = mouse.trap[1],
            mouse = mouse[1],
            deer =deer[1],
            acorns = acorns[1],
            repellent = repellent[1],
            hiking = hiking[1],
            deet = deet[1],
            state = state[1])
summary(L1$incidence.dma)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0000   0.0000   0.4248   7.8521   1.6906 268.9252

``` r
#L <- L1
L2 = L1
save(L2, file = "L2.Rdata")
summary(L2)
```

    ##    dma.ggl               year         pop.dma           Cases.dma     
    ##  Length:2379        Min.   :2004   Min.   :    1385   Min.   :   0.0  
    ##  Class :character   1st Qu.:2007   1st Qu.:  323586   1st Qu.:   0.0  
    ##  Mode  :character   Median :2010   Median :  794328   Median :   4.0  
    ##                     Mean   :2010   Mean   : 1644339   Mean   : 162.3  
    ##                     3rd Qu.:2013   3rd Qu.: 1903494   3rd Qu.:  20.0  
    ##                     Max.   :2016   Max.   :21955677   Max.   :8238.0  
    ##                                                                       
    ##  incidence.dma          garden          mowing          hunting      
    ##  Min.   :  0.0000   Min.   :  0.0   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:  0.0000   1st Qu.: 30.0   1st Qu.:  7.00   1st Qu.: 16.00  
    ##  Median :  0.4248   Median : 36.0   Median : 19.00   Median : 26.00  
    ##  Mean   :  7.8521   Mean   : 36.9   Mean   : 22.16   Mean   : 30.26  
    ##  3rd Qu.:  1.6906   3rd Qu.: 42.0   3rd Qu.: 32.00   3rd Qu.: 40.00  
    ##  Max.   :268.9252   Max.   :100.0   Max.   :100.00   Max.   :100.00  
    ##                                                                      
    ##       tick          deer.tick        tick.bite        tick.bites    
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 18.00   1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
    ##  Median : 30.00   Median :  5.00   Median : 10.00   Median :  0.00  
    ##  Mean   : 31.14   Mean   : 11.74   Mean   : 15.26   Mean   : 12.45  
    ##  3rd Qu.: 41.00   3rd Qu.: 16.00   3rd Qu.: 24.00   3rd Qu.: 20.00  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :100.00  
    ##                   NA's   :104                                       
    ##      ticks           chipmunk        mouse.trap         mouse       
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 14.00   1st Qu.: 11.00   1st Qu.:  0.00   1st Qu.: 35.00  
    ##  Median : 23.00   Median : 22.00   Median : 16.00   Median : 55.00  
    ##  Mean   : 24.77   Mean   : 22.35   Mean   : 21.24   Mean   : 51.43  
    ##  3rd Qu.: 33.00   3rd Qu.: 31.00   3rd Qu.: 37.00   3rd Qu.: 64.00  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :100.00  
    ##                   NA's   :183      NA's   :183      NA's   :183     
    ##       deer            acorns         repellent          hiking      
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.: 19.00   1st Qu.:  0.00   1st Qu.: 11.00   1st Qu.: 16.00  
    ##  Median : 28.00   Median : 11.00   Median : 25.00   Median : 22.00  
    ##  Mean   : 31.07   Mean   : 15.67   Mean   : 27.43   Mean   : 25.81  
    ##  3rd Qu.: 40.00   3rd Qu.: 23.00   3rd Qu.: 42.00   3rd Qu.: 31.00  
    ##  Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :100.00  
    ##  NA's   :366      NA's   :366                                       
    ##       deet          state          
    ##  Min.   :  0.0   Length:2379       
    ##  1st Qu.:  0.0   Class :character  
    ##  Median : 13.0   Mode  :character  
    ##  Mean   : 16.6                     
    ##  3rd Qu.: 27.0                     
    ##  Max.   :100.0                     
    ## 

##### predict cases, predictors include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 10000, lr 0.001

##### testing whether folds works with no zeros

``` r
# set.seed(1234)
# 
# load("L2.Rdata")
# # L2$tick.bites = as.numeric(L2$tick.bites)
# # L2$hiking = as.numeric(L2$hiking)
# L2$Cases.dma=as.numeric(L2$Cases.dma)
# #library(dismo)
# L = L2
# # L = subset(L,!is.na(hiking))
# #L$deer.tick = as.numeric(as.character(L$deer.tick))
# L$dma.ggl = factor(L$dma.ggl)
# L = data.frame(L)#have to change back from tibble to data.frame!!
# L$state = factor(L$state)
# L = L[,c("Cases.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]
# 
# 
# DP =createDataPartition(L$Cases.dma, p = 0.8)
# Train = L[DP$Resample1,]
# Test = L[-DP$Resample1,]
# save(Test, file = "Test.Rdata")
# 
# ntrees = 10000
# Train = Train[,c("dma.ggl", "year", "pop.dma", "Cases.dma", "deet")]
# gbm.dma = gbm(data=Train,
#                             Cases.dma ~pop.dma+deet,
#                             distribution = "poisson",
#                             n.trees = ntrees,#fit up to two-way interactions
#                             shrinkage = 0.001,
#                             cv.folds = 2,#getting subscript out of bounds with cv.folds>0
#                             interaction.depth = 2,
#                             bag.fraction = 0.5)#default 
# 
# print(1-sum((Train$Cases.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Train$Cases.dma - mean(Train$Cases.dma))^2))
# 
# #check names of columns used as predictors: 
# save(gbm.dma, file = "gbm.dma.Rdata")
# x = summary(gbm.dma)
# 
# #write results to csv
# x.df= data.frame(variable = x$var, 
#                  relative.influence = x$rel.inf)
# x.df$relative.influence = round(x.df$relative.influence, digits = 3)
# write.csv(x.df, file = "relative.influence.csv")
# #check number of trees
# ntrees = gbm.dma$n.trees
# print(ntrees)#made 
# print(x.df)
# ind = which(x.df$variable == "pop.dma")
# variable = as.character(x.df$variable)
# variable[ind]=as.character("population")
# x.df$variable = variable
# search = rep("Google.search", dim(x.df)[1])
# search[x.df$variable == "population"]="population"
# x.df$search = factor(search) 
# ggplot(data = x.df, aes(x =variable, y = relative.influence, fill = search))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
# #  plot.tmp = ggplot()+
# 
#   geom_bar(stat="identity")
# ggsave("Figure.search.jpg")
# save(gbm.dma, file="gbm.dma.cases.pop.t10000.lr001.Rdata")
```

##### predict cases, predictors include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 10000, lr 0.001

``` r
set.seed(1234)

load("L2.Rdata")
L2$tick.bites = as.numeric(L2$tick.bites)
L2$hiking = as.numeric(L2$hiking)
L2$Cases.dma=as.numeric(L2$Cases.dma)
#library(dismo)
L = L2
L = subset(L,!is.na(hiking))
#L$deer.tick = as.numeric(as.character(L$deer.tick))
L$dma.ggl = factor(L$dma.ggl)
L = data.frame(L)#have to change back from tibble to data.frame!!
L$state = factor(L$state)
L = L[,c("Cases.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]

DP =createDataPartition(L$Cases.dma, p = 0.8)
Train = L[DP$Resample1,]
Test = L[-DP$Resample1,]
save(Test, file = "Test.Rdata")

ntrees = 10000
gbm.dma = gbm(data=Train,
                            Cases.dma ~pop.dma+garden +mowing +hunting+tick+deer.tick +tick.bite+tick.bites+ticks+chipmunk+mouse.trap+deer+acorns+repellent+hiking+deet
,
                            distribution = "poisson",
                            n.trees = ntrees,#fit up to two-way interactions
                            shrinkage = 0.001,
                            cv.folds = 10,#getting subscript out of bounds with cv.folds>0
                            interaction.depth = 4,
                            bag.fraction = 0.5)#default 

print(1-sum((Train$Cases.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
                                         type="response"))^2)/
        sum((Train$Cases.dma - mean(Train$Cases.dma))^2))
```

    ## [1] 0.9125059

``` r
# print(1-sum((Test$Cases.dma - predict(gbm.dma, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$Cases.dma - mean(Test$Cases.dma))^2))

#check names of columns used as predictors: 
save(gbm.dma, file = "gbm.dma.Rdata")
x = summary(gbm.dma)
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
#write results to csv
x.df= data.frame(variable = x$var, 
                 relative.influence = x$rel.inf)
x.df$relative.influence = round(x.df$relative.influence, digits = 3)
write.csv(x.df, file = "relative.influence.csv")
#check number of trees
ntrees = gbm.dma$n.trees
print(ntrees)#made 
```

    ## [1] 10000

``` r
print(x.df)
```

    ##      variable relative.influence
    ## 1     pop.dma             60.959
    ## 2   deer.tick             21.609
    ## 3  tick.bites              5.106
    ## 4   tick.bite              3.522
    ## 5      garden              2.347
    ## 6      hiking              1.660
    ## 7        tick              1.541
    ## 8     hunting              0.867
    ## 9    chipmunk              0.684
    ## 10      ticks              0.458
    ## 11     acorns              0.331
    ## 12       deer              0.316
    ## 13 mouse.trap              0.254
    ## 14       deet              0.125
    ## 15     mowing              0.113
    ## 16  repellent              0.109

``` r
ind = which(x.df$variable == "pop.dma")
variable = as.character(x.df$variable)
variable[ind]=as.character("population")
x.df$variable = variable
search = rep("Google.search", dim(x.df)[1])
search[x.df$variable == "population"]="population"
x.df$search = factor(search) 
ggplot(data = x.df, aes(x =variable, y = relative.influence, fill = search))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  plot.tmp = ggplot()+

  geom_bar(stat="identity")
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
ggsave("Figure.search.jpg")
```

    ## Saving 7 x 5 in image

``` r
save(gbm.dma, file="gbm.dma.cases.pop.t10000.lr001.Rdata")
```

##### plot deviance curve -- not clear whether it has leveled off or not

``` r
library(ggplot2)
df = data.frame(deviance = gbm.dma$train.error, trees = seq(from = 1, to = length(gbm.dma$train.error)))
p <- ggplot(data = df, aes(x=trees, y = deviance))+
  geom_line()
p
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-13-1.png)

##### predict cases, predictors include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 50000, lr 0.001

``` r
set.seed(1234)

load("L2.Rdata")

#library(dismo)
L = L2
L$deer.tick = as.numeric(as.character(L$deer.tick))
L$dma.ggl = factor(L$dma.ggl)
L = data.frame(L)#have to change back from tibble to data.frame!!
L$state = factor(L$state)

L = L[,c("Cases.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]

DP =createDataPartition(L$Cases.dma, p = 0.8)
Train = L[DP$Resample1,]
Test = L[-DP$Resample1,]
save(Test, file = "Test.Rdata")

ntrees = 50000
gbm.dma = gbm(data=Train,
                            Cases.dma ~pop.dma+garden +mowing +hunting+tick+deer.tick +tick.bite+tick.bites+ticks+chipmunk+mouse.trap+deer+acorns+repellent+hiking+deet,
                            distribution = "poisson",#default
                            n.trees = ntrees,#fit up to two-way interactions
                            shrinkage = 0.001,
                            cv.folds = 10,#getting subscript out of bounds with cv.folds>0
                            interaction.depth = 4,
                            bag.fraction = 0.5)#default 

print(1-sum((Train$Cases.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
                                         type="response"))^2)/
        sum((Train$Cases.dma - mean(Train$Cases.dma))^2))
```

    ## [1] 0.990378

``` r
# print(1-sum((Test$Cases.dma - predict(gbm.dma, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$Cases.dma - mean(Test$Cases.dma))^2))

#check names of columns used as predictors: 
save(gbm.dma, file = "gbm.dma.Rdata")
x = summary(gbm.dma)
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
#write results to csv
x.df= data.frame(variable = x$var, 
                 relative.influence = x$rel.inf)
x.df$relative.influence = round(x.df$relative.influence, digits = 3)
write.csv(x.df, file = "relative.influence.csv")
#check number of trees
ntrees = gbm.dma$n.trees
print(ntrees)#made 
```

    ## [1] 50000

``` r
print(x.df)
```

    ##      variable relative.influence
    ## 1     pop.dma             58.601
    ## 2   deer.tick             21.341
    ## 3  tick.bites              5.240
    ## 4   tick.bite              3.640
    ## 5      garden              2.534
    ## 6      hiking              1.716
    ## 7        tick              1.666
    ## 8     hunting              1.221
    ## 9    chipmunk              0.971
    ## 10      ticks              0.655
    ## 11       deer              0.587
    ## 12     acorns              0.490
    ## 13 mouse.trap              0.474
    ## 14       deet              0.362
    ## 15     mowing              0.259
    ## 16  repellent              0.244

``` r
ind = which(x.df$variable == "pop.dma")
variable = as.character(x.df$variable)
variable[ind]=as.character("population")
x.df$variable = variable
search = rep("Google.search", dim(x.df)[1])
search[x.df$variable == "population"]="population"

#order variables by relative influence 
x.df$variable = factor(x.df$variable, levels = 
x.df$variable[order(x.df$relative.influence)])
ggplot(data = x.df, aes(x =variable, y = relative.influence, fill = search))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  plot.tmp = ggplot()+

  geom_bar(stat="identity")
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-14-2.png)

``` r
ggsave("Figure.search.jpg")
```

    ## Saving 7 x 5 in image

``` r
save(gbm.dma, file="gbm.dma.cases.pop.t10000.lr001.Rdata")
```

##### plot deviance curve -- appears to have leveled off with 50000 trees, lr. 0.001

``` r
library(ggplot2)
df = data.frame(deviance = gbm.dma$train.error, trees = seq(from = 1, to = length(gbm.dma$train.error)))
p <- ggplot(data = df, aes(x=trees, y = deviance))+
  geom_line()
p
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-15-1.png)

####### predict cases, predictors DO NOT include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 50000, lr 0.001

``` r
set.seed(1234)

load("L2.Rdata")

#library(dismo)
L = L2
L$deer.tick = as.numeric(as.character(L$deer.tick))
L$dma.ggl = factor(L$dma.ggl)
L = data.frame(L)#have to change back from tibble to data.frame!!
L$state = factor(L$state)
L = L[,c("Cases.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]


DP =createDataPartition(L$Cases.dma, p = 0.8)
Train = L[DP$Resample1,]
Test = L[-DP$Resample1,]
save(Test, file = "Test.Rdata")

ntrees = 50000
gbm.dma = gbm(data=Train,
                            Cases.dma ~garden +mowing +hunting+tick+deer.tick +tick.bite+tick.bites+ticks+chipmunk+mouse.trap+deer+acorns+repellent+hiking+deet,
                            distribution = "poisson",#default
                            n.trees = ntrees,#fit up to two-way interactions
                            shrinkage = 0.001,
                            cv.folds = 10,#getting subscript out of bounds with cv.folds>0
                            interaction.depth = 4,
                            bag.fraction = 0.5)#default 

print(1-sum((Train$Cases.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
                                         type="response"))^2)/
        sum((Train$Cases.dma - mean(Train$Cases.dma))^2))
```

    ## [1] 0.9775653

``` r
# print(1-sum((Test$Cases.dma - predict(gbm.dma, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$Cases.dma - mean(Test$Cases.dma))^2))

#check names of columns used as predictors: 
save(gbm.dma, file = "gbm.dma.Rdata")
x = summary(gbm.dma)
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
#write results to csv
x.df= data.frame(variable = x$var, 
                 relative.influence = x$rel.inf)
x.df$relative.influence = round(x.df$relative.influence, digits = 3)
write.csv(x.df, file = "relative.influence.csv")
#check number of trees
ntrees = gbm.dma$n.trees
print(ntrees)#made 
```

    ## [1] 50000

``` r
print(x.df)
```

    ##      variable relative.influence
    ## 1   deer.tick             24.117
    ## 2     hunting             20.742
    ## 3      garden             17.038
    ## 4  tick.bites              5.482
    ## 5   tick.bite              5.069
    ## 6        deer              4.696
    ## 7   repellent              4.349
    ## 8      hiking              3.883
    ## 9        deet              3.743
    ## 10 mouse.trap              2.912
    ## 11     mowing              2.170
    ## 12     acorns              1.767
    ## 13   chipmunk              1.480
    ## 14      ticks              1.429
    ## 15       tick              1.122

``` r
ind = which(x.df$variable == "pop.dma")
variable = as.character(x.df$variable)
variable[ind]=as.character("population")
x.df$variable = variable
# search = rep("Google.search", dim(x.df)[1])
# search[x.df$variable == "population"]="population"

#order variables by relative influence 
x.df$variable = factor(x.df$variable, levels = 
x.df$variable[order(x.df$relative.influence)])
ggplot(data = x.df, aes(x =variable, y = relative.influence))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  plot.tmp = ggplot()+

  geom_bar(stat="identity")
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-16-2.png)

``` r
ggsave("Figure.search.jpg")
```

    ## Saving 7 x 5 in image

``` r
save(gbm.dma, file="gbm.dma.cases.t10000.lr001.Rdata")
```

##### plot deviance curve with 50000 trees, lr. 0.001, population not included as predictor of cases

``` r
library(ggplot2)
df = data.frame(deviance = gbm.dma$train.error, trees = seq(from = 1, to = length(gbm.dma$train.error)))
p <- ggplot(data = df, aes(x=trees, y = deviance))+
  geom_line()
p
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-17-1.png)

##### predict incidence, predictors include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 50000, lr 0.001

``` r
set.seed(1234)

load("L2.Rdata")

#library(dismo)
L = L2
L$deer.tick = as.numeric(as.character(L$deer.tick))
L$dma.ggl = factor(L$dma.ggl)
L = data.frame(L)#have to change back from tibble to data.frame!!
L$state = factor(L$state)
# keep.col = c("incidence.dma", "deer.tick", "tick.bite", "ticks", "tick.bites", "hiking", "deet", "repellent")
L = L[,c("incidence.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]

DP =createDataPartition(L$incidence.dma, p = 0.8)
Train = L[DP$Resample1,]
Test = L[-DP$Resample1,]
save(Test, file = "Test.Rdata")

ntrees = 50000
gbm.dma = gbm(data=Train,
                            incidence.dma ~pop.dma+ garden +mowing +hunting+tick+deer.tick +tick.bite+tick.bites+ticks+chipmunk+mouse.trap+deer+acorns+repellent+hiking+deet,
                            distribution = "gaussian",#default
                            n.trees = ntrees,#fit up to two-way interactions
                            shrinkage = 0.001,
                            interaction.depth = 4,
                            cv.folds = 10,#getting subscript out of bounds with cv.folds>0

                            bag.fraction = 0.5)#default 

print(1-sum((Train$incidence.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
                                         type="response"))^2)/
        sum((Train$incidence.dma - mean(Train$incidence.dma))^2))
```

    ## [1] 0.9440023

``` r
# print(1-sum((Test$incidence.dma - predict(gbm.dma, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$incidence.dma - mean(Test$incidence.dma))^2))


#check names of columns used as predictors: 
save(gbm.dma, file = "gbm.dma.Rdata")
x = summary(gbm.dma)
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
#write results to csv
x.df= data.frame(variable = x$var, 
                 relative.influence = x$rel.inf)
x.df$relative.influence = round(x.df$relative.influence, digits = 3)
write.csv(x.df, file = "relative.influence.csv")
#check number of trees
ntrees = gbm.dma$n.trees
print(ntrees)#made 
```

    ## [1] 50000

``` r
print(x.df)
```

    ##      variable relative.influence
    ## 1   deer.tick             33.377
    ## 2     pop.dma             12.293
    ## 3   tick.bite              6.991
    ## 4      acorns              6.893
    ## 5     hunting              6.046
    ## 6        deer              5.056
    ## 7  mouse.trap              4.708
    ## 8       ticks              4.591
    ## 9        tick              3.838
    ## 10 tick.bites              3.485
    ## 11     hiking              3.024
    ## 12   chipmunk              2.736
    ## 13  repellent              2.306
    ## 14     mowing              1.791
    ## 15       deet              1.646
    ## 16     garden              1.218

``` r
ind = which(x.df$variable == "pop.dma")
variable = as.character(x.df$variable)
variable[ind]=as.character("population")
x.df$variable = variable
search = rep("Google.search", dim(x.df)[1])
search[x.df$variable == "population"]="population"

#order variables by relative influence 
x.df$variable = factor(x.df$variable, levels = 
x.df$variable[order(x.df$relative.influence)])
ggplot(data = x.df, aes(x =variable, y = relative.influence, fill = search))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  plot.tmp = ggplot()+

  geom_bar(stat="identity")
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
ggsave("Figure.search.jpg")
```

    ## Saving 7 x 5 in image

``` r
#save(gbm.dma, file="gbm.dma.cases.pop.t10000.lr001.Rdata")
```

##### plot deviance curve -- appears to have leveled off with 50000 trees, lr. 0.001

``` r
library(ggplot2)
df = data.frame(deviance = gbm.dma$train.error, trees = seq(from = 1, to = length(gbm.dma$train.error)))
p <- ggplot(data = df, aes(x=trees, y = deviance))+
  geom_line()
p
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-19-1.png)

##### predict incidence, predictors do not include population: run boosted regression tree analysis -- analyze at dma-level w/ gbm. trees 50000, lr 0.001

``` r
set.seed(1234)

load("L2.Rdata")

#library(dismo)
L = L2
L$deer.tick = as.numeric(as.character(L$deer.tick))
L$dma.ggl = factor(L$dma.ggl)
L = data.frame(L)#have to change back from tibble to data.frame!!
L$state = factor(L$state)
L = L[,c("incidence.dma", "pop.dma","garden","mowing","hunting","tick","deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")]

DP =createDataPartition(L$incidence.dma, p = 0.8)
Train = L[DP$Resample1,]
Test = L[-DP$Resample1,]
save(Test, file = "Test.Rdata")

ntrees = 50000
gbm.dma = gbm(data=Train,
                            incidence.dma ~garden +mowing +hunting+tick+deer.tick +tick.bite+tick.bites+ticks+chipmunk+mouse.trap+deer+acorns+repellent+hiking+deet,
 
                            distribution = "gaussian",#default
                            n.trees = ntrees,#fit up to two-way interactions
                            shrinkage = 0.001,
                            interaction.depth = 4,
                             cv.folds = 10,#getting subscript out of bounds with cv.folds>0

                            bag.fraction = 0.5)#default 

print(1-sum((Train$incidence.dma - predict(gbm.dma, newdata=Train, n.trees =ntrees,
                                         type="response"))^2)/
        sum((Train$incidence.dma - mean(Train$incidence.dma))^2))
```

    ## [1] 0.9212673

``` r
#get accuracy against test set
# print(1-sum((Test$incidence.dma - predict(gbm.dma, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$incidence.dma - mean(Test$incidence.dma))^2))

#check names of columns used as predictors: 
save(gbm.dma, file = "gbm.dma.Rdata")
x = summary(gbm.dma)
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
#write results to csv
x.df= data.frame(variable = x$var, 
                 relative.influence = x$rel.inf)
x.df$relative.influence = round(x.df$relative.influence, digits = 3)
write.csv(x.df, file = "relative.influence.csv")
#check number of trees
ntrees = gbm.dma$n.trees
print(ntrees)#made 
```

    ## [1] 50000

``` r
print(x.df)
```

    ##      variable relative.influence
    ## 1   deer.tick             34.847
    ## 2      acorns              8.763
    ## 3   tick.bite              7.468
    ## 4     hunting              6.854
    ## 5  mouse.trap              6.397
    ## 6        deer              5.736
    ## 7       ticks              5.389
    ## 8        tick              4.336
    ## 9  tick.bites              4.109
    ## 10     hiking              3.542
    ## 11   chipmunk              3.357
    ## 12     mowing              2.699
    ## 13  repellent              2.639
    ## 14       deet              2.279
    ## 15     garden              1.585

``` r
ind = which(x.df$variable == "pop.dma")
variable = as.character(x.df$variable)
variable[ind]=as.character("population")
x.df$variable = variable
# search = rep("Google.search", dim(x.df)[1])
# search[x.df$variable == "population"]="population"

#order variables by relative influence 
x.df$variable = factor(x.df$variable, levels = 
x.df$variable[order(x.df$relative.influence)])
ggplot(data = x.df, aes(x =variable, y = relative.influence))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  plot.tmp = ggplot()+

  geom_bar(stat="identity")
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-20-2.png)

``` r
ggsave("Figure.search.jpg")
```

    ## Saving 7 x 5 in image

``` r
#save(gbm.dma, file="gbm.dma.cases.pop.t10000.lr001.Rdata")
```

##### plot deviance curve -- with 50000 trees, lr. 0.001

``` r
library(ggplot2)
df = data.frame(deviance = gbm.dma$train.error, trees = seq(from = 1, to = length(gbm.dma$train.error)))
p <- ggplot(data = df, aes(x=trees, y = deviance))+
  geom_line()
p
```

![](tick_searches_files/figure-markdown_github/unnamed-chunk-21-1.png)

dismo: fit cofeed model, ntrees = 10000, lr = 0.001 -- Cases, including population
==================================================================================

``` r
# library(dismo)
# 
# load("L2.Rdata")
# L = L2
# L = data.frame(L)#have to change back from tibble to data.frame!!
# L$dma.ggl = factor(L$dma.ggl)
# L$state = factor(L$state)
# L$Cases.dma=as.numeric(L$Cases.dma)
# DP =createDataPartition(L$Cases.dma, p = 0.8)
# Train = L[DP$Resample1,]
# Test = L[-DP$Resample1,]
# #ntrees = 50000
# 
# ntrees = 10000
#  
#  # gbm.x = c("pop.dma", "garden","mowing","hunting", "tick", "deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet")
#  gbm.x = which(names(Train) %in% c("pop.dma", "garden","mowing","hunting", "tick", "deer.tick", "tick.bite","tick.bites","ticks","chipmunk","mouse.trap","deer","acorns","repellent","hiking","deet"))
# #gbm.x = which(names(Train) %in% c("pop.dma", "deer.tick"))
# #gbm.x = c("pop.dma","deer.tick")
# gbm.y = which(names(Train)=="Cases.dma")
# #Train$deer.tick[is.na(Train$deer.tick)]=0
# 
# #Train = Train[,c(gbm.x,gbm.y)]
# 
# gbm.dismo <- gbm.step(data=Train,
#                             gbm.x = gbm.x,
#                             gbm.y = gbm.y,
#                             tree.complexity = 4,
#                             learning.rate =0.001,
#                             max.trees = ntrees,
#                           family="poisson",#default
#                             n.folds = 10,
#                       bag.fraction =0.5)#default 
# 
# print(1-sum((Train$Cases.dma - predict(gbm.dismo, newdata=Train, n.trees =ntrees,
#                                         type="response"))^2)/
#         sum((Train$Cases.dma - mean(Train$Cases.dma))^2))
# 
# #once Train is set, then compute R2 for Test
# print(1-sum((Test$Cases.dma - predict(gbm.dismo, newdata=Test, n.trees =ntrees,
#                                          type="response"))^2)/
#         sum((Test$Cases.dma - mean(Test$Cases.dma))^2))
# 
# 
# #check names of columns used as predictors: 
#  x = summary(gbm.dismo)
# # 
#  x.df= data.frame(variable = x$var, 
#                   relative.influence = x$rel.inf)
# x.df$variable=as.character(x.df$variable)
# x.df$variable = factor(x.df$variable, levels = x.df$variable[order(x.df$relative.influence)])
# ggplot(data = x.df, aes(x =variable, y = relative.influence, fill = search))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
# #  plot.tmp = ggplot()+
# 
#   geom_bar(stat="identity")
# #ggsave("Figure.2.cofeed.relative.jpg")
# save(gbm.dismo, file = "gbm.dismo.Rdata")
```

DL in keras --preparing data
============================

``` r
library(anchors)
library(caret)
library(scales)
load("L2.Rdata")
L2$row.id = sample(seq(1,dim(L2)[1]), replace = FALSE)

L2$dma.ggl = as.numeric(factor(L2$dma.ggl))
L2$state = as.numeric(factor(L2$state))
pred_name_inds = c(1:3, 6:22)
names= names(L2)[pred_name_inds]
L2 = replace.value( L2, names=names, from=NA, to=0)
L2 = data.frame(L2)

DP =createDataPartition(L2$incidence, p = 0.8)
Train = L2[DP$Resample1,pred_name_inds]
rown = dim(Train)[1]
coln = dim(Train)[2]
train_mat = array(0, dim = c(rown, coln))
for (a in 1:rown){
  for (b in 1:coln){
    train_mat[a,b]=Train[a,b]
  }
}
train_data = train_mat

#test data
Test = L2[-DP$Resample1,pred_name_inds]

rown = dim(Test)[1]
coln = dim(Test)[2]
Test = data.frame(Test)
test_mat = array(0, dim = c(rown, coln))
for (a in 1:rown){
  #print(a)
  for (b in 1:coln){
    #print(b)
    test_mat[a,b]=Test[a,b]
  }
}
test_data = test_mat

train_targets = L2[DP$Resample1,c("incidence.dma")]

test_targets = L2[-DP$Resample1,c("incidence.dma")]

mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)
```

load keras
==========

``` r
library(keras)#loads keras
install_keras()#install Tensorflow
```

    ## Creating virtualenv for TensorFlow at  ~/.virtualenvs/r-tensorflow 
    ## Upgrading pip ...
    ## Upgrading wheel ...
    ## Upgrading setuptools ...
    ## Installing TensorFlow ...
    ## 
    ## Installation complete.

building network in keras
=========================

``` r
# Because we will need to instantiate the same model multiple times,
# we use a function to construct it.
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data)[[2]]) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1) 
    
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mae")
  )
}
```

k-fold validation
=================

seems warnings about dtype can be ignored:
==========================================

<https://github.com/ContinuumIO/anaconda-issues/issues/6678>
============================================================

``` r
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 100
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
                
  # Evaluate the model on the validation data
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$mean_absolute_error)
}  
```

    ## processing fold # 1 
    ## processing fold # 2 
    ## processing fold # 3 
    ## processing fold # 4

get scores
==========

``` r
all_scores
```

    ## [1] 6.131741 8.874018 5.345030 8.744286

``` r
mean(all_scores)
```

    ## [1] 7.273769

``` r
median(train_targets)
```

    ## [1] 0.4248542

some memory cleanup
===================

``` r
k_clear_session()
```

run for same number of epochs and save output
=============================================

``` r
num_epochs <- 100
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}
```

    ## processing fold # 1 
    ## processing fold # 2 
    ## processing fold # 3 
    ## processing fold # 4

compute average of the per-epoch MAE scores
===========================================

``` r
average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)
```

plot MAE over epoch
===================

``` r
library(ggplot2)
average_mae_history[c(1:20),]#looks like lowest is epoch 8
```

    ##    epoch validation_mae
    ## 1      1       7.744311
    ## 2      2       7.291939
    ## 3      3       6.896945
    ## 4      4       6.643671
    ## 5      5       6.583371
    ## 6      6       6.360977
    ## 7      7       6.359647
    ## 8      8       6.239744
    ## 9      9       6.317265
    ## 10    10       6.287138
    ## 11    11       6.367192
    ## 12    12       6.220681
    ## 13    13       6.358806
    ## 14    14       6.541164
    ## 15    15       6.330617
    ## 16    16       6.218168
    ## 17    17       6.352088
    ## 18    18       6.343019
    ## 19    19       6.656819
    ## 20    20       6.221958

``` r
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()+
  geom_line()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](tick_searches_files/figure-markdown_github/unnamed-chunk-31-1.png)

some memory cleanup
===================

``` r
k_clear_session()
```

k-fold validation
=================

seems warnings about dtype can be ignored:
==========================================

<https://github.com/ContinuumIO/anaconda-issues/issues/6678>
============================================================

``` r
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 8
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
                
  # Evaluate the model on the validation data
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$mean_absolute_error)
}  
```

    ## processing fold # 1 
    ## processing fold # 2 
    ## processing fold # 3 
    ## processing fold # 4

get scores
==========

``` r
all_scores
```

    ## [1] 5.546057 7.658791 4.456352 7.796789

``` r
mean(all_scores)
```

    ## [1] 6.364497

``` r
median(train_targets)
```

    ## [1] 0.4248542
