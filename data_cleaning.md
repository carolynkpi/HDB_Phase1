Singapore HDB Flat Resale Price Analysis - Part 1
================
Carolyn Koay
20 January 2018

## Data Preparation

Data source: <https://data.gov.sg/dataset/resale-flat-prices>

Data time range: Jan 2015 - Nov 2017

Libraries used:

``` r
library(dplyr)
```

Here are the variables in the data loaded
    in.

    ##  [1] "month"               "town"                "flat_type"          
    ##  [4] "block"               "street_name"         "storey_range"       
    ##  [7] "floor_area_sqm"      "flat_model"          "lease_commence_date"
    ## [10] "remaining_lease"     "resale_price"

Data is read with stringsAsFactors = TRUE by default. Variables that
should not be factor type are changed back to character type.

``` r
data[['block']] = sapply(data[['block']], as.character)
data[['street_name']] = sapply(data[['street_name']], as.character)
```

Date comes in “YYYY-mm” format in factor
    type.

``` r
str(data[['month']])
```

    ##  Factor w/ 35 levels "2015-01","2015-02",..: 1 1 1 1 1 1 1 1 1 1 ...

The following code convert it to a “YYYY-mm-dd” in character type and
date type. Day is treated to be “01” but it shall bear no significance.

``` r
data[['date_char']] = sapply(data[['month']], as.character)
data[['date_char']] = paste(data[['date_char']], "-01",sep="")
data['date']=as.Date(data[['date_char']],"%Y-%m-%d")

dates = data.frame(matrix(unlist(strsplit(data[['date_char']],'-')),nrow = dim(data)[1], byrow=TRUE),stringsAsFactors = TRUE)
colnames(dates) = c('year','month','day')
data = cbind(dates[,-3], data[,-1])
remove(dates)
```

``` r
str(data[['date_char']])
```

    ##  chr [1:55979] "2015-01-01" "2015-01-01" "2015-01-01" ...

``` r
str(data[['date']])
```

    ##  Date[1:55979], format: "2015-01-01" "2015-01-01" "2015-01-01" "2015-01-01" ...

Create new variables.

``` r
data['floor_area_sqft'] = data['floor_area_sqm']*10.7639
data['age']=as.integer(format(Sys.Date(),'%Y'))-data['lease_commence_date']
data['total_lease']=data['age']+data['remaining_lease']
data ['price_per_sqft'] = data['resale_price']/data['floor_area_sqft']
```

Let’s map the 26 towns to 5 main regions per HDB’s categorization.  
<http://www.hdb.gov.sg/cs/infoweb/about-us/history/hdb-towns-your-home>

    ##               town     region
    ## 1       ANG MO KIO NORTH-EAST
    ## 2            BEDOK       EAST
    ## 3           BISHAN    CENTRAL
    ## 4      BUKIT BATOK       WEST
    ## 5      BUKIT MERAH    CENTRAL
    ## 6    BUKIT PANJANG       WEST
    ## 7      BUKIT TIMAH    CENTRAL
    ## 8     CENTRAL AREA    CENTRAL
    ## 9    CHOA CHU KANG       WEST
    ## 10        CLEMENTI       WEST
    ## 11         GEYLANG    CENTRAL
    ## 12         HOUGANG NORTH-EAST
    ## 13     JURONG EAST       WEST
    ## 14     JURONG WEST       WEST
    ## 15 KALLANG/WHAMPOA    CENTRAL
    ## 16   MARINE PARADE    CENTRAL
    ## 17       PASIR RIS       EAST
    ## 18         PUNGGOL NORTH-EAST
    ## 19      QUEENSTOWN    CENTRAL
    ## 20       SEMBAWANG      NORTH
    ## 21        SENGKANG NORTH-EAST
    ## 22       SERANGOON NORTH-EAST
    ## 23        TAMPINES       EAST
    ## 24       TOA PAYOH    CENTRAL
    ## 25       WOODLANDS      NORTH
    ## 26          YISHUN      NORTH

``` r
data = full_join(data,regions,by='town')
```

Bin continuous
data.

``` r
year_break = seq(as.integer(min(data$lease_commence_date)/10)*10,as.integer(max(data$lease_commence_date)/10)*10+10,10)
year_bin = cut(data[['lease_commence_date']],year_break)
year_bin_names = paste(as.character(year_break[-length(year_break)]),'s',sep='')
levels(year_bin)=year_bin_names
data['lease_commence_year_bin'] = year_bin
remove(year_break, year_bin, year_bin_names)

remaining_lease_break = seq(as.integer(min(data$remaining_lease)/10)*10,as.integer(max(data$remaining_lease)/10)*10+10,10)
remaining_lease_bin = cut(data[['remaining_lease']],remaining_lease_break)
data['remaining_lease_bin'] = remaining_lease_bin
remove(remaining_lease_break, remaining_lease_bin)

age_break = seq(as.integer(min(data$age)/10)*10,as.integer(max(data$age)/10)*10+10,10)
age_bin = cut(data[['age']],age_break)
data['age_bin'] = age_bin
remove(age_break, age_bin)

sqft_break = c(250,500,750,1000,1250,1500, max(data[['floor_area_sqft']]))
sqft_bin = cut(data[['floor_area_sqft']],sqft_break)
sqft_bin_names = paste(as.character(sqft_break),as.character(sqft_break+250), sep="-")
sqft_bin_names = c(sqft_bin_names[1:(length(sqft_bin_names)-2)],">1500")
levels(sqft_bin)=sqft_bin_names 
data['sqft_bin'] = sqft_bin
remove(sqft_break, sqft_bin,sqft_bin_names)

lower = as.integer(min(data$resale_price)/100000)*100000
higher = as.integer(max(data$resale_price)/100000)*100000 + 100000
resale_price_break = seq(lower,higher,100000)
resale_price_bin = cut(data[['resale_price']],resale_price_break)
resale_price_bin_names = paste(as.character(resale_price_break/1000),'-',as.character(resale_price_break/1000+100),'k',sep='')
levels(resale_price_bin) = resale_price_bin_names[-length(resale_price_bin_names)]
data['resale_price_bin'] = resale_price_bin

lower = as.integer(min(data$price_per_sqft)/100)*100
higher = as.integer(max(data$price_per_sqft)/100)*100 + 100
price_psf_break = seq(lower,higher,100)
price_psf_bin = cut(data[['price_per_sqft']],price_psf_break)
data['price_psf_bin'] = price_psf_bin
```

Let’s take a look at the final data.

``` r
str(data)
```

    ## 'data.frame':    55979 obs. of  25 variables:
    ##  $ year                   : Factor w/ 3 levels "2015","2016",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ month                  : Factor w/ 12 levels "01","02","03",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ town                   : Factor w/ 26 levels "ANG MO KIO","BEDOK",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ flat_type              : Factor w/ 7 levels "1 ROOM","2 ROOM",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ block                  : chr  "174" "541" "163" "446" ...
    ##  $ street_name            : chr  "ANG MO KIO AVE 4" "ANG MO KIO AVE 10" "ANG MO KIO AVE 4" "ANG MO KIO AVE 10" ...
    ##  $ storey_range           : Factor w/ 17 levels "01 TO 03","04 TO 06",..: 3 1 1 1 3 3 1 1 1 5 ...
    ##  $ floor_area_sqm         : num  60 68 69 68 68 67 68 68 67 68 ...
    ##  $ flat_model             : Factor w/ 20 levels "2-room","Adjoined flat",..: 5 12 12 12 12 12 12 12 12 12 ...
    ##  $ lease_commence_date    : int  1986 1981 1980 1979 1980 1980 1980 1981 1978 1985 ...
    ##  $ remaining_lease        : int  70 65 64 63 64 64 64 65 62 69 ...
    ##  $ resale_price           : num  255000 275000 285000 290000 290000 ...
    ##  $ date_char              : chr  "2015-01-01" "2015-01-01" "2015-01-01" "2015-01-01" ...
    ##  $ date                   : Date, format: "2015-01-01" "2015-01-01" ...
    ##  $ floor_area_sqft        : num  646 732 743 732 732 ...
    ##  $ age                    : int  32 37 38 39 38 38 38 37 40 33 ...
    ##  $ total_lease            : int  102 102 102 102 102 102 102 102 102 102 ...
    ##  $ price_per_sqft         : num  395 376 384 396 396 ...
    ##  $ region                 : Factor w/ 5 levels "CENTRAL","EAST",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ lease_commence_year_bin: Factor w/ 6 levels "1960s","1970s",..: 3 3 2 2 2 2 2 3 2 3 ...
    ##  $ remaining_lease_bin    : Factor w/ 6 levels "(40,50]","(50,60]",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ age_bin                : Factor w/ 6 levels "(0,10]","(10,20]",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ sqft_bin               : Factor w/ 6 levels "250-500","500-750",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ resale_price_bin       : Factor w/ 11 levels "100-200k","200-300k",..: 2 2 2 2 2 2 2 2 2 3 ...
    ##  $ price_psf_bin          : Factor w/ 8 levels "(200,300]","(300,400]",..: 2 2 2 2 2 3 2 3 3 3 ...

``` r
summary(data)
```

    ##    year           month                town                  flat_type    
    ##  2015:17780   05     : 5359   JURONG WEST: 4387   1 ROOM          :   24  
    ##  2016:19374   04     : 5277   WOODLANDS  : 4029   2 ROOM          :  553  
    ##  2017:18825   08     : 5277   SENGKANG   : 4008   3 ROOM          :14716  
    ##               06     : 5271   TAMPINES   : 3848   4 ROOM          :23066  
    ##               10     : 5208   BEDOK      : 3498   5 ROOM          :13291  
    ##               11     : 5034   YISHUN     : 3318   EXECUTIVE       : 4321  
    ##               (Other):24553   (Other)    :32891   MULTI-GENERATION:    8  
    ##     block           street_name          storey_range   floor_area_sqm 
    ##  Length:55979       Length:55979       04 TO 06:13332   Min.   : 31.0  
    ##  Class :character   Class :character   07 TO 09:12290   1st Qu.: 75.0  
    ##  Mode  :character   Mode  :character   10 TO 12:10673   Median : 96.0  
    ##                                        01 TO 03:10341   Mean   : 97.2  
    ##                                        13 TO 15: 4934   3rd Qu.:111.0  
    ##                                        16 TO 18: 2036   Max.   :280.0  
    ##                                        (Other) : 2373                  
    ##              flat_model    lease_commence_date remaining_lease
    ##  Model A          :16862   Min.   :1966        Min.   :48     
    ##  Improved         :14148   1st Qu.:1984        1st Qu.:66     
    ##  New Generation   : 9107   Median :1989        Median :72     
    ##  Premium Apartment: 5751   Mean   :1991        Mean   :74     
    ##  Simplified       : 2787   3rd Qu.:2000        3rd Qu.:83     
    ##  Apartment        : 2191   Max.   :2015        Max.   :97     
    ##  (Other)          : 5133                                      
    ##   resale_price      date_char              date           
    ##  Min.   : 175000   Length:55979       Min.   :2015-01-01  
    ##  1st Qu.: 340000   Class :character   1st Qu.:2015-10-01  
    ##  Median : 408888   Mode  :character   Median :2016-07-01  
    ##  Mean   : 438932                      Mean   :2016-06-23  
    ##  3rd Qu.: 500000                      3rd Qu.:2017-04-01  
    ##  Max.   :1180000                      Max.   :2017-11-01  
    ##                                                           
    ##  floor_area_sqft       age         total_lease    price_per_sqft 
    ##  Min.   : 333.7   Min.   : 3.00   Min.   : 99.0   Min.   :242.7  
    ##  1st Qu.: 807.3   1st Qu.:18.00   1st Qu.:100.0   1st Qu.:357.3  
    ##  Median :1033.3   Median :29.00   Median :100.0   Median :399.2  
    ##  Mean   :1046.3   Mean   :26.51   Mean   :100.5   Mean   :424.1  
    ##  3rd Qu.:1194.8   3rd Qu.:34.00   3rd Qu.:101.0   3rd Qu.:457.4  
    ##  Max.   :3013.9   Max.   :52.00   Max.   :103.0   Max.   :989.0  
    ##                                                                  
    ##         region      lease_commence_year_bin remaining_lease_bin
    ##  CENTRAL   :10858   1960s: 1289             (40,50] :  456     
    ##  EAST      : 9108   1970s: 9369             (50,60] : 6168     
    ##  NORTH     : 8842   1980s:18119             (60,70] :18536     
    ##  NORTH-EAST:13490   1990s:13288             (70,80] :12190     
    ##  WEST      :13681   2000s: 9946             (80,90] :13566     
    ##                     2010s: 3968             (90,100]: 5063     
    ##                                                                
    ##     age_bin           sqft_bin     resale_price_bin   price_psf_bin  
    ##  (0,10] : 5293   250-500  :  394   300-400k:19465   (300,400]:26955  
    ##  (10,20]:14182   500-750  :10404   400-500k:15502   (400,500]:18553  
    ##  (20,30]:12428   750-1000 :13876   200-300k: 7252   (500,600]: 5296  
    ##  (30,40]:18503   1000-1250:19211   500-600k: 6506   (600,700]: 2058  
    ##  (40,50]: 5215   1250-1500: 8688   600-700k: 3746   (700,800]: 1327  
    ##  (50,60]:  358   >1500    : 3406   700-800k: 2002   (200,300]: 1310  
    ##                                    (Other) : 1506   (Other)  :  480

Wow, no missing values in the factor and numeric variables. Let’s check
for missing values in the character variables.

``` r
sum(is.na(data$block))
```

    ## [1] 0

``` r
sum(is.na(data$street_name))
```

    ## [1] 0

The data is very clean\! Thank you data.gov.sg\!\!\!

Now save the cleaned data for use later.

``` r
write.csv(data,'cleaned_data.csv',row.names=FALSE)
classes = sapply(data,class)
write.csv(classes, 'cleaned_data_classes.csv')
save(data,file = 'cleaned_data')
```

<br><br> ***End of part 1*** <br><br>
