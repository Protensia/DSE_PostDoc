#title: "Code to read in and organize Zambia district prices"
#author: "Protensia Hadunka"
#date: "January 2, 2025"


# 1. Setup ----------------------------------------------------------------

options(scipen=999)
set.seed(12345)


###  Check for and load Packages   ###
library(installr)
#updateR()
# ## get packages installed
# packs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)
# 
# ## and now re-install install packages using install.packages()
# install.packages(packs$Package)

## Clear worksace
rm(list = ls())
gc()


if (!require("pacman")) install.packages("pacman")

pacman::p_load(data.table,tidyverse,readxl,haven,janitor,stringr,styler,sjmisc,magrittr,gdata,labelled,stringdist,tidygeocoder)
pacman::p_update()

# 2. Load data ------------------------------------------------------------

setwd("~/")

units <- read_excel("C:/Users/hadunka2/Box/Zambia_volatility_storage/Data/Raw Data/CSO Data/CSO Data-District/Units_112221.xlsx", 
                    sheet = "Sheet1", skip = 2)

# Step 2: Clean column names
units <- units %>% clean_names()

# Step 3: Convert product_code to numeric
units <- units %>% mutate(product_code = as.numeric(gsub('\\D+', '', product_code)))

# Step 4: Rename columns
units <- units %>% dplyr::rename(product = description, unit = prefd_uo_m, unit_qty = prefd_qty)

folders<-seq(2010,2022,by=1)

for(i in folders){
  filepath <- file.path(paste("C:/Users/hadunka2/Box/Zambia_volatility_storage/Data/Raw Data/CSO Data/CSO Data-District/",i,"/19,",i,".xls",sep="")) #name each excel file
  
  df<-read_excel(filepath,sheet = "Document map",col_names = c("district"),skip=1)%>% #import the file with the list of districts
    mutate(district=paste(gsub(" ","_",gsub("North Western","NorthWestern",gsub("Kapiri Mposhi","KapiriMposhi",district)),fixed=TRUE),i,sep="_"), #change the district name to include the year and no spaces
           index=paste0("Sheet",1:n()))
  
  sheetnames<-excel_sheets(filepath) #read in the names of all the sheets
  sheetnames<-sheetnames[2:(length(sheetnames)-1)] #delete the document map from the list of names and the blank final sheet
  
  list_all<-map_dfr(sheetnames,~read_excel(filepath,sheet=.x),.id="sheet")%>% #read in all sheets
    remove_constant(na.rm=TRUE)%>%
    clean_names()%>%
    mutate(sheet=as.factor(paste0("Sheet",sheet)))%>%
    as.data.frame()%>%
    filter(!is.na(x2))%>%
    mutate(central_statistical_office=as.numeric(gsub('\\D+','',central_statistical_office)))%>%
    filter(is.finite(central_statistical_office))%>%
    left_join(df,by=c("sheet"="index"))%>%
    separate(district,into=c("province","district","year"),sep="_")%>%
    mutate(year=as.numeric(year))%>%
    dplyr::select(province,district,year,everything(),-sheet)
  
  col_del <- function(y) {
    if ("x3" %in% colnames(y)) { # Check if column `x3` exists
      if (any(y$year >= 2016)) { # Check if any `year` is >= 2016
        y <- within(y, rm(x3))   # Remove the `x3` column
      }
    }
    return(y)
  }
  
  list_all <- col_del(list_all)  
  
  colnames(list_all) <- c("province", "district", "year", "product_code", "product",
                          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                          "Oct", "Nov", "Dec", "m_inf", "y_inf")
  
  list_all <- list_all %>%
    mutate_at(c(6:19), as.numeric) %>% # Recode prices as numeric
    mutate_at(c(6:17), ~ifelse(year %in% c(2010, 2011, 2012), ./1000, .)) %>% # Remove '000 for 2010-12
    mutate_at(c(6:10), ~ifelse(year == 2013, ./1000, .)) %>% # Adjust Jan-May 2013
    mutate_if(is.numeric, ~ifelse(. == 0, NA, .)) # Replace 0 with NA
  
  
  all_years <- mget(ls(pattern = "table_")) %>%
    bind_rows() %>%
    mutate(district = gsub("KapiriMposhi", "Kapiri Mposhi", district),
           province = gsub("NorthWestern", "North Western", province)) %>%
    pivot_wider(names_from = year, values_from = c(Jan:y_inf), names_sort = TRUE) %>%
    left_join(units, by = c("product", "product_code")) %>%
    dplyr::select(province, district, product_code, product, unit, unit_qty,
                  contains("2010"), contains("2011"), contains("2012"), contains("2013"),
                  contains("2014"), contains("2015"), contains("2016"), contains("2017"),
                  contains("2018"), contains("2019"), contains("2020")) %>%
    arrange(province, district, product_code)
  
  rm(df,list_all,filepath,i,folders,sheetnames,list=ls(pattern="table"),col_del,units)

  # 3. Geocode data ------------------------------------------------------------
  geo_years=all_years%>%
    mutate(district=gsub("Chienge","Chiengi",gsub("Milengi","Milenge",gsub("(Chizera)","",district))),g_var=paste(district,"Zambia",sep=", "))%>%
    tidygeocoder::geocode(g_var, method='osm', lat=lat,long=lon)%>%
    dplyr::select(-g_var)
  
  
  ###separate price and inflation data
  geo_prices=geo_years%>%
    dplyr::select(-contains("inf"))
  
  geo_inflation=geo_years%>%
    dplyr::select(province,district,product_code,product,unit,unit_qty,contains("inf"),lat,lon)
 
  # 4. Save files ------------------------------------------------------------
  write_csv(geo_prices, "C:/Users/hadunka2/Box/Zambia_volatility_storage/Data/Clean Data/protensia_districtprices.csv")
  write_csv(geo_inflation,"C:/Users/hadunka2/Box/Zambia_volatility_storage/Data/Clean Data/protensia_inflation.csv")
  
  