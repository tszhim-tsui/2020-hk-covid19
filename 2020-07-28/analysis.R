library(pdftools)


# Get current time for later use
currenttime <- as.POSIXct(Sys.time(), tz=Sys.timezone())
attributes(currenttime)$tzone <- "Asia/Hong_Kong"

currenttimetext <- paste("最後更新於香港時間 ", format(currenttime, "%Y-%m-%d %H:%M"), sep="")




# download data file
download.file("https://www.chp.gov.hk/files/pdf/building_list_eng_20200728.pdf", "../00_original/building_list_eng_20200728.pdf")

# read pdf
current <- pdf_text("../00_original/building_list_eng_20200728.pdf")

# also get date in file name
currentdate <- stringr::str_extract("building_list_eng_20200728.pdf", "2020[0-9]+")
currentdate2 <- format(as.Date(currentdate, "%Y%m%d"), format="%b%d")
currentdate2 <- tolower(currentdate2)








# read in hk district
district <- read.csv("../00_original/district-population.csv")

# copy to a master
master <- district

# make a new column with the dat
master[, currentdate2] <- NA










# from pdf,
# read everything till end of first table
current2 <- current[1:grep("List of buildings with confirmed / probable cases visited from 2 days", current)-1]


# loop through each district
for (i in 1:dim(district)[1]){
	
	# since districts in PDF always appear after a line break
	# set up regex pattern to match
	targetdistrict <- paste("\\\n[ ]*", district$district_en[i], sep="")
	
	# grep all matches, then unlist to get matching locations
	tempcount <- gregexpr(targetdistrict, current2)
	tempcount <- unlist(tempcount)
	
	# note non-matches are -1
	
	# get num of district's cases
	numofcase <- sum(tempcount!=-1)
	
	# add num of case to district
	master[i, currentdate2] <- numofcase
	
}



# copy again
master_100k <- master

# calculate the per100k cases
master_100k[,4] <- round(master[,4]/master$pop*100000, 2)




# extract only the district names and the num of cases
case_raw <- master[, c(1,4)]
names(case_raw) <- c("District", "cases")

case_100k <- master_100k[, c(1,4)]
names(case_100k) <- c("District", "cases100k")





#######
# map #
#######

library(sf)
library(leaflet)


# read, then merge with numbers
districtmap <- st_read("../00_original/hksar_18_district_boundary.json")
districtmap <- merge(districtmap, case_raw, by="District")
districtmap <- merge(districtmap, case_100k, by="District")


# cf https://rstudio.github.io/leaflet/choropleths.html

bins_raw <- c(0, 5, 10, 15, 25, 40, 70, 100, Inf)
palette_raw <- colorBin("Reds", domain= districtmap$casesraw, bins=bins_raw)
label_raw <- sprintf("<strong>%s</strong><br/>過去14日有 %g 宗確診或疑似個案",
  							districtmap$地區, districtmap$cases) %>%
  						lapply(htmltools::HTML)


map_raw <- 
leaflet() %>%
	setView(114.167265, 22.360296, zoom=10) %>%
	addPolygons(data=districtmap, color="black", weight=2, dashArray="3",
				fillColor=~palette_raw(cases), fillOpacity=0.7, 
				highlight=highlightOptions(weight=5, dashArray = "", bringToFront = TRUE),
				label=label_raw) %>%
	addProviderTiles(providers$Esri.WorldTopoMap)







bins_100k <- c(0, 5, 10, 15, 25, 40, Inf)
palette_100k <- colorBin("Reds", domain=districtmap$cases100k, bins=bins_100k)
label_100k <- sprintf("<strong>%s</strong><br/>過去14日，每十萬人有 %g 宗確診或疑似個案",
  							districtmap$地區, districtmap$cases100k) %>%
  						lapply(htmltools::HTML)

map_100k <- 
leaflet() %>%
	setView(114.167265, 22.360296, zoom=10) %>%
	addPolygons(data=districtmap, color="black", weight=2, dashArray="3",
				fillColor=~palette_100k(cases100k), fillOpacity=0.7, 
				highlight=highlightOptions(weight=5, dashArray = "", bringToFront = TRUE),
				label=label_100k) %>%
	addProviderTiles(providers$Esri.WorldTopoMap)







##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../docs/", overwrite=T)

