library(pdftools)
library(stringr)
library(plotly)


# Get current time for later use
currenttime <- as.POSIXct(Sys.time(), tz=Sys.timezone())
attributes(currenttime)$tzone <- "Asia/Hong_Kong"
currenttimetext <- paste("最後更新於香港時間 ", format(currenttime, "%Y-%m-%d %H:%M"), sep="")


# download latest data file
download.file("https://www.chp.gov.hk/files/pdf/building_list_eng_20210411.pdf", "../00_original/building_list_eng_20210411.pdf")


# remember to manually check if this is the correct separator
# "List of buildings with confirmed / probable cases visited from 2 days before onset"


# read in all pdf names 
pdflist <- dir(path="../00_original/", pattern=".pdf")


###################################
# preparation for info extraction #
###################################

# read in hk district
district <- read.csv("../00_original/district-population.csv")

# set up empty df for 2-week numbers
master2wk <- data.frame(district_en = NULL, day = NULL, case = NULL)




##########################
# actual info extraction #
##########################



# from building_list_eng_20200123_184843.pdf == pdflist[1]
# to building_list_eng_20200212_202406.pdf == pdflist[18]
for (i_file in 1:18){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}


table(master2wk$day)







# from building_list_eng_20200213_000000.pdf == pdflist[19]
# to building_list_eng_20200216_220021.pdf == pdflist[22]
# need to remove all text after
# "\n List of non-residential building with 2 or more confirmed cases of novel coronavirus infection\n"


for (i_file in 19:22){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	temptext <- gsub("\n List of non-residential building with 2 or more confirmed cases of novel coronavirus infection\n.*", "", temptext)


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}










# only on building_list_eng_20200217_231207.pdf == pdflist[23]
targetfile <- paste("../00_original/", pdflist[23], sep="")
temptext <- pdf_text(targetfile)
temptext <- temptext[1:grep("List of non-residential buildings with confirmed cases visited after onset of symptoms", temptext)-1]
tempdate <- stringr::str_extract(pdflist[23], "2020[0-9]+")
tempdate <- format(as.Date(tempdate, "%Y%m%d"))


for (i_district in 1:dim(district)[1]){

  # since districts in PDF always appear after a line break
  # set up regex pattern to match

  if (district$district_en[i_district] == "Sha Tin"){
    targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
  } else if (district$district_en[i_district] == "Central & Western") {
    targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
  } else {
    targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
  }

  # grep all matches, then unlist to get matching locations
  tempcount <- gregexpr(targetdistrict, temptext)
  tempcount <- unlist(tempcount)

  # note non-matches are -1

  # get num of district's cases
  numofcase <- sum(tempcount!=-1)

  # cbind required information, then rbind it to the df master2wk 
  temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
          day = tempdate, 
          case = numofcase)
  
  master2wk <- rbind(master2wk, temprow)
}









# from building_list_eng_20200218_215230.pdf == pdflist[24]
# to building_list_eng_20200219_173655.pdf == pdflist[25]
	
for (i_file in 24:25){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed cases visited after onset of symptoms or", temptext)-1]


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}










# from building_list_eng_20200220_224016.pdf == pdflist[26]
# to building_list_eng_20200221_213153.pdf == pdflist[27]

for (i_file in 26:27){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	temptext <- temptext[1:grep("List of buildings with probable/confirmed cases visited after onset of symptoms", temptext)-1]


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}










# for building_list_eng_20200222_000000.pdf == pdflist[28]

targetfile <- paste("../00_original/", pdflist[28], sep="")
temptext <- pdf_text(targetfile)
temptext <- temptext[1:grep("List of buildings with probable/confirmed cases visited from 2 days before onset of symptoms", temptext)-1]
tempdate <- stringr::str_extract(pdflist[28], "2020[0-9]+")
tempdate <- format(as.Date(tempdate, "%Y%m%d"))


for (i_district in 1:dim(district)[1]){

  # since districts in PDF always appear after a line break
  # set up regex pattern to match

  if (district$district_en[i_district] == "Sha Tin"){
    targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
  } else if (district$district_en[i_district] == "Central & Western") {
    targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
  } else {
    targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
  }

  # grep all matches, then unlist to get matching locations
  tempcount <- gregexpr(targetdistrict, temptext)
  tempcount <- unlist(tempcount)

  # note non-matches are -1

  # get num of district's cases
  numofcase <- sum(tempcount!=-1)

  # cbind required information, then rbind it to the df master2wk 
  temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
          day = tempdate, 
          case = numofcase)
  
  master2wk <- rbind(master2wk, temprow)
}










# List of buildings with confirmed cases visited from 2 days before onset of symptoms

# building_list_eng_20200223_225143.pdf == pdflist[29]
# building_list_eng_20200310_225716.pdf == pdflist[45]

for (i_file in 29:45){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed cases visited from 2 days before onset of symptoms", temptext)]
	temptext <- gsub("List of buildings with confirmed cases visited from 2 days before onset of symptoms.*", "", temptext)


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}









# List of buildings with confirmed / probable cases visited from 2 days
# from building_list_eng_20200311_223130.pdf == pdflist[46]
# to building_list_eng_20200326_000000.pdf == pdflist[61]


for (i_file in 46:61){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed / probable cases visited from 2 days", temptext)-1]


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}





# "List of buildings with confirmed / probable cases visited"
# from building_list_eng_20200327_234929.pdf == pdflist[62]
# to building_list_eng_20200425_000000.pdf == pdflist[91]


for (i_file in 62:91){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")
	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed / probable cases visited", temptext)-1]


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}










# "List of buildings visited by confirmed / probable cases"
# from building_list_eng_20200426_000000.pdf == pdflist[92]
# to building_list_eng_20200428.pdf == pdflist[94]


for (i_file in 92:94){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of buildings visited by confirmed / probable cases", temptext)]
	temptext <- gsub("List of buildings visited by confirmed / probable cases.*", "", temptext)


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}




# "List of Buildings Visited by Confirmed / Probable Cases"
# from building_list_eng_20200429.pdf == pdflist[95]
# to building_list_eng_20200707.pdf == pdflist[164]

for (i_file in 95:164){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of Buildings Visited by Confirmed / Probable Cases", temptext)]
	temptext <- gsub("List of Buildings Visited by Confirmed / Probable Cases.*", "", temptext)

	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}









# "List of buildings with confirmed / probable cases visited from 2 days"
# from building_list_eng_20200708.pdf == pdflist[165]
# to building_list_eng_20200829.pdf == pdflist[217]


for (i_file in 165:217){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed / probable cases visited from 2 days", temptext)]
	temptext <- gsub("List of buildings with confirmed / probable cases visited from 2 days.*", "", temptext)

	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}




# "List of buildings with confirmed / probable cases visited from 2 days before onset"
# from building_list_eng_20200830.pdf == pdflist[218]
# to building_list_eng_20201231.pdf == pdflist[340]

for (i_file in 218:340){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "2020[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed / probable cases visited from 2 days before onset", temptext)]
	temptext <- gsub("List of buildings with confirmed / probable cases visited from 2 days before onset.*", "", temptext)


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}






# "List of buildings with confirmed / probable cases visited from 2 days before onset"
# from building_list_eng_20200830.pdf == pdflist[218]
# to current

for (i_file in 341:length(pdflist)){
	
	# loop through each target file, also retrive date from file name
	targetfile <- paste("../00_original/", pdflist[i_file], sep="")

	temptext <- pdf_text(targetfile)
	tempdate <- stringr::str_extract(pdflist[i_file], "202[0-9]+")
	tempdate <- format(as.Date(tempdate, "%Y%m%d"))

	# remove text from second, irrelevant table
	# step 1, remove pages after the beginning of the table
	# step 2, remove text after the title of the irrelevant table
	temptext <- temptext[1:grep("List of buildings with confirmed / probable cases visited from 2 days before onset", temptext)]
	temptext <- gsub("List of buildings with confirmed / probable cases visited from 2 days before onset.*", "", temptext)


	# loop through each district	
	for (i_district in 1:dim(district)[1]){
	
		# since districts in PDF always appear after a line break
		# set up regex pattern to match

		if (district$district_en[i_district] == "Sha Tin"){
			targetdistrict <- paste("\\\n[ ]*", "(Shatin|Sha Tin)", sep="")
		} else if (district$district_en[i_district] == "Central & Western") {
			targetdistrict <- paste("\\\n[ ]*", "(Central|Central & Western)", sep="")
		} else {
			targetdistrict <- paste("\\\n[ ]*", district$district_en[i_district], sep="")
		}
	
		# grep all matches, then unlist to get matching locations
		tempcount <- gregexpr(targetdistrict, temptext)
		tempcount <- unlist(tempcount)
	
		# note non-matches are -1
	
		# get num of district's cases
		numofcase <- sum(tempcount!=-1)
	
		# cbind required information, then rbind it to the df master2wk 
		temprow <- cbind(district_en = as.character(district$district_en[i_district]), 
						day = tempdate, 
						case = numofcase)
		
		master2wk <- rbind(master2wk, temprow)

	}
}




















#############################
# done with data extraction #
#############################

write.csv(master2wk, "hk-covid19-2wk.csv", row.names=F)



###################################
# data wrangle / summary for plot # 
###################################

master2wk$case <- as.numeric(as.character(master2wk$case))
master2wk$day <- as.Date(master2wk$day)

masterday <- master2wk
masterday$case <- round(masterday$case/14, 3)
masterday <- merge(masterday, district, by="district_en", all.x=T)
masterday$case100k <- round(masterday$case / masterday$pop*100000, 2)

# aesthetics
masterday <- masterday[order(masterday$day, masterday$district_en), ]
masterday <- masterday[, c(1, 4, 5, 2, 3, 6)]

day_earliest <- min(masterday$day)
day_latest <- max(masterday$day)


# find latest PDF name
# extract data for map
pdflist[length(pdflist)]
latestdate <- stringr::str_extract(pdflist[length(pdflist)], "202[0-9]+")
latestdate <- format(as.Date(latestdate, "%Y%m%d"))

master2wk_latest <- subset(master2wk, master2wk$day == latestdate)
master2wk_latest <- merge(master2wk_latest, district, by="district_en")
master2wk_latest$case100k <- round(master2wk_latest$case / master2wk_latest$pop * 100000, 3)

master2wk_latest$day <- NULL
master2wk_latest$district_ch <- NULL
master2wk_latest$pop <- NULL

names(master2wk_latest) <- c("District", "case", "case100k")



head(masterday)


#########
# plots #
#########

fig_day <- 
plot_ly() %>%
	add_trace(data=masterday, type="scatter", mode="lines", 
		x=~day, y=~case, color=~district_ch) %>%
	layout(title=list(text="14日平均每日新增新型肺炎個案", y=0.99),
		xaxis=list(title="日期", range=c(day_earliest-1, day_latest +1)),
		yaxis=list(title="14日平均新增新型肺炎個案"), 
		legend=list(x=0.025, y=0.975))
		
fig_100k <- 
plot_ly() %>%
	add_trace(data=masterday, type="scatter", mode="lines", 
		x=~day, y=~case100k, color=~district_ch) %>%
	layout(title=list(text="14日平均每日每十萬人新增新型肺炎個案", y=0.99),
		xaxis=list(title="日期", range=c(day_earliest-1, day_latest +1)),
		yaxis=list(title="14日平均每十萬人新增新型肺炎個案"), 
		legend=list(x=0.025, y=0.975))









#######
# map #
#######

library(sf)
library(leaflet)


# read, then merge with numbers
districtmap <- st_read("../00_original/hksar_18_district_boundary.json")
districtmap <- merge(districtmap, master2wk_latest, by="District")


# cf https://rstudio.github.io/leaflet/choropleths.html

bins_raw <- c(0, 1, 2, 5, 10, 15, 70, 100, Inf)
palette_raw <- colorBin("Reds", domain=districtmap$case, bins=bins_raw)
label_raw <- sprintf("<strong>%s</strong><br/>過去14日有 %g 宗確診或疑似個案",
  							districtmap$地區, districtmap$case) %>%
  						lapply(htmltools::HTML)


map_raw <- 
leaflet() %>%
	setView(114.167265, 22.360296, zoom=10) %>%
	addPolygons(data=districtmap, color="black", weight=2, dashArray="3",
				fillColor=~palette_raw(case), fillOpacity=0.7, 
				highlight=highlightOptions(weight=5, dashArray = "", bringToFront = TRUE),
				label=label_raw) %>%
	addProviderTiles(providers$Esri.WorldTopoMap)







bins_100k <- c(0, 1, 2.5, 5, 10, 25, Inf)
palette_100k <- colorBin("Reds", domain=districtmap$case100k, bins=bins_100k)
label_100k <- sprintf("<strong>%s</strong><br/>過去14日，每十萬人有 %g 宗確診或疑似個案",
  							districtmap$地區, districtmap$case100k) %>%
  						lapply(htmltools::HTML)

map_100k <- 
leaflet() %>%
	setView(114.167265, 22.360296, zoom=10) %>%
	addPolygons(data=districtmap, color="black", weight=2, dashArray="3",
				fillColor=~palette_100k(case100k), fillOpacity=0.7, 
				highlight=highlightOptions(weight=5, dashArray = "", bringToFront = TRUE),
				label=label_100k) %>%
	addProviderTiles(providers$Esri.WorldTopoMap)







##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../docs/", overwrite=T)










