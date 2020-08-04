# Note:
# all links to PDF files are hidden in https://www.chp.gov.hk/js/building_list_eng.js


library(stringr)


# "download" the JS file
pdf_js <- read.table("https://www.chp.gov.hk/js/building_list_eng.js")

# not too sure why
# but contents are actually under $V2

# extract all names with ".pdf"
pdf_list <- str_extract_all(pdf_js$V2, "/files/pdf/building_list_eng_[0-9]+(_[0-9]+)?.pdf")
pdf_list <- unlist(pdf_list)

# note two formats of file names
pdf_list[21]
pdf_list[120]

# test
str_extract(pdf_list[21], "/files/pdf/building_list_eng_[0-9]+(_[0-9]+)?.pdf")
str_extract(pdf_list[120], "/files/pdf/building_list_eng_[0-9]+(_[0-9]+)?.pdf")



# loop through list of PDFs
for(i in 1:length(pdf_list)){

	targetURL <- paste("https://www.chp.gov.hk", pdf_list$[i], sep="")
	
	targetfilename <- str_extract(pdf_list[i], "building_list_eng_[0-9]+(_[0-9]+)?.pdf")
	targetfilepath <- paste("../00_original/", targetfilename, sep="")

	download.file(targetURL, targetfilepath)
}



