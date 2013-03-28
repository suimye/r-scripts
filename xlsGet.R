xlsGet <- function(path,fileFormat){
	print("Warning, this function is limited the reading about 600,000 columns from xls file!")
	require(gregmisc)

	f <- list.files(path)
	files <- grep(".xls",f, ignore.case = FALSE, perl = TRUE, value = TRUE)
	len <- length(files)
	if(len < 1 ){ print("Files not found"); break }
	fileList <- NULL
	fileName <- NULL

	for (i in 1:len){
			fileName <- cbind(fileName,sub(".xls","",files[i]))
			fileList[[i]] <- read.xls(paste(path,"/",files[i],sep=""))

	}
	names(fileList) <- fileName
	return(fileList);
}
