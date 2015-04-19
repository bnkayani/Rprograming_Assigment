corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	co_r<-c()
	myfiles <-list.files(directory)
	fileid  <- tools::file_path_sans_ext(myfiles)
	file_num <- as.numeric(fileid)
	fullpath <- paste(directory, myfiles, sep="/")

	#comp<-complete(directory)

	for(file_num in 1:332){
		hf<-read.csv(fullpath[file_num],header=T)
		y<-complete.cases(hf)
		z<-sum(as.numeric(y))
		sulfate<-hf[y,2]
		nitrate<-hf[y,3]
	if(z>threshold){
		co_r<-c(co_r,cor(sulfate,nitrate))
		}
}
return(co_r)
}







