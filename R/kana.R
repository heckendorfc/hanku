#' kanagrid
#' 
#' @export
kanagrid <- function(){
	gr <- data.frame(x=c(sapply(11:5,FUN=function(x)rep(x,5)),rep(4,3),rep(3,5),2,2,1),
					 y=c(rep(5:1,7),5,3,1,5:1,5,1,5))
	gr
}

#' kanasample
#' 
#' @param ro
#' TODO
#' @param kana
#' TODO
#' @param size
#' TODO
#' @param reduce
#' TODO
#' @param exclude
#' TODO
#' 
#' @export
kanasample <- function(ro,kana,size,reduce,exclude){
	if(length(kana)!=length(ro)){
		stop("length mismatch")
	}

	ttt <- 1:length(kana)
	if(!is.null(reduce)){
		ttr <- ttt[kana %in% reduce]
		i1 <- sample(ttr,size/2,replace=F)
	} else{
		ttr <- integer(0)
		i1 <- integer(0)
	}

	if(!is.null(exclude)){
		ttt <- ttt[!(kana %in% exclude)]
	}
	ttt <- setdiff(ttt,ttr)

	if(length(ttt)<size)
		ttt <- c(ttt,sample(ttr,size-length(ttt),replace=F))

	i2 <- sample(ttt,size,replace=F)
	ic <- c(i1,i2)
	i <- sample(ic,length(ic),replace=F)

	return(i)
}

#' trial.hiragana
#' 
#' @param time
#' TODO
#' @param reduce
#' TODO
#' @param exclude
#' TODO
#' 
#' @export
trial.hiragana <- function(time=30,reduce=NULL,exclude=NULL){
	ro <- romaji
	hi <- hiragana

	res <- NULL

	times <- integer(0)
	si <- file("stdin")
	while(sum(times)<time){
		i <- kanasample(ro,hi,5,reduce,exclude)
		bat <- do.call("rbind",lapply(i,FUN=function(xi){
			print(hi[xi])
			start.time <- as.numeric(format(Sys.time(),"%OS3"))
			te <- readLines(si,1)
			end.time <- as.numeric(format(Sys.time(),"%OS3"))
			if(end.time<start.time) end.time <- end.time+60
			return(data.frame(ind=xi,guess=te,time=end.time-start.time))
		}))
		res <- rbind(res,bat)
		times <- res$time
	}
	close(si)

	mt <- sapply(1:length(hi),FUN=function(x){
			tin <- which(res$ind==x)
			if(length(tin)==0){ return(0) }
			mean(times[tin])
		})

	errors <- rep(0,length(hi))
	ans <- res$guess!=ro[res$ind]
	if(length(which(ans))>0){
		errors[ans] <- 1
		print(data.frame(
						  kana=hi[res$ind[ans]],
						  right=ro[res$ind[ans]],
						  wrong=res$guess[ans]))
	}

	hier <- sapply(1:length(hi),FUN=function(x){
			tin <- which(res$ind==x)
			if(length(tin)==0){ return(0) }
			sum(errors[tin])
		})


	gdf <- kanagrid()
	tdf <- data.frame(kana=hi,time=mt,errors=hier)
	df <- cbind(gdf,tdf)

	return(df)
}

#' @export
plot.trial <- function(x, ...){
	y <- errors <- kana <- NULL;rm(y, errors, kana)
	
	mt <- mean(x$time[which(x$time>0)])
	title <- paste("Kana trial. Mean character time (s):",mt,sep="",collapse="")

	ggplot(data=x,aes(x=x,y=y))+
	geom_tile(aes(fill=time),color="white") + scale_fill_gradient(low = "white",high = "orange") +
	geom_text(aes(label=kana,size=errors)) + scale_size(range=c(4,8)) +
	labs(title = title)
}

#' flash.hiragana
#' 
#' @param size
#' TODO
#' @param reduce
#' TODO
#' @param exclude
#' TODO
#' 
#' @export
flash.hiragana <- function(size=10,reduce=NULL,exclude=NULL){
	ro <- romaji
	hi <- hiragana

	i <- kanasample(ro,hi,size,reduce,exclude)

	print(hi[i])
	start.time <- as.integer(format(Sys.time(),"%s"))
	si <- file("stdin")
	te <- readLines(si,1)
	close(si)
	te <- unlist(strsplit(te," "))

	ans <- te!=ro[i]
	if(length(which(ans))>0){
		return(data.frame(
						  kana=hi[i[ans]],
						  right=ro[i[ans]],
						  wrong=te[ans]))
	} else{
		end.time <- as.integer(format(Sys.time(),"%s"))
		return(paste("OK! elapsed time (s):",end.time-start.time,collapse=" "))
	}
}
