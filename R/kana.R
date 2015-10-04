#' kanagrid
#' 
#' @export
kanagrid <- function(){
	gr <- data.frame(x=c(sapply(11:5,FUN=function(x)rep(x,5)),rep(4,3),rep(3,5),2,2,1),
					 y=c(rep(5:1,7),5,3,1,5:1,5,1,5))
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

#' plot.trial
#' 
#' @param x
#' TODO
#' 
#' @export
plot.trial <- function(x, ...){
	y <- errors <- kana <- NULL;rm(y, errors, kana)

	if(length(x$x)==0){
		if(nrow(x)>=11){
			rows <- floor(nrow(x)/11)
			x$x <- rep(1:11,rows)
			x$y <- c(sapply(1:rows,FUN=function(rn)rep(rn,11)),rep(rows+1,nrow(x)%%11))
		}else{
			x$x <- 1:nrow(x)
			rep(1,nrow(x))
		}
	}
	
	mt <- mean(x$time[which(x$time>0)])
	title <- paste("Kana trial. Mean character time (s):",mt,sep="",collapse="")

	ggplot(data=x,aes(x=x,y=y))+
	geom_tile(aes(fill=time),color="white") + scale_fill_gradient(low = "white",high = "orange") +
	geom_text(aes(label=kana,size=errors)) + scale_size(range=c(4,8)) +
	labs(title = title)
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
trial.kana <- function(time=30,kana=NULL,romaji=NULL,reduce=NULL,exclude=NULL){
	res <- NULL

	times <- integer(0)
	si <- file("stdin")
	while(sum(times)<time){
		i <- kanasample(ro,kana,5,reduce,exclude)
		bat <- do.call("rbind",lapply(i,FUN=function(xi){
			print(kana[xi])
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

	mt <- sapply(1:length(kana),FUN=function(x){
			tin <- which(res$ind==x)
			if(length(tin)==0){ return(0) }
			mean(times[tin])
		})

	errors <- rep(0,length(kana))
	ans <- res$guess!=ro[res$ind]
	if(length(which(ans))>0){
		errors[ans] <- 1
		print(data.frame(
						  kana=kana[res$ind[ans]],
						  right=ro[res$ind[ans]],
						  wrong=res$guess[ans]))
	}

	hier <- sapply(1:length(kana),FUN=function(x){
			tin <- which(res$ind==x)
			if(length(tin)==0){ return(0) }
			sum(errors[tin])
		})

	tdf <- data.frame(kana=kana,time=mt,errors=hier)

	return(tdf)
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
trial.katakana <- function(time=30,reduce=NULL,exclude=NULL){
	ro <- romaji
	ka <- katakana
	res <- trial.kana(time=time,romaji=ro,kana=ka,reduce=reduce,exclude=exclude)
	gdf <- kanagrid()
	cbind(gdf,res)
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
	res <- trial.kana(time=time,romaji=ro,kana=hi,reduce=reduce,exclude=exclude)
	gdf <- kanagrid()
	cbind(gdf,res)
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
flash.kana <- function(size=10,romaji=NULL,kana=NULL,reduce=NULL,exclude=NULL){
	i <- kanasample(romaji,kana,size,reduce,exclude)

	print(kana[i])
	start.time <- as.integer(format(Sys.time(),"%s"))
	si <- file("stdin")
	te <- readLines(si,1)
	close(si)
	te <- unlist(strsplit(te," "))

	ans <- te!=romaji[i]
	if(length(which(ans))>0){
		return(data.frame(
						  kana=kana[i[ans]],
						  right=romaji[i[ans]],
						  wrong=te[ans]))
	} else{
		end.time <- as.integer(format(Sys.time(),"%s"))
		return(paste("OK! elapsed time (s):",end.time-start.time,collapse=" "))
	}
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
	flash.kana(size=size,romaji=ro,kana=hi,reduce=reduce,exclude=exclude)
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
flash.katakana <- function(size=10,reduce=NULL,exclude=NULL){
	ro <- romaji
	ka <- katakana
	flash.kana(size=size,romaji=ro,kana=ka,reduce=reduce,exclude=exclude)
}
