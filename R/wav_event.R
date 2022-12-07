wav_keep_version_0 <- F

wav_event <- function (filename, offset, data=T) {
  header.size <- 20
  to_32 <- function (v) v[1] + 65536 * v[2]

  f <- cpp_read_sndfile (filename, from=offset, to=offset + header.size - 1)
  if (0xFFFF != f$data[1]) stop("marker not found")
  if (header.size != f$data[2]) stop("wrong header size")
  counter <- to_32 (f$data[3:4])
  time.tag <- to_32 (f$data[5:6])
  n.samples <- to_32 (f$data[7:8])
  cpu.time.ms <- to_32 (f$data[9:10])
  n.channels <- f$data[11]
  version <- f$data[12] / 100

  # keep lagacy data
  if (wav_keep_version_0 && !is.null(f$date) && as.Date(f$date, "%c") < as.Date("2016-08-24")) {
    n.channels <- 1
    version <- 0
  } else if (version == 0) version = 1

  data.size <- if (version >= 2) to_32 (f$data[13:14]) else n.channels * n.samples
  
  v <- list(counter=counter, time.tag=time.tag, n.samples=n.samples, n.channels=n.channels, cpu.time.ms=cpu.time.ms, version=version, 
	    offset=offset, header.size=header.size, data.size=data.size, sample.rate=f$sample_rate, sample.nbits=f$sample_nbits, date=f$date, raw.data=f$data)


  if (data) {
    f <- cpp_read_sndfile (filename, from=offset + header.size, to=offset + header.size + data.size - 1, metadata=F)
    if (n.channels == 1) {
      if (version < 2) v$data <- list('1' = f$data)
      else {
	v$data <- list(tail(f$data, -4))
	names(v$data) <- as.character(f$data[1])
      }
    } else {
      if (version < 2) v$data <- split(f$data, rep(1:n.channels, each=n.samples))
      else {
	v$data <- vector("list", length = n.channels)
	idx <- 1
	for (i in seq(n.channels)) {
	  id <- f$data[idx]
	  n <- f$data[idx + 1]
	  v$data[[i]] <- f$data[seq(idx + 5, idx + n)]
	  names(v$data)[i] <- as.character(id)
	  idx <- idx + 4 + n
	}
      }
    }
  }
  v
}

wav_entries <- function (filename) {
  if(!file.exists(filename)) stop(paste("file", filename, "not present"))

  f <- cpp_read_sndfile (filename, to=0)
  total.size <- f$samples
  index <- rep(0, 10000)
  offset <- 1
  n <- 0
  while (offset < total.size) {
    e <- wav_event(filename, offset, data=F)
    n <- n + 1
    if (n >= length(index)) index <- c(index, rep(0, n * 10))
    index[n] <- offset
    offset <- offset + e$header.size + e$data.size
  }
  attr(n, "index") <- index[1:n]
  n
}

wav_apply <- local({ 
  # environment is locked in package, global assignement <<- does not work
  # this solution creates a sub-environment for wav_apply
  #   (https://stackoverflow.com/questions/59126349/emulating-static-variable-within-r-functions)
  # alternativelly an explicit environment could be created 
  #   (https://adv-r.hadley.nz/environments.html)
  #   .e <- new.env(parent = emptyenv()); .e$indexes <- list()
  # or using a global environment dot variable 
  #   (https://community.rstudio.com/t/how-to-use-modify-a-dataset-internal-to-an-r-package/84577)
  #   .onLoad <- function(...) assign(".indexes", list(), envir = globalenv())
  indexes <- list()
    function (filename, index=NULL, start=1, entries=Inf, data=T, apply_fun=sapply, fun, ...) {
    if (is.null(index)) index <- indexes[[filename]]
    else if (is.na(index)) index <- NULL

    if (is.null(index)) {
      cat("Building index\n")
      t <- wav_entries (filename)
      index <- attr(t, "index")
    } 
    total_entries <- length(index)


    end <- ifelse(is.infinite(entries), total_entries, ifelse(entries > 0, min(start + entries - 1, total_entries), total_entries + entries))
		  
    if (end < start) return()
    cat("Processing", end - start + 1, "entries from", filename, "\n")

    process_event <- function (id, indexes, filename, data, fun, ...) fun(wav_event(filename, indexes[id], data), id=id,...)

    k <- apply_fun(start:end, process_event, index, filename, data, fun, ...)


    indexes[[filename]] <<- index

    attr(k, "index") <- index
    k
}})

unroll_timetag <- function (time.tag, cpu.time, tag.step=8e-9) {
  d <- data.frame(tag=time.tag, cpu=cpu.time)
  d$rt <- diff(c(0, d$cpu))
  d$dt <- diff(c(0, d$tag))
  d$dt[d$dt < 0] <- 2**31 + d$dt[d$dt < 0]
  d$dt <- d$dt * tag.step
  d$dt[d$rt > 5] <- d$rt[d$rt > 5]
  t <- cumsum(d$dt)
  t
}

analyze <- function (file, fun, entries=0, parallel=T, ...) {
  f_ <- function (x, id, ...)  {
    h <- c(id=id, time=x$time.tag, cpu.time=x$cpu.time.ms * 1e-3, dt=NA)
    r <- fun(x, ...)
    c(h, r)
  }
  
  v <- wav_apply(file, entries=entries, data=T, apply_fun=ifelse(parallel,  mclapply, lapply), fun=f_, ...)
  if (exists("rbindlist")) d <- rbindlist(v) #, fill=T)
  else d <- do.call(rbind.data.frame, v)

  d$name <- file
  d$time <- unroll_timetag(d$time, d$cpu.time)
  d$dt <- c(0, diff(d$time))
  d$time <- d$time + as.POSIXct(wav_event(file, offset=1, data=F)$date, format="%c")
  d$cpu.time <- NULL
  d$index <- attr(v, "index")[d$id]
  d$unique <- diff(c(0, d$id)) > 0

  d
}
