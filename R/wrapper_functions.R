# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

url = "http://keynumbers.com/api/"

kn.getCollection <- function(coll, size=25, page=1) {
  if (exists("coll"))
    url <- paste0(url, "collection/", coll, "?size=", size, "&page=", page)
  else
    stop(paste('argument "coll" is missing, with no default'))

  kn_key <- Sys.getenv("kn_key")
  if (nchar(kn_key)==0)
    stop("Could not retreive Keynumbers API key. Please set it in ~/.Renviron and restart R.")

  print(paste("GET", url))
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", kn_key, sep = " ")))

  if (res$status_code == 200)
    result <- content(res)
  else
    stop("Could not retreive Keynumbers API key. Please set it in ~/.Renviron and restart R.")

  for (I in 1:length(result$keynumbers$dividends)) {
    result$keynumbers$dividends[[I]]$date <- as.POSIXct(result$keynumbers$dividends[[I]]$date)
  }

  for (I in 1:length(result$dates))
    result$dates[[I]]$date = as.POSIXct(result$dates[[I]]$date)

  result
}
#a<-kn.getCollection(coll="sydney-water-consumption-3023")

kn.getModel <- function(modelname) {
  if (exists("modelname"))
    url <- paste0(url, "model/", modelname)
  else
    stop(paste('argument "modelname" is missing, with no default'))

  kn_key <- Sys.getenv("kn_key")
  if (nchar(kn_key)==0)
    stop("Could not retreive the Keynumbers API key. Please set it.")

  print(paste("GET", url))

  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", kn_key, sep = " ")))

  if (res$status_code == 200)
    result <- content(res)
  else
    stop(paste("Status code:", res$status_code), ", Message: ", content(res)$message)

  for (I in 1:length(result$data$segments)){
    result$data$segments[[I]]$dividend$number <- as.numeric(result$data$segments[[I]]$dividend$number)
    result$data$segments[[I]]$divisor$number <- as.numeric(result$data$segments[[I]]$divisor$number)
    result$data$segments[[I]]$dividend$date <- as.POSIXct(result$data$segments[[I]]$dividend$date)
    result$data$segments[[I]]$dividend$decimal_points <- as.numeric(result$data$segments[[I]]$dividend$decimal_points)
    result$data$segments[[I]]$dividend$min <- as.numeric(result$data$segments[[I]]$dividend$min)
    result$data$segments[[I]]$dividend$max <- as.numeric(result$data$segments[[I]]$dividend$max)
  }

  result
}
#a<-kn.getModel(model="sydney-water-usuage-1879")

#Returns a numerical value
kn.modelExec <- function(model) {
  formula = model$data$formula

  #Append '1' into variable names stored in the formula
  nul <- sapply(1:length(LETTERS), function(x) {
    formula <<- gsub(LETTERS[x], paste0(LETTERS[x],"1"), formula)
  })

  segments = model$data$segments

  for (x in 1:length(segments)) {
    out <- as.numeric(segments[[x]]$dividend$number)/as.numeric(segments[[x]]$divisor$number)
    assign(paste0(LETTERS[x],"1"), out)
  }

  eval(parse(text = formula))
}

kn.coll2df <- function(coll){

  fullcoll <- lapply(coll$keynumbers$dividends, function(x) {
    data.frame(number=x$number, name=x$name, location=x$location, date=x$date,unit=x$unit)
  })
  fullcoll <- do.call(rbind, fullcoll)
  fullcoll$date <- as.POSIXct(fullcoll$date)
  fullcoll
}

kn.modelSegmentColNames <- function(model) {
  out <- sapply(model$data$segments, function(x) x$dividend$collection_id)
  out <- as.character(out)
  names(out) <- LETTERS[1:length(out)]
  out
}

#Replicates the model as many times as there is data in the collection of the segment
kn.modelRep <- function(model, segment_no) {

  collection_id <- model$data$segments[[segment_no]]$dividend$collection_id
  collection <- kn.getCollection(collection_id)

  coll_length = length(collection$keynumbers$dividends)

  Models <- lapply(1:coll_length, function(x) model)

  for (x in 1:coll_length) {
    Models[[x]]$data$segments[[segment_no]]$dividend$number = collection$keynumbers$dividends[[x]]$number
    Models[[x]]$data$segments[[segment_no]]$dividend$unitc = collection$keynumbers$dividends[[x]]$unitc
    Models[[x]]$data$segments[[segment_no]]$dividend$id = collection$keynumbers$dividends[[x]]$id
    Models[[x]]$data$segments[[segment_no]]$dividend$dataset_id = collection$keynumbers$dividends[[x]]$dataset_id
    Models[[x]]$data$segments[[segment_no]]$dividend$location = collection$keynumbers$dividends[[x]]$location
    Models[[x]]$data$segments[[segment_no]]$dividend$date = collection$keynumbers$dividends[[x]]$date
  }
  Models
}