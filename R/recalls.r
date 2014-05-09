recalls <-
function(
    query = NULL,
    organization = NULL,
    start_date = NULL,
    end_date = NULL,
    food_type = NULL,
    upc = NULL,
    make = NULL,
    model = NULL,
    year = NULL,
    code = NULL,
    page = 1,
    per_page = 50,
    sort = NULL,
    ...){
    
    baseurl <- 'http://api.usa.gov/recalls/search.json'
    
    once <- function(a){
        h <- basicTextGatherer()
        curlPerform(url = paste(baseurl, a, sep='?'),
                    followlocation = 1L, writefunction=h$update)
        response <- h$value()
        out <- fromJSON(response)[[1]]
        return(out)
    }
    
    arg <- as.list(match.call())[-1]
    arg <- list()
    if(!is.null(query)) arg$query <- query
    if(!is.null(organization))
        arg$organization <- curlEscape(paste(organization,collapse='+'))
    if(!is.null(start_date)) arg$start_date <- start_date
    if(!is.null(end_date)) arg$end_date <- end_date
    if(!is.null(page)) arg$page <- page
    if(!is.null(per_page)) arg$per_page <- per_page
    if(!is.null(sort)) arg$sort <- sort
    if(!is.null(food_type)) arg$food_type <- food_type
    if(!is.null(upc)) arg$upc <- upc
    if(!is.null(make)) arg$make <- make
    if(!is.null(model)) arg$model <- model
    if(!is.null(year)) arg$year <- year
    if(!is.null(code)) arg$code <- code
    
    if('page' %in% names(arg)) {
        #  return requested page
        arg <- paste(names(arg), arg, sep='=', collapse='&')
        out <- once(arg)
        total <- out$total
        out <- out$results
        out <- lapply(out, `class<-`, 'recall')
        attr(out,'total') <- total
        return(out)
    } else {
        # return all pages
        arg$per_page <- 50
        arg$page <- 1
        tmparg <- paste(names(arg), arg, sep='=', collapse='&')
        first <- once(tmparg)
        pages <- ceiling(first$total/50)
        if(pages>20){
            message('Total pages needed to return all results is ', pages,
                    ' but only first 20 pages can be returned.')
        }
        out <- lapply(2:min(pages,20), function(x) {
            arg$page <- 1
            tmparg <- paste(names(arg), arg, sep='=', collapse='&')
            once(tmparg)$results
        })
        out <- c(first$results,unlist(out,recursive=FALSE))
        out <- lapply(out, `class<-`, 'recall')
        attr(out,'total') <- first$total
        return(out)
    }
     
    
}
