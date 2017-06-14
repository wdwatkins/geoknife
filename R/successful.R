#' @title Convenience function for GDP process state
#' @rdname successful-methods
#' @aliases
#' successful
#' running
#' error
#' @usage
#' successful(.Object, retry)
#' error(.Object, retry)
#' running(.Object, retry)
#'
#' @param .Object a \linkS4class{geojob} object or geojob ID (character)
#' @param retry logical, attempt to retry again if communication failed with the server
#' @param returnPercent logical Also return job percent complete? 
#' @return TRUE/FALSE indicating if process is in the given state (error, processing, successful)
#' @description Simple wrapper to check process status
#'
#' @author Luke Winslow, Jordan S Read
#' @seealso \code{\link{check}}
#'
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism')
#' check(job)
#'
#' running(job)
#' error(job)
#' successful(job)
#' }
#'
#' @export
successful <- function(.Object, retry = FALSE){
  status_is(id(.Object), status = 'successful', retry = retry)
}

#' @rdname successful-methods
#' @aliases running
#' @param returnPercent logical Should percent complete also be returned?
#' @export
running <- function(.Object, retry = FALSE, returnPercent = FALSE){
  status_is(id(.Object), status = 'running', retry = retry, 
            returnPercent = returnPercent)
}

#' @rdname successful-methods
#' @aliases error
#' @export
error <- function(.Object, retry = FALSE){
  status_is(id(.Object), status = 'error', retry = retry)
}

status_is <- function(jobID, status, retry, returnPercent = FALSE){
  process = check(jobID)
  if (process$status == 'unknown' && retry){
    Sys.sleep(gconfig('sleep.time'))
    process = check(jobID)
  }
  state <- process$statusType
  #if option, return list with percent?
  statusReturn <- switch(status, 
                     error = state == "ProcessFailed",
                     running = state == "ProcessStarted" | state == "ProcessAccepted",
                     successful = state == "ProcessSucceeded")
  if(returnPercent) {
    statusReturn <- c(statusIs = statusReturn, percentComplete = process$percentComplete)
  }
  return(statusReturn)
}