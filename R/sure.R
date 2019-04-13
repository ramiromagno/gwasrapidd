#' Stop R execution without triggering an error
#'
#' This function stops execution quietly.
#'
#' @keywords internal
stop_quietly <- function() {
  # https://stackoverflow.com/questions/14469522/stop-an-r-program-without-error
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' Are you sure?
#'
#' This function asks you interactively for permission to continue or not.
#' You can specify a custom message before the question and also different
#' messages for a both a positive and negative answer.
#'
#' @param before_question String with message to be printed before question.
#' @param after_saying_no String with message to be printed after answering 'no'.
#' @param after_saying_yes String with message to be printed after answering 'yes'.
#' @param default_answer String with answer to question, if run in
#'   non-interactive mode.
#'
#' @return A logical indicating if answer was 'yes'/'y' (TRUE) or otherwise
#'   (FALSE).
#' @keywords internal
sure <- function(before_question = NULL, after_saying_no = NULL, after_saying_yes = NULL, default_answer = NULL) {

  # If default_answer is set then assume that we are running in non-interactive mode.
  # Return TRUE is default_answer == 'y' or FALSE otherwise.
  if(!rlang::is_null(default_answer)) {
    ans <- tolower(default_answer)
    return(identical(ans, "y") || identical(ans, "yes"))
  }

  if (!rlang::is_null(before_question)) {
    message(before_question)
    utils::flush.console()
  }

  answer <- tolower(readline("Do you still want to proceed (y/n)? "))

  if (identical(answer, "yes") || identical(answer, "y")) {
    if (!rlang::is_null(after_saying_yes)) {
      message(after_saying_yes)
      utils::flush.console()
    }
    return(invisible(TRUE))
  } else {

  if (!rlang::is_null(after_saying_no)) {
    message(after_saying_no)
    utils::flush.console()
  }
    return(invisible(FALSE))
  }
}
