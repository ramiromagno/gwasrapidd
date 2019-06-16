#' Are you sure?
#'
#' This function asks you interactively for permission to continue or not. You
#' can specify a custom message before the question and also different messages
#' for a both a positive and negative answer.
#'
#' If you run this function in non-interactive mode, you should pass an
#' automatic answer to \code{default_answer}: \code{'yes'} or \code{'no'}.
#'
#' @param before_question String with message to be printed before question.
#' @param after_saying_no String with message to be printed after answering
#'   \code{'no'}.
#' @param after_saying_yes String with message to be printed after answering
#'   \code{'yes'}.
#' @param default_answer String with answer to question, if run in
#'   non-interactive mode.
#'
#' @return A logical indicating if answer was \code{'yes'}/\code{'y'}
#'   (\code{TRUE}) or otherwise (\code{FALSE}).
#' @keywords internal
sure <- function(before_question = NULL,
                 after_saying_no = NULL,
                 after_saying_yes = NULL,
                 default_answer = NULL) {

  # If default_answer is set then assume that we are running in non-interactive
  # mode. Return TRUE is default_answer == 'y' or FALSE otherwise.
  if (!rlang::is_null(default_answer)) {
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
