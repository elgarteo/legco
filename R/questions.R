#' Questions by LegCo Members
#'
#' Fetch questions put to the government by LegCo members in council meetings.
#'
#' @param rundown_id The id of a rundown, or a vector of ids. Defaults to
#'   `NULL`.
#'
#' @param speaker_id The id of a speaker, or a vector of ids. Defaults to
#'   `NULL`.
#'
#' @param type The type of question. `'oral'` returns oral questions.
#'   `'written'` returns written questions. `'all'` returns all questions.
#'   Defaults to `'all'`.
#'
#' @inheritParams legco_api
#' @inheritParams hansard
#'
#' @export
#' 
questions <- function(rundown_id = NULL, speaker_id = NULL, type = "all",
                      lang = "en", from = '1900-01-01', to = Sys.Date(),
                      floor = FALSE, n = 1000, extra_param = NULL, count = FALSE, verbose = TRUE) {
  query <- "Questions?$select=MeetingDate,QuestionType,Subject,Speaker,SpeakerID,RundownID,HansardFileURL"
  
  filter_args <- {}
  
  if (!is.null(rundown_id)) {
    filter_args <- c(filter_args, generate_filter("RundownID", rundown_id))
  }

  if (!is.null(speaker_id)) {
    filter_args <- c(filter_args, generate_filter("SpeakerID", speaker_id))
  }
  
  type <- tolower(type)
  if (type == "oral") {
    filter_args <- c(filter_args, "QuestionType eq 'Oral'")
  } else if (type == "written") {
    filter_args <- c(filter_args, "QuestionType eq 'Written'")
  }
  
  if (is.null(rundown_id)) {
    lang <- tolower(lang)
    if (floor) {
      filter_args <- c(filter_args, "HansardType eq 'Floor'")
    } else if (lang == "en") {
      filter_args <- c(filter_args, "HansardType eq 'English'")
    } else if (lang == "zh") {
      filter_args <- c(filter_args, "HansardType eq 'Chinese'")
    }
  }
  
  from <- as.Date(from)
  to <- as.Date(to)
  filter_args <- c(filter_args, paste0("MeetingDate ge datetime\'", from, 
                                       "\' and MeetingDate le datetime\'", to, "\'"))
  
  query <- paste0(query, "&$filter=", paste(filter_args, collapse = " and "))
  
  if (!is.null(extra_param)) {
    query <- paste0(query, extra_param)
  }
  
  legco_api("hansard", query, n, count, verbose)
}

#' @rdname questions
#' @export
legco_questions <- questions
