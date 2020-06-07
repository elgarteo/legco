#' Search Question in LegCo
#'
#' Search full text of questions put to the government by LegCo member(s).
#'
#' This is a complementary function to \code{\link{questions}()}.
#'
#' @param n the number of record to fetch. Defaults to \code{50}.
#'
#' @inheritParams meeting_schedule-db
#' @inheritParams hansard-db
#' @inheritParams questions
#' @inherit comp-fun
#'
#' @export
#' 
search_question <- function(speaker_id = NULL, member_id = NULL, rundown_id = NULL,
                           type = "all", lang = "en", from = '1900-01-01', to = Sys.Date(), 
                           floor = FALSE, n = 50, verbose = TRUE) {
  if (is.null(speaker_id) & is.null(member_id) & is.null(rundown_id)) 
    stop("Please specifiy at least one LegCo member or Rundown ID.")

  if (!is.null(member_id)) {
    tmp <- search_member(member_id = member_id, verbose = verbose)
    speaker_id <- c(speaker_id, tmp$SpeakerID)
  }
  df <- questions(speaker_id = speaker_id, rundown_id = rundown_id,
                  type = type, lang = lang, from = from, to = to,
                  floor = floor, n = n, verbose = verbose)
  
  if (!is.null(df)) {
    txt <- lapply(1:nrow(df), function(i) {
      # Locate range of Rundown ID of question
      hansard_id <- rundown(df$RundownID[i], verbose = verbose)
      hansard_id <- hansard_id$HansardID
      max_rundown_id <- subjects(hansard_id = hansard_id, verbose = verbose)
      max_rundown_id <- max_rundown_id$RundownID[min(which(max_rundown_id$RundownID > df$RundownID[i]))] - 1
      
      # Fetch full text with Rundown IDs
      full_txt <- rundown((df$RundownID[i] + 1):max_rundown_id, verbose = verbose)
      full_txt <- full_txt[order(full_txt$RundownID), ]
      
      # Extract text by speaker type
      question_txt <- full_txt$Content[grep("^SP_MB", full_txt$BookmarkName)]
      answer_txt <- full_txt$Content[grep("^SP_PO", full_txt$BookmarkName)]
      misc_txt <- full_txt$Content[grep("^SP_LC", full_txt$BookmarkName)]
      if (!length(misc_txt))
        misc_txt <- NA
      
      data.frame(AskingSpeakerID = I(list(unique(full_txt$SpeakerID[grep("^SP_MB", full_txt$BookmarkName)]))),
                 AnsweringSpeakerID = I(list(unique(full_txt$SpeakerID[grep("^SP_PO", full_txt$BookmarkName)]))),
                 Question = I(list(question_txt)),
                 Answer = I(list(answer_txt)),
                 Misc = I(list(misc_txt)), stringsAsFactors = FALSE)
    })
    txt <- do.call("rbind", txt)
    df <- cbind(df, txt)
    if (verbose) {
      message(nrow(df), " record(s) match(es) your parameters.")
    }
    df
  }
}

#' @rdname search_question
#' @export
legco_search_question <- search_question
