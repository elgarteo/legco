#' Search LegCo Member
#'
#' Search LegCo member by SpeakerID, MemberID or/and full or partial English or
#' Chinese name.
#'
#' This is a complementary function to \code{\link{speakers}()} and
#' \code{\link{member}()}.
#'
#' @param search_string search string of member's name. Accepts Chinese or
#'   English full or partial name. Defaults to \code{NULL}.
#'
#' @param exact logical: Whether to look for exact match of the search term.
#'   Defaults to \code{TRUE}.
#'
#' @inheritParams meeting_schedule-db
#' @inheritParams hansard-db
#' @inherit comp-fun
#'
#' @export
#' 
search_member <- function(search_string = NULL, speaker_id = NULL, member_id = NULL, 
                          exact = TRUE, verbose = TRUE) {
  if (is.null(search_string) & is.null(speaker_id) & is.null(member_id))
    stop("Please specifiy at least one search string/Speaker ID/Member ID.")
  
  df_speaker <- speakers(verbose = verbose)
  df_member <- member(verbose = verbose)
  df_term <- member_term(verbose = verbose)
  
  df_member$NameChi <- paste0(df_member$SurnameChi, toupper(df_member$FirstnameChi))
  df_member <- dplyr::full_join(df_member, df_term, by = "MemberID")
  df <- dplyr::full_join(df_speaker, df_member, by = "NameChi")
  df <- df[c("SpeakerID", "MemberID", "Type", "NameChi",  "NameEng", "TermID", "TitleEng", 
             "SurnameEng", "FirstnameEng", "EnglishName", "HonourableEng", "SurnameChi",
             "FirstnameChi", "TitleChi", "HonourableChi")]
  
  # Drop rows if SpeakerID or MemberID specified
  if (!(is.null(speaker_id) & is.null(member_id))) {
    df <- df[df$SpeakerID %in% speaker_id | df$MemberID %in% member_id, ]
    
    if (!nrow(df)) {
      if (is.null(member_id)) 
        stop("SpeakerID(s) ", paste(speaker_id, collapse = ", "), " not found.")
      else if (is.null(speaker_id)) 
        stop("MemberID(s) ", paste(member_id, collapse = ", "), " not found.")
      else 
        stop("SpeakerID(s) ", paste(speaker_id, collapse = ", "),
             " and MemberID(s) ", paste(member_id, collapse = ", "), " not found.")
    }
    exist_list <- speaker_id %in% df$SpeakerID
    if (FALSE %in% exist_list)
      warning("SpeakerID(s) ", paste(speaker_id[!exist_list], collapse = ", "), " not found.")
    exist_list <- member_id %in% df$MemberID
    if (FALSE %in% exist_list) 
      warning("MemberID(s) ", paste(member_id[!exist_list], collapse = ", "), " not found.")
  }
  
  # Drop rows if name search string specified
  if (!is.null(search_string)) {
    # Detect language of input
    if (grepl("[^\001-\177]", search_string)) { 
      # If Chinese
      if (exact)
        search_tmp <- search_string
      else
        search_tmp <- strsplit(search_string, "")[[1]]
      
      index <- sapply(search_tmp, function(x) grep(x, df$NameChi))
      index <- c(index, sapply(search_tmp, function(x) grep(x, df$SurnameChi)))
      index <- c(index, sapply(search_tmp, function(x) grep(x, df$FirstnameChi)))
    } else {
      # If English
      firstname_tmp <- gsub("-", " ", df$FirstnameEng)
      if (exact) 
        search_tmp <- search_string
      else
        search_tmp <- strsplit(search_string, " ")[[1]]
      
      search_tmp <- paste0("(^|[^a-z])", search_tmp, "([^a-z]|$)")
      index <- sapply(search_tmp, function(x) grep(x, df$NameEng, ignore.case = TRUE))
      index <- c(index, sapply(search_tmp, function(x) grep(x, df$SurnameEng, ignore.case = TRUE)))
      index <- c(index, sapply(search_tmp, function(x) grep(x, firstname_tmp, ignore.case = TRUE)))
      index <- c(index, sapply(search_tmp, function(x) grep(x, df$EnglishName, ignore.case = TRUE)))
    }
    index <- unique(unlist(index, use.names = FALSE))
    if (!length(index)) 
      stop("No matching result for search string \"", search_string, "\".")
    df <- df[index, ]
    rownames(df) <- 1:nrow(df)
  }
  if (verbose) 
    message(nrow(df), " record(s) match(es) your parameters.")
  df
}

#' @rdname search_member
#' @export
legco_search_member <- search_member
