keys <- c("b", "f#", "c#", "g#", "b", "a#", "f", "c", "g", "d", "a", "e", "b", "f#", "c#")
tonics <- c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b")
tonic_mapper <- setNames(tonics, tonics)

# to normalize to use only sharps
tonic_mapper <- c(tonic_mapper,
                  "d'"  = "c#", "d-" = "c#", "db" = "c#",
                  "e'"  = "d#", "e-" = "d#", "eb" = "d#",
                  "g'"  = "f#", "g-" = "f#", "gb" = "f#",
                  "a'"  = "g#", "a-" = "g#", "ab" = "g#",
                  "b'"  = "a#", "b-" = "a#", "bb" = "a#")

guess_duration <- function(pitches, meter, durations) {
  if(length(durations) == 1 && is.na(durations)){
    if(meter == "4/4") {
      len <- length(pitches)
      dur <- switch(len, "1" = "w", "2" = "h", "4" = "q")
      if(is.null(dur))
        dur <- as.character(len)
      if(len %in% 2^(0:10)) {
        durations <- as.list(rep(dur, len))
      } else {
        stop("Cannot determine default durations from given amount of pitches")
      }

    } else {
      stop("For non 4/4 meter there is no default duration")
    }
  }
  durations
}

process_durations <- function(notes, durations) {
  if(length(durations) == 1) {
    # guess duration if missing
    if(is.na(durations)) durations <- as.character(length(pitches))
    # recycle duration
    durations <- rep(list(durations), length(notes))
  }
  durations
}

key_to_numeric <- function(key) {
  # remove explicit major
  if(endsWith(key, "M"))
    key <- substr(key, 1, nchar(key) - 1)
  key <- tolower(key)
  if(is.na(key)) stop("unexpected key")
  key_is_minor <- endsWith(key, "m")
  if(key_is_minor) {
    key <- substr(key, 1, nchar(key) - 1)
    key <- tonic_mapper[key]
    key <- match(key, keys) - 8 - 3
  } else {
    key <- tonic_mapper[key]
    key <- match(key, keys) - 8
  }
  key
}
