#' @export
new_song <- function(
  pitches,
  durations = NA,
  tempo = 120,
  key = "C",
  meter = "4/4",
  instrument = "piano") {

  song <- tibble(
    tempo = tempo,
    key = key,
    meter = meter,
    instrument = instrument,
    pitches = pitches,
    durations = durations)

  song %>%
    mutate(durations = pmap(list(pitches, meter, durations), guess_duration)) %>%
    with_groups("instrument", mutate, bar_id = row_number()) %>%
    select(bar_id, everything())
}

#' @export
copy_and_paste <- function(data, subset_from, subset_to = NULL, what = c("pitches", "durations")) {
  copy <- filter(data, {{subset_from}})
  if(is.null(subset_to)) {
    copy$bar_id <- max(data$bar_id) - min(copy$bar_id) + 1 + copy$bar_id
    return(bind_rows(data, copy))
  } else {
    indices_to <- eval(substitute(subset_to), data, parent.frame())
    if(sum(indices_to) != nrow(copy))
      stop("copy and paste areas don't contain the same number of observations")
    data[indices_to, what] <- copy[what]
  }
}

#' @export
add_bars <- function(
  data,
  pitches,
  durations = NA,
  tempo = NULL,
  key = NULL,
  meter = NULL,
  instrument = NULL,
  at = NULL) {
  res <- data
  if(is.null(at)) {
    restricted <- filter(data, bar_id == max(bar_id))
    if(is.null(tempo))  tempo <- restricted$tempo[1] # should be unique per bar id
    if(is.null(key))    key   <- restricted$key[1]
    if(is.null(meter))  meter <- restricted$meter[1]
    if(is.null(instrument)) {
      instrument <- unique(restricted$instrument)
      if(length(instrument) != 1) stop("ambiguous instrument, please specify explicitly")
    }
    to_add <- new_song(pitches, durations, tempo, key, meter, instrument)
    to_add$bar_id <- max(data$bar_id) + to_add$bar_id
    res <- bind_rows(res, to_add)
  } else {

    restricted <- data %>%
      filter(bar_id %in% at) %>%
      distinct(bar_id, .keep_all = TRUE)

    if(is.null(tempo))
      tempo <- restricted$tempo

    if(is.null(key))    key   <- restricted$key
    if(is.null(meter))  meter <- restricted$meter

    if(is.null(instrument)) {
      instrument <- restricted$instrument
    }

    to_add <- new_song(pitches, durations, tempo, key, meter, instrument)
    to_add$bar_id <- restricted$bar_id
    res <- bind_rows(res, to_add)
  }

  res
}

#' @export
play <- function(data) {
  data <- data %>%
    mutate(
      # convert vectors to lists when relevant
      pitches = map(pitches, as.list),
      durations = map(durations, as.list),
      # to make sure we start at bar 1
      bar_id = bar_id - min(bar_id) + 1
    )

  # complete the data
  data <- data %>%
    complete(instrument, bar_id) %>%
    arrange(bar_id) %>%
    group_by(bar_id) %>%
    # tempo key and meter are given as was for other instruments of same bar_id
    fill(tempo, key, meter, .direction = "downup") %>%
    ungroup() %>%
    mutate(pitches = map(pitches, ~if(!length(.)) NA else .),
           # the following will work in 4/4, not in general
           durations = map(durations,  ~if(!length(.)) "whole" else .))

  meta0 <- data %>%
    arrange(bar_id) %>%
    mutate(grp = data.table::rleid(tempo, key, meter)) %>%
    group_by(grp) %>%
    summarize(
      code = list(
        bquote(Tempo(.(tempo[1]), bar = .(bar_id[1]))),
        bquote(Meter(..(as.numeric(strsplit(meter[1], "/")[[1]])), bar = .(bar_id[1])), splice = TRUE),
        bquote(Key(.(key_to_numeric(key[1]))))
      ),
      .groups = "drop"
    ) %>%
    pull(code)

  code0 <- data %>%
    arrange(bar_id) %>%
    group_by(instrument) %>%
    summarize(
      code = list(
        bquote(Line(
          pitches = .(unlist(pitches, FALSE)),
          durations = .(as.list(unlist(map2(pitches, durations, process_durations)))),
          name = .(instrument[1])))
      ),
      .groups = "drop"
    ) %>%
    pull(code)

  code <- Reduce(function(x, y) call("+", x, y), c(meta0, code0), quote(Music()))
  m <- eval(code)
  show(m, to = c("score", "audio"))
}
