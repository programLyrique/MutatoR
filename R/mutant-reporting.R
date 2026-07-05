# Build a console report (a character vector of lines) listing surviving mutants
# with `file:line` (or `file:start-end` when the engine could only locate the
# mutant to an enclosing block), the mutation, and a bit of source context.
# `color = NULL` auto-detects ANSI support (via cli, if installed); `context` is
# the number of source lines shown either side of the mutated line (for a span,
# the head and tail of the span, elided in the middle); at most `max_show`
# mutants are detailed.
format_surviving_mutants <- function(survivors, pkg_dir = NULL, color = NULL, context = 1L, max_show = 50L) {
  if (length(survivors) == 0L) {
    return(character(0))
  }
  disp_path <- function(path) mutant_display_path(path, pkg_dir)
  have_cli <- requireNamespace("cli", quietly = TRUE)
  forced <- isTRUE(color)
  if (is.null(color)) {
    color <- have_cli && cli::num_ansi_colors() > 1L
  }
  styled <- isTRUE(color) && have_cli
  # cli only emits ANSI when it detects a colour-capable connection. When the
  # caller explicitly forces colour, tell cli to emit it regardless.
  if (styled && forced && cli::num_ansi_colors() <= 1L) {
    old <- options(cli.num_colors = 8L)
    on.exit(options(old), add = TRUE)
  }

  out <- sprintf("Surviving mutants (%d):", length(survivors))
  shown <- 0L
  for (m in survivors) {
    if (shown >= max_show) {
      out <- c(out, sprintf("  ... and %d more (in result$package_mutants)", length(survivors) - shown))
      break
    }
    shown <- shown + 1L
    info <- parse_mutation_info(m$mutation_info, m$src)
    file <- disp_path(info$file)
    line <- info$start_line
    # When the engine could only locate the mutant to an enclosing expression
    # (operator/constant mutants have no srcref of their own, so the reported
    # span is the whole containing block), end_line > start_line. Report that as
    # a `start-end` range rather than a single line, which would imply a
    # precision we do not have.
    end_line <- if (!is.na(info$end_line)) max(info$end_line, line) else line
    multi_line <- !is.na(line) && end_line > line
    det <- if (is.na(info$details)) "" else info$details
    line_label <- if (is.na(line)) {
      "?"
    } else if (multi_line) {
      sprintf("%d-%d", line, end_line)
    } else {
      as.character(line)
    }
    loc <- sprintf("%s:%s", file, line_label)
    out <- c(out, sprintf(
      "  %s   %s",
      if (styled) cli::style_bold(cli::col_cyan(loc)) else loc, det
    ))

    if (context > 0L && !is.na(line) && !is.null(m$src) && file.exists(m$src)) {
      srclines <- tryCatch(readLines(m$src, warn = FALSE), error = function(e) character(0))
      if (length(srclines) >= line) {
        end_line <- min(end_line, length(srclines))
        # Single-line mutants: a tight window with the mutated columns marked.
        # Multi-line (imprecise) mutants: show the whole span so the
        # `start-end` header is not contradicted by a lone caret; elide the
        # middle of long spans to keep the listing compact.
        gap_after <- NA_integer_
        if (!multi_line) {
          idx <- max(1L, line - context):min(length(srclines), line + context)
        } else {
          head_hi <- min(line + context, end_line)
          tail_lo <- max(end_line - context, head_hi + 1L)
          if (tail_lo > head_hi + 1L) {
            idx <- c(line:head_hi, tail_lo:end_line)
            gap_after <- head_hi
          } else {
            idx <- line:end_line
          }
        }
        width <- nchar(as.character(max(idx)))
        for (i in idx) {
          txt <- srclines[i]
          in_range <- i >= line && i <= end_line
          if (in_range && styled && !multi_line) {
            if (!is.na(info$start_col) && !is.na(info$end_col)) {
              sc <- max(1L, info$start_col)
              ec <- min(info$end_col, nchar(txt))
              if (ec >= sc) {
                txt <- paste0(
                  substr(txt, 1L, sc - 1L),
                  cli::col_red(substr(txt, sc, ec)),
                  substr(txt, ec + 1L, nchar(txt))
                )
              }
            } else {
              txt <- cli::style_bold(txt)
            }
          }
          gutter <- if (in_range) ">" else " "
          num <- formatC(i, width = width)
          if (styled) num <- cli::col_grey(num)
          out <- c(out, sprintf("    %s %s | %s", gutter, num, txt))
          if (!is.na(gap_after) && i == gap_after) {
            ell_num <- formatC("", width = width)
            if (styled) ell_num <- cli::col_grey(ell_num)
            out <- c(out, sprintf("    %s %s | ...", " ", ell_num))
          }
        }
      }
    }
  }
  out
}

