#' Spin a bash script (goat's hair) into Rmd (wool)
#'
#' @description
#' This function is derived from \code{knitr::spin()} for R-scripts and
#' is adapted to do a similar conversion of bash scripts into Rmd files.
#' This function (\code{spin_sh}) does not handle inline bash statements
#' which are bash commands inside a text. The only format that is
#' considered is Rmd.
#'
#' @param hair path to the bash scirpt
#' @param knit
#' @param report
#' @param text
#' @param envir
#' @param doc
#' @param comment  pair of pattern defining a comment
#' @param precious should output source document be kept
#'
#' @return
#' @export
#'
#' @examples
spin_sh <- function (hair, knit = TRUE, report = TRUE, text = NULL, envir = parent.frame(),
          doc      = "^#+'[ ]?",
          comment  = c("^[# ]*/[*]", "^.*[*]/ *$"),
          precious = !knit && is.null(text))
{
  format = 'Rmd'
  x = if (nosrc <- is.null(text)){
    read_utf8(hair)
  } else {
    split_lines(text)
  }

  stopifnot(length(comment) == 2L)
  c1 = grep(comment[1], x)
  c2 = grep(comment[2], x)
  if (length(c1) != length(c2))
    stop("comments must be put in pairs of start and end delimiters")
  if (length(c1))
    x = x[-unique(unlist(mapply(seq, c1, c2, SIMPLIFY = FALSE)))]
  # parsed_data = utils::getParseData(parse(text = x, keep.source = TRUE))
  # is_matchable = seq_along(x) %in% unique(parsed_data[parsed_data$col1 ==
                                                        # 1, "line1"])
  p = if (identical(tolower(format), "rmd")){
    .fmt.rmd(x)
  } else {
    stop(" * Unknown Format: ", format)
  }
  r = rle((is_matchable & grepl(doc, x)) | i)
  n = length(r$lengths)
  txt = vector("list", n)
  idx = c(0L, cumsum(r$lengths))
  p1 = gsub("\\{", "\\\\{", paste0("^", p[1L], ".*", p[2L],
                                   "$"))
  for (i in seq_len(n)) {
    block = x[seq(idx[i] + 1L, idx[i + 1])]
    txt[[i]] = if (r$values[i]) {
      sub(doc, "", block)
    }
    else {
      block = strip_white(block)
      if (!length(block))
        next
      if (length(opt <- grep(rc <- "^(#|--)+(\\+|-| ----+| @knitr)",
                             block))) {
        block[opt] = paste0(p[1L], gsub(paste0(rc, "\\s*|-*\\s*$"),
                                        "", block[opt]), p[2L])
        if (any(opt > 1)) {
          j = opt[opt > 1]
          block[j] = paste(p[3L], block[j], sep = "\n")
        }
      }
      if (!grepl(p1, block[1L])) {
        block = c(paste0(p[1L], p[2L]), block)
      }
      c("", block, p[3L], "")
    }
  }
  txt = unlist(txt)
  if (report && format %in% c("Rnw", "Rtex") && !grepl("^\\s*\\\\documentclass",
                                                       txt)) {
    txt = c("\\documentclass{article}", "\\begin{document}",
            txt, "\\end{document}")
  }
  if (nosrc) {
    outsrc = with_ext(hair, format)
    xfun::write_utf8(txt, outsrc)
    txt = NULL
  }
  else outsrc = NULL
  if (!knit)
    return(txt %n% outsrc)
  out = if (report) {
    if (format == "Rmd") {
      knit2html(outsrc, text = txt, envir = envir, encoding = "UTF-8")
    }
    else if (!is.null(outsrc) && (format %in% c("Rnw", "Rtex"))) {
      knit2pdf(outsrc, envir = envir)
    }
  }
  else knit(outsrc, text = txt, envir = envir, encoding = "UTF-8")
  if (!precious && !is.null(outsrc))
    file.remove(outsrc)
  invisible(out)
}
# <bytecode: 0x7f85d1290f70>
# <environment: namespace:knitr>


#' Split a string at newline into vector
#'
#' @param x original input string
#'
#' @return vector of lines
split_lines <- function (x) {
  if (length(grep("\n", x)) == 0L)
    return(x)
  x = gsub("\n$", "\n\n", x)
  x[x == ""] = "\n"
  unlist(strsplit(x, "\n"))
}
# <bytecode: 0x7f85d499adf8>
# <environment: namespace:knitr>

.fmt.rmd <- function (x)
{
  x = paste(x, collapse = "\n")
  l = attr(gregexpr("`+", x)[[1]], "match.length")
  l = max(l, 0)
  if (length(l) > 0) {
    i = highr:::spaces(l + 1, "`")
    b = highr:::spaces(max(l + 1, 3), "`")
  }  else {
    i = "`"
    b = "```"
  }
  c(paste0(b, "{bash "), "}", b, paste0(i, "bash \\1 ", i))
}
# <bytecode: 0x7fd6290c04a8>
#   <environment: namespace:knitr>
