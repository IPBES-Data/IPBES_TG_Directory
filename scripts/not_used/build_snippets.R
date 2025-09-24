#!/usr/bin/env Rscript

# build_snippets.R — Create TG snippets in ./tgs/ from ./metadata.qmd/*.yaml
# Each snippet is named by index_no if present, otherwise by subtitle fallback.
# The content follows the standardized pattern with icons, link, DOI badge,
# and abstract.

suppressWarnings({
  need <- function(pkg) { if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org") }
})

need("yaml")

this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args)
  if (length(m)) return(normalizePath(sub("^--file=", "", args[m[length(m)]])))
  stop("Run via Rscript")
}

script_path <- this_file()
scripts_dir <- normalizePath(file.path(dirname(script_path)))
dir_repo <- normalizePath(file.path(scripts_dir, ".."))
dir_meta <- file.path(dir_repo, "metadata.qmd")
dir_out  <- file.path(dir_repo, "tgs")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

header_icons <- function(tokens_norm) {
  slots <- c("expert","tsu","secretariat","assesment_tsu","assesment_experts")
  titles <- c(
    expert = "Expert",
    tsu = "TSU",
    secretariat = "Secretariat",
    assesment_tsu = "Assessment TSU",
    assesment_experts = "Assessment Experts"
  )
  has_slot <- function(slot, present) {
    syns <- switch(slot,
      assesment_tsu = c("assesment_tsu","assessment_tsu"),
      assesment_experts = c("assesment_experts","assesment_expert","assessment_experts","assessment_expert"),
      expert = c("expert","experts"), tsu = c("tsu","tsus"), secretariat = c("secretariat","secretary"), slot)
    any(syns %in% present)
  }
  parts <- vapply(slots, function(s) {
    if (has_slot(s, tokens_norm)) {
      sprintf("![%s](figures/icon-%s.svg \"%s\"){height=15}", tolower(titles[[s]]), s, titles[[s]])
    } else {
      sprintf("![none](figures/icon-none.svg \"None\"){height=15}")
    }
  }, character(1))
  paste(parts, collapse = "")
}

sanitize <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^a-z0-9_.-]", "", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_+", "", x)
  x <- sub("_+$", "", x)
  if (!nzchar(x)) x <- "untitled"
  x
}

files <- list.files(dir_meta, pattern = "\\.yaml$", full.names = TRUE)
if (!length(files)) quit(save = "no")

for (f in sort(files)) {
  repo <- sub("\\.yaml$", "", basename(f))
  ytxt <- readLines(f, warn = FALSE)
  # Strip separators if present
  if (length(ytxt) >= 1 && trimws(ytxt[1]) == "---") {
    ytxt <- ytxt[-1]
    k <- which(trimws(ytxt) == "---")
    if (length(k)) ytxt <- ytxt[seq_len(k[1]-1)]
  }
  y <- yaml::yaml.load(paste(ytxt, collapse = "\n"))
  title <- if (!is.null(y$title)) as.character(y$title) else repo
  subtitle <- if (!is.null(y$subtitle)) as.character(y$subtitle) else title
  doi <- if (!is.null(y$doi)) as.character(y$doi) else ""
  abstract <- if (!is.null(y$abstract)) as.character(y$abstract) else ""
  cats <- character(0)
  if (!is.null(y$categories)) cats <- unlist(y$categories, use.names = FALSE)
  else if (!is.null(y$keyword)) cats <- unlist(strsplit(as.character(y$keyword), ","), use.names = FALSE)
  cats <- trimws(cats); cats <- cats[nzchar(cats)]
  cats_norm <- tolower(gsub("[[:space:]]+", "_", cats))
  link <- sprintf("https://ipbes-data.github.io/%s/", repo)
  idx <- if (!is.null(y$index_no)) as.character(y$index_no) else ""
  name_key <- if (nzchar(idx)) sanitize(idx) else sanitize(subtitle)
  out <- file.path(dir_out, sprintf("%s.qmd", name_key))

  con <- file(out, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  ic <- header_icons(cats_norm)
  cat(sprintf("### %s [%s](%s)\n\n", ic, title, link), file = con)
  if (nzchar(doi)) {
    cat(sprintf("  [![DOI: %s](https://zenodo.org/badge/DOI/%s.svg)](https://doi.org/%s){target=\"_blank\"}\n", doi, doi, doi), file = con)
  }
  if (nzchar(abstract)) {
    cat(sprintf("  %s\n", abstract), file = con)
  }
  cat("\n\n", file = con)
}

message("Snippets written under ", dir_out)

