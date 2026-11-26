#!/usr/bin/env Rscript

# Generate tg_list.md by fetching metadata_qmd.yaml from each TG repo's GitHub Pages
# Repos considered: all org repos starting with IPBES_TG_, excluding IPBES_TG_Directory
# Output: IPBES_TG_Directory/generated/tg_list.md

suppressWarnings({
  quietly <- function(expr) {
    suppressWarnings(suppressMessages(force(expr)))
  }
})


library(yaml)
library(jsonlite)
library(httr2)

this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args)
  if (length(m)) {
    return(normalizePath(sub("^--file=", "", args[m[length(m)]])))
  }
  stop("Run via Rscript")
}

script_path <- this_file()
scripts_dir <- normalizePath(file.path(dirname(script_path)))
dir_repo <- normalizePath(file.path(scripts_dir, ".."))
dir_out <- file.path(dir_repo, "generated")
if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)
}
out_file <- file.path(dir_out, "tg_list.md")

# Helper: fixed icon header
header_icons <- function(tokens_norm) {
  slots <- c(
    "expert",
    "tsu",
    "secretariat",
    "assesment_tsu",
    "assesment_experts"
  )
  titles <- c(
    expert = "Expert",
    tsu = "TSU",
    secretariat = "Secretariat",
    assesment_tsu = "Assessment TSU",
    assesment_experts = "Assessment Experts"
  )

  has_slot <- function(slot, present) {
    syns <- switch(
      slot,
      assesment_tsu = c("assesment_tsu", "assessment_tsu"),
      assesment_experts = c(
        "assesment_experts",
        "assesment_expert",
        "assessment_experts",
        "assessment_expert"
      ),
      expert = c("expert", "experts"),
      tsu = c("tsu", "tsus"),
      secretariat = c("secretariat", "secretary"),
      slot
    )
    any(syns %in% present)
  }

  parts <- vapply(
    slots,
    function(s) {
      if (has_slot(s, tokens_norm)) {
        sprintf(
          '![%s](figures/icon-%s.svg "%s"){height=15}',
          tolower(titles[[s]]),
          s,
          titles[[s]]
        )
      } else {
        sprintf('![none](figures/icon-none.svg "None"){height=15}')
      }
    },
    character(1)
  )
  paste(parts, collapse = "")
}

fetch_json <- function(url, token) {
  req <- request(url) |>
    req_user_agent("IPBES_TG_Directory")
  if (!is.na(token) && nzchar(token)) {
    req <- req |> req_auth_bearer_token(token)
  }
  resp <- req |> req_error(is_error = function(resp) FALSE) |> req_perform()
  if (resp_status(resp) >= 300) {
    stop("GitHub API error: ", resp_body_string(resp))
  }
  resp_body_json(resp, simplifyVector = TRUE)
}

fetch_metadata_text <- function(url) {
  req <- request(url) |>
    req_user_agent("IPBES_TG_Directory") |>
    req_timeout(10)
  resp <- req |> req_error(is_error = function(resp) FALSE) |> req_perform()
  if (resp_status(resp) != 200) {
    return(NULL)
  }
  resp_body_string(resp)
}

# Discover repos via GitHub API
# Token only lifts the unauthenticated rate limit (60 req/h per shared runner IP); type=public keeps the listing public-only
gh_token <- Sys.getenv("GITHUB_TOKEN", unset = NA)
api_url <- "https://api.github.com/orgs/IPBES-Data/repos?type=public&per_page=100&sort=full_name"
repos <- fetch_json(api_url, gh_token)
names <- repos$name
names <- names[startsWith(names, "IPBES_TG_") & names != "IPBES_TG_Directory"]
names <- sort(names, method = "radix")

entries <- list()
for (nm in names) {
  base_url <- sprintf("https://ipbes-data.github.io/%s/metadata_qmd", nm)
  ytxt <- fetch_metadata_text(paste0(base_url, ".yaml"))
  if (is.null(ytxt)) {
    ytxt <- fetch_metadata_text(paste0(base_url, ".yml"))
  }
  if (is.null(ytxt)) {
    next
  }

  ytxt_clean <- sub(
    "^---
",
    "",
    ytxt
  )
  ytxt_clean <- sub(
    "
---
?$",
    "
",
    ytxt_clean
  )
  y <- yaml.load(ytxt_clean)
  title <- if (!is.null(y$subtitle)) as.character(y$subtitle) else nm
  doi <- if (!is.null(y$doi)) as.character(y$doi) else ""
  abstract <- if (!is.null(y$abstract)) as.character(y$abstract) else ""
  idx <- if (!is.null(y$index_no)) as.character(y$index_no) else ""
  cats <- character(0)
  if (!is.null(y$categories)) {
    cats <- unlist(y$categories, use.names = FALSE)
  } else if (!is.null(y$keyword)) {
    cats <- unlist(strsplit(as.character(y$keyword), ","), use.names = FALSE)
  }
  cats <- trimws(cats)
  cats <- cats[nzchar(cats)]
  cats_norm <- tolower(gsub("[[:space:]]+", "_", cats))
  link <- sprintf("https://ipbes-data.github.io/%s/", nm)

  entries[[length(entries) + 1]] <- list(
    repo = nm,
    title = title,
    doi = doi,
    abstract = abstract,
    cats_norm = cats_norm,
    link = link,
    index_no = idx
  )
}

if (length(entries) == 0) {
  writeLines("", out_file)
  quit(save = "no")
}

# Sort numerically by index_no, else by the number in "Part N..." titles,
# unnumbered last; ties (e.g. 12A/12B/12C) broken by title
titles <- vapply(entries, function(e) e$title, character(1))
nums <- suppressWarnings(as.numeric(vapply(entries, function(e) e$index_no, character(1))))
from_title <- suppressWarnings(as.numeric(sub("^Part\\s+([0-9]+).*$", "\\1", titles)))
nums[is.na(nums)] <- from_title[is.na(nums)]
ord <- order(nums, titles, na.last = TRUE, method = "radix")
entries <- entries[ord]

con <- file(out_file, open = "w", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

for (e in entries) {
  ic <- header_icons(e$cats_norm)
  cat(
    sprintf(
      "### %s [%s](%s)

",
      ic,
      e$title,
      e$link
    ),
    file = con
  )
  if (nzchar(e$doi)) {
    cat(
      sprintf(
        '  [![DOI: %s](https://zenodo.org/badge/DOI/%s.svg)](https://doi.org/%s){target="_blank"}',
        e$doi,
        e$doi,
        e$doi
      ),
      file = con
    )
  }
  if (nzchar(e$abstract)) {
    cat(
      sprintf(
        "  %s
",
        e$abstract
      ),
      file = con
    )
  }
  cat(
    "

",
    file = con
  )
}

message("Wrote ", out_file)
