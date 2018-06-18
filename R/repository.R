#' @importFrom purrr map_chr
parse_summary_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
parse_summary_repository <- function(x) {
  tibble::tibble(
    owner = x$owner$login,
    repo = x$repo,
    prs = x$prs$totalCount,
    watchers = x$watchers$totalCount,
    open_issues = x$open_issues$totalCount,
    description = list(desc::desc(text = x$DESCRIPTION$text %||% character())))
}

#' Compute an organization summary
#' @param org A GitHub user, either a normal user or an organization
#' @param privacy The repository privacy
#' @export
#' @importFrom dplyr group_by summarize left_join
#' @importFrom tidyr replace_na
org_data <- function(org, privacy = c("PUBLIC", "PRIVATE", "BOTH")) {
  privacy <- normalize_privacy(privacy)

  res <- paginate(function(cursor, ...) graphql_query("org_repo_summary.graphql", org = org, cursor = cursor, privacy = privacy))

  summary <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_summary_repository))
  issues <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_issues_repository))

  combine_summary_and_issues(summary, issues)
}
num_label <- function(x, label) {
  sum(map_lgl(x, ~ any(.x == label)))
}

combine_summary_and_issues <- function(summary, issues) {
  if (nrow(as_data_frame(issues)) > 0) {
    issues_for_summary <- issues %>%
      group_by(owner, repo) %>%
      summarize(p1 = sum(p1),
                bugs = num_label(labels, "bug"),
                features = num_label(labels, "feature"),
                unlabeled = sum(lengths(labels) == 0))
  } else {
    ch0 <- character(0)
    i0 <- integer(0)
    issues_for_summary <- data_frame(owner = ch0, repo = ch0, p1 = i0, bugs = i0, features = i0, unlabeled = i0)
  }

  summary <- left_join(summary, issues_for_summary) %>%
    replace_na(list(p1 = 0, bugs = 0, features = 0, unlabeled = 0))

  list(summary = summary, issues = issues)
}


#' @param repo repository belonging to the \code{org}
#' @export
#' @rdname org_data
repo_data <- function(org, repo) {
  res <- graphql_query("repo_summary.graphql", org = org, repo = repo)
  res$data$repository$owner <- list(login = org)

  summary <- parse_summary_repository(res$data$repository)
  issues <- parse_issues_repository(res$data$repository)

  combine_summary_and_issues(summary, issues)
}
