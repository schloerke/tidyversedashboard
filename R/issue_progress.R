dfs_val <- function(x, f) {
  idx <- dfs_idx(x, f)
  if (length(idx) == 0) {
    NULL
  } else {
    x[[idx]]
  }
}

dfs_idx <- function(x, f) {
  res <- integer()
  walk <- function(x, depth) {
    for (i in seq_along(x)) {
      if (isTRUE(f(x[[i]]))) {
        res[[depth]] <<- i
        return(TRUE)
      } else {
        if (is.list(x[[i]]) && isTRUE(walk(x[[i]], depth + 1))) {
          res[[depth]] <<- i
          return(TRUE)
        }
      }
    }
  }
  walk(x, 1)
  res
}

end_cursor <- function(x) is.list(x) && !is.null(x$endCursor)

paginate <- function(f, ...) {
  out <- append(list(), f(NULL, ...))
  cursor <- dfs_val(out, end_cursor)[[1]]
  while (!is.null(cursor)) {
    res <- f(cursor, ...)
    out <- append(out, res)
    cursor <- dfs_val(res, end_cursor)[[1]]
  }
  out
}

parse_weekly_issues <- function(x) {
  tibble::tibble(
    package = x$repository$name,
    type = x$type,
    issue = x$number,
    closed = parse_datetime_8601(x$closedAt %||% NA),
    opened = parse_datetime_8601(x$createdAt %||% NA),
    merged = parse_datetime_8601(x$mergedAt %||% NA))
}

#' Retrieve opened and closed issues and pull requests since a given date
#'
#' @param start Datetime to start query from
#' @inheritParams org_data
#' @importFrom lubridate today dweeks
#' @export
issue_progress <- function(org, start = today() - dweeks(1), privacy = c("PUBLIC", "PRIVATE", "BOTH")) {
  privacy <- normalize_privacy(privacy)
  query <- glue::glue("user:{org} updated:>={start} sort:updated-dsc")

  if (!is.null(privacy)) {
    query <- glue::glue("{query} is:{tolower(privacy)}")
  }
  issue_progress_query(query, org)
}


#' @param repo repository belonging to the \code{org}
#' @rdname issue_progress
#' @export
repo_issue_progress <- function(org, repo, start = today() - dweeks(1)) {
  query <- glue::glue("repo:{org}/{repo} updated:>={start} sort:updated-dsc")
  issue_progress_query(query, org)
}

issue_progress_query <- function(query, org) {
  res <- paginate(function(cursor, ...) {
    graphql_query("weekly_issues.graphql", query = query, cursor = cursor)
  })
  print(res)
  mutate(
    map_dfr(res, function(x) map_dfr(x$search$nodes, parse_weekly_issues)),
    owner = org)
}