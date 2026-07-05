# --- Optional source-reference refinement via the 'imputesrcref' package ------
#
# The mutation engine reports an operator mutant's location as the bounds of its
# enclosing *top-level* expression, because R attaches no `srcref` to nested call
# objects (e.g. the `+` in `x + y`). That makes the reported location as wide as
# the whole function. The 'imputesrcref' package (GitHub: PRL-PRG/imputesrcref)
# injects transparent `{ }` wrappers carrying parse-data-derived srcrefs around
# sub-expressions, which lets us recover a precise span for many operator
# mutants. It is an optional, GitHub-only package listed in Enhances and used
# only when installed, as a read-only location oracle. The imputed AST is never
# fed to the engine, so deparsed mutant output is byte-for-byte identical whether
# or not the package is present.

imputesrcref_available <- function() {
  requireNamespace("imputesrcref", quietly = TRUE)
}

# Index of the slot holding a `function(...) ...` definition in a top-level
# expression, for `name <- function(...)` / `= ` / `<<-` assignments. NULL when
# the expression is not a plain function-definition assignment.
imputed_function_slot <- function(expr) {
  if (!is.call(expr) || length(expr) < 3) {
    return(NULL)
  }
  op <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
  if (!op %in% c("<-", "=", "<<-")) {
    return(NULL)
  }
  rhs <- expr[[3]]
  if (is.call(rhs) && is.symbol(rhs[[1]]) && identical(as.character(rhs[[1]]), "function")) {
    return(3L)
  }
  NULL
}

# Build a list parallel to `parsed` in which each top-level function definition
# has imputed transparent-brace srcrefs in its body/formals. Non-functions and
# any definition that fails to impute are returned unchanged. Used only to look
# up precise source spans, never deparsed into mutant files.
build_imputed_exprs <- function(parsed) {
  lapply(parsed, function(expr) {
    slot <- imputed_function_slot(expr)
    if (is.null(slot)) {
      return(expr)
    }
    fn <- tryCatch(eval(expr[[slot]]), error = function(e) NULL)
    if (!is.function(fn)) {
      return(expr)
    }
    impute_srcrefs <- getExportedValue("imputesrcref", "impute_srcrefs")
    g <- tryCatch(impute_srcrefs(fn), error = function(e) NULL)
    if (is.null(g)) {
      return(expr)
    }
    out <- expr
    fdef <- out[[slot]]
    fdef[[3L]] <- body(g)
    fmls <- formals(g)
    if (!is.null(fmls)) {
      fdef[[2L]] <- fmls
    }
    out[[slot]] <- fdef
    out
  })
}

# An injected, transparent brace stores its srcref as a 2-element list whose
# entries are identical (both spanning the wrapped expression).
is_transparent_brace <- function(node) {
  if (!is.call(node) || !is.symbol(node[[1]]) || !identical(as.character(node[[1]]), "{")) {
    return(FALSE)
  }
  sr <- attr(node, "srcref", exact = TRUE)
  is.list(sr) && length(sr) >= 2 && identical(sr[[1]], sr[[2]])
}

# Path (sequence of `[[i]]` indices) at which `m` first differs from `o`, or
# NULL when identical. `integer(0)` means they differ at this node itself.
mutation_diff_path <- function(o, m) {
  if (is.call(o) && is.call(m) && length(o) == length(m)) {
    for (i in seq_along(o)) {
      p <- mutation_diff_path(o[[i]], m[[i]])
      if (!is.null(p)) {
        return(c(i, p))
      }
    }
    return(NULL)
  }
  if (!identical(o, m)) {
    return(integer(0))
  }
  NULL
}

# Walk `imp_node` in lock-step with `orig_node` down `path` (expressed in the
# original tree's coordinates), skipping the extra transparent braces present
# only in the imputed tree, and return the srcref of the nearest brace enclosing
# the target node (NULL when no brace encloses it, e.g. statement-level
# operators or deletions).
nearest_brace_srcref <- function(orig_node, imp_node, path) {
  best <- NULL
  cur_o <- orig_node
  cur_i <- imp_node
  descend_braces <- function() {
    while (is_transparent_brace(cur_i)) {
      best <<- attr(cur_i, "srcref", exact = TRUE)[[1]]
      cur_i <<- cur_i[[2L]]
    }
  }
  for (idx in path) {
    descend_braces()
    if (!is.call(cur_o) || length(cur_o) < idx) {
      return(best)
    }
    cur_o <- cur_o[[idx]]
    cur_i <- if (is.call(cur_i) && length(cur_i) >= idx) cur_i[[idx]] else cur_i
  }
  descend_braces()
  best
}

# Fallback location oracle that needs no imputesrcref: walk the original
# (keep.source) tree down `path` and return the srcref of the nearest enclosing
# `{`-block statement. A function body parsed with keep.source carries a
# per-statement srcref list on its `{` block, where entry `[[i]]` is the srcref
# of the block's i-th element (position 1 being `{` itself). So descending into
# child `idx` of a block yields that statement's line-precise span. This turns an
# operator/constant mutant's coarse whole-function bounds into the enclosing
# statement's line even when imputesrcref is absent. NULL when the path crosses
# no block (e.g. a statement-level operator the engine already located).
nearest_statement_srcref <- function(node, path) {
  best <- NULL
  cur <- node
  for (idx in path) {
    if (!is.call(cur) || length(cur) < idx) {
      return(best)
    }
    if (is.symbol(cur[[1]]) && identical(as.character(cur[[1]]), "{")) {
      srl <- attr(cur, "srcref", exact = TRUE)
      if (is.list(srl) && idx <= length(srl) && !is.null(srl[[idx]])) {
        best <- srl[[idx]]
      }
    }
    cur <- cur[[idx]]
  }
  best
}

# Given an AST mutant `m` (a full-file expression list with exactly one top-level
# expression changed), refine the coarse `info` location. Two oracles, tried in
# order of precision: (1) the nearest enclosing imputed transparent brace
# (sub-statement precise, only when imputesrcref supplied a non-NULL
# `imputed_exprs`); (2) the nearest enclosing `{`-block statement from the
# original keep.source tree (line precise, always available). Returns `info`
# unchanged when neither yields a usable span.
refine_mutation_info <- function(info, parsed, imputed_exprs, m) {
  if (!is.list(info) || length(m) != length(parsed)) {
    return(info)
  }
  changed <- NULL
  for (k in seq_along(parsed)) {
    if (!identical(parsed[[k]], m[[k]])) {
      changed <- k
      break
    }
  }
  if (is.null(changed)) {
    return(info)
  }
  path <- mutation_diff_path(parsed[[changed]], m[[changed]])
  if (is.null(path)) {
    return(info)
  }
  sr <- if (!is.null(imputed_exprs)) {
    tryCatch(
      nearest_brace_srcref(parsed[[changed]], imputed_exprs[[changed]], path),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  if (is.null(sr) || length(sr) < 4 || anyNA(sr[1:4])) {
    sr <- tryCatch(
      nearest_statement_srcref(parsed[[changed]], path),
      error = function(e) NULL
    )
  }
  if (is.null(sr) || length(sr) < 4 || anyNA(sr[1:4])) {
    return(info)
  }
  info$start_line <- as.integer(sr[1])
  info$start_col <- as.integer(sr[2])
  info$end_line <- as.integer(sr[3])
  info$end_col <- as.integer(sr[4])
  info
}

