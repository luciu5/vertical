# Internal ownership classification for vertical bargaining models.

## Ownership vectors identify co-control through pairwise equality.  Keeping
## this operation in one place is important: comparing owner vectors directly
## confounds owner labels with the control relationships they encode.
.vertical_control_relation <- function(owner) {
  owner <- as.vector(owner)
  if (!length(owner)) return(matrix(logical(), nrow = 0L, ncol = 0L))
  outer(owner, owner, FUN = function(x, y) {
    !is.na(x) & !is.na(y) & x == y
  })
}

.vertical_cross_relation <- function(up, down) {
  up <- as.vector(up)
  down <- as.vector(down)
  if (!length(up) || !length(down)) {
    return(matrix(logical(), nrow = length(up), ncol = length(down)))
  }
  outer(up, down, FUN = function(x, y) {
    !is.na(x) & !is.na(y) & x == y
  })
}

.vertical_relation_delta <- function(pre, post, exclude_diagonal = FALSE) {
  ## Owner vectors are required to retain their market lengths by the existing
  ## simulation validator.  The dimension guard keeps this helper informative
  ## for direct/internal use without adding a new public ownership restriction.
  if (!identical(dim(pre), dim(post))) {
    stop("pre- and post-counterfactual ownership must describe the same products")
  }
  pre <- !is.na(pre) & pre
  post <- !is.na(post) & post
  new <- !pre & post
  lost <- pre & !post
  if (exclude_diagonal) {
    diag(new) <- FALSE
    diag(lost) <- FALSE
  }
  list(
    new = new,
    lost = lost,
    changed = any(new | lost, na.rm = TRUE)
  )
}

.vertical_pair_endpoints <- function(pairs) {
  if (!length(pairs)) return(logical())
  if (!nrow(pairs)) return(logical(nrow(pairs)))
  rowSums(pairs, na.rm = TRUE) > 0 | colSums(pairs, na.rm = TRUE) > 0
}

.vertical_classify_ownership <- function(owner_pre_up, owner_post_up,
                                         owner_pre_down, owner_post_down) {
  owners <- list(owner_pre_up, owner_post_up, owner_pre_down, owner_post_down)
  if (any(vapply(owners, is.matrix, logical(1)))) {
    stop("hybrid merger classification requires ownership vectors, not matrices")
  }
  up_pre <- .vertical_control_relation(owner_pre_up)
  up_post <- .vertical_control_relation(owner_post_up)
  down_pre <- .vertical_control_relation(owner_pre_down)
  down_post <- .vertical_control_relation(owner_post_down)
  cross_pre <- .vertical_cross_relation(owner_pre_up, owner_pre_down)
  cross_post <- .vertical_cross_relation(owner_post_up, owner_post_down)

  ## A same-tier merger is a new co-control pair.  Diagonal entries represent
  ## a product controlling itself, and therefore are never merger pairs.
  up_delta <- .vertical_relation_delta(up_pre, up_post,
    exclude_diagonal = TRUE)
  down_delta <- .vertical_relation_delta(down_pre, down_post,
    exclude_diagonal = TRUE)
  cross_delta <- .vertical_relation_delta(cross_pre, cross_post)
  upstream_horizontal <- any(up_delta$new, na.rm = TRUE)
  downstream_horizontal <- any(down_delta$new, na.rm = TRUE)
  horizontal <- upstream_horizontal || downstream_horizontal
  vertical <- any(cross_delta$new, na.rm = TRUE)
  hybrid <- horizontal && vertical

  ## A lost relationship is an ownership restructuring unless another
  ## relationship identifies the change as a horizontal or vertical merger.
  ownership_changed <- isTRUE(up_delta$changed) ||
    isTRUE(down_delta$changed) || isTRUE(cross_delta$changed)
  type <- if (!ownership_changed) {
    "none"
  } else if (hybrid) {
    "hybrid"
  } else if (horizontal) {
    "horizontal"
  } else if (vertical) {
    "vertical"
  } else {
    "restructuring"
  }

  ## Endpoints identify products participating in newly formed control pairs;
  ## they remain logical vectors so callers can use them for display/filtering.
  up_new <- up_delta$new
  down_new <- down_delta$new
  cross_new <- cross_delta$new
  party_up <- .vertical_pair_endpoints(up_new)
  party_down <- .vertical_pair_endpoints(down_new)
  if (length(party_up) && nrow(cross_new)) {
    party_up <- party_up |
      rowSums(cross_new, na.rm = TRUE) > 0
  }
  if (length(party_down) && ncol(cross_new)) {
    party_down <- party_down |
      colSums(cross_new, na.rm = TRUE) > 0
  }

  list(
    upstream_horizontal = isTRUE(upstream_horizontal),
    downstream_horizontal = isTRUE(downstream_horizontal),
    horizontal = isTRUE(horizontal),
    vertical = isTRUE(vertical),
    hybrid = isTRUE(hybrid),
    ownership_changed = isTRUE(ownership_changed),
    type = type,
    party_up = as.logical(party_up),
    party_down = as.logical(party_down)
  )
}

.vertical_classify_model <- function(object) {
  .vertical_classify_ownership(
    owner_pre_up = object@up@ownerPre,
    owner_post_up = object@up@ownerPost,
    owner_pre_down = object@down@ownerPre,
    owner_post_down = object@down@ownerPost
  )
}

.vertical_merger_components <- function(classification) {
  if (methods::is(classification, "VertBargBertLogit")) {
    classification <- .vertical_classify_model(classification)
  }
  classification[c(
    "upstream_horizontal", "downstream_horizontal", "horizontal",
    "vertical", "hybrid", "ownership_changed", "type", "party_up",
    "party_down"
  )]
}

.vertical_attach_merger_metadata <- function(result) {
  classification <- .vertical_classify_model(result)
  metadata <- attr(result, "counterfactual")
  if (!is.list(metadata)) metadata <- list()
  metadata$merger_type <- classification$type
  metadata$merger_components <- .vertical_merger_components(classification)
  metadata$merger_parties <- list(
    upstream = classification$party_up,
    downstream = classification$party_down
  )
  attr(result, "counterfactual") <- metadata
  result
}

#' Classify the ownership change represented by a vertical model
#'
#' @param object A `VerticalFit`, a `VertBargBertLogit` subclass, or a
#'   `CounterfactualPath`.
#' @return A scalar merger-type string for a fit/model, or a named character
#'   vector with one type for each result in a path.
#' @export
merger_type <- function(object) {
  if (methods::is(object, "CounterfactualPath")) {
    n <- length(object@results)
    if (!n) return(stats::setNames(character(), character()))
    types <- vapply(object@results, function(result) {
      if (!methods::is(result, "VertBargBertLogit")) {
        stop("'object' is a CounterfactualPath with a non-vertical result")
      }
      .vertical_classify_model(result)$type
    }, character(1L))
    result_names <- names(object@results)
    if (is.null(result_names) || length(result_names) != n ||
        any(!nzchar(result_names)) || anyDuplicated(result_names)) {
      result_names <- paste0("step", seq_len(n))
    }
    return(stats::setNames(types, result_names))
  }

  if (methods::is(object, "VerticalFit")) object <- object@model
  if (methods::is(object, "VertBargBertLogit")) {
    return(.vertical_classify_model(object)$type)
  }
  stop("'object' must be a VerticalFit, a VertBargBertLogit subclass, or a CounterfactualPath")
}
