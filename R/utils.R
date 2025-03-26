
#' @keywords internal
#' @noRd
#' @importFrom utils adist
get_similar_icons <- function(invalid_name, max_suggestions = 5) {

  valid_icons <- bsicons:::icon_info$name

  # Split the invalid name into component parts
  parts <- unlist(strsplit(invalid_name, "-|_|\\s+"))

  # Calculate both edit distance and semantic similarity
  distances <- utils::adist(invalid_name, valid_icons)[1, ]
  edit_candidates <- valid_icons[order(distances)][1:max(10, max_suggestions)]

  # Calculate semantic similarity using token-based approach
  semantic_scores <- sapply(valid_icons, function(icon) {
    icon_parts <- unlist(strsplit(icon, "-|_|\\s+"))

    # Count matching tokens in any order
    matches <- sum(parts %in% icon_parts) + sum(icon_parts %in% parts)

    # Bonus for exact token matches
    exact_matches <- length(intersect(parts, icon_parts))

    # Penalize length differences
    length_diff <- abs(length(parts) - length(icon_parts))

    # Combined score (higher is better)
    return(matches + 2*exact_matches - 0.5*length_diff)
  })

  # Get semantic candidates
  semantic_candidates <- valid_icons[order(semantic_scores, decreasing = TRUE)][1:max(10, max_suggestions)]

  # Combine and re-rank candidates
  combined_candidates <- unique(c(semantic_candidates, edit_candidates))

  final_scores <- sapply(combined_candidates, function(icon) {
    edit_score <- 1 / (1 + distances[valid_icons == icon])
    sem_score <- semantic_scores[valid_icons == icon]

    # Weight semantic score higher than edit distance
    return(0.3 * edit_score + 0.7 * sem_score)
  })

  # Return final suggestions
  return(combined_candidates[order(final_scores, decreasing = TRUE)][1:max_suggestions])
}
