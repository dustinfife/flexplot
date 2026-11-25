# The hidden legacy selector retains automatic/manual provenance; the visible
# selection always shows the resolved family, never the internal auto sentinel.
# Inspect actual observations, never declared levels or predictor-complete subsets.
jamovi_glm_binary_outcome = function(response) {
  if (!is.null(dim(response))) return(FALSE)
  observed = response[!is.na(response)]
  if (length(unique(observed)) != 2L) return(FALSE)
  if (is.factor(response) || is.character(response) || is.logical(response)) return(TRUE)
  !is.object(response) && typeof(response) %in% c("integer","double") &&
    all(is.finite(observed)) && all(observed %in% c(0,1))
}

jamovi_glm_family = function(options, data) {
  if (!identical(options$family,"auto")) return(options$family)
  response = if (length(options$out)==1L && options$out %in% names(data)) data[[options$out]] else NULL
  if (jamovi_glm_binary_outcome(response)) "binomial" else "gaussian"
}

# Old documents normally store their named family. A legacy protobuf omitting the
# selector entirely must retain its historical Gaussian default, conservatively.
jamovi_glm_family_restore = function(options) {
  pb = options$asProtoBuf()
  if (!is.null(pb) && !"family" %in% pb$names)
    jamovi_output_option(options,"family","gaussian")
  invisible(NULL)
}

jamovi_glm_family_publish = function(options, data=NULL) {
  family = jamovi_glm_family(options,data)
  jamovi_output_option(options,"automaticFamily",family)
  jamovi_output_option(options,"familySelection",family)
  jamovi_output_option(options,"automaticFamilyOutcome",if(length(options$out)) options$out else "")
  invisible(family)
}
