## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE-------------------------------------------------------
source("taskq.R")

## ------------------------------------------------------------------------
q <- task_q$new()
q$push(function() { Sys.getpid() })
q$push(function() { Sys.sleep(.5); Sys.getpid() })
q$pop()

## ------------------------------------------------------------------------
sec <- as.difftime(1, units = "secs")
q$pop(sec * 1/2)$result
q$pop()

## ------------------------------------------------------------------------
q$poll(Inf)
q$pop()$result

## ------------------------------------------------------------------------
q$pop()

## ------------------------------------------------------------------------
q <- task_q$new()
for (i in 1:10) {
  q$push(function(i) { Sys.sleep(runif(1)); paste(i, "done") }, list(i = i))
}

## ------------------------------------------------------------------------
q$list_tasks()

## ------------------------------------------------------------------------
q$poll(sec)
q$list_tasks()

## ------------------------------------------------------------------------
while (!q$is_idle()) {
  task_result <- q$pop(Inf)
  print(task_result$result)
}

## ------------------------------------------------------------------------
task_result

## ------------------------------------------------------------------------
q$push(function() stop("This failed, sorry"))
res <- q$pop(Inf)
res$error

## ------------------------------------------------------------------------
res$error$parent$trace

## ------------------------------------------------------------------------
task_q <- R6::R6Class(
  "task_q",
  public = list(
    initialize = function(concurrency = 4L) {
      private$start_workers(concurrency)
      invisible(self)
    },
    list_tasks = function() private$tasks,
    get_num_waiting = function()
      sum(!private$tasks$idle & private$tasks$state == "waiting"),
    get_num_running = function()
      sum(!private$tasks$idle & private$tasks$state == "running"),
    get_num_done = function() sum(private$tasks$state == "done"),

    push = function(fun, args = list(), id = NULL) {
      if (is.null(id)) id <- private$get_next_id()
      if (id %in% private$tasks$id) stop("Duplicate task id")
      before <- which(private$tasks$idle)[1]
      private$tasks <- tibble::add_row(private$tasks, .before = before,
        id = id, idle = FALSE, state = "waiting", fun = list(fun),
        args = list(args), worker = list(NULL), result = list(NULL))
      private$schedule()
      invisible(id)
    },

    poll = function(timeout = 0) {
      limit <- Sys.time() + as.difftime(timeout / 1000, units = "secs")
      repeat{
        topoll <- which(private$tasks$state == "running")
        conns <- lapply(
          private$tasks$worker[topoll],
          function(x) x$get_poll_connection())
        pr <- unlist(processx::poll(conns, timeout))
        private$tasks$state[topoll][pr == "ready"] <- "ready"
        private$schedule()
        ret <- private$tasks$id[private$tasks$state == "done"]
        if (length(ret) || (timeout != -1 && Sys.time() > limit)) break;
        if (timeout != -1)
          timeout <- max(0, as.double(limit - Sys.time(), units = "secs"))
      }
      ret
    },

    pop = function(timeout = 0) {
      if (is.na(done <- self$poll(timeout)[1])) return(NULL)
      row <- match(done, private$tasks$id)
      result <- private$tasks$result[[row]]
      private$tasks <- private$tasks[-row, ]
      c(result, list(task_id = done))
    }
  ),

  private = list(
    tasks = NULL,
    next_id = 1L,
    get_next_id = function() {
      id <- private$next_id
      private$next_id <- id + 1L
      paste0(".", id)
    },

    start_workers = function(concurrency) {
      private$tasks <- tibble::tibble(
        id = character(), idle = logical(),
        state = c("waiting", "running", "ready", "done")[NULL],
        fun = list(), args = list(), worker = list(), result = list())
      for (i in seq_len(concurrency)) {
        rs <- callr::r_session$new(wait = FALSE)
        private$tasks <- tibble::add_row(private$tasks,
          id = paste0(".idle-", i), idle = TRUE, state = "running",
          fun = list(NULL), args = list(NULL), worker = list(rs),
          result = list(NULL))
      }
    },

    schedule = function() {
      ready <- which(private$tasks$state == "ready")
      if (!length(ready)) return()
      rss <- private$tasks$worker[ready]

      private$tasks$result[ready] <- lapply(rss, function(x) x$read())
      private$tasks$worker[ready] <- replicate(length(ready), NULL)
      private$tasks$state[ready] <-
        ifelse(private$tasks$idle[ready], "waiting", "done")

      waiting <- which(private$tasks$state == "waiting")[1:length(ready)]
      private$tasks$worker[waiting] <- rss
      private$tasks$state[waiting] <-
        ifelse(private$tasks$idle[waiting], "ready", "running")
      lapply(waiting, function(i) {
        if (! private$tasks$idle[i]) {
          private$tasks$worker[[i]]$call(private$tasks$fun[[i]],
                                         private$tasks$args[[i]])
        }
      })
    }
  )
)

