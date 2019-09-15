#' Alcohol use among youth
#'
#' A dataset containing alcohol use from 82 youth over the course of three years
#'
#' @format A data frame with 246 rows and 9 variables:
#' \describe{
#'   \item{ID}{ID of participant}
#'   \item{AGE}{AGE of participant}
#'   \item{COA}{Are they a child of an alcoholic? 1=yes}
#'   \item{MALE}{Gender (1=male)}
#'   \item{AGE_14}{Number of years since age 14}
#'   \item{ALCUSE}{Average number of alcoholic drinks a week }
#'   \item{PEER}{Average number of alcoholic drinks a week among their peers}
#'   \item{CPEER}{Median-centered amount their peers drink}
#'   \item{CCOA}{Child of alcoholic, centered}
#' }
"alcuse"

#' Sales of books on Amazon
#'
#' A data-scraped dataset of book sales from about 200K authors
#'
#' @format A data frame with 197,197 rows and 22 variables:
#' \describe{
#' \item{Sold.by}{Publisher of book}
#' \item{Title}{Deidentified book title}
#' \item{Author}{Deidentified book author}
#' \item{Kindle.eBooks.Sales.Rank}{Sales rank}
#' \item{Sale.Price}{Price of book}
#' \item{Total.Reviews}{Number of reviews}
#' \item{Average.Rating}{average of reviews}
#' \item{Preorder}{Available for pre-order?}
#' \item{Date.Published}{Date published}
#' \item{Categories}{Fiction category}
#' \item{DRM}{Digital Rights Management}
#' \item{KU}{Kindle Unlimited}
#' \item{Has.ISBN}{Does the book have an ISBN?}
#' \item{Price.Was.Set.By.Publisher}{Was the price set by the publisher?}
#' \item{Consolidated.Digital.List.Price}{Dunno}
#' \item{Daily.Units.Sold}{Estimated number of books sold a day}
#' \item{Daily.Gross.Sales}{Estimated daily sales}
#' \item{Daily.Author.Revenue}{Daily author revenue (estimated)}
#' \item{Publisher}{Publisher type}
#' \item{Year}{Year published}
#' \item{Month}{Month Published}
#' \item{age}{Age of book}
#' }
"authors"