

## group peaks according to RT
group_peaks_rt <- function(rt, rt.tol = 10) {
  rt_class <-
    lapply(rt, function(x) {
      which(rt >= x & rt < x + rt.tol)
    })

  rt_class <-
    lapply(seq_along(rt_class)[-1], function(i) {
      setdiff(rt_class[[i]], unlist(rt_class[1:(i - 1)]) %>% unique())
    }) %>%
    `c`(rt_class[1], .)

  rt_class <-
    rt_class[which(lapply(rt_class, length) != 0)]

  names(rt_class) <- seq_along(rt_class)

  rt_class <-
    purrr::map2(
      .x = rt_class,
      .y = names(rt_class),
      .f = function(x, y) {
        data.frame(rt = rt[x],
                   class = y,
                   stringsAsFactors = FALSE)
      }
    ) %>%
    do.call(rbind, .)
  rownames(rt_class) <- NULL
  return(rt_class)
}


#############------------------------------------------------------------------
#' @title sumFormula
#' @description Combine metabolite and adduct as a new sum formula.
#' If there are no enough element to remove, return NA.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @param adduct The adduct of metabolite.
#' @return  A sum formula.
#' @export

setGeneric(
  name = "sumFormula",
  def = function(formula = "C9H11NO2",
                 adduct = "M-H2O+H") {
    if (is.na(formula))
      return(NA)
    if (is.na(adduct))
      return(formula)
    if (adduct == "M+" | adduct == "M-") {
      return(formula)
    }

    formula1 <- splitFormula(formula)
    adduct1 <-
      strsplit(x = adduct, split = "\\-|\\+")[[1]][-1]
    polymer <-
      as.numeric(gsub(
        pattern = "M",
        replacement = "",
        strsplit(x = adduct, split = "\\-|\\+")[[1]][1]
      ))
    if (is.na(polymer))
      polymer <- 1

    plusorminus <- strsplit(x = adduct, split = "")[[1]]
    plusorminus <-
      grep("\\+|\\-", plusorminus, value = TRUE)

    formula1$number <- formula1$number * polymer

    adduct1 <- mapply(function(x, y) {
      temp <- splitFormula(x)
      temp$number <- temp$number * ifelse(y == "+", 1, -1)
      list(temp)
    },
    x = adduct1,
    y = plusorminus)

    adduct1 <- do.call(rbind, adduct1)

    formula <- rbind(formula1, adduct1)
    rownames(formula) <- NULL

    unique.element <- unique(formula$element.name)
    if (length(unique.element) == nrow(formula)) {
      if (any(formula$number < 0)) {
        return(NA)
      } else{
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element.name, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    } else{
      formula <- lapply(unique.element, function(x) {
        formula[formula$element.name == x, , drop = FALSE]
      })

      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element.name),
                   sum(x$number),
                   stringsAsFactors = FALSE)
      })

      formula <- do.call(rbind, formula)
      formula <- formula[formula[, 2] != 0, ]
      colnames(formula) <- c("element.name", "number")
      if (any(formula$number < 0)) {
        return(NA)
      } else{
        formula$number[formula$number == 1] <- "W"
        formula <-
          paste(paste(formula$element.name, formula$number, sep = ""),
                collapse = "")
        formula <- strsplit(formula, split = "")[[1]]
        formula[formula == "W"] <- ""
        formula <- paste(formula, collapse = "")
        return(formula)
      }
    }
  }
)










#-----------------------------------------------------------------------------
#' @title splitFormula
#' @description Split a formula into element and number.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @return  A splited formula.
#' @export

setGeneric(
  name = "splitFormula",
  def = function(formula = "C9H11NO2") {
    temp.formula <- strsplit(formula, split = "")[[1]]

    number <- NULL
    for (i in 1:length(temp.formula)) {
      if (length(grep("[0-9]{1}", temp.formula[i])) == 0) {
        break
      }
      number[i] <- temp.formula[i]
    }

    if (!is.null(number)) {
      number <- as.numeric(paste(number, collapse = ""))
    } else{
      number <- 1
    }
    ##first select the Na, Cl and so on element
    idx1 <- gregexpr("[A-Z][a-z][0-9]*", formula)[[1]]
    len1 <- attributes(idx1)$match.length
    ##no double element
    if (idx1[1] == -1) {
      double.formula <- matrix(NA, ncol = 2)
      formula1 <- formula
    } else{
      double.letter.element <- NULL
      double.number <- NULL
      remove.idx <- NULL
      for (i in 1:length(idx1)) {
        double.letter.element[i] <-
          substr(formula, idx1[i], idx1[i] + len1[i] - 1)
        if (nchar(double.letter.element[i]) == 2) {
          double.number[i] <- 1
        } else{
          double.number[i] <-
            as.numeric(substr(
              double.letter.element[i],
              3,
              nchar(double.letter.element[i])
            ))
        }
        double.letter.element[i] <-
          substr(double.letter.element[i], 1, 2)
        remove.idx <-
          c(remove.idx, idx1[i]:(idx1[i] + len1[i] - 1))
      }

      double.formula <- data.frame(double.letter.element,
                                   double.number, stringsAsFactors = FALSE)
      formula1 <- strsplit(formula, split = "")[[1]]
      formula1 <- formula1[-remove.idx]
      formula1 <- paste(formula1, collapse = "")
    }

    ## no one element
    if (formula1 == "") {
      one.formula <- matrix(NA, ncol = 2)
    } else{
      idx2 <- gregexpr("[A-Z][0-9]*", formula1)[[1]]
      len2 <- attributes(idx2)$match.length
      one.letter.element <- NULL
      one.number <- NULL
      for (i in 1:length(idx2)) {
        one.letter.element[i] <-
          substr(formula1, idx2[i], idx2[i] + len2[i] - 1)
        if (nchar(one.letter.element[i]) == 1) {
          one.number[i] <- 1
        } else{
          one.number[i] <-
            as.numeric(substr(one.letter.element[i], 2, nchar(one.letter.element[i])))
        }
        one.letter.element[i] <-
          substr(one.letter.element[i], 1, 1)
      }
      one.formula <-
        data.frame(one.letter.element, one.number,
                   stringsAsFactors = FALSE)
    }

    colnames(double.formula) <-
      colnames(one.formula) <- c("element.name", "number")
    formula <- rbind(double.formula, one.formula)
    formula <-
      formula[!apply(formula, 1, function(x)
        any(is.na(x))), ]

    formula <- formula[order(formula$element.name), ]
    formula$number <- formula$number * number
    unique.element <- unique(formula$element.name)
    if (length(unique.element) == nrow(formula)) {
      return(formula)
    } else{
      formula <- lapply(unique.element, function(x) {
        formula[formula$element.name == x, , drop = FALSE]
      })

      formula <- lapply(formula, function(x) {
        data.frame(unique(x$element.name),
                   sum(x$number),
                   stringsAsFactors = FALSE)
      })

      formula <- do.call(rbind, formula)
      colnames(formula) <- c("element.name", "number")
      return(formula)
    }
  }
)

###positive
### [M+H] 50
### [M+H] isotope 20
### other adduct 20
### other adduct 10

###negative
### [M-H] 50
### [M-H] isotope 20
### other adduct 20
### other adduct 10

score_peak_group <- function(peak_group){
score <- 0

if(any(peak_group$Adduct == "(M+H)+")){
  score <- score + 50
}

if(any(peak_group$Adduct == "(M+H)+" & peak_group$isotope != "[M]")){
  score <- score + 20
}

if(any(peak_group$polarity == "positive" & peak_group$Adduct != "(M+H)+")){
  score <- score + 20
}

if(any(peak_group$polarity == "positive" & peak_group$Adduct != "(M+H)+" &
       peak_group$isotope != "[M]")){
  score <- score + 10
}

if(any(peak_group$Adduct == "(M-H)-")){
  score <- score + 50
}

if(any(peak_group$Adduct == "(M-H)-" & peak_group$isotope != "[M]")){
  score <- score + 20
}

if(any(peak_group$polarity == "negative" & peak_group$Adduct != "(M-H)-")){
  score <- score + 20
}


if(any(peak_group$polarity == "negative" & peak_group$Adduct != "(M-H)-" &
       peak_group$isotope != "[M]")){
  score <- score + 10
}

return(score)
}


calculate_redundance <- function(x){
x <-
  dplyr::bind_rows(x)
x1 <-
  x %>%
  dplyr::group_by(Lab.ID) %>%
  dplyr::distinct(compound_class) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::pull(n) %>%
  mean()
x2 <-
  x %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::pull(n) %>%
  mean()
c(x1, x2)
}
