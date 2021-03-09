binNumAtLen <- function(num.at.len,len,bins) {
  freq <- tapply(num.at.len, cut(len, breaks = bins,
                                    right = FALSE), function(x) {
                                      sum(x, na.rm = T)
                                    })
  freq[is.na(freq)] <- 0
  names(freq) <- paste0("L",bins[-1])
  freq
}
