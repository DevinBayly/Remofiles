crossPartners <- function (data) {
  ##create copy flipped
  data %>%
    group_by(Dyad) %>%
    arrange(Dyad,desc(ID)) -> partner.data
  ##rename
  colnames(partner.data) <- paste(colnames(partner.data),"p",sep="_")
  ##mush back together
  res.df <- data.frame(data,partner.data)
  return (res.df)
}
