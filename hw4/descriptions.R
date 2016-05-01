# Load the descriptions file, parse it and make sure the levels
# match those in persado_data.csv.
LoadDescriptions <- function(dat, desc.file="persado_descriptions.csv") {
  raw <- read.csv(desc.file)
  raw <- raw[-nrow(raw),]
  msg <- ldply(strsplit(as.character(raw$MESSAGE), " [*]"))
  
  parsed <- data.frame(intro=rep(NA, nrow(raw)))
  
  # Intro
  parsed$intro <- factor(
    msg$V1, levels=levels(factor(msg$V1))[c(3, 4, 1, 2)])
  
  # Headline
  parsed$headline <- factor(
    msg$V2, levels=levels(factor(msg$V2))[c(4, 1, 3, 2)])
  
  # Main Text
  parsed$main_text <- factor(msg$V3, levels=levels(factor(msg$V3))[c(2,1)])
  
  # Button
  parsed$button <- "end"
  parsed$button[grep("^('>>|▶)", msg$V4)] <- "beginning"
  parsed$button <- factor(
    parsed$button, levels = c("end", "beginning"))
  
  # Action
  require(stringr)
  msg.no.symbol <- str_trim(gsub("('>>|▶)", "", msg$V4))
  
  parsed$action <- "none"
  parsed$action[grep("^Click", msg.no.symbol)] <- "click"
  parsed$action <- factor(parsed$action, levels = c("none", "click"))
  
  # Purpose
  msg.no.action <- str_trim(gsub("Click Here to", "", msg.no.symbol))
  parsed$purpose <- factor(
    msg.no.action, levels=levels(factor(msg.no.action))[c(3, 1, 4, 2)])
  
  
  # Symbol
  parsed$symbol <- "triangle"
  parsed$symbol[grep("'>>", msg$V4)] <- "caret"
  parsed$symbol <- factor(parsed$symbol, levels=c("triangle", "caret"))
  
  TestDescOrder(dat, parsed, 
                c('intro', 'headline', 'main_text', 
                  'button', 'action', 'purpose', 'symbol'),
                verb=F)
  return(parsed)
}

# Verify the description levels from dat match desc
TestDescOrder <- function(dat, desc, exp.cols, verb=T) {
  for (col in exp.cols) {
    if (verb) cat(sprintf("%s,", col))
    if (!all(as.numeric(desc[,col]) == as.numeric(dat[,col]))) {
      print(col)
      print(as.numeric(desc[,col]))
      print(as.numeric(dat[,col]))
      stop("cannot continue, levels don't match")
    }
  }
  if (verb) cat("  SUCCESS\n")
}


CreateMessageStrings <- function(dat, desc, show.more=F) {
  # Extract a bunch of strings
  intro <- levels(desc$intro)[as.numeric(dat$intro)]
  headline <- levels(desc$headline)[as.numeric(dat$headline)]
  main_text <- levels(desc$main_text)[as.numeric(dat$main_text)]
  action <- c("", "Click Here to")[as.numeric(dat$action)]
  purpose <- levels(desc$purpose)[as.numeric(dat$purpose)]
  symbol <- c("▶", "'>>")[as.numeric(dat$symbol)]
  
  # Now form the subject line
  # <symbol> text
  #  or
  # text <symbol>
  subject.text <- paste(action, purpose)
  subject <- Vectorize(function(button, symbol, text) {
    subject <- rep(NA, 2)
    subject[3-as.numeric(button)] <- symbol
    subject[as.numeric(button)] <- text
    paste(subject, collapse=" ")
  })(as.numeric(dat$button), symbol, subject.text)
  
  # Now paste everything together with the separator
  
  msg <- paste(intro, headline, main_text, subject, sep = " *")
  if (show.more) {
    msg <- paste(msg, "Show Me More", sep=" *")
  }
  msg <- gsub("  ", " ", msg)
  return(msg)
}

TestCreateMessageStrings <- function(
  dat, desc, desc.file="persado_descriptions.csv", verb=T) {
  raw <- read.csv(desc.file)
  msgs <- CreateMessageStrings(dat, desc, show.more = T)
  for (i in 1:nrow(dat)) {
    cat(sprintf("%d,", i))
    truth <- raw$MESSAGE[i]
    test <- as.character(msgs[i])
    if (truth != test) {
      stop(sprintf(">>>Expected: %s\n>>>Got: %s", truth, test))
    }
  }
  cat("done.  SUCCESS\n")
}


GetMessageLevels <- function(dat, desc) {
  data.frame(
    intro = levels(desc$intro)[as.numeric(dat$intro)],
    headline = levels(desc$headline)[as.numeric(dat$headline)],
    main_text = levels(desc$main_text)[as.numeric(dat$main_text)],
    button = levels(desc$button)[as.numeric(dat$button)],
    action = c("", "Click Here to")[as.numeric(dat$action)],
    purpose = levels(desc$purpose)[as.numeric(dat$purpose)],
    symbol = c("▶", "'>>")[as.numeric(dat$symbol)]
  )
}
