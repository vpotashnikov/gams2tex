


# Function that return list rs where every member is gams entity
parse_model_and_all <- function(begin_file, pth = getwd()) {
  rem_str <- function(txt, n = 1) substr(txt, n + 1, nchar(txt))
  substr1 <- function(x, n = 1) substr(x, 1, n)
  find_brack <- function(txt) {
    tt <- cumsum(2 * (strsplit(gsub("[^()]*", "", txt), '')[[1]] == '(') - 1)
    tt <- seq(along = tt)[tt == -1][1]
    return(sum(sapply(strsplit(txt, "[()]")[[1]][1:tt], nchar)) + tt)
  }
  # fl_nm <- dir(dr, '[.]gms$', recursive = TRUE)
  src <- list()
  # names(src) <- gsub('[.].*', '', fl_nm)
  exclude_semicolon <- function(frst_val = NULL, chk = "[;]", ss) {
    excl_sem = list(frst_val = frst_val, chk = chk, ss = ss)
    assign('excl_sem', excl_sem, globalenv())
    # frst_val <- excl_sem$frst_val
    # chk <- excl_sem$chk
    # ss <- excl_sem$ss
    if (!is.null(frst_val)) {
      ss[1] <- substr(ss[1], nchar(frst_val) + 1, nchar(ss[1]))
      if (gsub("[ ]*", "", ss[1]) == "") ss <- ss[-1]
    }
    # Fin exlude '"'
    hh <- grep(chk, ss)[1]
    k <- nchar(gsub(paste(chk, ".*", sep = ""), "", ss[hh])) + 1
    rest <- c(ss[seq(length.out = hh - 1)], substr(ss[hh], 1, k - 1)[k > 1])
    rest <- gsub("^[[:blank:]]*", "", rest)
    if (any(grep('["\']', rest))) {
      s2 <- gsub('["][^"]*["]', "", ss)
      s2 <- gsub("['][^']*[']", "", s2)
      hh <- grep(chk, s2)[1]
      rest <- c(ss[seq(length.out = hh - 1)])
      if (any(grep("['\"]", ss[hh])) && chk == "[/]") {
        # Find chk
        FL <- FALSE
        ff <- ss[hh];
        k <- 0;
        while (!FL) {
          if (substr(ff, 1, 1) == '"') {
            k <- k + nchar(ff) - nchar(sub('^["][^"]*["]', '', ff))
            ff <- sub('^["][^"]*["]', '', ff)
          } else if (substr(ff, 1, 1) == "'") {
            k <- k + nchar(ff) - nchar(sub("^['][^']*[']", '', ff))
            ff <- sub("^['][^']*[']", '', ff)
          } else if (any(grep(chk, substr(ff, 1, 1)))) {
            FL <- TRUE
            k <- k + 1
          } else {
            ff <- rem_str(ff)
            k <- k + 1
          }
        }
        rest <- c(rest, substr(ss[hh], 1, k))
        ss <- c(substr(ss[hh], k + 1, nchar(ss[hh]))[k + 1 < nchar(ss[hh])], ss[-seq(length.out = hh)])
        ss <- gsub("^[[:blank:]]*", "", ss)
      } else {
        k <- nchar(gsub(paste(chk, ".*", sep = ""), "", ss[hh])) + 1
        rest <- c(ss[seq(length.out = hh - 1)], substr(ss[hh], 1, k - 1)[k > 1])
        rest <- gsub("^[[:blank:]]*", "", rest)
        ss <- c(substr(ss[hh], k + 1, nchar(ss[hh]))[k + 1 < nchar(ss[hh])], ss[-seq(length.out = hh)])
        ss <- gsub("^[[:blank:]]*", "", ss)
      }
    } else {
      ss <- c(substr(ss[hh], k + 1, nchar(ss[hh]))[k + 1 < nchar(ss[hh])], ss[-seq(length.out = hh)])
      ss <- gsub("^[[:blank:]]*", "", ss)
    }
    list(src = ss, dst = rest)
  }
  remove_comment <- function(ss) {
    # Remove comments 
    if (any(grep("^[$][oO][nN][Tt][Ee][Xx][Tt]", ss))) 
      ss <- ss[-c(apply(cbind(grep("^[$][oO][nN][Tt][Ee][Xx][Tt]", ss), grep("^[$][oO][Ff][Ff][Tt][Ee][Xx][Tt]", ss)), 1, function(x) x[1]:x[2]), recursive = TRUE)]
    if (any(-grep("^[*]", ss))) ss <- ss[-grep("^[*]", ss)]
    ss <- gsub("[[:blank:]][[:blank:]]*", " ", ss);
    ss <- gsub("^[[:blank:]]*", "", ss);
    # ss <- grep("^[$]", ss, value = TRUE, invert = TRUE)
    ss <- grep("^[$]offorder", ss, value = TRUE, invert = TRUE, ignore.case = TRUE)
    ss <- grep("^[$](on|off)empty[[:blank:]]*$", ss, value = TRUE, invert = TRUE, ignore.case = TRUE)
    if (any(grep("^[$]inlinecom", ss, ignore.case = TRUE))) {
      cat(grep("^[$]inlinecom", ss, ignore.case = TRUE, value = TRUE), "\n")
      ss <- gsub("[{][{].*[}][}]", "", ss)
      ss <- grep("^[$]inlinecom", ss, value = TRUE, invert = TRUE, ignore.case = TRUE)
    }
    ss <- grep("^[ ]*$", ss, value = TRUE, invert = TRUE)
    if (any(grep('$exit', ss))) ss <- ss[1:grep('$exit', ss)[1]]
    ff <- rev(grep('^[[:blank:]]*(loop)', ss, ignore.case = TRUE)); ff <- ff[ff != 1];
    if (length(ff) > 0) {
      for(i in ff - 1) {
        ss <- c(ss[1:i], ';', ss[-(1:i)])
      }
    }
    ss <- gsub('[!][!].*', '', ss)
    ss
  }
  src[[begin_file]] <- readLines(paste(pth, begin_file, sep = ''))
  rs <- list()
  ss <- src[[begin_file]] 
  ss <- remove_comment(ss)
  setglobal <- list()
  set <- list()
  include_log <- list()
  while (length(ss) != 0) {
    HAVE_STEP <- FALSE
    # remove trash
    while (any(grep("^[[:blank:]]*;", ss[1L])) || ss[1L] == "") {
      ss[1L] <- gsub("^[[:blank:]]*;", "", ss[1L])
      if (ss[1L] == "") ss <- ss[-1L]
    }
    # variable definition
    frst_val <- tolower(gsub("[( ].*", "", ss[1L]))
    if (frst_val %in% c('free', 'positive', 'nonnegative')) {
      if (any(grep('(free|positive|nonnegative)[[:blank:]]*variable(s|)([[:blank:]]|$)', ss[1L], ignore.case = TRUE))) {
        frst_val <- paste(frst_val, 'variable')
      }
    }
    par_nm <- c("parameter", "variable", "nonnegative variable", "positive variable", "free variable", "equation", "set", "scalar", "model")
    par_nm2 <- c(par_nm, "table")
    for (ll in par_nm) {
      if (frst_val %in% paste(ll, c('', 's'), sep = '')) {
        HAVE_STEP <- TRUE
        # ss <- remove_comment(src$decl_sets); frst_val <- tolower(gsub("[( ].*", "", ss[1L])); # debug data
        ss[1L] <- gsub(paste('^', ll, '(|s)[[:blank:]]*', sep = ''), '', ss[1L], ignore.case = TRUE);
        ss <- ss[ss != '']
        jj <- exclude_semicolon(ss = ss); ss <- jj$src; rest <- jj$dst;
        while (length(rest) != 0) {
          #cat('rest: "', rest[1], '"\n', sep = '')
          nm <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub("[[:alnum:]_]*", "", rest[1])))
          if (any(tolower(nm) == c('set', 'alias', 'parameter', 'free', 'nonnegative', 'positive'))) {
            # stop('(set|alias|parameter|free|nonnegative|positive)')
            ss <- c(rest, ss); rest <- NULL
          } else {
            arg <- NULL; desc <- NULL; desc <- NULL; init <- NULL;
            rest[1] <- substr(rest[1], nchar(nm) + 1, nchar(rest[1]))
            rest[1] <- gsub("^[ ]*", "", rest[1])
            if (substr(rest[1], 1, 1) == "(") {
              arg <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub("[(][[:alnum:]_,* ]*[)]", "", rest[1])))
              rest[1] <- substr(rest[1], nchar(arg) + 1, nchar(rest[1]))
              rest[1] <- gsub("^[ ]*", "", rest[1])
            }
            if (any(grep("^[[:alnum:]_%()]", rest[1]))) {
              desc <- substr(rest[1], 1, nchar(sub("[,/].*", "", rest[1])))
              rest[1] <- substr(rest[1], nchar(desc) + 1, nchar(rest[1]))
              rest[1] <- gsub("^[ ]*", "", rest[1])
            } else if (any(grep("^[']", rest[1]))) {
              desc <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub("['][^']*[']", "", rest[1])))
              rest[1] <- substr(rest[1], nchar(desc) + 1, nchar(rest[1]))
              rest[1] <- gsub("^[ ]*", "", rest[1])
            } else if (any(grep('^["]', rest[1]))) {
              desc <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub('["][^"]*["]', "", rest[1])))
              rest[1] <- substr(rest[1], nchar(desc) + 1, nchar(rest[1]))
              rest[1] <- gsub("^[ ]*", "", rest[1])
            }
            while (length(rest) >= 1 && rest[1L] == "") rest <- rest[-1L]
            if (any(grep("^[/]", rest[1]))) {
              # exclude_semicolon(frst_val = "/", chk = "[/]", src = "rest", dst = "rest2")
              jj <- exclude_semicolon(frst_val = "/", chk = "[/]", ss = rest); rest <- jj$src; rest2 <- jj$dst;
              init <- rest2
            }
            while (length(rest) >= 1 && (rest[1L] == "" || any(grep("^[,[:blank:]]", rest[1L])))) {
              if (rest[1L] == "") rest <- rest[-1L] else rest <- c(gsub("^[,[:blank:]]*", "", rest[1L]), rest[-1L])
            }
            if (nm == '') stop("nm = ''")
            rs[[nm]] <- list(name = nm, arg = arg, desc = desc, type = ll, init = init)
            if (any(grep(paste("^(", paste(par_nm2, collapse = "|"), ")", sep = ""), rest[1L], ignore.case = TRUE))) {
              ss <- c(rest, ";", ss)
              rest <- NULL
            }
          }
        }
      }
    }
    # Table
    if (!HAVE_STEP && frst_val %in% paste("table", c('', 's'), sep = '')) {
      HAVE_STEP <- TRUE
      # exclude_semicolon(frst_val = frst_val, ss = ss)
      jj <- exclude_semicolon(frst_val = frst_val, ss = ss); ss <- jj$src; rest <- jj$dst;
      nm <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub("[[:alnum:]_%]*", "", rest[1])))
      arg <- NULL; desc <- NULL; desc <- NULL; init <- NULL;
      rest[1] <- substr(rest[1], nchar(nm) + 1, nchar(rest[1]))
      rest[1] <- gsub("^[ ]*", "", rest[1])
      if (substr(rest[1], 1, 1) == "(") {
        arg <- substr(rest[1], 1, nchar(rest[1]) - nchar(sub("[(][[:alnum:]_,]*[)]", "", rest[1])))
        rest[1] <- substr(rest[1], nchar(arg) + 1, nchar(rest[1]))
        rest[1] <- gsub("^[ ]*", "", rest[1])
      }
      if (any(grep("^[[:alnum:]_]", rest[1]))) {
        desc <- substr(rest[1], 1, nchar(sub("[,/].*", "", rest[1])))
        rest[1] <- substr(rest[1], nchar(desc) + 1, nchar(rest[1]))
        rest[1] <- gsub("^[ ]*", "", rest[1])
      }
      while (length(rest) >= 1 && rest[1L] == "") rest <- rest[-1L]
      if (any(grep("^[/]", rest[1]))) {
        jj <- exclude_semicolon(frst_val = "/", chk = "[/]", ss = rest); rest <- jj$src; rest2 <- jj$dst;
        init <- rest2
      } else {
        mm <- grep(paste("^(", paste(par_nm2, collapse = "|"), ")", sep = ""), gsub("^[[:blank:]]*", "", rest), ignore.case = TRUE)
        if (any(mm)) {
          mm <- mm[1]
          ss <- c(rest[mm:length(rest)], ";", ss)
          rest <- rest[-(mm:length(rest))]
        }
        init <- rest
        rest <- NULL
      }
      while (length(rest) >= 1 && (rest[1L] == "" || any(grep("^[,[:blank:]]", rest[1L])))) {
        if (rest[1L] == "") rest <- rest[-1L] else rest <- c(gsub("^[,[:blank:]]*", "", rest[1L]), rest[-1L])
      }
      rs[[nm]] <- list(name = nm, arg = arg, desc = desc, type = "table", init = init)
      if (any(grep(paste("^(", paste(par_nm, collapse = "|"), ")", sep = ""), rest[1L], ignore.case = TRUE))) {
        ss <- c(rest, ";", ss)
        rest <- NULL
      }
      
    }
    # Try find include macros
    if (!HAVE_STEP) {
      if (any(grep("^[$](bat|)include[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
        #stop('include  include  include')
        hh <- sub('[[:blank:]]*$', '', sub("^[$]include[[:blank:]]+", "", ss[1L], ignore.case = TRUE))
        if (!any(grep("^[$]batinclude", ss[1L], ignore.case = TRUE)) && nchar(gsub("[[:blank:]].*", "", hh)) != nchar(hh)) stop("include macros: 2")
        #hh <- gsub("[[:blank:]]*", "", gsub("[.][[:alnum:]]*$", "", hh))
        hh <- gsub('^[.][/]', '', gsub('[\\]', '/', tolower(hh)))
        if (all(tolower(names(src)) != tolower(hh))) {
          src[[hh]] <- readLines(paste(pth, hh, sep = ''))
        }
        if (any(tolower(names(src)) == tolower(hh))) {
          include_log[[length(include_log) + 1]] <- hh
          cat('--- include: ', hh, '\n', sep = '')
          ss <- c(remove_comment(src[[seq(along = src)[tolower(names(src)) == tolower(hh)]]]), ';', '### end file', ss[-1L])
        }  else stop("include macros: 3")
        HAVE_STEP <- TRUE
      }
    }
    # Try find exit macros
    if (!HAVE_STEP) {
      if (any(grep("^[$]exit", ss[1L], ignore.case = TRUE))) {
        if (any(grep('^[#][#][#] end file', ss))) {
          ss <- ss[-(1:grep('[#][#][#] end file', ss)[1])]
        } else {
          ss <- NULL
        }
        HAVE_STEP <- TRUE
      }
    }    
    # if
    if (!HAVE_STEP) {
      if (any(grep("^[$]if[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
        cat('if ##### : ', ss[1L], '\n')
        # setglobal
        if (any(grep('^[$]if[[:blank:]]+(not[[:blank:]]+|)setglobal[[:blank:]]+', ss[1L], ignore.case = TRUE))) {
          not_us <- (any(grep('^[$]if[[:blank:]]+not[[:blank:]]+setglobal[[:blank:]]+', ss[1], ignore.case = TRUE)))
          ss[1L] <- gsub('^[$]if[[:blank:]]+(not[[:blank:]]+|)setglobal[[:blank:]]+', '', ss[1L], ignore.case = TRUE)
          nm <- gsub('[[:blank:]]+.*', '', ss[1L])
          if (any(tolower(names(setglobal)) == tolower(nm)) == not_us) {
            cat('warning, not use setglobal: ', ss[1L], '\n')
            ss <- ss[-1L]
          } else {
            ss[1L] <- gsub('^[[:blank:]]*', '', rem_str(ss[1L], nchar(nm)))
          }
        } else if (any(grep('^[$]if[[:blank:]]+(not[[:blank:]]+|)set[[:blank:]]+', ss[1], ignore.case = TRUE))) {
          not_us <- (any(grep('^[$]if[[:blank:]]+not[[:blank:]]+set[[:blank:]]+', ss[1], ignore.case = TRUE)))          
          ss[1L] <- gsub('^[$]if[[:blank:]]+(not[[:blank:]]+|)set[[:blank:]]+', '', ss[1L], ignore.case = TRUE)
          nm <- gsub('[[:blank:]]+.*', '', ss[1L])
          if (any(tolower(names(set)) == tolower(nm)) == not_us) {
            cat('warning, not use set: ', ss[1L], '\n')
            ss <- ss[-1L]
          } else {
            ss[1L] <- gsub('^[[:blank:]]*', '', rem_str(ss[1L], nchar(nm)))
          }
        } else if (any(grep("^[$]if[[:blank:]]+[%]", ss[1L]))) {
          fl <- TRUE
          lgl <- gsub("^[$]if[[:blank:]]+", '', ss[1L])
          rst <- gsub('[%][^%]*[%]', '', lgl)
          arg1 <- gsub('[%]', '', substr(lgl, 1, nchar(lgl) - nchar(rst)))
          lgl <- rst
          val <- ''
          if (any(tolower(names(set)) == tolower(arg1))) {
            val <- tolower(set[tolower(names(set)) == tolower(arg1)][1])
          } else if (any(tolower(names(setglobal)) == tolower(arg1))) {
            val <- tolower(setglobal[tolower(names(setglobal)) == tolower(arg1)][[1]])
          } else fl <- FALSE
          opr <- gsub('[^=<>].*$', "", gsub("^[[:blank:]]*", "", lgl))
          if (all(opr != c('=='))) stop('if 1335')
          lgl <- gsub('[[:blank:]]+.*', '', gsub('[[:blank:]]*[=><]*[[:blank:]]*', '', lgl))
          if (any(grep('[%]', lgl))) stop('if 125376')
          if ((fl && tolower(lgl) == val) || (val == 'no' && !fl) || (val == 'yes' && fl)) {
            cat('warnings, if - yes: "', ss[1L], '"\n', sep = '')
            stop('if 3423')
            ss <- ss[-c(1, grep('^[$]endif', ss)[1])];
          } else {
            cat('warnings, if - no: "', ss[1L], '"\n', sep = '')
            ss <- ss[-1L]
          }
        } else stop('if 1')
        #cat('warnings if: "', ss[1L], '"\n', sep = '')
        HAVE_STEP <- TRUE
      }
  }
  # goto
  if (any(grep('^[$]goto', ss[1L]))) {
      # stop('goto')
      ff <- gsub('[[:blank:]]*', '', gsub('^[$]goto[[:blank:]]+', '', ss[1L]))
      ff <- grep(paste('^[$]label[[:blank:]]+', ff, '[[:blank:]]*$', sep = ''), ss, ignore.case = TRUE)
      if (length(ff) != 1) stop('goto 1')
      ss <- ss[-(1:ff)]
      HAVE_STEP <- TRUE
  }
    # setglobal
    if (!HAVE_STEP) {
      if (any(grep("^[$]setglobal[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
        cat('warnings setglobal: "', ss[1L], '"\n', sep = '')
        nm <- gsub('^[$]setglobal[[:blank:]]+', '', ss[1L])
        setglobal[[gsub('[[:blank:]]+.*', '', nm)]] <- gsub('[[:alnum:]]*[[:blank:]]+', '', nm)
        ss <- ss[-1L]
        HAVE_STEP <- TRUE
      }
    }
    # set
    if (!HAVE_STEP) {
      if (any(grep("^[$]set[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
        cat('warnings set: "', ss[1L], '"\n', sep = '')
        nm <- gsub('^[$]set[[:blank:]]+', '', ss[1L])
        set[[gsub('[[:blank:]]+.*', '', nm)]] <- gsub('[[:alnum:]]*[[:blank:]]+', '', nm)
        ss <- ss[-1L]
        HAVE_STEP <- TRUE
      }
    }
    # ifthen
    if (!HAVE_STEP) {
      if (any(grep("^[$]ifthen[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
        fl <- TRUE
        lgl <- gsub("^[$]ifthen[[:blank:]]+", '', ss[1L])
        if (substr(lgl, 1, 1) == '%') {
          rst <- gsub('[%][^%]*[%]', '', lgl)
          arg1 <- gsub('[%]', '', substr(lgl, 1, nchar(lgl) - nchar(rst)))
          lgl <- rst
          val <- ''
          if (any(tolower(names(set)) == tolower(arg1))) {
            val <- tolower(set[[tolower(names(set)) == tolower(arg1)]])
          } else if (any(tolower(names(setglobal)) == tolower(arg1))) {
            val <- tolower(set[[tolower(names(setglobal)) == tolower(arg1)]])
          } else fl <- FALSE
        } else stop('ifthen 124')
        opr <- gsub('[^=<>]*$', "", gsub("^[[:blank:]]*", "", lgl))
        if (all(opr != c('=='))) stop('ifthen')
        lgl <- gsub('[[:blank:]]*[=><]*[[:blank:]]*', '', lgl)
        if (any(grep('[%]', lgl))) stop('ifthen 125')
        if ((fl && tolower(lgl) == val) || (val == 'no' && !fl) || (val == 'yes' && fl)) {
          cat('warnings, ifthen - yes: "', ss[1L], '"\n', sep = '')
          ss <- ss[-c(1, grep('^[$]endif', ss)[1])];
        } else {
          cat('warnings, ifthen - no: "', ss[1L], '"\n', sep = '')
          ss <- ss[-(1:grep('^[$]endif', ss)[1])]
        }
        HAVE_STEP <- TRUE
      }
    }
    # File work
    if (!HAVE_STEP && any(grep("^file[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
      nm <- gsub('[[:blank:]]+.*', '', gsub('^file[[:blank:]]+', '', ss[1L], ignore.case = TRUE))
      rs[[nm]] <- list(name = nm, type = 'file', do = ss[1L]);
      ss <- ss[-1L]
      HAVE_STEP <- TRUE
    }    
    # put 
    if (!HAVE_STEP && any(grep("^put[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
      nm <- gsub('[;[:blank:]]+.*', '', gsub('^put[[:blank:]]+', '', ss[1L], ignore.case = TRUE))
      if (any(tolower(names(rs)) == tolower(nm))) {
        nm <- names(rs)[tolower(names(rs)) == tolower(nm)]
        last_put <- nm; 
        ss[1L] <- rem_str(ss[1L], nchar(gsub('[;].*', '', ss[1L])) + 1)
        if (any(grep("^[[:blank:]]*$", ss[1L]))) ss <- ss[-1L]
      } else {
        jj <- exclude_semicolon(chk = '[;]', ss = ss);
        rs[[last_put]]$put <- c(rs[[last_put]]$put, jj$dst)
        ss <- jj$src
        # stop('put --t')
      }
      HAVE_STEP <- TRUE
    }
    # putclose 
    if (!HAVE_STEP && any(grep("^putclose[[:blank:]]+", ss[1L], ignore.case = TRUE))) {
      cat('warning, putclose: "', ss[1L], '"\n', sep = '')
      ss <- ss[-1L]
      HAVE_STEP <- TRUE
    }
    # remove end file mark 
    if (!HAVE_STEP) {
      if (any(grep("[#][#][#] end file", ss[1L], ignore.case = TRUE))) {
        ss <- ss[-1L]
        HAVE_STEP <- TRUE
      }
    }
    # alias 
    if (!HAVE_STEP) {
      if (any(grep("^alias", ss[1L], ignore.case = TRUE))) {
        hh <- gsub("^alias[[:blank:]]*", "", ss[1L], ignore.case = TRUE)
        sp <- strsplit(hh, "[(]")[[1]]
        sp <- sp[sp != ""]
        for (i in seq(along = sp)) {
          psb_nm <- gsub("[[:blank:]]", "", gsub("[);].*", "", strsplit(sp[i], ',')[[1]]))
          als_ls <- lapply(names(rs)[sapply(rs, function(x) x$type == 'set')], function(x) c(x, rs[[x]]$alias))
          names(als_ls) <- names(rs)[sapply(rs, function(x) x$type == 'set')]
          nm <- gsub("[,].*", "", sp[i])
          if (any(tolower(c(als_ls, recursive = TRUE)) %in% tolower(psb_nm))) {
            nm <- names(als_ls) [sapply(als_ls, function(x) any(tolower(x) %in% tolower(psb_nm)))]
            psb_nm <- psb_nm[tolower(psb_nm) != tolower(nm)]
            if (all(names(rs[[nm]]) != 'alias')) rs[[nm]]$alias <- c()
            rs[[nm]]$alias <- unique(c(rs[[nm]]$alias, psb_nm))
          } else stop('-- alias --')
        }
        ss <- ss[-1L]    
        HAVE_STEP <- TRUE
      }
    }  
    # option 
    if (!HAVE_STEP) {
      if (any(grep("^option", ss[1L], ignore.case = TRUE))) {
        ss <- ss[-1L]    
        HAVE_STEP <- TRUE
      }
    }    
    # loop 
    if (!HAVE_STEP) {
      # cat(ss[1L], '\n')
      if (any(grep("^loop[[:blank:](]+", ss[1L], ignore.case = TRUE)) || any(grep("^if[[:blank:](]+", ss[1L], ignore.case = TRUE))) {
        if (any(grep("^loop", ss[1L], ignore.case = TRUE))) CC <- "^loop[[:blank:]]*[(]"
        if (any(grep("^if", ss[1L], ignore.case = TRUE))) CC <- "^if[[:blank:]]*[(]"
        if (!any(grep(CC, ss[1], ignore.case = TRUE))) stop("loop check")
        ss[1] <- gsub(CC, "", ss[1], ignore.case = TRUE)
        nn <- paste(ss, collapse = "")
        k <- 1;
        while (k != 0) {
          nn <- substr(nn, nchar(gsub("[)(].*", "", nn)) + 1, nchar(nn)) 
          if (substr(nn, 1, 1) == "(") k <- k + 1 else  if (substr(nn, 1, 1) == ")") k <- k - 1 else stop("(((())))")
          nn <- substr(nn, 2, nchar(nn)) 
        }
        kk <- nchar(paste(ss, collapse = "")) - nchar(nn)
        while (kk != 0) {
          if (nchar(ss[1]) <= kk) {
            kk <- kk - nchar(ss[1])
            ss <- ss[-1] 
          } else {
            ss[1] <- substr(ss[1], kk + 1, nchar(ss[1]))
            kk <- 0
          }
        }
        HAVE_STEP <- TRUE
      }
    }  
    # display 
    if (!HAVE_STEP) {
      if (any(grep("^(display|execute_loadpoint|solve)", ss[1L], ignore.case = TRUE))) {
        #exclude_semicolon(chk = "[;]", src = ss, dst = "rest")
        jj <- exclude_semicolon(chk = "[;]", ss = ss); ss <- jj$src; rest <- jj$dst;
        HAVE_STEP <- TRUE
      }
    }  
    # Check for statements
    if (!HAVE_STEP) {
      frst_val <- gsub("[.(= ].*", "", ss[1L])
      if (any(tolower(names(rs)) == tolower(frst_val))) {
        nm <- names(rs)[tolower(names(rs)) == tolower(frst_val)]
        if (rs[[nm]]$type == 'equation') {
          # exclude_semicolon(chk = "[;]", src = "ss", dst = "rest")
          jj <- exclude_semicolon(chk = "[;]", ss = ss); ss <- jj$src; rest <- jj$dst;
          if (!any(grep(paste('^[[:blank:]]*', nm, '[.]scale', sep = ''), rest[1], ignore.case = TRUE))) {
            if (!is.null(rs[[nm]]$declar)) {
              if (!is.null(rs[[nm]]$declar2)) stop('declar2')
              rs[[nm]]$declar2 <- rest
              cat('declar\n')
            } else rs[[nm]]$declar <- rest 
          }
        } else {
          # exclude_semicolon(chk = "[;]", src = "ss", dst = "rest")
          jj <- exclude_semicolon(chk = "[;]", ss = ss); ss <- jj$src; rest <- jj$dst;
          mm <- grep(paste("^(", paste(par_nm2, collapse = "|"), ")", sep = ""), gsub("^[[:blank:]]*", "", rest), ignore.case = TRUE)
          if (any(mm)) {
            mm <- mm[1]
            ss <- c(rest[mm:length(rest)], ";", ss)
            rest <- rest[-(mm:length(rest))]
          }
          
          if (all(names(rs[[nm]]) != 'do')) rs[[nm]]$do <- list()
          rs[[nm]]$do[[length(rs[[nm]]$do) + 1]] <- rest
        }
        HAVE_STEP <- TRUE
      }
    }
    # Try find exit macros
    if (!HAVE_STEP) {
      if (any(grep("^[$](call|gdxin|load|gdxrw|offlisting|onmulti|offmulti|onlisting|onsymlist|offsymlist|label|offeolcom|oneolcom)", ss[1L], ignore.case = TRUE))) {
        cat(ss[1], '\n')
        ss <- ss[-1L]
        HAVE_STEP <- TRUE
      }
    }
    # Macro
    if (!HAVE_STEP) {
      if (any(grep("^[$](macro)", ss[1L], ignore.case = TRUE))) {
        tmp <- gsub('^[$]macro[[:blank:]]*', '', ss[1L], ignore.case = TRUE)
        nm <- gsub('[[:blank:]]+.*', '', tmp)
        rs[[nm]] <-  list(name = nm, val = substr(tmp, nchar(nm)+1, nchar(tmp)), type = "macro")
        ss <- ss[-1L]
        HAVE_STEP <- TRUE
      }
    }    
    if (!HAVE_STEP) stop(paste("Unknown condition\n", ss[1], sep = ""))
  }
  return(rs)
}


# Generate table that consist gams entity from  "parse_model_and_all"
generate_table <- function(rs) {
  dtf <- data.frame(name = character(), dim = character(), full_name = character(), 
                    desc = character(), type = character(), alias = character(), init = character(), stringsAsFactors = FALSE)
  if (length(rs) > 0) {
    dtf[1:length(rs), ] <- ''
    dtf[, 'name'] <- sapply(rs, function(x) x$name)
    dtf[, 'type'] <- sapply(rs, function(x) x$type)
    dtf[, 'desc'] <- sapply(rs, function(x) if (is.null(x$desc)) '' else x$desc)
    dtf[, 'full_name'] <- dtf[, 'name']
    fl <- sapply(rs, function(x) !is.null(x$arg))
    dtf[fl, 'full_name'] <- paste(dtf[fl, 'full_name'], sapply(rs[fl], function(x) x$arg), sep = '')
    dtf[fl, 'dim'] <- sapply(rs[fl], function(x) x$arg)
    dtf[fl, 'ndim'] <- sapply(rs[fl], function(x) length(strsplit(x$arg, ',')[[1]]))
    dtf[!fl, 'ndim'] <- 0
    dtf[, 'init'] <- sapply(rs, function(x) if (is.null(x$init)) '' else paste(x$init, collapse = ', '))
    fl <- sapply(rs, function(x) !is.null(x$alias))
    dtf[fl, 'alias'] <- sapply(rs[fl], function(x) paste0(x$alias, collapse = ","))
  }
  dtf
} 


# Export to tex all set, table, parameter and variable
export_to_rtf_parameter <- function(rs, file_nm, width = 12, height = 8.5, font.size = 10, omi = c(1, 1, 1, 1)) {
  vv <- generate_table(rs)[, c("full_name", "desc", "type", "alias"), drop = FALSE]
  
  rtf <- RTF(file_nm, width = width, height = height, font.size = font.size, omi = omi)
  
  for (i in unique(vv$type)[unique(vv$type) %in% c("set", "scalar", "parameter", "table", "free variable", 
                                                   "positive variable", "nonnegative variable" , "variable")]) {
    addParagraph(rtf, paste0("List of ", i))
    addParagraph(rtf, "")
    addTable(rtf, vv[vv$type == i, c("full_name", "desc", "alias"[i == 'set'])])
    addParagraph(rtf, "")
    addPageBreak(rtf, width = width, height = height, font.size = font.size, omi = omi)
  }
  done(rtf)
  shell.exec(file_nm)
}


# Export to tex all set, table, parameter and variable by equation
export_to_rtf_parameter_by_equation <- function(rs, file_nm, width = 12, height = 8.5, 
                                                font.size = 10, omi = c(1, 1, 1, 1), 
                                                out_set = TRUE, out_parameter = TRUE, out_variable = TRUE, out_equation = TRUE) {
  # Find data about equation 
  zz <- generate_table(rs)
  alias_map <- list()
  fra <- zz[zz$alias != "", ]
  kk <- strsplit(tolower(fra[, "alias"]), ",")
  for (i in seq(length.out = nrow(fra))) {
    for (j in kk[[i]]) {
      alias_map[[j]] <- tolower(fra[i, "name"])
  }}
  # Get equation list
  eq <- zz[zz$type == 'equation',, drop = FALSE]
  ee <- eq[1, 1]
  rr <- list()
  for (ee in eq[, 1]) {
    dcl <- rs[[ee]]$declar
    lst <- unique(strsplit(gsub("[.][LlUuFfMm][PpOo]*", "", gsub("[.][.]", "", gsub("[# ]+", "#", 
                                                                      gsub("[()*--+/=,$^]", "#", gsub("=[EeLlGgNn]=", "=", 
      gsub("[[:blank:]]+", " ", gsub("['][^'\"]*[']", " ", gsub("[\"][^'\"]*[\"]", " ", 
       paste0(dcl, collapse = ' '))))))))), "#")[[1]])
    lst <- tolower(lst[!(tolower(lst) %in% c(ee, "ord", "smin", "smax", "ge", "and", "or", "card", "sameas", 
                                             "sum", "prod", "sqr", "gt", "eq", "ne", "", "lt", "le", "", "abs", "not", "lc_init"))])
    lst <- grep("^[0-9.]", lst, value = TRUE, invert = TRUE)
    no_data <- lst[!(lst %in% c(names(alias_map), tolower(zz[, 1])))]
    rr[[ee]] <- list(use = lst, no_data = no_data)
  }
  # export to rtf
  tt <- zz[, c("name", "full_name", "desc", "type", "alias"), drop = FALSE]
  rtf <- RTF(file_nm, width = width, height = height, font.size = font.size, omi = omi)
  
  for (ee in names(rr)) { 
    cat(ee, '\n')
    addParagraph(rtf, paste0("Information about equation \"", rs[[ee]]$name, "\""))
    addParagraph(rtf, paste0(rs[[ee]]$desc, "\""))
    if (out_equation) addParagraph(rtf, paste0(rs[[ee]]$declar, colapse = 'n'))
    vv <- tt[tolower(tt$name) %in% rr[[ee]]$use,, drop = FALSE]
    for (i in unique(vv$type)[unique(vv$type) %in% c("set"[out_set], "scalar"[out_parameter], "parameter"[out_parameter],
                                                     "table"[out_parameter], "free variable"[out_variable], 
                                                     "positive variable"[out_variable], "nonnegative variable"[out_variable], 
                                                     "variable"[out_variable])]) {
      addParagraph(rtf, paste0("List of ", i))
      addParagraph(rtf, "")
      addTable(rtf, vv[vv$type == i, c("full_name", "desc", "alias"[i == 'set'])])
      if (length(rr[[ee]]$no_data) != 0) 
        addParagraph(rtf, paste0(" No data abour \"", paste0(rr[[ee]]$no_data, collapse = '", "'), "\""))
      addParagraph(rtf, "")
    }
    addPageBreak(rtf, width = width, height = height, font.size = font.size, omi = omi)
  }
  done(rtf)
  shell.exec(file_nm)
}

