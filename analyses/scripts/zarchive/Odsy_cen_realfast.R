### Let's run some real models in Odyssey! W00!
## Cat - 6 June 2018
# Looking to do main model without interactions to start - just using brms for efficiency and accuracy

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Libraries
library(brms)

#### get the data
bb<-read.csv("/n/wolkovich_lab/Lab/Cat/bb_latprep.csv", header=TRUE)

bb$nao.z <- (bb$m.index-mean(bb$m.index,na.rm=TRUE))/(2*sd(bb$m.index,na.rm=TRUE))
bb$mat.z <- (bb$mst-mean(bb$mst,na.rm=TRUE))/(2*sd(bb$mst,na.rm=TRUE))
bb$cc.z <- (bb$cc-mean(bb$cc,na.rm=TRUE))/(2*sd(bb$cc,na.rm=TRUE))
bb$elev.z <- (bb$elev-mean(bb$elev,na.rm=TRUE))/(2*sd(bb$elev,na.rm=TRUE))
bb$lat.z <- (bb$lat-mean(bb$lat,na.rm=TRUE))/(2*sd(bb$lat,na.rm=TRUE))
bb$dist.z <-(bb$distkm-mean(bb$distkm,na.rm=TRUE))/(2*sd(bb$distkm,na.rm=TRUE))
bb$space.z <-(bb$space-mean(bb$space,na.rm=TRUE))/(2*sd(bb$space,na.rm=TRUE))

prior_ <- function(prior, ...) {
  call <- nlist(prior, ...)
  seval <- rmNULL(call[prior_seval_args()])
  call[prior_seval_args()] <- NULL
  as_string <- function(x) {
    if (is.formula(x) && length(x) == 2) {
      deparse_no_string(x[[2]])
    } else if (is.call(x) || is.name(x) || is.atomic(x)) {
      deparse_no_string(x)
    } else {
      stop2("Arguments must be one-sided formula, call, name, or constant.") 
    }
  }
  call <- lapply(call, as_string)
  do.call(set_prior, c(call, seval))
}

bprior1 <- prior(normal(0,1), class="b") + prior(student_t(1,0,2), group="species", class="sd")

brm <- function(formula, data, family = gaussian(), prior = NULL, 
                autocor = NULL, cov_ranef = NULL, 
                sample_prior = c("no", "yes", "only"), 
                sparse = FALSE, knots = NULL, stanvars = NULL,
                stan_funs = NULL, fit = NA, save_ranef = TRUE, 
                save_mevars = FALSE, save_all_pars = FALSE, 
                inits = "random", chains = 4, iter = 2000, 
                warmup = floor(iter / 2), thin = 1,
                cores = getOption("mc.cores", 1L), control = NULL,
                algorithm = c("sampling", "meanfield", "fullrank"),
                future = getOption("future", FALSE), silent = TRUE, 
                seed = NA, save_model = NULL, stan_model_args = list(),
                save_dso = TRUE, file = NULL, ...) {
  
  if (!is.null(file)) {
    # optionally load saved model object
    file <- paste0(as_one_character(file), ".rds")
    x <- suppressWarnings(try(readRDS(file), silent = TRUE))
    if (!is(x, "try-error")) {
      if (!is.brmsfit(x)) {
        stop2("Object loaded via 'file' is not of class 'brmsfit'.")
      }
      return(x)
    }
  }
  
  dots <- list(...)
  algorithm <- match.arg(algorithm)
  testmode <- isTRUE(dots$testmode)
  dots$testmode <- NULL
  if (is.brmsfit(fit)) {
    # re-use existing model
    x <- fit
    icnames <- c("loo", "waic", "kfold", "R2", "marglik")
    x[icnames] <- list(NULL)
    sdata <- standata(x)
    x$fit <- rstan::get_stanmodel(x$fit)
  } else {  
    # build new model
    formula <- validate_formula(
      formula, data = data, family = family, autocor = autocor
    )
    family <- get_element(formula, "family")
    autocor <- get_element(formula, "autocor")
    bterms <- parse_bf(formula)
    if (is.null(dots$data.name)) {
      data.name <- substr(collapse(deparse(substitute(data))), 1, 50)
    } else {
      data.name <- dots$data.name
      dots$data.name <- NULL
    }
    data <- update_data(data, bterms = bterms)
    prior <- check_prior(
      prior, formula, data = data,  
      sample_prior = sample_prior
    )
    # initialize S3 object
    x <- brmsfit(
      formula = formula, family = family, data = data, 
      data.name = data.name, prior = prior, 
      autocor = autocor, cov_ranef = cov_ranef, 
      stanvars = stanvars, stan_funs = stan_funs,
      algorithm = algorithm
    )
    x$ranef <- tidy_ranef(bterms, data = x$data)  
    x$exclude <- exclude_pars(
      bterms, data = x$data, ranef = x$ranef, 
      save_ranef = save_ranef, save_mevars = save_mevars,
      save_all_pars = save_all_pars
    )
    x$model <- make_stancode(
      formula, data = data, prior = prior, 
      sparse = sparse, cov_ranef = cov_ranef,
      sample_prior = sample_prior, knots = knots, 
      stanvars = stanvars, stan_funs = stan_funs, 
      save_model = save_model
    )
    # generate Stan data before compiling the model to avoid
    # unnecessary compilations in case of invalid data
    sdata <- make_standata(
      formula, data = data, prior = prior, 
      cov_ranef = cov_ranef, sample_prior = sample_prior,
      knots = knots, stanvars = stanvars
    )
    stopifnot(is.list(stan_model_args))
    silence_stan_model <- !length(stan_model_args)
    stan_model_args$model_code <- x$model
    if (!isTRUE(save_dso)) {
      warning2("'save_dso' is deprecated. Please use 'stan_model_args'.")
      stan_model_args$save_dso <- save_dso
    }
    message("Compiling the C++ model")
    x$fit <- eval_silent(
      do.call(rstan::stan_model, stan_model_args),
      silent = silence_stan_model
    )
  }
  
  # arguments to be passed to Stan
  if (is.character(inits) && !inits %in% c("random", "0")) {
    inits <- get(inits, mode = "function", envir = parent.frame())
  }
  args <- nlist(
    object = x$fit, data = sdata, pars = x$exclude, 
    include = FALSE, algorithm, iter, seed
  )
  args[names(dots)] <- dots
  
  message("Start sampling")
  if (args$algorithm == "sampling") {
    args$algorithm <- NULL
    args <- c(args,
              nlist(init = inits, warmup, thin, control, show_messages = !silent)
    )
    if (future) {
      require_package("future")
      if (cores > 1L) {
        warning2("Argument 'cores' is ignored when using 'future'.")
      }
      args$chains <- 1L
      futures <- fits <- vector("list", chains)
      for (i in seq_len(chains)) {
        args$chain_id <- i
        if (is.list(inits)) {
          args$init <- inits[i]
        }
        futures[[i]] <- future::future(
          do.call(rstan::sampling, args), packages = "rstan"
        )
      }
      for (i in seq_len(chains)) {
        fits[[i]] <- future::value(futures[[i]]) 
      }
      x$fit <- rstan::sflist2stanfit(fits)
      rm(futures, fits)
    } else {
      args <- c(args, nlist(chains, cores))
      x$fit <- do.call(rstan::sampling, args) 
    }
  } else {
    # vb does not support parallel execution
    x$fit <- do.call(rstan::vb, args)
  }
  if (!testmode) {
    x <- rename_pars(x)
  }
  if (!is.null(file)) {
    saveRDS(x, file = file)
  }
  x
}

cent.fast<-brm(fs.count~nao.z+mat.z+cc.z+elev.z+dist.z+nao.z:cc.z + mat.z:cc.z + elev.z:cc.z +
                 dist.z:cc.z + (0+nao.z||species) + (0+mat.z||species) + (0+cc.z||species) +
               (0+elev.z||species) + (0+dist.z||species), 
               data=bb.stan, prior=bprior1, chains=2)

save(cent.fast, file="/n/wolkovich_lab/Lab/Cat/cen_realfast.Rdata")

