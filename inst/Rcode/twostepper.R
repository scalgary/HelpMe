# settings:
#     trim - numeric[1]
#         0 : do not trim
#         1 : trim internally in calibration (does not guarentee that trim bounds satisfied)
#         2: (default) recursively trim until trim bounds satisfied
#     nonmember - numeric[1]
#         the value that should be assigned to rows that do not meet the weighting condition (Default : 0)
#     incomplete - numeric[1]
#         the value that should be assigned to rows which have missing weight variables
#     mintarget - numeric[1]
#         the minimum number of observations in the target to allow weighting (Default : 100)
#     mindata - numeric[1]
#         the minimum number of observations in the data to allow weighting (Default : 100)
#     minviolate - numeric[1]
#         the value that should be assigned when the minimum observations requirements are violated (Default : 1)
#     nonconverge - numeric[1]
#         the value that should be assigned when internal calibration does not converge (Default : 1)
#     bounds = numeric[2]
#         the bounds that the weights should be trimmed to. Not respected if trim=0. (Default : [0.1, 3])
#     hardbind - boolean[1]
#         whether to restrict weights to be strictly positive before trimming (Default : TRUE)
#     controls - string[] (Required)
#         the names of the weighting variables. These must exist in both the data and the targets
#     foreach - string[1]
#         the name of a variable for whose values to perform weights independently in a loop (Default : NULL)
#     recursive - boolean[1]
#         whether to use the data itself for the second step. Otherwise, only pre-data is used (Default : TRUE)
#     balance - boolean[1]
#         when using the data itself and auxdata, whether to balance their effects as targets using effective bases (Default : TRUE)
#     randmiss - boolean[1]
#         when there is missing values of control variables, whether to proceed with the assumption that they are missing at random
(function(projectdirs = ".", force = TRUE){
    DEBUG <- FALSE
    globalDefaults <- list(
        trim = 2,
        nonmember = 0,
        incompletes = 0,
        mintarget = 100,
        mindata = 100,
        minviolate = 1,
        nonconverge = 1,
        hardbind = TRUE,
        maxbin = 20,
        foreach = NULL,
        recursive = TRUE,
        balance = FALSE,
        maxiter = 100,
        randmiss = TRUE
    )
    absPath <- function(path){
        return(normalizePath(path, mustWork = FALSE))
    }
    .COND_TYPES = list(
        inlist = list(
            validate = function(x){ is.atomic(x) && (length(x) > 0) },
            reduce = function(left, right){ sort(union(left, right)) },
            evaluate = function(x, col){ col %in% x }
        ),
        notinlist = list(
            validate = function(x){ is.atomic(x) && (length(x) > 0) },
            reduce = function(left, right){ sort(union(left, right)) },
            evaluate = function(x, col){ !col %in% x }
        ),
        lessinc = list(
            validate = function(x){ is.atomic(x) && (length(x) == 1) },
            reduce = function(left, right){ min(left, right) },
            evaluate = function(x, col){ col <= x }
        ),
        lessexc = list(
            validate = function(x){ is.atomic(x) && (length(x) == 1) },
            reduce = function(left, right){ min(left, right) },
            evaluate = function(x, col){ col < x }
        ),
        moreinc = list(
            validate = function(x){ is.atomic(x) && (length(x) == 1) },
            reduce = function(left, right){ max(left, right) },
            evaluate = function(x, col){ col >= x }
        ),
        moreexc = list(
            validate = function(x){ is.atomic(x) && (length(x) == 1) },
            reduce = function(left, right){ max(left, right) },
            evaluate = function(x, col){ col > x }
        )
    )
    checkCondition <- function(condition){
        # a condition is a fully named list
        # where each list member is also a named list
        #   with one of a set of specially designated names
        #   each member of that list member must be atomic
        #   each special type has it's own validator (in .COND_TYPES) of these
        #   atomics
        if (!is.list(condition)){ return("Non-list condition block.") }
        if (length(condition) == 0){ return(NULL) }
        if (is.null(names(condition))){ return("Unnamed condition block.")}
        i = 1
        while (i <= length(condition)){
            varname <- names(condition)[i]
            conddef <- condition[[i]]
            if (is.atomic(conddef)){ conddef <- list(inlist = conddef) }
            if (varname == ""){
                condition[[i]] <- "Unnamed variable."
                i <- i + 1
            } else if (!is.list(conddef)){
                condition[[i]] <- "Non-list non-atomic condition block."
                i <- i + 1
            } else {
                j <- 1
                while (j <= length(conddef)){
                    typename <- names(conddef)[j]
                    if (!typename %in% names(.COND_TYPES)){
                        conddef[[j]] <- "Unrecognized condition member type."
                        j <- j + 1
                    } else {
                        condobj <- .COND_TYPES[[typename]]
                        condval <- conddef[[j]]
                        if (!condobj$validate(condval)){
                            conddef[[j]] <- "Requirement not pass validation"
                            j <- j + 1
                        } else {
                            conddef[[j]] <- NULL
                        }
                    }
                }
                if (length(conddef) == 0){ conddef <- NULL }
                condition[[i]] <- conddef
                if (!is.null(conddef)){ i <- i + 1 }
            }
        }
        if (length(condition) == 0){ condition <- NULL }
        return(condition)
    }
    factorProps <- function(facs, values = unique(facs), weights = rep(1, length(facs))){
        props <- NULL
        for (lvl in sort(values)){
            props <- c(
                props,
                sum(weights[facs %in% lvl]) / sum(weights[facs %in% values])
            )
        }
        names(props) <- sort(values)
        return(props)
    }
    runProject <- function(projectdir = "."){
        paths = list(
            config = absPath(file.path(projectdir, "config.R")),
            data = absPath(file.path(projectdir, "data.csv")),
            target = absPath(file.path(projectdir, "target.csv")),
            log = NULL,
            auxdata = list.files(
                projectdir,
                pattern = "auxdata\\d*.csv",
                full.names = TRUE
            ),
            benchmarks = absPath(file.path(projectdir, "benchmark.csv")),
            assessments = absPath(file.path(projectdir, "assessments.csv")),
            weights = absPath(file.path(projectdir, "weighted.csv")),
            auxweights = absPath(file.path(projectdir, "auxweights.csv"))
        )
        data <- NULL
        target <- NULL
        auxdata <- list()
        auxweights <- list()
        combined <- NULL
        config <- NULL
        benchmarks <- data.frame(
            Weight = character(),
            Version = character(),
            Variable = character(),
            Level = character(),
            Target = numeric(),
            Sampled = numeric(),
            Diff = numeric(),
            Alignment = numeric(),
            Final = numeric()
        )
        assessments <- data.frame(
            Weight = character(),
            Version = character(),
            Partial = character(),
            Count = numeric(),
            Efficiency = numeric(),
            SampleAlignment = numeric(),
            Accuracy = numeric(),
            ChiSqP = numeric()
        )
        logBuffer <- c()
        writeLog <- function(..., escape = NULL){
            if (is.null(escape)){
                condition <- ""
            } else if (escape){
                condition <- "(Breaking Condition)"
            } else {
                condition <- "(Adjustable Condition)"
            }
            if (is.null(paths$log)){
                logBuffer <<- c(logBuffer, paste(..., condition, "\n"))
                return(NULL)
            }
            cat(
                ...,
                condition,
                "\n",
                file = paths$log,
                append = TRUE
            )
        }
        onCondition <- function(cdata, condition, adjust=NULL, count=FALSE, name = ""){
            meets <- rep(0, nrow(cdata))
            types = c("inlist", "notinlist", "lessinc", "lessexc", "moreinc", "moreexc")
            for (condn in names(condition)){
                cond <- condition[[condn]]
                if (!condn %in% names(cdata)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Conditioning variable not found in data:",
                        condn,
                        escape = TRUE
                    )
                    return(rep(FALSE, nrow(cdata)))
                }
                col <- cdata[[condn]]
                meet <- rep(TRUE, nrow(cdata))
                if (is.atomic(cond)){
                    cond <- list(inlist = cond)
                }
                for (comp in names(cond)){
                    meet <- meet & .COND_TYPES[[comp]]$evaluate(cond[[comp]], col)
                }
                meets <- meets + meet
            }
            adjust <- ifelse(is.numeric(adjust), adjust, 0)
            if (adjust > 0){
                cutoff <- adjust
            } else {
                cutoff <- adjust + length(condition)
            }
            if (count){
                return(meets)
            } else {
                return(meets >= cutoff)
            }
        }
        validateConditions <- function(name, settings){
            escape <- FALSE
            if(!is.null(checkCondition(settings$conditions$data))){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Invalid data condition for weighting",
                    escape = TRUE
                )
                escape <- TRUE
            }
            if (!is.null(checkCondition(settings$conditions$target))){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Invalid target condition for weighting",
                    escape = TRUE
                )
                escape <- TRUE
            }
            return(escape)
        }
        validateSettings <- function(name, settings, wdata, adata, tdata){
            escape <- FALSE
            if (any(!settings$controls %in% names(wdata))){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Weight variables not in data:",
                    setdiff(settings$controls, names(wdata)),
                    escape = TRUE
                )
                escape <- TRUE
            } else {
                for (control in settings$controls){
                    if (length(unique(wdata[[control]])) > settings$maxbin){
                        writeLog(
                            "[",
                            name,
                            "]",
                            "Weight variables has too many bins in data:",
                            control,
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                }
            }
            if (any(!settings$controls %in% names(tdata))){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Weight variables not in target:",
                    setdiff(settings$controls, names(tdata)),
                    escape = TRUE
                )
                escape <- TRUE
            } else {
                for (control in settings$controls){
                    if (length(unique(tdata[[control]])) > settings$maxbin){
                        writeLog(
                            "[",
                            name,
                            "]",
                            "Weight variable has too many bins in target:",
                            control,
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                }
            }
            for (i in seq_along(adata)){
                if (any(!settings$controls %in% names(adata[[i]]))){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Weight variables not in auxilliary<",
                        i,
                        ">:",
                        setdiff(settings$controls, names(adata[[i]])),
                        escape = TRUE
                    )
                    escape <- TRUE
                } else {
                    for (control in settings$controls){
                        if (length(unique(adata[[i]][[control]])) > settings$maxbin){
                            writeLog(
                                "[",
                                name,
                                "]",
                                "Weight variable has too many bins in auxilliary<",
                                i,
                                ">:",
                                control,
                                escape = TRUE
                            )
                            escape <- TRUE
                        }
                    }
                }
            }
            if (is.null(settings$bounds)){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Bounds must be provided.",
                    escape = TRUE
                )
                escape <- TRUE
            } else if (!is.numeric(settings$bounds)){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Bounds must be numeric.",
                    escape = TRUE
                )
                escape <- TRUE
            } else if (!length(settings$bounds) == 2){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Bounds must be 2-length",
                    escape = TRUE
                )
                escape <- TRUE
            } else if (settings$bounds[1] >= settings$bounds[2]){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Bounds must be order lower to higher",
                    escape = TRUE
                )
                escape <- TRUE
            }
            if (!is.null(settings$hardbind)){
                if (!is.logical(settings$hardbind)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Hardbind setting is not a boolean.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (length(settings$hardbind) != 1){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Hardbind setting is not a singleton.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            if (!is.null(settings$foreach)){
                if (!is.character(settings$foreach)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "For-each setting is not a string.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (length(settings$foreach) != 1){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "For-each setting is not a singleton.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else {
                    if (!settings$foreach %in% names(wdata)){
                        writeLog(
                            "[",
                            name,
                            "]",
                            "For-each setting is not in the data.",
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                    for (i in seq_along(adata)){
                        if (!settings$foreach %in% names(adata[[i]])){
                            writeLog(
                                "[",
                                name,
                                "]",
                                "For-each setting is not in the auxilliary<",
                                i,
                                ">.",
                                escape = TRUE
                            )
                            escape <- TRUE
                        }
                    }
                }
            }
            if (!is.null(settings$preweight)){
                if (!is.character(settings$preweight)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Preweight setting is not a string.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (length(settings$preweight) != 1){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Preweight setting is not a singleton.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (!settings$preweight %in% names(tdata)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Preweight setting is not in the target.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            if (!is.null(settings$balance)){
                if (!is.logical(settings$balance)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Balance setting is not a boolean.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (length(settings$balance) != 1){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Balance setting is not a singleton.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            if (!is.null(settings$randmiss)){
                if (!is.logical(settings$randmiss)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Randmiss setting is not a boolean.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (length(settings$randmiss) != 1){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Randmiss setting is not a singleton.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            return(escape)
        }
        baseBenchmark <- function(standard, controlLevels, splits, weights = rep(1, nrow(standard))){
            benchmark <- NULL
            for (i in seq_along(controlLevels)){
                benchmark <- c(
                    benchmark,
                    setNames(
                        rep(0, length(controlLevels[[i]])),
                        paste0(names(controlLevels[i]), controlLevels[[i]])
                    )
                )
            }
            neff <- 0
            for (split in unique(splits)){
                splitmark <- NULL
                select <- splits == split
                splitweight <- weights[select]
                sneff <- sum(splitweight)^2 / sum(splitweight^2)
                neff <- neff + sneff
                for (i in seq_along(controlLevels)){
                    splitmark <- c(
                        splitmark,
                        setNames(
                            factorProps(
                                standard[[names(controlLevels)[i]]][select],
                                controlLevels[[i]],
                                splitweight
                            ),
                            paste0(names(controlLevels[i]), controlLevels[[i]])
                        )
                    )
                }
                benchmark <- benchmark + (splitmark * sneff)
            }
            benchmark <- benchmark / neff
        }
        makeBenchmark <- function(name, stack, settings, sample, standard, splits){
            controlLevels <- list()
            adjustBenchmark <- function(benchmark, control){
                sampleNamed <- paste0(control, sample[[control]])
                standardNamed <- paste0(control, na.omit(standard[[control]]))
                mismark <- names(benchmark)[benchmark == 0]
                misfactor <- 1 - (sum(sampleNamed %in% mismark) / length(sampleNamed))
                for (level in unique(sampleNamed)){
                    if (level %in% mismark){
                        benchmark[level] <- sum(sampleNamed %in% level) / length(sampleNamed)
                        writeLog(
                            "[",
                            name,
                            ": ",
                            stack,
                            "]",
                            "Weight variable",
                            control,
                            "has a value [",
                            level,
                            "] not present in the target.",
                            escape = FALSE
                        )
                    } else {
                        benchmark[level] <- benchmark[level] * misfactor
                    }
                }
                extmark <- setdiff(unique(standardNamed), unique(sampleNamed))
                extfactor <- 1 / (1 - sum(benchmark[extmark]))
                benchmark <- benchmark[!names(benchmark) %in% extmark]
                for (level in unique(standardNamed)){
                    if (level %in% names(benchmark)){
                        benchmark[level] <- benchmark[level] * extfactor
                    } else {
                        writeLog(
                            "[",
                            name,
                            ": ",
                            stack,
                            "]",
                            "Weight variable",
                            control,
                            "has a value [",
                            level,
                            "] not present in the data.",
                            escape = FALSE
                        )
                    }
                }
                controlbench <- sort(intersect(
                    union(unique(sampleNamed), unique(standardNamed)),
                    names(benchmark)
                ))
                sampleProps <- factorProps(sampleNamed)
                benchmarks <<- rbind(
                    benchmarks,
                    data.frame(
                        Weight = name,
                        Version = as.character(stack),
                        Variable = control,
                        Level = gsub(
                            paste0("^", control),
                            "",
                            controlbench
                        ),
                        Target = benchmark[controlbench],
                        Sampled = sampleProps[controlbench],
                        Diff = sampleProps[controlbench] - benchmark[controlbench],
                        Alignment = 1 - sqrt(
                            (sampleProps[controlbench] + benchmark[controlbench])/2 -
                            sqrt(sampleProps[controlbench] * benchmark[controlbench])
                        ),
                        Final = NA,
                        row.names = NULL
                    )
                )
                benchmark <- benchmark[setdiff(names(benchmark), controlbench[1])]
                return(benchmark)
            }
            if (!settings$recursive){
                standard <- standard[splits > 1,]
                splits <- splits[splits > 1]
            }
            if (is.null(settings$preweight)){
                preweight <- rep(1, nrow(standard))
            } else {
                preweight <- standard[[settings$preweight]]
            }
            for (control in settings$controls){
                sample[[control]] <- as.factor(sample[[control]])
                standard[[control]] <- as.factor(standard[[control]])
                controlLevels[[control]] <- sort(union(
                    levels(standard[[control]]),
                    levels(sample[[control]])
                ))
            }
            benchmark <- baseBenchmark(standard, controlLevels, splits, preweight)
            for (control in settings$controls){
                benchmark <- adjustBenchmark(benchmark, control)
            }
            benchmark <- c("(Intercept)" = 1, benchmark) * nrow(sample)
            return(benchmark)
        }
        rimweight <- function (design, formula, population, bounds, maxit, trim, ...){
            if (all(bounds == c(-Inf, Inf))){
                rval <- survey:::regcalibrate(
                    design,
                    formula,
                    population,
                    aggregate.stage = NULL,
                    stage = 0,
                    lambda = NULL,
                    sparse = FALSE,
                    ...
                )
                rval$call <- sys.call(-1)
                return(rval)
            }
            expit <- function(x) 1 - 1/(1 + exp(x))
            mm <- model.matrix(formula, model.frame(formula, model.frame(design)))
            ww <- weights(design)
            bounds <- lapply(bounds, function(x) x/ww)
            whalf <- sqrt(ww)
            sample.total <- colSums(mm * ww)
            if (any(sample.total == 0)) {
                zz <- (population == 0) & (apply(mm, 2, function(x) all(x == 0)))
                mm <- mm[, !zz]
                population <- population[!zz]
                sample.total <- sample.total[!zz]
            }
            if (length(sample.total) != length(population)) {
                stop("Population and sample totals are not the same length.")
            }
            if (!is.null(names(population))) {
                if (!all(names(sample.total) %in% names(population))){
                    stop("Sampling and population totals have different names.")
                }
            }
            tqr <- qr(mm * whalf)
            g <- survey::grake(
                mm,
                ww,
                survey::cal.linear,
                bounds = bounds,
                population = population,
                verbose = FALSE,
                epsilon = 1e-07,
                maxit = maxit,
                variance = NULL
            )
            #switched the order of the next 2 if blocks so that we always fail in cases of non-convergeance
            if (!is.null(attr(g, "failed"))) {
                stop("Calibration failed")
            }
            if (!is.null(trim)) {
                gnew <- pmax(trim[1], pmin(g, trim[2]))
                outside <- g < trim[1] | g > trim[2]
                if (any(outside)) {
                    trimmings <- (g - gnew) * ww
                    gnew[!outside] <- gnew[!outside] + sum(trimmings)/sum(ww[!outside])
                    g <- gnew
                    attr(g, "failed") <- NULL
                }
            }
            design$prob <- design$prob/g
            caldata <- list(qr = tqr, w = g * whalf, stage = 0, index = NULL)
            class(caldata) <- c("greg_calibration", "gen_raking")
            design$postStrata <- c(design$postStrata, list(caldata))
            design$call <- sys.call(-1)
            design
        }
        calibrate <- function(design, formula, benchmark, settings){
            if (settings$hardbind){
                prebounds = c(1e-5, Inf)
            } else {
                prebounds = c(-Inf, Inf)
            }
            if (settings$trim == 1){
                model = rimweight(
                    design = design,
                    formula = formula,
                    population = benchmark,
                    bounds = prebounds,
                    trim = settings$bounds,
                    maxit = settings$maxiter
                )
            } else {
                model <- rimweight(
                    design = design,
                    formula = formula,
                    population = benchmark,
                    bounds = prebounds,
                    trim = NULL,
                    maxit = settings$maxiter
                )
            }
            if (settings$trim > 1){
                model <- survey::trimWeights(
                    model,
                    lower = settings$bounds[1],
                    upper = settings$bounds[2],
                    strict = TRUE
                )
            }
            return(weights(model))
        }
        assessWeights <- function(name, stack, sample, weights, controls){
            benchmatch <- benchmarks$Weight == name & benchmarks$Version %in% as.character(stack)
            benchmark <- benchmarks[benchmatch,]
            benchmarks <<- benchmarks[!benchmatch,]
            partialEff <- function(control){
                agg <- aggregate(
                    as.formula(paste("weights ~", control)),
                    cbind(sample, weights),
                    function(x){
                        c(mean = mean(x), count = length(x))
                    }
                )$weights
                return(
                    (sum(agg[,1] * agg[,2])^2) / (sum(agg[,1]^2 * agg[,2]) * sum(agg[,2]))
                )
            }
            partialAcc <- function(control){
                targets <- benchmark$Target[benchmark$Variable %in% control]
                weighted <- benchmark$Final[benchmark$Variable %in% control]
                return(1 - sqrt(1 - round(sum(sqrt(targets * weighted)), 10)))
            }
            partialAlg <- function(control){
                targets <- benchmark$Target[benchmark$Variable %in% control]
                sampled <- benchmark$Sampled[benchmark$Variable %in% control]
                return(1 - sqrt(1 - round(sum(sqrt(targets * sampled)), 10)))
            }
            partialChi <- function(control){
                return(
                    suppressWarnings(chisq.test(
                        rbind(
                            benchmark$Final[benchmark$Variable %in% control] * length(weights),
                            benchmark$Target[benchmark$Variable %in% control] * length(weights)
                        ),
                        correct = FALSE
                    ))$p.value
                )
            }
            for (control in controls){
                props <- factorProps(
                    sample[[control]],
                    benchmark$Level[benchmark$Variable %in% control],
                    weights
                )
                benchmark$Final[match(
                    paste(control, names(props), sep = ".|.|."),
                    paste(benchmark$Variable, benchmark$Level, sep = ".|.|.")
                )] <- props
                assessments <<- rbind(
                    assessments,
                    data.frame(
                        Weight = name,
                        Version = as.character(stack),
                        Partial = control,
                        Count = length(weights),
                        Efficiency = partialEff(control),
                        SampleAlignment = partialAlg(control),
                        Accuracy = partialAcc(control),
                        ChiSqP = partialChi(control)
                    )
                )
            }
            assessments <<- rbind(
                assessments,
                data.frame(
                    Weight = name,
                    Version = as.character(stack),
                    Partial = "",
                    Count = length(weights),
                    Efficiency = (sum(weights)^2) / (length(weights) * sum(weights^2)),
                    SampleAlignment = 1 - (sqrt(length(controls) - round(sum(sqrt(benchmark$Target * benchmark$Sampled)), 10))),
                    Accuracy = 1 - (sqrt(length(controls) - round(sum(sqrt(benchmark$Target * benchmark$Final)), 10))),
                    ChiSqP = suppressWarnings(chisq.test(rbind(benchmark$Target * length(weights), benchmark$Final * length(weights)), correct = FALSE))$p.value
                )
            )
            benchmarks <<- rbind(
                benchmarks,
                benchmark
            )
            return(NULL)
        }
        makeWeights <- function(name, settings, sample, standard, splits){
            if (nrow(sample) == 0){
                writeLog(
                    "[",
                    name,
                    "]",
                    "No data matches condition.",
                    escape = TRUE
                )
                return(NULL)
            }
            if (is.null(settings$foreach)){
                stacks <- rep(0, nrow(sample))
            } else {
                stacks <- sample[[settings$foreach]]
            }
            weights <- rep(settings$nonmember, nrow(sample))
            nas <- rep(FALSE, nrow(sample))
            for (control in settings$controls){
                nas[is.na(sample[[control]])] <- TRUE
            }
            if (any(nas)){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Missing control data. Removing.",
                    escape = FALSE
                )
            }
            weights[nas] <- settings$incompletes
            anyelig <- rep(FALSE, length(weights))
            unistandard <- c()
            for (control in settings$control){
                if (length(unique(standard[[control]])) == 1){
                    unistandard <- c(unistandard, control)
                }
            }
            subsettings <- settings
            for (stack in unique(stacks)){
                elig <- !nas & (stacks == stack)
                if (sum(elig) < settings$mindata){
                    writeLog(
                        "[",
                        name,
                        ": ",
                        stack,
                        "]",
                        "Not enough sample to perform weights:",
                        "(",
                        sum(elig),
                        "of",
                        settings$mindata,
                        ")",
                        escape = TRUE
                    )
                    weights[elig] <- settings$minviolate
                    next
                }
                anyelig <- anyelig | elig
                unilevel <- c()
                for (control in unistandard){
                    if (length(unique(sample[[control]][elig])) == 1){
                        writeLog(
                            "[",
                            name,
                            ": ",
                            stack,
                            "]",
                            "Weight Variable: ",
                            control,
                            "only has a single level. Removing.",
                            escape = FALSE
                        )
                        unilevel <- c(unilevel, control)
                    }
                }
                subsettings$controls <- setdiff(settings$controls, unilevel)
                benchmark <- makeBenchmark(
                    name,
                    stack,
                    subsettings,
                    sample[elig,],
                    standard,
                    splits
                )
                formu <- as.formula(
                    paste(
                        "~",
                        paste(
                            subsettings$controls, collapse = "+"
                        )
                    )
                )
                svy <- survey::svydesign(ids = ~1, weights = ~1, data = sample[elig,])
                if (DEBUG){
                    weights[elig] <- calibrate(svy, formu, benchmark, settings)
                } else {
                    weights[elig] <- tryCatch(
                        calibrate(svy, formu, benchmark, settings),
                        error = function(e){
                            writeLog(
                                "[",
                                name,
                                    ": ",
                                    stack,
                                "] Calibration caused an error:",
                                e$message,
                                escape = TRUE
                            )
                            return(rep(settings$nonconverge, sum(elig)))
                        }
                    )
                }
                assessWeights(
                    name,
                    stack,
                    sample[elig,],
                    weights[elig],
                    subsettings$controls
                )
            }
            if (any(anyelig) && length(unique(stacks)) > 1){
                makeBenchmark(
                    name,
                    "<ALL>",
                    settings,
                    sample[anyelig,],
                    standard,
                    splits
                )
                assessWeights(
                    name,
                    "<ALL>",
                    sample[anyelig,],
                    weights[anyelig],
                    settings$controls
                )
            }
            return(weights)
        }
        mainWeight <- function(name, settings){
            settings <- c(
                settings,
                config$default[!names(config$default) %in% names(settings)]
            )
            escape <- FALSE
            if (is.null(settings$balance)){
                settings$balance <- FALSE
            } else if (settings$balance){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Balance setting for main weights can not be TRUE",
                    escape = FALSE
                )
                settings$balance <- FALSE
            }
            if (!settings$recursive){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Recursive setting for main weights can not be FALSE",
                    escape = FALSE
                )
                settings$recursive <- TRUE
            }
            escape <- escape || validateSettings(name, settings, data, auxdata, target)
            escape <- escape || validateConditions(name, settings)
            if (escape) { return(TRUE) }
            data[[name]] <<- rep(settings$nonmember, nrow(data))
            for (i in seq_along(auxdata)){
                auxdata[[i]][[name]] <<- settings$nonmember
            }
            selig <- onCondition(target, settings$conditions$target, name = name)
            if (!is.null(settings$preweight)){
                badweight <- is.na(target[[settings$preweight]]) | target[[settings$preweight]] <= 0
                if (any(selig & badweight)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Non-positive pre-weights exist. Removing.",
                        escape = FALSE
                    )
                }
                selig <- selig & !badweight
            }
            nas <- rep(FALSE, nrow(target))
            for (control in settings$controls){
                nas <- nas | is.na(target[[control]])
            }
            if (any(selig & nas)){
                if (settings$randmiss){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Missing target cells treated as missing at random.",
                        escape = FALSE
                    )
                } else {
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Missing target cells disallowed by settings.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            if (sum(selig) < settings$mintarget){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Not enough target to perform weights:",
                    "(",
                    sum(selig),
                    "of",
                    settings$mintarget,
                    ")",
                    escape = TRUE
                )
                escape <- TRUE
            }
            elig <- onCondition(data, settings$conditions$data, name = name)
            if (escape){
                data[[name]][elig] <<- settings$minviolate
            } else {
                data[[name]][elig] <<- makeWeights(
                    name,
                    settings,
                    data[elig,],
                    target[selig,],
                    rep(1, sum(selig))
                )
            }
            for (i in seq_along(auxdata)){
                elig <- onCondition(auxdata[[i]], settings$conditions$data, name = name)
                if (escape){
                    auxdata[[i]][[name]][elig] <<- settings$minviolate
                } else {
                    auxdata[[i]][[name]][elig] <<- makeWeights(
                        paste0(name, "<", i, ">"),
                        settings,
                        auxdata[[i]][elig,],
                        target[selig,],
                        rep(1, sum(selig))
                    )
                }
                auxweights[[name]] <<- c(
                    auxweights[[name]],
                    auxdata[[i]][[name]]
                )
            }
            return(escape)
        }
        combineData <- function(){
            allcols <- names(data)
            for (i in seq_along(auxdata)){
                allcols <- union(allcols, names(auxdata[[i]]))
            }
            combined <<- data
            combined[setdiff(allcols, names(combined))] <<- NA
            for (i in seq_along(auxdata)){
                full <- auxdata[[i]]
                full[setdiff(allcols, names(full))] <- NA
                combined <<- rbind(combined, full)
            }
            return(NULL)
        }
        depWeight <- function(name, settings){
            settings <- c(
                settings,
                config$default[!names(config$default) %in% names(settings)]
            )
            escape <- FALSE
            if (!settings$recursive && length(auxdata) == 0){
                writeLog(
                    "Recursive setting for rep weights without auxdata can not be FALSE",
                    escape = FALSE
                )
                settings$recursive <- TRUE
            }
            escape <- escape || validateSettings(name, settings, data, list(), combined)
            escape <- escape || validateConditions(name, settings)
            if (escape) { return(TRUE) }
            data[[name]] <<- rep(settings$nonmember, nrow(data))
            selig <- onCondition(combined, settings$conditions$target, name = name)
            if (!is.null(settings$preweight)){
                badweight <- is.na(combined[[settings$preweight]]) | combined[[settings$preweight]] <= 0
                if (any(selig & badweight)){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Non-positive pre-weights exist. Removing.",
                        escape = FALSE
                    )
                }
                selig <- selig & !badweight
            }
            nas <- rep(FALSE, nrow(combined))
            for (control in settings$controls){
                nas <- nas | is.na(combined[[control]])
            }
            if (any(selig & nas)){
                if (settings$randmiss){
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Missing target cells treated as missing at random.",
                        escape = FALSE
                    )
                } else {
                    writeLog(
                        "[",
                        name,
                        "]",
                        "Missing target cells dissalowed by settings.",
                        escape = TRUE
                    )
                    escape <- TRUE
                }
            }
            if (sum(selig) < settings$mintarget){
                writeLog(
                    "[",
                    name,
                    "]",
                    "Not enough target to perform weights:",
                    "(",
                    sum(selig),
                    "of",
                    settings$mintarget,
                    ")",
                    escape = TRUE
                )
                escape <- TRUE
            }
            elig <- onCondition(data, settings$conditions$data, name = name)
            splits <- rep(1, nrow(data))
            for (i in seq_along(auxdata)){
                if (settings$balance){
                    splits <- c(splits, rep(i + 1, nrow(auxdata[[i]])))
                } else {
                    splits <- c(splits, rep(1, nrow(auxdata[[i]])))
                }
            }
            if (escape){
                data[[name]][elig] <<- settings$minviolate
            } else {
                data[[name]][elig] <<- makeWeights(
                    name,
                    settings,
                    data[elig,],
                    combined[selig,],
                    splits[selig]
                )
            }
            return(escape)
        }
        parsePaths <- function(fconfig){
            checkPath <- function(name, path){
                if (is.null(path)){
                    return(FALSE)
                } else if (!is.character(path)){
                    writeLog(
                        "Files config [",
                        name,
                        "] is not a string.",
                        escape = TRUE
                    )
                    return(TRUE)
                } else if (length(path) != 1){
                    writeLog(
                        "Files config [",
                        name,
                        "] is not a singleton.",
                        escape = TRUE
                    )
                    return(TRUE)
                } else if(substr(path, 1, 1) == .Platform$file.sep){
                    paths[[name]] <<- path
                    return(FALSE)
                } else {
                    paths[[name]] <<- absPath(file.path(projectdir, path))
                    return(FALSE)
                }
            }
            sealPaths <- function(result){
                if (is.null(paths$log)){
                    paths$log <<- absPath(file.path(projectdir, "log.txt"))
                }
                cat("", file = paths$log, append = FALSE)
                for (msg in logBuffer){
                    cat(msg, file = paths$log, append = TRUE)
                }
                logBuffer <<- NULL
                return(result)
            }
            if (is.null(fconfig)){
                return(sealPaths(FALSE))
            } else if (!is.list(fconfig)){
                writeLog(
                    "Files member of config is not a list",
                    escape = TRUE
                )
                return(sealPaths(TRUE))
            }
            for (extra in setdiff(names(fconfig), names(paths))){
                writeLog(
                    "Unrecognized member of files config:",
                    extra,
                    ". Ignoring",
                    escape = FALSE
                )
            }
            escape <- FALSE
            for (ftype in setdiff(names(paths), "auxdata")){
                escape <- checkPath(ftype, fconfig[[ftype]]) || escape
            }
            if (!is.null(fconfig$auxdata)){
                if (!is.character(fconfig$auxdata)){
                    writeLog(
                        "Files config [ auxdata ] is not a string.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else {
                    paths$auxdata <<- c()
                    for (prepath in fconfig$auxdata){
                        if(substr(prepath, 1, 1) == .Platform$file.sep){
                            paths$auxdata <<- c(paths$auxdata, prepath)
                        } else {
                            paths$auxdata <<- c(
                                paths$auxdata,
                                absPath(file.path(projectdir, prepath))
                            )
                        }
                    }
                }
            }
            return(sealPaths(escape))
        }
        expired <- function(){
            if (force){
                writeLog("Forced Run. Expiration not checked.", escape = FALSE)
                return(TRUE)
            }
            if (!file.exists(paths$weights)){
                writeLog("Weights do not pre-exist. Must run.", escape = FALSE)
                return(TRUE)
            }
            if (file.mtime(paths$config) > file.mtime(paths$weights)){
                writeLog("Weights are older than config file. Must run.", escape = FALSE)
                return(TRUE)
            } else {
                writeLog("Weights are newer than config file. Will not run.", escape = TRUE)
                return(FALSE)
            }
        }
        return((main <- function(){
            escape <- FALSE
            if (!requireNamespace("survey", quietly = TRUE)){
                writeLog(
                    "Survey package needs to be installed."
                )
                if (DEBUG){
                    install.packages("survey")
                } else {
                    tryCatch(
                        install.packages("survey"),
                        error = function(e){
                            writeLog(
                                "Installation error:",
                                e$message,
                                escape = TRUE
                            )
                        }
                    )
                }
            }
            if (!requireNamespace("data.table", quietly = TRUE)){     # if statement added 11/4/20 to ensure proper package is installed
                writeLog(
                    "data.table package needs to be installed."
                )
                if (DEBUG){
                    install.packages("data.table")
                } else {
                    tryCatch(
                        install.packages("data.table"),
                        error = function(e){
                            writeLog(
                                "Installation error:",
                                e$message,
                                escape = TRUE
                            )
                        }
                    )
                }
            }
            if (!dir.exists(projectdir)){
                stop("Project Directory does not exist")
            }
            if (file.exists(paths$config)){
                if (DEBUG){
                    config <<- eval(parse(paths$config))
                } else {
                    tryCatch(
                        config <<- eval(parse(paths$config)),
                        error = function(e){
                            writeLog(
                                "Config file cannot be read. Error:",
                                e$message,
                                escape = TRUE
                            )
                        }
                    )
                }
            } else {
                writeLog(
                    "Config file does not exist. Expected at",
                    paths$config,
                    escape = TRUE
                )
                escape <- TRUE
            }
            if (escape) { return(FALSE) }
            if (is.null(config)){
                writeLog(
                    "Config file does not exist. Expected at",
                    paths$config,
                    escape = TRUE
                )
                return(FALSE)
            }
            if (!is.list(config)){
                writeLog(
                    "Config object is not a list.",
                    escape = TRUE
                )
                return(FALSE)
            }
            for (extra in setdiff(names(config), c("main", "deps", "default", "files"))){
                writeLog(
                    "Unrecognized member of config:",
                    extra,
                    ". Ignoring",
                    escape = FALSE
                )
            }
            if (is.null(config$main)){
                writeLog(
                    "Config object does not contain a main member.",,
                    escape = TRUE
                )
                escape <- TRUE
            } else if (!is.list(config$main)){
                writeLog(
                    "Main member of config object is not a list.",
                    escape = TRUE
                )
                escape <- TRUE
            }
            if (is.null(config$deps)){
                writeLog(
                    "Config object does not contain a deps member.",
                    escape = TRUE
                )
                escape <- TRUE
            } else if (!is.list(config$deps)){
                writeLog(
                    "Deps member of config object is not a list.",
                    escape = TRUE
                )
                escape <- TRUE
            }
            escape <- parsePaths(config$files)
            escape <- escape || !expired()
            if (escape){ return(FALSE) }
            if (file.exists(paths$data)){
                tryCatch(
                    data <<- read.csv(
                        paths$data,
                        stringsAsFactors = FALSE
                    ),
                    error = function(e){
                        writeLog(
                            "Data file cannot be read:",
                            e$message,
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                )
            } else {
                writeLog(
                    "Data file does not exist. Expected at",
                    paths$data,
                    escape = TRUE
                )
                escape <- TRUE
            }
            if (file.exists(paths$target)){
                tryCatch(
                    target <<- read.csv(
                        paths$target,
                        stringsAsFactors = FALSE
                    ),
                    error = function(e){
                        writeLog(
                            "Target file cannot be read:",
                            e$message,
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                )
            } else {
                writeLog(
                    "Target file does not exist. Expected at",
                    paths$target,
                    escape = TRUE
                )
                escape <- TRUE
            }
            for (i in seq_along(paths$auxdata)){
                tryCatch(
                    auxdata[[i]] <<- read.csv(
                        paths$auxdata[i],
                        stringsAsFactors = FALSE
                    ),
                    error = function(e){
                        writeLog(
                            "Auxilliary data file cannot be read:",
                            paths$auxdata[i],
                            escape = TRUE
                        )
                        escape <- TRUE
                    }
                )
                if (!escape){
                    auxweights$Version <<- c(
                        auxweights$Version,
                        rep(i, nrow(auxdata[[i]]))
                    )
                }
            }
            if (escape){ return(FALSE) }
            if (is.null(config$default)){
                writeLog(
                    "Default member missing, treating as no defaults.",
                    escape = FALSE
                )
                config$default <<- list()
            } else if (!is.list(config$default)){
                writeLog(
                    "Default member is not a list, treating as no defaults.",
                    escape = FALSE
                )
                config$default <<- list()
            }
            config$default <<- c(
                config$default,
                globalDefaults[!names(globalDefaults) %in% config$default]
            )
            escape <- FALSE
            allnames <- c()
            for (i in seq_along(config$main)){
                if (is.null(names(config$main[i]))){
                    writeLog(
                        "Unnamed weight definition. Member",
                        i,
                        " of main. Ignoring",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (names(config$main[i]) %in% allnames){
                    writeLog(
                        "Duplicate names in weight definitions:",
                        names(config$main[i]),
                        ". Member",
                        i,
                        " of main. Ignoring all but first entry.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else {
                    allnames <- c(allnames, names(config$main[i]))
                    escape <- mainWeight(names(config$main[i]), config$main[[i]]) || escape
                }
            }
            combineData()
            for (i in seq_along(config$deps)){
                if (is.null(names(config$deps[i]))){
                    writeLog(
                        "Unnamed weight definition. Member",
                        i,
                        " of deps. Ignoring",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else if (names(config$deps[i]) %in% allnames){
                    writeLog(
                        "Duplicate names in weight definitions:",
                        names(config$deps[i]),
                        ". Member",
                        i,
                        " of deps. Ignoring all but first entry.",
                        escape = TRUE
                    )
                    escape <- TRUE
                } else {
                    escape <- depWeight(names(config$deps[i]), config$deps[[i]]) || escape
                }
            }
            if (length(paths$auxdata)){
                if (!dir.exists(dirname(paths$auxweights))){
                    dir.create(dirname(paths$auxweights), recursive=TRUE)
                }
                is.num <- sapply(auxweights, is.numeric)                     # added 11/4/20 to round all numbers to 6 decimal places
                auxweights[is.num] <- lapply(auxweights[is.num], round, 6)   # added 11/4/20 to round all numbers to 6 decimal places
                data.table::fwrite(              # used to be `write.csv(`
                    as.data.frame(auxweights),
                    file = paths$auxweights,
                    row.names = FALSE,
                    na="NA",                     # added 10/23/20
                    dateTimeAs = c("write.csv")  # added 10/23/20
                )
            }
            if (!dir.exists(dirname(paths$benchmarks))){
                dir.create(dirname(paths$benchmarks), recursive=TRUE)
            }
            is.num <- sapply(benchmarks, is.numeric)                     # added 11/4/20 to round all numbers to 6 decimal places
            benchmarks[is.num] <- lapply(benchmarks[is.num], round, 6)   # added 11/4/20 to round all numbers to 6 decimal places
            data.table::fwrite(              # used to be `write.csv(`
                benchmarks,
                file = paths$benchmarks,
                row.names = FALSE,
                na="NA",                     # added 10/23/20
                dateTimeAs = c("write.csv")  # added 10/23/20
            )
            if (!dir.exists(dirname(paths$assessments))){
                dir.create(dirname(paths$assessments), recursive=TRUE)
            }
            is.num <- sapply(assessments, is.numeric)                      # added 11/4/20 to round all numbers to 6 decimal places
            assessments[is.num] <- lapply(assessments[is.num], round, 6)   # added 11/4/20 to round all numbers to 6 decimal places
            data.table::fwrite(              # used to be `write.csv(`
                assessments,
                file = paths$assessments,
                row.names = FALSE,
                na="NA",                     # added 10/23/20
                dateTimeAs = c("write.csv")  # added 10/23/20
            )
            if (!dir.exists(dirname(paths$weights))){
                dir.create(dirname(paths$weights), recursive=TRUE)
            }
            is.num <- sapply(data, is.numeric)                  # added 11/4/20 to round all numbers to 6 decimal places
            data[is.num] <- lapply(data[is.num], round, 6)   # added 11/4/20 to round all numbers to 6 decimal places
            data.table::fwrite(              # used to be `write.csv(`
                data,
                file = paths$weights,
                row.names = FALSE,
                na="NA",                     # added 10/23/20
                dateTimeAs = c("write.csv")  # added 10/23/20
            )
            if (escape){ return(FALSE) }
            return(TRUE)
        })())
    }
    clear <- TRUE
    for (i in seq_along(projectdirs)){
        projectdir <- normalizePath(projectdirs[i], mustWork = FALSE)
        if(DEBUG){
            if (runProject(projectdir)){
                print(paste0(
                    projectdir,
                    ": Run successful."
                ))
            } else {
                clear <- FALSE
                print(paste0(
                    projectdir,
                    ": Run unsuccessful, see log."
                ))
            }
        } else {
            tryCatch(
                {
                    if (runProject(projectdir)){
                        print(paste0(
                            projectdir,
                            ": Run successful."
                        ))
                    } else {
                        clear <- FALSE
                        print(paste0(
                            projectdir,
                            ": Run unsuccessful, see log."
                        ))
                    }
                },
                error = function(e){
                    clear <- FALSE
                    print(paste0(
                        projectdir,
                        ": Run caused exception:",
                        e$message
                    ))
                }
            )
        }
    }
    return(clear)
})(c(
    "."
))

# replace the "." above with a comma-separated list of directories
#   which contain your weighting project directories you would like to run.
# "." represents "current working directory", so you may leave it as is
#   if you just wish to run the script for the current directory
