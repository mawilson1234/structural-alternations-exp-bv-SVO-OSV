# Load libraries
source(file.path('Bayesian scripts', 'constants.r'))
suppressMessages(library(tools))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggpubr))
suppressMessages(library(forcats))
suppressMessages(library(stringr))
suppressMessages(library(R.utils))

dir.create(plots.dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models.dir, showWarnings = FALSE, recursive = TRUE)

posteriors_plot <- function(x, variables = '', labels = '', title = '', color_scheme = ''){
	if (color_scheme == ''){
		color_scheme_set('gray')
	} else {
		color_scheme_set(color_scheme)
	}
	
	if (title == ''){
		title <- deparse(substitute(x))
		title <- gsub('\\.', ' ', title)
		title <- paste(title, 'model posteriors')
	}
	
	if (variables == ''){
		variables <- rev(colnames(x)[grepl('^b\\_(?!Intercept)', colnames(x), perl = TRUE)])
	}
	
	if (labels == '') {
		labels <- gsub('^b_', '', variables)
		labels <- gsub('\\.n(_|:|$)', '\\1', labels)
		labels <- gsub(NESTING_SEPARATOR, ' (in) ', labels)
		labels <- gsub(LEVEL_SEPARATOR, ' @ ', labels)
		labels <- gsub('(\\.|_)', ' ', labels)
		labels <- gsub(':', ' × ', labels)
		labels <- toTitleCase(labels)
		labels <- gsub('Svo', 'SVO', labels)
		labels <- gsub('Ovs', 'OVS', labels)
		labels <- gsub(' Tr ', ' T.R. ', labels)
		labels <- gsub(' Ds ', ' D.S. ', labels)
		labels <- gsub(' Sit ', ' S.i.T. ', labels)
		labels <- gsub(' v ', ' V. ', labels)
		labels <- gsub(' l ', ' L. ', labels)
		labels <- gsub('b|Bert', 'BERT', labels)
		labels <- gsub('\\(|\\)', '', labels)
	} else if (length(labels) != length(variables)) {
		cat("Warning: the number of labels doesn't match the number of parameters to plot!")
		cat('Some parameters may not be labeled, or may be labeled incorrectly.')
	}
	
	# for some reason the variables are plotted in reverse, so we need to reverse
	# the labels to make it line up (???)
	plot <- mcmc_areas(x, pars = variables, prob = 0.95, prob_outer = 0.99, point_est = 'mean') +
		expand_limits(x = 0) +
		scale_x_continuous('', n.breaks = 8) +
		scale_y_discrete(labels = labels) +
		ggtitle(title)
	
	x_range <- range(plot$data$x)
	
	plot <- plot +
		theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), 'cm'))
	
	color_scheme_set()
	
	return (plot)
}

save_model_summaries <- function(
	models = list(), 
	filename = '', 
	overwrite = FALSE
){
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''), '\n')
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_summaries.txt')
	}
	
	if (file.exists(filename) & !overwrite){
		cat(paste0('File "', filename, '" already exists. Use overwrite = TRUE to overwrite.\n'))
		return ()
	}
	
	text <- ''
	
	withOptions(
		{
			for (model_name in names(models)){
				cat('Processing model "', model_name, '"...\n', sep = '')
				text <- paste0(text, topsep, model_name, midsep)
				
				output <- capture.output(print(summary(models[[model_name]])))
				for(line in output){
					if (grepl('^Formula:|\\$(.*):', line)) {
						line <- gsub('\\s+', ' ', line)
						pad <- ifelse(grepl('^Formula:', line), paste0(rep(' ', nchar('Formula: ')), collapse = ''), '\t')
						line <- gsub('(\\+ \\([01](.*?)\\|(\\|)?(.*?)\\))', paste0('\n', pad, '\\1'), line)
					}
					text <- paste0(text, line, '\n')
				}
				text <- paste0(text, botsep)
			}
			
			sink(filename)
			cat(text)
			sink()
		},
		# Increase the maximum printing range for saving results to files to work correctly
		max.print = 100000,
		width = 10000
	)
}

save_pmcmc <- function(
	models = list(),
	filename = '',
	variable = '^b_',
	regex = TRUE,
	max_digits = 4
) {
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''))
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	text <- ''
	
	printformat <- paste0('%.0', max_digits, 'f')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_pmcmcs.txt')
	}
	
	withOptions(
		{
			for (model_name in names(models)){
				model <- models[[model_name]]
				
				posteriors <- as_draws_df(model, variable = variable, regex = regex)
				
				summary <- posteriors |>
					pivot_longer(everything()) |>
					mutate(
						name = as.factor(name) |>
							fct_relevel(colnames(posteriors))
					) |> 
					group_by(name) |>
					summarize(
						across(
							everything(), 
							list(sum=\(x) sum(x > 0), length = length)
						)
					) |>
					mutate(p_mcmc = value_sum/value_length) |>
					select(name, p_mcmc)
				
				text <- paste0(text, topsep, model_name, ' posteriors', midsep)
				
				for (i in seq_len(nrow(summary))) {
					effect <- gsub('^b_', '', summary[i,'name'][[1]])
					effect <- gsub('\\.n(_|:|$)', '\\1', effect)
					effect <- gsub(NESTING_SEPARATOR, ' (in) ', effect)
					effect <- gsub(LEVEL_SEPARATOR, ' @ ', effect)
					effect <- gsub('(\\.|_)', ' ', effect)
					effect <- gsub(':', ' × ', effect)
					effect <- toTitleCase(effect)
					effect <- gsub('Svo', 'SVO', effect)
					effect <- gsub('Ovs', 'OVS', effect)
					effect <- gsub(' Tr ', ' Target response ', effect)
					effect <- gsub(' Ds ', ' Data source ', effect)
					effect <- gsub(' Sit ', ' Seen in Training ', effect)
					effect <- gsub(' v ', ' Voice ', effect)
					effect <- gsub(' l ', ' Linear ', effect)
					effect <- gsub('Bert', 'BERT', effect)
					pmcmc <- summary[i, 'p_mcmc'][[1]]
					dir <- ifelse(pmcmc > 0.5, ' < 0', ' > 0')
					pmcmc <- ifelse(pmcmc > 0.5, 1 - pmcmc, pmcmc)
					sig <- ifelse(pmcmc < 0.05, '*', '')
					sig <- ifelse(pmcmc < 0.001, '**', sig)
					sig <- ifelse(pmcmc < 0.0001, '***', sig)
					text <- paste0(text, '\n', sprintf(printformat, pmcmc), ': ', effect, dir, ' ', sig)
				}
				text <- paste0(text, botsep)
			}
			
			text <- gsub('\\n$', '', text)
			
			sink(filename)
			cat(text)
			sink()
		},
		max.print = 100000,
		width = 10000
	)
}

save_model_plots <- function(models = list(), plots.dir = '.') {	
	for (model_name in names(models)) {	
		# we have to put every plot in a list or else R flattens them out
		# and they're unusable. amazing behavior. great language
		model <- models[[model_name]]
		plots <- list()
		plot_types <- list('trace plot' = 'trace', 'marginal posteriors' = 'hist')
		variables <- list('(slopes)' = '^b_', '(standard deviations)' = '^sd_', '(correlations)' = '^cor_')
		for (plot_type in names(plot_types)){
			for (variable in names(variables)){
				plots <- append(
					plots,
					list(mcmc_plot(
						model, 
						type = plot_types[[plot_type]], 
						variable = variables[[variable]],
						regex = TRUE
					) + ggtitle(paste(model_name, plot_type, variable)))
				)
			}
		}
		
		plots <- append(plots, list(pp_check(model, ndraws = 100) + ggtitle(sprintf('%s PP check', model_name))))
		
		if (model$family$family == 'bernoulli') {
			type <- 'error_binned'
			title <- '%s binned residuals PP check'
		} else {
			type <- 'scatter_avg'
			title <- '%s scatterplot PP check'
		}
		
		plots <- append(
			plots, 
			list(
				pp_check(model, type = type) + 
				ggtitle(sprintf(title, model_name))
			)
		)
		
		posteriors <- as_draws_df(model, variable = '^b', regex = TRUE)
		plots <- append(list(posteriors_plot(posteriors, title = sprintf('%s posteriors', model_name))), plots)
		
		# save the plots
		ggexport(
			plotlist = plots,
			filename = file.path(
				plots.dir, 
				sprintf(
					'%s_plots.pdf', 
					gsub('\\(|\\)', '', gsub(' |\\.', '_', tolower(model_name)))
				)
			),
			width = 15,
			height = 12,
			scale = 0.9
		)
	}
}

get_nested_data <- function(data, cols, gcols, out_of_group_value = 0) {
	for (c in cols) {
		other_cols <- gcols[which(cols != c)]
		cat(sprintf('Getting nestings for %s within levels of %s', c, paste0(other_cols, collapse = ', ')), '\n')
		
		comb <- lapply(
			seq_along(other_cols),
			\(i) combn(
				other_cols,
				m = i
			)
		)
		
		comb <- lapply(
			comb,
			\(x) {
				x <- as.matrix(x)
				lapply(
					seq_len(ncol(x)),
					\(i) x[,i]
				)
			}
		)
		
		comb <- flatten(comb)
		
		for (co in comb) {
			cat(sprintf('Working on %s', paste0(co, collapse = ' X ')), '\n')
			groups <- data |>
				select_at(co) |>
				distinct()
			
			for (i in seq_len(nrow(groups))) {
				group <- groups[i,]
				# R/brms won't accept any non-word character in a variable name except for a dot or a single underscore
				group_s <- tolower(gsub('(?:(?![.])([[:punct:]]| ))+', '_', paste(paste0(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups))), group, sep = LEVEL_SEPARATOR, collapse = NESTING_SEPARATOR), perl = TRUE))
				group_s <- remove.double.underscores(group_s)
				# brms won't take trailing underscores in variable names
				group_s <- gsub('_$', '', group_s)
				cat(sprintf(paste0('Working on group nesting %s', NESTING_SEPARATOR, '%s'), c, group_s), '\n')
				nested_name <- paste0(c, NESTING_SEPARATOR, group_s)
				
				data <- data |> 
					rowwise() |>
					mutate(
						`__tmp__` = tolower(gsub('(?:(?![.])([[:punct:]])| )+', '_', paste(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups)), c(!!!rlang::syms(names(groups))), sep = LEVEL_SEPARATOR, collapse = NESTING_SEPARATOR), perl = TRUE)) |>
							remove.double.underscores() %>%
							gsub('_$', '', .),
						'{nested_name}' := case_when(
							`__tmp__` == group_s ~ !!!rlang::syms(c),
							TRUE ~ out_of_group_value
						)
					) |> 
					select(-`__tmp__`)
			}
		}
		cat('\n')
	}
	
	return (data)
}

remove.double.underscores <- function(s) {
	while ('__' %in% s) {
		s <- gsub('__', '_', s)
	}
	return (s)
}

re.findall <- function(regex, str) {
	# lol wut
	return (regmatches(str, gregexpr(regex, str, perl = TRUE)))
}

ident <- function(df) return (df)

fit.model <- function(
	data.type,
	model.type, 
	data.file,
	data.function,
	data.sources,
	formula.name,
	family = bernoulli()
) {
	models.dir <- file.path(models.dir, data.type)
	plots.dir <- file.path(plots.dir, data.type)
	dir.create(models.dir, showWarnings = FALSE, recursive = TRUE)
	dir.create(plots.dir, showWarnings = FALSE, recursive = TRUE)
	
	results <- read.csv(data.file) |> 
		mutate(
			subject = as.factor(subject),
			item = as.factor(item)
		) |>
		data.function()
	
	formula <- accuracy.formulas[[formula.name]]
	effects <- attr(terms(formula), 'term.labels')
	fixef <- effects[!grepl('^1|0 + ', effects)]
	priors <- c(
		set_prior('normal(0, 10)', class = 'Intercept'),
		set_prior('lkj(2)', class = 'cor'),
		set_prior('normal(0, 10)', class = 'b', coef = fixef),
		set_prior('normal(0, 10)', class = 'sd')
	)
	
	if (model.type == formula.name) {
		model.dir <- file.path(models.dir, paste0(model.type))
		plot.dir <- file.path(plots.dir, paste0(model.type))
		out.file <- sprintf('%s_model_%s', model.type, data.type)
		info.str <- sprintf('%s model (%s)', model.type, data.type)
		model.name <- sprintf('%s model (%s)', toTitleCase(model.type), data.type)
	} else {
		model.dir <- file.path(models.dir, paste0(model.type, '_', formula.name))
		plot.dir <- file.path(plots.dir, paste0(model.type, '_', formula.name))
		out.file <- sprintf('%s_model_%s_%s', model.type, data.type, formula.name)
		info.str <- sprintf('%s model (%s) %s', model.type, data.type, formula.name)
		model.name <- sprintf('%s model (%s) %s', toTitleCase(model.type), data.type, formula.name)
	}
	
	dir.create(model.dir, showWarnings = FALSE, recursive = TRUE)
	dir.create(plot.dir, showWarnings = FALSE, recursive = TRUE)
	
	if (file.exists(file.path(model.dir, paste0(out.file, '.rds')))) {
		cat('Loading ', info.str, '\n', sep = '')
	} else {
		cat('Fitting ', info.str, '\n', sep = '')
	}
	
	models <- list()
	models[model.name] <- do.call(
		brm, 
		append(
			brm.args, 
			list(
				formula = formula,
				data = results |> 
					filter(data_source %in% data.sources),
				family = family,
				prior = priors,
				file = file.path(model.dir, paste0(out.file, '.rds'))
			)
		)
	) |> list()
	
	save_model_summaries(
		models,
		filename = file.path(model.dir, paste0(out.file, '_summary.txt')),
		overwrite = TRUE
	)
	
	save_pmcmc(
		models,
		filename = file.path(model.dir, paste0(out.file, '_pmcmcs.txt'))
	)
	
	save_model_plots(
		models,
		plots.dir = plot.dir
	)
}