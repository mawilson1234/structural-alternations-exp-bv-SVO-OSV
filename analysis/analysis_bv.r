# Load libraries
library(plyr)
library(ggh4x)
library(ggpubr)
library(stringr)
library(tidyverse)
library(ggpattern)
library(data.table)
library(reticulate)

# do not change these
MAX_RT_IN_SECONDS <- 10
MAX_BREAKTIME_IN_SECONDS <- 15 * 60 # 15 minutes
OUTLIER_RT_SDS <- 2
SUBJECTS_RUN_WITH_BAD_DELAY <- c()

current.exp <- 'bv'

# confidence interval for beta distribution
beta_ci <- function(y, ci = 0.95) {
	alpha <- sum(y) + 1
	beta <- length(y) - sum(y) + 1
	lower_bound <- (1 - ci)/2
	upper_bound <- ci + lower_bound
	qs <- c(qbeta(lower_bound, alpha, beta), qbeta(upper_bound, alpha, beta))
	df <- data.frame(qs) |> t()
	colnames(df) <- c('ymin', 'ymax')
	rownames(df) <- NULL
	df <- cbind(df, data.frame(y = mean(y)))
	return(df)
}

# convenience
s_view <- function(d, x) return (d |> filter(subject %in% x))

# Load data
colnames <- c(
	'time_received', 'ip_md5', 'controller', 'order', 'element_no', 'condition', 
	'latin_square_group', 'element_type', 'element_name', 'parameter', 'value', 
	'event_time', 'item', 'word', 'target_response', 'args_group', 'sentence_type', 
	'sentence', 'adverb', 'seen_in_training', 'template', 'comments'
)

results <- read.csv(
		'results_prod.csv', 
		comment.char='#',
		header = FALSE, 
		quote='',
		col.names = colnames, 
		fill = TRUE
	) |> 
	as_tibble() |>
	select(ip_md5, condition, order, element_name:template) |>
	mutate(data_source = 'human')

# Relabel subjects with smaller values
results <- results |> 
	mutate(
		subject = match(ip_md5, unique(ip_md5)),
		subject = as.factor(subject)
	) |>
	select(subject, everything()) |>
	select(-ip_md5)

# add columns so that models can be added
results <- results |>
	as_tibble() |>
	mutate(
		subject = as.factor(subject),
		correct.pre.training = NA_real_,
		mask_added_tokens = "Don't mask blork",
		stop_at = 'convergence'
	)

# get break times
breaktimes <- results |>
	filter(condition == 'break') |>
	select(subject, value, event_time) |>
	group_by(subject) |>
	mutate(break_number = sort(rep(1:(n()/2),2))) |>
	spread(value, event_time) |>
	mutate(duration = End - Start) |>
	select(-Start, -End) |>
	mutate(
		minutes = duration/(60 * 1000),
		seconds = (minutes %% 1) * 60,
		milliseconds = (seconds %% 1) * 1000,
		break_time = paste0(
			str_pad(trunc(minutes), 2,pad = '0'), ':', 
			str_pad(trunc(seconds), 2, pad = '0'), '.', 
			str_pad(round(milliseconds), 3, pad = '0')
		)
	) |>
	select(subject, break_number, break_time, duration)

excessive.breaktimes <- breaktimes |>
	filter(duration > MAX_BREAKTIME_IN_SECONDS * 1000) |>
	droplevels()

training.accuracy.OSV.by.session.no <- results |>
	filter(
		condition %like% 'trial_train',
		parameter == 'Drop'
	) |>
	group_by(subject, mask_added_tokens, stop_at) |>
	mutate(order = rleid(order)) |>
	filter(
		grepl('(.*?)\\[obj\\](.*?)\\[subj\\](.*?)', sentence)
	) |>
	group_by(subject, data_source, condition, mask_added_tokens, stop_at, order) |> 
	summarize(n_tries = n()) |>
	ungroup() |>
	mutate(session.no = case_when(order <= 21 ~ 1, TRUE ~ ceiling((order - 21)/15)+1)) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, session.no) |>
	mutate(total = n()) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, session.no, n_tries, total) |>
	summarize(pr_correct = n()) |>
	ungroup() |>
	mutate(pr_correct = pr_correct/total) |>
	group_by(subject, data_source, mask_added_tokens, stop_at) |>
	filter(n_tries == min(n_tries)) |>
	rename(pr_first_choice_correct = pr_correct)

# training first choice accuracy by half
# for some subjects, this is inaccurate due to
# a bug in the way PCIbex records results
# with identical trial labels. the training
# repetition criteria worked correctly
training.accuracy.by.session.no <- results |>
	filter(
		condition %like% 'trial_train',
		parameter == 'Drop'
	) |> 
	group_by(subject, mask_added_tokens, stop_at) |>
	mutate(order = rleid(order)) |>
	group_by(subject, data_source, condition, mask_added_tokens, stop_at, order) |> 
	summarize(n_tries = n()) |>
	ungroup() |>
	mutate(session.no = case_when(order <= 21 ~ 1, TRUE ~ ceiling((order - 21)/15)+1)) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, session.no) |>
	mutate(total = n()) |>
	group_by(subject, data_source, mask_added_tokens, stop_at, session.no, n_tries, total) |>
	summarize(pr_correct = n()) |>
	ungroup() |>
	mutate(pr_correct = pr_correct/total) |>
	group_by(subject, data_source, mask_added_tokens, stop_at) |>
	filter(n_tries == min(n_tries)) |>
	select(-n_tries, -total) |>
	rename(pr_first_choice_correct = pr_correct)

less.than.90.on.training <- training.accuracy.by.session.no |>
	filter(
		session.no == max(session.no),
		pr_first_choice_correct < 0.9
	)

# Get feedback
feedback <- results |>
	filter(element_name == 'feedback', item != 'Shift', parameter == 'Final') |>
	select(subject, value) |>
	rename(feedback = value) |>
	mutate(
		feedback = feedback |>
			str_replace_all('%2C', ',') |>
			str_replace_all('%0A', '\n')
	)

# organize results to one row per trial
results <- results |> 
	filter(
		condition == 'trial',
		!(parameter %in% c('Click', 'Drag', 'Final')),
	) |> 
	mutate(
		parameter = case_when(
			value %in% c('Start', 'End') ~ paste0(parameter, value),
			TRUE 						 ~ parameter
		),
		element_name = case_when(
			element_name == 'dd'			~ 'response',
			startsWith(parameter, '_Trial')	~ parameter,
			TRUE 				 			~ element_name
		),
		value = case_when(
			parameter %in% c('_Trial_Start', '_Trial_End') ~ as.character(event_time),
			TRUE 										   ~ value
		)
	) |>
	select(-parameter, -event_time, -order, -template) |>
	pivot_wider(
		names_from = element_name,
		values_from = value
	)

# add log RTs
results <- results |>
	rename(Trial_Start = '_Trial_Start', Trial_End = '_Trial_End') |>
	mutate(
		Trial_Start = as.numeric(Trial_Start),
		Trial_End = as.numeric(Trial_End),
		sentence_delay = case_when(
			subject %in% SUBJECTS_RUN_WITH_BAD_DELAY ~ (str_count(sentence, ' ') + 1),
			TRUE ~ (str_count(sentence, ' ') + 1) * 325
		),
		log.RT = log(Trial_End - sentence_delay - Trial_Start)
	) |> 
	select(-Trial_Start, -Trial_End, -sentence_delay)

# add experimental conditions
results <- results |>
	mutate(
		condition = case_when(
			grepl('_', args_group) 	~ 'experimental',
			TRUE 					~ 'filler'
		),
		correct = case_when(
			response == target_response ~ TRUE,
			TRUE 						~ FALSE
		),
		voice = case_when(
			grepl('passive', sentence_type, fixed = TRUE) ~ 'OVS passive',
			TRUE 										~ 'SVO active'
		)
	)

# get unique item identifiers to add to the model results
item.ids <- results |>
	select(item, condition, word, args_group, sentence_type, adverb) |>
	distinct() |> 
	mutate(item = as.numeric(as.character(item))) |>
	arrange(item)

# add item.ids for drink fillers by manipulation
drink.filler.item.ids <- item.ids |>
	filter(args_group == 'eat') |>
	mutate(
		word = case_when(
				word == 'customer' ~ 'girl',
				word == 'person'   ~ 'man',
				word == 'guest'    ~ 'boy',
				word == 'child'    ~ 'woman',
				word == 'patient'  ~ 'person',
				word == 'king'     ~ 'customer',
				word == 'pizza'    ~ 'water',
				word == 'orange'   ~ 'milk',
				word == 'steak'    ~ 'wine',
				word == 'bread'    ~ 'beer',
				word == 'sandwich' ~ 'coffee',
				word == 'dessert'  ~ 'tea'
			),
		args_group = 'drink',
		item = max(item.ids$item) - min(item) + item + 1 
	)

item.ids <- item.ids |>
	rbind(drink.filler.item.ids)

exp.item.ids <- item.ids |>
	filter(condition == 'experimental') |>
	select(-args_group)

filler.item.ids <- item.ids |>
	filter(condition == 'filler') |>
	select(-word) |>
	distinct()

# We use python to read the gzipped files since it is MUCH faster than doing it in R
py_run_string('import pandas as pd')
py_run_string('from glob import glob')
py_run_string('from tqdm import tqdm')
py_run_string(
	paste0(
		'csvs = glob("D:/Users/mawilson/Yale backups/',
		'CLAY Lab/salts/nv_tr_SVO-OSV_f_h_e/*cu*/bv-ma-nmato/',
		'**/*odds_ratios.csv.gz", recursive = True)'
	)
)
py_run_string('model_results = pd.concat([pd.read_csv(f) for f in tqdm(csvs)], ignore_index = True)')

model.results <- py$model_results |> 
	as_tibble() |>
	mutate(
		data_source = gsub('(B|b)ert', 'BERT', str_to_title(model_name)),
		token = gsub('^\u0120', '', token) # formatting bug with roberta models
	)

# convert model results to format comparable with results from humans
model.results <- model.results |>
	rename(
		word = token,
		target_response = arg_type
	) |>
	mutate(
		condition = case_when(
			eval_data == 'syn_blorked_SVO-OSV_for_human_exp' ~ 'experimental',
			TRUE ~ 'filler'
		)
	) |>
	# filter out the unwanted eval for the training tokens from the filler items
	filter(
		# keep everything from the experimental conditions
		(condition == 'experimental') | 
		# keep everything from fillers only when the token_type is "eval special"
		(condition == 'filler' & token_type == 'eval special')
	) |>
	mutate(
		# we need to do this until we merge the item ids,
		# since we had to use a couple different tokens for modernbert.
		# we'll put these back to rights after merging the item ids.
		word = case_when(
			grepl('ModernBERT', data_source) ~ case_when(
				token_type == 'tuning' ~ case_when(
					word == 'chalk' ~ 'pearl',
					word == 'crab' ~ 'lobster',
					TRUE ~ word
				),
				TRUE ~ word
			),
			TRUE ~ word
		),
		stop_at = case_when(
			max_epochs == 5000 ~ 'convergence',
			TRUE ~ '260 epochs'
		),
		mask_added_tokens = case_when(
			 mask_added_tokens ~ "Mask blork",
			!mask_added_tokens ~ "Don't mask blork"
		),
		correct = odds_ratio > 0,
		correct.pre.training = (odds_ratio - odds_ratio_pre_post_difference) > 0,
		sentence = gsub('(\\D)(\\D+)', '\\U\\1\\L\\2', sentence, perl = TRUE),
		training = 'SVO+OSV',
		seen_in_training = case_when(
			token_type == 'tuning' ~ 'True',
			token_type == 'eval added' ~ 'False',
			token_type == 'eval special' ~ NA_character_
		),
		mouse = NA_character_,
		response = case_when(
			odds_ratio > 0 ~ target_response,
			TRUE ~ gsub('.*\\/(.*)', '\\1', ratio_name)
		),
		log.RT = NA_real_,
		voice = case_when(
			grepl('passive', sentence_type, fixed = TRUE) 		  ~ 'OVS passive',
			grepl('.*\\[obj\\].*\\[subj\\].*blorked.*', sentence) ~ 'OSV active',
			TRUE 												  ~ 'SVO active'
		),
		adverb = case_when(
			sentence %like% 'always' ~ 'always',
			sentence %like% 'often' ~ 'often',
			sentence %like% 'usually' ~ 'usually',
			sentence %like% 'typically' ~ 'typically'
		),
		subject = match(
			random_seed, 
			c(
				seq_len(max(0,as.numeric(as.character(results$subject)))),
				model.results$random_seed |> unique()
			)
		),
		args_group = case_when(
			condition == 'experimental' ~ args_group,
			condition == 'filler' ~ gsub('syn_(.*?)_ext_for_human_exp', '\\1', eval_data)
		)
	) |>
	filter(training == 'SVO+OSV') |>
	left_join(exp.item.ids) |>
	left_join(
		filler.item.ids, 
		by = c('condition', 'args_group', 'sentence_type'), 
		suffix = c('', '.y')
	) |>
	mutate(item = coalesce(item, item.y)) |>
	select(-item.y) |>
	select(
		subject, condition, training, item, word, target_response, args_group, sentence_type, 
		sentence, adverb, seen_in_training, data_source, mouse, response, log.RT,
		correct, correct.pre.training, voice, mask_added_tokens, stop_at, model_id
	) |> 
	arrange(subject, item, word, adverb) |>
	filter(!is.na(item))

# merge model results with human results
results <- results |> 
	mutate(
		training = 'SVO+OSV',
		subject = as.numeric(as.character(subject)),
		model_id = NA_character_
	) |> 
	rbind(
		model.results %>%
			mutate(subject = as.numeric(as.character(.$subject)))
	) |>
	mutate(
		subject = as.factor(subject),
		seen_in_training = case_when(
			seen_in_training == 'True' ~ 'Seen',
			seen_in_training == 'False' ~ 'Unseen'
		),
		target_response = case_when(
			target_response == '[subj]' ~ 'Subject target',
			target_response == '[obj]'  ~ 'Object target'
		) |> 
			fct_relevel('Subject target', 'Object target'),
		args_group = paste(gsub('\\_', '+', args_group), 'args') |> 
			fct_relevel(
				'buildings+vehicles args',
				'break args', 
				'buy args',
				'drink args',
				'eat args',
				'read args',
				'regret args'
			),
		sentence_type = fct_relevel(
			sentence_type,
			'perfect transitive',
			'raising perfect transitive',
			'cleft subject perfect transitive',
			'neg perfect transitive',
			'cleft subject raising perfect transitive',
			'perfect passive',
			'raising perfect passive',
			'cleft subject perfect passive',
			'neg perfect passive',
			'cleft subject raising perfect passive' #,
			# 'cleft object perfect transitive',
			# 'presentational ORC perfect transitive'
		),
		voice = case_when(
				grepl('passive', sentence_type, fixed = TRUE) 		  ~ 'OVS passive',
				# grepl('.*\\[obj\\].*\\[subj\\].*blorked.*', sentence) ~ 'OSV active',
				TRUE 												  ~ 'SVO active'
			) |>
			fct_relevel('SVO active', 'OVS passive'), #'OSV active'),
		data_source = fct_relevel(data_source, 'human', 'BERT', 'ModernBERT Base', 'ModernBERT Large'),
		mask_added_tokens = fct_relevel(mask_added_tokens, "Don't mask blork"), # "Mask blork", "Don't mask blork"),
		stop_at = fct_relevel(stop_at, 'convergence') # '260 epochs', 'convergence')
	)

# exclude subjects less than <75% accurate on fillers (no stats)
less.than.75.on.fillers <- results |>
	filter(condition == 'filler') |>
	group_by(subject, data_source, mask_added_tokens, stop_at) |>
	summarize(mean = mean(correct)) |>
	filter(mean < 0.75)

# exclude subjects/models not >50% on training structure as determined by chisq with p > 0.2.
not.above.chance.on.training.str <- results |>
	filter(
		condition == 'experimental',
		sentence_type == 'perfect transitive', 
		seen_in_training == 'Seen'
	) |>
	group_by(subject, data_source, training, mask_added_tokens, stop_at) |>
	summarize(
		correct.total = sum(correct),
		incorrect.total = n() - correct.total
	) |>
	rowwise() |>
	mutate(p.value = chisq.test(c(correct.total,incorrect.total))$p.value) |>
	filter(p.value > 0.2 | correct.total < incorrect.total) |>
	ungroup()

# subjects/models whose performance is purely linear
# the goal is to exclude subjects whose performance on passives is not distinguishable
# from the inverse of their performance on actives as determined by chisq.test > 0.05
purely.linear.by.arguments.both <- results |>
	filter(condition == 'experimental') |>
	select(
		subject, data_source, mask_added_tokens, 
		stop_at, training, voice, target_response, correct
	) |>
	filter(voice %in% c('SVO active', 'OVS passive')) |> 
	mutate(
		correct = case_when(
					voice %like% 'active' ~ !correct,
					voice %like% 'passive' ~ correct
				),
		target_response = case_when(
							target_response %like% 'Subject' ~ 'subj',
							target_response %like% 'Object' ~ 'obj'
						)
	) |> 
	group_by(
		subject, mask_added_tokens, stop_at, 
		training, data_source, target_response
	) |>
	summarize(
		n_active = length(correct[voice %like% 'active']),
		n_passive = length(correct[voice %like% 'passive']),
		inv.correct = sum(correct[voice %like% 'active']),
		pass.correct = sum(correct[voice %like% 'passive']),
	) |>
	pivot_wider(
		names_from = target_response, 
		values_from = c(n_active, n_passive, inv.correct, pass.correct)
	) |>
	rowwise() |>
	mutate(
		p.value.subj = chisq.test(
							matrix(c(
								inv.correct_subj, 	n_active_subj  - inv.correct_subj,
								pass.correct_subj, 	n_passive_subj - pass.correct_subj
						), ncol = 2))$p.value,
		# if both numbers are 0, it is indistinguishable from linear, 
		# but chisq can't tell us that (it gives NaN)
		# convert to a number (Inf) for ease of exclusion below
		p.value.subj = case_when(
							is.na(p.value.subj) ~ Inf,
							TRUE ~ p.value.subj
						),
		p.value.obj  = chisq.test(
							matrix(c(
								inv.correct_obj, 	n_active_obj  - inv.correct_obj,
								pass.correct_obj, 	n_passive_obj - pass.correct_obj
						), ncol = 2))$p.value,
		p.value.obj  = case_when(
							is.na(p.value.obj) ~ Inf,
							TRUE ~ p.value.obj
						)
	) |>
	filter(
		p.value.subj > 0.05, 
		p.value.obj > 0.05
	) |> 
	# change , to | for both arguments must differ from linear for INCLUSION. currently you are EXCLUDED
	# only if BOTH are not different from linear. changing to OR means you are EXCLUDED if EITHER
	# differs from linear. we favor the weaker criterion, where EXCLUSION means BOTH are NOT different from linear
	ungroup()

# tag linear performance
results <- results |>
	mutate(
		linear = case_when(
			subject %in% purely.linear.by.arguments.both$subject ~ 'Linear',
			TRUE ~ 'Non-linear'
		)
	)

# read in cosine similarity files for models
# to determine whether success distinguishing
# pre-fine-tuning BLORKED from non-BLORKED targets
# predicts accuracy post-fine-tuning
py_run_string(
	paste0(
		'cossim_csvs = glob("D:/Users/mawilson/Yale backups/',
		'CLAY Lab/salts/nv_tr_SVO-OSV_f_h_e/*cu*/bv-ma-nmato/',
		'**/*cossims.csv.gz", recursive = True)'
	)
)
py_run_string('cossim_results = pd.concat([pd.read_csv(f) for f in tqdm(cossim_csvs)], ignore_index = True)')

cossim.results <- py$cossim_results |> 
	as_tibble() |>
	mutate(
		data_source = gsub('(B|b)ert', 'BERT', str_to_title(model_name)),
		token = gsub('^\u0120', '', token) # formatting bug with roberta models
	)

# get mean cosine similarity (with and without correction for each model)
cossim.means <- cossim.results |> 
	filter(
		!grepl('most similar$', target_group),
		correction == 'all_but_the_top'
	) |>
	mutate(eval_epoch = case_when(eval_epoch == 0 ~ as.character(eval_epoch), TRUE ~ epoch_criteria)) |>
	group_by(model_id, target_group, eval_epoch) |>
	summarize(
		max_cossim_to_targets = max(cossim),
		mean_cossim_to_targets = mean(cossim)
	) |>
	filter(target_group == 'BLORKED') |> 
	ungroup() |>
	select(-target_group) |>
	mutate(eval_epoch = case_when(eval_epoch == '0' ~ '_pre.training', TRUE ~ '')) |>
	pivot_wider(
		names_from = eval_epoch, 
		values_from = c(max_cossim_to_targets, mean_cossim_to_targets),
		names_sep = ''
	)

results <- results |>
	left_join(cossim.means)

# get all excluded subjects
all.excluded.subjects <- c(
		as.numeric(as.character(less.than.90.on.training$subject)),
		as.numeric(as.character(less.than.75.on.fillers$subject)),
		as.numeric(as.character(not.above.chance.on.training.str$subject)),
		as.numeric(as.character(excessive.breaktimes$subject)),
		as.numeric(as.character(purely.linear.by.arguments.both$subject))
	) |> 
	sort() |>
	unique() %>%
	data.frame(subject = .) |>
	left_join(
		results |> 
			mutate(subject = as.numeric(as.character(subject))) |>
			select(subject, data_source, mask_added_tokens, stop_at) |> 
			unique()
	) |>
	mutate(
		subject = as.factor(subject),
		why = '',
		why = case_when(
				subject %in% less.than.90.on.training$subject ~ paste0(why, '<90% on training; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% less.than.75.on.fillers$subject ~ paste0(why, '<75% on fillers; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% not.above.chance.on.training.str$subject ~ paste0(why, 'n.d. from chance on training structure; '),
				TRUE ~ why
			), 
		why = case_when(
				subject %in% excessive.breaktimes$subject ~ paste0(why, 'break time > 15 minutes; '),
				TRUE ~ why
			),
		why = case_when(
				subject %in% purely.linear.by.arguments.both$subject ~ paste0(why, 'n.d. from linear; '),
				TRUE ~ why
			),
		why = gsub('; $', '', why)
	) |>
	rename(why.excluded = why) |>
	as_tibble()

excluded.for.reasons.other.than.linear <- all.excluded.subjects |>
	filter(why.excluded != 'n.d. from linear')

full.results <- results
results <- results |>
	filter(!(subject %in% excluded.for.reasons.other.than.linear$subject)) |>
	droplevels()

# split to separate data frames
exp <- results |>
	filter(condition == 'experimental') |>
	select(-condition)

filler <- results |>
	filter(condition == 'filler') |>
	select(-condition)

# f scores for experimental items
f.scores <- exp |>
	group_by(subject, voice) |>
	mutate(
		false.positives.subjects = case_when(
									(target_response == 'Object target' & response == '[subj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.subjects  = case_when(
									!correct & !false.positives.subjects ~ TRUE,
									TRUE ~ FALSE
								),
		false.positives.objects = case_when(
									(target_response == 'Subject target' & response == '[obj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.objects  = case_when(
									!correct & !false.positives.objects ~ TRUE,
									TRUE ~ FALSE
								)
	) |>
	summarize(
		true.positives.subjects = sum(correct[target_response == 'Subject target']),
		true.positives.objects  = sum(correct[target_response == 'Object target']),
		false.positives.subjects = sum(false.positives.subjects),
		false.negatives.subjects = sum(false.negatives.subjects),
		false.positives.objects = sum(false.positives.objects),
		false.negatives.objects = sum(false.negatives.objects),
		precision.subjects = true.positives.subjects/(true.positives.subjects+false.positives.subjects),
		precision.objects = true.positives.objects/(true.positives.objects+false.positives.objects),
		recall.subjects = true.positives.subjects/(true.positives.subjects+false.negatives.subjects),
		recall.objects = true.positives.objects/(true.positives.objects+false.negatives.objects),
		f.score.subjects = 2 * (precision.subjects*recall.subjects)/(precision.subjects+recall.subjects),
		f.score.objects = 2 * (precision.objects*recall.objects)/(precision.objects+recall.objects)
	) |>
	select(
		subject, voice, precision.subjects, precision.objects, 
		recall.subjects, recall.objects, 
		f.score.subjects, f.score.objects
	) |>
	distinct() |>
	pivot_longer(
		c(f.score.subjects,f.score.objects),
		names_to = 'target_response',
		values_to = 'f.score'
	) |>
	mutate(
		target_response = case_when(
							target_response == 'f.score.subjects' ~ 'Subject target',
							TRUE ~ 'Object target'
						),
		target_response = fct_relevel(target_response, 'Subject target', 'Object target')
	)

f.scores.pre.training <- exp |>
	group_by(subject, voice) |>
	mutate(response_pre_training = case_when(
									correct == correct.pre.training ~ response,
									response == '[subj]' ~ '[obj]',
									response == '[obj]'  ~ '[subj]'
								)
	) |>
	mutate(
		false.positives.subjects = case_when(
									(target_response == 'Object target' & response_pre_training == '[subj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.subjects  = case_when(
									!correct.pre.training & !false.positives.subjects ~ TRUE,
									TRUE ~ FALSE
								),
		false.positives.objects = case_when(
									(target_response == 'Subject target' & response_pre_training == '[obj]') ~ TRUE,
									TRUE ~ FALSE
								),
		false.negatives.objects  = case_when(
									!correct & !false.positives.objects ~ TRUE,
									TRUE ~ FALSE
								)
	) |>
	summarize(
		true.positives.subjects = sum(correct.pre.training[target_response == 'Subject target']),
		true.positives.objects  = sum(correct.pre.training[target_response == 'Object target']),
		false.positives.subjects = sum(false.positives.subjects),
		false.negatives.subjects = sum(false.negatives.subjects),
		false.positives.objects = sum(false.positives.objects),
		false.negatives.objects = sum(false.negatives.objects),
		precision.subjects = true.positives.subjects/(true.positives.subjects+false.positives.subjects),
		precision.objects = true.positives.objects/(true.positives.objects+false.positives.objects),
		recall.subjects = true.positives.subjects/(true.positives.subjects+false.negatives.subjects),
		recall.objects = true.positives.objects/(true.positives.objects+false.negatives.objects),
		f.score.subjects = 2 * (precision.subjects*recall.subjects)/(precision.subjects+recall.subjects),
		f.score.objects = 2 * (precision.objects*recall.objects)/(precision.objects+recall.objects)
	) |>
	select(
		subject, voice, precision.subjects, precision.objects, 
		recall.subjects, recall.objects, 
		f.score.subjects, f.score.objects
	) |>
	distinct() |> 
	pivot_longer(
		c(f.score.subjects,f.score.objects),
		names_to = 'target_response',
		values_to = 'f.score'
	) |>
	mutate(
		target_response = case_when(
							target_response == 'f.score.subjects' ~ 'Subject target',
							TRUE ~ 'Object target'
						),
		target_response = fct_relevel(target_response, 'Subject target', 'Object target')
	)

exp <- exp |>
	left_join(
		f.scores |> 
			select(subject, voice, target_response, f.score)
	) |>
	left_join(
		f.scores.pre.training |>
			select(subject, voice, target_response, f.score) |>
			rename(f.score.pre.training = f.score)	
	)

# save results for accuracy analysis
accuracy.data <- exp |>
	filter(
		mask_added_tokens == "Don't mask blork",
		stop_at == 'convergence',
		# filter to only the actual sentences humans saw
		paste(word, sentence) %in% (
			exp |> 
				filter(data_source == 'human') |>
				select(word, sentence) |>
				distinct() |>
				droplevels() |>
				mutate(word_sentence = paste(word, sentence)) |>
				pull(word_sentence)
		)
	) |>
	select(
		-word, -args_group, -mouse, -correct.pre.training,
		-f.score, -sentence, -training, -adverb, -seen_in_training,
		-linear
	) |>
	mutate(
		target_response.n = case_when(
			target_response == 'Object target'  ~  0.5,
			target_response == 'Subject target' ~ -0.5
		),
		data_source.n = case_when(
			data_source == 'human' ~  0.5,
			data_source %in% c('BERT', 'ModernBERT Base', 'ModernBERT Large') ~ -0.5
		),
		voice.n = case_when(
			voice == 'SVO active'  ~  0.5,
			voice == 'OVS passive' ~ -0.5
		),
		RT = exp(log.RT)
	)

write.csv(accuracy.data, 'accuracy-data.csv', row.names = FALSE)

# logistic regression including data source 
# separate regressions for each argument group
#	(model, human) * voice (active, passive) * (seen, unseen) * target_response
# 	random effects for subjects/model random seed, random effect of word (nested within (seen, unseen)), 
#   			       items
# 
# if humans succeed but models don't we expect interaction with data source (model, human)
# nested comparisons for sentence types within voice
#
# separate analyses for each argument group
# also correlation analyses including all human subjects who pass the filler exclusion criterion
# to see how well accuracy in actives predicts accuracy in passives for individual tokens
# (i.e., have they learned categories or individual tokens?)
# one where correlation of accuracy across voice (one point per subject)
# one where correlation of accuracy across voice (one point per word per subject)
# also grouped by (seen, unseen), target_response, sentence_type (paired active and passive constructions)
#
# maybe exclude nouns where subjects < 100% on training structure got it wrong?

linear_labels <- geom_text(
	data = exp |> 
		select(subject, data_source, mask_added_tokens, stop_at, linear) |>
		distinct() |>
		group_by(data_source, mask_added_tokens, stop_at, linear) |> 
		summarize(count = sprintf("n=%02d", n())),
	mapping = aes(x = -Inf, y = -Inf, label = count),
	hjust = -0.625,
	vjust = -1,
	inherit.aes = FALSE
)

linear_labels_no_humans <- geom_text(
	data = (
		exp |> 
			filter(data_source != 'human') |>
			select(subject, data_source, mask_added_tokens, stop_at, linear) |>
			distinct() |>
			group_by(data_source, mask_added_tokens, stop_at, linear) |> 
			summarize(count = sprintf("n=%02d", n()))
	),
	mapping = aes(x=-Inf, y=-Inf, label = count),
	hjust=-0.5,
	vjust=-1,
	inherit.aes = FALSE
)

linear_labels_humans <- geom_text(
	data = (
		exp |> 
			filter(data_source == 'human') |>
			select(subject, linear) |>
			distinct() |>
			group_by(linear) |> 
			summarize(count = sprintf("n=%02d", n()))
	),
	mapping = aes(x=-Inf, y=-Inf, label = count),
	hjust=-0.5,
	vjust=-1,
	inherit.aes = FALSE
)

########################################################################
###################### EXPERIMENTAL ITEMS ##############################
########################################################################
# accuracy by voice, target_response, and data_source
exp |> 
	mutate(
		data_source = case_when(
			data_source == 'human' ~ 'People', 
			TRUE ~ paste0(data_source, ' (fine-tuned)')
		)
	) |>
	select(-correct.pre.training) |>
	rbind(
		exp |> 
			filter(data_source != 'human') |>
			select(-correct) |> 
			droplevels() |>
			rename(correct = correct.pre.training) |>
			mutate(data_source = paste0(data_source, ' (pre-fine-tuning)'))
	) |>
	mutate(
		data_source = fct_relevel(data_source, 
			'People', 'BERT (fine-tuned)', 'BERT (pre-fine-tuning)',
			'ModernBERT Base (fine-tuned)', 'ModernBERT Base (pre-fine-tuning)',
			'ModernBERT Large (fine-tuned)', 'ModernBERT Large (pre-fine-tuning)'
		)
	) |>
	ggplot(aes(x = voice, y = as.numeric(correct), fill = target_response)) +
	stat_summary(fun = mean, geom = 'bar', position = 'dodge', width = 0.9, color = 'black') +
	stat_summary(fun.data = beta_ci, geom = 'errorbar', width = 0.33, position = position_dodge(0.9)) +
	ylim(0, 1) +
	scale_x_discrete(
		'Voice',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('Active', 'Passive', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Underlying role',
		breaks = c('Subject target', 'Object target'),
		labels = c('Buildings (subjects)', 'Vehicles (objects')
	) +
	ggtitle(paste0('Pr. Correct by Voice (building subjects; vehicle objects)')) +
	facet_grid(. ~ data_source)

# accuracy by voice, target_response, data_source, and linear
exp |>
	ggplot(aes(x = voice, y = as.numeric(correct), fill = target_response)) +
	stat_summary(fun = mean, geom = 'bar', position = 'dodge', width = 0.9, color = 'black') +
	stat_summary(
		fun.data = beta_ci, geom = 'errorbar', width = 0.33, 
		position = position_dodge(0.9)
	) +
	stat_summary(
		fun.data = \(y) data.frame(y = mean(y), label = sprintf('%.2f', mean(y)), fill = 'white'), 
		geom = 'label', position = position_dodge(0.9), show.legend = FALSE, size = 3
	) +
	linear_labels + 
	ylim(0, 1) +
	scale_x_discrete(
		'Template',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('SVO', 'OVS', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Target response',
		breaks = c('Subject target', 'Object target'),
		labels = c('Subject target', 'Object target')
	) +
	ggtitle(paste0('Pr. Correct by Voice')) +
	facet_grid(linear ~ data_source)

ranks <- full.results |>
	filter(data_source == 'human') |>
	group_by(subject) |>
	summarize(median = median(log.RT)) |>
	arrange(median) |>
	ungroup() |>
	mutate(rank = seq_len(n()))

full.results |>
	left_join(ranks |> select(-median)) |>
	filter(data_source == 'human') |>
	mutate(
		excluded = subject %in% unique(excluded.for.reasons.other.than.linear$subject)
	) |>
	ggplot(aes(x = as.factor(rank), y = log.RT, fill = excluded, pattern = linear)) +
	geom_boxplot_pattern(
		color = "black", 
	   pattern_fill = "black",
	   pattern_angle = 45,
	   pattern_density = 0.1,
	   pattern_spacing = 0.025,
	   pattern_key_scale_factor = 0.6
	) +
	scale_x_discrete(
		breaks = as.factor(ranks$rank),
		labels = ranks$subject
	)

########################################################################
###################### FILLERS #########################################
########################################################################

# accuracy by voice, target_response, and data_source
filler |> 
	mutate(
		data_source = case_when(
			data_source == 'human' ~ 'People', 
			TRUE ~ paste0(data_source, ' (fine-tuned)')
		)
	) |>
	select(-correct.pre.training) |>
	rbind(
		filler |> 
			filter(data_source != 'human') |>
			select(-correct) |> 
			droplevels() |>
			rename(correct = correct.pre.training) |>
			mutate(data_source = paste0(data_source, ' (pre-fine-tuning)'))
	) |>
	mutate(
		data_source = fct_relevel(data_source, 
			'People', 'BERT (fine-tuned)', 'BERT (pre-fine-tuning)',
			'ModernBERT Base (fine-tuned)', 'ModernBERT Base (pre-fine-tuning)',
			'ModernBERT Large (fine-tuned)', 'ModernBERT Large (pre-fine-tuning)'
		)
	) |>
	ggplot(aes(x = voice, y = as.numeric(correct), fill = target_response)) +
	stat_summary(fun = mean, geom = 'bar', position = 'dodge', width = 0.9, color = 'black') +
	stat_summary(fun.data = beta_ci, geom = 'errorbar', width = 0.33, position = position_dodge(0.9)) +
	ylim(0, 1) +
	scale_x_discrete(
		'Voice',
		breaks = c('SVO active', 'OVS passive', 'OSV active'),
		labels = c('Active', 'Passive', '(OSV)')
	) +
	ylab('Pr. Correct') +
	scale_fill_discrete(
		'Underlying role',
		breaks = c('Subject target', 'Object target'),
		labels = c('Subject', 'Object')
	) +
	ggtitle(paste0('Pr. Correct by Voice (fillers for building subjects; vehicle objects)')) +
	facet_grid(. ~ data_source)

########################################################################
###################### MOST RECENT SUBJECTS ############################
########################################################################
# accuracy by session in training
training.accuracy.by.session.no |>
	mutate(session.no = as.factor(session.no)) |>
	complete(session.no) |>
	mutate(
		included = case_when(
			subject %in% excluded.for.reasons.other.than.linear$subject ~ 'Excluded',
			TRUE ~ 'Included'
		)
	) |>
	ggplot(aes(x = session.no, y = pr_first_choice_correct)) +
	geom_bar(stat = 'identity') +
	facet_grid(. ~ paste0(subject, ' (', included, ')')) +
	xlab('Session No.') +
	ylab('Pr. of trials where first choice was correct') +
	expand_limits(y = c(0, 1)) +
	geom_hline(yintercept = 0.9, linetype = 'dashed', alpha = 0.5) +
	ggtitle('Subject performance in training phase') +
	stat_summary(
		fun.data = \(y) data.frame(y = y, label = sprintf('%.2f', y), fill = 'white'),
		geom = 'label', show.legend = FALSE
	)