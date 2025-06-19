accuracy.formulas <- list(
	crossed = correct ~ voice.n * data_source.n * target_response.n +
		(1 + voice.n * target_response.n | subject:data_source.n) +
		(1 + data_source.n | item:voice.n:target_response.n),
	voice.n_i_n_tr_i_n_ds = correct ~ 
		voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_human + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_human + 
		voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_nonhuman + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_nonhuman + 
		target_response.n * data_source.n +
		(
			1 + voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_human + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_human +
			    voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_nonhuman + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_nonhuman + 
			    target_response.n | subject:data_source.n
		) +
		(
			1 + data_source.n | item:(
				voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_human + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_human +
				voice.n_i_n_tr_a_t_object_target_i_n_ds_a_t_nonhuman + voice.n_i_n_tr_a_t_subject_target_i_n_ds_a_t_nonhuman
			):target_response.n
		)
)

suppressMessages(library(brms))
suppressMessages(library(bayesplot))

# brms will not accept essentially any
# symbol in a variable name, so we're
# stuck with this. it also won't 
# take double underscores.
NESTING_SEPARATOR <- '_i_n_'
LEVEL_SEPARATOR <- '_a_t_'

# for filtering RTs
MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

# Create directories to store results
models.dir <- file.path('Models', 'Bayesian')
plots.dir <- models.dir

bayesplot_theme_set(theme_default(base_family = getOption('bayesplot.base_family', 'sans')))

brm.args <- list(
	iter = 6500, 
	chains = 4, 
	cores = 4,
	refresh = 650,
	backend = 'cmdstanr', 
	threads = threading(4, static = TRUE),
	control = list(adapt_delta = 0.99),
	seed = 425
)