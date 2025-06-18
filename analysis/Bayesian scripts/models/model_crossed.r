source(file.path('Bayesian scripts', 'bayesian_functions.r'))

fit.model(
	data.type = 'accuracy',
	model.type = 'crossed',
	data.file = 'accuracy-data-nested.csv',
	data.function = ident,
	data.sources = c('human', 'BERT', 'ModernBERT Base', 'ModernBERT Large'),
	formula.name = 'crossed'
)
