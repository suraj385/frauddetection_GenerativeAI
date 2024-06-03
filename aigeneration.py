import pandas as pd
from ctgan import CTGAN
from table_evaluator import TableEvaluator


file_path = 'p1.csv'
data = pd.read_csv(file_path)


discrete_columns = ['Class']


num_epochs = 2 * len(data)


ctgan_model = CTGAN(verbose=True)


ctgan_model.fit(data, discrete_columns, epochs=num_epochs)


num_samples_to_generate = int(0.5 * len(data))  # Generate 50% more data
synthetic_data = ctgan_model.sample(num_samples_to_generate)


synthetic_data.to_csv('generation.csv', index=False)


length_of_generated_data = len(synthetic_data)
print(f"Length of the generated dataset: {length_of_generated_data}")

combined_data = pd.concat([data, synthetic_data], ignore_index=True)


combined_data.to_csv('combined_dataset.csv', index=False)
print(data.shape , synthetic_data.shape)
table_evaluator = TableEvaluator(data, synthetic_data,cat_cols=discrete_columns)
table_evaluator.visual_evaluation()

