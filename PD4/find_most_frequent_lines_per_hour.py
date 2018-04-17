import pandas as pd
import sys

def get_most_frequent_lines_per_hour(input_df):
	df = pd.DataFrame.copy(input_df)

	df["Hour"] = df.Time.apply(lambda t: int(t[11:13]))
	df = df.loc[:, ["Hour", "Lines", "Brigade"]]

	df2 = df.groupby(["Hour", "Lines"]) \
		.Brigade.nunique() \
		.reset_index() \
		.rename(index=str, columns={"Brigade":"UniqueBrigades"}) \

	res = pd.DataFrame([(hour, group.Lines[group.UniqueBrigades.idxmax()], \
				group.UniqueBrigades.max()) for hour, group in df2.groupby(["Hour"])], \
			columns=["hour", "mostFrequentLine", "uniqueBrigades"]) 

	return res


if len(sys.argv) != 2:
#	print("arguments:")
#	print("csv dataset path")
	pass
else:
	csv_path = sys.argv[1]
	print(get_most_frequent_lines_per_hour(pd.read_csv(csv_path)))
