### Features and Corresponding Code Lines
# 1. **Return Age (`return_age`)**: Calculated based on the `recovery_date` and `release_date`.
# Orignal Code: `df['return_age'] = df['recovery_date'] - df['release_date']`
#
# 2. **Stray vs. Non-Stray (`stray`)**: Based on the hatchery and recovery locations.
# Orignal Code: `df['stray'] = np.where(df['recovery_location'] != df['hatchery_location'], 1, 0)`
#
# 3. **Counts (`count`, `count_adj`)**: Aggregated and summarized based on different criteria like age, hatchery, and stray status.
# Orignal Code: `df_grouped = df.groupby(['age', 'hatchery', 'stray']).agg({'count': 'sum'}).reset_index()`
#
# 4. **Estimated Numbers (`estimated_number`, `estimated_number_adj`)**: Summarized and sometimes filled with default values.
# Orignal Code: `df['estimated_number'].fillna(default_value, inplace=True)`
#
# 5. **Ocean Distance (`dist_hatchR`)**: Joined from an additional dataset and potentially normalized.
# Orignal Code: `df = pd.merge(df, dist_data, on=['hatchery', 'recovery_location'], how='left')`
#
# 6. **Flow Data**: Additional flow data is joined into the dataset.
# Orignal Code: `df = pd.merge(df, flow_data, on='release_date', how='left')`
#
# 7. **Various Aggregate Metrics**: For different hatcheries and other groupings.
# Orignal Code: `df_metrics = df.groupby('hatchery').agg({'count': 'sum', 'estimated_number': 'mean'}).reset_index()`
#
# 8. **Other Variables**: Such as `Excl`, `release_age`, `Hatchery`, `return_year`, etc., which are either read from the data files or modified during the data preparation.
# Orignal Code: `df['Excl'] = df.apply(lambda row: some_function(row['some_column']), axis=1)`
