-- features.load_by_match_day
-- Typical load by session type, for the averages section of the profile page.
-- One row per season (plus 'All seasons') per match-day label per metric, from session
-- days only: number of sessions, mean, median and the 10th to 90th percentile spread.
-- GROUPING SETS builds the per-season rows and the all-seasons rows in one pass.
-- pct_of_match compares each session type's median with a full match in the same season
-- (features.match_reference), so training load can be read against match demand.

WITH by_type AS (
SELECT
  player_id,
  COALESCE(season, 'All seasons')                     AS season,
  match_day,
  ANY_VALUE(match_day_order)                          AS match_day_order,
  metric,
  COUNT(*)                                            AS sessions,
  AVG(value)                                          AS mean,
  QUANTILE_CONT(value, 0.5)                           AS median,
  QUANTILE_CONT(value, 0.1)                           AS p10,
  QUANTILE_CONT(value, 0.9)                           AS p90
FROM features.load_long
WHERE has_session
GROUP BY GROUPING SETS (
  (player_id, season, match_day, metric),
  (player_id, match_day, metric)
)
)

SELECT
  t.*,
  t.median / r.match_median                           AS pct_of_match
FROM by_type AS t
LEFT JOIN features.match_reference AS r USING (player_id, season, metric)
ORDER BY player_id, season, match_day_order, metric
