-- features.match_reference
-- What a full match looks like for this player: the typical value of every load metric
-- in matches lasting 85 to 125 minutes (is_full_match), per season and across all seasons.
-- Used to express each training day as a share of a match.

SELECT
  player_id,
  COALESCE(season, 'All seasons')   AS season,
  metric,
  COUNT(*)                          AS matches,
  QUANTILE_CONT(value, 0.5)         AS match_median
FROM features.load_long
WHERE is_full_match
GROUP BY GROUPING SETS ((player_id, season, metric), (player_id, metric))
ORDER BY player_id, season, metric
