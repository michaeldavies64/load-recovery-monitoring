-- monitoring.player_status
-- The player's current status for the profile page: the most recent value of every GPS
-- load metric and every recovery score, with its range and status, and how long ago it
-- was measured. Every metric is included; the app lets the user hide any of them.
--
-- GPS rows also carry pct_of_match, so a session without an adaptive range (such as a
-- short substitute appearance) can still be compared with a full match.
--
-- The app's date (app_date) is fixed at the last day with any data: the latest GPS
-- session or recovery score, 13 March 2025. "Current" means as of then. The GPS file
-- carries on for another 29 days, but only with days without a session, which say nothing
-- about the player's status.

WITH app AS (
  SELECT GREATEST(
    (SELECT MAX(date) FROM features.daily_load WHERE has_session),
    (SELECT MAX(date) FROM staging.recovery WHERE value IS NOT NULL)
  ) AS app_date
),

latest_load AS (
  SELECT *
  FROM monitoring.load_ranges
  QUALIFY ROW_NUMBER() OVER (PARTITION BY player_id, metric ORDER BY date DESC) = 1
),

latest_recovery AS (
  SELECT *
  FROM monitoring.recovery_ranges
  QUALIFY ROW_NUMBER() OVER (PARTITION BY player_id, metric ORDER BY date DESC) = 1
)

SELECT
  'GPS'                         AS source,
  g.group                       AS metric_group,
  g.display_order,
  l.player_id, l.metric, g.label, g.short_label, g.unit, g.decimals,
  l.date, a.app_date, a.app_date - l.date AS days_ago, l.match_day,
  l.value, l.typical, l.lower, l.upper, l.score, l.status, l.pct_of_match
FROM latest_load AS l
JOIN reference.gps_glossary AS g ON g.column_name = l.metric
CROSS JOIN app AS a

UNION ALL

SELECT
  'Recovery'                    AS source,
  g.measure                     AS metric_group,
  100 + g.display_order         AS display_order,
  r.player_id, r.metric, g.label, g.short_label, g.unit, g.decimals,
  r.date, a.app_date, a.app_date - r.date AS days_ago, r.match_day,
  r.value, r.typical, r.lower, r.upper, r.score, r.status, NULL AS pct_of_match
FROM latest_recovery AS r
JOIN reference.recovery_glossary AS g ON g.metric = r.metric
CROSS JOIN app AS a

ORDER BY player_id, display_order
