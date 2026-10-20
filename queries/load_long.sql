-- features.load_long
-- One row per player per calendar day per load metric, for the longitudinal page.
--   value        the day's value; 0 on days without a session, so bars show rest days
--   rolling_7d   7-day total (days off count as 0)
--   rolling_28d  28-day weekly average: the 28-day total divided by 4, so it is in the
--                same per-week units as rolling_7d and both fit on one chart
--   *_complete   FALSE until the window has a full 7 or 28 days of data. GPS has no
--                June or July, so windows restart each August.
-- Peak speed is a maximum, not an amount, so its rolling values are the highest
-- peak speed in the window rather than a total. Per-minute values are rates, so their
-- rolling values are the average over the sessions in the window.
-- The rolling windows use calendar days (RANGE), so a missing date never shortens them.

WITH long AS (
  SELECT *
  FROM features.daily_load
  UNPIVOT INCLUDE NULLS (
    session_value FOR metric IN (
      distance, distance_over_21, distance_over_24, distance_over_27,
      accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
      day_duration, peak_speed,
      hr_zone_1_min, hr_zone_2_min, hr_zone_3_min, hr_zone_4_min, hr_zone_5_min,
      hr_yellow_min, hr_red_min,
      distance_per_min, distance_over_21_per_min, distance_over_24_per_min, distance_over_27_per_min,
      accel_decel_over_2_5_per_min, accel_decel_over_3_5_per_min, accel_decel_over_4_5_per_min
    )
  )
),

daily AS (
  SELECT
    player_id, date, season, match_day, match_day_order, has_session, is_match_day, is_full_match, metric,
    session_value,
    COALESCE(session_value, 0) AS value
  FROM long
)

SELECT
  d.* EXCLUDE (session_value),
  CASE WHEN metric = 'peak_speed'        THEN MAX(value) OVER w7
       WHEN metric LIKE '%\_per\_min' ESCAPE '\' THEN AVG(session_value) OVER w7
       ELSE SUM(value) OVER w7 END                                      AS rolling_7d,
  CASE WHEN metric = 'peak_speed'        THEN MAX(value) OVER w28
       WHEN metric LIKE '%\_per\_min' ESCAPE '\' THEN AVG(session_value) OVER w28
       ELSE SUM(value) OVER w28 / 4 END                                 AS rolling_28d,
  COUNT(*) OVER w7  = 7                                                 AS rolling_7d_complete,
  COUNT(*) OVER w28 = 28                                                AS rolling_28d_complete
FROM daily AS d
WINDOW
  w7  AS (PARTITION BY player_id, metric ORDER BY date
          RANGE BETWEEN INTERVAL 6 DAYS PRECEDING AND CURRENT ROW),
  w28 AS (PARTITION BY player_id, metric ORDER BY date
          RANGE BETWEEN INTERVAL 27 DAYS PRECEDING AND CURRENT ROW)
ORDER BY player_id, metric, date
