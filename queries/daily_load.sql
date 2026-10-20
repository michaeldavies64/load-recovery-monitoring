-- features.daily_load
-- One row per player per calendar day, built from staging.gps.
--   season, is_baseline_season  2022/23 is the baseline season: it sets the priors for
--                               the adaptive ranges and gets no flags itself
--   has_session                 TRUE when any distance was recorded
--   load metrics                NA on days without a session, so averages and ranges
--                               use session days only. Rolling totals, which should
--                               count a day off as zero load, replace NA with 0 themselves.
--   match_day                   hybrid match-week label (see below); match_day_order sorts it
--   has_range                   FALSE for extended-gap days, which get no adaptive range
--   is_full_match               TRUE for match days lasting 85 to 125 minutes (see below)
--   has_load_range              has_range, and for match days only those in the band
--   *_per_min                   distance, speed bands and efforts divided by duration
--   md_plus_code, md_minus_code kept as supplied
--
-- Match-duration band. Match-day durations run from 8 to 156 minutes with no natural gap
-- between starts and substitute appearances, and they do not follow real match length
-- (the 2024 League Cup final went to extra time but records 73 minutes). Matches lasting
-- 85 to 125 minutes are treated as full matches: they are the match-day reference for the
-- adaptive ranges and for comparing training days with matches. Shorter and longer match
-- days keep the MD label but get no load range. Recovery ranges are unaffected.
--
-- Match-day label (hybrid). Each day has two codes: days since the last match and days to
-- the next. The label keeps each day's purpose:
--   MD               match day
--   MD-1             the day before a match, always, even in a 3-day turnaround
--   MD+1, MD+2       the two days after a match
--   MD-5 to MD-2     the days in between, counting down to the next match
--   Extended gap     anything further from a match: pre-season, international breaks,
--                    the end of a season. Mixed purposes, so no adaptive range.

WITH settings AS (
  SELECT
    '2022/2023' AS baseline_season,
    85          AS full_match_min,
    125         AS full_match_max
)

SELECT
  g.player_id,
  g.date,
  g.season,
  g.season = s.baseline_season                                     AS is_baseline_season,
  g.distance > 0                                                   AS has_session,
  CASE
    WHEN g.md_plus_code = 0          THEN 'MD'
    WHEN g.md_minus_code = -1        THEN 'MD-1'
    WHEN g.md_plus_code IN (1, 2)    THEN 'MD+' || g.md_plus_code
    WHEN g.md_minus_code >= -5       THEN 'MD' || g.md_minus_code
    ELSE 'Extended gap'
  END                                                              AS match_day,
  CASE
    WHEN g.md_plus_code = 0          THEN 8
    WHEN g.md_minus_code = -1        THEN 7
    WHEN g.md_plus_code IN (1, 2)    THEN g.md_plus_code
    WHEN g.md_minus_code >= -5       THEN 8 + g.md_minus_code
    ELSE 9
  END                                                              AS match_day_order,
  NOT (g.md_plus_code NOT IN (0, 1, 2) AND g.md_minus_code < -5)  AS has_range,
  g.md_plus_code = 0                                               AS is_match_day,
  g.md_plus_code = 0 AND g.distance > 0
    AND g.day_duration BETWEEN s.full_match_min AND s.full_match_max AS is_full_match,
  NOT (g.md_plus_code NOT IN (0, 1, 2) AND g.md_minus_code < -5)
    AND (g.md_plus_code <> 0
         OR g.day_duration BETWEEN s.full_match_min AND s.full_match_max) AS has_load_range,
  g.md_plus_code,
  g.md_minus_code,
  g.opposition_code,
  g.opposition_full,

  -- Load metrics: kept only on session days
  CASE WHEN g.distance > 0 THEN g.distance             END         AS distance,
  CASE WHEN g.distance > 0 THEN g.distance_over_21     END         AS distance_over_21,
  CASE WHEN g.distance > 0 THEN g.distance_over_24     END         AS distance_over_24,
  CASE WHEN g.distance > 0 THEN g.distance_over_27     END         AS distance_over_27,
  CASE WHEN g.distance > 0 THEN g.accel_decel_over_2_5 END         AS accel_decel_over_2_5,
  CASE WHEN g.distance > 0 THEN g.accel_decel_over_3_5 END         AS accel_decel_over_3_5,
  CASE WHEN g.distance > 0 THEN g.accel_decel_over_4_5 END         AS accel_decel_over_4_5,
  CASE WHEN g.distance > 0 THEN g.day_duration         END         AS day_duration,
  CASE WHEN g.distance > 0 THEN g.peak_speed           END         AS peak_speed,
  CASE WHEN g.distance > 0 THEN g.hr_zone_1_min        END         AS hr_zone_1_min,
  CASE WHEN g.distance > 0 THEN g.hr_zone_2_min        END         AS hr_zone_2_min,
  CASE WHEN g.distance > 0 THEN g.hr_zone_3_min        END         AS hr_zone_3_min,
  CASE WHEN g.distance > 0 THEN g.hr_zone_4_min        END         AS hr_zone_4_min,
  CASE WHEN g.distance > 0 THEN g.hr_zone_5_min        END         AS hr_zone_5_min,

  -- Heart-rate time in two bands: yellow is 50-80% of max HR (zones 1 to 3) and red is
  -- 80-100% (zones 4 and 5). The zones don't overlap, so they can be added. Time below
  -- 50% of max HR is not recorded, so yellow is not all the time below 80%.
  CASE WHEN g.distance > 0 THEN g.hr_zone_1_min + g.hr_zone_2_min + g.hr_zone_3_min END AS hr_yellow_min,
  CASE WHEN g.distance > 0 THEN g.hr_zone_4_min + g.hr_zone_5_min                   END AS hr_red_min,

  -- Per-minute values: intensity rather than volume. Duration may include time outside
  -- the session itself (such as warm-ups), so these understate true intensity, but they
  -- are consistent across the data.
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.distance             / g.day_duration END AS distance_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.distance_over_21     / g.day_duration END AS distance_over_21_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.distance_over_24     / g.day_duration END AS distance_over_24_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.distance_over_27     / g.day_duration END AS distance_over_27_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.accel_decel_over_2_5 / g.day_duration END AS accel_decel_over_2_5_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.accel_decel_over_3_5 / g.day_duration END AS accel_decel_over_3_5_per_min,
  CASE WHEN g.distance > 0 AND g.day_duration > 0 THEN g.accel_decel_over_4_5 / g.day_duration END AS accel_decel_over_4_5_per_min

FROM staging.gps AS g
CROSS JOIN settings AS s
ORDER BY g.player_id, g.date
