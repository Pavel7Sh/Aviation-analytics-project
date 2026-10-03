CREATE TABLE flights AS
SELECT *
FROM read_csv(
    '/Users/s1pavel/Desktop/Aviation Analytics/data/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2025_1.csv',
    header = true,
    sample_size = -1);

-- Check the number of imported flights
SELECT COUNT(*) AS total_flights
FROM flights;

-- Check the date range
SELECT
    MIN(FlightDate) AS first_date,
    MAX(FlightDate) AS last_date
FROM flights;

-- Calculate the overall cancellation rate
SELECT
    COUNT(*) AS total_flights,
    SUM(Cancelled) AS cancelled_flights,
    ROUND(100.0 * SUM(Cancelled) / COUNT(*), 2) AS cancelled_pct
FROM flights;

-- Calculate daily cancellation rates for OH
SELECT
    FlightDate,
    COUNT(*) AS total_flights,
    SUM(Cancelled) AS cancelled_flights,
    ROUND(100.0 * SUM(Cancelled) / COUNT(*), 2) AS cancelled_pct
FROM flights
WHERE Reporting_Airline = 'OH'
GROUP BY FlightDate
ORDER BY cancelled_pct DESC;

-- Compare the three peak days (06.01, 30.01, 10.01) with the rest of the month
SELECT
    CASE
        WHEN FlightDate IN ('2025-01-06', '2025-01-10', '2025-01-30')
            THEN 'Three peak days'
        ELSE 'Other days'
    END AS day_group,
    COUNT(*) AS total_flights,
    SUM(Cancelled) AS cancelled_flights,
    ROUND(100.0 * SUM(Cancelled) / COUNT(*), 2) AS cancelled_pct
FROM flights
WHERE Reporting_Airline = 'OH'
GROUP BY day_group
ORDER BY cancelled_pct DESC;

-- 811 / (811 + 1088) × 100 = 42,71
-- The three peak days accounted for 42.71% of OH's monthly cancellations
-- and 10.16% of its scheduled flights.

-- Compare origin airports during the three peak days
SELECT
    Origin,
    COUNT(*) AS total_flights,
    SUM(Cancelled) AS cancelled_flights,
    ROUND(100.0 * SUM(Cancelled) / COUNT(*), 2) AS cancelled_pct
FROM flights
WHERE Reporting_Airline = 'OH'
    AND FlightDate IN ('2025-01-06', '2025-01-10', '2025-01-30')
GROUP BY Origin
ORDER BY cancelled_flights DESC;

-- Examine reported cancellation reasons on the three peak days
SELECT
    FlightDate,
    CASE CancellationCode
        WHEN 'A' THEN 'Carrier'
        WHEN 'B' THEN 'Weather'
        WHEN 'C' THEN 'National Aviation System'
        WHEN 'D' THEN 'Security'
        ELSE 'Unknown / other'
    END AS cancellation_reason,
    COUNT(*) AS cancelled_flights
FROM flights
WHERE Reporting_Airline = 'OH'
    AND Cancelled = 1
    AND FlightDate IN ('2025-01-06', '2025-01-10', '2025-01-30')
GROUP BY FlightDate, cancellation_reason
ORDER BY FlightDate, cancelled_flights DESC;

-- Reported cancellation reasons differed across the three peak days.
-- On January 6, 234 of 235 cancellations were classified as Weather.
-- On January 10, 259 of 262 cancellations were classified as Weather.
-- On January 30, 301 of 314 cancellations were classified as Carrier.
-- These categories do not identify the specific events behind each cancellation.