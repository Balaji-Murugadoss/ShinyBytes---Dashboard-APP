-- SQL Statements from Shiny App

-- Q1-DB Query Logic
USE zomato;
SELECT * 
FROM zomato_rest_char 
WHERE votes BETWEEN 100 AND 1000
AND name LIKE '%flat iron%';


-- Q2-Maps Logic
SELECT name, Latitude, Longitude 
FROM zomato_rest_char 
WHERE Latitude IS NOT NULL AND Longitude IS NOT NULL;