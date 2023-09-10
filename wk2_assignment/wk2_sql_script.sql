CREATE DATABASE movies;

# Use the new database to create a table with the movie ratings data collected
USE movies;

CREATE TABLE movie_ratings(
user_id INT AUTO_INCREMENT PRIMARY KEY,
barbie INT,
oppenheimer INT,
the_little_mermaid INT,
guardians_of_the_galaxy INT,
spiderman_across_the_spiderverse INT,
the_super_mario_bros_movie INT
);

#Insert values into the table from the 
INSERT INTO movie_ratings(barbie, oppenheimer, the_little_mermaid, 
guardians_of_the_galaxy, spiderman_across_the_spiderverse, the_super_mario_bros_movie)
VALUES 
(4, 4, 3, 5, 4, 2),
(5, NULL, NULL, 4, NULL, NULL),
(NULL, NULL, 1, 2, 2, 1), 
(4, 4, 3, 4, 4, 4),
(4, NULL, 5, 3, 3, 2);  
