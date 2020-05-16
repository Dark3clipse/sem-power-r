DELIMITER $$
DROP PROCEDURE IF EXISTS preprocess;
CREATE PROCEDURE preprocess()
BEGIN
	DECLARE mx FLOAT DEFAULT 1;

    #first, obtain the top tracks  ----------------------------------------- ~ 0.0014s without index, 0.0027s with index (INDEX skey (cid_0))
    DROP TABLE IF EXISTS tmp_top;
    CREATE TEMPORARY TABLE tmp_top
	(PRIMARY KEY p_id_ttop (pos_0))
	AS (
        SELECT t.pos as 'pos_0', t.concept_id as 'cid_0'
        FROM top t
        WHERE t.concept_type = 'track'
        AND t.participant_id = userid
    );

    #first order ------------------------------------------------ ~ 11.0969s with index (INDEX skey (cid_0)), 9.8208s without index (INDEX skey (cid_0))
    DROP TABLE IF EXISTS tmp_1;
    CREATE TEMPORARY TABLE tmp_1
    (id INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
    INDEX skey (cid_1))
	AS (
        SELECT p.*, t.pos as 'pos_1', t.concept_id as 'cid_1'
        FROM top t
        INNER JOIN tmp_top p
        ON p.cid_0 = t.seed
        WHERE t.concept_type = 'track'
    );

    #second order  ------------------------------------------------ ~ 30.5354s with all 3 indices
    DROP TABLE IF EXISTS tmp_2;
    CREATE TEMPORARY TABLE tmp_2
    (id INT PRIMARY KEY NOT NULL AUTO_INCREMENT)
	AS (
        SELECT p.pos_0, p.cid_0, p.pos_1, p.cid_1, t.pos as 'pos_2', t.concept_id as 'cid_2'
        FROM top t
        INNER JOIN tmp_1 p
        ON p.cid_1 = t.seed
        WHERE t.concept_type = 'track'
    );


    #insert in temporary table withe unique concept id
    DROP TABLE IF EXISTS tmp_insert;
	CREATE TEMPORARY TABLE tmp_insert (
	  concept_id varchar(24) NOT NULL,
	  score float NOT NULL,
	  scoring_info varchar(256) NOT NULL,
	  PRIMARY KEY (concept_id)
	);

    #generate ratings for the top tracks
    INSERT INTO tmp_insert(
    	concept_id, score, scoring_info
    )
    SELECT DISTINCT t.cid_0 as 'concept_id', ((r0_count-t.pos_0)/r0_count)*(r0_max-r0_min)+r0_min as 'score', '' # CONCAT('0:', t.pos_0, ',')
    FROM tmp_top t
    ON DUPLICATE KEY UPDATE score = score + VALUES(score);

    #generate ratings for the first order
    INSERT INTO tmp_insert(
    	concept_id, score, scoring_info
    )
    SELECT DISTINCT t.cid_1 as 'concept_id', ((r1_count-t.pos_1)/r1_count)*(r1_max-r1_min)+r1_min as 'score', '' # CONCAT('1:', t.pos_1, ',')
    FROM tmp_1 t
    ON DUPLICATE KEY UPDATE score = score + VALUES(score), scoring_info = CONCAT(scoring_info, VALUES(scoring_info));

    #generate ratings for the second order
    INSERT INTO tmp_insert(
    	concept_id, score, scoring_info
    )
    SELECT DISTINCT t.cid_2 as 'concept_id', ((r2_count-t.pos_2)/r2_count)*(r2_max-r2_min)+r2_min as 'score', '' # CONCAT('2:', t.pos_2, ',')
    FROM tmp_2 t
    ON DUPLICATE KEY UPDATE score = score + VALUES(score), scoring_info = CONCAT(scoring_info, VALUES(scoring_info));


    #drop temporary tables
    DROP TABLE tmp_top;
    DROP TABLE tmp_1;
    DROP TABLE tmp_2;

    #normalize scores
	SET mx = (SELECT MAX(score) FROM tmp_insert);

    #start transaction
	START TRANSACTION;

	#clear old ratings
    DELETE FROM recommendations
    WHERE algorithm=alg
    AND configuration=config
    AND participant_id=userid
	AND concept_type='track';

	#insert new ratings
	INSERT INTO recommendations(
    	concept_id, concept_type, participant_id, algorithm, configuration, score, scoring_info, timestamp
    )
    SELECT DISTINCT t.concept_id, 'track', userid, alg, config, -t.score/mx, t.scoring_info, NOW()
    FROM tmp_insert t;

    #commit transaction
    COMMIT;

    #drop temporary tables
    DROP TABLE tmp_insert;
END$$
DELIMITER ;
