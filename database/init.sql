--
-- This file initializes CIA's SQL database
-- This version requires MySQL, and you -must-
-- be using a MySQL server with InnoDB enabled.
--
-- For example:
--  mysql -u root -p < init.sql
--
-- Micah Dowty
--

CREATE DATABASE IF NOT EXISTS cia;
USE cia;

----------------------------------------------------------- Security

CREATE TABLE IF NOT EXISTS capabilities
(
    key_data  TEXT NOT NULL,
    id        TEXT NOT NULL,
    owner     TEXT
);

----------------------------------------------------------- Rulesets

CREATE TABLE IF NOT EXISTS rulesets
(
    uri TEXT NOT NULL,
    xml TEXT NOT NULL
);

----------------------------------------------------------- Stats

-- Map the full path of each stats target to its parent.
-- Every stats target we know about is in this table, and
-- deleting a target causes all metadata, messages, subtargets,
-- and counters for that target to be automatically deleted.
CREATE TABLE IF NOT EXISTS stats_catalog
(
    parent_path  VARCHAR(128),
    target_path  VARCHAR(128) PRIMARY KEY,

    FOREIGN KEY (parent_path) REFERENCES stats_catalog (target_path) ON DELETE CASCADE,
    INDEX (parent_path)
) TYPE=INNODB;

-- A place to store all stats messages, with a global continuously increasing ID.
-- This doesn't yet include rules to automatically delete old messages, so the log is infinite.
-- Note the index on (id). This is required to get reasonable performance.
CREATE TABLE IF NOT EXISTS stats_messages
(
    target_path  VARCHAR(128),
    id           BIGINT PRIMARY KEY AUTO_INCREMENT,
    timestamp    BIGINT,
    xml          TEXT NOT NULL,

    INDEX (target_path),
    FOREIGN KEY (target_path) REFERENCES stats_catalog(target_path) ON DELETE CASCADE
) TYPE=INNODB;

-- Store metadata keys, times, and values for each target
CREATE TABLE IF NOT EXISTS stats_metadata
(
    target_path  VARCHAR(128),
    name         VARCHAR(32),
    mime_type    VARCHAR(32),
    value        LONGBLOB,

    PRIMARY KEY(target_path, name),
    INDEX (target_path),
    INDEX (name),
    FOREIGN KEY (target_path) REFERENCES stats_catalog(target_path) ON DELETE CASCADE
) TYPE=INNODB;

-- Store counters
CREATE TABLE IF NOT EXISTS stats_counters
(
    target_path  VARCHAR(128),
    name         VARCHAR(32),
    event_count  INT NOT NULL DEFAULT 0,
    first_time   BIGINT,
    last_time    BIGINT,

    PRIMARY KEY(target_path, name),
    INDEX (target_path),
    INDEX (name),
    FOREIGN KEY (target_path) REFERENCES stats_catalog(target_path) ON DELETE CASCADE
) TYPE=INNODB;

--- The End ---