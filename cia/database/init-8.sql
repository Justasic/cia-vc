-- MySQL dump 10.10
--
-- Host: localhost    Database: cia
-- ------------------------------------------------------
-- Server version	5.0.24a-Debian_9-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `accounts_assetchangeitem`
--

DROP TABLE IF EXISTS `accounts_assetchangeitem`;
CREATE TABLE `accounts_assetchangeitem` (
  `id` int(11) NOT NULL auto_increment,
  `changeset_id` int(11) NOT NULL,
  `field` varchar(32) NOT NULL,
  `new_value` longtext,
  `old_value` longtext,
  PRIMARY KEY  (`id`),
  KEY `accounts_assetchangeitem_changeset_id` (`changeset_id`),
  KEY `accounts_assetchangeitem_field` (`field`)
) AUTO_INCREMENT=15 DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_assetchangeset`
--

DROP TABLE IF EXISTS `accounts_assetchangeset`;
CREATE TABLE `accounts_assetchangeset` (
  `id` int(11) NOT NULL auto_increment,
  `time` datetime NOT NULL,
  `user_id` int(11) NOT NULL,
  `remote_addr` varchar(32) default NULL,
  `content_type_id` int(11) NOT NULL,
  `object_id` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_assetchangeset_time` (`time`),
  KEY `accounts_assetchangeset_user_id` (`user_id`),
  KEY `accounts_assetchangeset_content_type_id` (`content_type_id`),
  KEY `accounts_assetchangeset_object_id` (`object_id`)
) AUTO_INCREMENT=10 DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_author`
--

DROP TABLE IF EXISTS `accounts_author`;
CREATE TABLE `accounts_author` (
  `id` int(11) NOT NULL auto_increment,
  `target_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_author_target_id` (`target_id`)
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_bot`
--

DROP TABLE IF EXISTS `accounts_bot`;
CREATE TABLE `accounts_bot` (
  `id` int(11) NOT NULL auto_increment,
  `network_id` int(11) NOT NULL,
  `location` varchar(64) NOT NULL,
  `filter_mode` smallint(5) unsigned NOT NULL,
  `custom_ruleset` longtext NOT NULL,
  `project_list` longtext NOT NULL,
  `show_project_names` tinyint(1) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_bot_network_id` (`network_id`),
  KEY `accounts_bot_location` (`location`)
) AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_network`
--

DROP TABLE IF EXISTS `accounts_network`;
CREATE TABLE `accounts_network` (
  `id` int(11) NOT NULL auto_increment,
  `uri` varchar(128) NOT NULL,
  `description` varchar(200) NOT NULL,
  `is_popular` tinyint(1) NOT NULL,
  `reviewed_by_admin` tinyint(1) NOT NULL,
  `created_by_id` int(11) default NULL,
  `date_added` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_network_created_by_id` (`created_by_id`)
)  AUTO_INCREMENT=7 DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_project`
--

DROP TABLE IF EXISTS `accounts_project`;
CREATE TABLE `accounts_project` (
  `id` int(11) NOT NULL auto_increment,
  `target_id` int(11) NOT NULL,
  `repos_id` int(11) default NULL,
  `secret_key` varchar(64) default NULL,
  `allow_anonymous_messages` tinyint(1) NOT NULL,
  `allow_trusted_messages` tinyint(1) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_project_target_id` (`target_id`),
  KEY `accounts_project_repos_id` (`repos_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `accounts_userasset`
--

DROP TABLE IF EXISTS `accounts_userasset`;
CREATE TABLE `accounts_userasset` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `content_type_id` int(11) NOT NULL,
  `object_id` int(10) unsigned NOT NULL,
  `access` smallint(5) unsigned NOT NULL,
  `date_added` datetime NOT NULL,
  `trusted_by` datetime default NULL,
  PRIMARY KEY  (`id`),
  KEY `accounts_userasset_user_id` (`user_id`),
  KEY `accounts_userasset_content_type_id` (`content_type_id`)
)  AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Table structure for table `audit_trail`
--

DROP TABLE IF EXISTS `audit_trail`;
CREATE TABLE `audit_trail` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `timestamp` int(10) unsigned NOT NULL,
  `uid` int(10) unsigned NOT NULL,
  `action_domain` varchar(32) NOT NULL,
  `action_name` text NOT NULL,
  `main_param` text,
  `params` longblob,
  `allowed` tinyint(1) NOT NULL,
  `results` longblob,
  PRIMARY KEY  (`id`),
  KEY `id` (`id`),
  KEY `uid` (`uid`),
  CONSTRAINT `audit_trail_ibfk_1` FOREIGN KEY (`uid`) REFERENCES `users` (`uid`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_group`
--

DROP TABLE IF EXISTS `auth_group`;
CREATE TABLE `auth_group` (
  `id` int(11) NOT NULL auto_increment,
  `name` varchar(80) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `name` (`name`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_group_permissions`
--

DROP TABLE IF EXISTS `auth_group_permissions`;
CREATE TABLE `auth_group_permissions` (
  `id` int(11) NOT NULL auto_increment,
  `group_id` int(11) NOT NULL,
  `permission_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `group_id` (`group_id`,`permission_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_message`
--

DROP TABLE IF EXISTS `auth_message`;
CREATE TABLE `auth_message` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `message` longtext NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `auth_message_user_id` (`user_id`)
)  AUTO_INCREMENT=8 DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_permission`
--

DROP TABLE IF EXISTS `auth_permission`;
CREATE TABLE `auth_permission` (
  `id` int(11) NOT NULL auto_increment,
  `name` varchar(50) NOT NULL,
  `content_type_id` int(11) NOT NULL,
  `codename` varchar(100) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `content_type_id` (`content_type_id`,`codename`),
  KEY `auth_permission_content_type_id` (`content_type_id`)
)  AUTO_INCREMENT=73 DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_user`
--

DROP TABLE IF EXISTS `auth_user`;
CREATE TABLE `auth_user` (
  `id` int(11) NOT NULL auto_increment,
  `username` varchar(30) NOT NULL,
  `first_name` varchar(30) NOT NULL,
  `last_name` varchar(30) NOT NULL,
  `email` varchar(75) NOT NULL,
  `password` varchar(128) NOT NULL,
  `is_staff` tinyint(1) NOT NULL,
  `is_active` tinyint(1) NOT NULL,
  `is_superuser` tinyint(1) NOT NULL,
  `last_login` datetime NOT NULL,
  `date_joined` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `username` (`username`)
)  AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_user_groups`
--

DROP TABLE IF EXISTS `auth_user_groups`;
CREATE TABLE `auth_user_groups` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `group_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `user_id` (`user_id`,`group_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `auth_user_user_permissions`
--

DROP TABLE IF EXISTS `auth_user_user_permissions`;
CREATE TABLE `auth_user_user_permissions` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `permission_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `user_id` (`user_id`,`permission_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `blog_post`
--

DROP TABLE IF EXISTS `blog_post`;
CREATE TABLE `blog_post` (
  `id` int(11) NOT NULL auto_increment,
  `slug` varchar(50) NOT NULL,
  `pub_date` datetime NOT NULL,
  `posted_by_id` int(11) NOT NULL,
  `listed` tinyint(1) NOT NULL,
  `title` varchar(100) NOT NULL,
  `content` longtext NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `blog_post_slug` (`slug`),
  KEY `blog_post_pub_date` (`pub_date`),
  KEY `blog_post_posted_by_id` (`posted_by_id`)
)  AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Table structure for table `capabilities`
--

DROP TABLE IF EXISTS `capabilities`;
CREATE TABLE `capabilities` (
  `uid` int(10) unsigned NOT NULL,
  `cap_md5` char(32) NOT NULL,
  `cap_repr` text NOT NULL,
  UNIQUE KEY `uid` (`uid`,`cap_md5`),
  KEY `cap_md5` (`cap_md5`),
  CONSTRAINT `capabilities_ibfk_1` FOREIGN KEY (`uid`) REFERENCES `users` (`uid`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `comments_comment`
--

DROP TABLE IF EXISTS `comments_comment`;
CREATE TABLE `comments_comment` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `content_type_id` int(11) NOT NULL,
  `object_id` int(11) NOT NULL,
  `headline` varchar(255) NOT NULL,
  `comment` longtext NOT NULL,
  `rating1` smallint(5) unsigned default NULL,
  `rating2` smallint(5) unsigned default NULL,
  `rating3` smallint(5) unsigned default NULL,
  `rating4` smallint(5) unsigned default NULL,
  `rating5` smallint(5) unsigned default NULL,
  `rating6` smallint(5) unsigned default NULL,
  `rating7` smallint(5) unsigned default NULL,
  `rating8` smallint(5) unsigned default NULL,
  `valid_rating` tinyint(1) NOT NULL,
  `submit_date` datetime NOT NULL,
  `is_public` tinyint(1) NOT NULL,
  `ip_address` char(15) default NULL,
  `is_removed` tinyint(1) NOT NULL,
  `site_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `comments_comment_user_id` (`user_id`),
  KEY `comments_comment_content_type_id` (`content_type_id`),
  KEY `comments_comment_site_id` (`site_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `comments_freecomment`
--

DROP TABLE IF EXISTS `comments_freecomment`;
CREATE TABLE `comments_freecomment` (
  `id` int(11) NOT NULL auto_increment,
  `content_type_id` int(11) NOT NULL,
  `object_id` int(11) NOT NULL,
  `comment` longtext NOT NULL,
  `person_name` varchar(50) NOT NULL,
  `submit_date` datetime NOT NULL,
  `is_public` tinyint(1) NOT NULL,
  `ip_address` char(15) NOT NULL,
  `approved` tinyint(1) NOT NULL,
  `site_id` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `comments_freecomment_content_type_id` (`content_type_id`),
  KEY `comments_freecomment_site_id` (`site_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `comments_karmascore`
--

DROP TABLE IF EXISTS `comments_karmascore`;
CREATE TABLE `comments_karmascore` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `comment_id` int(11) NOT NULL,
  `score` smallint(6) NOT NULL,
  `scored_date` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `user_id` (`user_id`,`comment_id`),
  KEY `comments_karmascore_user_id` (`user_id`),
  KEY `comments_karmascore_comment_id` (`comment_id`),
  KEY `comments_karmascore_score` (`score`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `comments_moderatordeletion`
--

DROP TABLE IF EXISTS `comments_moderatordeletion`;
CREATE TABLE `comments_moderatordeletion` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `comment_id` int(11) NOT NULL,
  `deletion_date` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `user_id` (`user_id`,`comment_id`),
  KEY `comments_moderatordeletion_user_id` (`user_id`),
  KEY `comments_moderatordeletion_comment_id` (`comment_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `comments_userflag`
--

DROP TABLE IF EXISTS `comments_userflag`;
CREATE TABLE `comments_userflag` (
  `id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `comment_id` int(11) NOT NULL,
  `flag_date` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `user_id` (`user_id`,`comment_id`),
  KEY `comments_userflag_user_id` (`user_id`),
  KEY `comments_userflag_comment_id` (`comment_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `django_content_type`
--

DROP TABLE IF EXISTS `django_content_type`;
CREATE TABLE `django_content_type` (
  `id` int(11) NOT NULL auto_increment,
  `name` varchar(100) NOT NULL,
  `app_label` varchar(100) NOT NULL,
  `model` varchar(100) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `app_label` (`app_label`,`model`)
)  AUTO_INCREMENT=25 DEFAULT CHARSET=latin1;

--
-- Table structure for table `django_session`
--

DROP TABLE IF EXISTS `django_session`;
CREATE TABLE `django_session` (
  `session_key` varchar(40) NOT NULL,
  `session_data` longtext NOT NULL,
  `expire_date` datetime NOT NULL,
  PRIMARY KEY  (`session_key`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `django_site`
--

DROP TABLE IF EXISTS `django_site`;
CREATE TABLE `django_site` (
  `id` int(11) NOT NULL auto_increment,
  `domain` varchar(100) NOT NULL,
  `name` varchar(50) NOT NULL,
  PRIMARY KEY  (`id`)
)  AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Table structure for table `images_imageinstance`
--

DROP TABLE IF EXISTS `images_imageinstance`;
CREATE TABLE `images_imageinstance` (
  `id` int(11) NOT NULL auto_increment,
  `source_id` int(11) NOT NULL,
  `is_original` tinyint(1) NOT NULL,
  `thumbnail_size` int(10) unsigned default NULL,
  `path` varchar(32) NOT NULL,
  `delete_file` tinyint(1) NOT NULL,
  `width` int(10) unsigned NOT NULL,
  `height` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `images_imageinstance_source_id` (`source_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `images_imagesource`
--

DROP TABLE IF EXISTS `images_imagesource`;
CREATE TABLE `images_imagesource` (
  `id` int(11) NOT NULL auto_increment,
  `is_temporary` tinyint(1) NOT NULL,
  `reviewed_by_admin` tinyint(1) NOT NULL,
  `created_by_id` int(11) default NULL,
  `date_added` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `images_imagesource_created_by_id` (`created_by_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `meta`
--

DROP TABLE IF EXISTS `meta`;
CREATE TABLE `meta` (
  `name` varchar(32) default NULL,
  `value` varchar(255) default NULL
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `repos_repository`
--

DROP TABLE IF EXISTS `repos_repository`;
CREATE TABLE `repos_repository` (
  `id` int(11) NOT NULL auto_increment,
  `is_active` tinyint(1) NOT NULL,
  `type` smallint(5) unsigned NOT NULL,
  `location` varchar(255) NOT NULL,
  `enable_polling` tinyint(1) NOT NULL,
  `poll_frequency` int(10) unsigned NOT NULL,
  `pinger_name` varchar(64) NOT NULL,
  `forward_pinger_mail` tinyint(1) NOT NULL,
  `project_name` varchar(128) NOT NULL,
  `created_by_id` int(11) default NULL,
  `default_module_name` varchar(64) default NULL,
  `path_regexes` longtext,
  `revision_url` varchar(255) default NULL,
  `root_url` varchar(255) default NULL,
  `uuid` varchar(255) default NULL,
  `last_revision` int(10) unsigned default NULL,
  `last_update_time` datetime default NULL,
  PRIMARY KEY  (`id`),
  KEY `repos_repository_pinger_name` (`pinger_name`),
  KEY `repos_repository_project_name` (`project_name`),
  KEY `repos_repository_created_by_id` (`created_by_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `rulesets`
--

DROP TABLE IF EXISTS `rulesets`;
CREATE TABLE `rulesets` (
  `uri` text NOT NULL,
  `xml` text NOT NULL
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_catalog`
--

DROP TABLE IF EXISTS `stats_catalog`;
CREATE TABLE `stats_catalog` (
  `parent_path` varchar(128) default NULL,
  `target_path` varchar(128) NOT NULL,
  PRIMARY KEY  (`target_path`),
  KEY `parent_path` (`parent_path`),
  CONSTRAINT `stats_catalog_ibfk_1` FOREIGN KEY (`parent_path`) REFERENCES `stats_catalog` (`target_path`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_counters`
--

DROP TABLE IF EXISTS `stats_counters`;
CREATE TABLE `stats_counters` (
  `target_path` varchar(128) NOT NULL default '',
  `name` varchar(32) NOT NULL default '',
  `event_count` int(10) unsigned NOT NULL default '0',
  `first_time` int(10) unsigned default NULL,
  `last_time` int(10) unsigned default NULL,
  PRIMARY KEY  (`target_path`,`name`),
  KEY `target_path` (`target_path`),
  KEY `name` (`name`),
  KEY `event_count` (`event_count`),
  KEY `first_time` (`first_time`),
  KEY `last_time` (`last_time`),
  CONSTRAINT `stats_counters_ibfk_1` FOREIGN KEY (`target_path`) REFERENCES `stats_catalog` (`target_path`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_metadata`
--

DROP TABLE IF EXISTS `stats_metadata`;
CREATE TABLE `stats_metadata` (
  `target_path` varchar(128) NOT NULL default '',
  `name` varchar(32) NOT NULL default '',
  `mime_type` varchar(32) NOT NULL,
  `value` longblob NOT NULL,
  `mtime` int(10) unsigned default NULL,
  PRIMARY KEY  (`target_path`,`name`),
  KEY `target_path` (`target_path`),
  KEY `name` (`name`),
  CONSTRAINT `stats_metadata_ibfk_1` FOREIGN KEY (`target_path`) REFERENCES `stats_catalog` (`target_path`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_relations`
--

DROP TABLE IF EXISTS `stats_relations`;
CREATE TABLE `stats_relations` (
  `target_a_path` varchar(128) NOT NULL default '',
  `target_b_path` varchar(128) NOT NULL default '',
  `strength` int(10) unsigned NOT NULL default '0',
  `freshness` int(10) unsigned default NULL,
  PRIMARY KEY  (`target_a_path`,`target_b_path`),
  KEY `target_a_path` (`target_a_path`),
  KEY `target_b_path` (`target_b_path`),
  CONSTRAINT `stats_relations_ibfk_1` FOREIGN KEY (`target_a_path`) REFERENCES `stats_catalog` (`target_path`) ON DELETE CASCADE,
  CONSTRAINT `stats_relations_ibfk_2` FOREIGN KEY (`target_b_path`) REFERENCES `stats_catalog` (`target_path`) ON DELETE CASCADE
) DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_statstarget`
--

DROP TABLE IF EXISTS `stats_statstarget`;
CREATE TABLE `stats_statstarget` (
  `id` int(11) NOT NULL auto_increment,
  `path` varchar(255) NOT NULL,
  `title` varchar(128) default NULL,
  `subtitle` varchar(128) default NULL,
  `url` varchar(255) default NULL,
  `description` longtext,
  `photo_id` int(11) default NULL,
  `icon_id` int(11) default NULL,
  `links_filter` longtext,
  `related_filter` longtext,
  PRIMARY KEY  (`id`),
  KEY `stats_statstarget_path` (`path`),
  KEY `stats_statstarget_photo_id` (`photo_id`),
  KEY `stats_statstarget_icon_id` (`icon_id`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `stats_subscriptions`
--

DROP TABLE IF EXISTS `stats_subscriptions`;
CREATE TABLE `stats_subscriptions` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `target_path` varchar(128) NOT NULL,
  `expiration` int(10) unsigned NOT NULL,
  `scope` varchar(32) default NULL,
  `client` varchar(64) default NULL,
  `trigger` blob NOT NULL,
  `failures` int(11) NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `target_path` (`target_path`),
  KEY `expiration` (`expiration`),
  KEY `client` (`client`)
)  DEFAULT CHARSET=latin1;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `uid` int(10) unsigned NOT NULL auto_increment,
  `secret_key` varchar(32) NOT NULL,
  `active` tinyint(1) NOT NULL default '1',
  `full_name` text,
  `email` text,
  `creation_time` int(10) unsigned NOT NULL,
  `key_atime` int(10) unsigned default NULL,
  `login_name` varchar(32) default NULL,
  `login_passwd_md5` char(32) default NULL,
  `login_atime` int(10) unsigned default NULL,
  `login_mtime` int(10) unsigned default NULL,
  PRIMARY KEY  (`uid`),
  KEY `secret_key` (`secret_key`),
  KEY `login_name` (`login_name`)
) DEFAULT CHARSET=latin1;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

