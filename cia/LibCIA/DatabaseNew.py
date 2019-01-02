""" LibCIA.Database

Utilities for accessing CIA's persistent data stored in an SQL database
"""
#
# CIA open source notification system
# Copyright (C) 2003-2007 Micah Dowty <micah@navi.cx>
# Copyright (C) 2013-2019 Justin Crawford <Justin@stacksmash.net>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
from sqlalchemy import Column, ForeignKey, Integer, PickleType, String, create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship

Base = declarative_base()


class AuditTrail(Base):
	__tablename__ = 'audit_trail'

	id = Column(Integer, primary_key=True)
	timestamp = Column(Integer)
	uid = Column(Integer)
	action_domain = Column(String(96))
	action_name = Column(String())
	# The large number forces SQLAlchemy to make a LongText string instead of varchar
	main_param = Column(String(16000000), nullable=True)
	params = Column(PickleType(), nullable=True)
	allowed = Column(Integer)
	results = Column(PickleType(), nullable=True)


class Users(Base):
	__tablename__ = 'users'

	uid = Column(Integer, primary_key=True)
	secret_key = Column(String(96))
	active = Column(Integer)
	full_name = Column(String(length=255), nullable=True)
	email = Column(String(length=255), nullable=True)
	creation_time = Column(Integer)
	key_atime = Column(Integer, nullable=True, default=1)
	login_name = Column(String(96), nullable=True)
	login_passwd_md5 = Column(String(96), nullable=True)
	login_atime = Column(Integer, nullable=True, default=1)
	login_mtime = Column(Integer, nullable=True, default=1)


class Capabilities(Base):
	__tablename__ = 'capabilities'

	uid = Column(ForeignKey("users.uid")) #models.ForeignKey(Users, db_column='uid', on_delete=models.CASCADE)
	cap_md5 = Column(String(96))
	cap_repr = Column(String(16000000))


class Meta(Base):
	__tablename__ = 'meta'

	name = Column(String(32), nullable=True)
	value = Column(String(255), nullable=True) #models.CharField(blank=True, max_length=255)


class Rulesets(Base):
	__tablename__ = 'rulesets'

	uri = Column(String(16000000), nullable=True)
	xml = Column(String(16000000), nullable=True)


class StatsCatalog(Base):
	__tablename__ = 'stats_catalog'

	parent_path = Column(ForeignKey("stats_catalog.target_path")) # models.ForeignKey( 'self', null=True, blank=True, db_column='parent_path', on_delete=models.CASCADE)
	target_path = Column(String(255), primary_key=True) # models.CharField( primary_key=True, max_length=255, db_column='target_path')


class StatsCounters(Base):
	__tablename__ = 'stats_counters'

	target = Column(ForeignKey("stats_catalog.target_path"), primary_key=True) #models.OneToOneField( StatsCatalog, db_column='target_path', primary_key=True, on_delete=models.CASCADE)
	name = Column(String(96))
	event_count = Column(Integer)
	first_time = Column(Integer, nullable=True, default=1) #models.IntegerField(null=True, blank=True)
	last_time = Column(Integer, nullable=True, default=1) # models.IntegerField(null=True, blank=True)


class StatsMetadata(Base):
	__tablename__ = 'stats_metadata'

	target = Column(ForeignKey()) # models.OneToOneField(StatsCatalog, db_column='target_path', primary_key=True, related_name='metadata', on_delete=models.CASCADE)
	name = Column(String(96))
	mime_type = Column(String(96))
	value = Column(String(16000000), nullable=True)
	mtime = models.IntegerField(null=True, blank=True)


class StatsRelations(Base):
	__tablename__ = 'stats_relations'

	target_a_path = models.ForeignKey(StatsCatalog, related_name="%(class)s_target_a_path", on_delete=models.CASCADE)
	target_b_path = models.ForeignKey(StatsCatalog, on_delete=models.CASCADE)
	strength = Column(Integer)
	freshness = models.IntegerField(null=True, blank=True)

class StatsSubscriptions(Base):
	__tablename__ = 'stats_subscriptions'

	id = models.IntegerField(primary_key=True)
	target_path = models.CharField(max_length=255)
	expiration = Column(Integer)
	scope = Column(String(96), nullable=True)
	client = models.CharField(blank=True, max_length=192)
	trigger = models.TextField()
	failures = Column(Integer)
