#
# Models for compatibility with the LibCIA database.
# These are only used by conversion scripts for data migration.
#

from django.db import models

class AuditTrail(models.Model):
    id = models.IntegerField(primary_key=True)
    timestamp = models.IntegerField()
    uid = models.IntegerField()
    action_domain = models.CharField(max_length=96)
    action_name = models.TextField()
    main_param = models.TextField(blank=True)
    params = models.TextField(blank=True)
    allowed = models.IntegerField()
    results = models.TextField(blank=True)
    class Meta:
        managed = False
        db_table = 'audit_trail'

class Users(models.Model):
    uid = models.IntegerField(primary_key=True)
    secret_key = models.CharField(max_length=96)
    active = models.IntegerField()
    full_name = models.TextField(blank=True)
    email = models.TextField(blank=True)
    creation_time = models.IntegerField()
    key_atime = models.IntegerField(null=True, blank=True)
    login_name = models.CharField(blank=True, max_length=96)
    login_passwd_md5 = models.CharField(blank=True, max_length=96)
    login_atime = models.IntegerField(null=True, blank=True)
    login_mtime = models.IntegerField(null=True, blank=True)
    class Meta:
        managed = False
        db_table = 'users'

class Capabilities(models.Model):
    uid = models.ForeignKey(Users, db_column='uid', on_delete=models.CASCADE)
    cap_md5 = models.CharField(max_length=96)
    cap_repr = models.TextField()
    class Meta:
        managed = False
        db_table = 'capabilities'

class Meta(models.Model):
    name = models.CharField(blank=True, max_length=96)
    value = models.CharField(blank=True, max_length=255)
    class Meta:
        managed = False
        db_table = 'meta'

class Rulesets(models.Model):
    uri = models.TextField()
    xml = models.TextField()
    class Meta:
        managed = False
        db_table = 'rulesets'

class StatsCatalog(models.Model):
    parent = models.ForeignKey('self', null=True, blank=True, db_column='parent_path', on_delete=models.CASCADE)
    path = models.CharField(primary_key=True, max_length=255, db_column='target_path')
    class Meta:
        managed = False
        db_table = 'stats_catalog'

class StatsCounters(models.Model):
    target = models.OneToOneField(StatsCatalog, db_column='target_path', primary_key=True, on_delete=models.CASCADE)
    name = models.CharField(max_length=96)
    event_count = models.IntegerField()
    first_time = models.IntegerField(null=True, blank=True)
    last_time = models.IntegerField(null=True, blank=True)
    class Meta:
        managed = False
        db_table = 'stats_counters'

class StatsMetadata(models.Model):
    target = models.OneToOneField(StatsCatalog, db_column='target_path',
                               primary_key=True, related_name='metadata', on_delete=models.CASCADE)
    name = models.CharField(max_length=96)
    mime_type = models.CharField(max_length=96)
    value = models.TextField()
    mtime = models.IntegerField(null=True, blank=True)
    class Meta:
        managed = False
        db_table = 'stats_metadata'

class StatsRelations(models.Model):
    target_a_path = models.ForeignKey(StatsCatalog, related_name="%(class)s_target_a_path", on_delete=models.CASCADE)
    target_b_path = models.ForeignKey(StatsCatalog, on_delete=models.CASCADE)
    strength = models.IntegerField()
    freshness = models.IntegerField(null=True, blank=True)
    class Meta:
        managed = False
        db_table = 'stats_relations'

class StatsSubscriptions(models.Model):
    id = models.IntegerField(primary_key=True)
    target_path = models.CharField(max_length=255)
    expiration = models.IntegerField()
    scope = models.CharField(blank=True, max_length=96)
    client = models.CharField(blank=True, max_length=192)
    trigger = models.TextField()
    failures = models.IntegerField()
    class Meta:
        managed = False
        db_table = 'stats_subscriptions'

