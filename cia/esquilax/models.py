from django.db import models


class Atom(models.Model):
    name = models.CharField(maxlength=128, db_index=True, unique=True)

class Message(models.Model):
    file_id = models.PositiveIntegerField()
    offset = models.PositiveIntegerField()

    project_index = models.ForeignKey(Atom, related_name='project_messages', on_delete=models.CASCADE)
    module_index = models.ForeignKey(Atom, related_name='module_messages', on_delete=models.CASCADE)
    author_index = models.ForeignKey(Atom, related_name='author_messages', on_delete=models.CASCADE)

esquilax_indexes = {
    'project_index': '/message/source/project',
    'module_index': '/message/source/module',
    'author_index': '/message/body/commit/author',
    }

# Create your models here.
