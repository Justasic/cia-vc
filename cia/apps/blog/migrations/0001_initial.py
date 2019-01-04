# Generated by Django 2.1.3 on 2019-01-01 19:48

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='Comment',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('submit_date', models.DateTimeField(auto_now_add=True)),
                ('person_name', models.CharField(max_length=60)),
                ('comment', models.TextField()),
                ('is_public', models.BooleanField()),
            ],
            options={
                'db_table': 'comment_comment',
            },
        ),
        migrations.CreateModel(
            name='Post',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('slug', models.SlugField(unique_for_date='pub_date', verbose_name='slug')),
                ('pub_date', models.DateTimeField(db_index=True)),
                ('listed', models.BooleanField(default=False, verbose_name='Listed in public indexes?')),
                ('title', models.CharField(max_length=100)),
                ('content', models.TextField()),
                ('posted_by', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
        ),
        migrations.AddField(
            model_name='comment',
            name='post',
            field=models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='blog.Post'),
        ),
    ]
