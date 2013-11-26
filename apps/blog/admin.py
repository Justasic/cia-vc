from django.contrib import admin
from cia.apps.blog.models import Post, Comment

class PostAdmin(admin.ModelAdmin):
        list_display = ['title']
        list_filter = ['listed', 'pub_date']
        search_fields = ['title', 'content']
        date_heirachy = 'pub_date'
        save_on_top = True
        prepopulated_fields = {"slug": ("title",)}

class CommentAdmin(admin.ModelAdmin):
	display_fields = ["post", "person_name", "created"]

admin.site.register(Post, PostAdmin)
admin.site.register(Comment, CommentAdmin)
