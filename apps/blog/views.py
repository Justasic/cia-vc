from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.contrib.syndication.feeds import Feed
from django.http import HttpResponse, Http404, HttpResponseRedirect
from django.template.defaultfilters import slugify
from django.conf import settings
from cia.apps.blog.models import Post, Comment
import django.forms as forms
import datetime

def is_blog_admin(request):
    return request.user.is_authenticated() and request.user.is_staff

def get_recent_comments():
    return Comment.objects.filter(is_public = True)

def get_blog_context(request):
    is_admin = is_blog_admin(request)

    if is_admin:
        posts = Post.objects.all()
    else:
        posts = Post.objects.filter(
            pub_date__lte = datetime.datetime.now(),
            listed = True)

    return RequestContext(request, {
        'posts': posts,
        'archive_dates': posts.dates('pub_date', 'month')[::-1],
        'can_post': is_admin,
        'comment_list': get_recent_comments().order_by('-id'),
        'comment_count': int(len(get_recent_comments())),
        })

def archive(request, num_latest=None, year=None, month=None):
    ctx = get_blog_context(request)

    # Filter by a month/year
    latest = ctx['posts']
    if year and month:
        latest = latest.filter(pub_date__year = int(year),
                               pub_date__month = int(month))
        current_archive_date = datetime.datetime(year = int(year),
                                                 month = int(month),
                                                 day = 1)
    else:
        current_archive_date = None

    latest = latest.order_by('-pub_date')
    if num_latest:
        latest = latest[:num_latest]

    ctx.update({
        'latest': latest,
        'current_archive_date': current_archive_date,
        })
    return render_to_response('blog/archive.html', ctx)


class EditPostForm(forms.Form):
    title = forms.CharField()
    listed = forms.BooleanField(required = False, widget = forms.CheckboxInput(attrs = {'class': 'checkbox'}),)
    content = forms.CharField(widget = forms.Textarea,)


class CommentForm(forms.ModelForm):
	class Meta:
		model = Comment
		exclude = ["post"]

def detail(request, year=None, month=None, slug=None):
    ctx = get_blog_context(request)

    if slug is None:
        # Start a new blank Post. If the user isn't logged in,
        # this link won't even show up on the page- so let's
        # just keep it hidden with a 404.

        if not ctx['can_post']:
            raise Http404
        post = Post(posted_by = request.user)

    else :
        # Look up an existing post. The slug and date must both match.

        try:
            post = Post.objects.get(pub_date__year = int(year),
                                    pub_date__month = int(month),
                                    slug = slug)
        except Post.DoesNotExist:
            raise Http404

    # Show an editing form if this user is allowed to make posts
    # and if they own the current post.

    post_form = None
    if ctx['can_post'] and post.posted_by.id == request.user.id:

        if request.POST:
            model = dict(request.POST.items())
            model.setdefault('listed', False)
            post_form = EditPostForm(model)

            if post_form.is_valid():
                post.content = post_form.cleaned_data['content']
                post.title = post_form.cleaned_data['title']

                # Bump the publication date if we're transitioning from draft to listed.
                if (not post.pub_date) or (post_form.cleaned_data['listed'] and not post.listed):
                    post.pub_date = datetime.datetime.now()
                post.listed = post_form.cleaned_data['listed']

                # Update the slug if the post is unlisted
                if (not post.slug) or (not post.listed):
                    post.slug = slugify(post.title)

                # If the post is listed, mark all of its images as permanent.
                # Images in drafts are temporary!
                if post.listed:
                    post.reference_images()

                post.save()
                post.invalidate_cache()

        else:
            post_form = EditPostForm(initial = post.__dict__)

    if request.user.is_authenticated():
        if request.user.first_name and request.user.last_name:
            user_full_name = u"%s %s" % (request.user.first_name, request.user.last_name)
        else:
            user_full_name = request.user.username
    else:
        user_full_name = "Anonymous"

    ctx.update({
        'latest': ctx['posts'].order_by('-pub_date')[:15],
        'post': post,
        'post_form': post_form,
        'comment_form': CommentForm(initial={'person_name': user_full_name}),
        })
    return render_to_response('blog/detail.html', ctx)


class BlogFeed(Feed):
    title = 'CIA Blog'
    link = '/blog/'
    description = ('Announcements and discussion for CIA, the '
                   'open source change notification system. Find '
                   'out about new and upcoming features here.')

    def items(self):
        return Post.objects.filter(
            pub_date__lte = datetime.datetime.now(),
            listed = True,
            ).order_by('-pub_date')[:20]

    def item_author_name(self, item):
        return item.posted_by.get_full_name()

    def item_pubdate(self, item):
        return item.pub_date

def blog_feed(request):
    f = BlogFeed('blog', request.path).get_feed()
    response = HttpResponse(mimetype = f.mime_type)
    f.write(response, 'utf-8')
    return response


class CommentFeed(Feed):
    title = 'CIA Blog Comments'
    link = '/blog/'

    def items(self):
        return get_recent_comments()

    def item_author_name(self, item):
        return item.person_name

    def item_pubdate(self, item):
        return item.submit_date

    def item_link(self, item):
        obj = item.get_content_object()
        if obj:
            item_url = obj.get_absolute_url()
        else:
            # The object doesn't exist any more, but this will at
            # least give us a unique URL for the comment.
            item_url = '/'
        return "%s#c%d" % (item_url, item.id)


def comment_feed(request):
    f = CommentFeed('comment', request.path).get_feed()
    response = HttpResponse(mimetype = f.mime_type)
    f.write(response, 'utf-8')
    return response

def post_comment(request):
    """Post a comment, and redirect back to the blog entry on success."""

    if not request.POST:
        raise Http404

    p = request.POST
    object_id = p['post_id']
    if p.has_key("comment") and p["comment"]:
        author = "Anonymous"
        if p["person_name"]:
            author = p["person_name"]

        comment = Comment(post=Post.objects.get(pk=object_id))
        cf = CommentForm(p, instance=comment)
        comment = cf.save(commit=False)
        comment.author = author
        comment.is_public = True
        comment.save()

    response = HttpResponseRedirect(Post.objects.get(pk=int(object_id)).get_absolute_url())
    return response

def delete_comment(request, post_pk, pk=None):
    """ Delete a blog comment """

    if not request.user.is_staff:
        raise Http404

    p = request.POST
    if not pk:
        pklst = request.POST.getlist("delete")
    else:
        pklst = [pk]

    post = Post.objects.get(pk=post_pk)
    for pkk in pklst:
        Comment.objects.get(pk=pkk).delete()
    return HttpResponseRedirect(post.get_absolute_url())

