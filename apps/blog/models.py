from django.db import models
from django.contrib.auth.models import User
from django.core.cache import cache
from docutils.core import publish_doctree, Publisher
from docutils.readers import doctree
from docutils.io import DocTreeInput, StringOutput
from docutils import nodes
from cia.apps.images.models import ImageSource, ImageInstance
import re


class ImageTranslator(nodes.SparseNodeVisitor):
    """This Docutils translator implements support for a new type of
       image element with a numeric URL, referring to an image managed
       by our cia.apps.images app. Examples:

       A full-size image:

          .. image:: #123

       The default thumbnail:

          .. image:: #123/

       A 64-pixel thumbnail:

          .. image:: #123/64


       A 256-pixel thumbnail, floating in a box:

          .. image:: #123/256
             :class: float

       We include in-line styles for such images, so that we can still
       float them when we're rendered without doc.css (for example,
       via RSS).
       """

    styles = {
        'float': ('div', 'float: right; margin: 0 0 1em 1em; clear: right;'),
        'centered': ('div', 'text-align: center;'),
        'dashed': ('img', 'border: 1px dashed #888; padding: 2px;'),
        'framed': ('img', 'border: 1px solid black; padding: 0;'),
        }

    def __init__(self, document):
        nodes.SparseNodeVisitor.__init__(self, document)
        self.images = []

    def visit_image(self, node):
        m = re.match(r"^#(\d+)(/(\d*))?$", node['uri'])
        if not m:
            return
        image_id, use_thumbnail, thumbnail_size = m.groups()

        try:
            image = ImageSource.objects.get(pk=image_id)
        except ImageSource.DoesNotExist:
            err = self.document.reporter.error("Image ID %s does not exist" % image_id)
            node.replace_self(err)
            return

        try:
            if use_thumbnail:
                # Use a thumbnail, and link to the full image
                instance = image.get_thumbnail(int(thumbnail_size or 128))
                href = image.get_original().get_url()
            else:
                # No link, put the whole thing in directly
                instance = image.get_original()
                href = None
        except ImageInstance.DoesNotExist:
            err = self.document.reporter.error("Image not available in the specified size")
            node.replace_self(err)
            return

        # Apply all of the image's styles. Some styles go on the
        # image itself, some go on a containing div.

        styles = {}
        for c in node.get('classes'):
            if c in self.styles:
                element, style = self.styles[c]
                styles[element] = styles.get(element, '') + style

        # If we got this far, there are no more potential user errors.
        # Store this image in our list of images to reference.

        self.images.append(image)

        # Build some raw HTML output, which will replace this image in the doc tree

        parts = []

        if 'div' in styles:
            parts.append('<div style="%s">' % styles['div'])
        if href:
            parts.append('<a href="%s">' % href)

        parts.append('<img src="%s" width="%d" height="%d" style="%s" />' % (
            instance.get_url(),
            instance.width, instance.height,
            styles.get('img', ''),
            ))

        if href:
            parts.append('</a>')
        if 'div' in styles:
            parts.append('</div>')

        node.parent.replace(node, nodes.raw(text=''.join(parts), format='html'))


class Post(models.Model):
    slug = models.SlugField('slug', unique_for_date='pub_date', db_index=True)
    pub_date = models.DateTimeField(db_index=True)
    posted_by = models.ForeignKey(User)
    listed = models.BooleanField('Listed in public indexes?', default=False)
    title = models.CharField(max_length=100)
    content = models.TextField()

    def __unicode__(self):
        return '"%s" posted by %s at %s' % (self.title, self.posted_by, self.pub_date)

    def get_absolute_url(self):
        """ New posts we don't need to worry about getting the url, just return empty string """
        if self.pub_date is None:
            return ""
        else:
            return '/blog/%04d/%02d/%s/' % (self.pub_date.year, self.pub_date.month, self.slug)

    def invalidate_cache(self):
        cache.delete('cia.apps.blog.%d' % self.id)

    def getComments(self):
        return Comment.objects.filter(post=self, is_public=True)

    def getCommentCount(self):
        return Comment.objects.filter(post=self, is_public=True).count()

    def reference_images(self):
        """Find all images used by this blog post, and reference them
           permanently. This prevents the images from being treated
           as temporary, and automatically discarded.
           """
        document = publish_doctree(source = self.content)
        visitor = ImageTranslator(document)
        document.walkabout(visitor)
        for image in visitor.images:
            image.reference()

    def render(self):
        key = 'cia.apps.blog.%d' % self.id
        parts = cache.get(key)
        if not parts:

            # Convert the reST markup to a document tree
            document = publish_doctree(source = self.content)

            visitor = ImageTranslator(document)
            document.walkabout(visitor)

            #
            # Publish that document tree as HTML.  We can't use any of
            # the simpler methods in docutils.core, since we need
            # access to writer.parts
            #
            reader = doctree.Reader(parser_name='null')
            pub = Publisher(reader, None, None,
                            source = DocTreeInput(document),
                            destination_class = StringOutput)
            pub.set_writer('html4css1')
            pub.process_programmatic_settings(None, {
                'cloak_email_addresses': True,
                'initial_header_level': 2,
                }, None)

            pub.publish()
            parts = pub.writer.parts

            cache.set(key, parts)
        return parts

    class Admin:
        pass

class Comment(models.Model):
    submit_date = models.DateTimeField(auto_now_add=True)
    person_name = models.CharField(max_length=60)
    comment = models.TextField()
    post = models.ForeignKey(Post)
    is_public = models.BooleanField()

    def __unicode__(self):
        return unicode("%s: %s" % (self.post, self.comment[:60]))

    def get_text(self):
        return self.comment

    class Admin:
        pass
