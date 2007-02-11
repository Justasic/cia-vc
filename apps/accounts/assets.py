from cia.apps.accounts import models, authplus, formtools
from cia.apps.api.util import json_result
from cia.apps.images.widgets import ImageWidget
from cia.apps.stats.models import StatsTarget
from django.core.paginator import ObjectPaginator
from django.core.mail import mail_managers, send_mail
from django.http import Http404, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.template.context import RequestContext, Context
from django.contrib.contenttypes.models import ContentType
import django.newforms as forms
from django.template import loader


###########################
#    Asset Navigation     #
###########################

def get_default_asset_id(request, asset_type):
    """Return the default asset ID for a particular type. This will
       try, in order:

          1. The default recorded in the current session
          2. The first object returned by all_for_user
          3. 'add', to direct users to an asset creation page.
       """
    model = get_asset_by_type(asset_type)
    all = model.objects.all_for_user(request.user)

    default_id = request.session.get('default_' + asset_type)
    if default_id:
        try:
            all.get(pk=default_id)
            return default_id
        except models.UserAsset.DoesNotExist:
            pass

    try:
        return all[0].id
    except IndexError:
        pass

    return 'add'

def get_user_asset_types(request, current=None):
    """Return a list which summarizes a user's assets, for navigation."""
    return [{
        'asset_type': model._meta.asset_type,
        'is_current': model._meta.asset_type == current,
        'verbose_name_plural': model._meta.verbose_name_plural,
        'count': model.objects.all_for_user(request.user).count,
        'default_id': get_default_asset_id(request, model._meta.asset_type),
        }
        for model in models.AssetManager.models]

def get_asset_by_type(asset_type):
    for model in models.AssetManager.models:
        if model._meta.asset_type == asset_type:
            return model
    raise Http404

def get_asset_add_context(request, asset_type):
    """Returns extra context items used when adding any asset. This
       raises a 404 error if the asset_type is not valid. This
       includes all context variables required for navigation.

       Note that this doesn't return a RequestContext directly, and
       for a good reason. Context processors invoked by RequestContext
       may have side-effects, like deleting a user's pending
       messages. If these side-effects are wanted at all, they should
       occur only when the view is ready for them.
       """
    model = get_asset_by_type(asset_type)
    return {
        'asset_types': get_user_asset_types(request, asset_type),
        'asset_type': asset_type,
        'asset_type_name': model._meta.verbose_name,
        'user_assets': model.objects.all_for_user(request.user),
        'ACCESS': models.ACCESS,
        'add': True,
        'form_path': request.path,
        }

def get_asset_edit_context(request, asset_type, asset_id):
    """Returns a context for editing any asset. This raises a 404
       error if the asset_type is not valid or if the asset_id does
       not refer to a UserAsset for the current user. The result
       includes all context variables required for navigation.
       """
    model = get_asset_by_type(asset_type)
    asset_id = int(asset_id)
    try:
        user_asset = model.objects.all_for_user(request.user).get(pk=asset_id)
    except models.UserAsset.DoesNotExist:
        raise Http404

    # Set as the default asset for this type
    request.session['default_' + asset_type] = asset_id

    return {
        'asset_types': get_user_asset_types(request, asset_type),
        'asset_type': asset_type,
        'asset_type_name': model._meta.verbose_name,
        'user_assets': model.objects.all_for_user(request.user),
        'ACCESS': models.ACCESS,
        'asset_id': asset_id,
        'user_asset': user_asset,
        'other_community_user_assets': user_asset.asset.assets.exclude(user=request.user),
        'form_path': request.path,
        }


###########################
#     Profile Editing     #
###########################

@authplus.login_required
def profile(request):
    if 'change-password' in request.POST:
        password_form = authplus.do_change_password(request)
    else:
        password_form = None

    if 'change-profile' in request.POST:
        profile_form = authplus.do_change_profile(request)
    else:
        profile_form = authplus.ChangeProfileForm(request.user)

    return render_to_response('accounts/profile.html', RequestContext(request, {
        'profile': True,
        'asset_types': get_user_asset_types(request),
        'password_form': password_form,
        'profile_form': profile_form,
        }))


###########################
#      Asset Editing      #
###########################

class EditAssetForm(forms.Form):
    access = forms.ChoiceField(
        choices = models.access_choices,
        widget = forms.RadioSelect,
        )

    def clean_access(self):
        return int(self.clean_data['access'])

    def should_delete(self):
        """Are we removing access to this asset?"""
        return self.clean_data['access'] == models.ACCESS.NONE

    def delete(self, request, user_asset):
        """Delete this UserAsset, create a message indicating that we
           were successful, then redirect back to the 'add' page.
           """
        user_asset.delete()
        request.user.message_set.create(message="Removed access to %s" % user_asset.asset)
        return HttpResponseRedirect("/account/%s/add/" % 
                                    user_asset.asset._meta.asset_type)

    def apply_meta_changes(self, request, user_asset):
        """Apply changes to a UserAsset, returning a 'meta' dictionary
           with special asset changeset items describing those changes.
           """
        meta = []
        new_access = self.clean_data['access']
        if new_access != user_asset.access:

            if new_access == models.ACCESS.NONE:
                # We'll actually do the deletion later

                meta.append('_lost_access')

            elif new_access == models.ACCESS.COMMUNITY:
                # Decreasing the access level

                request.user.message_set.create(message="You now have community-level access to %s" % user_asset.asset)
                assert user_asset.access > new_access
                meta.append('_community_access')
                user_asset.access = new_access
                user_asset.save()

            elif new_access == models.ACCESS.EXCLUSIVE:
                # Increasting the access level. Gaining exclusive
                # access also means revoking access from any other
                # (community-access) users!
                
                request.user.message_set.create(message="You have taken exclusive access to %s" % user_asset.asset)
                meta.append('_exclusive_access')
                user_asset.access = new_access
                user_asset.save()
                user_asset.asset.assets.exclude(user=request.user).delete()

        return meta


@authplus.login_required
@json_result
def changes(request, asset_type, asset_id, page_number, num_per_page=10):
    """Return a paginated list of changes to a particular asset. This
       is called by some AJAX code in order to populate an asset's
       Change History box.
       """
    # Don't bother checking whether the user owns this asset, change
    # history should be public information anyway.
    changes = models.AssetChangeset.objects.filter(
        content_type = ContentType.objects.get_for_model(get_asset_by_type(asset_type)),
        object_id = int(asset_id),
        ).order_by('-id')

    paginator = ObjectPaginator(changes,
                                num_per_page = num_per_page,
                                orphans = num_per_page / 2)
    return {
        'remaining': paginator.hits - paginator.last_on_page(page_number),

        'html': loader.render_to_string(
            'accounts/asset_changes.html',
            RequestContext(request, {
                'changesets': paginator.get_page(page_number),
            })),
        }


def send_conflict_message(request, user_asset, message):
    """Send an e-mail message for conflict resolution purposes.
       The disputed user_asset must be provided, as well as a message
       to be sent to that user_asset's owner. A customized moderator
       message will be sent to all managers.
       """
    ctx = Context(locals())
    
    subject, message = authplus.render_to_email("accounts/conflict_mail_managers.txt", ctx)
    mail_managers(subject, message)

    subject, message = authplus.render_to_email("accounts/conflict_mail_user.txt", ctx)
    send_mail(subject, message, authplus.get_email_for_user(request.user),
              [authplus.get_email_for_user(user_asset.user)])

class ConflictForm(forms.Form):
    message = forms.CharField(widget=forms.Textarea)

@authplus.login_required
def conflict(request, asset_type, asset_id):
    """Asset conflict resolution. We redirect to this page when an
       'add asset' page encounters an exclusive access restriction.
       This informs the user of the situation, and gives them an
       opportunity to complain.
       """
    disable_submit = False
    model = get_asset_by_type(asset_type)
    ctx = get_asset_add_context(request, asset_type)
    try:
        asset = model.objects.get(pk=int(asset_id))
    except model.DoesNotExist:
        raise Http404

    # Find the exclusive owner of this asset. If there is none, this
    # page is not valid: return a 404.
    try:
        owner_ua = asset.assets.get(access__gte = models.ACCESS.EXCLUSIVE)
    except models.UserAsset.DoesNotExist:
        raise Http404
    assert owner_ua.asset == asset

    form = ConflictForm(request.POST)
    if request.POST:
        form.full_clean()
        if form.is_valid():
            send_conflict_message(request, owner_ua, form.clean_data['message'])
            request.user.message_set.create(message="Message sent.")

            # Make it less convenient to send a bunch of rapid-fire messages
            disable_submit = True
    
    ctx.update({
        'disable_submit': disable_submit,
        'form': form,
        'asset': asset,
        'owner_ua': owner_ua,
        })
    return render_to_response(('accounts/%s_conflict.html' % model._meta.object_name.lower(),
                               'accounts/asset_conflict.html'), RequestContext(request, ctx))


###########################
#      Stats Assets       #
###########################

class StatsMetadataForm(forms.Form):
    title = forms.CharField(
        widget = forms.TextInput(attrs = {'class': 'text'}),
        )
    subtitle = forms.CharField(
        required = False,
        widget = forms.TextInput(attrs = {'class': 'text'}),
        )
    url = forms.URLField(
        required = False,
        widget = forms.TextInput(attrs = {'class': 'text'}),
        # Disabled for now, since it seems to imply required=True.
        # verify_exists = True,
        )
    description = forms.CharField(
        required = False,
        widget = forms.Textarea,
        )
    photo_id = forms.IntegerField(
        required = False,
        widget = ImageWidget,
        )
    icon_id = forms.IntegerField(
        required = False,
        widget = ImageWidget,
        )

    def clean_subtitle(self):
        return self.clean_data.get('subtitle') or None

    def clean_url(self):
        return self.clean_data.get('url') or None

    def clean_description(self):
        return self.clean_data.get('description') or None

    def clean_photo_id(self):
        return self.clean_data.get('photo_id') or None

    def clean_icon_id(self):
        return self.clean_data.get('icon_id') or None

@authplus.login_required
def stats_asset(request, asset_type, asset_id):
    """Generic form for editing stats-based assets
       XXX: This whole function need refactoring and cleanup.
       """
    ctx = get_asset_edit_context(request, asset_type, asset_id)
    user_asset = ctx['user_asset']
    asset = user_asset.asset

    form = formtools.MultiForm(request.POST)
    form.validate(EditAssetForm, user_asset)

    # Don't allow blank titles- replace them with the
    # default title (based on our stats path) before
    # displaying the title for edit.
    if not asset.target.title:
        asset.target.title = asset.target.get_default_title()

    form.validate(StatsMetadataForm, asset.target)

    if request.POST and form.is_valid():
        models.AssetChangeset.objects.apply_changes(
            request = request,
            asset = asset,
            changes = form.StatsMetadataForm.clean_data,
            meta = form.EditAssetForm.apply_meta_changes(request, user_asset),
            fieldmap = {
                'title': 'target.title',
                'subtitle': 'target.subtitle',
                'url': 'target.url',
                'description': 'target.description',
                'photo_id': 'target.photo_id',
                'icon_id': 'target.icon_id',
                },
            )

        if asset.target.photo:
            asset.target.photo.reference()
        if asset.target.icon:
            asset.target.icon.reference()
        
        if form.EditAssetForm.should_delete():
            return form.EditAssetForm.delete(request, user_asset)

    ctx.update({
        'form': form,
        'levels': formtools.RadioChoices(form['access'], models.ACCESS),
        })

    return render_to_response('accounts/stats_asset_edit.html', RequestContext(request, ctx))


class AddStatsAssetForm(forms.Form):
    name = forms.CharField(
        widget = forms.TextInput(attrs = {'class': 'text'}),
        )

@authplus.login_required
def add_stats_asset(request, asset_type, prefix, template):
    """Generic form for adding stats-based assets"""
    model = get_asset_by_type(asset_type)
    ctx = get_asset_add_context(request, asset_type)
    form = formtools.MultiForm(request.POST)
    form.validate(AddStatsAssetForm)

    if request.POST:
        if form.is_valid():
            meta = []

            # Get/create the stats target
            target = StatsTarget.objects.get_or_create(path = prefix + form.clean_data['name'])[0]

            # Now get/create the matching asset
            asset, created_asset = model.objects.get_or_create(target = target)
            if created_asset:
                meta.append('_created')

            # Finally, create a new UserAsset.
            user_asset = models.UserAsset.objects.get_or_create_if_allowed(request.user, asset, meta)

            # Record these changes, if any
            models.AssetChangeset.objects.store_changes(
                request = request,
                asset = asset,
                meta = meta,
                )

            # Redirect either to the new UserAsset or to a conflict resolution page
            if user_asset is None:
                return HttpResponseRedirect("/account/conflict/%s/%s/" % (asset_type, asset.id))
            else:
                return HttpResponseRedirect("/account/%s/%s/" % (asset_type, user_asset.id))
    else:
        # Don't show errors if the form hasn't been submitted once
        form.errors = None

    ctx.update({
        'form': form,
        })
    return render_to_response(template, RequestContext(request, ctx))
