from django import forms as forms
from django.conf import settings
from django.contrib import auth
from django.contrib.auth.models import User
from django.http import HttpResponseRedirect
from django.shortcuts import render
from django.template import loader
from django.template.context import Context
from cia.apps.mailutil import send_mail_to_user
from cia.apps.token import TokenClass
from django.contrib import messages
import datetime, urllib.request, urllib.parse, urllib.error


###########################
#       User Login        #
###########################

def login_url(next_page):
    if next_page:
        return settings.LOGIN_URL + "?next_page=" + urllib.parse.quote(next_page)
    else:
        return settings.LOGIN_URL

# I don't quite want to get rid of this yet because of it's impersonate feature.
# I would like to find a way to integrate that better into django's core. - Justasic
def old_login_required(view_func):
    """Simplified version of auth.decorators.login_required,
       which works with our LOGIN_URL and removes the 'next'
       parameter which we don't need yet.
       """
    def _checklogin(request, *args, **kwargs):
        if request.user.is_authenticated and request.user.is_active:

            # This is a big hammer for superusers: if you're logged
            # in as a superuser, you can add ?impersonate=username
            # to nearly any URL to morph your current session
            # into a session for that user. This is useful as an
            # abuse management tool.
            if request.user.is_superuser:
                impersonate = request.GET.get('impersonate')
                if impersonate:
                    user = User.objects.get(username=impersonate)
                    user.backend = "django.contrib.auth.backends.ModelBackend"
                    auth.login(request, user)

            return view_func(request, *args, **kwargs)
        else:
            return HttpResponseRedirect(login_url(request.path))
    _checklogin.__doc__ = view_func.__doc__
    _checklogin.__dict__ = view_func.__dict__
    return _checklogin


def internal_login(request, username, password):
    try:
        user = auth.authenticate(username = username.encode(),
                                 password = password)
    except UnicodeDecodeError:
        user = None
    if user is None:
        return "Incorrect username or password."
    elif not user.is_active:
        return "This account is inactive."
    elif not request.session.test_cookie_worked():
        return "Cookies must be enabled."

    auth.login(request, user)
    request.session.delete_test_cookie()
    user.last_login = datetime.datetime.now()
    user.save()

def get_user(username):
    return auth.models.User.objects.get(username=username)


###########################
#     Lost Password       #
###########################

PasswordToken = TokenClass("account_recovery_session", expire_hours=12)

def lost(request, next_page, recovery_page):
    error = None
    if request.POST:
        user = get_user(request.POST.get('username'))
        if not user:
            error = "Incorrect username."
        if not error:
            send_mail_to_user(user, "accounts/recovery_mail.txt",
                              request = request,
                              recovery_path = recovery_page % PasswordToken.new({'username': user.username}))
            return render(request, 'accounts/recovery_mail_sent.html')
    return render(request, 'accounts/recovery_form.html', {'error': error})

class ResetPasswordForm(forms.Form):
    password = forms.CharField(min_length=5, max_length=30)
    password2 = forms.CharField()

def reset(request, key, next_page):
    t = PasswordToken.get(key)
    if not t:
        return render(request, 'accounts/recovery_key_error.html')
    user = get_user(t.get('username'))

    if request.POST:
        form = ResetPasswordForm(request.POST)
        form.full_clean()
        validate_password_confirmation(form)
        validate_test_cookie(form, request)

        if not form.errors:
            user.set_password(form.cleaned_data['password'])
            user.save()

            # Try to log in using the new password
            loginError = internal_login(request, user.username, form.cleaned_data['password'])
            if loginError:
                # This might happen if the account is deactivated.
                raise forms.ValidationError(loginError)
                #form.errors['submit'] = forms.util.ErrorList([loginError])
            else:
                # We're in successfully. Expire the recovery session.
                PasswordToken.delete(key)
                return HttpResponseRedirect(next_page)
    else:
        form = None

    request.session.set_test_cookie()
    return render(request, 'accounts/reset_password.html', {
        'username': user.username,
        'form': form,
        })


###########################
#    User Registration    #
###########################

class RegistrationForm(forms.Form):
    first_name = forms.CharField(max_length=30)
    last_name = forms.CharField(max_length=30)
    email = forms.EmailField()
    username = forms.RegexField(r"^[a-zA-Z0-9_\-\.]*$", max_length=30)
    password = forms.CharField(min_length=5, max_length=30)
    password2 = forms.CharField()

def validate_old_password(form, user, field_name='password'):
    if not form.errors.get(field_name) and not user.check_password(form.data.get(field_name)):
        raise forms.ValidationError("Incorrect Password.")
        #form.errors[field_name] = forms.util.ErrorList(["Incorrect password."])

def validate_password_confirmation(form, field_name='password'):
    field2_name = field_name + '2'
    if not form.errors.get(field2_name) and form.data.get(field_name) != form.data.get(field2_name):
        raise forms.ValidationError("Your passwords do not match.")
        #form.errors[field2_name] = forms.util.ErrorList(["Your passwords do not match."])

def validate_test_cookie(form, request):
    if not request.session.test_cookie_worked():
        raise forms.ValidationError("Cookies must be enabled.")
        #form.errors['submit'] = forms.util.ErrorList(["Cookies must be enabled."])

def register(request, next_page, template_name="accounts/register.html"):

    # Ignore if they're already registered, they need to logout first - Justasic
    if request.user.is_authenticated:
        return HttpResponseRedirect(reverse('index'))

    if settings.CIA_REGISTRATION_IS_CLOSED:
        return render(request, "accounts/registration_closed.html")

    form = RegistrationForm(request.POST, initial=request.GET)

    if request.POST and form.is_valid():
        validate_password_confirmation(form)
        validate_test_cookie(form, request)

        if not form.errors:
            # The username still might be taken, but let's test for that atomically
            # as a side-effect of trying to create the user.
            user = auth.models.User(first_name = form.cleaned_data.get('first_name').title(),
                                    last_name = form.cleaned_data.get('last_name').title(),
                                    email = form.cleaned_data.get('email'),
                                    username = form.cleaned_data.get('username'))
            user.set_password(form.cleaned_data.get('password'))
            try:
                user.save()
            except:
                # The actual error we get for a duplicate username is database-specific.
                # To make this portable, we'll do a second check to see if the username
                # was taken. This is also slightly racy, but much less so than checking
                # beforehand. Easier to ask forgiveness than permission.
                if get_user(form.cleaned_data.get('username')):
                    raise forms.ValidationError("Sorry, this username is taken.")
                    #form.errors['username'] = forms.util.ErrorList(["Sorry, this username is taken."])
                else:
                    # Something else happened.. pass on the exception
                    raise

        if not form.errors:
            # Something's wrong internally if we can't log in to the
            # account we just created...
            assert not internal_login(request, form.cleaned_data.get('username'), form.cleaned_data.get('password'))
            return HttpResponseRedirect(next_page)

    else:
        form = RegistrationForm(initial=request.GET)

    request.session.set_test_cookie()
    return render(request, template_name, {'form': form})


###########################
#     Profile Editing     #
###########################

class ChangePasswordForm(forms.Form):
    old_password = forms.CharField()
    new_password = forms.CharField(min_length=5, max_length=30)
    new_password2 = forms.CharField()

def do_change_password(request):
    form = ChangePasswordForm(request.POST)
    form.full_clean()
    validate_password_confirmation(form, 'new_password')
    validate_old_password(form, request.user, 'old_password')
    if not form.errors:
        request.user.set_password(form.cleaned_data['new_password'])
        request.user.save()
        messages.add_message(request, messages.INFO, "Your password was changed successfully.")
    return form

class ChangeProfileForm(forms.ModelForm):
    class Meta:
        model = User
        fields = ['first_name', 'last_name', 'email']

def do_change_profile(request):
    form = ChangeProfileForm(request.POST, instance=request.user)
    if form.is_valid():
        request.user.save()
        messages.add_message(request, messages.INFO, "Your profile was updated successfully.")
    return form
