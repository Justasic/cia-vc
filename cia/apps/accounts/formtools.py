class MultiForm:
    """A simple wrapper for conditionally validating multiple forms
       with the same data and error dictionaries.
       """
    def __init__(self, POST):
        self.is_valid = lambda: True
        self.POST = POST
        self.data = {}
        self.cleaned_data = {}
        self.errors = {}
        self._bound = {}

    def add(self, form, model=None, post_defaults=None, defaults=None):
        """Instantiate a new form and add it to this MultiForm without validating."""
        inst = form(ModelData(model, self.POST, post_defaults=post_defaults, defaults=defaults))
        setattr(self, form.__name__, inst)

        for name, field in list(inst.fields.items()):
            self._bound[name] = inst[name]

        return inst

    def validate(self, *args, **kw):
        """Add the supplied form class to the list of forms we're
           using, validate that form, and merge its errors/results in.

           The forms are always bound to a ModelData instance. Note
           that the model will be available at 'form.data.model'.

           Individual forms will be made available via attributes
           named after that form's class.
           """
        inst = self.add(*args, **kw)
        inst.full_clean()
        self.is_valid = lambda _=(self.is_valid() and inst.is_valid()): _
        self.errors.update(inst.errors)
        self.data.update(list(inst.data.items()))
        if hasattr(inst, 'cleaned_data'):
            self.cleaned_data.update(list(inst.cleaned_data.items()))

    def is_valid(self):
        return bool(self.cleaned_data)

    def __getitem__(self, name):
        return self._bound[name]


class RadioChoices:
    """This object provides a dictionary-like interface for looking up
       individual choices on a RadioSelect widget. This lets the
       template make decisions about how to lay out a group of radio
       buttons, while letting the form render each individual button.
       """
    def __init__(self, boundField, enum):
        self.renderer = boundField.as_widget(boundField.field.widget, attrs={'class': 'radio'})
        self.enum = enum

    def __getitem__(self, enumName):
        enumValue = getattr(self.enum, str(enumName))
        input = self.renderer[enumValue]
        return '<label class="radio">%s %s</label>' % (input.tag(), input.choice_label)

class ModelData(dict):
    """Wrapper for using an existing model as initial data for a form,
       with new data supplied via HTTP POST. If the model is None,
       we'll always return POST data.
       """
    def __init__(self, model, POST=None, post_defaults=None, defaults=None):
        self.POST = POST
        self.model = model

        if defaults:
            self.update(defaults)
        if model:
            self.update(dict(
                [(f.attname, getattr(model, f.attname))
                 for f in model._meta.fields]))
        if POST:
            if post_defaults:
                self.update(post_defaults)
            self.update(list(POST.items()))
