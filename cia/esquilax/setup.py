#!/usr/bin/env python3
from distutils.core import setup, Extension
#from distutils.extension import Extension
from distutils.sysconfig import get_python_version
import subprocess

def pkgconfig(*packages, **kw):
    flag_map = {'-I': 'include_dirs', '-L': 'library_dirs', '-l': 'libraries'}
    for token in subprocess.getoutput("pkg-config --libs --cflags %s" % ' '.join(packages)).split():
        kw.setdefault(flag_map.get(token[:2]), []).append(token[2:])
    return kw
setup(
    name = "fidtool",
    ext_modules=[ 
        Extension("_fidtool", sources=["_fidtool.c"], libraries=['png'],  extra_compile_args=["-Werror=implicit-function-declaration"]),
        Extension("_bsax", sources=["_bsax.c"], **pkgconfig('glib-2.0'), extra_compile_args=["-Werror=implicit-function-declaration"]),
    ],
)
