# setup file for building with distutils.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
from distutils.core import setup, Extension

module1 = Extension('spam', sources = ['spammodule.c'])

setup (name = 'PackageName',
        version = '1.0',
        description = 'This is a demo package',
        ext_modules = [module1])
