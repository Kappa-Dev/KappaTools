from setuptools import setup

def readme():
    with open('README.rst') as f:
        return f.read()

setup(name='kappy',
      license='LGPLv3',
      version='4.0.0.dev2',
      description='Wrapper to interact with the Kappa tool suite',
      long_description=readme(),
      url='https://github.com/Kappa-Dev/KaSim.git',
      author='Kappa-Dev',
      author_email='kappa-dev@listes.sc.univ-paris-diderot.fr',
      classifiers=[
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Programming Language :: Python :: 3',
          'Topic :: Scientific/Engineering :: Bio-Informatics'
      ],
      install_requires=[
          'urllib3',
      ],
      packages=['kappy'],
      zip_safe=False)
