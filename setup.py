#!
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#from distutils.core import setup
from setuptools import setup , find_packages

setup(
    name="PyDatcomLab",
    version="0.1",
    description="PyDatcomLab  is the GUI Clients for Datcom program .",
    long_description="""PyDatcomLab  is the GUI Clients for Datcom program .
""",
    author="linger",
    author_email="lingo_renlo@qq.com",
    maintainer="BostenAI",
    maintainer_email="BostenAI",
    url="https://github.com/darkspring2015/PyDatcomLab",
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Development Status :: 4 - Beta",
        "Environment :: Win32 (MS Windows)",
        "Environment :: X11 Applications :: Qt",
        "Intended Audience :: Education",
        "Natural Language :: Chinese (Simplified)",
        "Operating System :: Microsoft :: Windows :: Windows 7",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python",
        "Programming Language :: YACC",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        "Topic :: Scientific/Engineering :: Astronomy",
        "Topic :: Scientific/Engineering :: Atmospheric Science"
    ],
    packages= find_packages(),
#    [
#        "PyDatcomLab",
#        "PyDatcomLab.Core",
#        "PyDatcomLab.GUIs",
#        "PyDatcomLab.GUIs.components",
#        "PyDatcomLab.GUIs.InputCard",
#        "PyDatcomLab.GUIs.PlaneConfiguration",
#        "PyDatcomLab.GUIs.tools.HelperSystem",
#        "PyDatcomLab.GUIs.tools.XMLEditer",
#        #"PyDatcomLab.bin",
#        #"PyDatcomLab.config",
#        #"PyDatcomLab.docs",
#        #"PyDatcomLab.scripts",
#        #"PyDatcomLab.extras",
#        #"PyDatcomLab.tests",
#    ],

#    # 需要安装的依赖
    install_requires=[
        'markdown>=2.6.11',
        'setuptools>=16.0',
        'pyqt5>=5.6.0',
        'python-markdown-math'
    ],
    #包含所有的包资源
    include_package_data=True,
#    package_data={
#    #所有的xml文件
#
#    '':['*.xml', '*.md', '*.png', '*.jpg', '*.ico', '*.dcxml'
#    , 'bin/*.*',
##    'bin':['*.*'],
##    'config':['*.*'],
#    'docs/PyDatcomLab-Help/*.*',
##    'extras':['*.*'],
##    '3rdparty':['*.*'],
#],
#    },
    #排除性资源
#    exclude_package_data={'':['.gitignore'],
#    #'3rdparty':['.git/*.*'],
#    },
    # 添加这个选项，在windows下Python目录的scripts下生成exe文件
    # 注意：模块与函数之间是冒号:
    entry_points={'console_scripts': [
         'pydatcomlab = PyDatcomLab.PyDatcomLab:main',
     ]},

    install_scripts =['scripts/zip3rdParty.py']
)
