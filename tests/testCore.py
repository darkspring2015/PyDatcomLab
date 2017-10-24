#!/usr/bin/env python

import unittest
import os
from PyDatcomLab.Core import DatcomParser, DatcomExporter, DatcomPlotter


class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse(self):
        parser = DatcomParser(os.path.join('PyDatcomCore','test', 'data', 'Citation.out'))
        self.assertTrue(parser == None)

    def test_export(self):
        parser = DatcomParser(os.path.join('PyDatcomCore','test', 'data', 'Citation.out'))
        moPath = os.path.abspath(os.path.join('PyDatcomCore','pydatcom', 'templates', 'modelica.mo'))
        exporter = DatcomExporter(parser.get_common(), moPath)
        result = exporter.get_export()
        self.assertTrue(result != None)

    def test_plot(self):
        parser = DatcomParser(os.path.join('PyDatcomCore','test', 'data', 'Citation.out'))
        plotter = DatcomPlotter(parser.get_common())
        plotter.common_plots()
        self.assertTrue(plotter != None)
        
if __name__ == '__main__':
    #加入直接运行无法工作，请直接在命令行中制定当前工作目录
    unittest.main()
