from PyQt5 import QtWidgets,QtDesigner,QtGui
from PyDatcom.DatcomWidgets.TIPECASE import CASE

class CASEPlugin(QtDesigner.QPyDesignerCustomWidgetPlugin):

    def __init__(self):
        QtDesigner.QPyDesignerCustomWidgetPlugin.__init__(self)
        self.initialized = False

    def initialize(self, core):
        if self.initialized:
            return
        self.initialized = True

    def isInitialized(self):
        return  self.initialized

    def createWidget(self, parent):
        return  CASE.CASE(parent)

    def name(self):
        return  "CASE GUI"

    def group(self):
        return  "Datcom"

    #def icon(self):
    #    return  QtGui.QIcon()

    def toolTip(self):
        return "Datcom CASE GUI"

    def whatsThis(self):
        return  "Datcom CASE GUI"

    def isContainer(self):
        return  False

    def domXml(self):
        return (
             '<widget class="PyDatcom.TIPECASE.CASE.CASE" name="CASE">\n' \
               ' <property name="toolTip">\n' \
               '  <string>Click and drag here</string>\n' \
               ' </property>\n' \
               ' <property name="whatsThis">\n' \
               '  <string>The bubbles widget displays colorful ' \
               'bubbles.</string>\n' \
               ' </property>\n' \
               '</widget>\n'
        )
    def includeFile(self):
        return  "CASE"

