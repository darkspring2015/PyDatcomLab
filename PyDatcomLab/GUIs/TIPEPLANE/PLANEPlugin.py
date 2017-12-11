from PyQt5 import QtWidgets,QtDesigner,QtGui
from PyDatcom.DatcomWidgets.TIPEPLANE import PLANE

class PLANEPlugin(QtDesigner.QPyDesignerCustomWidgetPlugin):

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
        return  PLANE.PLANE(parent)

    def name(self):
        return  "PLANE GUI"

    def group(self):
        return  "Datcom"

    #def icon(self):
    #    return  QtGui.QIcon()

    def toolTip(self):
        return "Datcom PLANE GUI"

    def whatsThis(self):
        return  "Datcom PLANE GUI"

    def isContainer(self):
        return  False

    def domXml(self):
        return (
             '<widget class="PyDatcom.TIPEPLANE.PLANE.PLANE" name="PLANE">\n' \
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
        return  "PLANE"

