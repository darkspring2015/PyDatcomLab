# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\ProjectsManager.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_ProjectsMainWindow(object):
    def setupUi(self, ProjectsMainWindow):
        ProjectsMainWindow.setObjectName("ProjectsMainWindow")
        ProjectsMainWindow.resize(300, 600)
        self.centralWidget = QtWidgets.QWidget(ProjectsMainWindow)
        self.centralWidget.setObjectName("centralWidget")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.centralWidget)
        self.verticalLayout.setContentsMargins(0, 0, 0, 0)
        self.verticalLayout.setObjectName("verticalLayout")
        self.treeView_Projects = QtWidgets.QTreeView(self.centralWidget)
        self.treeView_Projects.setObjectName("treeView_Projects")
        self.verticalLayout.addWidget(self.treeView_Projects)
        ProjectsMainWindow.setCentralWidget(self.centralWidget)
        self.toolBar = QtWidgets.QToolBar(ProjectsMainWindow)
        self.toolBar.setObjectName("toolBar")
        ProjectsMainWindow.addToolBar(QtCore.Qt.TopToolBarArea, self.toolBar)

        self.retranslateUi(ProjectsMainWindow)
        QtCore.QMetaObject.connectSlotsByName(ProjectsMainWindow)

    def retranslateUi(self, ProjectsMainWindow):
        _translate = QtCore.QCoreApplication.translate
        ProjectsMainWindow.setWindowTitle(_translate("ProjectsMainWindow", "项目管理器"))
        self.toolBar.setWindowTitle(_translate("ProjectsMainWindow", "toolBar"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    ProjectsMainWindow = QtWidgets.QMainWindow()
    ui = Ui_ProjectsMainWindow()
    ui.setupUi(ProjectsMainWindow)
    ProjectsMainWindow.show()
    sys.exit(app.exec_())

