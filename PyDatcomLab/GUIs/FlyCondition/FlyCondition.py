import  os,sys
from PyQt5 import QtCore, QtWidgets
from PyQt5.QtWidgets import QMessageBox
from PyDatcom.DatcomWidgets.FlyCondition import FlyConditionUI


class FlyCondition(QtWidgets.QWidget):
    def __init__(self):
        super(FlyCondition,self).__init__()
        self.Ui = FlyConditionUI.Ui_Form()
        self.Ui.setupUi(self)

    #slotItemClicked 响应左侧点击的界面逻辑
    def slot1(self):
        QtWidgets.QMessageBox.information(self,"标题","Hello world!",QMessageBox.Yes|QMessageBox.No)




if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = FlyCondition()
    window.show()
    sys.exit(app.exec_())