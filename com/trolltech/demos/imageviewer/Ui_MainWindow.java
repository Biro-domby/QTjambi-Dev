/********************************************************************************
** Form generated from reading ui file 'MainWindowUI.jui'
**
** Created: Mon Jul 27 13:12:56 2009
**      by: Qt User Interface Compiler version 4.5.2
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

package com.trolltech.demos.imageviewer;

import com.trolltech.qt.core.*;
import com.trolltech.qt.gui.*;

public class Ui_MainWindow implements com.trolltech.qt.QUiForm<QMainWindow>
{
    public QAction actionZoom_In;
    public QAction actionZoom_Out;
    public QAction actionExit;
    public QAction actionAbout_Image_Viewer;
    public QAction actionAbout_Qt_Jambi;
    public QAction actionSave;
    public QAction actionClose;
    public QAction actionAbout_Qt;
    public QWidget centralwidget;
    public QMenuBar menubar;
    public QMenu menuHelp;
    public QMenu menuFile;
    public QStatusBar statusbar;
    public QDockWidget treeDock;
    public QWidget treeDockContents;
    public QGridLayout gridLayout;
    public QTreeView dirView;
    public QDockWidget tableDock;
    public QWidget tableDockContents;
    public QGridLayout gridLayout1;
    public QTreeView tableView;
    public QDockWidget effectsDock;
    public QWidget effectsDockContents;
    public QGridLayout gridLayout2;
    public QGroupBox groupBox;
    public QGridLayout gridLayout3;
    public QLabel labelCyan;
    public QSlider redCyanBalance;
    public QLabel labelRed;
    public QLabel labelMagenta;
    public QSlider greenMagentaBalance;
    public QLabel labelGreen;
    public QLabel labelYellow;
    public QSlider blueYellowBalance;
    public QLabel labelBlue;
    public QLabel labelWhite;
    public QSlider colorBalance;
    public QLabel labelBlack;
    public QCheckBox inverted;
    public QPushButton resetColorBalance;
    public QSpacerItem spacerItem;
    public QToolBar toolBar_2;

    public Ui_MainWindow() { super(); }

    public void setupUi(QMainWindow MainWindow)
    {
        MainWindow.setObjectName("MainWindow");
        MainWindow.resize(new QSize(1064, 566).expandedTo(MainWindow.minimumSizeHint()));
        actionZoom_In = new QAction(MainWindow);
        actionZoom_In.setObjectName("actionZoom_In");
        actionZoom_In.setEnabled(false);
        actionZoom_In.setIcon(new QIcon(new QPixmap("classpath:*#/com/trolltech/demos/imageviewer/zoomin.png")));
        actionZoom_Out = new QAction(MainWindow);
        actionZoom_Out.setObjectName("actionZoom_Out");
        actionZoom_Out.setEnabled(false);
        actionZoom_Out.setIcon(new QIcon(new QPixmap("classpath:*#/com/trolltech/demos/imageviewer/zoomout.png")));
        actionExit = new QAction(MainWindow);
        actionExit.setObjectName("actionExit");
        actionAbout_Image_Viewer = new QAction(MainWindow);
        actionAbout_Image_Viewer.setObjectName("actionAbout_Image_Viewer");
        actionAbout_Qt_Jambi = new QAction(MainWindow);
        actionAbout_Qt_Jambi.setObjectName("actionAbout_Qt_Jambi");
        actionSave = new QAction(MainWindow);
        actionSave.setObjectName("actionSave");
        actionSave.setEnabled(false);
        actionSave.setIcon(new QIcon(new QPixmap("classpath:*#/com/trolltech/images/save.png")));
        actionClose = new QAction(MainWindow);
        actionClose.setObjectName("actionClose");
        actionClose.setEnabled(false);
        actionClose.setIcon(new QIcon(new QPixmap("classpath:*#/com/trolltech/images/close.png")));
        actionAbout_Qt = new QAction(MainWindow);
        actionAbout_Qt.setObjectName("actionAbout_Qt");
        centralwidget = new QWidget(MainWindow);
        centralwidget.setObjectName("centralwidget");
        MainWindow.setCentralWidget(centralwidget);
        menubar = new QMenuBar(MainWindow);
        menubar.setObjectName("menubar");
        menubar.setGeometry(new QRect(0, 0, 1064, 21));
        menuHelp = new QMenu(menubar);
        menuHelp.setObjectName("menuHelp");
        menuFile = new QMenu(menubar);
        menuFile.setObjectName("menuFile");
        MainWindow.setMenuBar(menubar);
        statusbar = new QStatusBar(MainWindow);
        statusbar.setObjectName("statusbar");
        MainWindow.setStatusBar(statusbar);
        treeDock = new QDockWidget(MainWindow);
        treeDock.setObjectName("treeDock");
        treeDock.setFeatures(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.createQFlags(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetFloatable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetMovable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.NoDockWidgetFeatures));
        treeDockContents = new QWidget();
        treeDockContents.setObjectName("treeDockContents");
        gridLayout = new QGridLayout(treeDockContents);
        gridLayout.setSpacing(6);
        gridLayout.setMargin(9);
        gridLayout.setObjectName("gridLayout");
        dirView = new QTreeView(treeDockContents);
        dirView.setObjectName("dirView");

        gridLayout.addWidget(dirView, 0, 0, 1, 1);

        treeDock.setWidget(treeDockContents);
        MainWindow.addDockWidget(com.trolltech.qt.core.Qt.DockWidgetArea.resolve(1), treeDock);
        tableDock = new QDockWidget(MainWindow);
        tableDock.setObjectName("tableDock");
        tableDock.setFeatures(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.createQFlags(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetFloatable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetMovable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.NoDockWidgetFeatures));
        tableDockContents = new QWidget();
        tableDockContents.setObjectName("tableDockContents");
        gridLayout1 = new QGridLayout(tableDockContents);
        gridLayout1.setSpacing(6);
        gridLayout1.setMargin(9);
        gridLayout1.setObjectName("gridLayout1");
        tableView = new QTreeView(tableDockContents);
        tableView.setObjectName("tableView");
        tableView.setAlternatingRowColors(true);
        tableView.setSelectionMode(com.trolltech.qt.gui.QAbstractItemView.SelectionMode.SingleSelection);
        tableView.setSelectionBehavior(com.trolltech.qt.gui.QAbstractItemView.SelectionBehavior.SelectRows);
        tableView.setRootIsDecorated(false);
        tableView.setUniformRowHeights(true);
        tableView.setItemsExpandable(false);

        gridLayout1.addWidget(tableView, 0, 0, 1, 1);

        tableDock.setWidget(tableDockContents);
        MainWindow.addDockWidget(com.trolltech.qt.core.Qt.DockWidgetArea.resolve(2), tableDock);
        effectsDock = new QDockWidget(MainWindow);
        effectsDock.setObjectName("effectsDock");
        effectsDock.setFeatures(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.createQFlags(com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetFloatable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.DockWidgetMovable,com.trolltech.qt.gui.QDockWidget.DockWidgetFeature.NoDockWidgetFeatures));
        effectsDockContents = new QWidget();
        effectsDockContents.setObjectName("effectsDockContents");
        gridLayout2 = new QGridLayout(effectsDockContents);
        gridLayout2.setObjectName("gridLayout2");
        groupBox = new QGroupBox(effectsDockContents);
        groupBox.setObjectName("groupBox");
        groupBox.setEnabled(false);
        gridLayout3 = new QGridLayout(groupBox);
        gridLayout3.setObjectName("gridLayout3");
        labelCyan = new QLabel(groupBox);
        labelCyan.setObjectName("labelCyan");
        labelCyan.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_cyan_16.png")));

        gridLayout3.addWidget(labelCyan, 0, 0, 1, 1);

        redCyanBalance = new QSlider(groupBox);
        redCyanBalance.setObjectName("redCyanBalance");
        redCyanBalance.setMinimum(-100);
        redCyanBalance.setMaximum(100);
        redCyanBalance.setOrientation(com.trolltech.qt.core.Qt.Orientation.Horizontal);

        gridLayout3.addWidget(redCyanBalance, 0, 1, 1, 1);

        labelRed = new QLabel(groupBox);
        labelRed.setObjectName("labelRed");
        labelRed.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_red_16.png")));

        gridLayout3.addWidget(labelRed, 0, 2, 1, 1);

        labelMagenta = new QLabel(groupBox);
        labelMagenta.setObjectName("labelMagenta");
        labelMagenta.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_magenta_16.png")));

        gridLayout3.addWidget(labelMagenta, 1, 0, 1, 1);

        greenMagentaBalance = new QSlider(groupBox);
        greenMagentaBalance.setObjectName("greenMagentaBalance");
        greenMagentaBalance.setMinimum(-100);
        greenMagentaBalance.setMaximum(100);
        greenMagentaBalance.setOrientation(com.trolltech.qt.core.Qt.Orientation.Horizontal);

        gridLayout3.addWidget(greenMagentaBalance, 1, 1, 1, 1);

        labelGreen = new QLabel(groupBox);
        labelGreen.setObjectName("labelGreen");
        labelGreen.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_green_16.png")));

        gridLayout3.addWidget(labelGreen, 1, 2, 1, 1);

        labelYellow = new QLabel(groupBox);
        labelYellow.setObjectName("labelYellow");
        labelYellow.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_yellow_16.png")));

        gridLayout3.addWidget(labelYellow, 2, 0, 1, 1);

        blueYellowBalance = new QSlider(groupBox);
        blueYellowBalance.setObjectName("blueYellowBalance");
        blueYellowBalance.setMinimum(-100);
        blueYellowBalance.setMaximum(100);
        blueYellowBalance.setOrientation(com.trolltech.qt.core.Qt.Orientation.Horizontal);

        gridLayout3.addWidget(blueYellowBalance, 2, 1, 1, 1);

        labelBlue = new QLabel(groupBox);
        labelBlue.setObjectName("labelBlue");
        labelBlue.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_blue_16.png")));

        gridLayout3.addWidget(labelBlue, 2, 2, 1, 1);

        labelWhite = new QLabel(groupBox);
        labelWhite.setObjectName("labelWhite");
        labelWhite.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_white_16.png")));

        gridLayout3.addWidget(labelWhite, 3, 0, 1, 1);

        colorBalance = new QSlider(groupBox);
        colorBalance.setObjectName("colorBalance");
        colorBalance.setMinimum(-100);
        colorBalance.setMaximum(100);
        colorBalance.setOrientation(com.trolltech.qt.core.Qt.Orientation.Horizontal);

        gridLayout3.addWidget(colorBalance, 3, 1, 1, 1);

        labelBlack = new QLabel(groupBox);
        labelBlack.setObjectName("labelBlack");
        labelBlack.setPixmap(new QPixmap(("classpath:*#/com/trolltech/demos/imageviewer/circle_black_16.png")));

        gridLayout3.addWidget(labelBlack, 3, 2, 1, 1);

        inverted = new QCheckBox(groupBox);
        inverted.setObjectName("inverted");

        gridLayout3.addWidget(inverted, 4, 1, 1, 1);

        resetColorBalance = new QPushButton(groupBox);
        resetColorBalance.setObjectName("resetColorBalance");

        gridLayout3.addWidget(resetColorBalance, 5, 1, 1, 1);


        gridLayout2.addWidget(groupBox, 0, 0, 1, 1);

        spacerItem = new QSpacerItem(256, 20, com.trolltech.qt.gui.QSizePolicy.Policy.Minimum, com.trolltech.qt.gui.QSizePolicy.Policy.Expanding);

        gridLayout2.addItem(spacerItem, 1, 0, 1, 1);

        effectsDock.setWidget(effectsDockContents);
        MainWindow.addDockWidget(com.trolltech.qt.core.Qt.DockWidgetArea.resolve(2), effectsDock);
        toolBar_2 = new QToolBar(MainWindow);
        toolBar_2.setObjectName("toolBar_2");
        toolBar_2.setOrientation(com.trolltech.qt.core.Qt.Orientation.Horizontal);
        MainWindow.addToolBar(com.trolltech.qt.core.Qt.ToolBarArea.TopToolBarArea, toolBar_2);

        menubar.addAction(menuFile.menuAction());
        menubar.addAction(menuHelp.menuAction());
        menuHelp.addAction(actionAbout_Image_Viewer);
        menuHelp.addSeparator();
        menuHelp.addAction(actionAbout_Qt_Jambi);
        menuHelp.addAction(actionAbout_Qt);
        menuFile.addAction(actionSave);
        menuFile.addAction(actionClose);
        menuFile.addSeparator();
        menuFile.addAction(actionExit);
        toolBar_2.addAction(actionSave);
        toolBar_2.addAction(actionClose);
        retranslateUi(MainWindow);

        MainWindow.connectSlotsByName();
    } // setupUi

    void retranslateUi(QMainWindow MainWindow)
    {
        MainWindow.setWindowTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Qt Jambi Image Viewer", null));
        actionZoom_In.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Zoom In", null));
        actionZoom_Out.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Zoom Out", null));
        actionExit.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "E&xit", null));
        actionAbout_Image_Viewer.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "About &Image Viewer", null));
        actionAbout_Qt_Jambi.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "About Qt &Jambi", null));
        actionSave.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "&Save", null));
        actionSave.setShortcut(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Ctrl+S", null));
        actionClose.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Cl&ose", null));
        actionAbout_Qt.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "About &Qt", null));
        menuHelp.setTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "&Help", null));
        menuFile.setTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "&File", null));
        treeDock.setWindowTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Directories", null));
        tableDock.setWindowTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Images ", null));
        effectsDock.setWindowTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Image Effects", null));
        groupBox.setTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Color Balance", null));
        labelCyan.setText("");
        labelRed.setText("");
        labelMagenta.setText("");
        labelGreen.setText("");
        labelYellow.setText("");
        labelBlue.setText("");
        labelWhite.setText("");
        labelBlack.setText("");
        inverted.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Inverted", null));
        resetColorBalance.setText(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "Reset", null));
        toolBar_2.setWindowTitle(com.trolltech.qt.core.QCoreApplication.translate("MainWindow", "File toolbar", null));
    } // retranslateUi

}

