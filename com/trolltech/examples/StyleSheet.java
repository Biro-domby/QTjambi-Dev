/****************************************************************************
 **
 ** Copyright (C) 1992-2009 Nokia. All rights reserved.
 **
 ** This file is part of Qt Jambi.
 **
 ** ** $BEGIN_LICENSE$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
** 
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
** 
** In addition, as a special exception, Nokia gives you certain
** additional rights. These rights are described in the Nokia Qt LGPL
** Exception version 1.0, included in the file LGPL_EXCEPTION.txt in this
** package.
** 
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
** 
** If you are unsure which license is appropriate for your use, please
** contact the sales department at qt-sales@nokia.com.
** $END_LICENSE$

 **
 ** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 ** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 **
 ****************************************************************************/

package com.trolltech.examples;

import com.trolltech.examples.stylesheet.Ui_MainWindow;
import com.trolltech.examples.stylesheet.Ui_StyleSheetEditor;
import com.trolltech.qt.core.*;
import com.trolltech.qt.core.QIODevice.OpenModeFlag;
import com.trolltech.qt.gui.*;

@QtJambiExample(name = "Style Sheets")
public class StyleSheet extends QMainWindow {

    private Ui_MainWindow ui = new Ui_MainWindow();
    private StyleSheetEditor styleSheetEditor;

    public static void main(String args[]) {
        QApplication.initialize(args);

        StyleSheet styleSheet = new StyleSheet();
        styleSheet.show();

        QApplication.exec();
    }


    public StyleSheet() {
        ui.setupUi(this);

        ui.nameLabel.setProperty("class", "mandatory");

        styleSheetEditor = new StyleSheetEditor(this);

        statusBar().addWidget(new QLabel(tr("Ready")));

        ui.exitAction.triggered.connect(this, "close()");
        ui.aboutQtAction.triggered.connect(QApplication.instance(), "aboutQt()");
        ui.aboutQtJambiAction.triggered.connect(QApplication.instance(), "aboutQtJambi()");

        setWindowIcon(new QIcon("classpath:com/trolltech/images/qt-logo.png"));
    }

    private void on_editStyleAction_triggered() {
        styleSheetEditor.show();
        styleSheetEditor.activateWindow();
    }

    private void on_aboutAction_triggered() {
        QMessageBox.about(this, tr("About Style sheet"),
                tr("The <b>Style Sheet</b> example shows how widgets can be styled "
                        + "using <a href=\"http://doc.trolltech.com/4.2/stylesheet.html\">Qt "
                        + "Style Sheets</a>. Click <b>File|Edit Style Sheet</b> to pop up the "
                        + "style editor, and either choose an existing style sheet or design "
                        + "your own."));
    }

    private class StyleSheetEditor extends QDialog {

        private Ui_StyleSheetEditor ui = new Ui_StyleSheetEditor();
        private QWidget main;

        StyleSheetEditor(QWidget parent) {
            super(parent);
            main = parent;
            ui.setupUi(this);

            QRegExp regExp = new QRegExp("Q(.*)Style");
            String defaultStyle = QApplication.style().getClass().getSimpleName();
            if (regExp.exactMatch(defaultStyle))
                defaultStyle = regExp.cap(1);

            ui.styleCombo.addItems(QStyleFactory.keys());
            ui.styleCombo.setCurrentIndex(ui.styleCombo.findText(defaultStyle));
            ui.styleSheetCombo.setCurrentIndex(ui.styleSheetCombo.findText("Coffee"));
            loadStyleSheet("Coffee");
        }

        void on_styleCombo_activated(final String styleName) {
            QStyle style = QStyleFactory.create(styleName);
            setStyle(style, main);
            setStyle(style, this);
            ui.applyButton.setEnabled(false);
        }

        void setStyle(QStyle style, QObject object)
        {
            for (QObject obj : object.children()) {
                if (obj instanceof QWidget) {
                    ((QWidget) obj).setStyle(style);
                    setStyle(style, obj);
                }
            }
        }

        void on_styleSheetCombo_activated(final String sheetName) {
            loadStyleSheet(sheetName);
        }

        void on_styleTextEdit_textChanged() {
            ui.applyButton.setEnabled(true);
        }

        void on_applyButton_clicked() {
            main.setStyleSheet(ui.styleTextEdit.toPlainText());
            ui.applyButton.setEnabled(false);
        }

        void loadStyleSheet(final String sheetName) {
            QFile file = new QFile("classpath:com/trolltech/examples/stylesheet/qss/"
                    + sheetName.toLowerCase() + ".qss");

            file.open(OpenModeFlag.ReadOnly);
            String styleSheet = file.readAll().toString();

            ui.styleTextEdit.setPlainText(styleSheet);
            main.setStyleSheet(styleSheet);
            ui.applyButton.setEnabled(false);
            file.close();
        }
    }

}
