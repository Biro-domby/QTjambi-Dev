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

package com.trolltech.examples.tutorial;

import com.trolltech.qt.gui.*;
import com.trolltech.qt.core.*;

public class ConnectedSliders extends QWidget
{
    public ConnectedSliders()
    {
        QPushButton quit = new QPushButton(tr("Quit"));
//! [0]
        quit.setFont(new QFont("Times", 18, QFont.Weight.Bold.value()));
//! [0]

        quit.clicked.connect(QApplication.instance(), "quit()");

        QGridLayout grid = new QGridLayout();
//! [1]
        LCDRange previousRange = null;
//! [1] //! [2]

        for (int row = 0; row < 3; ++row) {
            for (int column = 0; column < 3; ++column) {
                LCDRange lcdRange = new LCDRange();
                grid.addWidget(lcdRange, row, column);

            if (previousRange != null)
                lcdRange.valueChanged.
                connect(previousRange, "setValue(int)");
//! [2]

//! [3]
                previousRange = lcdRange;
//! [3] //! [4]
            }
//! [4] //! [5]
        }
//! [5]
        QVBoxLayout layout = new QVBoxLayout();
        layout.addWidget(quit);
        layout.addLayout(grid);
        setLayout(layout);
        setWindowTitle(tr("One Thing Leads to Another"));
    }

    class LCDRange extends QWidget
    {
        private QSlider slider;
        private int value;

//! [6]
        public final Signal1<Integer> valueChanged = new Signal1<Integer>();
//! [6]

//! [7]
        public LCDRange()
//! [7] //! [8]
        {
//! [8]
            QLCDNumber lcd = new QLCDNumber(2);
            lcd.setSegmentStyle(QLCDNumber.SegmentStyle.Filled);

            slider = new QSlider(Qt.Orientation.Horizontal);
            slider.setRange(0, 99);
            slider.setValue(0);

//! [9]
            slider.valueChanged.connect(lcd, "display(int)");
//! [9] //! [10]
            slider.valueChanged.connect(valueChanged);
//! [10]

            QVBoxLayout layout = new QVBoxLayout();
            layout.addWidget(lcd);
            layout.addWidget(slider);
            setLayout(layout);
//! [11]
        }
//! [11]

//! [12]
        public int value()
        {
            return value;
        }
//! [12]

//! [13]
        public void setValue(int value)
        {
            slider.setValue(value);
        }
//! [13]
    }

    public static void main(String args[])
    {
        QApplication.initialize(args);

        ConnectedSliders widget = new ConnectedSliders();
        widget.show();

        QApplication.exec();
    }
}
