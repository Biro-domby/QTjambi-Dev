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

package com.trolltech.qt.gui;

import java.lang.reflect.Method;

import com.trolltech.qt.QNoNativeResourcesException;
import com.trolltech.qt.core.QByteArray;

// Not actually generated, but just to make sure not all
// virtual calls go through Java
@com.trolltech.qt.QtJambiGeneratedClass
public class QItemEditorCreator extends QItemEditorCreatorBase {

    public QItemEditorCreator(String valuePropertyName, Class<? extends QWidget> widgetType) {
        super((QPrivateConstructor) null);

        boolean createWidgetOverride = false;
        boolean valuePropertyNameOverride = false;
        try {
            Method createWidgetMethod = getClass().getMethod("createWidget", QWidget.class);
            Method valuePropertyNameMethod = getClass().getMethod("valuePropertyName");

            createWidgetOverride = com.trolltech.qt.internal.QtJambiInternal.isImplementedInJava(createWidgetMethod);
            valuePropertyNameOverride = com.trolltech.qt.internal.QtJambiInternal.isImplementedInJava(valuePropertyNameMethod);
        } catch (Throwable t) {
            throw new RuntimeException("Cannot construct QItemEditorCreator", t);
        }

        QByteArray valuePropertyNameByteArray = new QByteArray(valuePropertyName);
        __qt_QItemEditorCreator(valuePropertyNameByteArray.nativeId(), widgetType,
                createWidgetOverride, valuePropertyNameOverride);
    }
    private native void __qt_QItemEditorCreator(long valuePropertyNameId, Class<? extends QWidget> widgetType, boolean createWidgetOverride, boolean valuePropertyNameOverride);

    @Override
    public QWidget createWidget(QWidget parent) {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_createWidget(nativeId(), parent.nativeId());
    }
    private native QWidget __qt_createWidget(long nativeId, long parentId);

    @Override
    public QByteArray valuePropertyName() {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return valuePropertyName(nativeId());
    }
    private native QByteArray valuePropertyName(long nativeId);

}
