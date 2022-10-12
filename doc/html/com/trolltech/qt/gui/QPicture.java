package com.trolltech.qt.gui;

import com.trolltech.qt.*;

import com.trolltech.qt.core.QIODevice;


/**
The QPicture class is a paint device that records and replays {@link com.trolltech.qt.gui.QPainter QPainter} commands. A picture serializes painter commands to an IO device in a platform-independent format. They are sometimes referred to as meta-files. <p>Qt pictures use a proprietary binary format. Unlike native picture (meta-file) formats on many window systems, Qt pictures have no limitations regarding their contents. Everything that can be painted on a widget or pixmap (e.g., fonts, pixmaps, regions, transformed graphics, etc.) can also be stored in a picture. <p>QPicture is resolution independent, i.e. a QPicture can be displayed on different devices (for example svg, pdf, ps, printer and screen) looking the same. This is, for instance, needed for WYSIWYG print preview. QPicture runs in the default system dpi, and scales the painter to match differences in resolution depending on the window system. <p>Example of how to record a picture: <pre class="snippet">
            QPicture picture = new QPicture();
            QPainter painter = new QPainter();
            painter.begin(picture);           // paint in picture
            painter.drawEllipse(10,20, 80,70); // draw an ellipse
            painter.end();                     // painting done
            picture.save("drawing.pic");       // save picture
    </pre> Note that the list of painter commands is reset on each call to the {@link com.trolltech.qt.gui.QPainter#begin(com.trolltech.qt.gui.QPaintDeviceInterface) QPainter::begin()} function. <p>Example of how to replay a picture: <pre class="snippet">
            QPicture picture = new QPicture();
            picture.load("drawing.pic");           // load picture
            QPainter painter = new QPainter();
            painter.begin(myWidget);              // paint in myWidget
            painter.drawPicture(0, 0, picture);    // draw the picture at (0,0)
            painter.end();                         // painting done
    </pre> Pictures can also be drawn using {@link com.trolltech.qt.gui.QPicture#play(com.trolltech.qt.gui.QPainter) play()}. Some basic data about a picture is available, for example, {@link com.trolltech.qt.gui.QPicture#size() size()}, {@link com.trolltech.qt.gui.QPicture#isNull() isNull()} and {@link com.trolltech.qt.gui.QPicture#boundingRect() boundingRect()}. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QMovie QMovie}. <br></DD></DT>
*/
@QtJambiGeneratedClass
public class QPicture extends com.trolltech.qt.QtJambiObject
    implements com.trolltech.qt.gui.QPaintDeviceInterface,
            java.lang.Cloneable
{

    static {
        com.trolltech.qt.gui.QtJambi_LibraryInitializer.init();
    }

/**
Constructs a copy of <tt>pic</tt>. <p>This constructor is fast thanks to {@link <a href="../shared.html">implicit sharing</a>}.
*/

    public QPicture(com.trolltech.qt.gui.QPicture arg__1){
        super((QPrivateConstructor)null);
        __qt_QPicture_QPicture(arg__1 == null ? 0 : arg__1.nativeId());
    }

    native void __qt_QPicture_QPicture(long arg__1);


/**
Constructs an empty picture. <p>The <tt>formatVersion</tt> parameter may be used to create a QPicture that can be read by applications that are compiled with earlier versions of Qt. <p>Note that the default formatVersion is -1 which signifies the current release, i.e. for Qt 4.0 a formatVersion of 7 is the same as the default formatVersion of -1. <p>Reading pictures generated by earlier versions of Qt is not supported in Qt 4.0.
*/

    public QPicture() {
        this((int)-1);
    }
/**
Constructs an empty picture. <p>The <tt>formatVersion</tt> parameter may be used to create a QPicture that can be read by applications that are compiled with earlier versions of Qt. <p>Note that the default formatVersion is -1 which signifies the current release, i.e. for Qt 4.0 a formatVersion of 7 is the same as the default formatVersion of -1. <p>Reading pictures generated by earlier versions of Qt is not supported in Qt 4.0.
*/

    public QPicture(int formatVersion){
        super((QPrivateConstructor)null);
        __qt_QPicture_int(formatVersion);
    }

    native void __qt_QPicture_int(int formatVersion);

/**
Returns the picture's bounding rectangle or an invalid rectangle if the picture contains no data. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPicture#setBoundingRect(com.trolltech.qt.core.QRect) setBoundingRect()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final com.trolltech.qt.core.QRect boundingRect()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_boundingRect(nativeId());
    }
    @QtBlockedSlot
    native com.trolltech.qt.core.QRect __qt_boundingRect(long __this__nativeId);

    @QtBlockedSlot
    private final com.trolltech.qt.QNativePointer data_private()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_data_private(nativeId());
    }
    @QtBlockedSlot
    native com.trolltech.qt.QNativePointer __qt_data_private(long __this__nativeId);

/**
Returns the bit depth (number of bit planes) of the paint device.
*/

    @QtBlockedSlot
    public final int depth()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_depth(nativeId());
    }
    @QtBlockedSlot
    native int __qt_depth(long __this__nativeId);

/**
Returns the height of the paint device in default coordinate system units (e. . pixels for {@link com.trolltech.qt.gui.QPixmap QPixmap} and {@link com.trolltech.qt.gui.QWidget QWidget}). <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#heightMM() heightMM()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int height()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_height(nativeId());
    }
    @QtBlockedSlot
    native int __qt_height(long __this__nativeId);

/**
Returns the height of the paint device in millimeters. Due to platform limitations it may not be possible to use this function to determine the actual physical size of a widget on the screen. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#height() height()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int heightMM()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_heightMM(nativeId());
    }
    @QtBlockedSlot
    native int __qt_heightMM(long __this__nativeId);

/**
Returns true if the picture contains no data; otherwise returns false.
*/

    @QtBlockedSlot
    public final boolean isNull()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_isNull(nativeId());
    }
    @QtBlockedSlot
    native boolean __qt_isNull(long __this__nativeId);

    @QtBlockedSlot
    private final boolean load(com.trolltech.qt.core.QIODevice dev, com.trolltech.qt.QNativePointer format)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_load_QIODevice_nativepointer(nativeId(), dev == null ? 0 : dev.nativeId(), format);
    }
    @QtBlockedSlot
    native boolean __qt_load_QIODevice_nativepointer(long __this__nativeId, long dev, com.trolltech.qt.QNativePointer format);

    @QtBlockedSlot
    private final boolean load(java.lang.String fileName, com.trolltech.qt.QNativePointer format)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_load_String_nativepointer(nativeId(), fileName, format);
    }
    @QtBlockedSlot
    native boolean __qt_load_String_nativepointer(long __this__nativeId, java.lang.String fileName, com.trolltech.qt.QNativePointer format);

/**
Returns the horizontal resolution of the device in dots per inch, which is used when computing font sizes. For X11, this is usually the same as could be computed from {@link com.trolltech.qt.gui.QPaintDevice#widthMM() widthMM()}. <p>Note that if the {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiX() logicalDpiX()} doesn't equal the {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiX() physicalDpiX()}, the corresponding {@link com.trolltech.qt.gui.QPaintEngine QPaintEngine} must handle the resolution mapping. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#logicalDpiY() logicalDpiY()}, and {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiX() physicalDpiX()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int logicalDpiX()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_logicalDpiX(nativeId());
    }
    @QtBlockedSlot
    native int __qt_logicalDpiX(long __this__nativeId);

/**
Returns the vertical resolution of the device in dots per inch, which is used when computing font sizes. For X11, this is usually the same as could be computed from {@link com.trolltech.qt.gui.QPaintDevice#heightMM() heightMM()}. <p>Note that if the {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiY() logicalDpiY()} doesn't equal the {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiY() physicalDpiY()}, the corresponding {@link com.trolltech.qt.gui.QPaintEngine QPaintEngine} must handle the resolution mapping. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#logicalDpiX() logicalDpiX()}, and {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiY() physicalDpiY()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int logicalDpiY()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_logicalDpiY(nativeId());
    }
    @QtBlockedSlot
    native int __qt_logicalDpiY(long __this__nativeId);

/**
Returns the number of different colors available for the paint device. Since this value is an int, it will not be sufficient to represent the number of colors on 32 bit displays, in this case INT_MAX is returned instead.
*/

    @QtBlockedSlot
    public final int numColors()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_numColors(nativeId());
    }
    @QtBlockedSlot
    native int __qt_numColors(long __this__nativeId);

/**
<brief>Writes thisQPicture
*/

    @QtBlockedSlot
    public final void writeTo(com.trolltech.qt.core.QDataStream arg__1)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_writeTo_QDataStream(nativeId(), arg__1 == null ? 0 : arg__1.nativeId());
    }
    @QtBlockedSlot
    native void __qt_writeTo_QDataStream(long __this__nativeId, long arg__1);

/**
<brief>Reads a QPicture
*/

    @QtBlockedSlot
    public final void readFrom(com.trolltech.qt.core.QDataStream arg__1)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_readFrom_QDataStream(nativeId(), arg__1 == null ? 0 : arg__1.nativeId());
    }
    @QtBlockedSlot
    native void __qt_readFrom_QDataStream(long __this__nativeId, long arg__1);

/**
Returns true if the device is currently being painted on, i. . someone has called {@link com.trolltech.qt.gui.QPainter#begin(com.trolltech.qt.gui.QPaintDeviceInterface) QPainter::begin()} but not yet called {@link com.trolltech.qt.gui.QPainter#end() QPainter::end()} for this device; otherwise returns false. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainter#isActive() QPainter::isActive()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final boolean paintingActive()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_paintingActive(nativeId());
    }
    @QtBlockedSlot
    native boolean __qt_paintingActive(long __this__nativeId);

/**
Returns the horizontal resolution of the device in dots per inch. For example, when printing, this resolution refers to the physical printer's resolution. The logical DPI on the other hand, refers to the resolution used by the actual paint engine. <p>Note that if the {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiX() physicalDpiX()} doesn't equal the {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiX() logicalDpiX()}, the corresponding {@link com.trolltech.qt.gui.QPaintEngine QPaintEngine} must handle the resolution mapping. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#physicalDpiY() physicalDpiY()}, and {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiX() logicalDpiX()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int physicalDpiX()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_physicalDpiX(nativeId());
    }
    @QtBlockedSlot
    native int __qt_physicalDpiX(long __this__nativeId);

/**
Returns the horizontal resolution of the device in dots per inch. For example, when printing, this resolution refers to the physical printer's resolution. The logical DPI on the other hand, refers to the resolution used by the actual paint engine. <p>Note that if the {@link com.trolltech.qt.gui.QPaintDevice#physicalDpiY() physicalDpiY()} doesn't equal the {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiY() logicalDpiY()}, the corresponding {@link com.trolltech.qt.gui.QPaintEngine QPaintEngine} must handle the resolution mapping. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#physicalDpiX() physicalDpiX()}, and {@link com.trolltech.qt.gui.QPaintDevice#logicalDpiY() logicalDpiY()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int physicalDpiY()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_physicalDpiY(nativeId());
    }
    @QtBlockedSlot
    native int __qt_physicalDpiY(long __this__nativeId);

/**
Replays the picture using <tt>painter</tt>, and returns true if successful; otherwise returns false. <p>This function does exactly the same as {@link com.trolltech.qt.gui.QPainter#drawPicture(com.trolltech.qt.core.QPoint, com.trolltech.qt.gui.QPicture) QPainter::drawPicture()} with (x, y) = (0, 0).
*/

    @QtBlockedSlot
    public final boolean play(com.trolltech.qt.gui.QPainter p)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_play_QPainter(nativeId(), p == null ? 0 : p.nativeId());
    }
    @QtBlockedSlot
    native boolean __qt_play_QPainter(long __this__nativeId, long p);

    @QtBlockedSlot
    private final boolean save(com.trolltech.qt.core.QIODevice dev, com.trolltech.qt.QNativePointer format)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_save_QIODevice_nativepointer(nativeId(), dev == null ? 0 : dev.nativeId(), format);
    }
    @QtBlockedSlot
    native boolean __qt_save_QIODevice_nativepointer(long __this__nativeId, long dev, com.trolltech.qt.QNativePointer format);

    @QtBlockedSlot
    private final boolean save(java.lang.String fileName, com.trolltech.qt.QNativePointer format)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_save_String_nativepointer(nativeId(), fileName, format);
    }
    @QtBlockedSlot
    native boolean __qt_save_String_nativepointer(long __this__nativeId, java.lang.String fileName, com.trolltech.qt.QNativePointer format);

/**
Sets the picture's bounding rectangle to <tt>r</tt>. The automatically calculated value is overridden. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPicture#boundingRect() boundingRect()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setBoundingRect(com.trolltech.qt.core.QRect r)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setBoundingRect_QRect(nativeId(), r == null ? 0 : r.nativeId());
    }
    @QtBlockedSlot
    native void __qt_setBoundingRect_QRect(long __this__nativeId, long r);

/**
Returns the size of the picture data. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPicture#data() data()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int size()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_size(nativeId());
    }
    @QtBlockedSlot
    native int __qt_size(long __this__nativeId);

/**
Returns the width of the paint device in default coordinate system units (e. . pixels for {@link com.trolltech.qt.gui.QPixmap QPixmap} and {@link com.trolltech.qt.gui.QWidget QWidget}). <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#widthMM() widthMM()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int width()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_width(nativeId());
    }
    @QtBlockedSlot
    native int __qt_width(long __this__nativeId);

/**
Returns the width of the paint device in millimeters. Due to platform limitations it may not be possible to use this function to determine the actual physical size of a widget on the screen. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice#width() width()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final int widthMM()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_widthMM(nativeId());
    }
    @QtBlockedSlot
    native int __qt_widthMM(long __this__nativeId);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    @QtBlockedSlot
    public int devType()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_devType(nativeId());
    }
    @QtBlockedSlot
    native int __qt_devType(long __this__nativeId);

/**
Returns the metric information for the given paint device <tt>metric</tt>. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPaintDevice.PaintDeviceMetric PaintDeviceMetric }. <br></DD></DT>
*/

    @QtBlockedSlot
    public int metric(com.trolltech.qt.gui.QPaintDevice.PaintDeviceMetric m)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_metric_PaintDeviceMetric(nativeId(), m.value());
    }
    @QtBlockedSlot
    native int __qt_metric_PaintDeviceMetric(long __this__nativeId, int m);

/**
Returns a pointer to the paint engine used for drawing on the device.
*/

    @QtBlockedSlot
    public com.trolltech.qt.gui.QPaintEngine paintEngine()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_paintEngine(nativeId());
    }
    @QtBlockedSlot
    native com.trolltech.qt.gui.QPaintEngine __qt_paintEngine(long __this__nativeId);

/**
Sets the picture data directly from <tt>data</tt> and <tt>size</tt>. This function copies the input data. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPicture#data() data()}, and {@link com.trolltech.qt.gui.QPicture#size() size()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public void setData(byte[] data)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setData_nativepointer_int(nativeId(), data);
    }
    @QtBlockedSlot
    native void __qt_setData_nativepointer_int(long __this__nativeId, byte[] data);

/**
@exclude
*/

    public static native QPicture fromNativePointer(QNativePointer nativePointer);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    protected QPicture(QPrivateConstructor p) { super(p); } 

/**
@exclude
*/

    public static native QNativePointer nativePointerArray(QPicture array[]);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    @QtBlockedSlot public native long __qt_cast_to_QPaintDevice(long ptr);

/**
This is an overloaded member function, provided for convenience.

<tt>dev</tt> is the device used for loading.
*/

    public final boolean load(QIODevice dev) {
        return load(dev, (QNativePointer) null);
    }

/**
Loads a picture from the file specified by fileName and returns true
if successful; otherwise returns false.
*/

    public final boolean load(String fileName) {
        return load(fileName, (com.trolltech.qt.QNativePointer) null);
    }

/**
This is an overloaded member function, provided for convenience.
<p>
<tt>dev</tt> is the device to use for saving.
*/

    public final boolean save(QIODevice dev) {
        return save(dev, (com.trolltech.qt.QNativePointer) null);
    }

/**
Saves a picture to the file specified by <tt>fileName</tt> and returns
true if successful; otherwise returns false.
*/

    public final boolean save(String fileName) {
        return save(fileName, (com.trolltech.qt.QNativePointer) null);
    }

/**
Returns the picture data.
*/

    public final byte[] data() {
        QNativePointer npData = data_private();
        if (npData == null)
            return null;
        byte returned[] = new byte[size()];
        for (int i = 0; i < returned.length; ++i)
            returned[i] = npData.byteAt(i);
        return returned;
    }


/**
This method is reimplemented for internal reasons
*/

    @Override
    public QPicture clone() {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_clone(nativeId());
    }
    native QPicture __qt_clone(long __this_nativeId);
}
