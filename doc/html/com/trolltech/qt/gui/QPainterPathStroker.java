package com.trolltech.qt.gui;

import com.trolltech.qt.*;



/**
The QPainterPathStroker class is used to generate fillable outlines for a given painter path. By calling the {@link com.trolltech.qt.gui.QPainterPathStroker#createStroke(com.trolltech.qt.gui.QPainterPath) createStroke()} function, passing a given {@link com.trolltech.qt.gui.QPainterPath QPainterPath} as argument, a new painter path representing the outline of the given path is created. The newly created painter path can then be filled to draw the original painter path's outline. <p>You can control the various design aspects (width, cap styles, join styles and dash pattern) of the outlining using the following functions: <ul><li> {@link com.trolltech.qt.gui.QPainterPathStroker#setWidth(double) setWidth()}</li><li> {@link com.trolltech.qt.gui.QPainterPathStroker#setCapStyle(com.trolltech.qt.core.Qt.PenCapStyle) setCapStyle()}</li><li> {@link com.trolltech.qt.gui.QPainterPathStroker#setJoinStyle(com.trolltech.qt.core.Qt.PenJoinStyle) setJoinStyle()}</li><li> {@link com.trolltech.qt.gui.QPainterPathStroker#setDashPattern(com.trolltech.qt.core.Qt.PenStyle) setDashPattern()}</li></ul> The {@link com.trolltech.qt.gui.QPainterPathStroker#setDashPattern(com.trolltech.qt.core.Qt.PenStyle) setDashPattern()} function accepts both a {@link com.trolltech.qt.core.Qt.PenStyle Qt::PenStyle } object and a vector representation of the pattern as argument. <p>In addition you can specify a curve's threshold, controlling the granularity with which a curve is drawn, using the {@link com.trolltech.qt.gui.QPainterPathStroker#setCurveThreshold(double) setCurveThreshold()} function. The default threshold is a well adjusted value (0.25), and normally you should not need to modify it. However, you can make the curve's appearance smoother by decreasing its value. <p>You can also control the miter limit for the generated outline using the {@link com.trolltech.qt.gui.QPainterPathStroker#setMiterLimit(double) setMiterLimit()} function. The miter limit describes how far from each join the miter join can extend. The limit is specified in the units of width so the pixelwise miter limit will be <tt>miterlimit * width</tt>. This value is only used if the join style is {@link com.trolltech.qt.core.Qt.PenJoinStyle Qt::MiterJoin }. <p>The painter path generated by the {@link com.trolltech.qt.gui.QPainterPathStroker#createStroke(com.trolltech.qt.gui.QPainterPath) createStroke()} function should only be used for outlining the given painter path. Otherwise it may cause unexpected behavior. Generated outlines also require the {@link com.trolltech.qt.core.Qt.FillRule Qt::WindingFill } rule which is set by default. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPen QPen}, and {@link com.trolltech.qt.gui.QBrush QBrush}. <br></DD></DT>
*/
@QtJambiGeneratedClass
public class QPainterPathStroker extends com.trolltech.qt.QtJambiObject
{

    static {
        com.trolltech.qt.gui.QtJambi_LibraryInitializer.init();
    }

/**
Creates a new stroker.
*/

    public QPainterPathStroker(){
        super((QPrivateConstructor)null);
        __qt_QPainterPathStroker();
    }

    native void __qt_QPainterPathStroker();

/**
Returns the cap style of the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setCapStyle(com.trolltech.qt.core.Qt.PenCapStyle) setCapStyle()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final com.trolltech.qt.core.Qt.PenCapStyle capStyle()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return com.trolltech.qt.core.Qt.PenCapStyle.resolve(__qt_capStyle(nativeId()));
    }
    @QtBlockedSlot
    native int __qt_capStyle(long __this__nativeId);

/**
Generates a new path that is a fillable area representing the outline of the given <tt>path</tt>. <p>The various design aspects of the outline are based on the stroker's properties: {@link com.trolltech.qt.gui.QPainterPathStroker#width() width()}, {@link com.trolltech.qt.gui.QPainterPathStroker#capStyle() capStyle()}, {@link com.trolltech.qt.gui.QPainterPathStroker#joinStyle() joinStyle()}, {@link com.trolltech.qt.gui.QPainterPathStroker#dashPattern() dashPattern()}, {@link com.trolltech.qt.gui.QPainterPathStroker#curveThreshold() curveThreshold()} and {@link com.trolltech.qt.gui.QPainterPathStroker#miterLimit() miterLimit()}. <p>The generated path should only be used for outlining the given painter path. Otherwise it may cause unexpected behavior. Generated outlines also require the {@link com.trolltech.qt.core.Qt.FillRule Qt::WindingFill } rule which is set by default.
*/

    @QtBlockedSlot
    public final com.trolltech.qt.gui.QPainterPath createStroke(com.trolltech.qt.gui.QPainterPath path)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_createStroke_QPainterPath(nativeId(), path == null ? 0 : path.nativeId());
    }
    @QtBlockedSlot
    native com.trolltech.qt.gui.QPainterPath __qt_createStroke_QPainterPath(long __this__nativeId, long path);

/**
Returns the curve flattening threshold for the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setCurveThreshold(double) setCurveThreshold()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final double curveThreshold()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_curveThreshold(nativeId());
    }
    @QtBlockedSlot
    native double __qt_curveThreshold(long __this__nativeId);

/**
Returns the dash offset for the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setDashOffset(double) setDashOffset()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final double dashOffset()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_dashOffset(nativeId());
    }
    @QtBlockedSlot
    native double __qt_dashOffset(long __this__nativeId);

/**
Returns the dash pattern for the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setDashPattern(com.trolltech.qt.core.Qt.PenStyle) setDashPattern()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final java.util.List<java.lang.Double> dashPattern()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_dashPattern(nativeId());
    }
    @QtBlockedSlot
    native java.util.List<java.lang.Double> __qt_dashPattern(long __this__nativeId);

/**
Returns the join style of the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setJoinStyle(com.trolltech.qt.core.Qt.PenJoinStyle) setJoinStyle()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final com.trolltech.qt.core.Qt.PenJoinStyle joinStyle()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return com.trolltech.qt.core.Qt.PenJoinStyle.resolve(__qt_joinStyle(nativeId()));
    }
    @QtBlockedSlot
    native int __qt_joinStyle(long __this__nativeId);

/**
Returns the miter limit for the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setMiterLimit(double) setMiterLimit()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final double miterLimit()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_miterLimit(nativeId());
    }
    @QtBlockedSlot
    native double __qt_miterLimit(long __this__nativeId);

/**
Sets the cap style of the generated outlines to <tt>style</tt>. If a dash pattern is set, each segment of the pattern is subject to the cap <tt>style</tt>. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#capStyle() capStyle()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setCapStyle(com.trolltech.qt.core.Qt.PenCapStyle style)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setCapStyle_PenCapStyle(nativeId(), style.value());
    }
    @QtBlockedSlot
    native void __qt_setCapStyle_PenCapStyle(long __this__nativeId, int style);

/**
Specifies the curve flattening <tt>threshold</tt>, controlling the granularity with which the generated outlines' curve is drawn. <p>The default threshold is a well adjusted value (0.25), and normally you should not need to modify it. However, you can make the curve's appearance smoother by decreasing its value. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#curveThreshold() curveThreshold()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setCurveThreshold(double threshold)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setCurveThreshold_double(nativeId(), threshold);
    }
    @QtBlockedSlot
    native void __qt_setCurveThreshold_double(long __this__nativeId, double threshold);

/**
Sets the dash offset for the generated outlines to <tt>offset</tt>. <p>See the documentation for {@link com.trolltech.qt.gui.QPen#setDashOffset(double) QPen::setDashOffset()} for a description of the dash offset. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#dashOffset() dashOffset()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setDashOffset(double offset)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setDashOffset_double(nativeId(), offset);
    }
    @QtBlockedSlot
    native void __qt_setDashOffset_double(long __this__nativeId, double offset);

/**
Sets the dash pattern for the generated outlines to <tt>style</tt>. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#dashPattern() dashPattern()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setDashPattern(com.trolltech.qt.core.Qt.PenStyle arg__1)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setDashPattern_PenStyle(nativeId(), arg__1.value());
    }
    @QtBlockedSlot
    native void __qt_setDashPattern_PenStyle(long __this__nativeId, int arg__1);

/**
This is an overloaded member function, provided for convenience. <p>Sets the dash pattern for the generated outlines to <tt>dashPattern</tt>. This function makes it possible to specify custom dash patterns. <p>Each element in the vector contains the lengths of the dashes and spaces in the stroke, beginning with the first dash in the first element, the first space in the second element, and alternating between dashes and spaces for each following pair of elements. <p>The vector can contain an odd number of elements, in which case the last element will be extended by the length of the first element when the pattern repeats.
*/

    @QtBlockedSlot
    public final void setDashPattern(java.util.List<java.lang.Double> dashPattern)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setDashPattern_List(nativeId(), dashPattern);
    }
    @QtBlockedSlot
    native void __qt_setDashPattern_List(long __this__nativeId, java.util.List<java.lang.Double> dashPattern);

/**
Sets the join style of the generated outlines to <tt>style</tt>. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#joinStyle() joinStyle()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setJoinStyle(com.trolltech.qt.core.Qt.PenJoinStyle style)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setJoinStyle_PenJoinStyle(nativeId(), style.value());
    }
    @QtBlockedSlot
    native void __qt_setJoinStyle_PenJoinStyle(long __this__nativeId, int style);

/**
Sets the miter limit of the generated outlines to <tt>limit</tt>. <p>The miter limit describes how far from each join the miter join can extend. The limit is specified in units of the currently set width. So the pixelwise miter limit will be <tt>miterlimit * width</tt>. <p>This value is only used if the join style is {@link com.trolltech.qt.core.Qt.PenJoinStyle Qt::MiterJoin }. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#miterLimit() miterLimit()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setMiterLimit(double length)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setMiterLimit_double(nativeId(), length);
    }
    @QtBlockedSlot
    native void __qt_setMiterLimit_double(long __this__nativeId, double length);

/**
Sets the width of the generated outline painter path to <tt>width</tt>. <p>The generated outlines will extend approximately 50% of <tt>width</tt> to each side of the given input path's original outline. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#width() width()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final void setWidth(double width)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setWidth_double(nativeId(), width);
    }
    @QtBlockedSlot
    native void __qt_setWidth_double(long __this__nativeId, double width);

/**
Returns the width of the generated outlines. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.gui.QPainterPathStroker#setWidth(double) setWidth()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final double width()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_width(nativeId());
    }
    @QtBlockedSlot
    native double __qt_width(long __this__nativeId);

/**
@exclude
*/

    public static native QPainterPathStroker fromNativePointer(QNativePointer nativePointer);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    protected QPainterPathStroker(QPrivateConstructor p) { super(p); } 
}
