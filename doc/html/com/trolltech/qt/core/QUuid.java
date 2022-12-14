package com.trolltech.qt.core;

import com.trolltech.qt.*;



/**
The QUuid class stores a Universally Unique Identifier (UUID). Using Universally Unique IDentifiers (UUID) is a standard way to uniquely identify entities in a distributed computing environment. A UUID is a 16-byte (128-bit) number generated by some algorithm that is meant to guarantee that the UUID will be unique in the distributed computing environment where it is used. The acronym GUID is often used instead, Globally Unique IDentifiers, but it refers to the same thing.<a name="variant-field"> Actually, the GUID is one variant of UUID. Multiple variants are in use. Each UUID contains a bit field that specifies which type (variant) of UUID it is. Call {@link com.trolltech.qt.core.QUuid#variant() variant()} to discover which type of UUID an instance of QUuid contains. It extracts the three most signifcant bits of byte 8 of the 16 bytes. In QUuid, byte 8 is <tt>QUuid::data4[0]</tt>. If you create instances of QUuid using the constructor that accepts all the numeric values as parameters, use the following table to set the three most significant bits of parameter <tt>b1</tt>, which becomes <tt>QUuid::data4[0]</tt> and contains the variant field in its three most significant bits. In the table, 'x' means don't care. <table align="center" border="0" cellpadding="2" cellspacing="1"><thead><tr class="qt-style" valign="top"><th><center> msb0</center></th><th><center> msb1</center></th><th><center> msb2</center></th><th><center> Variant</center></th></tr></thead><tr valign="top" class="even"><td> 0</td><td> x</td><td> x</td><td> NCS (Network Computing System)</td></tr><tr valign="top" class="odd"><td> 1</td><td> 0</td><td> x</td><td> DCE (Distributed Computing Environment)</td></tr><tr valign="top" class="even"><td> 1</td><td> 1</td><td> 0</td><td> Microsoft (GUID)</td></tr><tr valign="top" class="odd"><td> 1</td><td> 1</td><td> 1</td><td> Reserved for future expansion</td></tr></table><a name="version-field"> If {@link com.trolltech.qt.core.QUuid#variant() variant()} returns {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE }, the UUID also contains a version field in the four most significant bits of <tt>QUuid::data3</tt>, and you can call {@link com.trolltech.qt.core.QUuid#version() version()} to discover which version your QUuid contains. If you create instances of QUuid using the constructor that accepts all the numeric values as parameters, use the following table to set the four most significant bits of parameter <tt>w2</tt>, which becomes <tt>QUuid::data3</tt> and contains the version field in its four most significant bits. <table align="center" border="0" cellpadding="2" cellspacing="1"><thead><tr class="qt-style" valign="top"><th><center> msb0</center></th><th><center> msb1</center></th><th><center> msb2</center></th><th><center> msb3</center></th><th><center> Version</center></th></tr></thead><tr valign="top" class="even"><td> 0</td><td> 0</td><td> 0</td><td> 1</td><td> Time</td></tr><tr valign="top" class="odd"><td> 0</td><td> 0</td><td> 1</td><td> 0</td><td> Embedded POSIX</td></tr><tr valign="top" class="even"><td> 0</td><td> 0</td><td> 1</td><td> 1</td><td> Name</td></tr><tr valign="top" class="odd"><td> 0</td><td> 1</td><td> 0</td><td> 0</td><td> Random</td></tr></table> The field layouts for the DCE versions listed in the table above are specified in the Network Working Group UUID Specification. <p>Most platforms provide a tool for generating new UUIDs, e.g. <tt>uuidgen</tt> and <tt>guidgen</tt>. You can also use {@link com.trolltech.qt.core.QUuid#createUuid() createUuid()}. UUIDs generated by {@link com.trolltech.qt.core.QUuid#createUuid() createUuid()} are of the random type. Their {@link com.trolltech.qt.core.QUuid.Version QUuid::Version } bits are set to {@link com.trolltech.qt.core.QUuid.Version QUuid::Random }, and their {@link com.trolltech.qt.core.QUuid.Variant QUuid::Variant } bits are set to {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE }. The rest of the UUID is composed of random numbers. Theoretically, this means there is a small chance that a UUID generated by {@link com.trolltech.qt.core.QUuid#createUuid() createUuid()} will not be unique. But it is a very small chance. <p>UUIDs can be constructed from numeric values or from strings, or using the static {@link com.trolltech.qt.core.QUuid#createUuid() createUuid()} function. They can be converted to a string with {@link com.trolltech.qt.core.QUuid#toString() toString()}. UUIDs have a {@link com.trolltech.qt.core.QUuid#variant() variant()} and a {@link com.trolltech.qt.core.QUuid#version() version()}, and null UUIDs return true from {@link com.trolltech.qt.core.QUuid#isNull() isNull()}.
*/
@QtJambiGeneratedClass
public class QUuid extends com.trolltech.qt.QtJambiObject
    implements java.lang.Comparable<Object>,
            java.lang.Cloneable
{

    static {
        com.trolltech.qt.core.QtJambi_LibraryInitializer.init();
    }
/**
This enum defines the values used in the {@link com.trolltech.qt.core.QUuid variant field} of the UUID. The value in the variant field determines the layout of the 128-bit value.
*/

    public enum Variant implements com.trolltech.qt.QtEnumerator {
/**
 Variant is unknown
*/

        VarUnknown(-1),
/**
 Reserved for NCS (Network Computing System) backward compatibility
*/

        NCS(0),
/**
 Distributed Computing Environment, the scheme used by {@link com.trolltech.qt.core.QUuid QUuid}
*/

        DCE(2),
/**
 Reserved for Microsoft backward compatibility (GUID)
*/

        Microsoft(6),
/**
 Reserved for future definition
*/

        Reserved(7);

        Variant(int value) { this.value = value; }
/**
This function should return an integer value for the enum values of the enumeration that implements this interface.
*/

        public int value() { return value; }

/**
<brief>Returns the QUuid$Variant constant with the specified <tt>int</tt>.</brief>
*/

        public static Variant resolve(int value) {
            return (Variant) resolve_internal(value);
        }
        private static Object resolve_internal(int value) {
            switch (value) {
            case -1: return VarUnknown;
            case 0: return NCS;
            case 2: return DCE;
            case 6: return Microsoft;
            case 7: return Reserved;
            }
            throw new com.trolltech.qt.QNoSuchEnumValueException(value);
        }
        private final int value;

    }
/**
This enum defines the values used in the {@link com.trolltech.qt.core.QUuid version field} of the UUID. The version field is meaningful only if the value in the {@link com.trolltech.qt.core.QUuid variant field} is {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE }.
*/

    public enum Version implements com.trolltech.qt.QtEnumerator {
/**
 Version is unknown
*/

        VerUnknown(-1),
/**
 Time-based, by using timestamp, clock sequence, and MAC network card address (if available) for the node sections
*/

        Time(1),
/**
 DCE Security version, with embedded POSIX UUIDs
*/

        EmbeddedPOSIX(2),
/**
 Name-based, by using values from a name for all sections
*/

        Name(3),
/**
 Random-based, by using random numbers for all sections
*/

        Random(4);

        Version(int value) { this.value = value; }
/**
This function should return an integer value for the enum values of the enumeration that implements this interface.
*/

        public int value() { return value; }

/**
<brief>Returns the QUuid$Version constant with the specified <tt>int</tt>.</brief>
*/

        public static Version resolve(int value) {
            return (Version) resolve_internal(value);
        }
        private static Object resolve_internal(int value) {
            switch (value) {
            case -1: return VerUnknown;
            case 1: return Time;
            case 2: return EmbeddedPOSIX;
            case 3: return Name;
            case 4: return Random;
            }
            throw new com.trolltech.qt.QNoSuchEnumValueException(value);
        }
        private final int value;

    }


/**
Creates the null UUID. {@link com.trolltech.qt.core.QUuid#toString() toString()} will output the null UUID as "{00000000-0000-0000-0000-000000000000}".
*/

    public QUuid(){
        super((QPrivateConstructor)null);
        __qt_QUuid();
    }

    native void __qt_QUuid();

/**
Creates a QUuid object from the string <tt>text</tt>, which must be formatted as five hex fields separated by '-', e. ., "{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}" where 'x' is a hex digit. The curly braces shown here are optional, but it is normal to include them. If the conversion fails, a null UUID is created. See {@link com.trolltech.qt.core.QUuid#toString() toString()} for an explanation of how the five hex fields map to the public data members in QUuid. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.core.QUuid#toString() toString()}, and QUuid(). <br></DD></DT>
*/

    public QUuid(java.lang.String arg__1){
        super((QPrivateConstructor)null);
        __qt_QUuid_String(arg__1);
    }

    native void __qt_QUuid_String(java.lang.String arg__1);

/**
Creates a UUID with the value specified by the parameters, <tt>l</tt>, <tt>w1</tt>, <tt>w2</tt>, <tt>b1</tt>, <tt>b2</tt>, <tt>b3</tt>, <tt>b4</tt>, <tt>b5</tt>, <tt>b6</tt>, <tt>b7</tt>, <tt>b8</tt>. <p>Example:<br><pre class="snippet">
Missing snippet: doc/src/snippets/code/src_corelib_plugin.quuid.cpp.</pre><br></pre>
*/

    public QUuid(int l, char w1, char w2, byte b1, byte b2, byte b3, byte b4, byte b5, byte b6, byte b7, byte b8){
        super((QPrivateConstructor)null);
        __qt_QUuid_int_char_char_byte_byte_byte_byte_byte_byte_byte_byte(l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8);
    }

    native void __qt_QUuid_int_char_char_byte_byte_byte_byte_byte_byte_byte_byte(int l, char w1, char w2, byte b1, byte b2, byte b3, byte b4, byte b5, byte b6, byte b7, byte b8);

/**
Returns true if this is the null UUID {00000000-0000-0000-0000-000000000000}; otherwise returns false.
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
    private final boolean operator_less(com.trolltech.qt.core.QUuid other)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_operator_less_QUuid(nativeId(), other == null ? 0 : other.nativeId());
    }
    @QtBlockedSlot
    native boolean __qt_operator_less_QUuid(long __this__nativeId, long other);

/**
<brief>Writes thisQUuid
*/

    @QtBlockedSlot
    public final void writeTo(com.trolltech.qt.core.QDataStream arg__1)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_writeTo_QDataStream(nativeId(), arg__1 == null ? 0 : arg__1.nativeId());
    }
    @QtBlockedSlot
    native void __qt_writeTo_QDataStream(long __this__nativeId, long arg__1);

    @QtBlockedSlot
    private final boolean operator_equal(com.trolltech.qt.core.QUuid orig)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_operator_equal_QUuid(nativeId(), orig == null ? 0 : orig.nativeId());
    }
    @QtBlockedSlot
    native boolean __qt_operator_equal_QUuid(long __this__nativeId, long orig);

/**
<brief>Reads a QUuid
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
<brief>Returns a string representation of the <tt>this QUuid</tt>. </brief>
*/

    @QtBlockedSlot
    public final java.lang.String toString()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_toString(nativeId());
    }
    @QtBlockedSlot
    native java.lang.String __qt_toString(long __this__nativeId);

/**
Returns the value in the variant field of the UUID. If the return value is {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE }, call {@link com.trolltech.qt.core.QUuid#version() version()} to see which layout it uses. The null UUID is considered to be of an unknown variant. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.core.QUuid#version() version()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final com.trolltech.qt.core.QUuid.Variant variant()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return com.trolltech.qt.core.QUuid.Variant.resolve(__qt_variant(nativeId()));
    }
    @QtBlockedSlot
    native int __qt_variant(long __this__nativeId);

/**
Returns the version field of the UUID, if the UUID's variant field is {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE }. Otherwise it returns {@link com.trolltech.qt.core.QUuid.Version QUuid::VerUnknown }. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.core.QUuid#variant() variant()}. <br></DD></DT>
*/

    @QtBlockedSlot
    public final com.trolltech.qt.core.QUuid.Version version()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return com.trolltech.qt.core.QUuid.Version.resolve(__qt_version(nativeId()));
    }
    @QtBlockedSlot
    native int __qt_version(long __this__nativeId);

/**
On any platform other than Windows, this function returns a new UUID with variant {@link com.trolltech.qt.core.QUuid.Variant QUuid::DCE } and version {@link com.trolltech.qt.core.QUuid.Version QUuid::Random }. The random numbers used to construct the UUID are obtained from the local pseudo-random generator, which is usually not a cryptographic quality random number generator. Therefore, a UUID generated by this function can't be guaranteed to be unique. <p>On a Windows platform, a GUID is generated, which almost certainly will be unique, on this or any other system, networked or not. <p><DT><b>See also:</b><br><DD>{@link com.trolltech.qt.core.QUuid#variant() variant()}, and {@link com.trolltech.qt.core.QUuid#version() version()}. <br></DD></DT>
*/

    public native static com.trolltech.qt.core.QUuid createUuid();

/**
Sets the data of this QUuid to <tt>data1</tt>.
*/

    @QtBlockedSlot
    public final void setData1(int data1)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setData1_int(nativeId(), data1);
    }
    @QtBlockedSlot
    native void __qt_setData1_int(long __this__nativeId, int data1);

/**
Returns the data of this QUuid.
*/

    @QtBlockedSlot
    public final int data1()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_data1(nativeId());
    }
    @QtBlockedSlot
    native int __qt_data1(long __this__nativeId);

/**
Sets the data of this QUuid to <tt>data2</tt>.
*/

    @QtBlockedSlot
    public final void setData2(char data2)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setData2_char(nativeId(), data2);
    }
    @QtBlockedSlot
    native void __qt_setData2_char(long __this__nativeId, char data2);

/**
Returns the data of this QUuid.
*/

    @QtBlockedSlot
    public final char data2()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_data2(nativeId());
    }
    @QtBlockedSlot
    native char __qt_data2(long __this__nativeId);

/**
Sets the data of this QUuid to <tt>data3</tt>.
*/

    @QtBlockedSlot
    public final void setData3(char data3)    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setData3_char(nativeId(), data3);
    }
    @QtBlockedSlot
    native void __qt_setData3_char(long __this__nativeId, char data3);

/**
Returns the data of this QUuid.
*/

    @QtBlockedSlot
    public final char data3()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_data3(nativeId());
    }
    @QtBlockedSlot
    native char __qt_data3(long __this__nativeId);

/**

*/

    @QtBlockedSlot
    public final void setData4(byte[] data4)    {
        if (data4.length != 8)
            throw new IllegalArgumentException("Wrong number of elements in array. Found: " + data4.length + ", expected: 8");

        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        __qt_setData4_byte_3(nativeId(), data4);
    }
    @QtBlockedSlot
    native void __qt_setData4_byte_3(long __this__nativeId, byte[] data4);

    @QtBlockedSlot
    public final byte[] data4()    {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_data4(nativeId());
    }
    @QtBlockedSlot
    native byte[] __qt_data4(long __this__nativeId);

/**
@exclude
*/

    public static native QUuid fromNativePointer(QNativePointer nativePointer);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    protected QUuid(QPrivateConstructor p) { super(p); } 

/**
@exclude
*/

    public static native QNativePointer nativePointerArray(QUuid array[]);

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object other) {
    if (other instanceof com.trolltech.qt.core.QUuid)
        return operator_equal((com.trolltech.qt.core.QUuid) other);
        return false;
    }

/**
This method is internal to Qt Jambi. 

	@exclude
*/

    public int compareTo(Object other) {
        if (equals(other)) return 0;
        else if (other instanceof com.trolltech.qt.core.QUuid) {
            if (operator_less((com.trolltech.qt.core.QUuid) other)) return -1;
            else return 1;
        }
        throw new ClassCastException();
    }

/**
This method is reimplemented for internal reasons
*/

    @Override
    public QUuid clone() {
        if (nativeId() == 0)
            throw new QNoNativeResourcesException("Function call on incomplete object of type: " +getClass().getName());
        return __qt_clone(nativeId());
    }
    native QUuid __qt_clone(long __this_nativeId);
}
