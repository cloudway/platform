/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.math.BigInteger;

import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.scheme.numsys.Num;
import static com.cloudway.fp.scheme.LispVal.*;

/**
 * Convert Java object to Scheme object.
 */
@FunctionalInterface
interface Packer {
    LispVal apply(Object obj);

    static Packer get(Class<?> type) {
        return Impl.packers.findFirst(t -> t.first().isAssignableFrom(type)).orElseThrow(
            () -> new LispError("Unrecognized java type: " + type.getName())).second();
    }

    static LispVal pack(Object obj) {
        if (obj == null)
            return VOID;

        if (obj instanceof LispVal)
            return (LispVal)obj;

        return get(obj.getClass()).apply(obj);
    }

    final class Impl {
        private Impl() {}

        static final Packer OBJECT_PACKER = Impl::packObject;

        @SuppressWarnings("unchecked")
        static final Seq<Tuple<Class<?>, Packer>> packers = Seq.of(
            Tuple.of(Void.TYPE ,       obj -> VOID),
            Tuple.of(String.class,     obj -> new CText((String)obj)),
            Tuple.of(Boolean.TYPE,     obj -> Bool.valueOf((Boolean)obj)),
            Tuple.of(Boolean.class,    obj -> Bool.valueOf((Boolean)obj)),
            Tuple.of(Character.TYPE,   obj -> new Char((Character)obj)),
            Tuple.of(Character.class,  obj -> new Char((Character)obj)),
            Tuple.of(Byte.TYPE,        obj -> Num.make((int)(Byte)obj)),
            Tuple.of(Byte.class,       obj -> Num.make((int)(Byte)obj)),
            Tuple.of(Short.TYPE,       obj -> Num.make((Short)obj)),
            Tuple.of(Short.class,      obj -> Num.make((Short)obj)),
            Tuple.of(Integer.TYPE,     obj -> Num.make((Integer)obj)),
            Tuple.of(Integer.class,    obj -> Num.make((Integer)obj)),
            Tuple.of(Long.TYPE,        obj -> Num.make((Long)obj)),
            Tuple.of(Long.class,       obj -> Num.make((Long)obj)),
            Tuple.of(Float.TYPE,       obj -> Num.make((double)(Float)obj)),
            Tuple.of(Float.class,      obj -> Num.make((double)(Float)obj)),
            Tuple.of(Double.TYPE,      obj -> Num.make((Double)obj)),
            Tuple.of(Double.class,     obj -> Num.make((Double)obj)),
            Tuple.of(BigInteger.class, obj -> Num.make((BigInteger)obj)),
            Tuple.of(LispVal.class,    obj -> (LispVal)obj),
            Tuple.of(Object.class,     OBJECT_PACKER)
        );

        static LispVal packObject(Object obj) {
            Packer packer = get(obj.getClass());
            if (packer != OBJECT_PACKER) {
                return packer.apply(obj);
            } else {
                return new JObject(obj);
            }
        }
    }
}
