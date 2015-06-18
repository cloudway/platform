/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.math.BigInteger;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;
import com.cloudway.fp.scheme.LispError.TypeMismatch;
import static com.cloudway.fp.scheme.LispVal.*;
import static com.cloudway.fp.data.Either.left;
import static com.cloudway.fp.data.Either.right;
import com.cloudway.fp.scheme.numsys.BigInt;
import com.cloudway.fp.scheme.numsys.Field;
import com.cloudway.fp.scheme.numsys.Int16;
import com.cloudway.fp.scheme.numsys.Int32;
import com.cloudway.fp.scheme.numsys.Int64;
import com.cloudway.fp.scheme.numsys.Num;
import com.cloudway.fp.scheme.numsys.Real;

/**
 * Convert Scheme object to Java object.
 */
@FunctionalInterface
interface Unpacker {
    Either<LispError, Object> apply(Class<?> type, LispVal val);
    
    static Unpacker get(Class<?> type) {
        return Impl.unpackers.findFirst(t -> t.first().isAssignableFrom(type)).orElseThrow(
            () -> new LispError("Unrecognized java type: " + type.getName())).second();
    }

    static Either<LispError, Object> unpack(Class<?> type, LispVal value) {
        return get(type).apply(type, value);
    }

    final class Impl {
        private Impl() {}
        
        static final Seq<Tuple<Class<?>, Unpacker>> unpackers = Seq.of(
            Tuple.of(Void.TYPE,          Impl::unpackVoid),
            Tuple.of(CharSequence.class, Impl::unpackString),
            Tuple.of(Boolean.class,      Impl::unpackBoolean),
            Tuple.of(Boolean.TYPE,       Impl::unpackBoolean),
            Tuple.of(Character.class,    Impl::unpackChar),
            Tuple.of(Character.TYPE,     Impl::unpackChar),
            Tuple.of(Byte.class,         Impl::unpackByte),
            Tuple.of(Byte.TYPE,          Impl::unpackByte),
            Tuple.of(Short.class,        Impl::unpackShort),
            Tuple.of(Short.TYPE,         Impl::unpackShort),
            Tuple.of(Integer.class,      Impl::unpackInt),
            Tuple.of(Integer.TYPE,       Impl::unpackInt),
            Tuple.of(Long.class,         Impl::unpackLong),
            Tuple.of(Long.TYPE,          Impl::unpackLong),
            Tuple.of(Float.class,        Impl::unpackFloat),
            Tuple.of(Float.TYPE,         Impl::unpackFloat),
            Tuple.of(Double.class,       Impl::unpackDouble),
            Tuple.of(Double.TYPE,        Impl::unpackDouble),
            Tuple.of(BigInteger.class,   Impl::unpackBigInteger),
            Tuple.of(LispVal.class,      Impl::unpackLispVal),
            Tuple.of(Class.class,        Impl::unpackClass),
            Tuple.of(Object.class,       Impl::unpackObject)
        );

        static Either<LispError, Object> unpackVoid(Class<?> type, LispVal val) {
            return right(null);
        }
    
        static Either<LispError, Object> unpackBoolean(Class<?> type, LispVal val) {
            return val instanceof Bool
                ? right(((Bool)val).value)
                : left(new TypeMismatch("boolean", val));
        }
    
        static Either<LispError, Object> unpackChar(Class<?> type, LispVal val) {
            return val instanceof Char
                ? right(((Char)val).value)
                : left(new TypeMismatch("char", val));
        }
    
        static Either<LispError, Object> unpackString(Class<?> type, LispVal val) {
            return val instanceof Text
                ? right(((Text)val).value())
                : left(new TypeMismatch("string", val));
        }
    
        static Either<LispError, Object> unpackByte(Class<?> type, LispVal val) {
            if (val instanceof Num) {
                Num n = ((Num)val).lower();
                if (n instanceof Int16) {
                    short i = ((Int16)n).value;
                    if ((byte)i == i)
                        return right((byte)i);
                }
            }
            return left(new TypeMismatch("8 bit integer", val));
        }
    
        static Either<LispError, Object> unpackShort(Class<?> type, LispVal val) {
            if (val instanceof Num) {
                Num n = ((Num)val).lower();
                if (n instanceof Int16) {
                    return right(((Int16)n).value);
                }
            }
            return left(new TypeMismatch("16 bit integer", val));
        }
    
        static Either<LispError, Object> unpackInt(Class<?> type, LispVal val) {
            if (val instanceof Int32) {
                return right(((Int32)val).value);
            }
    
            if (val instanceof Num) {
                Int32 n = Field.raise(((Num)val).lower(), Int32.TAG);
                if (n != null) {
                    return right(n.value);
                }
            }
    
            return left(new TypeMismatch("integer", val));
        }
    
        static Either<LispError, Object> unpackLong(Class<?> type, LispVal val) {
            if (val instanceof Int64) {
                return right(((Int64)val).value);
            }
    
            if (val instanceof Num) {
                Int64 n = Field.raise(((Num)val).lower(), Int64.TAG);
                if (n != null) {
                    return right(n.value);
                }
            }
    
            return left(new TypeMismatch("integer", val));
        }
    
        @SuppressWarnings("FloatingPointEquality")
        static Either<LispError, Object> unpackFloat(Class<?> type, LispVal val) {
            if (val instanceof Num) {
                Real n = Field.raise(((Num)val).lower(), Real.TAG);
                if (n != null) {
                    return right((float)n.value);
                }
            }
            return left(new TypeMismatch("32 bit real", val));
        }
    
        static Either<LispError, Object> unpackDouble(Class<?> type, LispVal val) {
            if (val instanceof Real) {
                return right(((Real)val).value);
            }
    
            if (val instanceof Num) {
                Real n = Field.raise(((Num)val).lower(), Real.TAG);
                if (n != null) {
                    return right(n.value);
                }
            }
    
            return left(new TypeMismatch("real", val));
        }
    
        static Either<LispError, Object> unpackBigInteger(Class<?> type, LispVal val) {
            if (val instanceof Num) {
                BigInt n = Field.raise(((Num)val).lower(), BigInt.TAG);
                if (n != null)
                    return right(n.value);
            }
            return left(new TypeMismatch("integer", val));
        }
    
        static Either<LispError, Object> unpackLispVal(Class<?> type, LispVal val) {
            if (type.isInstance(val)) {
                return right(val);
            } else {
                return left(new TypeMismatch(type.getSimpleName().toLowerCase(), val));
            }
        }
    
        static Either<LispError, Object> unpackClass(Class<?> type, LispVal val) {
            return val instanceof JClass
                ? right(((JClass)val).value)
                : left(new TypeMismatch("jclass", val));
        }
    
        static Either<LispError, Object> unpackObject(Class<?> type, LispVal val) {
            if (val == VOID)
                return right(null);

            if (type == Object.class)
                return right(val.getObject());

            if (val instanceof JObject) {
                Object obj = ((JObject)val).value;
                if (type.isInstance(obj)) {
                    return right(obj);
                }
            }
    
            return left(new TypeMismatch(type.getName(), val));
        }
    }
}
