/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.util.StringJoiner;

/**
 * SRFI-9 record type.
 */
public class Record implements LispVal {
    private static final int RECORD_TYPE = 0;
    private static final int RECORD_NAME = 1;

    private final LispVal[] fields;

    public Record(int size) {
        fields = new LispVal[size];
    }

    public LispVal ref(int i) {
        return fields[i];
    }

    public void set(int i, LispVal value) {
        fields[i] = value;
    }

    @Override
    public String show() {
        return "#" + getRecordName() + getRecordFields();
    }

    private String getRecordName() {
        return ((Record)fields[RECORD_TYPE]).fields[RECORD_NAME].show();
    }

    private String getRecordFields() {
        StringJoiner sj = new StringJoiner(" ", "(", ")");
        for (int i = 1; i < fields.length; i++) {
            sj.add(fields[i].show());
        }
        return sj.toString();
    }

    @Name("record?")
    public static boolean isRecord(LispVal x) {
        return x instanceof Record;
    }

    public static LispVal make_record(int size) {
        return new Record(size);
    }

    public static LispVal record_ref(LispVal rec, int i) {
        if (!(rec instanceof Record))
            throw new LispError.TypeMismatch("record", rec);
        return ((Record)rec).ref(i);
    }

    @Name("record-set!")
    public static void record_set(LispVal rec, int i, LispVal x) {
        if (!(rec instanceof Record))
            throw new LispError.TypeMismatch("record", rec);
        ((Record)rec).set(i, x);
    }
}
