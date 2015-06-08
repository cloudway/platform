/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

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
    public void show(Printer pr) {
        pr.addReference(this);
        pr.add("#");
        addRecordName(pr);
        pr.add("(");
        addRecordFields(pr);
        pr.add(")");
    }

    private void addRecordName(Printer pr) {
        pr.add(((Record)fields[RECORD_TYPE]).fields[RECORD_NAME]);
    }

    private void addRecordFields(Printer pr) {
        for (int i = 1; i < fields.length; i++) {
            if (i != 1)
                pr.add(" ");
            pr.add(fields[i]);
        }
    }

    @Name("record?")
    @SuppressWarnings("unused")
    public static boolean isRecord(LispVal x) {
        return x instanceof Record;
    }

    @SuppressWarnings("unused")
    public static LispVal make_record(int size) {
        return new Record(size);
    }

    @SuppressWarnings("unused")
    public static LispVal record_ref(LispVal rec, int i) {
        if (!(rec instanceof Record))
            throw new LispError.TypeMismatch("record", rec);
        return ((Record)rec).ref(i);
    }

    @Name("record-set!")
    @SuppressWarnings("unused")
    public static void record_set(LispVal rec, int i, LispVal x) {
        if (!(rec instanceof Record))
            throw new LispError.TypeMismatch("record", rec);
        ((Record)rec).set(i, x);
    }
}
