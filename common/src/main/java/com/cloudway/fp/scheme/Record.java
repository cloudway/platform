/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

/**
 * SRFI-9 record type.
 */
@SuppressWarnings("unused")
public class Record implements LispVal {
    private static final int RECORD_TYPE = 0;
    private static final int RECORD_NAME = 1;

    private final LispVal[] fields;

    Record(int size) {
        fields = new LispVal[size];
    }

    LispVal ref(int i) {
        return fields[i];
    }

    void set(int i, LispVal value) {
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
        pr.removeReference(this);
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

    public static LispVal make_record(int size) {
        return new Record(size);
    }

    @Name("record?")
    public static boolean isRecord(LispVal x) {
        return x instanceof Record;
    }

    public static LispVal record_ref(Record rec, int i) {
        return rec.ref(i);
    }

    @Name("record-set!")
    public static void record_set(Record rec, int i, LispVal x) {
        rec.set(i, x);
    }
}
