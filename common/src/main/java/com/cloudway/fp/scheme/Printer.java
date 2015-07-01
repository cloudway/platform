/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.IOException;
import java.util.ArrayList;
import java.util.IdentityHashMap;

import com.cloudway.fp.data.Maybe;
import com.cloudway.fp.io.IO;
import com.cloudway.fp.io.IOConsumer;

public class Printer implements OutputPort {
    private static class RefID {
        int ref, pos;
        boolean active;

        RefID(int ref, int pos, boolean active) {
            this.ref = ref;
            this.pos = pos;
            this.active = active;
        }
    }

    private final IdentityHashMap<LispVal, RefID> references = new IdentityHashMap<>();
    private final ArrayList<String> buffer = new ArrayList<>();
    private int nextRefId;
    private boolean display;
    private LispError error;

    public void addReference(LispVal ref) {
        RefID refid = references.get(ref);
        if (refid == null) {
            references.put(ref, new RefID(-1, buffer.size(), true));
        } else {
            refid.active = true;
        }
    }

    public void removeReference(LispVal ref) {
        RefID refid = references.get(ref);
        if (refid != null) {
            refid.active = false;
        }
    }

    public boolean isReference(LispVal val) {
        RefID refid = references.get(val);
        return refid != null && refid.active;
    }

    public boolean isDisplay() {
        return display;
    }

    public void setDisplay(boolean display) {
        this.display = display;
    }

    public void setError(LispError err) {
        error = err;
    }

    public Maybe<LispError> checkError() {
        return Maybe.ofNullable(error);
    }

    public void add(LispVal val) {
        if (error != null) {
            return;
        }

        if (display) {
            if (val instanceof Text) {
                add(((Text)val).value());
                return;
            }
            if (val instanceof Char) {
                add(String.valueOf(((Char)val).value));
                return;
            }
        }

        RefID refid = references.get(val);
        if (refid != null && refid.active) {
            if (refid.ref == -1)
                refid.ref = nextRefId++;
            add("#" + refid.ref + "#");
        } else {
            val.show(this);
        }
    }

    public void add(String literal) {
        if (error == null) {
            buffer.add(literal);
        }
    }

    @Override
    public void write(int c) {
        add(String.valueOf(c));
    }

    @Override
    public void write(char[] cbuf) {
        add(new String(cbuf));
    }

    @Override
    public void write(String s) {
        add(s);
    }

    @Override
    public void flush() {
        // no-op
    }

    @Override
    public void close() {
        // no-op
    }

    public void print(IOConsumer<String> out) throws IOException {
        backfill();
        IO.forEach(buffer, out);
    }

    public String toString() {
        backfill();
        StringBuilder out = new StringBuilder();
        buffer.forEach(out::append);
        return out.toString();
    }

    private void backfill() {
        for (RefID refid : references.values()) {
            if (refid.ref != -1) {
                String s = buffer.get(refid.pos);
                buffer.set(refid.pos, "#" + refid.ref + "=" + s);
            }
        }
    }
}
