/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.util.Objects;

/**
 * Represents the source position.
 *
 * <p>This object is immutable, any mutable operation will return a fresh
 * new {@code SourcePos} object.</p>
 */
public class SourcePos implements Comparable<SourcePos> {
    /* Encodes and decodes source code positions, Source code positions
     * are internally represented as integers that contain both column
     * and line number information.
     */
    public static final int LINESHIFT  = 10;
    public static final int LINEINC    = (1 << LINESHIFT);
    public static final int COLUMNMASK = (1 << LINESHIFT) - 1;
    public static final int NOPOS      = 0;
    public static final int STARTPOS   = (1 << LINESHIFT) + 1;

    public static int line(int pos) {
        return pos >>> LINESHIFT;
    }

    public static int column(int pos) {
        return pos & COLUMNMASK;
    }

    public static int pos(int line, int col) {
        return (line << LINESHIFT) + col;
    }

    public static int nextline(int pos) {
        return (pos & ~COLUMNMASK) + LINEINC + 1;
    }

    public static int nexttab(int pos) {
        int line = line(pos);
        int column = column(pos);
        return pos(line, column + 8 - ((column - 1) % 8));
    }

    public static int nextchar(int pos, char c) {
        if (c == '\n') {
            return nextline(pos);
        } else if (c == '\t') {
            return nexttab(pos);
        } else {
            return pos + 1;
        }
    }

    public static int addDelta(int pos, int pos1, int pos2) {
        if (pos == pos1) {
            return pos2;
        } else if ((pos1 & ~COLUMNMASK) != (pos2 & ~COLUMNMASK)) {
            return (pos & ~COLUMNMASK) + pos2 - (pos1 & ~COLUMNMASK);
        } else {
            return pos + pos2 - pos1;
        }
    }

    private final String name;
    private final int pos;

    // Package private constructor
    SourcePos(String name, int pos) {
        this.name = name;
        this.pos = pos;
    }

    /**
     * Construct a source position.
     *
     * @param name the source name
     * @param line the line number in the source
     * @param column the column number in the source
     */
    public SourcePos(String name, int line, int column) {
        this(name, pos(line, column));
    }

    /**
     * Construct a source position with the given source name, and line
     * number and column number set to 1.
     */
    public SourcePos(String name) {
        this(name, STARTPOS);
    }

    /**
     * Returns the source name.
     */
    public String getName() {
        return name;
    }

    /**
     * Set the source name.
     */
    public SourcePos setName(String newName) {
        return new SourcePos(newName, pos);
    }

    /**
     * Returns the line number in the source.
     */
    public int getLine() {
        return line(pos);
    }

    /**
     * Set the line number of a source position.
     */
    public SourcePos setLine(int line) {
        return new SourcePos(name, line, 1);
    }

    /**
     * Returns the column number in the source.
     */
    public int getColumn() {
        return column(pos);
    }

    /**
     * Set the column number of a source position.
     */
    public SourcePos setColumn(int column) {
        return new SourcePos(name, (pos & ~COLUMNMASK) | column);
    }

    /**
     * Return the encoded source code position.
     */
    public int getPosition() {
        return pos;
    }

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof SourcePos))
            return false;
        SourcePos other = (SourcePos)obj;
        return Objects.equals(name, other.name) && pos == other.pos;
    }

    public int hashCode() {
        return Objects.hash(name, pos);
    }

    @Override
    public int compareTo(SourcePos other) {
        int c;
        if (this.name == null)
            return -1;
        if (other.name == null)
            return 1;
        if ((c = this.name.compareTo(other.name)) != 0)
            return c;
        return this.pos - other.pos;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (name != null && !name.isEmpty()) {
            sb.append('"').append(name).append("\" ");
        }
        if (pos != NOPOS) {
            sb.append("(line ").append(line(pos));
            sb.append(", column ").append(column(pos));
            sb.append(")");
        }
        return sb.toString();
    }
}
