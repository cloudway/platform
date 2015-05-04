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
    private final String name;
    private final int line;
    private final int column;

    /**
     * Construct a source position.
     *
     * @param name the source name
     * @param line the line number in the source
     * @param column the column number in the source
     */
    public SourcePos(String name, int line, int column) {
        this.name = name;
        this.line = line;
        this.column = column;
    }

    /**
     * Construct a source position with the given source name, and line
     * number and column number set to 1.
     */
    public SourcePos(String name) {
        this.name = name;
        this.line = 1;
        this.column = 1;
    }

    /**
     * Returns the source name.
     */
    public String getName() {
        return name;
    }

    /**
     * Set the name of the source.
     */
    public SourcePos setName(String newName) {
        return new SourcePos(newName, line, column);
    }

    /**
     * Returns the line number in the source.
     */
    public int getLine() {
        return line;
    }

    /**
     * Set the line number of a source position.
     */
    public SourcePos setLine(int newLine) {
        return new SourcePos(name, newLine, column);
    }

    /**
     * Returns the column number in the source.
     */
    public int getColumn() {
        return column;
    }

    /**
     * Set the column number of a source position.
     */
    public SourcePos setColumn(int newColumn) {
        return new SourcePos(name, line, newColumn);
    }

    /**
     * Update a source position given a character. If the character is a
     * newline ('\n') the line number is incremented by 1. If the character
     * is a tab ('\t') the column number is incremented to the nearest 8'th
     * column, ie. {@code column + 8 - ((column-1) % 8)}. In all other cases,
     * the column is incremented by 1.
     */
    public SourcePos updatePosChar(char c) {
        switch (c) {
        case '\n':
            return new SourcePos(name, line + 1, 1);
        case '\t':
            return new SourcePos(name, line, column + 8 - ((column - 1) % 8));
        default:
            return new SourcePos(name, line, column + 1);
        }
    }

    /**
     * Updates the source position by calling {@link #updatePosChar} on every
     * character in the given string.
     */
    public SourcePos updatePosString(String s) {
        int ln = this.line;
        int cn = this.column;
        int len = s.length();
        for (int i = 0; i < len; i++) {
            switch (s.charAt(i)) {
            case '\n': ln++; cn = 1;
                       break;
            case '\t': cn = cn + 8 - (cn - 1) % 8;
                       break;
            default:   cn++;
                       break;
            }
        }
        return new SourcePos(name, ln, cn);
    }

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof SourcePos))
            return false;
        SourcePos other = (SourcePos)obj;
        return Objects.equals(name, other.name)
            && line == other.line
            && column == other.column;
    }

    public int hashCode() {
        return Objects.hash(name, line, column);
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
        if (this.line != other.line)
            return this.line - other.line;
        if (this.column != other.column)
            return this.column - other.column;
        return 0;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (name != null && !name.isEmpty())
            sb.append('"').append(name).append("\" ");
        sb.append("(line ").append(line);
        sb.append(", column ").append(column);
        sb.append(")");
        return sb.toString();
    }
}
