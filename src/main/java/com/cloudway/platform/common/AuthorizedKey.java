/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common;

public class AuthorizedKey
{
    public static final String ID_PREFIX = "CLOUDWAY-";

    private String type;
    private String bits;
    private String options;
    private String comment;

    public AuthorizedKey(String type, String bits, String options, String comment) {
        this.type = type;
        this.bits = bits;
        this.options = options;
        this.comment = comment;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getBits() {
        return bits;
    }

    public void setBits(String bits) {
        this.bits = bits;
    }

    public String getOptions() {
        return options;
    }

    public void setOptions(String options) {
        this.options = options;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public String getId(String uuid) {
        String prefix = ID_PREFIX + uuid + "-";
        if (comment == null || !comment.startsWith(prefix)) {
            return null;
        } else {
            return comment.substring(prefix.length());
        }
    }

    public void setId(String uuid, String id) {
        this.comment = ID_PREFIX + uuid + "-" + id;
    }

    private static AuthorizedKey parsePublicKey(String key, String options) {
        String[] splits = key.trim().split("\\s+");
        if (splits.length != 2 && splits.length != 3)
            throw new IllegalArgumentException("Invalid SSH public key");

        String type = splits[0];
        String bits = splits[1];
        String comment = splits.length == 3 ? splits[2] : null;
        return new AuthorizedKey(type, bits, options, comment);
    }

    public static AuthorizedKey parsePublicKey(String key) {
        return parsePublicKey(key, null);
    }

    public static AuthorizedKey parse(String key) {
        // The authorized key may contains quoted strings in options
        String options = null;
        String pubkey = null;
        int len = key.length();
        boolean quoted = false;
        for (int i = 0; i < len; i++) {
            char c = key.charAt(i);
            if (c == ' ' && !quoted) {
                options = key.substring(0, i);
                pubkey = key.substring(i+1);
                break;
            } else if (c == '\"') {
                quoted = !quoted;
            }
        }

        if (pubkey == null) {
            throw new IllegalArgumentException("Invalid authorized key");
        } else {
            return parsePublicKey(pubkey, options);
        }
    }

    public String toString() {
        StringBuilder b = new StringBuilder();
        if (options != null)
            b.append(options).append(' ');
        b.append(type).append(' ').append(bits);
        if (comment != null)
            b.append(' ').append(comment);
        return b.toString();
    }
}
