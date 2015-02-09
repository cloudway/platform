/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.os;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static java.util.Objects.requireNonNull;

/**
 * <p>The authorized_keys file contains public keys for public key authentication.
 * Each line of the file contains one key.</p>
 *
 * <p>The options (if present) consist of comma-separated option specifications.
 * No spaces are permitted, except within double quotes.</p>
 */
public final class AuthorizedKey implements Serializable
{
    private static final long serialVersionUID = -4123896732191837527L;

    private static final String ID_PREFIX = "CLOUDWAY-";
    private static final Pattern ID_PATTERN = Pattern.compile("\\A" + ID_PREFIX + "(.+)-(.+)\\Z");

    @SuppressWarnings("MethodOnlyUsedFromInnerClass")
    private static String composeId(String id, String name) {
        return ID_PREFIX + requireNonNull(id) + "-" + requireNonNull(name);
    }

    private static Optional<String> extractId(Optional<String> comment, int group) {
        return comment.map(ID_PATTERN::matcher).filter(Matcher::matches).map(m -> m.group(group));
    }

    private final String type;
    private final String bits;
    private final Optional<String> options;
    private final Optional<String> comment;

    private AuthorizedKey(String type, String bits, Optional<String> options, Optional<String> comment) {
        this.type = requireNonNull(type);
        this.bits = requireNonNull(bits);
        this.options = requireNonNull(options);
        this.comment = requireNonNull(comment);
    }

    public String getType() {
        return type;
    }

    public String getBits() {
        return bits;
    }

    public Optional<String> getOptions() {
        return options;
    }

    public Optional<String> getComment() {
        return comment;
    }

    public Optional<String> getId() {
        return extractId(comment, 1);
    }

    public Optional<String> getName() {
        return extractId(comment, 2);
    }

    public AuthorizedKey toPublicKey() {
        return new AuthorizedKey(type, bits, Optional.empty(), getName());
    }

    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof AuthorizedKey))
            return false;
        AuthorizedKey that = (AuthorizedKey)obj;
        return type.equals(that.type)
            && bits.equals(that.bits)
            && options.equals(that.options)
            && comment.equals(that.comment);
    }

    public int hashCode() {
        return Objects.hash(type, bits, options, comment);
    }

    public String toString() {
        StringBuilder b = new StringBuilder();
        options.ifPresent(o -> b.append(o).append(' '));
        b.append(type).append(' ').append(bits);
        comment.ifPresent(c -> b.append(' ').append(c));
        return b.toString();
    }

    public static Parser parser() {
        return new Parser();
    }

    public static AuthorizedKey parse(String key) {
        return parser().parse(key);
    }

    public static AuthorizedKey parsePublicKey(String key) {
        return parser().parsePublicKey(key);
    }

    public static class Parser {
        private Optional<String> options = Optional.empty();
        private Optional<String> comment = Optional.empty();

        public Parser withId(String id, String name) {
            return withComment(composeId(id, name));
        }

        public Parser withOptions(String options) {
            this.options = Optional.of(options);
            return this;
        }

        public Parser withComment(String comment) {
            this.comment = Optional.of(comment);
            return this;
        }

        AuthorizedKey parse(String key) {
            requireNonNull(key);

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
            }
            return parse(pubkey, Optional.ofNullable(options), Optional.empty());
        }

        public AuthorizedKey parsePublicKey(String key) {
            return parse(requireNonNull(key), this.options, this.comment);
        }

        private static AuthorizedKey parse(String key, Optional<String> options, Optional<String> comment) {
            String[] splits = key.trim().split("\\s+");
            if (splits.length != 2 && splits.length != 3)
                throw new IllegalArgumentException("Invalid SSH public key");

            String type = splits[0];
            String bits = splits[1];
            if (!comment.isPresent() && splits.length == 3)
                comment = Optional.of(splits[2]);
            return new AuthorizedKey(type, bits, options, comment);
        }
    }
}
