/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ForwardingMap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;

/**
 * The {@code ExtendedProperties} class represents a persistent set of
 * properties. This class is similar to {@link java.util.Properties}
 * but it supports property grouping and the values in the property list
 * are immutable.
 */
public class ExtendedProperties implements Serializable
{
    private static final long serialVersionUID = -1609979648378396045L;

    /**
     * The map that represents a property list.
     */
    public static final class PropertyMap extends ForwardingMap<String, String>
        implements Serializable
    {
        private static final long serialVersionUID = 134305430937523343L;

        private final ImmutableMap<String, String> delegate;
        private final PropertyMap defaults;

        PropertyMap(ImmutableMap<String, String> delegate, PropertyMap defaults) {
            this.delegate = delegate;
            this.defaults = defaults;
        }

        @Override
        protected ImmutableMap<String,String> delegate() {
            return delegate;
        }

        @Override
        public boolean containsKey(Object key) {
            return delegate.containsKey(key) || (defaults != null && defaults.containsKey(key));
        }

        @Override
        public String get(Object key) {
            String val = delegate.get(key);
            return (val == null && defaults != null) ? defaults.get(key) : val;
        }

        public Optional<String> getOptional(Object key) {
            return Optional.ofNullable(get(key));
        }

        public String get(String key, String deflt) {
            return getOptional(key).orElse(deflt);
        }

        public Optional<Boolean> getBool(String key) {
            return getOptional(key).map(Boolean::valueOf);
        }

        public boolean getBool(String key, boolean deflt) {
            return getBool(key).orElse(deflt);
        }

        public Optional<Integer> getInt(String key) {
            return getOptional(key).flatMap(Optionals.of(Integer::parseInt));
        }

        public int getInt(String key, int deflt) {
            return getInt(key).orElse(deflt);
        }
    }

    private static final PropertyMap EMPTY = new PropertyMap(ImmutableMap.of(), null);

    /**
     * The global property list.
     */
    private PropertyMap global = EMPTY;

    /**
     * Property lists grouped by categories.
     */
    private ImmutableMap<String, PropertyMap> categories = ImmutableMap.of();

    /**
     * A property list that contains default values for any keys not
     * found in this property list.
     */
    private ExtendedProperties defaults;

    /**
     * Creates an empty property list with no default values.
     */
    public ExtendedProperties() {
        this(null);
    }

    /**
     * Creates an empty property list with the specified defaults.
     *
     * @param defaults the defaults
     */
    public ExtendedProperties(ExtendedProperties defaults) {
        this.defaults = defaults;
    }

    /**
     * Create property list populated from given table.
     *
     * @param global the global property mapping
     * @param categories the table contains (category, key, value) tuple
     * @param defaults the defaults
     */
    public ExtendedProperties(Map<String,String> global,
                              Table<String,String,String> categories,
                              ExtendedProperties defaults) {
        this.global = new PropertyMap(ImmutableMap.copyOf(global), defaults != null ? defaults.global : null);

        if (defaults == null) {
            this.categories = categories.rowKeySet()
                .stream().collect(MoreCollectors.toImmutableMap(
                    Function.identity(),
                    cat -> new PropertyMap(ImmutableMap.copyOf(categories.row(cat)), null)));
        } else {
            this.categories = Sets.union(categories.rowKeySet(), defaults.categories.keySet())
                .stream().collect(MoreCollectors.toImmutableMap(
                    Function.identity(),
                    cat -> new PropertyMap(ImmutableMap.copyOf(categories.row(cat)),
                                           defaults.categories.get(cat))));
        }
    }

    /**
     * Returns the global property list.
     *
     * @return the global property list
     */
    public PropertyMap global() {
        return global;
    }

    /**
     * Returns a property list for the given category.
     *
     * @param name the category name
     * @return the property list associated to the given category
     */
    public PropertyMap category(String name) {
        return Optionals.firstNonNull(categories.get(name), EMPTY);
    }

    /**
     * Returns all property list grouped by categories.
     *
     * @return map of category name to property list
     */
    public Map<String,PropertyMap> categories() {
        return categories;
    }

    /**
     * Returns the property with the specified key in the global property list.
     * If the key is not found in the global property list, the default property
     * list, and its defaults, recursively, are then checked. The method returns
     * {@code Optional.empty()} if the property is not found.
     *
     * @param key the property key
     * @return the value in the global property list with the specified key value
     */
    public Optional<String> getOptionalProperty(String key) {
        return global.getOptional(key);
    }

    /**
     * Returns the property with the specified key in the global property list.
     * If the key is not found in the global property list, the default property
     * list, and its defaults, recursively, are then checked. The method returns
     * default value argument if the property is not found.
     *
     * @param key the property key
     * @param deflt a default value.
     * @return the value in the global property list with the specified key value
     */
    public String getProperty(String key, String deflt) {
        return global.get(key, deflt);
    }

    /**
     * Returns the property with the specified key in the given category.
     * If the key is not found in the category property list, the default property
     * list, and its defaults, recursively, are then checked. The method returns
     * {@code Optional.empty()} if the property is not found.
     *
     * @param category the category name
     * @param key the property key
     * @return the value in the category property list with the specified key value
     */
    public Optional<String> getOptionalProperty(String category, String key) {
        return category(category).getOptional(key);
    }

    /**
     * Returns the property with the specified key in the given category.
     * If the key is not found in the category property list, the default property
     * list, and its defaults, recursively, are then checked. The method returns
     * the default value argument if the property is not found.
     *
     * @param category the category name
     * @param key the property key
     * @param deflt a default value
     * @return the value in the category property list with the specified key value
     */
    public String getProperty(String category, String key, String deflt) {
        return category(category).get(key, deflt);
    }

    /**
     * Search the property in the global property list and convert the resulting
     * {@code String} value to the corresponding {@code boolean} value. The method
     * returns the default value argument if the property is not found.
     *
     * @param key the property key
     * @param deflt the default value
     * @return the value in the global property list with the specified key value
     */
    public boolean getBooleanProperty(String key, boolean deflt) {
        return global.getBool(key, deflt);
    }

    /**
     * Search the property in the category property list and convert the resulting
     * {@code String} value to the corresponding {@code boolean} value. The method
     * returns the default value argument if the property is not found.
     *
     * @param category the category name
     * @param key the property key
     * @param deflt the default value
     * @return the value in the category property list with the specified key value
     */
    public boolean getBooleanProperty(String category, String key, boolean deflt) {
        return category(category).getBool(key, deflt);
    }

    /**
     * Search the property in the global property list and convert the resulting
     * {@code String} value to the corresponding {@code int} value. The method
     * returns the default value argument if the property is not found.
     *
     * @param key the property key
     * @param deflt the default value
     * @return the value in the global property list with the specified key value
     */
    public int getIntProperty(String key, int deflt) {
        return global.getInt(key, deflt);
    }

    /**
     * Search the property in the category property list and convert the resulting
     * {@code String} value to the corresponding {@code int} value. The method
     * returns the default value argument if the property is not found.
     *
     * @param category the category name
     * @param key the property key
     * @param deflt the default value
     * @return the value in the category property list with the specified key value
     */
    public int getIntProperty(String category, String key, int deflt) {
        return category(category).getInt(key, deflt);
    }

    /**
     * Reads a property list (category, key and element tuples) from the input
     * character stream in a simple line-oriented format.
     *
     * @param reader the input character stream
     * @throws IOException if an error occurred when reading from the input stream
     */
    public void load(Reader reader) throws IOException {
        new LoadingProperties().load(reader, this);
    }

    /**
     * Reads a property list (category, key and element tuples) from the input
     * byte stream. The input stream is in a simple line-oriented format and is
     * assumed to use the ISO 8859-1 character encoding.
     *
     * @param stream the input stream
     * @throws IOException if an error occurred when reading from the input stream
     */
    public void load(InputStream stream) throws IOException {
        new LoadingProperties().load(stream, this);
    }

    @SuppressWarnings("serial")
    static class LoadingProperties extends Properties {
        private final ImmutableMap.Builder<String,String> global = ImmutableMap.builder();
        private final Map<String, ImmutableMap.Builder<String,String>> categories = new HashMap<>();
        private ImmutableMap.Builder<String,String> current = global;

        /**
         * This method is overriding to handle property grouping.
         */
        @Override
        public Object put(Object key, Object value) {
            if (key instanceof String && value instanceof String) {
                String skey = (String)key;
                if (skey.startsWith("[") && skey.endsWith("]")) {
                    skey = skey.substring(1, skey.length()-1);
                    current = categories.computeIfAbsent(skey, x -> ImmutableMap.builder());
                } else {
                    current.put((String)key, unquote((String)value));
                }
            }
            return null;
        }

        private static final Pattern QUOTED_STRING = Pattern.compile("\"(.*)\"\\s*(#.*)?");

        static String unquote(String val) {
            Matcher m = QUOTED_STRING.matcher(val);
            if (m.matches()) {
                val = m.group(1);
            } else {
                int i = val.indexOf('#');
                if (i != -1) {
                    val = val.substring(0, i).trim();
                }
            }
            return val;
        }

        void load(Reader from, ExtendedProperties to) throws IOException {
            super.load(from); populate(to);
        }

        void load(InputStream from, ExtendedProperties to) throws IOException {
            super.load(from); populate(to);
        }

        private void populate(ExtendedProperties to) {
            to.global = new PropertyMap(this.global.build(), to.defaults != null ? to.defaults.global : null);

            if (to.defaults == null) {
                to.categories = this.categories.entrySet().stream().collect(
                    MoreCollectors.toImmutableMap(
                        Map.Entry::getKey,
                        e -> new PropertyMap(e.getValue().build(), null))
                );
            } else {
                for (String c : to.defaults.categories.keySet()) {
                    this.categories.computeIfAbsent(c, x -> ImmutableMap.builder());
                }
                to.categories = this.categories.entrySet().stream().collect(
                    MoreCollectors.toImmutableMap(
                        Map.Entry::getKey,
                        e -> new PropertyMap(e.getValue().build(), to.defaults.categories.get(e.getKey())))
                );
            }
        }
    }

    public String toString() {
        return MoreObjects.toStringHelper(this)
            .add("global", global)
            .add("categories", categories)
            .add("defaults", defaults)
            .toString();
    }
}
