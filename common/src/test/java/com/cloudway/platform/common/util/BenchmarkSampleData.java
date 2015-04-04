/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.platform.common.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.primitives.Ints;

public class BenchmarkSampleData {
    private final boolean isUserTypeFast;
    private final SpecialRandom random;
    private final double hitRate;
    private final int size;

    private final Set<Element> values;
    private final Element[] queries;

    BenchmarkSampleData(boolean isUserTypeFast,
                        SpecialRandom random,
                        double hitRate,
                        int size) {
        this.isUserTypeFast = isUserTypeFast;
        this.random = random;
        this.hitRate = hitRate;
        this.size = size;

        this.values = createData();
        this.queries = createQueries(values, 1024);
    }

    Set<Element> getValues() {
        return values;
    }

    Element[] getQueries() {
        return queries;
    }

    private Set<Element> createData() {
        Set<Element> set = new HashSet<>(size);
        while (set.size() < size) {
            set.add(newElement());
        }
        return set;
    }

    private Element[] createQueries(Set<Element> elementsInUse, int numQueries) {
        List<Element> queryList = new ArrayList<>(numQueries);

        int numGoodQueries = (int)(numQueries * hitRate + 0.5);

        // add good queries
        int size = elementsInUse.size();
        if (size > 0) {
            int minCopiesOfEachGoodQuery = numGoodQueries / size;
            int extras = numGoodQueries % size;

            for (int i = 0; i < minCopiesOfEachGoodQuery; i++) {
                queryList.addAll(elementsInUse);
            }

            List<Element> tmp = new ArrayList<>(elementsInUse);
            Collections.shuffle(tmp, random);
            queryList.addAll(tmp.subList(0, extras));
        }

        // now add bad queries
        while (queryList.size() < numQueries) {
            Element candidate = newElement();
            if (!elementsInUse.contains(candidate)) {
                queryList.add(candidate);
            }
        }

        Collections.shuffle(queryList, random);
        return queryList.toArray(new Element[queryList.size()]);
    }

    private Element newElement() {
        int value = random.nextInt();
        return isUserTypeFast
            ? new Element(value)
            : new SlowElement(value);
    }

    static class Element implements Comparable<Element> {
        final int hash;
        Element(int hash) {
            this.hash = hash;
        }
        @Override public boolean equals(Object obj) {
            return this == obj || (obj instanceof Element && ((Element)obj).hash == hash);
        }
        @Override public int hashCode() {
            return hash;
        }
        @Override public int compareTo(Element that) {
            return Ints.compare(hash, that.hash);
        }
        @Override public String toString() {
            return String.valueOf(hash);
        }
    }

    static class SlowElement extends Element {
        SlowElement(int hash) {
            super(hash);
        }
        @Override public boolean equals(Object obj) {
            return slowItDown() != 1 && super.equals(obj);
        }
        @Override public int hashCode() {
            return slowItDown() + hash;
        }
        @Override public int compareTo(Element that) {
            int x = slowItDown();
            return x + super.compareTo(that) - x; // silly attempt to prevent opt
        }
        static int slowItDown() {
            int result = 0;
            for (int i = 1; i < 1000; i++) {
                result += i;
            }
            return result;
        }
    }
}
