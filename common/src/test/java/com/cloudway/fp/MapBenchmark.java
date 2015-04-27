/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.google.caliper.BeforeExperiment;
import com.google.caliper.Benchmark;
import com.google.caliper.Param;

import com.cloudway.fp.data.HashPMap;
import com.cloudway.fp.data.MutablePMap;
import com.cloudway.fp.data.TreePMap;
import com.cloudway.fp.BenchmarkSampleData.Element;

/**
 * A microbenchmark that tests the performance of get() and iteration on persistent
 * map and mutable map implementations.
 */
public class MapBenchmark {
    public enum Impl {
        PHashMap {
            @Override Map<Element, Element> create() {
                return new MutablePMap<>(HashPMap.empty());
            }
        },
        JHashMap {
            @Override Map<Element, Element> create() {
                return new HashMap<>();
            }
        },
        PTreeMap {
            @Override Map<Element, Element> create() {
                return new MutablePMap<Element, Element>(TreePMap.empty());
            }
        },
        JTreeMap {
            @Override Map<Element, Element> create() {
                return new TreeMap<>();
            }
        };

        abstract Map<Element, Element> create();
    }

    @Param({"JHashMap", "PHashMap", "JTreeMap", "PTreeMap"})
    private Impl impl;

    @Param("500")
    private int size;

    @Param("0.9")
    private double hitRate;

    @Param("true")
    private boolean isUserTypeFast;

    @Param("0")
    private SpecialRandom random;

    @Param("false")
    private boolean sortedData;

    // the following must be set during setUp
    private Collection<Element> values;
    private Map<Element, Element> mapToTest;
    private Element[] queries;

    @BeforeExperiment void setUp() {
        BenchmarkSampleData sampleData =
            new BenchmarkSampleData(isUserTypeFast, random, hitRate, size);

        if (sortedData) {
            List<Element> valueList = new ArrayList<>(sampleData.getValues());
            Collections.sort(valueList);
            values = valueList;
        } else {
            values = sampleData.getValues();
        }

        this.mapToTest = populate(values);
        this.queries = sampleData.getQueries();
    }

    private Map<Element, Element> populate(Collection<Element> keys) {
        Map<Element, Element> map = impl.create();
        for (Element element : keys) {
            map.put(element, element);
        }
        return map;
    }

    @Benchmark int populate(int reps) {
        int dummy = 0;
        for (int i = 0; i < reps; i++) {
            dummy += populate(values).size();
        }
        return dummy;
    }

    @Benchmark boolean query(int reps) {
        Map<Element, Element> map = this.mapToTest;
        Element[] queries = this.queries;

        // Allows us to use & instead of %, acting on hearsay that division
        // operators (/%) are disproportionately expensive; should test this too!
        int mask = queries.length - 1;

        boolean dummy = false;
        for (int i = 0; i < reps; i++) {
            dummy ^= map.get(queries[i & mask]) != null;
        }
        return dummy;
    }

    @Benchmark boolean iterate(int reps) {
        Map<Element, Element> map = mapToTest;
        boolean dummy = false;
        for (int i = 0; i < reps; i++) {
            for (Map.Entry<Element, Element> entry : map.entrySet()) {
                dummy ^= entry.getKey() != entry.getValue();
            }
        }
        return dummy;
    }
}
