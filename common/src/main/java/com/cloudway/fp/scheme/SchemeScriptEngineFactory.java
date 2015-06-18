/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;

public class SchemeScriptEngineFactory implements ScriptEngineFactory {
    public String getName() {
        return (String)getParameter(ScriptEngine.NAME);
    }

    @Override
    public String getEngineName() {
        return (String)getParameter(ScriptEngine.ENGINE);
    }

    @Override
    public String getEngineVersion() {
        return (String)getParameter(ScriptEngine.ENGINE_VERSION);
    }

    @Override
    public String getLanguageName() {
        return (String)getParameter(ScriptEngine.LANGUAGE);
    }

    @Override
    public String getLanguageVersion() {
        return (String)getParameter(ScriptEngine.LANGUAGE);
    }

    @Override
    public List<String> getExtensions() {
        return extensions;
    }

    public List<String> getMimeTypes() {
        return mimeTypes;
    }

    public List<String> getNames() {
        return names;
    }

    @Override
    public Object getParameter(String key) {
        switch (key) {
        case ScriptEngine.NAME:
            return "scheme";
        case ScriptEngine.ENGINE:
            return "Scheva";
        case ScriptEngine.ENGINE_VERSION:
            return "1.0";
        case ScriptEngine.LANGUAGE:
            return "Scheme";
        case ScriptEngine.LANGUAGE_VERSION:
            return "r5rs";
        default:
            throw new IllegalArgumentException("Invalid key: " + key);
        }
    }

    @Override
    public ScriptEngine getScriptEngine() {
        return new SchemeScriptEngine(this);
    }

    @Override
    public String getMethodCallSyntax(String obj, String method, String... args) {
        StringJoiner sj = new StringJoiner(" ", "(", ")");
        sj.add(obj);
        sj.add(method + ":");
        for (String a : args)
            sj.add(a);
        return sj.toString();
    }

    @Override
    public String getOutputStatement(String toDisplay) {
        StringBuilder buf = new StringBuilder();
        buf.append("(print \"");
        for (int i = 0, len = toDisplay.length(); i < len; i++) {
            char c = toDisplay.charAt(i);
            switch (c) {
            case '"':
                buf.append("\\\"");
                break;
            case '\\':
                buf.append("\\\\");
                break;
            default:
                buf.append(c);
                break;
            }
        }
        buf.append("\")");
        return buf.toString();
    }
    
    @Override
    public String getProgram(String... statements) {
        return "(begin " + String.join(" ", statements) + ")";
    }
    
    private static final List<String> names
        = Collections.singletonList("scheme");
    private static final List<String> mimeTypes
        = Collections.emptyList();
    private static final List<String> extensions
        = Collections.singletonList("scm");
}
