/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.scheme;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;

import com.cloudway.fp.data.Either;
import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.PMap;
import com.cloudway.fp.scheme.LispVal.Symbol;
import com.cloudway.fp.scheme.LispVal.Printer;
import jline.Completor;
import jline.ConsoleReader;

public class REPL implements Completor{
    private final Evaluator evaluator = new Evaluator();
    private final Env env = evaluator.getSchemeReportEnv();

    public Either<LispError, LispVal> runFile(String filename) throws IOException {
        try (InputStream is = Files.newInputStream(Paths.get(filename))) {
            Reader input = new InputStreamReader(is, StandardCharsets.UTF_8);
            return evaluator.run(env, evaluator.parse(filename, input));
        }
    }

    public void runREPL() throws IOException {
        ConsoleReader console = new ConsoleReader(System.in, new PrintWriter(System.out));
        console.addCompletor(this);

        String line;
        while ((line = console.readLine("> ")) != null) {
            if ("quit".equals(line.trim()))
                break;
            if (line.trim().isEmpty())
                continue;
            printResult(evaluator.run(env, evaluator.parse(line)));
        }
    }

    private static void printResult(Either<LispError, LispVal> result) {
        result.either(
            err -> {
                System.err.println(err.getMessage());
                return null;
            },

            res -> {
                if (!(res instanceof LispVal.Void)) {
                    try {
                        Printer pr = new Printer();
                        pr.add(res);
                        pr.print(System.out::print);
                        System.out.println();
                    } catch (IOException ex) {
                        throw new UncheckedIOException(ex);
                    }
                }
                return null;
            }
        );
    }

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    public int complete(String buffer, int cursor, List candidates) {
        String prefix = scanSymbol(buffer, cursor, 0);
        if (prefix != null) {
            if (!prefix.isEmpty())
                completeKeywords(prefix, candidates);
            completeDefinitions(prefix, candidates);
            Collections.sort(candidates);
            return cursor - prefix.length();
        }
        return cursor;
    }

    private static final String[] KEYWORDS = {
        "define", "define-macro", "lambda", "set!", "if", "cond", "match",
        "and", "or", "quote", "quasiquote", "unquote", "unquote-splicing",
        "begin", "delay", "let", "let*", "letrec", "do"
    };

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static void completeKeywords(String prefix, List candidates) {
        for (String key : KEYWORDS) {
            if (key.startsWith(prefix) && !candidates.contains(key)) {
                candidates.add(key + " ");
            }
        }
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void completeDefinitions(String prefix, List candidates) {
        PMap<Symbol, ?> bindings = env.getBindings();
        for (Symbol var : bindings.keys()) {
            String name = var.name;
            if (!name.startsWith("%") && name.startsWith(prefix) && !candidates.contains(name)) {
                candidates.add(name + " ");
            }
        }
    }

    private static String scanSymbol(String str, int from, int to) {
        int start = -1;
        for (int i = from; --i >= to; ) {
            char ch = str.charAt(i);
            if (isSymbolChar(ch)) {
                start = i;
            } else {
                break;
            }
        }
        return start == -1 ? "" : str.substring(start, from);
    }

    private static boolean isSymbolChar(char ch) {
        return ch >= 'a' && ch <= 'z' ||
               ch >= 'A' && ch <= 'Z' ||
               ch >= '0' && ch <= '9' ||
               "!$%&*+\\-/:.<=>?@^_~".indexOf(ch) != -1;
    }

    public static void main(String[] args) throws IOException {
        String  filename    = null;
        boolean interactive = false;

        int argIndex = 0;
        for (; argIndex < args.length; argIndex++) {
            if ("-i".equals(args[argIndex])) {
                interactive = true;
            } else {
                break;
            }
        }

        if (argIndex < args.length) {
            filename = args[argIndex];
        } else {
            interactive = true;
        }

        REPL repl = new REPL();
        if (interactive) {
            if (filename != null)
                printResult(repl.runFile(filename));
            repl.runREPL();
        } else {
            repl.runFile(filename).getOrThrow(Fn.id());
        }
    }
}
