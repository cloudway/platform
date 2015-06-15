/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import com.cloudway.fp.data.Seq;
import com.cloudway.fp.data.Tuple;

import static com.cloudway.fp.parser.Message.*;

/**
 * The exception type represents parse errors. It provides the source position
 * of the error and a list of error messages.
 */
@SuppressWarnings("ExceptionClassNameDoesntEndWithException")
public class ParseError extends RuntimeException {
    private static final long serialVersionUID = -786296266993458970L;

    private final SourcePos errorPos;
    private final Seq<Message> messages;

    public ParseError(SourcePos pos, Seq<Message> msgs) {
        this.errorPos = pos;
        this.messages = msgs.sorted();
    }

    public ParseError(SourcePos pos, Message msg) {
        this.messages = Seq.of(msg);
        this.errorPos = pos;
    }

    public ParseError(SourcePos pos) {
        this.messages = Seq.nil();
        this.errorPos = pos;
    }

    public ParseError(Throwable cause) {
        super(cause);
        this.messages = Seq.nil();
        this.errorPos = new SourcePos("", SourcePos.NOPOS);
    }

    public SourcePos getErrorPos() {
        return errorPos;
    }

    public ParseError setErrorPos(SourcePos pos) {
        return new ParseError(pos, messages);
    }

    public Seq<Message> getMessages() {
        return messages;
    }

    public ParseError addMessage(Message msg) {
        return new ParseError(errorPos, Seq.cons(msg, messages).distinct());
    }

    public boolean isUnknown() {
        return messages.isEmpty();
    }

    public ParseError merge(ParseError other) {
        // prefer meaningful errors
        if (!this.messages.isEmpty() && other.messages.isEmpty())
            return this;
        if (this.messages.isEmpty() && !other.messages.isEmpty())
            return other;

        // select the longest match
        int c = this.errorPos.compareTo(other.errorPos);
        if (c == 0) {
            return new ParseError(errorPos, this.messages.append(other.messages));
        } else if (c > 0) {
            return this;
        } else {
            return other;
        }
    }

    public String toString() {
        return errorPos + ":" + showErrorMessages(
            "or", "unknown parse error", "expecting",
            "unexpected", "end of input", messages);
    }

    private static String showErrorMessages(
            String msgOr,
            String msgUnknown,
            String msgExpecting,
            String msgUnExpected,
            String msgEndOfInput,
            Seq<Message> msgs) {
        if (msgs.isEmpty()) {
            return msgUnknown;
        }

        Tuple<Seq<Message>, Seq<Message>> sysUnExpect =
            msgs.span(m -> m instanceof SysUnExpect);
        Tuple<Seq<Message>, Seq<Message>> unExpect =
            sysUnExpect.second().span(m -> m instanceof UnExpect);
        Tuple<Seq<Message>, Seq<Message>> expect =
            unExpect.second().span(m -> m instanceof Expect);

        String showExpect = showMany(msgExpecting, msgOr, expect.first());
        String showUnExpect = showMany(msgUnExpected, msgOr, unExpect.first());

        String showSysUnExpect;
        if (!unExpect.first().isEmpty() || sysUnExpect.first().isEmpty()) {
            showSysUnExpect = "";
        } else {
            String firstMsg = sysUnExpect.first().head().getMessage();
            if (firstMsg.isEmpty()) {
                showSysUnExpect = msgUnExpected + " " + msgEndOfInput;
            } else {
                showSysUnExpect = msgUnExpected + " " + firstMsg;
            }
        }

        String showMessages = showMany("", msgOr, expect.second());

        return Seq.of(showSysUnExpect, showUnExpect, showExpect, showMessages)
                  .filter(s -> !s.isEmpty())
                  .distinct()
                  .show("\n", "\n", "");
    }

    private static String showMany(String pre, String or, Seq<Message> msgs) {
        Seq<String> ms = msgs.map(Message::getMessage).filter(s -> !s.isEmpty()).distinct();
        if (ms.isEmpty()) {
            return "";
        } else if (pre.isEmpty()) {
            return commasOr(or, ms);
        } else {
            return pre + " " + commasOr(or, ms);
        }
    }

    private static String commasOr(String or, Seq<String> ms) {
        if (ms.isEmpty()) {
            return "";
        } else if (ms.tail().isEmpty()) {
            return ms.head();
        } else {
            StringBuilder sb = new StringBuilder();
            while (!ms.tail().isEmpty()) {
                sb.append(ms.head()).append(", ");
                ms = ms.tail();
            }
            sb.append(or).append(" ").append(ms.head());
            return sb.toString();
        }
    }
}
