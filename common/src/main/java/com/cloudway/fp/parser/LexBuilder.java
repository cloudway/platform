/**
 * Cloudway Platform
 * Copyright (c) 2012-2013 Cloudway Technology, Inc.
 * All rights reserved.
 */

package com.cloudway.fp.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import com.cloudway.fp.data.Fn;
import com.cloudway.fp.data.Maybe;

public class LexBuilder<T> {
    /* The NFA state */
    static class NFA
    {
        int         num;            /* The state number of this node.        */
        int         edge;           /* Label for edge: character, CCL, EMPTY */
                                    /* or EPSILON. */
        BitSet      bitset;         /* Set to store character classes.       */
        boolean     compl;          /* is a negative character class set.    */
        NFA         next;           /* Next state (or NULL if none)          */
        NFA         next2;          /* Another next state if edge==EPSILON   */
        Accept      accept;         /* null if not an accepting state, else  */
                                    /* the accepting code of the rule        */

        NFA         end;            /* temporarily used for machine building */
    }

    /* The accept function that translate lexeme to token. */
    @FunctionalInterface
    interface Accept {
        Object apply(InputBuffer input);
    }

    /* The special marker object that may be returned by an accept function
     * to indicate the result is ignored. */
    private static final Object IGNORE = new Object();

    /* Non-character values of NFA.edge */
    static final int EPSILON  = -1;
    static final int CCL      = -2;
    static final int EMPTY    = -3;

    /* Tokens */
    static final int EOS         = 1,       /* end of string     */
                     ANY         = 2,       /* .                 */
                     CCL_END     = 3,       /* ]                 */
                     CCL_START   = 4,       /* [                 */
                     CLOSE_CURLY = 5,       /* }                 */
                     CLOSE_PAREN = 6,       /* )                 */
                     CLOSURE     = 7,       /* *                 */
                     COMPLEMENT  = 8,       /* ^                 */
                     DASH        = 9,       /* -                 */
                     L           = 10,      /* literal character */
                     OPEN_CURLY  = 11,      /* {                 */
                     OPEN_PAREN  = 12,      /* (                 */
                     OPTIONAL    = 13,      /* ?                 */
                     OR          = 14,      /* |                 */
                     PCCL        = 15,      /* predefined CCL    */
                     PLUS_CLOSE  = 16;      /* +                 */

    static final int _tokmap[] =
    {
    /*  ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N      */
         L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  ^O  ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]      */
         L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  ^^  ^_  SPACE  !   "   #    $   %   &    '                      */
        L,  L,  L,     L,  L,  L,   L,  L,  L,   L,

    /*  (            )            *        +           ,  -     .       */
        OPEN_PAREN,  CLOSE_PAREN, CLOSURE, PLUS_CLOSE, L, DASH, ANY,

    /*  /   0   1   2   3   4   5   6   7   8   9   :   ;   <   =       */
        L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  >         ?                                                     */
        L,        OPTIONAL,

    /*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N       */
        L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  O   P   Q   R   S   T   U   V   W   X   Y   Z                   */
        L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  [               \       ]               ^                       */
        CCL_START,      L,      CCL_END,        COMPLEMENT,

    /*  _   `   a   b   c   d   e   f   g   h   i   j   k   l   m       */
        L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  n   o   p   q   r   s   t   u   v   w   x   y   z               */
        L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,

    /*  {            |    }            DEL                              */
        OPEN_CURLY,  OR,  CLOSE_CURLY, L
    };

    private NFA[]    _nfa_states = new NFA[16];      /* State-machine array */
    private int      _nstates;                       /* Index of next element of the array */
    private NFA      _start;                         /* The start state of machine */

    private static final int SSIZE = 32;

    private NFA[]    _sstack = new NFA[SSIZE];       /* Stack used by new_nfa() */
    private int      _sp = -1;                       /* Stack pointer */

    private String   _input;                         /* The input string */
    private boolean  _inquote;                       /* Processing quoted string */
    private int      _next;                          /* The position of current token */
    private int      _current_tok;                   /* Current token */
    private int      _lexeme;                        /* Value associated with LITERAL */

    private final Map<String,String> _macros = new HashMap<String, String>();
    private String   _mac_stack[] = new String[SSIZE]; /* Input-source stack */
    private int      _mac_sp = -1;                     /* and stack pointer */

    /*----------------------------------------------------------------
     * Predefined character classes
     */

    private static final BitSet LETTERS = new BitSet();
    private static final BitSet DIGITS  = new BitSet();
    private static final BitSet SPACES  = new BitSet();

    static {
        LETTERS.set('a', 'z'+1);
        LETTERS.set('A', 'Z'+1);
        LETTERS.set('_');

        DIGITS.set('0', '9'+1);

        SPACES.set(' ');
        SPACES.set('\t');
        SPACES.set('\n');
        SPACES.set('\r');
        SPACES.set('\f');
    }

    /*----------------------------------------------------------------
     * Error processing stuff. Note that all errors are fatal.
     *----------------------------------------------------------------
     */

    private enum ERR_NUM {
        E_BADEXPR("Malformed regular expression"),
        E_PAREN("Missing close parenthesis"),
        E_BRACKET("Missing [ in character class"),
        E_CLOSE("+ ? or * must follow expression"),
        E_BADMAC("Missing } in macro expansion"),
        E_NOMAC("Macro doesn't exist"),
        E_EMPTYMAC("Macro body is empty"),
        E_MACDEPTH("Macro expansions nested too deeply.");

        final String errmsg;
        ERR_NUM(String msg) { this.errmsg = msg; }
    }

    private static void parse_error(ERR_NUM type) {
        throw new LexError(type.errmsg);
    }

    /*--------------------------------------------------------------*/
    /* NFA management methods */

    private NFA new_nfa() {
        NFA p;

        if (_sp == -1) {
            if (_nstates >= _nfa_states.length) {
                NFA[] t = new NFA[_nfa_states.length * 2];
                System.arraycopy(_nfa_states, 0, t, 0, _nstates);
                _nfa_states = t;
            }

            p = new NFA();
            p.num = _nstates;
            _nfa_states[_nstates++] = p;
        } else {
            p = _sstack[_sp--];
        }

        p.edge   = EPSILON;
        p.accept = null;

        return p;
    }

    private void discard(NFA nfa_to_discard) {
        nfa_to_discard.edge   = EMPTY;
        nfa_to_discard.bitset = null;
        nfa_to_discard.compl  = false;
        nfa_to_discard.next   = null;
        nfa_to_discard.next2  = null;
        nfa_to_discard.accept = null;
        nfa_to_discard.end    = null;
        _sstack[++_sp] = nfa_to_discard;
    }

    /*----------------------------------------------------------------
     * Lexical analyzer:
     *
     * Lexical analysis is trivial because all lexemes are single-character values.
     * The only complications are escape sequences and quoted strings, both
     * of which are handled by advance(), below. This routine advances past the
     * current token, putting the new token into Current_tok and the equivalent
     * lexeme into Lexeme. If the character was escaped, Lexeme holds the actual
     * value. For example, if a "\s" is encountered, Lexeme will hold a space
     * character.  Advance both modifies Current_tok to the current token and
     * returns it.
     *
     * Macro expansion is handled by means of a stack (declared at the top
     * of the subroutine). When an expansion request is encountered, the
     * current input buffer is stacked, and input is read from the macro
     * text. This process repeats with nested macros, so SSIZE controls
     * the maximum macro-nesting depth.
     */
    private int advance() {
        int t, c;

        while (_next >= _input.length()) {
            if (_mac_sp >= 0) {            // Restore previous input source
                _input = _mac_stack[_mac_sp--];
                _next = 0;
                continue;
            }

            _current_tok = EOS;           // No more input sources to restore
            _lexeme = '\0';               // ie. you're at the real end of string.
            return EOS;
        }

        if (!_inquote) {
            String name, text;
            int i;

            while (_input.charAt(_next) == '{') {   // Macro expansion required
                if ((i = _input.indexOf('}', _next+1)) == -1)   // skip { and find }
                    parse_error(ERR_NUM.E_BADMAC);
                name = _input.substring(_next+1, i);
                if ((text = _macros.get(name)) == null)
                    parse_error(ERR_NUM.E_NOMAC);
                else if (text.isEmpty())
                    parse_error(ERR_NUM.E_EMPTYMAC);
                if (_mac_sp+1 >= _mac_stack.length)
                    parse_error(ERR_NUM.E_MACDEPTH);

                // Stack current input string. Use macro body as input string.
                assert text != null;
                _mac_stack[++_mac_sp] = _input.substring(i+1);
                _input = text;
                _next  = 0;
            }
        }

        c = _input.charAt(_next++);

        if (_inquote) {
            t = L;
        } else if (c == '\\' && _next < _input.length()) {
            c = _input.charAt(_next++);
            t = L;
            switch (c) {
            case 't': c = '\t'; break;
            case 'n': c = '\n'; break;
            case 'r': c = '\r'; break;
            case 'f': c = '\f'; break;
            // predefined character classes
            case 'd': case 'D':
            case 's': case 'S':
            case 'w': case 'W': t = PCCL; break;
            }
        } else {
            t = c >= _tokmap.length ? L : _tokmap[c];
        }

        _current_tok = t;
        _lexeme = c;
        return t;
    }

    /*--------------------------------------------------------------
     * The Parser:
     *  A simple recursive descent parser that creates a Thompson NFA for
     *  a regular expression. The NFA is created as a directed graph, with
     *  each node containing pointer's to the next node. Since the structures
     *  are allocated from an array, the machine can also be considered as an
     *  array where the state number is the array index.
     */

    /**
     * Add a macro definition to the LexBuilder.
     *
     * @param name the macro name
     * @param text the text to be substituted
     */
    public LexBuilder<T> macro(String name, String text) {
        _macros.put(name, "(" + text + ")");
        return this;
    }

    /**
     * Add a new rule to the LexBuilder.
     *
     * @param rule a regular expression that match the rule
     * @param accept the accept code
     */
    public LexBuilder<T> rule(String rule, Function<String, T> accept) {
        Objects.requireNonNull(accept);
        add(rule, false, s -> accept.apply(s.lexeme()));
        return this;
    }

    /**
     * Add a new literal text rule to the LexBuilder.
     *
     * @param text the literal text
     * @param token the token to be returned when accepting the rule
     */
    public LexBuilder<T> literal(String text, T token) {
        add(text, true, __ -> token);
        return this;
    }

    /**
     * Add a ignorable rule to the LexBuilder. For example whitespaces or
     * comments are ignorable rules.
     *
     * @param rule a regular expression that match the rule
     */
    @SuppressWarnings("unchecked")
    public LexBuilder<T> ignore(String rule) {
        add(rule, false, __ -> IGNORE);
        return this;
    }

    /**
     * Perform a low-level action on the input buffer. If the function returns
     * nothing, the result is ignored, otherwise returns that token.
     *
     * @param rule a regular expression that match the rule
     * @param action a function to apply on low-level input buffer
     */
    @SuppressWarnings("unchecked")
    public LexBuilder<T> action(String rule, Function<InputBuffer, Maybe<T>> action) {
        Objects.requireNonNull(action);
        add(rule, false, s -> ((Maybe<Object>)action.apply(s)).orElse(IGNORE));
        return this;
    }

    private void add(String input, boolean inquote, Accept accept) {
        NFA p;

        if (_start == null) {
            p = _start = new_nfa();
        } else {
            p = _start.end;
            p.next2 = new_nfa();
            p = p.next2;
        }
        _start.end = p;

        this._input   = input;
        this._inquote = inquote;
        this._next    = 0;

        advance();
        p.next = rule(accept);
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

    private NFA rule(Accept accept) {
        /*      rule    --> expr  EOS
         */

        NFA start;

        start = expr();
        start.end.accept = accept;
        advance();
        return start;
    }

    private NFA expr() {
        /* Because a recursive descent compiler can't handle left recursion,
         * the productions:
         *
         *      expr    -> expr OR cat_expr
         *              |  cat_expr
         *
         * must be translated into:
         *
         *      expr    -> cat_expr expr'
         *      expr'   -> OR cat_expr expr'
         *                 epsilon
         *
         * which can be implemented with this loop:
         *
         *      cat_expr
         *      while( match(OR) )
         *              cat_expr
         *              do the OR
         */

        NFA start, end;
        NFA e2_start, e2_end;
        NFA compose = null;
        NFA p;

        start = cat_expr();
        end   = start.end;

        if (start.next == end)
            compose = start;

        while (_current_tok == OR) {
            advance();
            e2_start = cat_expr();
            e2_end   = e2_start.end;

            if (compose != null && e2_start.next == e2_end) {
                // Compose multiple terms into one character class.
                // For example, given an expression "a|b", convert it to
                // an equivalent expression "[ab]".

                if (compose.edge != CCL) {
                    compose.bitset = new BitSet();
                    if (compose.edge != EPSILON)
                        compose.bitset.set(compose.edge);
                    compose.edge = CCL;
                }

                if (e2_start.edge == CCL) {                         // handle complement sets
                    if (!compose.compl && !e2_start.compl) {        // A | B
                        compose.bitset.or(e2_start.bitset);
                    } else if (compose.compl && e2_start.compl) {   // ~A | ~B = ~(A & B)
                        compose.bitset.and(e2_start.bitset);
                    } else if (compose.compl) {                     // ~A | B = ~(A & ~B)
                        compose.bitset.andNot(e2_start.bitset);
                    } else {                                        // A | ~B = ~(B & ~A)
                        e2_start.bitset.andNot(compose.bitset);
                        compose.bitset = e2_start.bitset;
                        compose.compl = true;
                    }
                } else if (e2_start.edge != EPSILON) {
                    compose.bitset.set(e2_start.edge, !compose.compl);
                }

                discard(e2_start);
                discard(e2_end);
            } else {
                p = new_nfa();
                p.next2 = e2_start;
                p.next  = start;
                start   = p;

                p = new_nfa();
                end.next = p;
                e2_end.next = p;
                end = p;

                if (e2_start.next == e2_end)
                    compose = e2_start;
            }
        }

        start.end = end;
        return start;
    }

    private NFA cat_expr() {
        /* The same translations that were needed in the expr rules are needed again
         * here:
         *
         *      cat_expr  -> cat_expr | factor
         *                   factor
         *
         * is translated to:
         *
         *      cat_expr  -> factor cat_expr'
         *      cat_expr' -> | factor cat_expr'
         *                   epsilon
         */

        NFA start = null;
        NFA e2_start;

        if (first_in_cat(_current_tok)) {
            start = factor();

            while (first_in_cat(_current_tok)) {
                e2_start = factor();

                // discard e2_start
                start.end.edge   = e2_start.edge;
                start.end.bitset = e2_start.bitset;
                start.end.compl  = e2_start.compl;
                start.end.next   = e2_start.next;
                start.end.next2  = e2_start.next2;
                start.end        = e2_start.end;
                discard(e2_start);
            }
        }

        return start;
    }

    private static boolean first_in_cat(int tok) {
        switch (tok) {
        case CLOSE_PAREN:
        case OR:
        case EOS:           return false;

        case CLOSURE:
        case PLUS_CLOSE:
        case OPTIONAL:      parse_error(ERR_NUM.E_CLOSE);   return false;

        case CCL_END:       parse_error(ERR_NUM.E_BRACKET); return false;
        }

        return true;
    }

    private NFA factor() {
        /* factor --> term*  | term+  | term? */

        NFA startp, endp;
        NFA start, end;

        startp = term();
        endp   = startp.end;

        if (_current_tok==CLOSURE || _current_tok==PLUS_CLOSE || _current_tok==OPTIONAL) {
            start = new_nfa();
            end   = new_nfa();
            start.next = startp;
            endp.next  = end;

            if (_current_tok==CLOSURE || _current_tok==OPTIONAL)
                start.next2 = end;

            if (_current_tok==CLOSURE || _current_tok==PLUS_CLOSE)
                endp.next2 = startp;

            startp = start;
            startp.end = end;
            advance();
        }

        return startp;
    }

    private NFA term() {
        /* Process the term productions:
         *
         * term  --> [...]  |  [^...]  |  []  |  [^] |  .  | (expr) | <character>
         *
         * The [] is nonstandard. It matches a space, tab, form feed, or newline,
         * but not a carriage return (\r). All of these are single nodes in the
         * NFA.
         */

        NFA start;

        if (_current_tok == OPEN_PAREN) {
            advance();
            start = expr();
            if (_current_tok == CLOSE_PAREN)
                advance();
            else
                parse_error(ERR_NUM.E_PAREN);
        } else {
            start = new_nfa();
            start.end = start.next = new_nfa();

            if (!(_current_tok==ANY || _current_tok==CCL_START || _current_tok==PCCL)) {
                start.edge = _lexeme;
                advance();
            } else if (_current_tok == PCCL) {
                start.edge = CCL;
                predefined(start);
                advance();
            } else {
                start.edge = CCL;
                start.bitset = new BitSet();

                if (_current_tok == ANY) {
                    start.bitset.set('\n');
                    start.compl = true;
                } else {
                    advance();
                    if (_current_tok == COMPLEMENT) {   // Negative character class
                        advance();
                        start.bitset.set('\n');
                        start.compl = true;
                    }
                    if (_current_tok != CCL_END)
                        dodash(start.bitset);
                    else                                // [] or [^]
                        start.bitset.set(0, ' '+1);
                }
                advance();
            }
        }

        return start;
    }

    private void predefined(NFA p) {
        switch (_lexeme) {
        case 'D': p.compl = true;
        case 'd': p.bitset = DIGITS;
                  break;

        case 'S': p.compl = true;
        case 's': p.bitset = SPACES;
                  break;

        case 'W': p.compl = true;
        case 'w': p.bitset = LETTERS;
                  break;

        default:  assert false;
        }
    }

    private void dodash(BitSet set) {
        int first = 0;

        if (_current_tok == DASH) {         // Treat [-...] as a literal dash
            set.set('-');
            advance();
        }

        for (; _current_tok != EOS && _current_tok != CCL_END; advance()) {
            if (_current_tok != DASH) {
                first = _lexeme;
                set.set(_lexeme);
            } else { // looking at a dash
                advance();
                if (_current_tok == CCL_END) { // Treat [...-] as literal
                    set.set('-');
                } else if (first <= _lexeme) {
                    set.set(first, _lexeme+1);
                }
            }
        }
    }

    /*--------------------------------------------------------------
     * Routine to print out a NFA structure in human-readable form.
     */

    private static void printccl(BitSet set) {
        System.out.print('[');
        for (int i = 0; i <= 0x7f; i++) {
            if (set.get(i)) {
                if (i < ' ')
                    System.out.printf("^%c", (char)(i+'@'));
                else
                    System.out.printf("%c", (char)i);
            }
        }
        System.out.print(']');
    }

    private static String plab(NFA nfa) {
        return nfa == null ? "--" : String.format("%2d", nfa.num);
    }

    @SuppressWarnings("unused")
    public void print_nfa() {
        System.out.println("----------------- NFA ----------------");

        for (int i = 0; i < _nstates; i++) {
            NFA nfa = _nfa_states[i];

            if (nfa.edge == EMPTY)
                continue;

            System.out.printf("NFA state %s: ", plab(nfa));

            if (nfa.next == null) {
                System.out.print("(TERMINAL)");
            } else {
                System.out.printf("--> %s ", plab(nfa.next));
                System.out.printf("(%s) on ", plab(nfa.next2));

                switch (nfa.edge) {
                case CCL:       printccl(nfa.bitset);             break;
                case EPSILON:   System.out.print("EPSILON ");     break;
                default:        System.out.print((char)nfa.edge); break;
                }
            }

            if (nfa == _start)
                System.out.print(" (START STATE)");

            if (nfa.accept != null)
                System.out.print(" accepting ");

            System.out.println();
        }

        System.out.println("--------------------------------");
    }

    /*----------------------------------------------------------------*/

    private static final int MAX_CHARS = 128; // Maximum width of dfa transition table
    private static final int FAIL = -1;       // Marks failure states in the table
    private static final int EOI = -1;        // Marks end of input

    // The DFA state
    static class DFA {
        int         num;                /* The DFA state number               */
        boolean     mark;               /* Mark used by make_dtran()          */
        BitSet      nfa_set;            /* Set of NFA states represented by   */
        Accept      accept;             /* Accept action if accept state */

        DFA(BitSet nfa_set, Accept accept) {
            this.num     = -1;
            this.nfa_set = nfa_set;
            this.accept  = accept;
        }
    }

    /*
     * Encapsulate a DFA transition table.
     */
    static class Dtran {
        private int[]     table;    /* The transition table                  */
        private final int ncols;    /* number of columns in transition table */

        Dtran(int ncols, int nrows) {
            this.table = new int[nrows * ncols];
            this.ncols = ncols;
        }

        Dtran(int ncols) {
            this(ncols, 16);
        }

        /**
         * Get transition from current state and input.
         */
        int get(int row, int col) {
            return table[row * ncols + col];
        }

        /**
         * Add a transition.
         */
        void add(int row, int col, int val) {
            ensureCapacity(row + 1);
            table[row * ncols + col] = val;
        }

        private void ensureCapacity(int rows) {
            int minCapacity = rows * ncols;
            if (minCapacity > table.length) {
                int newCapacity = table.length * 2;
                if (newCapacity < minCapacity)
                    newCapacity = minCapacity;
                table = Arrays.copyOf(table, newCapacity);
            }
        }

        /**
         * Returns true if the two columns are equivalent, else return false.
         */
        boolean col_equiv(int col1, int col2, int nrows) {
            int[] tab = table;
            while (--nrows >= 0 && tab[col1] == tab[col2]) {
                col1 += ncols;
                col2 += ncols;
            }
            return !(nrows >= 0);
        }

        /**
         * Copy a column from one table to this table.
         */
        void col_copy(Dtran src, int dest_col, int src_col, int nrows) {
            ensureCapacity(nrows);

            int[] tab1 = this.table;
            int[] tab2 = src.table;
            while (--nrows >= 0) {
                tab1[dest_col] = tab2[src_col];
                dest_col += this.ncols;
                src_col  += src.ncols;
            }
        }

        /**
         * Returns true if the two rows are equivalent, else return false.
         */
        boolean row_equiv(int row1, int row2) {
            int[] tab   = table;
            int   ncols = this.ncols;
            int   off1  = row1 * ncols;
            int   off2  = row2 * ncols;

            while (--ncols >= 0 && tab[off1++] == tab[off2++])
                ;
            return !(ncols >= 0);
        }

        /**
         * Copy a row from one table to this table. Both table must have same
         * number of columns.
         */
        void row_copy(Dtran src, int dest_row, int src_row) {
            ensureCapacity(dest_row + 1);

            int[] tab1  = this.table;
            int[] tab2  = src.table;
            int   ncols = this.ncols;
            int   off1  = dest_row * ncols;
            int   off2  = src_row  * ncols;

            while (--ncols >= 0) {
                tab1[off1++] = tab2[off2++];
            }
        }
    }

    static class CompressedDtran extends Dtran {
        private final int[] col_map;
        private final int[] row_map;

        CompressedDtran(int ncols, int nrows, int[] col_map, int[] row_map) {
            super(ncols, nrows);
            this.col_map = col_map;
            this.row_map = row_map;
        }

        @Override
        int get(int row, int col) {
            return super.get(row_map[row], col_map[col]);
        }
    }

    /*------------------------------------------------------------------------
     * The DFA driven lexer
     */
    static final class Machine<T> implements Lexer<T> {
        private NFA[] nfa_states;   /* NFA state array                */
        private DFA[] dfa_states;   /* DFA state table                */
        private int   nstates;      /* number of DFA states           */
        private int   last_marked;  /* Most-recently marked DFA state */

        private Dtran dtran;                 /* DFA transition table  */
        private final Map<BitSet, DFA> dmap; /* DFA transition map    */

        Machine(NFA[] states, int start) {
            this.nfa_states  = states;
            this.dfa_states  = new DFA[16];
            this.nstates     = 0;
            this.last_marked = 0;

            dtran = make_dtran(start);
            dtran = squash(dtran);

            if (dfa_states.length != nstates) {
                dfa_states = Arrays.copyOf(dfa_states, nstates);
            }

            // create a NFA-set to DFA state map for quick lookup
            dmap = new HashMap<>();
            for (DFA p : dfa_states) {
                dmap.put(p.nfa_set, p);
            }
        }

        /**
         * Compute the epsilon closure set for the input states. The set is
         * updated with all states that can be reached by making epsilon
         * transitions from all NFA states in the input set. Making an empty
         * set if the input set or the closure set is empty. Returns the
         * accepting function if one of the elements of the output state is an
         * accepting state.
         */
        private Accept e_closure(BitSet nfa_set) {
            if (nfa_set == null) {
                return null;
            }

            LinkedList<NFA> stack       = new LinkedList<>();
            Accept          accept      = null;
            int             accept_num  = Integer.MAX_VALUE;

            for (int i = nfa_set.nextSetBit(0); i >= 0; i = nfa_set.nextSetBit(i+1)) {
                stack.push(nfa_states[i]);
            }

            while (!stack.isEmpty()) {
                NFA p = stack.pop();

                if (p.accept != null && p.num < accept_num) {
                    accept     = p.accept;
                    accept_num = p.num;
                }

                if (p.edge == EPSILON) {
                    if (p.next != null) {
                        if (!nfa_set.get(p.next.num)) {
                            nfa_set.set(p.next.num);
                            stack.push(p.next);
                        }
                    }
                    if (p.next2 != null) {
                        if (!nfa_set.get(p.next2.num)) {
                            nfa_set.set(p.next2.num);
                            stack.push(p.next2);
                        }
                    }
                }
            }

            return accept;
        }

        /**
         * Returns a set that contains all NFA states that can be reached by
         * making transitions on "c" from any NFA states in "inpset". Returns
         * null if there are no such transitions. The "inpset" is not modified.
         */
        private BitSet move(BitSet inpset, int c) {
            BitSet outset = null;

            for (int i = inpset.nextSetBit(0); i >= 0; i = inpset.nextSetBit(i+1)) {
                NFA p = nfa_states[i];

                if (p.edge==c || (p.edge==CCL && (p.bitset.get(c) ^ p.compl))) {
                    if (outset == null)
                        outset = new BitSet();
                    outset.set(p.next.num);
                }
            }

            return outset;
        }

        /**
         * Turns an NFA with the indicated start state into a DFA.
         */
        private Dtran make_dtran(int sstate) {
            BitSet  nfa_set;    // Set of NFA states that define
                                // the next DFA state
            Accept  accept;     // Current DFA state is an accept
            DFA     current;    // State currently being expanded.
            int     nextstate;  // Goto DFA state for current char

            /* Initially Dstates contains a single, unmarked, start state
             * formed by taking the epsilon closure of the NFA start state.
             * So, Dstates[0] is the DFA start state.
             */
            nfa_set = new BitSet();
            nfa_set.set(sstate);
            accept = e_closure(nfa_set);
            add_to_dstates(nfa_set, accept);

            /* Make DFA transition table by simulate NFA machine */
            Dtran dtran = new Dtran(MAX_CHARS);

            while ((current = get_unmarked()) != null) {
                current.mark = true;

                for (int c = MAX_CHARS; --c >= 0; ) {
                    nfa_set = move(current.nfa_set, c);
                    if (nfa_set != null) {
                        accept = e_closure(nfa_set);
                        nextstate = add_to_dstates(nfa_set, accept);
                    } else {
                        nextstate = FAIL;
                    }

                    dtran.add(current.num, c, nextstate);
                }
            }

            return dtran;
        }

        private int add_to_dstates(BitSet nfa_set, Accept accept) {
            int nextstate;

            // If there's a set in dstates that is identical to nfa_set, return
            // the index of the dstate entry.
            for (int i = 0; i < nstates; i++) {
                if (nfa_set.equals(dfa_states[i].nfa_set))
                    return i;
            }

            if ((nextstate = nstates++) >= dfa_states.length) {
                DFA[] t = new DFA[dfa_states.length * 2];
                System.arraycopy(dfa_states, 0, t, 0, nextstate);
                dfa_states = t;
            }

            DFA p = new DFA(nfa_set, accept);
            p.num = nextstate;
            dfa_states[nextstate] = p;
            return nextstate;
        }

        /**
         * Returns a pointer to an unmarked state in Dstates. If no such state
         * exists, return null.
         */
        private DFA get_unmarked() {
            for (; last_marked < nstates; ++last_marked) {
                DFA p = dfa_states[last_marked];
                if (!p.mark)
                    return p;
            }
            return null;
        }

        /**
         * Compress dtran using equivalent-column elimination.
         */
        private Dtran squash(Dtran dtran) {
            int     ncols = MAX_CHARS;  /* number or columns in original machine     */
            int     nrows = nstates;    /* number of rows    in original machine     */
            int     r_ncols;            /* number of columns in reduced  machine     */
            int     r_nrows;            /* number of rows    in reduced  machine     */
            BitSet  save;               /* rows or columns that will remain in table */
            Dtran   compressed;         /* compressed array                          */
            int     i, j;

            // First do the columns

            int[] col_map = new int[ncols];
            Arrays.fill(col_map, -1);
            save = new BitSet();

            for (r_ncols = 0;; r_ncols++) {
                // Skip past any states in the col_map that have already been
                // processed. If the entire col_map has been processed, break.

                for (i = r_ncols; i < ncols && col_map[i] != -1; i++)
                    ;
                if (i >= ncols)
                    break;

                // Add the current column to the save set. It eventually ends up
                // in the reduced array as column "r_ncols" so modify the col_map
                // entry accordingly. Now scan trough the array looking for
                // duplicates of the current column (pointer to by current). If you
                // find a duplicate, make the associated col_map entry also point to
                // "r_ncols."

                save.set(i);
                col_map[i] = r_ncols;

                for (j = i; ++j < ncols; ) {
                    if (col_map[j] == -1 && dtran.col_equiv(i, j, nrows))
                        col_map[j] = r_ncols;
                }
            }

            // Compress the array horizontally by removing all of the columns that
            // aren't in the save set. We're doing this by moving all the columns
            // that are in the save set to the proper position in a newly allocated
            // array. You can't do it in place because there's no guarantee that the
            // equivalent rows are next to each other.

            compressed = new Dtran(r_ncols);
            for (i = save.nextSetBit(0), j = 0; i != -1; i = save.nextSetBit(i+1), j++) {
                compressed.col_copy(dtran, j, i, nrows);
            }

            // Eliminate equivalent rows, working on the reduced array
            // created in the previous step. The algorithm used is the
            // same

            int[] row_map = new int[nrows];
            Arrays.fill(row_map, -1);
            save.clear();

            for (r_nrows = 0;; r_nrows++) {
                for (i = r_nrows; i < nrows && row_map[i] != -1; i++)
                    ;
                if (i >= nrows)
                    break;

                save.set(i);
                row_map[i] = r_nrows;

                for (j = i; ++j < nrows; )
                    if (row_map[j] == -1 && compressed.row_equiv(i, j))
                        row_map[j] = r_nrows;
            }

            // Actually compress rows, copying back into the original array space.
            // Note that both dimensions of the array have been changed.

            Dtran otran = new CompressedDtran(r_ncols, r_nrows, col_map, row_map);
            for (i = save.nextSetBit(0), j = 0; i != -1; i = save.nextSetBit(i+1), j++) {
                otran.row_copy(compressed, j, i);
            }

            return otran;
        }

        /*---------------------------------------------------------------------*/

        DFA start() {
            return dfa_states[0];
        }

        DFA step(DFA current, int c) {
            if (c < MAX_CHARS && current.num >= 0) {
                int next = dtran.get(current.num, c);
                return next == FAIL ? null : dfa_states[next];
            } else {
                BitSet nfa_set = move(current.nfa_set, c);
                Accept accept  = e_closure(nfa_set);
                DFA    p;

                if (nfa_set == null) {
                    return null;
                } else if ((p = dmap.get(nfa_set)) != null) {
                    return p;
                } else {
                    return new DFA(nfa_set, accept);
                }
            }
        }

        Stream<T> scan(InputBuffer s) {
            do {
                DFA         p           = start();
                int         pos         = s.pos();
                Accept      lastaccept  = null;
                Object      res         = null;
                ParseError  err         = null;
                int         c;

                if ((c = s.nextchar()) == EOI) {
                    return new EmptyStream<>(pos);
                }

                while (c != EOI && (p = step(p, c)) != null) {
                    if (p.accept != null) {
                        // save state for this accepted token
                        lastaccept = p.accept;
                        s.mark();
                    }
                    c = s.nextchar();
                }

                if (lastaccept == null) {
                    Message message = new Message.Fail("unknown token: " + s.lexeme());
                    err = new ParseError(new SourcePos("", pos), message);
                } else {
                    try {
                        s.reset(); // reset to last accepted position
                        res = lastaccept.apply(s);
                    } catch (ParseError e) {
                        err = e;
                    }
                }

                s.skip(); // skip the current lexeme

                if (err != null) {
                    return new ErrorStream<>(err);
                } else if (res != IGNORE) {
                    @SuppressWarnings("unchecked") T token = (T)res;
                    return new TokenStream<>(token, pos, Fn.lazy(() -> scan(s)));
                }
            } while (true);
        }

        @Override
        public Stream<T> getTokenStream(String input) {
            return scan(new StringInputBuffer(input));
        }

        @Override
        public Stream<T> getTokenStream(Reader input) {
            return scan(new ReaderInputBuffer(input));
        }

        @Override
        public Scanner<T> getScanner(String input) {
            return new TokenScanner<>(this, new StringInputBuffer(input));
        }

        @Override
        public Scanner<T> getScanner(Reader input) {
            return new TokenScanner<>(this, new ReaderInputBuffer(input));
        }
    }

    /**
     * Build the lexer.
     */
    public Lexer<T> build() {
        return new Machine<>(Arrays.copyOf(_nfa_states, _nstates), _start.num);
    }

    /*--------------------------------------------------------------*/

    private static class EmptyStream<T> implements Stream<T> {
        private final int pos;

        EmptyStream(int pos) {
            this.pos = pos;
        }

        @Override
        public <R> R uncons(BiFunction<T, Stream<T>, R> f, Supplier<R> e) {
            return e.get();
        }

        @Override
        public <R> R foldRight(BiFunction<? super T, Supplier<R>, R> f, Supplier<R> r) {
            return r.get();
        }

        @Override
        public <R> R foldLeft(R r, BiFunction<R, ? super T, R> f) {
            return r;
        }

        @Override
        public int getPosition() {
            return pos;
        }
    }

    private static class TokenStream<T> implements Stream<T> {
        private final T token;
        private final int pos;
        private final Supplier<Stream<T>> tail;

        TokenStream(T token, int pos, Supplier<Stream<T>> tail) {
            this.token = token;
            this.pos   = pos;
            this.tail  = tail;
        }

        @Override
        public <R> R uncons(BiFunction<T, Stream<T>, R> f, Supplier<R> e) {
            return f.apply(token, tail.get());
        }

        @Override
        public <R> R foldRight(BiFunction<? super T, Supplier<R>, R> f, Supplier<R> r) {
            return f.apply(token, () -> tail.get().foldRight(f, r));
        }

        @Override
        public <R> R foldLeft(R r, BiFunction<R, ? super T, R> f) {
            Stream<T> ts = this;
            while (!(ts instanceof EmptyStream)) {
                TokenStream<T> tts = (TokenStream<T>)ts;
                r = f.apply(r, tts.token);
                ts = tts.tail.get();
            }
            return r;
        }

        @Override
        public int getPosition() {
            return pos;
        }
    }

    private static class ErrorStream<T> implements Stream<T> {
        private final ParseError err;

        ErrorStream(ParseError err) {
            this.err = err;
        }

        @Override
        public <R> R uncons(BiFunction<T, Stream<T>, R> f, Supplier<R> e, Function<ParseError, R> h) {
            return h.apply(err);
        }

        @Override
        public int getPosition() {
            return err.getErrorPos().getPosition();
        }
    }

    static final class TokenScanner<T> implements Scanner<T> {
        private final Machine<T> lexer;
        private final InputBuffer input;
        private T     yytok;
        private int   yypos;
        private int   state;

        TokenScanner(Machine<T> lexer, InputBuffer input) {
            this.lexer = lexer;
            this.input = input;
        }

        @Override
        public boolean hasNext() {
            if (state == 0)
                state = scan();
            return state != -1;
        }

        @Override
        public T next() {
            if (state == 0)
                state = scan();
            if (state == -1)
                throw new NoSuchElementException("EOI");
            state = 0;
            return yytok;
        }

        private int scan() {
            do {
                DFA     yystate    = lexer.start();
                Accept  lastaccept = null;
                Object  res;
                int     c;

                input.skip(); // skip previous lexeme
                yypos = input.pos();

                if ((c = input.nextchar()) == -1) {
                    return -1;
                }

                while (c != -1 && (yystate = lexer.step(yystate, c)) != null) {
                    if (yystate.accept != null) {
                        // save state for this accepted token
                        lastaccept = yystate.accept;
                        input.mark();
                    }
                    c = input.nextchar();
                }

                if (lastaccept == null) {
                    Message message = new Message.Fail("unknown token: " + input.lexeme());
                    throw new ParseError(new SourcePos("", input.pos()), message);
                }

                input.reset(); // reset to last accepted position
                res = lastaccept.apply(input);

                if (res != IGNORE) {
                    @SuppressWarnings("unchecked")
                    T tok = (T)res;
                    yytok = tok;
                    return 1;
                }
            } while (true);
        }

        @Override
        public String text() {
            return input.lexeme();
        }

        @Override
        public int line() {
            return SourcePos.line(yypos);
        }

        @Override
        public int column() {
            return SourcePos.column(yypos);
        }
    }

    /*--------------------------------------------------------------*/

    /**
     * The low-level input buffer.
     */
    public static abstract class InputBuffer {
        // The input buffer
        private final char[] buf = new char[1024];
        private int buflen;

        // The next read position
        private int next;

        // The current source code position
        private int pos = SourcePos.STARTPOS;

        // The start of lexeme
        private int begin;

        // The secondary lexeme buffer when input buffer overflow
        private final StringBuilder sbuf = new StringBuilder();

        // Mark for current read position
        private int mark;
        private int markpos;

        /**
         * Read next character and skip current lexeme.
         */
        public int input() {
            int c = nextchar();
            skip();
            return c;
        }

        /**
         * Read next character.
         */
        public int nextchar() {
            do {
                if (buflen == -1) {
                    return EOI;
                } else if (next < buflen) {
                    return buf[next++];
                } else {
                    fill();
                }
            } while (true);
        }

        /**
         * Fill buffer when overflow.
         */
        private void fill() {
            // save lexeme to the secondary buffer
            if (next > begin) {
                pos = updatePos(pos, begin, next);
                sbuf.append(buf, begin, next - begin);
            }

            if (mark > begin) {
                markpos = updatePos(markpos, begin, mark);
            }

            // load characters from external resource
            buflen = load(buf);
            next = begin = mark = 0;
        }

        /**
         * Load characters from external resource.
         *
         * @param buf the buffer to be load
         * @return -1 if EOI, otherwise the number of characters read
         */
        protected abstract int load(char[] buf);

        /**
         * Returns current lexeme.
         */
        public String lexeme() {
            int len = next - begin;
            if (sbuf.length() != 0) {
                if (len != 0) {
                    String lexeme = sbuf.append(buf, begin, len).toString();
                    sbuf.setLength(sbuf.length() - len);
                    return lexeme;
                } else {
                    return sbuf.toString();
                }
            } else {
                return new String(buf, begin, len);
            }
        }

        /**
         * Skip current lexeme.
         */
        public void skip() {
            pos = updatePos(pos, begin, next);
            begin = next;

            if (sbuf.length() > 0) {
                sbuf.setLength(0);
            }
        }

        /**
         * Returns the current source code position.
         */
        public int pos() {
            return pos;
        }

        /**
         * Update current source code position.
         */
        private int updatePos(int pos, int begin, int end) {
            char[] buf = this.buf;
            for (int i = begin; i < end; i++) {
                char c = buf[i];
                if (c == '\n') {
                    pos = SourcePos.nextline(pos);
                } else if (c == '\t') {
                    pos = SourcePos.nexttab(pos);
                } else {
                    pos++;
                }
            }
            return pos;
        }

        /**
         * Save current read position.
         */
        public void mark() {
            mark = next;
            markpos = pos;
        }

        /**
         * Reset read position to mark.
         */
        public void reset() {
            next = mark;
            pos = markpos;
        }

        /**
         * Raise a lexical error.
         */
        public void fail(String format, Object... args) {
            Message message = new Message.Fail(String.format(format, args));
            throw new ParseError(new SourcePos("", pos), message);
        }
    }

    private static class StringInputBuffer extends InputBuffer {
        private final String input;
        private int offset;

        StringInputBuffer(String input) {
            this.input = input;
            this.offset = 0;
        }

        @Override
        protected int load(char[] buf) {
            int len = Math.min(buf.length, input.length() - offset);
            if (len == 0) {
                return -1;
            } else {
                input.getChars(offset, offset + len, buf, 0);
                offset += len;
                return len;
            }
        }
    }

    private static class ReaderInputBuffer extends InputBuffer {
        private final Reader reader;

        ReaderInputBuffer(Reader reader) {
            this.reader = reader;
        }

        @Override
        protected int load(char[] buf) {
            try {
                return reader.read(buf);
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }
    }
}
