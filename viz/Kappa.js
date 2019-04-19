/* Example definition of a simple mode that understands a subset of
 * JavaScript:
 */

CodeMirror.defineSimpleMode("Kappa", {
  // The start state contains the rules that are intially used
  start: [
      {regex: /\/\/(.*)/, token: "comment"},
      {regex: /\/\*/, token: "comment", push: "comment"},
      {regex: /(\%agent:|\%def:|\%var:|\%plot:|\%obs:|\%init:|\%mod:|\%token:|do|repeat|alarm)\s/,
       token: "keyword"},
      {regex: /(\$ADD|\$DEL|\$SNAPSHOT|\$STOP|\$DIN|\$TRACK|\$UPDATE|\$PRINT)\b/,
       token: "variable-2"},
      {regex: /(\[not\]|\[log\]|\[sin\]|\[cos\]|\[tan\]|\[sqrt\]|\[mod\]|\[exp\]|\[int\]|INF)/,
       token: "atom"},
      {regex: /(\[E\]|\[E\+\]|\[E\-\]|\[Emax\]|\[T\]|\[Tsim\]|\[Tmax\]|\[pi\]|\[true\]|\[false\])/,
       token: "atom"},
      {regex: /'([^']+)'/, token: "variable"},
      {regex: /(\@|\,|\(|\)|\{|\}|\[|\]|\||\.|\+|\*|\-|\^|\/|\<|\>|\=|\%|\:|\#|\;)/,
       token: "variable-2"},
      {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i,
       token: "number"},
      {regex: /"(?:[^\\]|\\.)*?"/, token: "string"},
  ],
  // The multi-line comment state.
    comment: [
        {regex: /\/\/(.*)/, token: "comment"},
        {regex: /\/\*/, token: "comment", push: "comment"},
        {regex: /\*\//, token: "comment", pop: true},
        {regex: /./, token: "comment"}
    ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
  meta: { }
});
