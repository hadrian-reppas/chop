from enum import Enum, auto
from typing import NamedTuple
import re
import os


class Kind(Enum):
    NOTHING = auto()
    COMMENT = auto()
    SYMBOL = auto()
    KEYWORD = auto()
    NAME = auto()
    SPECIAL = auto()
    TYPE = auto()

    INT = auto()
    FLOAT = auto()
    BOOL = auto()
    CHAR = auto()
    BYTE = auto()
    STRING = auto()


format = {
    Kind.NOTHING: None,
    Kind.COMMENT: "comment",
    Kind.SYMBOL: None,
    Kind.KEYWORD: "keyword",
    Kind.NAME: "title",
    Kind.SPECIAL: None,
    Kind.TYPE: "built_in",
    Kind.INT: "number",
    Kind.FLOAT: "number",
    Kind.BOOL: "literal",
    Kind.CHAR: "string",
    Kind.BYTE: "string",
    Kind.STRING: "string",
}


class Token(NamedTuple):
    text: str
    kind: Kind


def tokenize(code):
    while code:
        try_number = lex_number(code)
        if try_number is not None:
            code, token = try_number
            yield token
        elif code[0].isspace():
            c = code[0]
            code = code[1:]
            yield Token(c, Kind.NOTHING)
        elif code[0] in "(){}[]:,":
            c = code[0]
            code = code[1:]
            yield Token(c, Kind.SYMBOL)
        elif code.startswith("->"):
            code = code[2:]
            yield Token("->", Kind.SYMBOL)
        elif code.startswith("//"):
            for i in range(len(code)):
                if code[i] == "\n":
                    break
            comment, code = code[:i], code[i:]
            yield Token(comment, Kind.COMMENT)
        elif code.startswith('"'):
            for i in range(1, len(code)):
                if code[i] == '"':
                    break
            string, code = code[: i + 1], code[i + 1 :]
            yield Token(string, Kind.STRING)
        elif code.startswith("'"):
            for i in range(1, len(code)):
                if code[i] == "'":
                    break
            char, code = code[: i + 1], code[i + 1 :]
            yield Token(char, Kind.STRING)
        elif code.startswith("b'"):
            for i in range(2, len(code)):
                if code[i] == "'":
                    break
            byte, code = code[: i + 1], code[i + 1 :]
            yield Token(byte, Kind.BYTE)
        else:
            code, token = name(code)
            yield token


keywords = {
    "if",
    "else",
    "while",
    "fn",
    "for",
    "to",
    "let",
    "struct",
    "global",
    "import",
    "size_of",
    "cast_to",
    "alloc",
    "zalloc",
    "alloc_arr",
    "zalloc_arr",
    "assert",
    "abort",
    "call",
}
types = {"int", "float", "bool", "byte"}


def lex_number(code):
    pattern = re.compile("^-?((\d*\.\d+)|(\d+\.?))([Ee][+-]?\d+)?")
    m = re.match(pattern, code)
    if m is None:
        return None
    assert m.start() == 0
    num, code = code[: m.end()], code[m.end() :]
    if set(".eE") & set(num):
        return code, Token(num, Kind.FLOAT)
    return code, Token(num, Kind.INT)


def name(code):
    if code[0].isalpha() or code[0] == "_":
        i = 0
        while i < len(code) and (
            code[i].isalpha() or code[i].isdigit() or code[i] == "_"
        ):
            i += 1
        name, code = code[:i], code[i:]
        if name in keywords:
            return code, Token(name, Kind.KEYWORD)
        elif name == "true" or name == "false":
            return code, Token(name, Kind.BOOL)
        elif name in types:
            return code, Token(name, Kind.TYPE)
        return code, Token(name, Kind.NAME)
    else:
        i = 0
        while i < len(code) and is_special_continue(code[i:]):
            i += 1
        name, code = code[:i], code[i:]
        return code, Token(name, Kind.SPECIAL)


def is_special_continue(code):
    return (
        not code[0].isspace()
        and code[0] not in ""
        and not code[0].isdigit()
        and not code[0].isalpha()
        and code[0] != "_"
        and not code.startswith("->")
        and not code.startswith("//")
    )


def highlight(code):
    out = '<pre><code class="language-plaintext hljs">'
    for token in tokenize(code):
        if format[token.kind] is None:
            out += token.text
        else:
            out += f'<span class="hljs-{format[token.kind]}">{token.text}</span>'
    return out + "</code></pre>"


def translate(lines):
    out = ""
    while lines:
        if lines[0].strip() == "```hop":
            for i in range(len(lines)):
                if lines[i].strip() == "```":
                    break
            code = "\n".join(lines[1:i])
            out += highlight(code) + "\n"
            lines = lines[i + 1 :]
        else:
            line, *lines = lines
            out += line + "\n"
    return out


for _, _, file_names in os.walk("./src/"):
    for file_name in file_names:
        if file_name.endswith(".mdx"):
            lines = open(f"./src/{file_name}").read().split("\n")
            out = translate(lines)
            new_file_name = file_name[:-1]
            with open(f"./src/{new_file_name}", "w") as file:
                file.write(out)
