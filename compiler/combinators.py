# Copyright 2014, Jay Conrod. All rights reserved.
#
# This file is part of Gypsum. Use of this source code is governed by
# the GPL license that can be found in the LICENSE.txt file.


from token import Location

class Reader(object):
    def __init__(self, filename, tokens, pos=0):
        self.filename = filename
        self.tokens = tokens
        self.pos = pos

    def isEmpty(self):
        return self.pos == len(self.tokens)

    def token(self):
        assert not self.isEmpty()
        return self.tokens[self.pos]

    def location(self):
        if not self.isEmpty():
            return self.tokens[self.pos].location
        elif len(self.tokens) > 0:
            return self.tokens[-1].location
        else:
            return Location(self.filename, 1, 1, 1, 1)

    def next(self):
        assert not self.isEmpty()
        return Reader(self.filename, self.tokens, self.pos + 1)


class ParseResult(object):
    def __init__(self, location):
        self.location = location


class Success(ParseResult):
    def __init__(self, location, value, next):
        super(Success, self).__init__(location)
        self.value = value;
        self.next = next

    def __nonzero__(self):
        return True

    def __repr__(self):
        return "Success(%s, %s, %s)" % \
          (repr(self.location), repr(self.value), repr(self.next.pos))


class Failure(ParseResult):
    def __init__(self, location, message):
        super(Failure, self).__init__(location)
        self.message = message
        self.retry = True

    def __nonzero__(self):
        return False

    def __repr__(self):
        return "Failure(%s, %s, %s)" % (self.location, self.message, self.retry)


class FailValue(object):
    def __init__(self, message="syntax error"):
        self.message = message


class Parser(object):
    def __xor__(self, other):
        return Process(self, other)

    def __add__(self, other):
        return Concatenate(self, other)

    def __or__(self, other):
        return Alternate(self, other)


class Reserved(Parser):
    def __init__(self, tag, text):
        self.tag = tag
        self.text = text

    def __call__(self, reader):
        if reader.isEmpty():
            return Failure(reader.location(), "unexpected end of file")
        token = reader.token()
        if token.tag is not self.tag or token.text != self.text:
            return Failure(token.location, "expected %s but found %s" % (self.text, token.text))
        else:
            return Success(token.location, token.text, reader.next())

        
class Tag(Parser):
    def __init__(self, tag):
        self.tag = tag

    def __call__(self, reader):
        if reader.isEmpty():
            return Failure(reader.location(), "unexpected end of file")
        token = reader.token()
        if token.tag is not self.tag:
            return Failure(token.location, "expected %s but found %s" % (self.tag, token.tag))
        else:
            return Success(token.location, token.text, reader.next())


class Commit(Parser):
    def __init__(self, parser):
        assert isinstance(parser, Parser)
        self.parser = parser

    def __call__(self, reader):
        result = self.parser(reader)
        if not result:
            result.retry = False
        return result


class Lazy(Parser):
    def __init__(self, parserFunc):
        self.parserFunc = parserFunc
        self.parser = None

    def __call__(self, reader):
        if not self.parser:
            self.parser = self.parserFunc()
        return self.parser(reader)


class Phrase(Parser):
    def __init__(self, parser):
        assert isinstance(parser, Parser)
        self.parser = parser

    def __call__(self, reader):
        result = self.parser(reader)
        if result and not result.next.isEmpty():
            return Failure(result.location, "found garbage at end of file")
        return result


class Process(Parser):
    def __init__(self, parser, process):
        assert isinstance(parser, Parser)
        self.parser = parser
        self.process = process

    def __call__(self, reader):
        result = self.parser(reader)
        if not result:
            return result
        value = self.process(result.value)
        if isinstance(value, FailValue):
            return Failure(result.location, value.message)
        else:
            return Success(result.location, value, result.next)


def If(parser, f):
    return Process(parser, lambda p: p if f(p) else FailValue())


class Opt(Parser):
    def __init__(self, parser):
        assert isinstance(parser, Parser)
        self.parser = parser

    def __call__(self, reader):
        result = self.parser(reader)
        if result or not result.retry:
            return result
        return Success(reader.location(), None, reader)


class Concatenate(Parser):
    def __init__(self, left, right):
        assert isinstance(left, Parser) and isinstance(right, Parser)
        self.left = left
        self.right = right

    def __call__(self, reader):
        left_result = self.left(reader)
        if not left_result:
            return left_result
        right_result = self.right(left_result.next)
        if not right_result:
            return right_result
        return Success(left_result.location.combine(right_result.location),
                       (left_result.value, right_result.value),
                       right_result.next)


class Alternate(Parser):
    def __init__(self, left, right):
        assert isinstance(left, Parser) and isinstance(right, Parser)
        self.left = left
        self.right = right

    def __call__(self, reader):
        result = self.left(reader)
        if result:
            return result
        if not result.retry:
            return result
        return self.right(reader)


class Rep(Parser):
    def __init__(self, parser):
        assert isinstance(parser, Parser)
        self.parser = parser

    def __call__(self, reader):
        elements = []
        location = reader.location()
        next = reader
        result = self.parser(next)
        while result:
            elements.append(result.value)
            location = location.combine(result.location)
            next = result.next
            result = self.parser(next)
        if not result.retry:
            return result
        return Success(location, elements, next)


def Rep1(parser):
    def process(parsed):
        (l, r) = parsed
        return [l] + r
    return parser + Rep(parser) ^ process


def Rep1Sep(parser, separator):
    def processElem(parsed):
        return parsed[1]
    def process(parsed):
        (l, r) = parsed
        return [l] + r
    return parser + Rep(separator + parser ^ processElem) ^ process


def RepSep(parser, separator):
    return Opt(Rep1Sep(parser, separator)) ^ (lambda p: [] if p is None else p)


class LeftRec(Parser):
    def __init__(self, left, next, combine):
        assert isinstance(left, Parser) and isinstance(next, Parser)
        self.left = left
        self.next = next
        self.combine = combine

    def __call__(self, reader):
        result = self.left(reader)
        if not result:
            return result

        while True:
            nextResult = self.next(result.next)
            if not nextResult:
                break

            nextValue = self.combine(result.value, nextResult.value)
            if isinstance(nextValue, FailValue):
                break

            result = nextResult
            result.value = nextValue
        return result


class Break(Parser):
    def __init__(self, parser):
        assert isinstance(parser, Parser)
        self.parser = parser

    def __call__(self, reader):
        import pdb; pdb.set_trace()
        result = self.parser(reader)
        return result


def untangle(parsed):
    if type(parsed) is not tuple:
        return [parsed]
    else:
        elements = []
        for element in parsed:
            elements.extend(untangle(element))
        return elements
