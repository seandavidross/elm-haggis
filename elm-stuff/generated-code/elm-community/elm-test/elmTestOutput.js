// Apply Node polyfills as necessary.
var window = {
  Date: Date,
  addEventListener: function() {},
  removeEventListener: function() {}
};

var location = {
  href: "",
  host: "",
  hostname: "",
  protocol: "",
  origin: "",
  port: "",
  pathname: "",
  search: "",
  hash: "",
  username: "",
  password: ""
};
var document = { body: {}, createTextNode: function() {}, location: location };

if (typeof XMLHttpRequest === "undefined") {
  XMLHttpRequest = function() {
    return {
      addEventListener: function() {},
      open: function() {},
      send: function() {}
    };
  };
}

if (typeof FormData === "undefined") {
  FormData = function() {
    this._data = [];
  };
  FormData.prototype.append = function() {
    this._data.push(Array.prototype.slice.call(arguments));
  };
}

var Elm = (function(module) { 

(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _eeue56$elm_lazy$Lazy$force = function (piece) {
	var _p0 = piece;
	if (_p0.ctor === 'Evaluated') {
		return _p0._0;
	} else {
		return _p0._0(
			{ctor: '_Tuple0'});
	}
};
var _eeue56$elm_lazy$Lazy$Evaluated = function (a) {
	return {ctor: 'Evaluated', _0: a};
};
var _eeue56$elm_lazy$Lazy$evaluate = function (piece) {
	var _p1 = piece;
	if (_p1.ctor === 'Evaluated') {
		return _eeue56$elm_lazy$Lazy$Evaluated(_p1._0);
	} else {
		return _eeue56$elm_lazy$Lazy$Evaluated(
			_p1._0(
				{ctor: '_Tuple0'}));
	}
};
var _eeue56$elm_lazy$Lazy$Lazy = function (a) {
	return {ctor: 'Lazy', _0: a};
};
var _eeue56$elm_lazy$Lazy$lazy = function (thunk) {
	return _eeue56$elm_lazy$Lazy$Lazy(thunk);
};
var _eeue56$elm_lazy$Lazy$map = F2(
	function (f, a) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p2) {
				var _p3 = _p2;
				return f(
					_eeue56$elm_lazy$Lazy$force(a));
			});
	});
var _eeue56$elm_lazy$Lazy$map2 = F3(
	function (f, a, b) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p4) {
				var _p5 = _p4;
				return A2(
					f,
					_eeue56$elm_lazy$Lazy$force(a),
					_eeue56$elm_lazy$Lazy$force(b));
			});
	});
var _eeue56$elm_lazy$Lazy$map3 = F4(
	function (f, a, b, c) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p6) {
				var _p7 = _p6;
				return A3(
					f,
					_eeue56$elm_lazy$Lazy$force(a),
					_eeue56$elm_lazy$Lazy$force(b),
					_eeue56$elm_lazy$Lazy$force(c));
			});
	});
var _eeue56$elm_lazy$Lazy$map4 = F5(
	function (f, a, b, c, d) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p8) {
				var _p9 = _p8;
				return A4(
					f,
					_eeue56$elm_lazy$Lazy$force(a),
					_eeue56$elm_lazy$Lazy$force(b),
					_eeue56$elm_lazy$Lazy$force(c),
					_eeue56$elm_lazy$Lazy$force(d));
			});
	});
var _eeue56$elm_lazy$Lazy$map5 = F6(
	function (f, a, b, c, d, e) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p10) {
				var _p11 = _p10;
				return A5(
					f,
					_eeue56$elm_lazy$Lazy$force(a),
					_eeue56$elm_lazy$Lazy$force(b),
					_eeue56$elm_lazy$Lazy$force(c),
					_eeue56$elm_lazy$Lazy$force(d),
					_eeue56$elm_lazy$Lazy$force(e));
			});
	});
var _eeue56$elm_lazy$Lazy$apply = F2(
	function (f, x) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p12) {
				var _p13 = _p12;
				return A2(
					_eeue56$elm_lazy$Lazy$force,
					f,
					_eeue56$elm_lazy$Lazy$force(x));
			});
	});
var _eeue56$elm_lazy$Lazy$andThen = F2(
	function (callback, a) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p14) {
				var _p15 = _p14;
				return _eeue56$elm_lazy$Lazy$force(
					callback(
						_eeue56$elm_lazy$Lazy$force(a)));
			});
	});

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Random$onSelfMsg = F3(
	function (_p1, _p0, seed) {
		return _elm_lang$core$Task$succeed(seed);
	});
var _elm_lang$core$Random$magicNum8 = 2147483562;
var _elm_lang$core$Random$range = function (_p2) {
	return {ctor: '_Tuple2', _0: 0, _1: _elm_lang$core$Random$magicNum8};
};
var _elm_lang$core$Random$magicNum7 = 2147483399;
var _elm_lang$core$Random$magicNum6 = 2147483563;
var _elm_lang$core$Random$magicNum5 = 3791;
var _elm_lang$core$Random$magicNum4 = 40692;
var _elm_lang$core$Random$magicNum3 = 52774;
var _elm_lang$core$Random$magicNum2 = 12211;
var _elm_lang$core$Random$magicNum1 = 53668;
var _elm_lang$core$Random$magicNum0 = 40014;
var _elm_lang$core$Random$step = F2(
	function (_p3, seed) {
		var _p4 = _p3;
		return _p4._0(seed);
	});
var _elm_lang$core$Random$onEffects = F3(
	function (router, commands, seed) {
		var _p5 = commands;
		if (_p5.ctor === '[]') {
			return _elm_lang$core$Task$succeed(seed);
		} else {
			var _p6 = A2(_elm_lang$core$Random$step, _p5._0._0, seed);
			var value = _p6._0;
			var newSeed = _p6._1;
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p7) {
					return A3(_elm_lang$core$Random$onEffects, router, _p5._1, newSeed);
				},
				A2(_elm_lang$core$Platform$sendToApp, router, value));
		}
	});
var _elm_lang$core$Random$listHelp = F4(
	function (list, n, generate, seed) {
		listHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$reverse(list),
					_1: seed
				};
			} else {
				var _p8 = generate(seed);
				var value = _p8._0;
				var newSeed = _p8._1;
				var _v2 = {ctor: '::', _0: value, _1: list},
					_v3 = n - 1,
					_v4 = generate,
					_v5 = newSeed;
				list = _v2;
				n = _v3;
				generate = _v4;
				seed = _v5;
				continue listHelp;
			}
		}
	});
var _elm_lang$core$Random$minInt = -2147483648;
var _elm_lang$core$Random$maxInt = 2147483647;
var _elm_lang$core$Random$iLogBase = F2(
	function (b, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, b) < 0) ? 1 : (1 + A2(_elm_lang$core$Random$iLogBase, b, (i / b) | 0));
	});
var _elm_lang$core$Random$command = _elm_lang$core$Native_Platform.leaf('Random');
var _elm_lang$core$Random$Generator = function (a) {
	return {ctor: 'Generator', _0: a};
};
var _elm_lang$core$Random$list = F2(
	function (n, _p9) {
		var _p10 = _p9;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				return A4(
					_elm_lang$core$Random$listHelp,
					{ctor: '[]'},
					n,
					_p10._0,
					seed);
			});
	});
var _elm_lang$core$Random$map = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p13 = _p12._0(seed0);
				var a = _p13._0;
				var seed1 = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: func(a),
					_1: seed1
				};
			});
	});
var _elm_lang$core$Random$map2 = F3(
	function (func, _p15, _p14) {
		var _p16 = _p15;
		var _p17 = _p14;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p18 = _p16._0(seed0);
				var a = _p18._0;
				var seed1 = _p18._1;
				var _p19 = _p17._0(seed1);
				var b = _p19._0;
				var seed2 = _p19._1;
				return {
					ctor: '_Tuple2',
					_0: A2(func, a, b),
					_1: seed2
				};
			});
	});
var _elm_lang$core$Random$pair = F2(
	function (genA, genB) {
		return A3(
			_elm_lang$core$Random$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			genA,
			genB);
	});
var _elm_lang$core$Random$map3 = F4(
	function (func, _p22, _p21, _p20) {
		var _p23 = _p22;
		var _p24 = _p21;
		var _p25 = _p20;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p26 = _p23._0(seed0);
				var a = _p26._0;
				var seed1 = _p26._1;
				var _p27 = _p24._0(seed1);
				var b = _p27._0;
				var seed2 = _p27._1;
				var _p28 = _p25._0(seed2);
				var c = _p28._0;
				var seed3 = _p28._1;
				return {
					ctor: '_Tuple2',
					_0: A3(func, a, b, c),
					_1: seed3
				};
			});
	});
var _elm_lang$core$Random$map4 = F5(
	function (func, _p32, _p31, _p30, _p29) {
		var _p33 = _p32;
		var _p34 = _p31;
		var _p35 = _p30;
		var _p36 = _p29;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p37 = _p33._0(seed0);
				var a = _p37._0;
				var seed1 = _p37._1;
				var _p38 = _p34._0(seed1);
				var b = _p38._0;
				var seed2 = _p38._1;
				var _p39 = _p35._0(seed2);
				var c = _p39._0;
				var seed3 = _p39._1;
				var _p40 = _p36._0(seed3);
				var d = _p40._0;
				var seed4 = _p40._1;
				return {
					ctor: '_Tuple2',
					_0: A4(func, a, b, c, d),
					_1: seed4
				};
			});
	});
var _elm_lang$core$Random$map5 = F6(
	function (func, _p45, _p44, _p43, _p42, _p41) {
		var _p46 = _p45;
		var _p47 = _p44;
		var _p48 = _p43;
		var _p49 = _p42;
		var _p50 = _p41;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p51 = _p46._0(seed0);
				var a = _p51._0;
				var seed1 = _p51._1;
				var _p52 = _p47._0(seed1);
				var b = _p52._0;
				var seed2 = _p52._1;
				var _p53 = _p48._0(seed2);
				var c = _p53._0;
				var seed3 = _p53._1;
				var _p54 = _p49._0(seed3);
				var d = _p54._0;
				var seed4 = _p54._1;
				var _p55 = _p50._0(seed4);
				var e = _p55._0;
				var seed5 = _p55._1;
				return {
					ctor: '_Tuple2',
					_0: A5(func, a, b, c, d, e),
					_1: seed5
				};
			});
	});
var _elm_lang$core$Random$andThen = F2(
	function (callback, _p56) {
		var _p57 = _p56;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p58 = _p57._0(seed);
				var result = _p58._0;
				var newSeed = _p58._1;
				var _p59 = callback(result);
				var genB = _p59._0;
				return genB(newSeed);
			});
	});
var _elm_lang$core$Random$State = F2(
	function (a, b) {
		return {ctor: 'State', _0: a, _1: b};
	});
var _elm_lang$core$Random$initState = function (seed) {
	var s = A2(_elm_lang$core$Basics$max, seed, 0 - seed);
	var q = (s / (_elm_lang$core$Random$magicNum6 - 1)) | 0;
	var s2 = A2(_elm_lang$core$Basics_ops['%'], q, _elm_lang$core$Random$magicNum7 - 1);
	var s1 = A2(_elm_lang$core$Basics_ops['%'], s, _elm_lang$core$Random$magicNum6 - 1);
	return A2(_elm_lang$core$Random$State, s1 + 1, s2 + 1);
};
var _elm_lang$core$Random$next = function (_p60) {
	var _p61 = _p60;
	var _p63 = _p61._1;
	var _p62 = _p61._0;
	var k2 = (_p63 / _elm_lang$core$Random$magicNum3) | 0;
	var rawState2 = (_elm_lang$core$Random$magicNum4 * (_p63 - (k2 * _elm_lang$core$Random$magicNum3))) - (k2 * _elm_lang$core$Random$magicNum5);
	var newState2 = (_elm_lang$core$Native_Utils.cmp(rawState2, 0) < 0) ? (rawState2 + _elm_lang$core$Random$magicNum7) : rawState2;
	var k1 = (_p62 / _elm_lang$core$Random$magicNum1) | 0;
	var rawState1 = (_elm_lang$core$Random$magicNum0 * (_p62 - (k1 * _elm_lang$core$Random$magicNum1))) - (k1 * _elm_lang$core$Random$magicNum2);
	var newState1 = (_elm_lang$core$Native_Utils.cmp(rawState1, 0) < 0) ? (rawState1 + _elm_lang$core$Random$magicNum6) : rawState1;
	var z = newState1 - newState2;
	var newZ = (_elm_lang$core$Native_Utils.cmp(z, 1) < 0) ? (z + _elm_lang$core$Random$magicNum8) : z;
	return {
		ctor: '_Tuple2',
		_0: newZ,
		_1: A2(_elm_lang$core$Random$State, newState1, newState2)
	};
};
var _elm_lang$core$Random$split = function (_p64) {
	var _p65 = _p64;
	var _p68 = _p65._1;
	var _p67 = _p65._0;
	var _p66 = _elm_lang$core$Tuple$second(
		_elm_lang$core$Random$next(_p65));
	var t1 = _p66._0;
	var t2 = _p66._1;
	var new_s2 = _elm_lang$core$Native_Utils.eq(_p68, 1) ? (_elm_lang$core$Random$magicNum7 - 1) : (_p68 - 1);
	var new_s1 = _elm_lang$core$Native_Utils.eq(_p67, _elm_lang$core$Random$magicNum6 - 1) ? 1 : (_p67 + 1);
	return {
		ctor: '_Tuple2',
		_0: A2(_elm_lang$core$Random$State, new_s1, t2),
		_1: A2(_elm_lang$core$Random$State, t1, new_s2)
	};
};
var _elm_lang$core$Random$Seed = function (a) {
	return {ctor: 'Seed', _0: a};
};
var _elm_lang$core$Random$int = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (_p69) {
				var _p70 = _p69;
				var _p75 = _p70._0;
				var base = 2147483561;
				var f = F3(
					function (n, acc, state) {
						f:
						while (true) {
							var _p71 = n;
							if (_p71 === 0) {
								return {ctor: '_Tuple2', _0: acc, _1: state};
							} else {
								var _p72 = _p75.next(state);
								var x = _p72._0;
								var nextState = _p72._1;
								var _v27 = n - 1,
									_v28 = x + (acc * base),
									_v29 = nextState;
								n = _v27;
								acc = _v28;
								state = _v29;
								continue f;
							}
						}
					});
				var _p73 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p73._0;
				var hi = _p73._1;
				var k = (hi - lo) + 1;
				var n = A2(_elm_lang$core$Random$iLogBase, base, k);
				var _p74 = A3(f, n, 1, _p75.state);
				var v = _p74._0;
				var nextState = _p74._1;
				return {
					ctor: '_Tuple2',
					_0: lo + A2(_elm_lang$core$Basics_ops['%'], v, k),
					_1: _elm_lang$core$Random$Seed(
						_elm_lang$core$Native_Utils.update(
							_p75,
							{state: nextState}))
				};
			});
	});
var _elm_lang$core$Random$bool = A2(
	_elm_lang$core$Random$map,
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		})(1),
	A2(_elm_lang$core$Random$int, 0, 1));
var _elm_lang$core$Random$float = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p76 = A2(
					_elm_lang$core$Random$step,
					A2(_elm_lang$core$Random$int, _elm_lang$core$Random$minInt, _elm_lang$core$Random$maxInt),
					seed);
				var number = _p76._0;
				var newSeed = _p76._1;
				var negativeOneToOne = _elm_lang$core$Basics$toFloat(number) / _elm_lang$core$Basics$toFloat(_elm_lang$core$Random$maxInt - _elm_lang$core$Random$minInt);
				var _p77 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p77._0;
				var hi = _p77._1;
				var scaled = ((lo + hi) / 2) + ((hi - lo) * negativeOneToOne);
				return {ctor: '_Tuple2', _0: scaled, _1: newSeed};
			});
	});
var _elm_lang$core$Random$initialSeed = function (n) {
	return _elm_lang$core$Random$Seed(
		{
			state: _elm_lang$core$Random$initState(n),
			next: _elm_lang$core$Random$next,
			split: _elm_lang$core$Random$split,
			range: _elm_lang$core$Random$range
		});
};
var _elm_lang$core$Random$init = A2(
	_elm_lang$core$Task$andThen,
	function (t) {
		return _elm_lang$core$Task$succeed(
			_elm_lang$core$Random$initialSeed(
				_elm_lang$core$Basics$round(t)));
	},
	_elm_lang$core$Time$now);
var _elm_lang$core$Random$Generate = function (a) {
	return {ctor: 'Generate', _0: a};
};
var _elm_lang$core$Random$generate = F2(
	function (tagger, generator) {
		return _elm_lang$core$Random$command(
			_elm_lang$core$Random$Generate(
				A2(_elm_lang$core$Random$map, tagger, generator)));
	});
var _elm_lang$core$Random$cmdMap = F2(
	function (func, _p78) {
		var _p79 = _p78;
		return _elm_lang$core$Random$Generate(
			A2(_elm_lang$core$Random$map, func, _p79._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Random'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Random$init, onEffects: _elm_lang$core$Random$onEffects, onSelfMsg: _elm_lang$core$Random$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Random$cmdMap};

var _eeue56$elm_lazy_list$Lazy_List$toArray = function (list) {
	var _p0 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p0.ctor === 'Nil') {
		return _elm_lang$core$Array$empty;
	} else {
		return A2(
			_elm_lang$core$Array$append,
			A2(_elm_lang$core$Array$push, _p0._0, _elm_lang$core$Array$empty),
			_eeue56$elm_lazy_list$Lazy_List$toArray(_p0._1));
	}
};
var _eeue56$elm_lazy_list$Lazy_List$toList = function (list) {
	var _p1 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p1.ctor === 'Nil') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p1._0,
			_1: _eeue56$elm_lazy_list$Lazy_List$toList(_p1._1)
		};
	}
};
var _eeue56$elm_lazy_list$Lazy_List$foldr = F3(
	function (reducer, b, list) {
		return A3(
			_elm_lang$core$Array$foldr,
			reducer,
			b,
			_eeue56$elm_lazy_list$Lazy_List$toArray(list));
	});
var _eeue56$elm_lazy_list$Lazy_List$reduce = F3(
	function (reducer, b, list) {
		reduce:
		while (true) {
			var _p2 = _eeue56$elm_lazy$Lazy$force(list);
			if (_p2.ctor === 'Nil') {
				return b;
			} else {
				var _v3 = reducer,
					_v4 = A2(reducer, _p2._0, b),
					_v5 = _p2._1;
				reducer = _v3;
				b = _v4;
				list = _v5;
				continue reduce;
			}
		}
	});
var _eeue56$elm_lazy_list$Lazy_List$foldl = _eeue56$elm_lazy_list$Lazy_List$reduce;
var _eeue56$elm_lazy_list$Lazy_List$sum = A2(
	_eeue56$elm_lazy_list$Lazy_List$reduce,
	F2(
		function (x, y) {
			return x + y;
		}),
	0);
var _eeue56$elm_lazy_list$Lazy_List$product = A2(
	_eeue56$elm_lazy_list$Lazy_List$reduce,
	F2(
		function (x, y) {
			return x * y;
		}),
	1);
var _eeue56$elm_lazy_list$Lazy_List$length = A2(
	_eeue56$elm_lazy_list$Lazy_List$reduce,
	F2(
		function (_p3, n) {
			return n + 1;
		}),
	0);
var _eeue56$elm_lazy_list$Lazy_List$member = F2(
	function (a, list) {
		var _p4 = _eeue56$elm_lazy$Lazy$force(list);
		if (_p4.ctor === 'Nil') {
			return false;
		} else {
			return _elm_lang$core$Native_Utils.eq(_p4._0, a) || A2(_eeue56$elm_lazy_list$Lazy_List$member, a, _p4._1);
		}
	});
var _eeue56$elm_lazy_list$Lazy_List$headAndTail = function (list) {
	var _p5 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p5.ctor === 'Nil') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p5._0, _1: _p5._1});
	}
};
var _eeue56$elm_lazy_list$Lazy_List$tail = function (list) {
	var _p6 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p6.ctor === 'Nil') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(_p6._1);
	}
};
var _eeue56$elm_lazy_list$Lazy_List$head = function (list) {
	var _p7 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p7.ctor === 'Nil') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(_p7._0);
	}
};
var _eeue56$elm_lazy_list$Lazy_List$isEmpty = function (list) {
	var _p8 = _eeue56$elm_lazy$Lazy$force(list);
	if (_p8.ctor === 'Nil') {
		return true;
	} else {
		return false;
	}
};
var _eeue56$elm_lazy_list$Lazy_List$Cons = F2(
	function (a, b) {
		return {ctor: 'Cons', _0: a, _1: b};
	});
var _eeue56$elm_lazy_list$Lazy_List$cons = F2(
	function (a, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p9) {
				var _p10 = _p9;
				return A2(_eeue56$elm_lazy_list$Lazy_List$Cons, a, list);
			});
	});
var _eeue56$elm_lazy_list$Lazy_List_ops = _eeue56$elm_lazy_list$Lazy_List_ops || {};
_eeue56$elm_lazy_list$Lazy_List_ops[':::'] = _eeue56$elm_lazy_list$Lazy_List$cons;
var _eeue56$elm_lazy_list$Lazy_List$append = F2(
	function (list1, list2) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p11) {
				var _p12 = _p11;
				var _p13 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p13.ctor === 'Nil') {
					return _eeue56$elm_lazy$Lazy$force(list2);
				} else {
					return _eeue56$elm_lazy$Lazy$force(
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
							_p13._0,
							A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], _p13._1, list2)));
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List_ops = _eeue56$elm_lazy_list$Lazy_List_ops || {};
_eeue56$elm_lazy_list$Lazy_List_ops['+++'] = _eeue56$elm_lazy_list$Lazy_List$append;
var _eeue56$elm_lazy_list$Lazy_List$cycle = function (list) {
	return A2(
		_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
		list,
		_eeue56$elm_lazy$Lazy$lazy(
			function (_p14) {
				var _p15 = _p14;
				return _eeue56$elm_lazy$Lazy$force(
					_eeue56$elm_lazy_list$Lazy_List$cycle(list));
			}));
};
var _eeue56$elm_lazy_list$Lazy_List$interleave = F2(
	function (list1, list2) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p16) {
				var _p17 = _p16;
				var _p18 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p18.ctor === 'Nil') {
					return _eeue56$elm_lazy$Lazy$force(list2);
				} else {
					var _p19 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p19.ctor === 'Nil') {
						return _eeue56$elm_lazy$Lazy$force(list1);
					} else {
						return _eeue56$elm_lazy$Lazy$force(
							A2(
								_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
								_p18._0,
								A2(
									_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
									_p19._0,
									A2(_eeue56$elm_lazy_list$Lazy_List$interleave, _p18._1, _p19._1))));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$repeat = function (a) {
	return _eeue56$elm_lazy$Lazy$lazy(
		function (_p20) {
			var _p21 = _p20;
			return A2(
				_eeue56$elm_lazy_list$Lazy_List$Cons,
				a,
				_eeue56$elm_lazy_list$Lazy_List$repeat(a));
		});
};
var _eeue56$elm_lazy_list$Lazy_List$iterate = F2(
	function (f, a) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p22) {
				var _p23 = _p22;
				return A2(
					_eeue56$elm_lazy_list$Lazy_List$Cons,
					a,
					A2(
						_eeue56$elm_lazy_list$Lazy_List$iterate,
						f,
						f(a)));
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$numbers = A2(
	_eeue56$elm_lazy_list$Lazy_List$iterate,
	F2(
		function (x, y) {
			return x + y;
		})(1),
	1);
var _eeue56$elm_lazy_list$Lazy_List$Nil = {ctor: 'Nil'};
var _eeue56$elm_lazy_list$Lazy_List$empty = _eeue56$elm_lazy$Lazy$lazy(
	function (_p24) {
		var _p25 = _p24;
		return _eeue56$elm_lazy_list$Lazy_List$Nil;
	});
var _eeue56$elm_lazy_list$Lazy_List$singleton = function (a) {
	return A2(_eeue56$elm_lazy_list$Lazy_List$cons, a, _eeue56$elm_lazy_list$Lazy_List$empty);
};
var _eeue56$elm_lazy_list$Lazy_List$reverse = A2(_eeue56$elm_lazy_list$Lazy_List$reduce, _eeue56$elm_lazy_list$Lazy_List$cons, _eeue56$elm_lazy_list$Lazy_List$empty);
var _eeue56$elm_lazy_list$Lazy_List$fromList = A2(_elm_lang$core$List$foldr, _eeue56$elm_lazy_list$Lazy_List$cons, _eeue56$elm_lazy_list$Lazy_List$empty);
var _eeue56$elm_lazy_list$Lazy_List$fromArray = A2(_elm_lang$core$Array$foldr, _eeue56$elm_lazy_list$Lazy_List$cons, _eeue56$elm_lazy_list$Lazy_List$empty);
var _eeue56$elm_lazy_list$Lazy_List$intersperse = F2(
	function (a, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p26) {
				var _p27 = _p26;
				var _p28 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p28.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p33 = _p28._0;
					var _p29 = _eeue56$elm_lazy$Lazy$force(_p28._1);
					if (_p29.ctor === 'Nil') {
						return _eeue56$elm_lazy$Lazy$force(
							A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], _p33, _eeue56$elm_lazy_list$Lazy_List$empty));
					} else {
						var _p32 = _p29._1;
						var _p31 = _p29._0;
						var _p30 = _eeue56$elm_lazy$Lazy$force(_p32);
						if (_p30.ctor === 'Nil') {
							return _eeue56$elm_lazy$Lazy$force(
								A2(
									_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
									_p33,
									A2(
										_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
										a,
										A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], _p31, _eeue56$elm_lazy_list$Lazy_List$empty))));
						} else {
							return _eeue56$elm_lazy$Lazy$force(
								A2(
									_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
									_p33,
									A2(
										_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
										a,
										A2(
											_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
											_p31,
											A2(
												_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
												a,
												A2(_eeue56$elm_lazy_list$Lazy_List$intersperse, a, _p32))))));
						}
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$take = F2(
	function (n, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p34) {
				var _p35 = _p34;
				if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p36 = _eeue56$elm_lazy$Lazy$force(list);
					if (_p36.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						return A2(
							_eeue56$elm_lazy_list$Lazy_List$Cons,
							_p36._0,
							A2(_eeue56$elm_lazy_list$Lazy_List$take, n - 1, _p36._1));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$takeWhile = F2(
	function (predicate, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p37) {
				var _p38 = _p37;
				var _p39 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p39.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p40 = _p39._0;
					return predicate(_p40) ? A2(
						_eeue56$elm_lazy_list$Lazy_List$Cons,
						_p40,
						A2(_eeue56$elm_lazy_list$Lazy_List$takeWhile, predicate, _p39._1)) : _eeue56$elm_lazy_list$Lazy_List$Nil;
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$drop = F2(
	function (n, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p41) {
				var _p42 = _p41;
				if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
					return _eeue56$elm_lazy$Lazy$force(list);
				} else {
					var _p43 = _eeue56$elm_lazy$Lazy$force(list);
					if (_p43.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						return _eeue56$elm_lazy$Lazy$force(
							A2(_eeue56$elm_lazy_list$Lazy_List$drop, n - 1, _p43._1));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$dropWhile = F2(
	function (predicate, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p44) {
				var _p45 = _p44;
				var _p46 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p46.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					return predicate(_p46._0) ? _eeue56$elm_lazy$Lazy$force(
						A2(_eeue56$elm_lazy_list$Lazy_List$dropWhile, predicate, _p46._1)) : _eeue56$elm_lazy$Lazy$force(list);
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$unique = function (list) {
	return _eeue56$elm_lazy$Lazy$lazy(
		function (_p47) {
			var _p48 = _p47;
			var _p49 = _eeue56$elm_lazy$Lazy$force(list);
			if (_p49.ctor === 'Nil') {
				return _eeue56$elm_lazy_list$Lazy_List$Nil;
			} else {
				var _p51 = _p49._1;
				var _p50 = _p49._0;
				return A2(_eeue56$elm_lazy_list$Lazy_List$member, _p50, _p51) ? _eeue56$elm_lazy$Lazy$force(
					_eeue56$elm_lazy_list$Lazy_List$unique(_p51)) : A2(
					_eeue56$elm_lazy_list$Lazy_List$Cons,
					_p50,
					_eeue56$elm_lazy_list$Lazy_List$unique(_p51));
			}
		});
};
var _eeue56$elm_lazy_list$Lazy_List$keepIf = F2(
	function (predicate, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p52) {
				var _p53 = _p52;
				var _p54 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p54.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p56 = _p54._1;
					var _p55 = _p54._0;
					return predicate(_p55) ? A2(
						_eeue56$elm_lazy_list$Lazy_List$Cons,
						_p55,
						A2(_eeue56$elm_lazy_list$Lazy_List$keepIf, predicate, _p56)) : _eeue56$elm_lazy$Lazy$force(
						A2(_eeue56$elm_lazy_list$Lazy_List$keepIf, predicate, _p56));
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$dropIf = function (predicate) {
	return _eeue56$elm_lazy_list$Lazy_List$keepIf(
		function (n) {
			return !predicate(n);
		});
};
var _eeue56$elm_lazy_list$Lazy_List$filterMap = F2(
	function (transform, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p57) {
				var _p58 = _p57;
				var _p59 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p59.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p61 = _p59._1;
					var _p60 = transform(_p59._0);
					if (_p60.ctor === 'Just') {
						return A2(
							_eeue56$elm_lazy_list$Lazy_List$Cons,
							_p60._0,
							A2(_eeue56$elm_lazy_list$Lazy_List$filterMap, transform, _p61));
					} else {
						return _eeue56$elm_lazy$Lazy$force(
							A2(_eeue56$elm_lazy_list$Lazy_List$filterMap, transform, _p61));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$flatten = function (list) {
	return _eeue56$elm_lazy$Lazy$lazy(
		function (_p62) {
			var _p63 = _p62;
			var _p64 = _eeue56$elm_lazy$Lazy$force(list);
			if (_p64.ctor === 'Nil') {
				return _eeue56$elm_lazy_list$Lazy_List$Nil;
			} else {
				return _eeue56$elm_lazy$Lazy$force(
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						_p64._0,
						_eeue56$elm_lazy_list$Lazy_List$flatten(_p64._1)));
			}
		});
};
var _eeue56$elm_lazy_list$Lazy_List$map = F2(
	function (f, list) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p65) {
				var _p66 = _p65;
				var _p67 = _eeue56$elm_lazy$Lazy$force(list);
				if (_p67.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					return A2(
						_eeue56$elm_lazy_list$Lazy_List$Cons,
						f(_p67._0),
						A2(_eeue56$elm_lazy_list$Lazy_List$map, f, _p67._1));
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$andThen = F2(
	function (f, list) {
		return _eeue56$elm_lazy_list$Lazy_List$flatten(
			A2(_eeue56$elm_lazy_list$Lazy_List$map, f, list));
	});
var _eeue56$elm_lazy_list$Lazy_List$map2 = F3(
	function (f, list1, list2) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p68) {
				var _p69 = _p68;
				var _p70 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p70.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p71 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p71.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						return A2(
							_eeue56$elm_lazy_list$Lazy_List$Cons,
							A2(f, _p70._0, _p71._0),
							A3(_eeue56$elm_lazy_list$Lazy_List$map2, f, _p70._1, _p71._1));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$andMap = F2(
	function (listVal, listFuncs) {
		return A3(
			_eeue56$elm_lazy_list$Lazy_List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			listFuncs,
			listVal);
	});
var _eeue56$elm_lazy_list$Lazy_List$zip = _eeue56$elm_lazy_list$Lazy_List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _eeue56$elm_lazy_list$Lazy_List$map3 = F4(
	function (f, list1, list2, list3) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p72) {
				var _p73 = _p72;
				var _p74 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p74.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p75 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p75.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p76 = _eeue56$elm_lazy$Lazy$force(list3);
						if (_p76.ctor === 'Nil') {
							return _eeue56$elm_lazy_list$Lazy_List$Nil;
						} else {
							return A2(
								_eeue56$elm_lazy_list$Lazy_List$Cons,
								A3(f, _p74._0, _p75._0, _p76._0),
								A4(_eeue56$elm_lazy_list$Lazy_List$map3, f, _p74._1, _p75._1, _p76._1));
						}
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$zip3 = _eeue56$elm_lazy_list$Lazy_List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _eeue56$elm_lazy_list$Lazy_List$map4 = F5(
	function (f, list1, list2, list3, list4) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p77) {
				var _p78 = _p77;
				var _p79 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p79.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p80 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p80.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p81 = _eeue56$elm_lazy$Lazy$force(list3);
						if (_p81.ctor === 'Nil') {
							return _eeue56$elm_lazy_list$Lazy_List$Nil;
						} else {
							var _p82 = _eeue56$elm_lazy$Lazy$force(list4);
							if (_p82.ctor === 'Nil') {
								return _eeue56$elm_lazy_list$Lazy_List$Nil;
							} else {
								return A2(
									_eeue56$elm_lazy_list$Lazy_List$Cons,
									A4(f, _p79._0, _p80._0, _p81._0, _p82._0),
									A5(_eeue56$elm_lazy_list$Lazy_List$map4, f, _p79._1, _p80._1, _p81._1, _p82._1));
							}
						}
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$zip4 = _eeue56$elm_lazy_list$Lazy_List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _eeue56$elm_lazy_list$Lazy_List$map5 = F6(
	function (f, list1, list2, list3, list4, list5) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p83) {
				var _p84 = _p83;
				var _p85 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p85.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p86 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p86.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p87 = _eeue56$elm_lazy$Lazy$force(list3);
						if (_p87.ctor === 'Nil') {
							return _eeue56$elm_lazy_list$Lazy_List$Nil;
						} else {
							var _p88 = _eeue56$elm_lazy$Lazy$force(list4);
							if (_p88.ctor === 'Nil') {
								return _eeue56$elm_lazy_list$Lazy_List$Nil;
							} else {
								var _p89 = _eeue56$elm_lazy$Lazy$force(list5);
								if (_p89.ctor === 'Nil') {
									return _eeue56$elm_lazy_list$Lazy_List$Nil;
								} else {
									return A2(
										_eeue56$elm_lazy_list$Lazy_List$Cons,
										A5(f, _p85._0, _p86._0, _p87._0, _p88._0, _p89._0),
										A6(_eeue56$elm_lazy_list$Lazy_List$map5, f, _p85._1, _p86._1, _p87._1, _p88._1, _p89._1));
								}
							}
						}
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$zip5 = _eeue56$elm_lazy_list$Lazy_List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _eeue56$elm_lazy_list$Lazy_List$product2 = F2(
	function (list1, list2) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p90) {
				var _p91 = _p90;
				var _p92 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p92.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p93 = _eeue56$elm_lazy$Lazy$force(list2);
					if (_p93.ctor === 'Nil') {
						return _eeue56$elm_lazy_list$Lazy_List$Nil;
					} else {
						return _eeue56$elm_lazy$Lazy$force(
							A2(
								_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
								A2(
									_eeue56$elm_lazy_list$Lazy_List$map,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										})(_p92._0),
									list2),
								A2(_eeue56$elm_lazy_list$Lazy_List$product2, _p92._1, list2)));
					}
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$product3 = F3(
	function (list1, list2, list3) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p94) {
				var _p95 = _p94;
				var _p96 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p96.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					return _eeue56$elm_lazy$Lazy$force(
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A2(
								_eeue56$elm_lazy_list$Lazy_List$map,
								function (_p97) {
									var _p98 = _p97;
									return {ctor: '_Tuple3', _0: _p96._0, _1: _p98._0, _2: _p98._1};
								},
								A2(_eeue56$elm_lazy_list$Lazy_List$product2, list2, list3)),
							A3(_eeue56$elm_lazy_list$Lazy_List$product3, _p96._1, list2, list3)));
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$product4 = F4(
	function (list1, list2, list3, list4) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p99) {
				var _p100 = _p99;
				var _p101 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p101.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					return _eeue56$elm_lazy$Lazy$force(
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A2(
								_eeue56$elm_lazy_list$Lazy_List$map,
								function (_p102) {
									var _p103 = _p102;
									return {ctor: '_Tuple4', _0: _p101._0, _1: _p103._0, _2: _p103._1, _3: _p103._2};
								},
								A3(_eeue56$elm_lazy_list$Lazy_List$product3, list2, list3, list4)),
							A4(_eeue56$elm_lazy_list$Lazy_List$product4, _p101._1, list2, list3, list4)));
				}
			});
	});
var _eeue56$elm_lazy_list$Lazy_List$product5 = F5(
	function (list1, list2, list3, list4, list5) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p104) {
				var _p105 = _p104;
				var _p106 = _eeue56$elm_lazy$Lazy$force(list1);
				if (_p106.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					return _eeue56$elm_lazy$Lazy$force(
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A2(
								_eeue56$elm_lazy_list$Lazy_List$map,
								function (_p107) {
									var _p108 = _p107;
									return {ctor: '_Tuple5', _0: _p106._0, _1: _p108._0, _2: _p108._1, _3: _p108._2, _4: _p108._3};
								},
								A4(_eeue56$elm_lazy_list$Lazy_List$product4, list2, list3, list4, list5)),
							A5(_eeue56$elm_lazy_list$Lazy_List$product5, _p106._1, list2, list3, list4, list5)));
				}
			});
	});

var _eeue56$elm_shrink$Shrink$seriesFloat = F2(
	function (low, high) {
		if (_elm_lang$core$Native_Utils.cmp(low, high - 1.0e-4) > -1) {
			return (!_elm_lang$core$Native_Utils.eq(high, 1.0e-6)) ? _eeue56$elm_lazy_list$Lazy_List$singleton(low + 1.0e-6) : _eeue56$elm_lazy_list$Lazy_List$empty;
		} else {
			var low_ = low + ((high - low) / 2);
			return A2(
				_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
				low,
				A2(_eeue56$elm_shrink$Shrink$seriesFloat, low_, high));
		}
	});
var _eeue56$elm_shrink$Shrink$seriesInt = F2(
	function (low, high) {
		if (_elm_lang$core$Native_Utils.cmp(low, high) > -1) {
			return _eeue56$elm_lazy_list$Lazy_List$empty;
		} else {
			if (_elm_lang$core$Native_Utils.eq(low, high - 1)) {
				return A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], low, _eeue56$elm_lazy_list$Lazy_List$empty);
			} else {
				var low_ = low + (((high - low) / 2) | 0);
				return A2(
					_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
					low,
					A2(_eeue56$elm_shrink$Shrink$seriesInt, low_, high));
			}
		}
	});
var _eeue56$elm_shrink$Shrink$andMap = _eeue56$elm_lazy_list$Lazy_List$andMap;
var _eeue56$elm_shrink$Shrink$map = _eeue56$elm_lazy_list$Lazy_List$map;
var _eeue56$elm_shrink$Shrink$merge = F3(
	function (shrink1, shrink2, a) {
		return _eeue56$elm_lazy_list$Lazy_List$unique(
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				shrink1(a),
				shrink2(a)));
	});
var _eeue56$elm_shrink$Shrink$keepIf = F3(
	function (predicate, shrink, a) {
		return A2(
			_eeue56$elm_lazy_list$Lazy_List$keepIf,
			predicate,
			shrink(a));
	});
var _eeue56$elm_shrink$Shrink$dropIf = function (predicate) {
	return _eeue56$elm_shrink$Shrink$keepIf(
		function (_p0) {
			return !predicate(_p0);
		});
};
var _eeue56$elm_shrink$Shrink$convert = F4(
	function (f, g, shrink, b) {
		return A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			f,
			shrink(
				g(b)));
	});
var _eeue56$elm_shrink$Shrink$tuple5 = F2(
	function (_p2, _p1) {
		var _p3 = _p2;
		var _p14 = _p3._4;
		var _p13 = _p3._3;
		var _p12 = _p3._2;
		var _p11 = _p3._1;
		var _p10 = _p3._0;
		var _p4 = _p1;
		var _p9 = _p4._4;
		var _p8 = _p4._3;
		var _p7 = _p4._2;
		var _p6 = _p4._1;
		var _p5 = _p4._0;
		return A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				function (e) {
					return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: _p7, _3: _p8, _4: e};
				},
				_p14(_p9)),
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					function (d) {
						return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: _p7, _3: d, _4: _p9};
					},
					_p13(_p8)),
				A2(
					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
					A2(
						_eeue56$elm_lazy_list$Lazy_List$map,
						function (c) {
							return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: c, _3: _p8, _4: _p9};
						},
						_p12(_p7)),
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						A2(
							_eeue56$elm_lazy_list$Lazy_List$map,
							function (b) {
								return {ctor: '_Tuple5', _0: _p5, _1: b, _2: _p7, _3: _p8, _4: _p9};
							},
							_p11(_p6)),
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A2(
								_eeue56$elm_lazy_list$Lazy_List$map,
								function (a) {
									return {ctor: '_Tuple5', _0: a, _1: _p6, _2: _p7, _3: _p8, _4: _p9};
								},
								_p10(_p5)),
							A2(
								_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
								A3(
									_eeue56$elm_lazy_list$Lazy_List$map2,
									F2(
										function (d, e) {
											return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: _p7, _3: d, _4: e};
										}),
									_p13(_p8),
									_p14(_p9)),
								A2(
									_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
									A3(
										_eeue56$elm_lazy_list$Lazy_List$map2,
										F2(
											function (c, e) {
												return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: c, _3: _p8, _4: e};
											}),
										_p12(_p7),
										_p14(_p9)),
									A2(
										_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
										A3(
											_eeue56$elm_lazy_list$Lazy_List$map2,
											F2(
												function (b, e) {
													return {ctor: '_Tuple5', _0: _p5, _1: b, _2: _p7, _3: _p8, _4: e};
												}),
											_p11(_p6),
											_p14(_p9)),
										A2(
											_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
											A3(
												_eeue56$elm_lazy_list$Lazy_List$map2,
												F2(
													function (a, e) {
														return {ctor: '_Tuple5', _0: a, _1: _p6, _2: _p7, _3: _p8, _4: e};
													}),
												_p10(_p5),
												_p14(_p9)),
											A2(
												_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
												A3(
													_eeue56$elm_lazy_list$Lazy_List$map2,
													F2(
														function (c, d) {
															return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: c, _3: d, _4: _p9};
														}),
													_p12(_p7),
													_p13(_p8)),
												A2(
													_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
													A3(
														_eeue56$elm_lazy_list$Lazy_List$map2,
														F2(
															function (b, d) {
																return {ctor: '_Tuple5', _0: _p5, _1: b, _2: _p7, _3: d, _4: _p9};
															}),
														_p11(_p6),
														_p13(_p8)),
													A2(
														_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
														A3(
															_eeue56$elm_lazy_list$Lazy_List$map2,
															F2(
																function (a, d) {
																	return {ctor: '_Tuple5', _0: a, _1: _p6, _2: _p7, _3: d, _4: _p9};
																}),
															_p10(_p5),
															_p13(_p8)),
														A2(
															_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
															A3(
																_eeue56$elm_lazy_list$Lazy_List$map2,
																F2(
																	function (b, c) {
																		return {ctor: '_Tuple5', _0: _p5, _1: b, _2: c, _3: _p8, _4: _p9};
																	}),
																_p11(_p6),
																_p12(_p7)),
															A2(
																_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																A3(
																	_eeue56$elm_lazy_list$Lazy_List$map2,
																	F2(
																		function (a, c) {
																			return {ctor: '_Tuple5', _0: a, _1: _p6, _2: c, _3: _p8, _4: _p9};
																		}),
																	_p10(_p5),
																	_p12(_p7)),
																A2(
																	_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																	A3(
																		_eeue56$elm_lazy_list$Lazy_List$map2,
																		F2(
																			function (a, b) {
																				return {ctor: '_Tuple5', _0: a, _1: b, _2: _p7, _3: _p8, _4: _p9};
																			}),
																		_p10(_p5),
																		_p11(_p6)),
																	A2(
																		_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																		A4(
																			_eeue56$elm_lazy_list$Lazy_List$map3,
																			F3(
																				function (a, b, c) {
																					return {ctor: '_Tuple5', _0: a, _1: b, _2: c, _3: _p8, _4: _p9};
																				}),
																			_p10(_p5),
																			_p11(_p6),
																			_p12(_p7)),
																		A2(
																			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																			A4(
																				_eeue56$elm_lazy_list$Lazy_List$map3,
																				F3(
																					function (a, b, d) {
																						return {ctor: '_Tuple5', _0: a, _1: b, _2: _p7, _3: d, _4: _p9};
																					}),
																				_p10(_p5),
																				_p11(_p6),
																				_p13(_p8)),
																			A2(
																				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																				A4(
																					_eeue56$elm_lazy_list$Lazy_List$map3,
																					F3(
																						function (a, c, d) {
																							return {ctor: '_Tuple5', _0: a, _1: _p6, _2: c, _3: d, _4: _p9};
																						}),
																					_p10(_p5),
																					_p12(_p7),
																					_p13(_p8)),
																				A2(
																					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																					A4(
																						_eeue56$elm_lazy_list$Lazy_List$map3,
																						F3(
																							function (b, c, d) {
																								return {ctor: '_Tuple5', _0: _p5, _1: b, _2: c, _3: d, _4: _p9};
																							}),
																						_p11(_p6),
																						_p12(_p7),
																						_p13(_p8)),
																					A2(
																						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																						A4(
																							_eeue56$elm_lazy_list$Lazy_List$map3,
																							F3(
																								function (a, b, e) {
																									return {ctor: '_Tuple5', _0: a, _1: b, _2: _p7, _3: _p8, _4: e};
																								}),
																							_p10(_p5),
																							_p11(_p6),
																							_p14(_p9)),
																						A2(
																							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																							A4(
																								_eeue56$elm_lazy_list$Lazy_List$map3,
																								F3(
																									function (a, c, e) {
																										return {ctor: '_Tuple5', _0: a, _1: _p6, _2: c, _3: _p8, _4: e};
																									}),
																								_p10(_p5),
																								_p12(_p7),
																								_p14(_p9)),
																							A2(
																								_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																								A4(
																									_eeue56$elm_lazy_list$Lazy_List$map3,
																									F3(
																										function (b, c, e) {
																											return {ctor: '_Tuple5', _0: _p5, _1: b, _2: c, _3: _p8, _4: e};
																										}),
																									_p11(_p6),
																									_p12(_p7),
																									_p14(_p9)),
																								A2(
																									_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																									A4(
																										_eeue56$elm_lazy_list$Lazy_List$map3,
																										F3(
																											function (a, d, e) {
																												return {ctor: '_Tuple5', _0: a, _1: _p6, _2: _p7, _3: d, _4: e};
																											}),
																										_p10(_p5),
																										_p13(_p8),
																										_p14(_p9)),
																									A2(
																										_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																										A4(
																											_eeue56$elm_lazy_list$Lazy_List$map3,
																											F3(
																												function (b, d, e) {
																													return {ctor: '_Tuple5', _0: _p5, _1: b, _2: _p7, _3: d, _4: e};
																												}),
																											_p11(_p6),
																											_p13(_p8),
																											_p14(_p9)),
																										A2(
																											_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																											A4(
																												_eeue56$elm_lazy_list$Lazy_List$map3,
																												F3(
																													function (c, d, e) {
																														return {ctor: '_Tuple5', _0: _p5, _1: _p6, _2: c, _3: d, _4: e};
																													}),
																												_p12(_p7),
																												_p13(_p8),
																												_p14(_p9)),
																											A2(
																												_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																												A5(
																													_eeue56$elm_lazy_list$Lazy_List$map4,
																													F4(
																														function (b, c, d, e) {
																															return {ctor: '_Tuple5', _0: _p5, _1: b, _2: c, _3: d, _4: e};
																														}),
																													_p11(_p6),
																													_p12(_p7),
																													_p13(_p8),
																													_p14(_p9)),
																												A2(
																													_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																													A5(
																														_eeue56$elm_lazy_list$Lazy_List$map4,
																														F4(
																															function (a, c, d, e) {
																																return {ctor: '_Tuple5', _0: a, _1: _p6, _2: c, _3: d, _4: e};
																															}),
																														_p10(_p5),
																														_p12(_p7),
																														_p13(_p8),
																														_p14(_p9)),
																													A2(
																														_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																														A5(
																															_eeue56$elm_lazy_list$Lazy_List$map4,
																															F4(
																																function (a, b, d, e) {
																																	return {ctor: '_Tuple5', _0: a, _1: b, _2: _p7, _3: d, _4: e};
																																}),
																															_p10(_p5),
																															_p11(_p6),
																															_p13(_p8),
																															_p14(_p9)),
																														A2(
																															_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																															A5(
																																_eeue56$elm_lazy_list$Lazy_List$map4,
																																F4(
																																	function (a, b, c, d) {
																																		return {ctor: '_Tuple5', _0: a, _1: b, _2: c, _3: d, _4: _p9};
																																	}),
																																_p10(_p5),
																																_p11(_p6),
																																_p12(_p7),
																																_p13(_p8)),
																															A6(
																																_eeue56$elm_lazy_list$Lazy_List$map5,
																																F5(
																																	function (v0, v1, v2, v3, v4) {
																																		return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
																																	}),
																																_p10(_p5),
																																_p11(_p6),
																																_p12(_p7),
																																_p13(_p8),
																																_p14(_p9)))))))))))))))))))))))))))))));
	});
var _eeue56$elm_shrink$Shrink$tuple4 = F2(
	function (_p16, _p15) {
		var _p17 = _p16;
		var _p26 = _p17._3;
		var _p25 = _p17._2;
		var _p24 = _p17._1;
		var _p23 = _p17._0;
		var _p18 = _p15;
		var _p22 = _p18._3;
		var _p21 = _p18._2;
		var _p20 = _p18._1;
		var _p19 = _p18._0;
		return A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				function (d) {
					return {ctor: '_Tuple4', _0: _p19, _1: _p20, _2: _p21, _3: d};
				},
				_p26(_p22)),
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					function (c) {
						return {ctor: '_Tuple4', _0: _p19, _1: _p20, _2: c, _3: _p22};
					},
					_p25(_p21)),
				A2(
					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
					A2(
						_eeue56$elm_lazy_list$Lazy_List$map,
						function (b) {
							return {ctor: '_Tuple4', _0: _p19, _1: b, _2: _p21, _3: _p22};
						},
						_p24(_p20)),
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						A2(
							_eeue56$elm_lazy_list$Lazy_List$map,
							function (a) {
								return {ctor: '_Tuple4', _0: a, _1: _p20, _2: _p21, _3: _p22};
							},
							_p23(_p19)),
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A3(
								_eeue56$elm_lazy_list$Lazy_List$map2,
								F2(
									function (c, d) {
										return {ctor: '_Tuple4', _0: _p19, _1: _p20, _2: c, _3: d};
									}),
								_p25(_p21),
								_p26(_p22)),
							A2(
								_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
								A3(
									_eeue56$elm_lazy_list$Lazy_List$map2,
									F2(
										function (b, d) {
											return {ctor: '_Tuple4', _0: _p19, _1: b, _2: _p21, _3: d};
										}),
									_p24(_p20),
									_p26(_p22)),
								A2(
									_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
									A3(
										_eeue56$elm_lazy_list$Lazy_List$map2,
										F2(
											function (a, d) {
												return {ctor: '_Tuple4', _0: a, _1: _p20, _2: _p21, _3: d};
											}),
										_p23(_p19),
										_p26(_p22)),
									A2(
										_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
										A3(
											_eeue56$elm_lazy_list$Lazy_List$map2,
											F2(
												function (b, c) {
													return {ctor: '_Tuple4', _0: _p19, _1: b, _2: c, _3: _p22};
												}),
											_p24(_p20),
											_p25(_p21)),
										A2(
											_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
											A3(
												_eeue56$elm_lazy_list$Lazy_List$map2,
												F2(
													function (a, c) {
														return {ctor: '_Tuple4', _0: a, _1: _p20, _2: c, _3: _p22};
													}),
												_p23(_p19),
												_p25(_p21)),
											A2(
												_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
												A3(
													_eeue56$elm_lazy_list$Lazy_List$map2,
													F2(
														function (a, b) {
															return {ctor: '_Tuple4', _0: a, _1: b, _2: _p21, _3: _p22};
														}),
													_p23(_p19),
													_p24(_p20)),
												A2(
													_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
													A4(
														_eeue56$elm_lazy_list$Lazy_List$map3,
														F3(
															function (b, c, d) {
																return {ctor: '_Tuple4', _0: _p19, _1: b, _2: c, _3: d};
															}),
														_p24(_p20),
														_p25(_p21),
														_p26(_p22)),
													A2(
														_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
														A4(
															_eeue56$elm_lazy_list$Lazy_List$map3,
															F3(
																function (a, c, d) {
																	return {ctor: '_Tuple4', _0: a, _1: _p20, _2: c, _3: d};
																}),
															_p23(_p19),
															_p25(_p21),
															_p26(_p22)),
														A2(
															_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
															A4(
																_eeue56$elm_lazy_list$Lazy_List$map3,
																F3(
																	function (a, b, d) {
																		return {ctor: '_Tuple4', _0: a, _1: b, _2: _p21, _3: d};
																	}),
																_p23(_p19),
																_p24(_p20),
																_p26(_p22)),
															A2(
																_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
																A4(
																	_eeue56$elm_lazy_list$Lazy_List$map3,
																	F3(
																		function (a, b, c) {
																			return {ctor: '_Tuple4', _0: a, _1: b, _2: c, _3: _p22};
																		}),
																	_p23(_p19),
																	_p24(_p20),
																	_p25(_p21)),
																A5(
																	_eeue56$elm_lazy_list$Lazy_List$map4,
																	F4(
																		function (v0, v1, v2, v3) {
																			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
																		}),
																	_p23(_p19),
																	_p24(_p20),
																	_p25(_p21),
																	_p26(_p22))))))))))))))));
	});
var _eeue56$elm_shrink$Shrink$tuple3 = F2(
	function (_p28, _p27) {
		var _p29 = _p28;
		var _p36 = _p29._2;
		var _p35 = _p29._1;
		var _p34 = _p29._0;
		var _p30 = _p27;
		var _p33 = _p30._2;
		var _p32 = _p30._1;
		var _p31 = _p30._0;
		return A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				function (c) {
					return {ctor: '_Tuple3', _0: _p31, _1: _p32, _2: c};
				},
				_p36(_p33)),
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					function (b) {
						return {ctor: '_Tuple3', _0: _p31, _1: b, _2: _p33};
					},
					_p35(_p32)),
				A2(
					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
					A2(
						_eeue56$elm_lazy_list$Lazy_List$map,
						function (a) {
							return {ctor: '_Tuple3', _0: a, _1: _p32, _2: _p33};
						},
						_p34(_p31)),
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						A3(
							_eeue56$elm_lazy_list$Lazy_List$map2,
							F2(
								function (b, c) {
									return {ctor: '_Tuple3', _0: _p31, _1: b, _2: c};
								}),
							_p35(_p32),
							_p36(_p33)),
						A2(
							_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
							A3(
								_eeue56$elm_lazy_list$Lazy_List$map2,
								F2(
									function (a, c) {
										return {ctor: '_Tuple3', _0: a, _1: _p32, _2: c};
									}),
								_p34(_p31),
								_p36(_p33)),
							A2(
								_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
								A3(
									_eeue56$elm_lazy_list$Lazy_List$map2,
									F2(
										function (a, b) {
											return {ctor: '_Tuple3', _0: a, _1: b, _2: _p33};
										}),
									_p34(_p31),
									_p35(_p32)),
								A4(
									_eeue56$elm_lazy_list$Lazy_List$map3,
									F3(
										function (v0, v1, v2) {
											return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
										}),
									_p34(_p31),
									_p35(_p32),
									_p36(_p33))))))));
	});
var _eeue56$elm_shrink$Shrink$tuple = F2(
	function (_p38, _p37) {
		var _p39 = _p38;
		var _p44 = _p39._1;
		var _p43 = _p39._0;
		var _p40 = _p37;
		var _p42 = _p40._1;
		var _p41 = _p40._0;
		return A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					})(_p41),
				_p44(_p42)),
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					A2(
						_elm_lang$core$Basics$flip,
						F2(
							function (v0, v1) {
								return {ctor: '_Tuple2', _0: v0, _1: v1};
							}),
						_p42),
					_p43(_p41)),
				A3(
					_eeue56$elm_lazy_list$Lazy_List$map2,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p43(_p41),
					_p44(_p42))));
	});
var _eeue56$elm_shrink$Shrink$lazylist = F2(
	function (shrink, l) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p45) {
				var _p46 = _p45;
				var removes = F3(
					function (k, n, l) {
						return _eeue56$elm_lazy$Lazy$lazy(
							function (_p47) {
								var _p48 = _p47;
								if (_elm_lang$core$Native_Utils.cmp(k, n) > 0) {
									return _eeue56$elm_lazy$Lazy$force(_eeue56$elm_lazy_list$Lazy_List$empty);
								} else {
									if (_eeue56$elm_lazy_list$Lazy_List$isEmpty(l)) {
										return _eeue56$elm_lazy$Lazy$force(
											A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], _eeue56$elm_lazy_list$Lazy_List$empty, _eeue56$elm_lazy_list$Lazy_List$empty));
									} else {
										var rest = A2(_eeue56$elm_lazy_list$Lazy_List$drop, k, l);
										var first = A2(_eeue56$elm_lazy_list$Lazy_List$take, k, l);
										return _eeue56$elm_lazy$Lazy$force(
											A2(
												_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
												rest,
												A2(
													_eeue56$elm_lazy_list$Lazy_List$map,
													F2(
														function (x, y) {
															return A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], x, y);
														})(first),
													A3(removes, k, n - k, rest))));
									}
								}
							});
					});
				var shrinkOne = function (l) {
					return _eeue56$elm_lazy$Lazy$lazy(
						function (_p49) {
							var _p50 = _p49;
							var _p51 = _eeue56$elm_lazy$Lazy$force(l);
							if (_p51.ctor === 'Nil') {
								return _eeue56$elm_lazy$Lazy$force(_eeue56$elm_lazy_list$Lazy_List$empty);
							} else {
								var _p53 = _p51._1;
								var _p52 = _p51._0;
								return _eeue56$elm_lazy$Lazy$force(
									A2(
										_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
										A2(
											_eeue56$elm_lazy_list$Lazy_List$map,
											A2(
												_elm_lang$core$Basics$flip,
												F2(
													function (x, y) {
														return A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], x, y);
													}),
												_p53),
											shrink(_p52)),
										A2(
											_eeue56$elm_lazy_list$Lazy_List$map,
											F2(
												function (x, y) {
													return A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], x, y);
												})(_p52),
											shrinkOne(_p53))));
							}
						});
				};
				var n = _eeue56$elm_lazy_list$Lazy_List$length(l);
				return _eeue56$elm_lazy$Lazy$force(
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						A2(
							_eeue56$elm_lazy_list$Lazy_List$andThen,
							function (k) {
								return A3(removes, k, n, l);
							},
							A2(
								_eeue56$elm_lazy_list$Lazy_List$takeWhile,
								function (x) {
									return _elm_lang$core$Native_Utils.cmp(x, 0) > 0;
								},
								A2(
									_eeue56$elm_lazy_list$Lazy_List$iterate,
									function (n) {
										return (n / 2) | 0;
									},
									n))),
						shrinkOne(l)));
			});
	});
var _eeue56$elm_shrink$Shrink$list = function (shrink) {
	return A3(
		_eeue56$elm_shrink$Shrink$convert,
		_eeue56$elm_lazy_list$Lazy_List$toList,
		_eeue56$elm_lazy_list$Lazy_List$fromList,
		_eeue56$elm_shrink$Shrink$lazylist(shrink));
};
var _eeue56$elm_shrink$Shrink$array = function (shrink) {
	return A3(
		_eeue56$elm_shrink$Shrink$convert,
		_eeue56$elm_lazy_list$Lazy_List$toArray,
		_eeue56$elm_lazy_list$Lazy_List$fromArray,
		_eeue56$elm_shrink$Shrink$lazylist(shrink));
};
var _eeue56$elm_shrink$Shrink$result = F3(
	function (shrinkError, shrinkValue, r) {
		var _p54 = r;
		if (_p54.ctor === 'Ok') {
			return A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				_elm_lang$core$Result$Ok,
				shrinkValue(_p54._0));
		} else {
			return A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				_elm_lang$core$Result$Err,
				shrinkError(_p54._0));
		}
	});
var _eeue56$elm_shrink$Shrink$maybe = F2(
	function (shrink, m) {
		var _p55 = m;
		if (_p55.ctor === 'Just') {
			return A2(
				_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
				_elm_lang$core$Maybe$Nothing,
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					_elm_lang$core$Maybe$Just,
					shrink(_p55._0)));
		} else {
			return _eeue56$elm_lazy_list$Lazy_List$empty;
		}
	});
var _eeue56$elm_shrink$Shrink$atLeastFloat = F2(
	function (min, n) {
		return ((_elm_lang$core$Native_Utils.cmp(n, 0) < 0) && (_elm_lang$core$Native_Utils.cmp(n, min) > -1)) ? A2(
			_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
			0 - n,
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				F2(
					function (x, y) {
						return x * y;
					})(-1),
				A2(_eeue56$elm_shrink$Shrink$seriesFloat, 0, 0 - n))) : A2(
			_eeue56$elm_shrink$Shrink$seriesFloat,
			A2(_elm_lang$core$Basics$max, 0, min),
			n);
	});
var _eeue56$elm_shrink$Shrink$float = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 0) < 0) ? A2(
		_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
		0 - n,
		A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			F2(
				function (x, y) {
					return x * y;
				})(-1),
			A2(_eeue56$elm_shrink$Shrink$seriesFloat, 0, 0 - n))) : A2(_eeue56$elm_shrink$Shrink$seriesFloat, 0, n);
};
var _eeue56$elm_shrink$Shrink$atLeastInt = F2(
	function (min, n) {
		return ((_elm_lang$core$Native_Utils.cmp(n, 0) < 0) && (_elm_lang$core$Native_Utils.cmp(n, min) > -1)) ? A2(
			_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
			0 - n,
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				F2(
					function (x, y) {
						return x * y;
					})(-1),
				A2(_eeue56$elm_shrink$Shrink$seriesInt, 0, 0 - n))) : A2(
			_eeue56$elm_shrink$Shrink$seriesInt,
			A2(_elm_lang$core$Basics$max, 0, min),
			n);
	});
var _eeue56$elm_shrink$Shrink$atLeastChar = function ($char) {
	return A3(
		_eeue56$elm_shrink$Shrink$convert,
		_elm_lang$core$Char$fromCode,
		_elm_lang$core$Char$toCode,
		_eeue56$elm_shrink$Shrink$atLeastInt(
			_elm_lang$core$Char$toCode($char)));
};
var _eeue56$elm_shrink$Shrink$character = _eeue56$elm_shrink$Shrink$atLeastChar(
	_elm_lang$core$Char$fromCode(32));
var _eeue56$elm_shrink$Shrink$string = A3(
	_eeue56$elm_shrink$Shrink$convert,
	_elm_lang$core$String$fromList,
	_elm_lang$core$String$toList,
	_eeue56$elm_shrink$Shrink$list(_eeue56$elm_shrink$Shrink$character));
var _eeue56$elm_shrink$Shrink$int = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 0) < 0) ? A2(
		_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
		0 - n,
		A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			F2(
				function (x, y) {
					return x * y;
				})(-1),
			A2(_eeue56$elm_shrink$Shrink$seriesInt, 0, 0 - n))) : A2(_eeue56$elm_shrink$Shrink$seriesInt, 0, n);
};
var _eeue56$elm_shrink$Shrink$char = A3(_eeue56$elm_shrink$Shrink$convert, _elm_lang$core$Char$fromCode, _elm_lang$core$Char$toCode, _eeue56$elm_shrink$Shrink$int);
var _eeue56$elm_shrink$Shrink$order = function (o) {
	var _p56 = o;
	switch (_p56.ctor) {
		case 'GT':
			return A2(
				_eeue56$elm_lazy_list$Lazy_List_ops[':::'],
				_elm_lang$core$Basics$EQ,
				A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], _elm_lang$core$Basics$LT, _eeue56$elm_lazy_list$Lazy_List$empty));
		case 'LT':
			return A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], _elm_lang$core$Basics$EQ, _eeue56$elm_lazy_list$Lazy_List$empty);
		default:
			return _eeue56$elm_lazy_list$Lazy_List$empty;
	}
};
var _eeue56$elm_shrink$Shrink$bool = function (b) {
	var _p57 = b;
	if (_p57 === true) {
		return A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], false, _eeue56$elm_lazy_list$Lazy_List$empty);
	} else {
		return _eeue56$elm_lazy_list$Lazy_List$empty;
	}
};
var _eeue56$elm_shrink$Shrink$noShrink = function (_p58) {
	return _eeue56$elm_lazy_list$Lazy_List$empty;
};
var _eeue56$elm_shrink$Shrink$unit = _eeue56$elm_shrink$Shrink$noShrink;
var _eeue56$elm_shrink$Shrink$shrink = F3(
	function (keepShrinking, shrinker, originalVal) {
		var helper = F2(
			function (lazyList, val) {
				helper:
				while (true) {
					var _p59 = _eeue56$elm_lazy$Lazy$force(lazyList);
					if (_p59.ctor === 'Nil') {
						return val;
					} else {
						var _p60 = _p59._0;
						if (keepShrinking(_p60)) {
							var _v17 = shrinker(_p60),
								_v18 = _p60;
							lazyList = _v17;
							val = _v18;
							continue helper;
						} else {
							var _v19 = _p59._1,
								_v20 = val;
							lazyList = _v19;
							val = _v20;
							continue helper;
						}
					}
				}
			});
		return A2(
			helper,
			shrinker(originalVal),
			originalVal);
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$elm_test$Test_Runner_Failure$verticalBar = F3(
	function (comparison, expected, actual) {
		return A2(
			_elm_lang$core$String$join,
			'\n',
			{
				ctor: '::',
				_0: actual,
				_1: {
					ctor: '::',
					_0: '╵',
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Basics_ops['++'], '│ ', comparison),
						_1: {
							ctor: '::',
							_0: '╷',
							_1: {
								ctor: '::',
								_0: expected,
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
	});
var _elm_community$elm_test$Test_Runner_Failure$listDiffToString = F4(
	function (index, description, _p0, originals) {
		listDiffToString:
		while (true) {
			var _p1 = _p0;
			var _p2 = {ctor: '_Tuple2', _0: _p1.expected, _1: _p1.actual};
			if (_p2._0.ctor === '[]') {
				if (_p2._1.ctor === '[]') {
					return A2(
						_elm_lang$core$String$join,
						'',
						{
							ctor: '::',
							_0: 'Two lists were unequal previously, yet ended up equal later.',
							_1: {
								ctor: '::',
								_0: 'This should never happen!',
								_1: {
									ctor: '::',
									_0: 'Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: ',
									_1: {
										ctor: '::',
										_0: '\n',
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Basics$toString(originals.originalExpected),
											_1: {
												ctor: '::',
												_0: '\n',
												_1: {
													ctor: '::',
													_0: _elm_lang$core$Basics$toString(originals.originalActual),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						});
				} else {
					return A3(
						_elm_community$elm_test$Test_Runner_Failure$verticalBar,
						A2(_elm_lang$core$Basics_ops['++'], description, ' was longer than'),
						_elm_lang$core$Basics$toString(originals.originalExpected),
						_elm_lang$core$Basics$toString(originals.originalActual));
				}
			} else {
				if (_p2._1.ctor === '[]') {
					return A3(
						_elm_community$elm_test$Test_Runner_Failure$verticalBar,
						A2(_elm_lang$core$Basics_ops['++'], description, ' was shorter than'),
						_elm_lang$core$Basics$toString(originals.originalExpected),
						_elm_lang$core$Basics$toString(originals.originalActual));
				} else {
					var _p4 = _p2._0._0;
					var _p3 = _p2._1._0;
					if (_elm_lang$core$Native_Utils.eq(_p4, _p3)) {
						var _v2 = index + 1,
							_v3 = description,
							_v4 = {expected: _p2._0._1, actual: _p2._1._1},
							_v5 = originals;
						index = _v2;
						description = _v3;
						_p0 = _v4;
						originals = _v5;
						continue listDiffToString;
					} else {
						return A2(
							_elm_lang$core$String$join,
							'',
							{
								ctor: '::',
								_0: A3(
									_elm_community$elm_test$Test_Runner_Failure$verticalBar,
									description,
									_elm_lang$core$Basics$toString(originals.originalExpected),
									_elm_lang$core$Basics$toString(originals.originalActual)),
								_1: {
									ctor: '::',
									_0: '\n\nThe first diff is at index ',
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Basics$toString(index),
										_1: {
											ctor: '::',
											_0: ': it was `',
											_1: {
												ctor: '::',
												_0: _p3,
												_1: {
													ctor: '::',
													_0: '`, but `',
													_1: {
														ctor: '::',
														_0: _p4,
														_1: {
															ctor: '::',
															_0: '` was expected.',
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							});
					}
				}
			}
		}
	});
var _elm_community$elm_test$Test_Runner_Failure$format = F2(
	function (description, reason) {
		var _p5 = reason;
		switch (_p5.ctor) {
			case 'Custom':
				return description;
			case 'Equality':
				return A3(_elm_community$elm_test$Test_Runner_Failure$verticalBar, description, _p5._0, _p5._1);
			case 'Comparison':
				return A3(_elm_community$elm_test$Test_Runner_Failure$verticalBar, description, _p5._0, _p5._1);
			case 'TODO':
				return description;
			case 'Invalid':
				if (_p5._0.ctor === 'BadDescription') {
					return _elm_lang$core$Native_Utils.eq(description, '') ? 'The empty string is not a valid test description.' : A2(_elm_lang$core$Basics_ops['++'], 'This is an invalid test description: ', description);
				} else {
					return description;
				}
			case 'ListDiff':
				var _p7 = _p5._0;
				var _p6 = _p5._1;
				return A4(
					_elm_community$elm_test$Test_Runner_Failure$listDiffToString,
					0,
					description,
					{expected: _p7, actual: _p6},
					{originalExpected: _p7, originalActual: _p6});
			default:
				var _p9 = _p5._0.missing;
				var _p8 = _p5._0.extra;
				var missingStr = _elm_lang$core$List$isEmpty(_p9) ? '' : A2(
					_elm_lang$core$Basics_ops['++'],
					'\nThese keys are missing: ',
					function (d) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'[ ',
							A2(_elm_lang$core$Basics_ops['++'], d, ' ]'));
					}(
						A2(_elm_lang$core$String$join, ', ', _p9)));
				var extraStr = _elm_lang$core$List$isEmpty(_p8) ? '' : A2(
					_elm_lang$core$Basics_ops['++'],
					'\nThese keys are extra: ',
					function (d) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'[ ',
							A2(_elm_lang$core$Basics_ops['++'], d, ' ]'));
					}(
						A2(_elm_lang$core$String$join, ', ', _p8)));
				return A2(
					_elm_lang$core$String$join,
					'',
					{
						ctor: '::',
						_0: A3(_elm_community$elm_test$Test_Runner_Failure$verticalBar, description, _p5._0.expected, _p5._0.actual),
						_1: {
							ctor: '::',
							_0: '\n',
							_1: {
								ctor: '::',
								_0: extraStr,
								_1: {
									ctor: '::',
									_0: missingStr,
									_1: {ctor: '[]'}
								}
							}
						}
					});
		}
	});
var _elm_community$elm_test$Test_Runner_Failure$Invalid = function (a) {
	return {ctor: 'Invalid', _0: a};
};
var _elm_community$elm_test$Test_Runner_Failure$TODO = {ctor: 'TODO'};
var _elm_community$elm_test$Test_Runner_Failure$CollectionDiff = function (a) {
	return {ctor: 'CollectionDiff', _0: a};
};
var _elm_community$elm_test$Test_Runner_Failure$ListDiff = F2(
	function (a, b) {
		return {ctor: 'ListDiff', _0: a, _1: b};
	});
var _elm_community$elm_test$Test_Runner_Failure$Comparison = F2(
	function (a, b) {
		return {ctor: 'Comparison', _0: a, _1: b};
	});
var _elm_community$elm_test$Test_Runner_Failure$Equality = F2(
	function (a, b) {
		return {ctor: 'Equality', _0: a, _1: b};
	});
var _elm_community$elm_test$Test_Runner_Failure$Custom = {ctor: 'Custom'};
var _elm_community$elm_test$Test_Runner_Failure$DuplicatedName = {ctor: 'DuplicatedName'};
var _elm_community$elm_test$Test_Runner_Failure$BadDescription = {ctor: 'BadDescription'};
var _elm_community$elm_test$Test_Runner_Failure$InvalidFuzzer = {ctor: 'InvalidFuzzer'};
var _elm_community$elm_test$Test_Runner_Failure$NonpositiveFuzzCount = {ctor: 'NonpositiveFuzzCount'};
var _elm_community$elm_test$Test_Runner_Failure$EmptyList = {ctor: 'EmptyList'};

var _elm_community$elm_test$Test_Expectation$Fail = function (a) {
	return {ctor: 'Fail', _0: a};
};
var _elm_community$elm_test$Test_Expectation$fail = function (_p0) {
	var _p1 = _p0;
	return _elm_community$elm_test$Test_Expectation$Fail(
		{given: _elm_lang$core$Maybe$Nothing, description: _p1.description, reason: _p1.reason});
};
var _elm_community$elm_test$Test_Expectation$withGiven = F2(
	function (newGiven, expectation) {
		var _p2 = expectation;
		if (_p2.ctor === 'Fail') {
			return _elm_community$elm_test$Test_Expectation$Fail(
				_elm_lang$core$Native_Utils.update(
					_p2._0,
					{
						given: _elm_lang$core$Maybe$Just(newGiven)
					}));
		} else {
			return expectation;
		}
	});
var _elm_community$elm_test$Test_Expectation$Pass = {ctor: 'Pass'};

var _elm_community$elm_test$Expect$relative = function (tolerance) {
	var _p0 = tolerance;
	switch (_p0.ctor) {
		case 'Relative':
			return _p0._0;
		case 'AbsoluteOrRelative':
			return _p0._1;
		default:
			return 0;
	}
};
var _elm_community$elm_test$Expect$absolute = function (tolerance) {
	var _p1 = tolerance;
	switch (_p1.ctor) {
		case 'Absolute':
			return _p1._0;
		case 'AbsoluteOrRelative':
			return _p1._0;
		default:
			return 0;
	}
};
var _elm_community$elm_test$Expect$nonNegativeToleranceError = F3(
	function (tolerance, name, result) {
		return ((_elm_lang$core$Native_Utils.cmp(
			_elm_community$elm_test$Expect$absolute(tolerance),
			0) < 0) && (_elm_lang$core$Native_Utils.cmp(
			_elm_community$elm_test$Expect$relative(tolerance),
			0) < 0)) ? _elm_community$elm_test$Test_Expectation$fail(
			{
				description: A2(
					_elm_lang$core$Basics_ops['++'],
					'Expect.',
					A2(_elm_lang$core$Basics_ops['++'], name, ' was given negative absolute and relative tolerances')),
				reason: _elm_community$elm_test$Test_Runner_Failure$Custom
			}) : ((_elm_lang$core$Native_Utils.cmp(
			_elm_community$elm_test$Expect$absolute(tolerance),
			0) < 0) ? _elm_community$elm_test$Test_Expectation$fail(
			{
				description: A2(
					_elm_lang$core$Basics_ops['++'],
					'Expect.',
					A2(_elm_lang$core$Basics_ops['++'], name, ' was given a negative absolute tolerance')),
				reason: _elm_community$elm_test$Test_Runner_Failure$Custom
			}) : ((_elm_lang$core$Native_Utils.cmp(
			_elm_community$elm_test$Expect$relative(tolerance),
			0) < 0) ? _elm_community$elm_test$Test_Expectation$fail(
			{
				description: A2(
					_elm_lang$core$Basics_ops['++'],
					'Expect.',
					A2(_elm_lang$core$Basics_ops['++'], name, ' was given a negative relative tolerance')),
				reason: _elm_community$elm_test$Test_Runner_Failure$Custom
			}) : result));
	});
var _elm_community$elm_test$Expect$withinCompare = F3(
	function (tolerance, a, b) {
		var withinRelativeTolerance = ((_elm_lang$core$Native_Utils.cmp(
			a * (1 - _elm_community$elm_test$Expect$relative(tolerance)),
			b) < 1) && (_elm_lang$core$Native_Utils.cmp(
			b,
			a * (1 + _elm_community$elm_test$Expect$relative(tolerance))) < 1)) || ((_elm_lang$core$Native_Utils.cmp(
			b * (1 - _elm_community$elm_test$Expect$relative(tolerance)),
			a) < 1) && (_elm_lang$core$Native_Utils.cmp(
			a,
			b * (1 + _elm_community$elm_test$Expect$relative(tolerance))) < 1));
		var withinAbsoluteTolerance = (_elm_lang$core$Native_Utils.cmp(
			a - _elm_community$elm_test$Expect$absolute(tolerance),
			b) < 1) && (_elm_lang$core$Native_Utils.cmp(
			b,
			a + _elm_community$elm_test$Expect$absolute(tolerance)) < 1);
		return _elm_lang$core$Native_Utils.eq(a, b) || (withinAbsoluteTolerance || withinRelativeTolerance);
	});
var _elm_community$elm_test$Expect$reportCollectionFailure = F5(
	function (comparison, expected, actual, missingKeys, extraKeys) {
		return _elm_community$elm_test$Test_Expectation$fail(
			{
				description: comparison,
				reason: _elm_community$elm_test$Test_Runner_Failure$CollectionDiff(
					{
						expected: _elm_lang$core$Basics$toString(expected),
						actual: _elm_lang$core$Basics$toString(actual),
						extra: A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, extraKeys),
						missing: A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, missingKeys)
					})
			});
	});
var _elm_community$elm_test$Expect$reportFailure = F3(
	function (comparison, expected, actual) {
		return _elm_community$elm_test$Test_Expectation$fail(
			{
				description: comparison,
				reason: A2(
					_elm_community$elm_test$Test_Runner_Failure$Comparison,
					_elm_lang$core$Basics$toString(expected),
					_elm_lang$core$Basics$toString(actual))
			});
	});
var _elm_community$elm_test$Expect$onFail = F2(
	function (str, expectation) {
		var _p2 = expectation;
		if (_p2.ctor === 'Pass') {
			return expectation;
		} else {
			return _elm_community$elm_test$Test_Expectation$Fail(
				_elm_lang$core$Native_Utils.update(
					_p2._0,
					{description: str, reason: _elm_community$elm_test$Test_Runner_Failure$Custom}));
		}
	});
var _elm_community$elm_test$Expect$fail = function (str) {
	return _elm_community$elm_test$Test_Expectation$fail(
		{description: str, reason: _elm_community$elm_test$Test_Runner_Failure$Custom});
};
var _elm_community$elm_test$Expect$pass = _elm_community$elm_test$Test_Expectation$Pass;
var _elm_community$elm_test$Expect$allHelp = F2(
	function (list, query) {
		allHelp:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return _elm_community$elm_test$Expect$pass;
			} else {
				var _p4 = _p3._0(query);
				if (_p4.ctor === 'Pass') {
					var _v5 = _p3._1,
						_v6 = query;
					list = _v5;
					query = _v6;
					continue allHelp;
				} else {
					return _p4;
				}
			}
		}
	});
var _elm_community$elm_test$Expect$all = F2(
	function (list, query) {
		return _elm_lang$core$List$isEmpty(list) ? _elm_community$elm_test$Test_Expectation$fail(
			{
				reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$EmptyList),
				description: 'Expect.all was given an empty list. You must make at least one expectation to have a valid test!'
			}) : A2(_elm_community$elm_test$Expect$allHelp, list, query);
	});
var _elm_community$elm_test$Expect$testWith = F5(
	function (makeReason, label, runTest, expected, actual) {
		return A2(runTest, actual, expected) ? _elm_community$elm_test$Expect$pass : _elm_community$elm_test$Test_Expectation$fail(
			{
				description: label,
				reason: A2(
					makeReason,
					_elm_lang$core$Basics$toString(expected),
					_elm_lang$core$Basics$toString(actual))
			});
	});
var _elm_community$elm_test$Expect$equateWith = _elm_community$elm_test$Expect$testWith(_elm_community$elm_test$Test_Runner_Failure$Equality);
var _elm_community$elm_test$Expect$compareWith = _elm_community$elm_test$Expect$testWith(_elm_community$elm_test$Test_Runner_Failure$Comparison);
var _elm_community$elm_test$Expect$equalSets = F2(
	function (expected, actual) {
		if (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$Set$toList(expected),
			_elm_lang$core$Set$toList(actual))) {
			return _elm_community$elm_test$Expect$pass;
		} else {
			var extraKeys = _elm_lang$core$Set$toList(
				A2(_elm_lang$core$Set$diff, actual, expected));
			var missingKeys = _elm_lang$core$Set$toList(
				A2(_elm_lang$core$Set$diff, expected, actual));
			return A5(_elm_community$elm_test$Expect$reportCollectionFailure, 'Expect.equalSets', expected, actual, missingKeys, extraKeys);
		}
	});
var _elm_community$elm_test$Expect$equalDicts = F2(
	function (expected, actual) {
		if (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$Dict$toList(expected),
			_elm_lang$core$Dict$toList(actual))) {
			return _elm_community$elm_test$Expect$pass;
		} else {
			var differ = F4(
				function (dict, k, v, diffs) {
					return _elm_lang$core$Native_Utils.eq(
						A2(_elm_lang$core$Dict$get, k, dict),
						_elm_lang$core$Maybe$Just(v)) ? diffs : {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: k, _1: v},
						_1: diffs
					};
				});
			var missingKeys = A3(
				_elm_lang$core$Dict$foldr,
				differ(actual),
				{ctor: '[]'},
				expected);
			var extraKeys = A3(
				_elm_lang$core$Dict$foldr,
				differ(expected),
				{ctor: '[]'},
				actual);
			return A5(_elm_community$elm_test$Expect$reportCollectionFailure, 'Expect.equalDicts', expected, actual, missingKeys, extraKeys);
		}
	});
var _elm_community$elm_test$Expect$equalLists = F2(
	function (expected, actual) {
		return _elm_lang$core$Native_Utils.eq(expected, actual) ? _elm_community$elm_test$Expect$pass : _elm_community$elm_test$Test_Expectation$fail(
			{
				description: 'Expect.equalLists',
				reason: A2(
					_elm_community$elm_test$Test_Runner_Failure$ListDiff,
					A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, expected),
					A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, actual))
			});
	});
var _elm_community$elm_test$Expect$err = function (result) {
	var _p5 = result;
	if (_p5.ctor === 'Ok') {
		return _elm_community$elm_test$Test_Expectation$fail(
			{
				description: 'Expect.err',
				reason: A2(
					_elm_community$elm_test$Test_Runner_Failure$Comparison,
					'Err _',
					_elm_lang$core$Basics$toString(result))
			});
	} else {
		return _elm_community$elm_test$Expect$pass;
	}
};
var _elm_community$elm_test$Expect$false = F2(
	function (message, bool) {
		return bool ? _elm_community$elm_test$Expect$fail(message) : _elm_community$elm_test$Expect$pass;
	});
var _elm_community$elm_test$Expect$true = F2(
	function (message, bool) {
		return bool ? _elm_community$elm_test$Expect$pass : _elm_community$elm_test$Expect$fail(message);
	});
var _elm_community$elm_test$Expect$notWithin = F3(
	function (tolerance, a, b) {
		return A3(
			_elm_community$elm_test$Expect$nonNegativeToleranceError,
			tolerance,
			'notWithin',
			A4(
				_elm_community$elm_test$Expect$compareWith,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Expect.notWithin ',
					_elm_lang$core$Basics$toString(tolerance)),
				F2(
					function (a, b) {
						return !A3(_elm_community$elm_test$Expect$withinCompare, tolerance, a, b);
					}),
				a,
				b));
	});
var _elm_community$elm_test$Expect$within = F3(
	function (tolerance, a, b) {
		return A3(
			_elm_community$elm_test$Expect$nonNegativeToleranceError,
			tolerance,
			'within',
			A4(
				_elm_community$elm_test$Expect$compareWith,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Expect.within ',
					_elm_lang$core$Basics$toString(tolerance)),
				_elm_community$elm_test$Expect$withinCompare(tolerance),
				a,
				b));
	});
var _elm_community$elm_test$Expect$atLeast = A2(
	_elm_community$elm_test$Expect$compareWith,
	'Expect.atLeast',
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.cmp(x, y) > -1;
		}));
var _elm_community$elm_test$Expect$greaterThan = A2(
	_elm_community$elm_test$Expect$compareWith,
	'Expect.greaterThan',
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.cmp(x, y) > 0;
		}));
var _elm_community$elm_test$Expect$atMost = A2(
	_elm_community$elm_test$Expect$compareWith,
	'Expect.atMost',
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.cmp(x, y) < 1;
		}));
var _elm_community$elm_test$Expect$lessThan = A2(
	_elm_community$elm_test$Expect$compareWith,
	'Expect.lessThan',
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.cmp(x, y) < 0;
		}));
var _elm_community$elm_test$Expect$notEqual = A2(
	_elm_community$elm_test$Expect$equateWith,
	'Expect.notEqual',
	F2(
		function (x, y) {
			return !_elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$elm_test$Expect$equal = A2(
	_elm_community$elm_test$Expect$equateWith,
	'Expect.equal',
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$elm_test$Expect$AbsoluteOrRelative = F2(
	function (a, b) {
		return {ctor: 'AbsoluteOrRelative', _0: a, _1: b};
	});
var _elm_community$elm_test$Expect$Relative = function (a) {
	return {ctor: 'Relative', _0: a};
};
var _elm_community$elm_test$Expect$Absolute = function (a) {
	return {ctor: 'Absolute', _0: a};
};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _mgold$elm_random_pcg$Random_Pcg$toJson = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$list(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Encode$int(_p1._0),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Json_Encode$int(_p1._1),
				_1: {ctor: '[]'}
			}
		});
};
var _mgold$elm_random_pcg$Random_Pcg$mul32 = F2(
	function (a, b) {
		var bl = b & 65535;
		var bh = 65535 & (b >>> 16);
		var al = a & 65535;
		var ah = 65535 & (a >>> 16);
		return 0 | ((al * bl) + ((((ah * bl) + (al * bh)) << 16) >>> 0));
	});
var _mgold$elm_random_pcg$Random_Pcg$listHelp = F4(
	function (list, n, generate, seed) {
		listHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
				return {ctor: '_Tuple2', _0: list, _1: seed};
			} else {
				var _p2 = generate(seed);
				var value = _p2._0;
				var newSeed = _p2._1;
				var _v1 = {ctor: '::', _0: value, _1: list},
					_v2 = n - 1,
					_v3 = generate,
					_v4 = newSeed;
				list = _v1;
				n = _v2;
				generate = _v3;
				seed = _v4;
				continue listHelp;
			}
		}
	});
var _mgold$elm_random_pcg$Random_Pcg$minInt = -2147483648;
var _mgold$elm_random_pcg$Random_Pcg$maxInt = 2147483647;
var _mgold$elm_random_pcg$Random_Pcg$bit27 = 1.34217728e8;
var _mgold$elm_random_pcg$Random_Pcg$bit53 = 9.007199254740992e15;
var _mgold$elm_random_pcg$Random_Pcg$peel = function (_p3) {
	var _p4 = _p3;
	var _p5 = _p4._0;
	var word = (_p5 ^ (_p5 >>> ((_p5 >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var _mgold$elm_random_pcg$Random_Pcg$step = F2(
	function (_p6, seed) {
		var _p7 = _p6;
		return _p7._0(seed);
	});
var _mgold$elm_random_pcg$Random_Pcg$retry = F3(
	function (generator, predicate, seed) {
		retry:
		while (true) {
			var _p8 = A2(_mgold$elm_random_pcg$Random_Pcg$step, generator, seed);
			var candidate = _p8._0;
			var newSeed = _p8._1;
			if (predicate(candidate)) {
				return {ctor: '_Tuple2', _0: candidate, _1: newSeed};
			} else {
				var _v7 = generator,
					_v8 = predicate,
					_v9 = newSeed;
				generator = _v7;
				predicate = _v8;
				seed = _v9;
				continue retry;
			}
		}
	});
var _mgold$elm_random_pcg$Random_Pcg$Generator = function (a) {
	return {ctor: 'Generator', _0: a};
};
var _mgold$elm_random_pcg$Random_Pcg$list = F2(
	function (n, _p9) {
		var _p10 = _p9;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed) {
				return A4(
					_mgold$elm_random_pcg$Random_Pcg$listHelp,
					{ctor: '[]'},
					n,
					_p10._0,
					seed);
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$constant = function (value) {
	return _mgold$elm_random_pcg$Random_Pcg$Generator(
		function (seed) {
			return {ctor: '_Tuple2', _0: value, _1: seed};
		});
};
var _mgold$elm_random_pcg$Random_Pcg$map = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p13 = _p12._0(seed0);
				var a = _p13._0;
				var seed1 = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: func(a),
					_1: seed1
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map2 = F3(
	function (func, _p15, _p14) {
		var _p16 = _p15;
		var _p17 = _p14;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p18 = _p16._0(seed0);
				var a = _p18._0;
				var seed1 = _p18._1;
				var _p19 = _p17._0(seed1);
				var b = _p19._0;
				var seed2 = _p19._1;
				return {
					ctor: '_Tuple2',
					_0: A2(func, a, b),
					_1: seed2
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$pair = F2(
	function (genA, genB) {
		return A3(
			_mgold$elm_random_pcg$Random_Pcg$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			genA,
			genB);
	});
var _mgold$elm_random_pcg$Random_Pcg$andMap = _mgold$elm_random_pcg$Random_Pcg$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _mgold$elm_random_pcg$Random_Pcg$map3 = F4(
	function (func, _p22, _p21, _p20) {
		var _p23 = _p22;
		var _p24 = _p21;
		var _p25 = _p20;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p26 = _p23._0(seed0);
				var a = _p26._0;
				var seed1 = _p26._1;
				var _p27 = _p24._0(seed1);
				var b = _p27._0;
				var seed2 = _p27._1;
				var _p28 = _p25._0(seed2);
				var c = _p28._0;
				var seed3 = _p28._1;
				return {
					ctor: '_Tuple2',
					_0: A3(func, a, b, c),
					_1: seed3
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map4 = F5(
	function (func, _p32, _p31, _p30, _p29) {
		var _p33 = _p32;
		var _p34 = _p31;
		var _p35 = _p30;
		var _p36 = _p29;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p37 = _p33._0(seed0);
				var a = _p37._0;
				var seed1 = _p37._1;
				var _p38 = _p34._0(seed1);
				var b = _p38._0;
				var seed2 = _p38._1;
				var _p39 = _p35._0(seed2);
				var c = _p39._0;
				var seed3 = _p39._1;
				var _p40 = _p36._0(seed3);
				var d = _p40._0;
				var seed4 = _p40._1;
				return {
					ctor: '_Tuple2',
					_0: A4(func, a, b, c, d),
					_1: seed4
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$map5 = F6(
	function (func, _p45, _p44, _p43, _p42, _p41) {
		var _p46 = _p45;
		var _p47 = _p44;
		var _p48 = _p43;
		var _p49 = _p42;
		var _p50 = _p41;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p51 = _p46._0(seed0);
				var a = _p51._0;
				var seed1 = _p51._1;
				var _p52 = _p47._0(seed1);
				var b = _p52._0;
				var seed2 = _p52._1;
				var _p53 = _p48._0(seed2);
				var c = _p53._0;
				var seed3 = _p53._1;
				var _p54 = _p49._0(seed3);
				var d = _p54._0;
				var seed4 = _p54._1;
				var _p55 = _p50._0(seed4);
				var e = _p55._0;
				var seed5 = _p55._1;
				return {
					ctor: '_Tuple2',
					_0: A5(func, a, b, c, d, e),
					_1: seed5
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$andThen = F2(
	function (callback, _p56) {
		var _p57 = _p56;
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed) {
				var _p58 = _p57._0(seed);
				var result = _p58._0;
				var newSeed = _p58._1;
				var _p59 = callback(result);
				var generateB = _p59._0;
				return generateB(newSeed);
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$maybe = F2(
	function (genBool, genA) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$andThen,
			function (b) {
				return b ? A2(_mgold$elm_random_pcg$Random_Pcg$map, _elm_lang$core$Maybe$Just, genA) : _mgold$elm_random_pcg$Random_Pcg$constant(_elm_lang$core$Maybe$Nothing);
			},
			genBool);
	});
var _mgold$elm_random_pcg$Random_Pcg$filter = F2(
	function (predicate, generator) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			A2(_mgold$elm_random_pcg$Random_Pcg$retry, generator, predicate));
	});
var _mgold$elm_random_pcg$Random_Pcg$Seed = F2(
	function (a, b) {
		return {ctor: 'Seed', _0: a, _1: b};
	});
var _mgold$elm_random_pcg$Random_Pcg$next = function (_p60) {
	var _p61 = _p60;
	var _p62 = _p61._1;
	return A2(_mgold$elm_random_pcg$Random_Pcg$Seed, ((_p61._0 * 1664525) + _p62) >>> 0, _p62);
};
var _mgold$elm_random_pcg$Random_Pcg$initialSeed = function (x) {
	var _p63 = _mgold$elm_random_pcg$Random_Pcg$next(
		A2(_mgold$elm_random_pcg$Random_Pcg$Seed, 0, 1013904223));
	var state1 = _p63._0;
	var incr = _p63._1;
	var state2 = (state1 + x) >>> 0;
	return _mgold$elm_random_pcg$Random_Pcg$next(
		A2(_mgold$elm_random_pcg$Random_Pcg$Seed, state2, incr));
};
var _mgold$elm_random_pcg$Random_Pcg$generate = F2(
	function (toMsg, generator) {
		return A2(
			_elm_lang$core$Task$perform,
			toMsg,
			A2(
				_elm_lang$core$Task$map,
				function (_p64) {
					return _elm_lang$core$Tuple$first(
						A2(
							_mgold$elm_random_pcg$Random_Pcg$step,
							generator,
							_mgold$elm_random_pcg$Random_Pcg$initialSeed(
								_elm_lang$core$Basics$round(_p64))));
				},
				_elm_lang$core$Time$now));
	});
var _mgold$elm_random_pcg$Random_Pcg$int = F2(
	function (a, b) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var _p65 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p65._0;
				var hi = _p65._1;
				var range = (hi - lo) + 1;
				if (_elm_lang$core$Native_Utils.eq((range - 1) & range, 0)) {
					return {
						ctor: '_Tuple2',
						_0: (((range - 1) & _mgold$elm_random_pcg$Random_Pcg$peel(seed0)) >>> 0) + lo,
						_1: _mgold$elm_random_pcg$Random_Pcg$next(seed0)
					};
				} else {
					var threshhold = A2(_elm_lang$core$Basics$rem, (0 - range) >>> 0, range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var seedN = _mgold$elm_random_pcg$Random_Pcg$next(seed);
							var x = _mgold$elm_random_pcg$Random_Pcg$peel(seed);
							if (_elm_lang$core$Native_Utils.cmp(x, threshhold) < 0) {
								var _v28 = seedN;
								seed = _v28;
								continue accountForBias;
							} else {
								return {
									ctor: '_Tuple2',
									_0: A2(_elm_lang$core$Basics$rem, x, range) + lo,
									_1: seedN
								};
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$bool = A2(
	_mgold$elm_random_pcg$Random_Pcg$map,
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		})(1),
	A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, 1));
var _mgold$elm_random_pcg$Random_Pcg$choice = F2(
	function (x, y) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (b) {
				return b ? x : y;
			},
			_mgold$elm_random_pcg$Random_Pcg$bool);
	});
var _mgold$elm_random_pcg$Random_Pcg$oneIn = function (n) {
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$map,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(1),
		A2(_mgold$elm_random_pcg$Random_Pcg$int, 1, n));
};
var _mgold$elm_random_pcg$Random_Pcg$sample = function () {
	var find = F2(
		function (k, ys) {
			find:
			while (true) {
				var _p66 = ys;
				if (_p66.ctor === '[]') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_elm_lang$core$Native_Utils.eq(k, 0)) {
						return _elm_lang$core$Maybe$Just(_p66._0);
					} else {
						var _v30 = k - 1,
							_v31 = _p66._1;
						k = _v30;
						ys = _v31;
						continue find;
					}
				}
			}
		});
	return function (xs) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (i) {
				return A2(find, i, xs);
			},
			A2(
				_mgold$elm_random_pcg$Random_Pcg$int,
				0,
				_elm_lang$core$List$length(xs) - 1));
	};
}();
var _mgold$elm_random_pcg$Random_Pcg$float = F2(
	function (min, max) {
		return _mgold$elm_random_pcg$Random_Pcg$Generator(
			function (seed0) {
				var range = _elm_lang$core$Basics$abs(max - min);
				var n0 = _mgold$elm_random_pcg$Random_Pcg$peel(seed0);
				var hi = _elm_lang$core$Basics$toFloat(67108863 & n0) * 1.0;
				var seed1 = _mgold$elm_random_pcg$Random_Pcg$next(seed0);
				var n1 = _mgold$elm_random_pcg$Random_Pcg$peel(seed1);
				var lo = _elm_lang$core$Basics$toFloat(134217727 & n1) * 1.0;
				var val = ((hi * _mgold$elm_random_pcg$Random_Pcg$bit27) + lo) / _mgold$elm_random_pcg$Random_Pcg$bit53;
				var scaled = (val * range) + min;
				return {
					ctor: '_Tuple2',
					_0: scaled,
					_1: _mgold$elm_random_pcg$Random_Pcg$next(seed1)
				};
			});
	});
var _mgold$elm_random_pcg$Random_Pcg$frequency = function (pairs) {
	var pick = F2(
		function (choices, n) {
			pick:
			while (true) {
				var _p67 = choices;
				if ((_p67.ctor === '::') && (_p67._0.ctor === '_Tuple2')) {
					var _p68 = _p67._0._0;
					if (_elm_lang$core$Native_Utils.cmp(n, _p68) < 1) {
						return _p67._0._1;
					} else {
						var _v33 = _p67._1,
							_v34 = n - _p68;
						choices = _v33;
						n = _v34;
						continue pick;
					}
				} else {
					return _elm_lang$core$Native_Utils.crashCase(
						'Random.Pcg',
						{
							start: {line: 677, column: 13},
							end: {line: 685, column: 77}
						},
						_p67)('Empty list passed to Random.Pcg.frequency!');
				}
			}
		});
	var total = _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			function (_p70) {
				return _elm_lang$core$Basics$abs(
					_elm_lang$core$Tuple$first(_p70));
			},
			pairs));
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$andThen,
		pick(pairs),
		A2(_mgold$elm_random_pcg$Random_Pcg$float, 0, total));
};
var _mgold$elm_random_pcg$Random_Pcg$choices = function (gens) {
	return _mgold$elm_random_pcg$Random_Pcg$frequency(
		A2(
			_elm_lang$core$List$map,
			function (g) {
				return {ctor: '_Tuple2', _0: 1, _1: g};
			},
			gens));
};
var _mgold$elm_random_pcg$Random_Pcg$independentSeed = _mgold$elm_random_pcg$Random_Pcg$Generator(
	function (seed0) {
		var gen = A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, 4294967295);
		var _p71 = A2(
			_mgold$elm_random_pcg$Random_Pcg$step,
			A4(
				_mgold$elm_random_pcg$Random_Pcg$map3,
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				gen,
				gen,
				gen),
			seed0);
		var state = _p71._0._0;
		var b = _p71._0._1;
		var c = _p71._0._2;
		var seed1 = _p71._1;
		var incr = (1 | (b ^ c)) >>> 0;
		return {
			ctor: '_Tuple2',
			_0: seed1,
			_1: _mgold$elm_random_pcg$Random_Pcg$next(
				A2(_mgold$elm_random_pcg$Random_Pcg$Seed, state, incr))
		};
	});
var _mgold$elm_random_pcg$Random_Pcg$fastForward = F2(
	function (delta0, _p72) {
		var _p73 = _p72;
		var _p76 = _p73._1;
		var helper = F6(
			function (accMult, accPlus, curMult, curPlus, delta, repeat) {
				helper:
				while (true) {
					var newDelta = delta >>> 1;
					var curMult_ = A2(_mgold$elm_random_pcg$Random_Pcg$mul32, curMult, curMult);
					var curPlus_ = A2(_mgold$elm_random_pcg$Random_Pcg$mul32, curMult + 1, curPlus);
					var _p74 = _elm_lang$core$Native_Utils.eq(delta & 1, 1) ? {
						ctor: '_Tuple2',
						_0: A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accMult, curMult),
						_1: (A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accPlus, curMult) + curPlus) >>> 0
					} : {ctor: '_Tuple2', _0: accMult, _1: accPlus};
					var accMult_ = _p74._0;
					var accPlus_ = _p74._1;
					if (_elm_lang$core$Native_Utils.eq(newDelta, 0)) {
						if ((_elm_lang$core$Native_Utils.cmp(delta0, 0) < 0) && repeat) {
							var _v36 = accMult_,
								_v37 = accPlus_,
								_v38 = curMult_,
								_v39 = curPlus_,
								_v40 = -1,
								_v41 = false;
							accMult = _v36;
							accPlus = _v37;
							curMult = _v38;
							curPlus = _v39;
							delta = _v40;
							repeat = _v41;
							continue helper;
						} else {
							return {ctor: '_Tuple2', _0: accMult_, _1: accPlus_};
						}
					} else {
						var _v42 = accMult_,
							_v43 = accPlus_,
							_v44 = curMult_,
							_v45 = curPlus_,
							_v46 = newDelta,
							_v47 = repeat;
						accMult = _v42;
						accPlus = _v43;
						curMult = _v44;
						curPlus = _v45;
						delta = _v46;
						repeat = _v47;
						continue helper;
					}
				}
			});
		var _p75 = A6(helper, 1, 0, 1664525, _p76, delta0, true);
		var accMultFinal = _p75._0;
		var accPlusFinal = _p75._1;
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$Seed,
			(A2(_mgold$elm_random_pcg$Random_Pcg$mul32, accMultFinal, _p73._0) + accPlusFinal) >>> 0,
			_p76);
	});
var _mgold$elm_random_pcg$Random_Pcg$fromJson = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: A3(
			_elm_lang$core$Json_Decode$map2,
			_mgold$elm_random_pcg$Random_Pcg$Seed,
			A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$int),
			A2(_elm_lang$core$Json_Decode$index, 1, _elm_lang$core$Json_Decode$int)),
		_1: {
			ctor: '::',
			_0: A2(_elm_lang$core$Json_Decode$map, _mgold$elm_random_pcg$Random_Pcg$initialSeed, _elm_lang$core$Json_Decode$int),
			_1: {ctor: '[]'}
		}
	});

var _elm_community$elm_test$RoseTree$children = function (_p0) {
	var _p1 = _p0;
	return _p1._1;
};
var _elm_community$elm_test$RoseTree$root = function (_p2) {
	var _p3 = _p2;
	return _p3._0;
};
var _elm_community$elm_test$RoseTree$Rose = F2(
	function (a, b) {
		return {ctor: 'Rose', _0: a, _1: b};
	});
var _elm_community$elm_test$RoseTree$singleton = function (a) {
	return A2(_elm_community$elm_test$RoseTree$Rose, a, _eeue56$elm_lazy_list$Lazy_List$empty);
};
var _elm_community$elm_test$RoseTree$addChild = F2(
	function (child, _p4) {
		var _p5 = _p4;
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			_p5._0,
			A2(_eeue56$elm_lazy_list$Lazy_List_ops[':::'], child, _p5._1));
	});
var _elm_community$elm_test$RoseTree$map = F2(
	function (f, _p6) {
		var _p7 = _p6;
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			f(_p7._0),
			A2(
				_eeue56$elm_lazy_list$Lazy_List$map,
				_elm_community$elm_test$RoseTree$map(f),
				_p7._1));
	});
var _elm_community$elm_test$RoseTree$filterMap = F2(
	function (f, _p8) {
		var _p9 = _p8;
		var _p10 = f(_p9._0);
		if (_p10.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_community$elm_test$RoseTree$Rose,
					_p10._0,
					A2(
						_eeue56$elm_lazy_list$Lazy_List$filterMap,
						_elm_community$elm_test$RoseTree$filterMap(f),
						_p9._1)));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$elm_test$RoseTree$filter = F2(
	function (predicate, tree) {
		var maybeKeep = function (x) {
			return predicate(x) ? _elm_lang$core$Maybe$Just(x) : _elm_lang$core$Maybe$Nothing;
		};
		return A2(_elm_community$elm_test$RoseTree$filterMap, maybeKeep, tree);
	});
var _elm_community$elm_test$RoseTree$filterBranches = F2(
	function (predicate, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			_p12._0,
			A2(
				_eeue56$elm_lazy_list$Lazy_List$filterMap,
				_elm_community$elm_test$RoseTree$filter(predicate),
				_p12._1));
	});
var _elm_community$elm_test$RoseTree$flatten = function (_p13) {
	var _p14 = _p13;
	return A2(
		_elm_community$elm_test$RoseTree$Rose,
		_p14._0._0,
		A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			_p14._0._1,
			A2(_eeue56$elm_lazy_list$Lazy_List$map, _elm_community$elm_test$RoseTree$flatten, _p14._1)));
};

var _elm_community$elm_test$Fuzz_Internal$invalidReason = function (valid) {
	var _p0 = valid;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(_p0._0);
	}
};
var _elm_community$elm_test$Fuzz_Internal$getValid = function (valid) {
	var _p1 = valid;
	if (_p1.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p1._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_community$elm_test$Fuzz_Internal$runAll = F2(
	function (xs, seed) {
		return _eeue56$elm_lazy$Lazy$lazy(
			function (_p2) {
				var _p3 = _eeue56$elm_lazy$Lazy$force(xs);
				if (_p3.ctor === 'Nil') {
					return _eeue56$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p4 = A2(_mgold$elm_random_pcg$Random_Pcg$step, _p3._0, seed);
					var x = _p4._0;
					var newSeed = _p4._1;
					return A2(
						_eeue56$elm_lazy_list$Lazy_List$Cons,
						x,
						A2(_elm_community$elm_test$Fuzz_Internal$runAll, _p3._1, newSeed));
				}
			});
	});
var _elm_community$elm_test$Fuzz_Internal$sequenceLazyList = function (xs) {
	return A2(
		_mgold$elm_random_pcg$Random_Pcg$map,
		_elm_community$elm_test$Fuzz_Internal$runAll(xs),
		_mgold$elm_random_pcg$Random_Pcg$independentSeed);
};
var _elm_community$elm_test$Fuzz_Internal$sequenceRoseTree = function (_p5) {
	var _p6 = _p5;
	return A3(
		_mgold$elm_random_pcg$Random_Pcg$map2,
		_elm_community$elm_test$RoseTree$Rose,
		_p6._0,
		_elm_community$elm_test$Fuzz_Internal$sequenceLazyList(
			A2(_eeue56$elm_lazy_list$Lazy_List$map, _elm_community$elm_test$Fuzz_Internal$sequenceRoseTree, _p6._1)));
};
var _elm_community$elm_test$Fuzz_Internal$removeInvalid = function (tree) {
	var _p7 = A2(_elm_community$elm_test$RoseTree$filterMap, _elm_community$elm_test$Fuzz_Internal$getValid, tree);
	if (_p7.ctor === 'Just') {
		return _p7._0;
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'Fuzz.Internal',
			{
				start: {line: 54, column: 5},
				end: {line: 59, column: 94}
			},
			_p7)('Returning an invalid fuzzer from `andThen` is currently unsupported');
	}
};
var _elm_community$elm_test$Fuzz_Internal$andThen = F2(
	function (fn, fuzzer) {
		var helper = F2(
			function (fn, xs) {
				return A2(
					_mgold$elm_random_pcg$Random_Pcg$map,
					_elm_community$elm_test$RoseTree$flatten,
					_elm_community$elm_test$Fuzz_Internal$sequenceRoseTree(
						_elm_community$elm_test$Fuzz_Internal$removeInvalid(
							A2(_elm_community$elm_test$RoseTree$map, fn, xs))));
			});
		return A2(
			_elm_lang$core$Result$map,
			_mgold$elm_random_pcg$Random_Pcg$andThen(
				helper(fn)),
			fuzzer);
	});
var _elm_community$elm_test$Fuzz_Internal$map = F2(
	function (fn, fuzzer) {
		return A2(
			function (_p9) {
				return _elm_lang$core$Result$map(
					_mgold$elm_random_pcg$Random_Pcg$map(
						_elm_community$elm_test$RoseTree$map(_p9)));
			},
			fn,
			fuzzer);
	});
var _elm_community$elm_test$Fuzz_Internal$combineValid = function (valids) {
	var _p10 = valids;
	if (_p10.ctor === '[]') {
		return _elm_lang$core$Result$Ok(
			{ctor: '[]'});
	} else {
		if (_p10._0.ctor === 'Ok') {
			return A2(
				_elm_lang$core$Result$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p10._0._0),
				_elm_community$elm_test$Fuzz_Internal$combineValid(_p10._1));
		} else {
			return _elm_lang$core$Result$Err(_p10._0._0);
		}
	}
};

var _elm_community$elm_test$Util$lengthString = F2(
	function (charGenerator, stringLength) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			_elm_lang$core$String$fromList,
			A2(_mgold$elm_random_pcg$Random_Pcg$list, stringLength, charGenerator));
	});
var _elm_community$elm_test$Util$rangeLengthString = F3(
	function (minLength, maxLength, charGenerator) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$andThen,
			_elm_community$elm_test$Util$lengthString(charGenerator),
			A2(_mgold$elm_random_pcg$Random_Pcg$int, minLength, maxLength));
	});
var _elm_community$elm_test$Util$rangeLengthList = F3(
	function (minLength, maxLength, generator) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$andThen,
			function (len) {
				return A2(_mgold$elm_random_pcg$Random_Pcg$list, len, generator);
			},
			A2(_mgold$elm_random_pcg$Random_Pcg$int, minLength, maxLength));
	});
var _elm_community$elm_test$Util$rangeLengthArray = F3(
	function (minLength, maxLength, generator) {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			_elm_lang$core$Array$fromList,
			A3(_elm_community$elm_test$Util$rangeLengthList, minLength, maxLength, generator));
	});

var _elm_community$elm_test$Fuzz$map5RoseTree = F6(
	function (transform, _p4, _p3, _p2, _p1, _p0) {
		var _p5 = _p4;
		var _p14 = _p5;
		var _p6 = _p3;
		var _p13 = _p6;
		var _p7 = _p2;
		var _p12 = _p7;
		var _p8 = _p1;
		var _p11 = _p8;
		var _p9 = _p0;
		var _p10 = _p9;
		var shrink5 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf5) {
				return A6(_elm_community$elm_test$Fuzz$map5RoseTree, transform, _p14, _p13, _p12, _p11, childOf5);
			},
			_p9._1);
		var shrink4 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf4) {
				return A6(_elm_community$elm_test$Fuzz$map5RoseTree, transform, _p14, _p13, _p12, childOf4, _p10);
			},
			_p8._1);
		var shrink3 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf3) {
				return A6(_elm_community$elm_test$Fuzz$map5RoseTree, transform, _p14, _p13, childOf3, _p11, _p10);
			},
			_p7._1);
		var shrink2 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf2) {
				return A6(_elm_community$elm_test$Fuzz$map5RoseTree, transform, _p14, childOf2, _p12, _p11, _p10);
			},
			_p6._1);
		var shrink1 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf1) {
				return A6(_elm_community$elm_test$Fuzz$map5RoseTree, transform, childOf1, _p13, _p12, _p11, _p10);
			},
			_p5._1);
		var root = A5(transform, _p5._0, _p6._0, _p7._0, _p8._0, _p9._0);
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			root,
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				shrink1,
				A2(
					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
					shrink2,
					A2(
						_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
						shrink3,
						A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], shrink4, shrink5)))));
	});
var _elm_community$elm_test$Fuzz$map4RoseTree = F5(
	function (transform, _p18, _p17, _p16, _p15) {
		var _p19 = _p18;
		var _p26 = _p19;
		var _p20 = _p17;
		var _p25 = _p20;
		var _p21 = _p16;
		var _p24 = _p21;
		var _p22 = _p15;
		var _p23 = _p22;
		var shrink4 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf4) {
				return A5(_elm_community$elm_test$Fuzz$map4RoseTree, transform, _p26, _p25, _p24, childOf4);
			},
			_p22._1);
		var shrink3 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf3) {
				return A5(_elm_community$elm_test$Fuzz$map4RoseTree, transform, _p26, _p25, childOf3, _p23);
			},
			_p21._1);
		var shrink2 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf2) {
				return A5(_elm_community$elm_test$Fuzz$map4RoseTree, transform, _p26, childOf2, _p24, _p23);
			},
			_p20._1);
		var shrink1 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf1) {
				return A5(_elm_community$elm_test$Fuzz$map4RoseTree, transform, childOf1, _p25, _p24, _p23);
			},
			_p19._1);
		var root = A4(transform, _p19._0, _p20._0, _p21._0, _p22._0);
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			root,
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				shrink1,
				A2(
					_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
					shrink2,
					A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], shrink3, shrink4))));
	});
var _elm_community$elm_test$Fuzz$map3RoseTree = F4(
	function (transform, _p29, _p28, _p27) {
		var _p30 = _p29;
		var _p35 = _p30;
		var _p31 = _p28;
		var _p34 = _p31;
		var _p32 = _p27;
		var _p33 = _p32;
		var shrink3 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf3) {
				return A4(_elm_community$elm_test$Fuzz$map3RoseTree, transform, _p35, _p34, childOf3);
			},
			_p32._1);
		var shrink2 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf2) {
				return A4(_elm_community$elm_test$Fuzz$map3RoseTree, transform, _p35, childOf2, _p33);
			},
			_p31._1);
		var shrink1 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (childOf1) {
				return A4(_elm_community$elm_test$Fuzz$map3RoseTree, transform, childOf1, _p34, _p33);
			},
			_p30._1);
		var root = A3(transform, _p30._0, _p31._0, _p32._0);
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			root,
			A2(
				_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
				shrink1,
				A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], shrink2, shrink3)));
	});
var _elm_community$elm_test$Fuzz$map2RoseTree = F3(
	function (transform, _p37, _p36) {
		var _p38 = _p37;
		var _p39 = _p36;
		var shrink2 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (subtree) {
				return A3(_elm_community$elm_test$Fuzz$map2RoseTree, transform, _p38, subtree);
			},
			_p39._1);
		var shrink1 = A2(
			_eeue56$elm_lazy_list$Lazy_List$map,
			function (subtree) {
				return A3(_elm_community$elm_test$Fuzz$map2RoseTree, transform, subtree, _p39);
			},
			_p38._1);
		var root = A2(transform, _p38._0, _p39._0);
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			root,
			A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], shrink1, shrink2));
	});
var _elm_community$elm_test$Fuzz$invalid = function (reason) {
	return _elm_lang$core$Result$Err(reason);
};
var _elm_community$elm_test$Fuzz$extractValid = function (_p40) {
	var _p41 = _p40;
	return A2(
		_elm_lang$core$Result$map,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			})(_p41._0),
		_p41._1);
};
var _elm_community$elm_test$Fuzz$frequency = function (list) {
	return _elm_lang$core$List$isEmpty(list) ? _elm_community$elm_test$Fuzz$invalid('You must provide at least one frequency pair.') : (A2(
		_elm_lang$core$List$any,
		function (_p42) {
			var _p43 = _p42;
			return _elm_lang$core$Native_Utils.cmp(_p43._0, 0) < 0;
		},
		list) ? _elm_community$elm_test$Fuzz$invalid('No frequency weights can be less than 0.') : ((_elm_lang$core$Native_Utils.cmp(
		_elm_lang$core$List$sum(
			A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, list)),
		0) < 1) ? _elm_community$elm_test$Fuzz$invalid('Frequency weights must sum to more than 0.') : A2(
		_elm_lang$core$Result$map,
		_mgold$elm_random_pcg$Random_Pcg$frequency,
		_elm_community$elm_test$Fuzz_Internal$combineValid(
			A2(_elm_lang$core$List$map, _elm_community$elm_test$Fuzz$extractValid, list)))));
};
var _elm_community$elm_test$Fuzz$oneOf = function (list) {
	return _elm_lang$core$List$isEmpty(list) ? _elm_community$elm_test$Fuzz$invalid('You must pass at least one Fuzzer to Fuzz.oneOf.') : _elm_community$elm_test$Fuzz$frequency(
		A2(
			_elm_lang$core$List$map,
			function (fuzzer) {
				return {ctor: '_Tuple2', _0: 1, _1: fuzzer};
			},
			list));
};
var _elm_community$elm_test$Fuzz$conditionalHelper = F2(
	function (opts, validFuzzer) {
		return (_elm_lang$core$Native_Utils.cmp(opts.retries, 0) < 1) ? A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (_p44) {
				return A2(
					_elm_community$elm_test$RoseTree$filterBranches,
					opts.condition,
					A2(_elm_community$elm_test$RoseTree$map, opts.fallback, _p44));
			},
			validFuzzer) : A2(
			_mgold$elm_random_pcg$Random_Pcg$andThen,
			function (tree) {
				var _p45 = A2(_elm_community$elm_test$RoseTree$filter, opts.condition, tree);
				if (_p45.ctor === 'Just') {
					return _mgold$elm_random_pcg$Random_Pcg$constant(_p45._0);
				} else {
					return A2(
						_elm_community$elm_test$Fuzz$conditionalHelper,
						_elm_lang$core$Native_Utils.update(
							opts,
							{retries: opts.retries - 1}),
						validFuzzer);
				}
			},
			validFuzzer);
	});
var _elm_community$elm_test$Fuzz$conditional = F2(
	function (opts, fuzzer) {
		return A2(
			_elm_lang$core$Result$map,
			_elm_community$elm_test$Fuzz$conditionalHelper(opts),
			fuzzer);
	});
var _elm_community$elm_test$Fuzz$andThen = _elm_community$elm_test$Fuzz_Internal$andThen;
var _elm_community$elm_test$Fuzz$map5 = F6(
	function (transform, fuzzA, fuzzB, fuzzC, fuzzD, fuzzE) {
		return A6(
			function (_p46) {
				return _elm_lang$core$Result$map5(
					_mgold$elm_random_pcg$Random_Pcg$map5(
						_elm_community$elm_test$Fuzz$map5RoseTree(_p46)));
			},
			transform,
			fuzzA,
			fuzzB,
			fuzzC,
			fuzzD,
			fuzzE);
	});
var _elm_community$elm_test$Fuzz$map4 = F5(
	function (transform, fuzzA, fuzzB, fuzzC, fuzzD) {
		return A5(
			function (_p47) {
				return _elm_lang$core$Result$map4(
					_mgold$elm_random_pcg$Random_Pcg$map4(
						_elm_community$elm_test$Fuzz$map4RoseTree(_p47)));
			},
			transform,
			fuzzA,
			fuzzB,
			fuzzC,
			fuzzD);
	});
var _elm_community$elm_test$Fuzz$map3 = F4(
	function (transform, fuzzA, fuzzB, fuzzC) {
		return A4(
			function (_p48) {
				return _elm_lang$core$Result$map3(
					_mgold$elm_random_pcg$Random_Pcg$map3(
						_elm_community$elm_test$Fuzz$map3RoseTree(_p48)));
			},
			transform,
			fuzzA,
			fuzzB,
			fuzzC);
	});
var _elm_community$elm_test$Fuzz$map2 = F3(
	function (transform, fuzzA, fuzzB) {
		return A3(
			function (_p49) {
				return _elm_lang$core$Result$map2(
					_mgold$elm_random_pcg$Random_Pcg$map2(
						_elm_community$elm_test$Fuzz$map2RoseTree(_p49)));
			},
			transform,
			fuzzA,
			fuzzB);
	});
var _elm_community$elm_test$Fuzz$andMap = _elm_community$elm_test$Fuzz$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _elm_community$elm_test$Fuzz$map = _elm_community$elm_test$Fuzz_Internal$map;
var _elm_community$elm_test$Fuzz$constant = function (x) {
	return _elm_lang$core$Result$Ok(
		_mgold$elm_random_pcg$Random_Pcg$constant(
			_elm_community$elm_test$RoseTree$singleton(x)));
};
var _elm_community$elm_test$Fuzz$tuple5 = function (_p50) {
	var _p51 = _p50;
	return A6(
		_elm_community$elm_test$Fuzz$map5,
		F5(
			function (v0, v1, v2, v3, v4) {
				return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
			}),
		_p51._0,
		_p51._1,
		_p51._2,
		_p51._3,
		_p51._4);
};
var _elm_community$elm_test$Fuzz$tuple4 = function (_p52) {
	var _p53 = _p52;
	return A5(
		_elm_community$elm_test$Fuzz$map4,
		F4(
			function (v0, v1, v2, v3) {
				return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
			}),
		_p53._0,
		_p53._1,
		_p53._2,
		_p53._3);
};
var _elm_community$elm_test$Fuzz$tuple3 = function (_p54) {
	var _p55 = _p54;
	return A4(
		_elm_community$elm_test$Fuzz$map3,
		F3(
			function (v0, v1, v2) {
				return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
			}),
		_p55._0,
		_p55._1,
		_p55._2);
};
var _elm_community$elm_test$Fuzz$tuple = function (_p56) {
	var _p57 = _p56;
	return A3(
		_elm_community$elm_test$Fuzz$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		_p57._0,
		_p57._1);
};
var _elm_community$elm_test$Fuzz$listShrinkRecurse = function (listOfTrees) {
	var removeOne = F2(
		function (index, list) {
			return A2(
				_elm_lang$core$List$append,
				A2(_elm_lang$core$List$take, index, list),
				A2(_elm_lang$core$List$drop, index + 1, list));
		});
	var shrinkOne = F2(
		function (prefix, list) {
			var _p58 = list;
			if (_p58.ctor === '[]') {
				return _eeue56$elm_lazy_list$Lazy_List$empty;
			} else {
				return A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					function (childTree) {
						return _elm_community$elm_test$Fuzz$listShrinkRecurse(
							A2(
								_elm_lang$core$Basics_ops['++'],
								prefix,
								{ctor: '::', _0: childTree, _1: _p58._1}));
					},
					_p58._0._1);
			}
		});
	var dropSecondHalf = function (list_) {
		return _elm_community$elm_test$Fuzz$listShrinkRecurse(
			A2(
				_elm_lang$core$List$take,
				(_elm_lang$core$List$length(list_) / 2) | 0,
				list_));
	};
	var dropFirstHalf = function (list_) {
		return _elm_community$elm_test$Fuzz$listShrinkRecurse(
			A2(
				_elm_lang$core$List$drop,
				(_elm_lang$core$List$length(list_) / 2) | 0,
				list_));
	};
	var root = A2(_elm_lang$core$List$map, _elm_community$elm_test$RoseTree$root, listOfTrees);
	var n = _elm_lang$core$List$length(listOfTrees);
	var halved = (_elm_lang$core$Native_Utils.cmp(n, 8) > -1) ? _eeue56$elm_lazy$Lazy$lazy(
		function (_p59) {
			return _eeue56$elm_lazy$Lazy$force(
				_eeue56$elm_lazy_list$Lazy_List$fromList(
					{
						ctor: '::',
						_0: dropFirstHalf(listOfTrees),
						_1: {
							ctor: '::',
							_0: dropSecondHalf(listOfTrees),
							_1: {ctor: '[]'}
						}
					}));
		}) : _eeue56$elm_lazy_list$Lazy_List$empty;
	var shrunkenVals = _eeue56$elm_lazy$Lazy$lazy(
		function (_p60) {
			return _eeue56$elm_lazy$Lazy$force(
				A2(
					_eeue56$elm_lazy_list$Lazy_List$andThen,
					function (i) {
						return A2(
							shrinkOne,
							A2(_elm_lang$core$List$take, i, listOfTrees),
							A2(_elm_lang$core$List$drop, i, listOfTrees));
					},
					A2(
						_eeue56$elm_lazy_list$Lazy_List$take,
						n,
						A2(
							_eeue56$elm_lazy_list$Lazy_List$map,
							function (i) {
								return i - 1;
							},
							_eeue56$elm_lazy_list$Lazy_List$numbers))));
		});
	var shortened = _eeue56$elm_lazy$Lazy$lazy(
		function (_p61) {
			return _eeue56$elm_lazy$Lazy$force(
				A2(
					_eeue56$elm_lazy_list$Lazy_List$map,
					_elm_community$elm_test$Fuzz$listShrinkRecurse,
					A2(
						_eeue56$elm_lazy_list$Lazy_List$map,
						function (index) {
							return A2(removeOne, index, listOfTrees);
						},
						_eeue56$elm_lazy_list$Lazy_List$fromList(
							A2(_elm_lang$core$List$range, 0, n - 1)))));
		});
	return A2(
		_elm_community$elm_test$RoseTree$Rose,
		root,
		A2(
			_eeue56$elm_lazy_list$Lazy_List_ops['+++'],
			halved,
			A2(_eeue56$elm_lazy_list$Lazy_List_ops['+++'], shortened, shrunkenVals)));
};
var _elm_community$elm_test$Fuzz$mapChildren = F2(
	function (fn, _p62) {
		var _p63 = _p62;
		return A2(
			_elm_community$elm_test$RoseTree$Rose,
			_p63._0,
			fn(_p63._1));
	});
var _elm_community$elm_test$Fuzz$listShrinkHelp = function (listOfTrees) {
	return A2(
		_elm_community$elm_test$Fuzz$mapChildren,
		_eeue56$elm_lazy_list$Lazy_List$cons(
			_elm_community$elm_test$RoseTree$singleton(
				{ctor: '[]'})),
		_elm_community$elm_test$Fuzz$listShrinkRecurse(listOfTrees));
};
var _elm_community$elm_test$Fuzz$list = function (fuzzer) {
	var genLength = _mgold$elm_random_pcg$Random_Pcg$frequency(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 1,
				_1: _mgold$elm_random_pcg$Random_Pcg$constant(0)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 1,
					_1: _mgold$elm_random_pcg$Random_Pcg$constant(1)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 3,
						_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 2, 10)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 2,
							_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 10, 100)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 0.5,
								_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 100, 400)
							},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
	return A2(
		_elm_lang$core$Result$map,
		function (validFuzzer) {
			return A2(
				_mgold$elm_random_pcg$Random_Pcg$map,
				_elm_community$elm_test$Fuzz$listShrinkHelp,
				A2(
					_mgold$elm_random_pcg$Random_Pcg$andThen,
					A2(_elm_lang$core$Basics$flip, _mgold$elm_random_pcg$Random_Pcg$list, validFuzzer),
					genLength));
		},
		fuzzer);
};
var _elm_community$elm_test$Fuzz$array = function (fuzzer) {
	return A2(
		_elm_community$elm_test$Fuzz$map,
		_elm_lang$core$Array$fromList,
		_elm_community$elm_test$Fuzz$list(fuzzer));
};
var _elm_community$elm_test$Fuzz$result = F2(
	function (fuzzerError, fuzzerValue) {
		var toResult = F3(
			function (useError, errorTree, valueTree) {
				return useError ? A2(_elm_community$elm_test$RoseTree$map, _elm_lang$core$Result$Err, errorTree) : A2(_elm_community$elm_test$RoseTree$map, _elm_lang$core$Result$Ok, valueTree);
			});
		return A2(
			_elm_lang$core$Result$map2(
				A2(
					_mgold$elm_random_pcg$Random_Pcg$map3,
					toResult,
					_mgold$elm_random_pcg$Random_Pcg$oneIn(4))),
			fuzzerError,
			fuzzerValue);
	});
var _elm_community$elm_test$Fuzz$maybe = function (fuzzer) {
	var toMaybe = F2(
		function (useNothing, tree) {
			return useNothing ? _elm_community$elm_test$RoseTree$singleton(_elm_lang$core$Maybe$Nothing) : A2(
				_elm_community$elm_test$RoseTree$addChild,
				_elm_community$elm_test$RoseTree$singleton(_elm_lang$core$Maybe$Nothing),
				A2(_elm_community$elm_test$RoseTree$map, _elm_lang$core$Maybe$Just, tree));
		});
	return A2(
		function (_p64) {
			return _elm_lang$core$Result$map(
				A2(_mgold$elm_random_pcg$Random_Pcg$map2, toMaybe, _p64));
		},
		_mgold$elm_random_pcg$Random_Pcg$oneIn(4),
		fuzzer);
};
var _elm_community$elm_test$Fuzz$whitespaceCharGenerator = A2(
	_mgold$elm_random_pcg$Random_Pcg$map,
	_elm_lang$core$Maybe$withDefault(
		_elm_lang$core$Native_Utils.chr(' ')),
	_mgold$elm_random_pcg$Random_Pcg$sample(
		{
			ctor: '::',
			_0: _elm_lang$core$Native_Utils.chr(' '),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Native_Utils.chr('\t'),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Native_Utils.chr('\n'),
					_1: {ctor: '[]'}
				}
			}
		}));
var _elm_community$elm_test$Fuzz$asciiCharGenerator = A2(
	_mgold$elm_random_pcg$Random_Pcg$map,
	_elm_lang$core$Char$fromCode,
	A2(_mgold$elm_random_pcg$Random_Pcg$int, 32, 126));
var _elm_community$elm_test$Fuzz$unit = _elm_lang$core$Result$Ok(
	_mgold$elm_random_pcg$Random_Pcg$constant(
		_elm_community$elm_test$RoseTree$singleton(
			{ctor: '_Tuple0'})));
var _elm_community$elm_test$Fuzz$custom = F2(
	function (generator, shrinker) {
		var shrinkTree = function (a) {
			return A2(
				_elm_community$elm_test$RoseTree$Rose,
				a,
				_eeue56$elm_lazy$Lazy$lazy(
					function (_p65) {
						return _eeue56$elm_lazy$Lazy$force(
							A2(
								_eeue56$elm_lazy_list$Lazy_List$map,
								shrinkTree,
								shrinker(a)));
					}));
		};
		return _elm_lang$core$Result$Ok(
			A2(_mgold$elm_random_pcg$Random_Pcg$map, shrinkTree, generator));
	});
var _elm_community$elm_test$Fuzz$bool = A2(_elm_community$elm_test$Fuzz$custom, _mgold$elm_random_pcg$Random_Pcg$bool, _eeue56$elm_shrink$Shrink$bool);
var _elm_community$elm_test$Fuzz$order = function () {
	var intToOrder = function (i) {
		return _elm_lang$core$Native_Utils.eq(i, 0) ? _elm_lang$core$Basics$LT : (_elm_lang$core$Native_Utils.eq(i, 1) ? _elm_lang$core$Basics$EQ : _elm_lang$core$Basics$GT);
	};
	return A2(
		_elm_community$elm_test$Fuzz$custom,
		A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			intToOrder,
			A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, 2)),
		_eeue56$elm_shrink$Shrink$order);
}();
var _elm_community$elm_test$Fuzz$int = function () {
	var generator = _mgold$elm_random_pcg$Random_Pcg$frequency(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 3,
				_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, -50, 50)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.2,
					_1: _mgold$elm_random_pcg$Random_Pcg$constant(0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, _mgold$elm_random_pcg$Random_Pcg$maxInt - _mgold$elm_random_pcg$Random_Pcg$minInt)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 1,
							_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, _mgold$elm_random_pcg$Random_Pcg$minInt - _mgold$elm_random_pcg$Random_Pcg$maxInt, 0)
						},
						_1: {ctor: '[]'}
					}
				}
			}
		});
	return A2(_elm_community$elm_test$Fuzz$custom, generator, _eeue56$elm_shrink$Shrink$int);
}();
var _elm_community$elm_test$Fuzz$intRange = F2(
	function (lo, hi) {
		return (_elm_lang$core$Native_Utils.cmp(hi, lo) < 0) ? _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Fuzz.intRange was given a lower bound of ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(lo),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' which is greater than the upper bound, ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(hi),
							'.'))))) : A2(
			_elm_community$elm_test$Fuzz$custom,
			_mgold$elm_random_pcg$Random_Pcg$frequency(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 8,
						_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, lo, hi)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 1,
							_1: _mgold$elm_random_pcg$Random_Pcg$constant(lo)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 1,
								_1: _mgold$elm_random_pcg$Random_Pcg$constant(hi)
							},
							_1: {ctor: '[]'}
						}
					}
				}),
			A2(
				_eeue56$elm_shrink$Shrink$keepIf,
				function (i) {
					return (_elm_lang$core$Native_Utils.cmp(i, lo) > -1) && (_elm_lang$core$Native_Utils.cmp(i, hi) < 1);
				},
				_eeue56$elm_shrink$Shrink$int));
	});
var _elm_community$elm_test$Fuzz$float = function () {
	var generator = _mgold$elm_random_pcg$Random_Pcg$frequency(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 3,
				_1: A2(_mgold$elm_random_pcg$Random_Pcg$float, -50, 50)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 0.5,
					_1: _mgold$elm_random_pcg$Random_Pcg$constant(0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: A2(_mgold$elm_random_pcg$Random_Pcg$float, -1, 1)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 1,
							_1: A2(
								_mgold$elm_random_pcg$Random_Pcg$float,
								0,
								_elm_lang$core$Basics$toFloat(_mgold$elm_random_pcg$Random_Pcg$maxInt - _mgold$elm_random_pcg$Random_Pcg$minInt))
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 1,
								_1: A2(
									_mgold$elm_random_pcg$Random_Pcg$float,
									_elm_lang$core$Basics$toFloat(_mgold$elm_random_pcg$Random_Pcg$minInt - _mgold$elm_random_pcg$Random_Pcg$maxInt),
									0)
							},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
	return A2(_elm_community$elm_test$Fuzz$custom, generator, _eeue56$elm_shrink$Shrink$float);
}();
var _elm_community$elm_test$Fuzz$floatRange = F2(
	function (lo, hi) {
		return (_elm_lang$core$Native_Utils.cmp(hi, lo) < 0) ? _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Fuzz.floatRange was given a lower bound of ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(lo),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' which is greater than the upper bound, ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(hi),
							'.'))))) : A2(
			_elm_community$elm_test$Fuzz$custom,
			_mgold$elm_random_pcg$Random_Pcg$frequency(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 8,
						_1: A2(_mgold$elm_random_pcg$Random_Pcg$float, lo, hi)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 1,
							_1: _mgold$elm_random_pcg$Random_Pcg$constant(lo)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 1,
								_1: _mgold$elm_random_pcg$Random_Pcg$constant(hi)
							},
							_1: {ctor: '[]'}
						}
					}
				}),
			A2(
				_eeue56$elm_shrink$Shrink$keepIf,
				function (i) {
					return (_elm_lang$core$Native_Utils.cmp(i, lo) > -1) && (_elm_lang$core$Native_Utils.cmp(i, hi) < 1);
				},
				_eeue56$elm_shrink$Shrink$float));
	});
var _elm_community$elm_test$Fuzz$percentage = function () {
	var generator = _mgold$elm_random_pcg$Random_Pcg$frequency(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 8,
				_1: A2(_mgold$elm_random_pcg$Random_Pcg$float, 0, 1)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 1,
					_1: _mgold$elm_random_pcg$Random_Pcg$constant(0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 1,
						_1: _mgold$elm_random_pcg$Random_Pcg$constant(1)
					},
					_1: {ctor: '[]'}
				}
			}
		});
	return A2(_elm_community$elm_test$Fuzz$custom, generator, _eeue56$elm_shrink$Shrink$float);
}();
var _elm_community$elm_test$Fuzz$char = A2(_elm_community$elm_test$Fuzz$custom, _elm_community$elm_test$Fuzz$asciiCharGenerator, _eeue56$elm_shrink$Shrink$character);
var _elm_community$elm_test$Fuzz$string = function () {
	var whitespaceGenerator = A2(
		_mgold$elm_random_pcg$Random_Pcg$andThen,
		_elm_community$elm_test$Util$lengthString(_elm_community$elm_test$Fuzz$whitespaceCharGenerator),
		A2(_mgold$elm_random_pcg$Random_Pcg$int, 1, 10));
	var asciiGenerator = A2(
		_mgold$elm_random_pcg$Random_Pcg$andThen,
		_elm_community$elm_test$Util$lengthString(_elm_community$elm_test$Fuzz$asciiCharGenerator),
		_mgold$elm_random_pcg$Random_Pcg$frequency(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 3,
					_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 1, 10)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 0.2,
						_1: _mgold$elm_random_pcg$Random_Pcg$constant(0)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 1,
							_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 11, 50)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 1,
								_1: A2(_mgold$elm_random_pcg$Random_Pcg$int, 50, 1000)
							},
							_1: {ctor: '[]'}
						}
					}
				}
			}));
	return A2(
		_elm_community$elm_test$Fuzz$custom,
		_mgold$elm_random_pcg$Random_Pcg$frequency(
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 9, _1: asciiGenerator},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 1, _1: whitespaceGenerator},
					_1: {ctor: '[]'}
				}
			}),
		_eeue56$elm_shrink$Shrink$string);
}();

var _elm_community$elm_test$Test_Internal$duplicatedName = function () {
	var insertOrFail = function (newName) {
		return _elm_lang$core$Result$andThen(
			function (oldNames) {
				return A2(_elm_lang$core$Set$member, newName, oldNames) ? _elm_lang$core$Result$Err(newName) : _elm_lang$core$Result$Ok(
					A2(_elm_lang$core$Set$insert, newName, oldNames));
			});
	};
	var names = function (test) {
		names:
		while (true) {
			var _p0 = test;
			switch (_p0.ctor) {
				case 'Labeled':
					return {
						ctor: '::',
						_0: _p0._0,
						_1: {ctor: '[]'}
					};
				case 'Batch':
					return A2(_elm_lang$core$List$concatMap, names, _p0._0);
				case 'UnitTest':
					return {ctor: '[]'};
				case 'FuzzTest':
					return {ctor: '[]'};
				case 'Skipped':
					var _v1 = _p0._0;
					test = _v1;
					continue names;
				default:
					var _v2 = _p0._0;
					test = _v2;
					continue names;
			}
		}
	};
	return function (_p1) {
		return A3(
			_elm_lang$core$List$foldl,
			insertOrFail,
			_elm_lang$core$Result$Ok(_elm_lang$core$Set$empty),
			A2(_elm_lang$core$List$concatMap, names, _p1));
	};
}();
var _elm_community$elm_test$Test_Internal$Batch = function (a) {
	return {ctor: 'Batch', _0: a};
};
var _elm_community$elm_test$Test_Internal$Only = function (a) {
	return {ctor: 'Only', _0: a};
};
var _elm_community$elm_test$Test_Internal$Skipped = function (a) {
	return {ctor: 'Skipped', _0: a};
};
var _elm_community$elm_test$Test_Internal$Labeled = F2(
	function (a, b) {
		return {ctor: 'Labeled', _0: a, _1: b};
	});
var _elm_community$elm_test$Test_Internal$FuzzTest = function (a) {
	return {ctor: 'FuzzTest', _0: a};
};
var _elm_community$elm_test$Test_Internal$UnitTest = function (a) {
	return {ctor: 'UnitTest', _0: a};
};
var _elm_community$elm_test$Test_Internal$failNow = function (record) {
	return _elm_community$elm_test$Test_Internal$UnitTest(
		function (_p2) {
			var _p3 = _p2;
			return {
				ctor: '::',
				_0: _elm_community$elm_test$Test_Expectation$fail(record),
				_1: {ctor: '[]'}
			};
		});
};
var _elm_community$elm_test$Test_Internal$blankDescriptionFailure = _elm_community$elm_test$Test_Internal$failNow(
	{
		description: 'This test has a blank description. Let\'s give it a useful one!',
		reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$BadDescription)
	});

var _elm_community$elm_test$Test_Fuzz$formatExpectation = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_community$elm_test$Test_Expectation$withGiven, _p1._0, _p1._1);
};
var _elm_community$elm_test$Test_Fuzz$shrinkAndAdd = F4(
	function (rootTree, getExpectation, rootsExpectation, failures) {
		var _p2 = rootTree;
		var failingValue = _p2._0;
		var shrink = F2(
			function (oldExpectation, _p3) {
				shrink:
				while (true) {
					var _p4 = _p3;
					var _p8 = _p4._0;
					var _p5 = _eeue56$elm_lazy_list$Lazy_List$headAndTail(_p4._1);
					if (_p5.ctor === 'Just') {
						var _p6 = getExpectation(_p5._0._0._0);
						if (_p6.ctor === 'Pass') {
							var _v4 = oldExpectation,
								_v5 = A2(_elm_community$elm_test$RoseTree$Rose, _p8, _p5._0._1);
							oldExpectation = _v4;
							_p3 = _v5;
							continue shrink;
						} else {
							var _p7 = A2(shrink, _p6, _p5._0._0);
							var minimalValue = _p7._0;
							var finalExpectation = _p7._1;
							return {ctor: '_Tuple2', _0: minimalValue, _1: finalExpectation};
						}
					} else {
						return {ctor: '_Tuple2', _0: _p8, _1: oldExpectation};
					}
				}
			});
		var _p9 = A2(shrink, rootsExpectation, rootTree);
		var minimalValue = _p9._0;
		var finalExpectation = _p9._1;
		return A3(
			_elm_lang$core$Dict$insert,
			_elm_lang$core$Basics$toString(minimalValue),
			finalExpectation,
			failures);
	});
var _elm_community$elm_test$Test_Fuzz$findNewFailure = F5(
	function (fuzzer, getExpectation, failures, currentSeed, value) {
		var _p10 = getExpectation(value);
		if (_p10.ctor === 'Pass') {
			return failures;
		} else {
			var _p11 = A2(_mgold$elm_random_pcg$Random_Pcg$step, fuzzer, currentSeed);
			var rosetree = _p11._0;
			var nextSeed = _p11._1;
			return A4(_elm_community$elm_test$Test_Fuzz$shrinkAndAdd, rosetree, getExpectation, _p10, failures);
		}
	});
var _elm_community$elm_test$Test_Fuzz$getFailures = F4(
	function (fuzzer, getExpectation, initialSeed, totalRuns) {
		var initialFailures = _elm_lang$core$Dict$empty;
		var genVal = A2(_mgold$elm_random_pcg$Random_Pcg$map, _elm_community$elm_test$RoseTree$root, fuzzer);
		var helper = F3(
			function (currentSeed, remainingRuns, failures) {
				helper:
				while (true) {
					var _p12 = A2(_mgold$elm_random_pcg$Random_Pcg$step, genVal, currentSeed);
					var value = _p12._0;
					var nextSeed = _p12._1;
					var newFailures = A5(_elm_community$elm_test$Test_Fuzz$findNewFailure, fuzzer, getExpectation, failures, currentSeed, value);
					if (_elm_lang$core$Native_Utils.cmp(remainingRuns, 1) < 1) {
						return newFailures;
					} else {
						var _v7 = nextSeed,
							_v8 = remainingRuns - 1,
							_v9 = newFailures;
						currentSeed = _v7;
						remainingRuns = _v8;
						failures = _v9;
						continue helper;
					}
				}
			});
		return A3(helper, initialSeed, totalRuns, initialFailures);
	});
var _elm_community$elm_test$Test_Fuzz$validatedFuzzTest = F3(
	function (fuzzer, desc, getExpectation) {
		var run = F2(
			function (seed, runs) {
				var failures = A4(_elm_community$elm_test$Test_Fuzz$getFailures, fuzzer, getExpectation, seed, runs);
				return _elm_lang$core$Dict$isEmpty(failures) ? {
					ctor: '::',
					_0: _elm_community$elm_test$Test_Expectation$Pass,
					_1: {ctor: '[]'}
				} : A2(
					_elm_lang$core$List$map,
					_elm_community$elm_test$Test_Fuzz$formatExpectation,
					_elm_lang$core$Dict$toList(failures));
			});
		return A2(
			_elm_community$elm_test$Test_Internal$Labeled,
			desc,
			_elm_community$elm_test$Test_Internal$FuzzTest(run));
	});
var _elm_community$elm_test$Test_Fuzz$fuzzTest = F3(
	function (fuzzer, untrimmedDesc, getExpectation) {
		var desc = _elm_lang$core$String$trim(untrimmedDesc);
		if (_elm_lang$core$String$isEmpty(desc)) {
			return _elm_community$elm_test$Test_Internal$blankDescriptionFailure;
		} else {
			var _p13 = fuzzer;
			if (_p13.ctor === 'Err') {
				return _elm_community$elm_test$Test_Internal$failNow(
					{
						description: _p13._0,
						reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$InvalidFuzzer)
					});
			} else {
				return A3(_elm_community$elm_test$Test_Fuzz$validatedFuzzTest, _p13._0, desc, getExpectation);
			}
		}
	});

var _elm_community$elm_test$Test$uncurry5 = F2(
	function (fn, _p0) {
		var _p1 = _p0;
		return A5(fn, _p1._0, _p1._1, _p1._2, _p1._3, _p1._4);
	});
var _elm_community$elm_test$Test$uncurry4 = F2(
	function (fn, _p2) {
		var _p3 = _p2;
		return A4(fn, _p3._0, _p3._1, _p3._2, _p3._3);
	});
var _elm_community$elm_test$Test$uncurry3 = F2(
	function (fn, _p4) {
		var _p5 = _p4;
		return A3(fn, _p5._0, _p5._1, _p5._2);
	});
var _elm_community$elm_test$Test$fuzz = _elm_community$elm_test$Test_Fuzz$fuzzTest;
var _elm_community$elm_test$Test$fuzz2 = F3(
	function (fuzzA, fuzzB, desc) {
		var fuzzer = _elm_community$elm_test$Fuzz$tuple(
			{ctor: '_Tuple2', _0: fuzzA, _1: fuzzB});
		return function (_p6) {
			return A3(
				_elm_community$elm_test$Test$fuzz,
				fuzzer,
				desc,
				_elm_lang$core$Basics$uncurry(_p6));
		};
	});
var _elm_community$elm_test$Test$fuzz3 = F4(
	function (fuzzA, fuzzB, fuzzC, desc) {
		var fuzzer = _elm_community$elm_test$Fuzz$tuple3(
			{ctor: '_Tuple3', _0: fuzzA, _1: fuzzB, _2: fuzzC});
		return function (_p7) {
			return A3(
				_elm_community$elm_test$Test$fuzz,
				fuzzer,
				desc,
				_elm_community$elm_test$Test$uncurry3(_p7));
		};
	});
var _elm_community$elm_test$Test$fuzz4 = F5(
	function (fuzzA, fuzzB, fuzzC, fuzzD, desc) {
		var fuzzer = _elm_community$elm_test$Fuzz$tuple4(
			{ctor: '_Tuple4', _0: fuzzA, _1: fuzzB, _2: fuzzC, _3: fuzzD});
		return function (_p8) {
			return A3(
				_elm_community$elm_test$Test$fuzz,
				fuzzer,
				desc,
				_elm_community$elm_test$Test$uncurry4(_p8));
		};
	});
var _elm_community$elm_test$Test$fuzz5 = F6(
	function (fuzzA, fuzzB, fuzzC, fuzzD, fuzzE, desc) {
		var fuzzer = _elm_community$elm_test$Fuzz$tuple5(
			{ctor: '_Tuple5', _0: fuzzA, _1: fuzzB, _2: fuzzC, _3: fuzzD, _4: fuzzE});
		return function (_p9) {
			return A3(
				_elm_community$elm_test$Test$fuzz,
				fuzzer,
				desc,
				_elm_community$elm_test$Test$uncurry5(_p9));
		};
	});
var _elm_community$elm_test$Test$fuzzWithHelp = F2(
	function (options, test) {
		var _p10 = test;
		switch (_p10.ctor) {
			case 'UnitTest':
				return test;
			case 'FuzzTest':
				return _elm_community$elm_test$Test_Internal$FuzzTest(
					F2(
						function (seed, _p11) {
							return A2(_p10._0, seed, options.runs);
						}));
			case 'Labeled':
				return A2(
					_elm_community$elm_test$Test_Internal$Labeled,
					_p10._0,
					A2(_elm_community$elm_test$Test$fuzzWithHelp, options, _p10._1));
			case 'Skipped':
				return _elm_community$elm_test$Test_Internal$Only(
					A2(_elm_community$elm_test$Test$fuzzWithHelp, options, _p10._0));
			case 'Only':
				return _elm_community$elm_test$Test_Internal$Only(
					A2(_elm_community$elm_test$Test$fuzzWithHelp, options, _p10._0));
			default:
				return _elm_community$elm_test$Test_Internal$Batch(
					A2(
						_elm_lang$core$List$map,
						_elm_community$elm_test$Test$fuzzWithHelp(options),
						_p10._0));
		}
	});
var _elm_community$elm_test$Test$fuzzWith = F4(
	function (options, fuzzer, desc, getTest) {
		return (_elm_lang$core$Native_Utils.cmp(options.runs, 1) < 0) ? _elm_community$elm_test$Test_Internal$failNow(
			{
				description: A2(
					_elm_lang$core$Basics_ops['++'],
					'Fuzz tests must have a run count of at least 1, not ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(options.runs),
						'.')),
				reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$NonpositiveFuzzCount)
			}) : A2(
			_elm_community$elm_test$Test$fuzzWithHelp,
			options,
			A3(_elm_community$elm_test$Test$fuzz, fuzzer, desc, getTest));
	});
var _elm_community$elm_test$Test$skip = _elm_community$elm_test$Test_Internal$Skipped;
var _elm_community$elm_test$Test$only = _elm_community$elm_test$Test_Internal$Only;
var _elm_community$elm_test$Test$todo = function (desc) {
	return _elm_community$elm_test$Test_Internal$failNow(
		{description: desc, reason: _elm_community$elm_test$Test_Runner_Failure$TODO});
};
var _elm_community$elm_test$Test$test = F2(
	function (untrimmedDesc, thunk) {
		var desc = _elm_lang$core$String$trim(untrimmedDesc);
		return _elm_lang$core$String$isEmpty(desc) ? _elm_community$elm_test$Test_Internal$blankDescriptionFailure : A2(
			_elm_community$elm_test$Test_Internal$Labeled,
			desc,
			_elm_community$elm_test$Test_Internal$UnitTest(
				function (_p12) {
					var _p13 = _p12;
					return {
						ctor: '::',
						_0: thunk(
							{ctor: '_Tuple0'}),
						_1: {ctor: '[]'}
					};
				}));
	});
var _elm_community$elm_test$Test$describe = F2(
	function (untrimmedDesc, tests) {
		var desc = _elm_lang$core$String$trim(untrimmedDesc);
		if (_elm_lang$core$String$isEmpty(desc)) {
			return _elm_community$elm_test$Test_Internal$failNow(
				{
					description: 'This `describe` has a blank description. Let\'s give it a useful one!',
					reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$BadDescription)
				});
		} else {
			if (_elm_lang$core$List$isEmpty(tests)) {
				return _elm_community$elm_test$Test_Internal$failNow(
					{
						description: A2(
							_elm_lang$core$Basics_ops['++'],
							'This `describe ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(desc),
								'` has no tests in it. Let\'s give it some!')),
						reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$EmptyList)
					});
			} else {
				var _p14 = _elm_community$elm_test$Test_Internal$duplicatedName(tests);
				if (_p14.ctor === 'Err') {
					return _elm_community$elm_test$Test_Internal$failNow(
						{
							description: A2(
								_elm_lang$core$Basics_ops['++'],
								'The tests \'',
								A2(
									_elm_lang$core$Basics_ops['++'],
									desc,
									A2(
										_elm_lang$core$Basics_ops['++'],
										'\' contain multiple tests named \'',
										A2(_elm_lang$core$Basics_ops['++'], _p14._0, '\'. Let\'s rename them so we know which is which.')))),
							reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$DuplicatedName)
						});
				} else {
					return A2(_elm_lang$core$Set$member, desc, _p14._0) ? _elm_community$elm_test$Test_Internal$failNow(
						{
							description: A2(
								_elm_lang$core$Basics_ops['++'],
								'The test \'',
								A2(_elm_lang$core$Basics_ops['++'], desc, '\' contains a child test of the same name. Let\'s rename them so we know which is which.')),
							reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$DuplicatedName)
						}) : A2(
						_elm_community$elm_test$Test_Internal$Labeled,
						desc,
						_elm_community$elm_test$Test_Internal$Batch(tests));
				}
			}
		}
	});
var _elm_community$elm_test$Test$concat = function (tests) {
	if (_elm_lang$core$List$isEmpty(tests)) {
		return _elm_community$elm_test$Test_Internal$failNow(
			{
				description: 'This `concat` has no tests in it. Let\'s give it some!',
				reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$EmptyList)
			});
	} else {
		var _p15 = _elm_community$elm_test$Test_Internal$duplicatedName(tests);
		if (_p15.ctor === 'Err') {
			return _elm_community$elm_test$Test_Internal$failNow(
				{
					description: A2(
						_elm_lang$core$Basics_ops['++'],
						'A test group contains multiple tests named \'',
						A2(_elm_lang$core$Basics_ops['++'], _p15._0, '\'. Do some renaming so that tests have unique names.')),
					reason: _elm_community$elm_test$Test_Runner_Failure$Invalid(_elm_community$elm_test$Test_Runner_Failure$DuplicatedName)
				});
		} else {
			return _elm_community$elm_test$Test_Internal$Batch(tests);
		}
	}
};
var _elm_community$elm_test$Test$FuzzOptions = function (a) {
	return {runs: a};
};

var _elm_community$elm_test$Test_Runner$formatLabels = F3(
	function (formatDescription, formatTest, labels) {
		var _p1 = A2(
			_elm_lang$core$List$filter,
			function (_p0) {
				return !_elm_lang$core$String$isEmpty(_p0);
			},
			labels);
		if (_p1.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return _elm_lang$core$List$reverse(
				A2(
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						}),
					formatTest(_p1._0),
					A2(_elm_lang$core$List$map, formatDescription, _p1._1)));
		}
	});
var _elm_community$elm_test$Test_Runner$isTodo = function (expectation) {
	var _p2 = expectation;
	if (_p2.ctor === 'Pass') {
		return false;
	} else {
		return _elm_lang$core$Native_Utils.eq(_p2._0.reason, _elm_community$elm_test$Test_Runner_Failure$TODO);
	}
};
var _elm_community$elm_test$Test_Runner$getFailureReason = function (expectation) {
	var _p3 = expectation;
	if (_p3.ctor === 'Pass') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(_p3._0);
	}
};
var _elm_community$elm_test$Test_Runner$getFailure = function (expectation) {
	var _p4 = expectation;
	if (_p4.ctor === 'Pass') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{
				given: _p4._0.given,
				message: A2(_elm_community$elm_test$Test_Runner_Failure$format, _p4._0.description, _p4._0.reason)
			});
	}
};
var _elm_community$elm_test$Test_Runner$fnvHash = F2(
	function (a, b) {
		return ((a ^ b) * 16777619) >>> 0;
	});
var _elm_community$elm_test$Test_Runner$fnvHashString = F2(
	function (hash, str) {
		return A3(
			_elm_lang$core$List$foldl,
			_elm_community$elm_test$Test_Runner$fnvHash,
			hash,
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Char$toCode,
				_elm_lang$core$String$toList(str)));
	});
var _elm_community$elm_test$Test_Runner$fnvInit = 2166136261;
var _elm_community$elm_test$Test_Runner$emptyDistribution = function (seed) {
	return {
		seed: seed,
		all: {ctor: '[]'},
		only: {ctor: '[]'},
		skipped: {ctor: '[]'}
	};
};
var _elm_community$elm_test$Test_Runner$run = function (_p5) {
	var _p6 = _p5;
	return _p6._0(
		{ctor: '_Tuple0'});
};
var _elm_community$elm_test$Test_Runner$fromRunnableTreeHelp = F2(
	function (labels, runner) {
		fromRunnableTreeHelp:
		while (true) {
			var _p7 = runner;
			switch (_p7.ctor) {
				case 'Runnable':
					return {
						ctor: '::',
						_0: {
							labels: labels,
							run: function (_p8) {
								return _elm_community$elm_test$Test_Runner$run(_p7._0);
							}
						},
						_1: {ctor: '[]'}
					};
				case 'Labeled':
					var _v6 = {ctor: '::', _0: _p7._0, _1: labels},
						_v7 = _p7._1;
					labels = _v6;
					runner = _v7;
					continue fromRunnableTreeHelp;
				default:
					return A2(
						_elm_lang$core$List$concatMap,
						_elm_community$elm_test$Test_Runner$fromRunnableTreeHelp(labels),
						_p7._0);
			}
		}
	});
var _elm_community$elm_test$Test_Runner$fromRunnableTree = _elm_community$elm_test$Test_Runner$fromRunnableTreeHelp(
	{ctor: '[]'});
var _elm_community$elm_test$Test_Runner$countAllRunnables = A2(
	_elm_lang$core$List$foldl,
	function (_p9) {
		return F2(
			function (x, y) {
				return x + y;
			})(
			_elm_community$elm_test$Test_Runner$countRunnables(_p9));
	},
	0);
var _elm_community$elm_test$Test_Runner$countRunnables = function (runnable) {
	countRunnables:
	while (true) {
		var _p10 = runnable;
		switch (_p10.ctor) {
			case 'Runnable':
				return 1;
			case 'Labeled':
				var _v9 = _p10._1;
				runnable = _v9;
				continue countRunnables;
			default:
				return _elm_community$elm_test$Test_Runner$countAllRunnables(_p10._0);
		}
	}
};
var _elm_community$elm_test$Test_Runner$Runner = F2(
	function (a, b) {
		return {run: a, labels: b};
	});
var _elm_community$elm_test$Test_Runner$Distribution = F4(
	function (a, b, c, d) {
		return {seed: a, only: b, all: c, skipped: d};
	});
var _elm_community$elm_test$Test_Runner$Shrunken = F2(
	function (a, b) {
		return {down: a, over: b};
	});
var _elm_community$elm_test$Test_Runner$Thunk = function (a) {
	return {ctor: 'Thunk', _0: a};
};
var _elm_community$elm_test$Test_Runner$Batch = function (a) {
	return {ctor: 'Batch', _0: a};
};
var _elm_community$elm_test$Test_Runner$Labeled = F2(
	function (a, b) {
		return {ctor: 'Labeled', _0: a, _1: b};
	});
var _elm_community$elm_test$Test_Runner$Runnable = function (a) {
	return {ctor: 'Runnable', _0: a};
};
var _elm_community$elm_test$Test_Runner$distributeSeedsHelp = F4(
	function (hashed, runs, seed, test) {
		var _p11 = test;
		switch (_p11.ctor) {
			case 'UnitTest':
				return {
					seed: seed,
					all: {
						ctor: '::',
						_0: _elm_community$elm_test$Test_Runner$Runnable(
							_elm_community$elm_test$Test_Runner$Thunk(
								function (_p12) {
									return _p11._0(
										{ctor: '_Tuple0'});
								})),
						_1: {ctor: '[]'}
					},
					only: {ctor: '[]'},
					skipped: {ctor: '[]'}
				};
			case 'FuzzTest':
				var _p13 = A2(_mgold$elm_random_pcg$Random_Pcg$step, _mgold$elm_random_pcg$Random_Pcg$independentSeed, seed);
				var firstSeed = _p13._0;
				var nextSeed = _p13._1;
				return {
					seed: nextSeed,
					all: {
						ctor: '::',
						_0: _elm_community$elm_test$Test_Runner$Runnable(
							_elm_community$elm_test$Test_Runner$Thunk(
								function (_p14) {
									return A2(_p11._0, firstSeed, runs);
								})),
						_1: {ctor: '[]'}
					},
					only: {ctor: '[]'},
					skipped: {ctor: '[]'}
				};
			case 'Labeled':
				var _p16 = _p11._1;
				var _p15 = _p11._0;
				if (hashed) {
					var next = A4(_elm_community$elm_test$Test_Runner$distributeSeedsHelp, true, runs, seed, _p16);
					return {
						seed: next.seed,
						all: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.all),
						only: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.only),
						skipped: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.skipped)
					};
				} else {
					var intFromSeed = _elm_lang$core$Tuple$first(
						A2(
							_mgold$elm_random_pcg$Random_Pcg$step,
							A2(_mgold$elm_random_pcg$Random_Pcg$int, 0, _mgold$elm_random_pcg$Random_Pcg$maxInt),
							seed));
					var hashedSeed = _mgold$elm_random_pcg$Random_Pcg$initialSeed(
						A2(
							_elm_community$elm_test$Test_Runner$fnvHash,
							intFromSeed,
							A2(_elm_community$elm_test$Test_Runner$fnvHashString, _elm_community$elm_test$Test_Runner$fnvInit, _p15)));
					var next = A4(_elm_community$elm_test$Test_Runner$distributeSeedsHelp, true, runs, hashedSeed, _p16);
					return {
						seed: seed,
						all: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.all),
						only: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.only),
						skipped: A2(
							_elm_lang$core$List$map,
							_elm_community$elm_test$Test_Runner$Labeled(_p15),
							next.skipped)
					};
				}
			case 'Skipped':
				var next = A4(_elm_community$elm_test$Test_Runner$distributeSeedsHelp, hashed, runs, seed, _p11._0);
				return {
					seed: next.seed,
					all: {ctor: '[]'},
					only: {ctor: '[]'},
					skipped: next.all
				};
			case 'Only':
				var next = A4(_elm_community$elm_test$Test_Runner$distributeSeedsHelp, hashed, runs, seed, _p11._0);
				return _elm_lang$core$Native_Utils.update(
					next,
					{only: next.all});
			default:
				return A3(
					_elm_lang$core$List$foldl,
					A2(_elm_community$elm_test$Test_Runner$batchDistribute, hashed, runs),
					_elm_community$elm_test$Test_Runner$emptyDistribution(seed),
					_p11._0);
		}
	});
var _elm_community$elm_test$Test_Runner$batchDistribute = F4(
	function (hashed, runs, test, prev) {
		var next = A4(_elm_community$elm_test$Test_Runner$distributeSeedsHelp, hashed, runs, prev.seed, test);
		return {
			seed: next.seed,
			all: A2(_elm_lang$core$Basics_ops['++'], prev.all, next.all),
			only: A2(_elm_lang$core$Basics_ops['++'], prev.only, next.only),
			skipped: A2(_elm_lang$core$Basics_ops['++'], prev.skipped, next.skipped)
		};
	});
var _elm_community$elm_test$Test_Runner$distributeSeeds = _elm_community$elm_test$Test_Runner$distributeSeedsHelp(false);
var _elm_community$elm_test$Test_Runner$Invalid = function (a) {
	return {ctor: 'Invalid', _0: a};
};
var _elm_community$elm_test$Test_Runner$Skipping = function (a) {
	return {ctor: 'Skipping', _0: a};
};
var _elm_community$elm_test$Test_Runner$Only = function (a) {
	return {ctor: 'Only', _0: a};
};
var _elm_community$elm_test$Test_Runner$Plain = function (a) {
	return {ctor: 'Plain', _0: a};
};
var _elm_community$elm_test$Test_Runner$fromTest = F3(
	function (runs, seed, test) {
		if (_elm_lang$core$Native_Utils.cmp(runs, 1) < 0) {
			return _elm_community$elm_test$Test_Runner$Invalid(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Test runner run count must be at least 1, not ',
					_elm_lang$core$Basics$toString(runs)));
		} else {
			var distribution = A3(_elm_community$elm_test$Test_Runner$distributeSeeds, runs, seed, test);
			return _elm_lang$core$List$isEmpty(distribution.only) ? (_elm_lang$core$Native_Utils.eq(
				_elm_community$elm_test$Test_Runner$countAllRunnables(distribution.skipped),
				0) ? _elm_community$elm_test$Test_Runner$Plain(
				A2(_elm_lang$core$List$concatMap, _elm_community$elm_test$Test_Runner$fromRunnableTree, distribution.all)) : _elm_community$elm_test$Test_Runner$Skipping(
				A2(_elm_lang$core$List$concatMap, _elm_community$elm_test$Test_Runner$fromRunnableTree, distribution.all))) : _elm_community$elm_test$Test_Runner$Only(
				A2(_elm_lang$core$List$concatMap, _elm_community$elm_test$Test_Runner$fromRunnableTree, distribution.only));
		}
	});
var _elm_community$elm_test$Test_Runner$Shrinkable = function (a) {
	return {ctor: 'Shrinkable', _0: a};
};
var _elm_community$elm_test$Test_Runner$fuzz = function (fuzzer) {
	var _p17 = fuzzer;
	if (_p17.ctor === 'Ok') {
		return A2(
			_mgold$elm_random_pcg$Random_Pcg$map,
			function (_p18) {
				var _p19 = _p18;
				return {
					ctor: '_Tuple2',
					_0: _p19._0,
					_1: _elm_community$elm_test$Test_Runner$Shrinkable(
						{down: _p19._1, over: _eeue56$elm_lazy_list$Lazy_List$empty})
				};
			},
			_p17._0);
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'Test.Runner',
			{
				start: {line: 499, column: 5},
				end: {line: 508, column: 83}
			},
			_p17)(
			A2(_elm_lang$core$Basics_ops['++'], 'Cannot call `fuzz` with an invalid fuzzer: ', _p17._0));
	}
};
var _elm_community$elm_test$Test_Runner$shrink = F2(
	function (causedPass, _p21) {
		var _p22 = _p21;
		var tryNext = causedPass ? _p22._0.over : _p22._0.down;
		var _p23 = _eeue56$elm_lazy_list$Lazy_List$headAndTail(tryNext);
		if (_p23.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '_Tuple2',
					_0: _p23._0._0._0,
					_1: _elm_community$elm_test$Test_Runner$Shrinkable(
						{down: _p23._0._0._1, over: _p23._0._1})
				});
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

var _matthewsj$elm_ordering$Ordering$greaterThanBy = F3(
	function (ordering, x, y) {
		var _p0 = A2(ordering, x, y);
		if (_p0.ctor === 'GT') {
			return true;
		} else {
			return false;
		}
	});
var _matthewsj$elm_ordering$Ordering$lessThanBy = F3(
	function (ordering, x, y) {
		var _p1 = A2(ordering, x, y);
		if (_p1.ctor === 'LT') {
			return true;
		} else {
			return false;
		}
	});
var _matthewsj$elm_ordering$Ordering$isOrdered = F2(
	function (ordering, items) {
		isOrdered:
		while (true) {
			var _p2 = items;
			if ((_p2.ctor === '::') && (_p2._1.ctor === '::')) {
				var _p4 = _p2._1;
				var _p3 = A2(ordering, _p2._0, _p2._1._0);
				switch (_p3.ctor) {
					case 'LT':
						var _v4 = ordering,
							_v5 = _p4;
						ordering = _v4;
						items = _v5;
						continue isOrdered;
					case 'EQ':
						var _v6 = ordering,
							_v7 = _p4;
						ordering = _v6;
						items = _v7;
						continue isOrdered;
					default:
						return false;
				}
			} else {
				return true;
			}
		}
	});
var _matthewsj$elm_ordering$Ordering$reverse = F3(
	function (ordering, x, y) {
		var _p5 = A2(ordering, x, y);
		switch (_p5.ctor) {
			case 'LT':
				return _elm_lang$core$Basics$GT;
			case 'EQ':
				return _elm_lang$core$Basics$EQ;
			default:
				return _elm_lang$core$Basics$LT;
		}
	});
var _matthewsj$elm_ordering$Ordering$noConflicts = _elm_lang$core$Basics$EQ;
var _matthewsj$elm_ordering$Ordering$ifStillTiedThen = F2(
	function (tiebreaker, mainOrder) {
		var _p6 = mainOrder;
		if (_p6.ctor === 'EQ') {
			return tiebreaker;
		} else {
			return mainOrder;
		}
	});
var _matthewsj$elm_ordering$Ordering$breakTiesWith = F4(
	function (tiebreaker, mainOrdering, x, y) {
		var _p7 = A2(mainOrdering, x, y);
		switch (_p7.ctor) {
			case 'LT':
				return _elm_lang$core$Basics$LT;
			case 'GT':
				return _elm_lang$core$Basics$GT;
			default:
				return A2(tiebreaker, x, y);
		}
	});
var _matthewsj$elm_ordering$Ordering$byFieldWith = F4(
	function (compareField, extractField, x, y) {
		return A2(
			compareField,
			extractField(x),
			extractField(y));
	});
var _matthewsj$elm_ordering$Ordering$explicit = F3(
	function (items, x, y) {
		var scanForY = function (items) {
			scanForY:
			while (true) {
				var _p8 = items;
				if (_p8.ctor === '::') {
					if (_elm_lang$core$Native_Utils.eq(_p8._0, y)) {
						return _elm_lang$core$Basics$LT;
					} else {
						var _v12 = _p8._1;
						items = _v12;
						continue scanForY;
					}
				} else {
					return _elm_lang$core$Basics$GT;
				}
			}
		};
		var scanForX = function (items) {
			scanForX:
			while (true) {
				var _p9 = items;
				if (_p9.ctor === '::') {
					if (_elm_lang$core$Native_Utils.eq(_p9._0, x)) {
						return _elm_lang$core$Basics$GT;
					} else {
						var _v14 = _p9._1;
						items = _v14;
						continue scanForX;
					}
				} else {
					return _elm_lang$core$Basics$LT;
				}
			}
		};
		var scanForEither = function (items) {
			scanForEither:
			while (true) {
				var _p10 = items;
				if (_p10.ctor === '::') {
					var _p12 = _p10._1;
					var _p11 = _p10._0;
					if (_elm_lang$core$Native_Utils.eq(_p11, x)) {
						return scanForY(_p12);
					} else {
						if (_elm_lang$core$Native_Utils.eq(_p11, y)) {
							return scanForX(_p12);
						} else {
							var _v16 = _p12;
							items = _v16;
							continue scanForEither;
						}
					}
				} else {
					return _elm_lang$core$Basics$EQ;
				}
			}
		};
		return _elm_lang$core$Native_Utils.eq(x, y) ? _elm_lang$core$Basics$EQ : scanForEither(items);
	});
var _matthewsj$elm_ordering$Ordering$fromLessThan = F3(
	function (lt, x, y) {
		return A2(lt, x, y) ? _elm_lang$core$Basics$LT : (A2(lt, y, x) ? _elm_lang$core$Basics$GT : _elm_lang$core$Basics$EQ);
	});
var _matthewsj$elm_ordering$Ordering$natural = _elm_lang$core$Basics$compare;
var _matthewsj$elm_ordering$Ordering$byField = _matthewsj$elm_ordering$Ordering$byFieldWith(_matthewsj$elm_ordering$Ordering$natural);
var _matthewsj$elm_ordering$Ordering$byToString = _matthewsj$elm_ordering$Ordering$byField(_elm_lang$core$Basics$toString);
var _matthewsj$elm_ordering$Ordering$byRank = F2(
	function (rank, withinRankOrdering) {
		return A2(
			_matthewsj$elm_ordering$Ordering$breakTiesWith,
			withinRankOrdering,
			_matthewsj$elm_ordering$Ordering$byField(rank));
	});

var _user$project$Test_Runner_Node_Vendor_Console$bgWhite = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[47m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgCyan = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[46m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgMagenta = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[45m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgBlue = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[44m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgYellow = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[43m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgGreen = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[42m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgRed = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[41m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bgBlack = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[40m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[49m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$white = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[37m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$cyan = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[36m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$magenta = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[35m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$blue = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[34m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$yellow = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[33m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$green = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[32m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$red = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[31m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$black = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[30m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[39m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$colorsInverted = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[7m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[27m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$underline = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[4m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[24m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$bold = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[1m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[22m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$dark = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[2m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[22m',
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Runner_Node_Vendor_Console$plain = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'',
		{
			ctor: '::',
			_0: '[0m',
			_1: {
				ctor: '::',
				_0: str,
				_1: {
					ctor: '::',
					_0: '[0m',
					_1: {ctor: '[]'}
				}
			}
		});
};

var _user$project$Console_Text$applyModifiersHelp = F2(
	function (modifier, str) {
		var _p0 = modifier;
		if (_p0.ctor === 'Inverted') {
			return _user$project$Test_Runner_Node_Vendor_Console$colorsInverted(str);
		} else {
			return _user$project$Test_Runner_Node_Vendor_Console$dark(str);
		}
	});
var _user$project$Console_Text$applyModifiers = F2(
	function (modifiers, str) {
		return A3(_elm_lang$core$List$foldl, _user$project$Console_Text$applyModifiersHelp, str, modifiers);
	});
var _user$project$Console_Text$applyStyle = F2(
	function (style, str) {
		var _p1 = style;
		switch (_p1.ctor) {
			case 'Normal':
				return str;
			case 'Bold':
				return _user$project$Test_Runner_Node_Vendor_Console$bold(str);
			default:
				return _user$project$Test_Runner_Node_Vendor_Console$underline(str);
		}
	});
var _user$project$Console_Text$colorizeBackground = F2(
	function (color, str) {
		var _p2 = color;
		switch (_p2.ctor) {
			case 'Default':
				return str;
			case 'Red':
				return _user$project$Test_Runner_Node_Vendor_Console$bgRed(str);
			case 'Green':
				return _user$project$Test_Runner_Node_Vendor_Console$bgGreen(str);
			case 'Yellow':
				return _user$project$Test_Runner_Node_Vendor_Console$bgYellow(str);
			case 'Black':
				return _user$project$Test_Runner_Node_Vendor_Console$bgBlack(str);
			case 'Blue':
				return _user$project$Test_Runner_Node_Vendor_Console$bgBlue(str);
			case 'Magenta':
				return _user$project$Test_Runner_Node_Vendor_Console$bgMagenta(str);
			case 'Cyan':
				return _user$project$Test_Runner_Node_Vendor_Console$bgCyan(str);
			default:
				return _user$project$Test_Runner_Node_Vendor_Console$bgWhite(str);
		}
	});
var _user$project$Console_Text$colorizeForeground = F2(
	function (color, str) {
		var _p3 = color;
		switch (_p3.ctor) {
			case 'Default':
				return str;
			case 'Red':
				return _user$project$Test_Runner_Node_Vendor_Console$red(str);
			case 'Green':
				return _user$project$Test_Runner_Node_Vendor_Console$green(str);
			case 'Yellow':
				return _user$project$Test_Runner_Node_Vendor_Console$yellow(str);
			case 'Black':
				return _user$project$Test_Runner_Node_Vendor_Console$black(str);
			case 'Blue':
				return _user$project$Test_Runner_Node_Vendor_Console$blue(str);
			case 'Magenta':
				return _user$project$Test_Runner_Node_Vendor_Console$magenta(str);
			case 'Cyan':
				return _user$project$Test_Runner_Node_Vendor_Console$cyan(str);
			default:
				return _user$project$Test_Runner_Node_Vendor_Console$white(str);
		}
	});
var _user$project$Console_Text$render = F2(
	function (useColor, txt) {
		var _p4 = txt;
		if (_p4.ctor === 'Text') {
			var _p7 = _p4._1;
			var _p6 = _p4._0;
			var _p5 = useColor;
			if (_p5.ctor === 'UseColor') {
				return A2(
					_user$project$Console_Text$applyStyle,
					_p6.style,
					A2(
						_user$project$Console_Text$applyModifiers,
						_p6.modifiers,
						A2(
							_user$project$Console_Text$colorizeForeground,
							_p6.foreground,
							A2(_user$project$Console_Text$colorizeBackground, _p6.background, _p7))));
			} else {
				return _p7;
			}
		} else {
			return A2(
				_elm_lang$core$String$join,
				'',
				A2(
					_elm_lang$core$List$map,
					_user$project$Console_Text$render(useColor),
					_p4._0));
		}
	});
var _user$project$Console_Text$Texts = function (a) {
	return {ctor: 'Texts', _0: a};
};
var _user$project$Console_Text$concat = _user$project$Console_Text$Texts;
var _user$project$Console_Text$Text = F2(
	function (a, b) {
		return {ctor: 'Text', _0: a, _1: b};
	});
var _user$project$Console_Text$Monochrome = {ctor: 'Monochrome'};
var _user$project$Console_Text$UseColor = {ctor: 'UseColor'};
var _user$project$Console_Text$White = {ctor: 'White'};
var _user$project$Console_Text$Cyan = {ctor: 'Cyan'};
var _user$project$Console_Text$Magenta = {ctor: 'Magenta'};
var _user$project$Console_Text$Blue = {ctor: 'Blue'};
var _user$project$Console_Text$Black = {ctor: 'Black'};
var _user$project$Console_Text$Yellow = {ctor: 'Yellow'};
var _user$project$Console_Text$Green = {ctor: 'Green'};
var _user$project$Console_Text$Red = {ctor: 'Red'};
var _user$project$Console_Text$Default = {ctor: 'Default'};
var _user$project$Console_Text$Dark = {ctor: 'Dark'};
var _user$project$Console_Text$dark = function (txt) {
	var _p8 = txt;
	if (_p8.ctor === 'Text') {
		var _p9 = _p8._0;
		return A2(
			_user$project$Console_Text$Text,
			_elm_lang$core$Native_Utils.update(
				_p9,
				{
					modifiers: {ctor: '::', _0: _user$project$Console_Text$Dark, _1: _p9.modifiers}
				}),
			_p8._1);
	} else {
		return _user$project$Console_Text$Texts(
			A2(_elm_lang$core$List$map, _user$project$Console_Text$dark, _p8._0));
	}
};
var _user$project$Console_Text$Inverted = {ctor: 'Inverted'};
var _user$project$Console_Text$inverted = function (txt) {
	var _p10 = txt;
	if (_p10.ctor === 'Text') {
		var _p11 = _p10._0;
		return A2(
			_user$project$Console_Text$Text,
			_elm_lang$core$Native_Utils.update(
				_p11,
				{
					modifiers: {ctor: '::', _0: _user$project$Console_Text$Inverted, _1: _p11.modifiers}
				}),
			_p10._1);
	} else {
		return _user$project$Console_Text$Texts(
			A2(_elm_lang$core$List$map, _user$project$Console_Text$inverted, _p10._0));
	}
};
var _user$project$Console_Text$Underline = {ctor: 'Underline'};
var _user$project$Console_Text$underline = function (txt) {
	var _p12 = txt;
	if (_p12.ctor === 'Text') {
		return A2(
			_user$project$Console_Text$Text,
			_elm_lang$core$Native_Utils.update(
				_p12._0,
				{style: _user$project$Console_Text$Underline}),
			_p12._1);
	} else {
		return _user$project$Console_Text$Texts(
			A2(_elm_lang$core$List$map, _user$project$Console_Text$dark, _p12._0));
	}
};
var _user$project$Console_Text$Bold = {ctor: 'Bold'};
var _user$project$Console_Text$bold = function (txt) {
	var _p13 = txt;
	if (_p13.ctor === 'Text') {
		return A2(
			_user$project$Console_Text$Text,
			_elm_lang$core$Native_Utils.update(
				_p13._0,
				{style: _user$project$Console_Text$Bold}),
			_p13._1);
	} else {
		return _user$project$Console_Text$Texts(
			A2(_elm_lang$core$List$map, _user$project$Console_Text$dark, _p13._0));
	}
};
var _user$project$Console_Text$Normal = {ctor: 'Normal'};
var _user$project$Console_Text$plain = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$default = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$red = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Red,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$green = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Green,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$yellow = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Yellow,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$black = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Black,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$blue = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Blue,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$magenta = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Magenta,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$cyan = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Cyan,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$white = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$White,
		background: _user$project$Console_Text$Default,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgRed = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Red,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgGreen = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Green,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgYellow = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Yellow,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgBlack = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Black,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgBlue = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Blue,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgMagenta = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Magenta,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgCyan = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$Cyan,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$bgWhite = _user$project$Console_Text$Text(
	{
		foreground: _user$project$Console_Text$Default,
		background: _user$project$Console_Text$White,
		style: _user$project$Console_Text$Normal,
		modifiers: {ctor: '[]'}
	});
var _user$project$Console_Text$normal = function (txt) {
	var _p14 = txt;
	if (_p14.ctor === 'Text') {
		return A2(
			_user$project$Console_Text$Text,
			_elm_lang$core$Native_Utils.update(
				_p14._0,
				{style: _user$project$Console_Text$Normal}),
			_p14._1);
	} else {
		return _user$project$Console_Text$Texts(
			A2(_elm_lang$core$List$map, _user$project$Console_Text$dark, _p14._0));
	}
};

var _user$project$Haggis_Card$order = function (card) {
	return card.order;
};
var _user$project$Haggis_Card$rank = function (card) {
	return card.rank;
};
var _user$project$Haggis_Card$equal = F2(
	function (card, card_) {
		return _elm_lang$core$Native_Utils.eq(
			_user$project$Haggis_Card$rank(card),
			_user$project$Haggis_Card$rank(card_));
	});
var _user$project$Haggis_Card$points = function (card) {
	var _p0 = _user$project$Haggis_Card$rank(card);
	switch (_p0.ctor) {
		case 'Three':
			return 1;
		case 'Five':
			return 1;
		case 'Seven':
			return 1;
		case 'Nine':
			return 1;
		case 'Jack':
			return 2;
		case 'Queen':
			return 3;
		case 'King':
			return 5;
		default:
			return 0;
	}
};
var _user$project$Haggis_Card$suit = function (card) {
	return card.suit;
};
var _user$project$Haggis_Card$Card = F3(
	function (a, b, c) {
		return {suit: a, rank: b, order: c};
	});
var _user$project$Haggis_Card$Wild = {ctor: 'Wild'};
var _user$project$Haggis_Card$isNatural = function (card) {
	return !_elm_lang$core$Native_Utils.eq(
		_user$project$Haggis_Card$suit(card),
		_user$project$Haggis_Card$Wild);
};
var _user$project$Haggis_Card$Blue = {ctor: 'Blue'};
var _user$project$Haggis_Card$Green = {ctor: 'Green'};
var _user$project$Haggis_Card$Yellow = {ctor: 'Yellow'};
var _user$project$Haggis_Card$Orange = {ctor: 'Orange'};
var _user$project$Haggis_Card$Red = {ctor: 'Red'};
var _user$project$Haggis_Card$suits = {
	ctor: '::',
	_0: _user$project$Haggis_Card$Red,
	_1: {
		ctor: '::',
		_0: _user$project$Haggis_Card$Orange,
		_1: {
			ctor: '::',
			_0: _user$project$Haggis_Card$Yellow,
			_1: {
				ctor: '::',
				_0: _user$project$Haggis_Card$Green,
				_1: {
					ctor: '::',
					_0: _user$project$Haggis_Card$Blue,
					_1: {
						ctor: '::',
						_0: _user$project$Haggis_Card$Wild,
						_1: {ctor: '[]'}
					}
				}
			}
		}
	}
};
var _user$project$Haggis_Card$King = {ctor: 'King'};
var _user$project$Haggis_Card$Queen = {ctor: 'Queen'};
var _user$project$Haggis_Card$Jack = {ctor: 'Jack'};
var _user$project$Haggis_Card$Ten = {ctor: 'Ten'};
var _user$project$Haggis_Card$Nine = {ctor: 'Nine'};
var _user$project$Haggis_Card$Eight = {ctor: 'Eight'};
var _user$project$Haggis_Card$Seven = {ctor: 'Seven'};
var _user$project$Haggis_Card$Six = {ctor: 'Six'};
var _user$project$Haggis_Card$Five = {ctor: 'Five'};
var _user$project$Haggis_Card$Four = {ctor: 'Four'};
var _user$project$Haggis_Card$Three = {ctor: 'Three'};
var _user$project$Haggis_Card$Two = {ctor: 'Two'};
var _user$project$Haggis_Card$ranks = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: _user$project$Haggis_Card$Two,
		_1: {
			ctor: '::',
			_0: _user$project$Haggis_Card$Three,
			_1: {
				ctor: '::',
				_0: _user$project$Haggis_Card$Four,
				_1: {
					ctor: '::',
					_0: _user$project$Haggis_Card$Five,
					_1: {
						ctor: '::',
						_0: _user$project$Haggis_Card$Six,
						_1: {
							ctor: '::',
							_0: _user$project$Haggis_Card$Seven,
							_1: {
								ctor: '::',
								_0: _user$project$Haggis_Card$Eight,
								_1: {
									ctor: '::',
									_0: _user$project$Haggis_Card$Nine,
									_1: {
										ctor: '::',
										_0: _user$project$Haggis_Card$Ten,
										_1: {
											ctor: '::',
											_0: _user$project$Haggis_Card$Jack,
											_1: {
												ctor: '::',
												_0: _user$project$Haggis_Card$Queen,
												_1: {
													ctor: '::',
													_0: _user$project$Haggis_Card$King,
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	});
var _user$project$Haggis_Card$rankOrdering = _matthewsj$elm_ordering$Ordering$explicit(
	_elm_lang$core$Array$toList(_user$project$Haggis_Card$ranks));
var _user$project$Haggis_Card$byRank = F2(
	function (r1, r2) {
		return A2(_user$project$Haggis_Card$rankOrdering, r1, r2);
	});
var _user$project$Haggis_Card$cardOrdering = A2(
	_matthewsj$elm_ordering$Ordering$byRank,
	function (card) {
		return 1;
	},
	F2(
		function (c1, c2) {
			return A2(
				_user$project$Haggis_Card$rankOrdering,
				_user$project$Haggis_Card$rank(c1),
				_user$project$Haggis_Card$rank(c2));
		}));
var _user$project$Haggis_Card$toRank = function (order) {
	var _p1 = order;
	switch (_p1) {
		case 2:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Two);
		case 3:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Three);
		case 4:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Four);
		case 5:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Five);
		case 6:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Six);
		case 7:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Seven);
		case 8:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Eight);
		case 9:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Nine);
		case 10:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Ten);
		case 11:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Jack);
		case 12:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$Queen);
		case 13:
			return _elm_lang$core$Maybe$Just(_user$project$Haggis_Card$King);
		default:
			return _elm_lang$core$Maybe$Nothing;
	}
};

var _user$project$Haggis_Cards$subsets = function (cards) {
	var _p0 = cards;
	if (_p0.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var _p1 = _p0._1;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p0._0),
				_user$project$Haggis_Cards$subsets(_p1)),
			_user$project$Haggis_Cards$subsets(_p1));
	}
};

var _user$project$Haggis_Combination$collectCardsWithRanks = F2(
	function (ranks, cards) {
		return A2(
			_elm_lang$core$List$map,
			function (r) {
				return A2(
					_elm_lang$core$List$filter,
					function (c) {
						return _elm_lang$core$Native_Utils.eq(
							_user$project$Haggis_Card$order(c),
							r);
					},
					cards);
			},
			ranks);
	});
var _user$project$Haggis_Combination$countWildsNeeded = F2(
	function (setSize, sets) {
		return _elm_lang$core$List$sum(
			A2(
				_elm_lang$core$List$map,
				function (s) {
					return setSize - _elm_lang$core$List$length(s);
				},
				sets));
	});
var _user$project$Haggis_Combination$hasEnoughCards = F2(
	function (setSize, cardCount) {
		return (_elm_lang$core$Native_Utils.eq(setSize, 1) && (_elm_lang$core$Native_Utils.cmp(cardCount, 3) > -1)) || ((_elm_lang$core$Native_Utils.cmp(setSize, 1) > 0) && (_elm_lang$core$Native_Utils.cmp(cardCount, setSize * 2) > -1));
	});
var _user$project$Haggis_Combination$findLowestOrder = function (cards) {
	return _elm_lang$core$List$minimum(
		A2(_elm_lang$core$List$map, _user$project$Haggis_Card$order, cards));
};
var _user$project$Haggis_Combination$collectRanksInRun = F2(
	function (runLength, cards) {
		var lowestOrder = _user$project$Haggis_Combination$findLowestOrder(cards);
		var _p0 = lowestOrder;
		if (_p0.ctor === 'Just') {
			var _p2 = _p0._0;
			var high = (_p2 + runLength) - 1;
			var highestRank = _user$project$Haggis_Card$toRank(high);
			var _p1 = highestRank;
			if (_p1.ctor === 'Just') {
				return _elm_lang$core$Maybe$Just(
					{
						ctor: '_Tuple2',
						_0: _p1._0,
						_1: A2(_elm_lang$core$List$range, _p2, high)
					});
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$Haggis_Combination$dropDuplicates_ = F2(
	function (existing, remaining) {
		dropDuplicates_:
		while (true) {
			var _p3 = remaining;
			if (_p3.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p5 = _p3._1;
				var _p4 = _p3._0;
				if (A2(_elm_lang$core$List$member, _p4, existing)) {
					var _v3 = existing,
						_v4 = _p5;
					existing = _v3;
					remaining = _v4;
					continue dropDuplicates_;
				} else {
					return {
						ctor: '::',
						_0: _p4,
						_1: A2(
							_user$project$Haggis_Combination$dropDuplicates_,
							{ctor: '::', _0: _p4, _1: existing},
							_p5)
					};
				}
			}
		}
	});
var _user$project$Haggis_Combination$dropDuplicates = function (suits) {
	return A2(
		_user$project$Haggis_Combination$dropDuplicates_,
		{ctor: '[]'},
		suits);
};
var _user$project$Haggis_Combination$countSuits = function (cards) {
	return _elm_lang$core$List$length(
		_user$project$Haggis_Combination$dropDuplicates(
			A2(_elm_lang$core$List$map, _user$project$Haggis_Card$suit, cards)));
};
var _user$project$Haggis_Combination$hasFourSuits = function (cards) {
	return _elm_lang$core$Native_Utils.eq(
		_user$project$Haggis_Combination$countSuits(cards),
		4);
};
var _user$project$Haggis_Combination$hasSameSuit = F2(
	function (c1, c2) {
		return _elm_lang$core$Native_Utils.eq(
			_user$project$Haggis_Card$suit(c1),
			_user$project$Haggis_Card$suit(c2));
	});
var _user$project$Haggis_Combination$allSameSuit = function (cards) {
	var _p6 = cards;
	if (_p6.ctor === '::') {
		return A2(
			_elm_lang$core$List$all,
			_user$project$Haggis_Combination$hasSameSuit(_p6._0),
			_p6._1);
	} else {
		return false;
	}
};
var _user$project$Haggis_Combination$findRank = function (cards) {
	findRank:
	while (true) {
		var _p7 = cards;
		if (_p7.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			if (_p7._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Card$rank(_p7._0));
			} else {
				var _p9 = _p7._0;
				var _p8 = _p9.suit;
				if (_p8.ctor === 'Wild') {
					var _v8 = _p7._1;
					cards = _v8;
					continue findRank;
				} else {
					return _elm_lang$core$Maybe$Just(
						_user$project$Haggis_Card$rank(_p9));
				}
			}
		}
	}
};
var _user$project$Haggis_Combination$allSameRank = function (cards) {
	var _p10 = cards;
	if (_p10.ctor === '[]') {
		return false;
	} else {
		return A2(
			_elm_lang$core$List$all,
			_user$project$Haggis_Card$equal(_p10._0),
			_p10._1);
	}
};
var _user$project$Haggis_Combination$split = function (cards) {
	return A2(_elm_lang$core$List$partition, _user$project$Haggis_Card$isNatural, cards);
};
var _user$project$Haggis_Combination$canFormSequence = F3(
	function (setSize, ranks, cards) {
		var wildsNeeded = A2(
			_user$project$Haggis_Combination$countWildsNeeded,
			setSize,
			A2(_user$project$Haggis_Combination$collectCardsWithRanks, ranks, cards));
		var _p11 = _user$project$Haggis_Combination$split(cards);
		var naturals = _p11._0;
		var wilds = _p11._1;
		var wildsUsed = _elm_lang$core$List$length(
			A2(
				_elm_lang$core$List$filter,
				function (w) {
					return A2(
						_elm_lang$core$List$member,
						_user$project$Haggis_Card$order(w),
						ranks);
				},
				wilds));
		return _elm_lang$core$Native_Utils.eq(
			wildsNeeded,
			_elm_lang$core$List$length(wilds) - wildsUsed);
	});
var _user$project$Haggis_Combination$Bomb = {ctor: 'Bomb'};
var _user$project$Haggis_Combination$Sequence = {ctor: 'Sequence'};
var _user$project$Haggis_Combination$Set = {ctor: 'Set'};
var _user$project$Haggis_Combination$EightOfAKind = function (a) {
	return {ctor: 'EightOfAKind', _0: a};
};
var _user$project$Haggis_Combination$SevenOfAKind = function (a) {
	return {ctor: 'SevenOfAKind', _0: a};
};
var _user$project$Haggis_Combination$SixOfAKind = function (a) {
	return {ctor: 'SixOfAKind', _0: a};
};
var _user$project$Haggis_Combination$FiveOfAKind = function (a) {
	return {ctor: 'FiveOfAKind', _0: a};
};
var _user$project$Haggis_Combination$FourOfAKind = function (a) {
	return {ctor: 'FourOfAKind', _0: a};
};
var _user$project$Haggis_Combination$Triple = function (a) {
	return {ctor: 'Triple', _0: a};
};
var _user$project$Haggis_Combination$Pair = function (a) {
	return {ctor: 'Pair', _0: a};
};
var _user$project$Haggis_Combination$Single = function (a) {
	return {ctor: 'Single', _0: a};
};
var _user$project$Haggis_Combination$makeSet = function (cards) {
	var highestRank = _user$project$Haggis_Combination$findRank(cards);
	var _p12 = highestRank;
	if (_p12.ctor === 'Just') {
		var _p14 = _p12._0;
		var _p13 = _elm_lang$core$List$length(cards);
		switch (_p13) {
			case 1:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$Single(_p14));
			case 2:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$Pair(_p14));
			case 3:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$Triple(_p14));
			case 4:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$FourOfAKind(_p14));
			case 5:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$FiveOfAKind(_p14));
			case 6:
				return _elm_lang$core$Maybe$Just(
					_user$project$Haggis_Combination$SixOfAKind(_p14));
			default:
				return _elm_lang$core$Maybe$Nothing;
		}
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Haggis_Combination$set = function (cards) {
	var highestRank = _user$project$Haggis_Combination$findRank(cards);
	var _p15 = _user$project$Haggis_Combination$split(cards);
	var naturals = _p15._0;
	var wilds = _p15._1;
	var _p16 = highestRank;
	if (_p16.ctor === 'Just') {
		return _user$project$Haggis_Combination$allSameRank(naturals) ? _user$project$Haggis_Combination$makeSet(cards) : ((_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(wilds),
			1) && _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(cards),
			1)) ? _elm_lang$core$Maybe$Just(
			_user$project$Haggis_Combination$Single(_p16._0)) : _elm_lang$core$Maybe$Nothing);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$Haggis_Combination$RunOfSixOfAKinds = F2(
	function (a, b) {
		return {ctor: 'RunOfSixOfAKinds', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$RunOfFiveOfAKinds = F2(
	function (a, b) {
		return {ctor: 'RunOfFiveOfAKinds', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$RunOfFourOfAKinds = F2(
	function (a, b) {
		return {ctor: 'RunOfFourOfAKinds', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$RunOfTriples = F2(
	function (a, b) {
		return {ctor: 'RunOfTriples', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$RunOfPairs = F2(
	function (a, b) {
		return {ctor: 'RunOfPairs', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$RunOfSingles = F2(
	function (a, b) {
		return {ctor: 'RunOfSingles', _0: a, _1: b};
	});
var _user$project$Haggis_Combination$makeSequence = F3(
	function (runLength, setSize, rank) {
		var _p17 = setSize;
		switch (_p17) {
			case 1:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfSingles, runLength, rank));
			case 2:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfPairs, runLength, rank));
			case 3:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfTriples, runLength, rank));
			case 4:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfFourOfAKinds, runLength, rank));
			case 5:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfFiveOfAKinds, runLength, rank));
			case 6:
				return _elm_lang$core$Maybe$Just(
					A2(_user$project$Haggis_Combination$RunOfSixOfAKinds, runLength, rank));
			default:
				return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$Haggis_Combination$maybeRunOfSets = F2(
	function (cards, setSize) {
		var cardCount = _elm_lang$core$List$length(cards);
		var runLength = (cardCount / setSize) | 0;
		var _p18 = _user$project$Haggis_Combination$split(cards);
		var naturals = _p18._0;
		var wilds = _p18._1;
		var ranksInRun = A2(_user$project$Haggis_Combination$collectRanksInRun, runLength, naturals);
		var _p19 = ranksInRun;
		if ((_p19.ctor === 'Just') && (_p19._0.ctor === '_Tuple2')) {
			return (A2(_user$project$Haggis_Combination$hasEnoughCards, setSize, cardCount) && (_elm_lang$core$Native_Utils.eq(cardCount, runLength * setSize) && A3(_user$project$Haggis_Combination$canFormSequence, setSize, _p19._0._1, cards))) ? A3(_user$project$Haggis_Combination$makeSequence, runLength, setSize, _p19._0._0) : _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _user$project$Haggis_Combination$sequence = function (cards) {
	var _p20 = _user$project$Haggis_Combination$split(cards);
	var naturals = _p20._0;
	var wilds = _p20._1;
	var setSizes = A2(
		_elm_lang$core$List$range,
		_user$project$Haggis_Combination$countSuits(naturals),
		_user$project$Haggis_Combination$countSuits(cards));
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$List$length(naturals),
		0) ? {ctor: '[]'} : A2(
		_elm_lang$core$List$filterMap,
		_user$project$Haggis_Combination$maybeRunOfSets(cards),
		setSizes);
};
var _user$project$Haggis_Combination$Suited = {ctor: 'Suited'};
var _user$project$Haggis_Combination$JQK = {ctor: 'JQK'};
var _user$project$Haggis_Combination$QK = {ctor: 'QK'};
var _user$project$Haggis_Combination$JK = {ctor: 'JK'};
var _user$project$Haggis_Combination$JQ = {ctor: 'JQ'};
var _user$project$Haggis_Combination$Rainbow = {ctor: 'Rainbow'};
var _user$project$Haggis_Combination$bomb = function (cards) {
	var ranks = A2(
		_elm_lang$core$List$sortWith,
		_user$project$Haggis_Card$byRank,
		A2(_elm_lang$core$List$map, _user$project$Haggis_Card$rank, cards));
	var _p21 = ranks;
	_v15_5:
	do {
		if ((_p21.ctor === '::') && (_p21._1.ctor === '::')) {
			if (_p21._1._1.ctor === '[]') {
				switch (_p21._0.ctor) {
					case 'Jack':
						switch (_p21._1._0.ctor) {
							case 'Queen':
								return _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$JQ);
							case 'King':
								return _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$JK);
							default:
								break _v15_5;
						}
					case 'Queen':
						if (_p21._1._0.ctor === 'King') {
							return _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$QK);
						} else {
							break _v15_5;
						}
					default:
						break _v15_5;
				}
			} else {
				if (_p21._1._1._1.ctor === '[]') {
					if (((_p21._0.ctor === 'Jack') && (_p21._1._0.ctor === 'Queen')) && (_p21._1._1._0.ctor === 'King')) {
						return _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$JQK);
					} else {
						break _v15_5;
					}
				} else {
					if (((((_p21._0.ctor === 'Three') && (_p21._1._0.ctor === 'Five')) && (_p21._1._1._0.ctor === 'Seven')) && (_p21._1._1._1._0.ctor === 'Nine')) && (_p21._1._1._1._1.ctor === '[]')) {
						return _user$project$Haggis_Combination$allSameSuit(cards) ? _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$Suited) : (_user$project$Haggis_Combination$hasFourSuits(cards) ? _elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$Rainbow) : _elm_lang$core$Maybe$Nothing);
					} else {
						break _v15_5;
					}
				}
			}
		} else {
			break _v15_5;
		}
	} while(false);
	return _elm_lang$core$Maybe$Nothing;
};

var _user$project$Haggis_Hand$collectBombs = function (hand) {
	return A2(
		_elm_lang$core$List$filterMap,
		_user$project$Haggis_Combination$bomb,
		_user$project$Haggis_Cards$subsets(hand));
};
var _user$project$Haggis_Hand$collectSequences = function (hand) {
	return A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Basics$identity,
		A2(
			_elm_lang$core$List$concatMap,
			_user$project$Haggis_Combination$sequence,
			_user$project$Haggis_Cards$subsets(hand)));
};
var _user$project$Haggis_Hand$collectSets = function (hand) {
	return A2(
		_elm_lang$core$List$filterMap,
		_user$project$Haggis_Combination$set,
		_user$project$Haggis_Cards$subsets(hand));
};

var _user$project$Native_RunTest = (function() {
  return {
    runThunk: function(thunk) {
      try {
        // Attempt to run the thunk as normal.
        return thunk({ ctor: "_Tuple0" });
      } catch (err) {
        // If it throws, return a test failure instead of crashing.
        return {
          ctor: "::",
          _0: _elm_community$elm_test$Expect$fail(
            'This test failed because it threw an exception: "' + err + '"'
          ),
          _1: { ctor: "[]" }
        };
      }
    }
  };
})();

var _user$project$Tests$king = {suit: _user$project$Haggis_Card$Wild, rank: _user$project$Haggis_Card$King, order: 13};
var _user$project$Tests$queen = {suit: _user$project$Haggis_Card$Wild, rank: _user$project$Haggis_Card$Queen, order: 12};
var _user$project$Tests$jack = {suit: _user$project$Haggis_Card$Wild, rank: _user$project$Haggis_Card$Jack, order: 11};
var _user$project$Tests$greenTen = {suit: _user$project$Haggis_Card$Green, rank: _user$project$Haggis_Card$Ten, order: 10};
var _user$project$Tests$blueTen = {suit: _user$project$Haggis_Card$Blue, rank: _user$project$Haggis_Card$Ten, order: 10};
var _user$project$Tests$yellowNine = {suit: _user$project$Haggis_Card$Yellow, rank: _user$project$Haggis_Card$Nine, order: 9};
var _user$project$Tests$redNine = {suit: _user$project$Haggis_Card$Red, rank: _user$project$Haggis_Card$Nine, order: 9};
var _user$project$Tests$orangeSeven = {suit: _user$project$Haggis_Card$Orange, rank: _user$project$Haggis_Card$Seven, order: 7};
var _user$project$Tests$redSeven = {suit: _user$project$Haggis_Card$Red, rank: _user$project$Haggis_Card$Seven, order: 7};
var _user$project$Tests$greenFive = {suit: _user$project$Haggis_Card$Green, rank: _user$project$Haggis_Card$Five, order: 5};
var _user$project$Tests$redFive = {suit: _user$project$Haggis_Card$Red, rank: _user$project$Haggis_Card$Five, order: 5};
var _user$project$Tests$blueFour = {suit: _user$project$Haggis_Card$Blue, rank: _user$project$Haggis_Card$Four, order: 4};
var _user$project$Tests$redThree = {suit: _user$project$Haggis_Card$Red, rank: _user$project$Haggis_Card$Three, order: 3};
var _user$project$Tests$greenThree = {suit: _user$project$Haggis_Card$Green, rank: _user$project$Haggis_Card$Three, order: 3};
var _user$project$Tests$blueThree = {suit: _user$project$Haggis_Card$Blue, rank: _user$project$Haggis_Card$Three, order: 3};
var _user$project$Tests$greenTwo = {suit: _user$project$Haggis_Card$Green, rank: _user$project$Haggis_Card$Two, order: 2};
var _user$project$Tests$blueTwo = {suit: _user$project$Haggis_Card$Blue, rank: _user$project$Haggis_Card$Two, order: 2};
var _user$project$Tests$hand = {
	ctor: '::',
	_0: _user$project$Tests$blueTwo,
	_1: {
		ctor: '::',
		_0: _user$project$Tests$greenTwo,
		_1: {
			ctor: '::',
			_0: _user$project$Tests$blueThree,
			_1: {
				ctor: '::',
				_0: _user$project$Tests$redThree,
				_1: {
					ctor: '::',
					_0: _user$project$Tests$greenThree,
					_1: {
						ctor: '::',
						_0: _user$project$Tests$blueFour,
						_1: {
							ctor: '::',
							_0: _user$project$Tests$redFive,
							_1: {
								ctor: '::',
								_0: _user$project$Tests$greenFive,
								_1: {
									ctor: '::',
									_0: _user$project$Tests$redSeven,
									_1: {
										ctor: '::',
										_0: _user$project$Tests$orangeSeven,
										_1: {
											ctor: '::',
											_0: _user$project$Tests$redNine,
											_1: {
												ctor: '::',
												_0: _user$project$Tests$yellowNine,
												_1: {
													ctor: '::',
													_0: _user$project$Tests$blueTen,
													_1: {
														ctor: '::',
														_0: _user$project$Tests$greenTen,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _user$project$Tests$all = A2(
	_elm_community$elm_test$Test$describe,
	'Haggis',
	{
		ctor: '::',
		_0: A2(
			_elm_community$elm_test$Test$describe,
			'Haggis.Card',
			{
				ctor: '::',
				_0: A2(
					_elm_community$elm_test$Test$test,
					'new card has correct suit',
					function (_p0) {
						var _p1 = _p0;
						return A2(
							_elm_community$elm_test$Expect$equal,
							_user$project$Haggis_Card$suit(_user$project$Tests$blueTwo),
							_user$project$Haggis_Card$Blue);
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_community$elm_test$Test$test,
						'new card has correct rank',
						function (_p2) {
							var _p3 = _p2;
							return A2(
								_elm_community$elm_test$Expect$equal,
								_user$project$Haggis_Card$rank(_user$project$Tests$blueTwo),
								_user$project$Haggis_Card$Two);
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_community$elm_test$Test$test,
							'new card has correct points',
							function (_p4) {
								var _p5 = _p4;
								return A2(
									_elm_community$elm_test$Expect$equal,
									_user$project$Haggis_Card$points(_user$project$Tests$redSeven),
									1);
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_community$elm_test$Test$test,
								'card with lower rank less than card with higher rank',
								function (_p6) {
									var _p7 = _p6;
									return A2(
										_elm_community$elm_test$Expect$lessThan,
										_user$project$Haggis_Card$order(_user$project$Tests$redSeven),
										_user$project$Haggis_Card$order(_user$project$Tests$blueTwo));
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_community$elm_test$Test$test,
									'two cards with the same rank are equal',
									function (_p8) {
										var _p9 = _p8;
										return A2(
											_elm_community$elm_test$Expect$equal,
											_user$project$Haggis_Card$order(_user$project$Tests$blueTwo),
											_user$project$Haggis_Card$order(_user$project$Tests$greenTwo));
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_community$elm_test$Test$test,
										'non-matching single cards do not match',
										function (_p10) {
											var _p11 = _p10;
											return A2(_elm_community$elm_test$Expect$notEqual, _user$project$Tests$blueTwo, _user$project$Tests$redSeven);
										}),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}),
		_1: {
			ctor: '::',
			_0: A2(
				_elm_community$elm_test$Test$describe,
				'Haggis.Combination',
				{
					ctor: '::',
					_0: A2(
						_elm_community$elm_test$Test$describe,
						'Haggis.Combination.Set',
						{
							ctor: '::',
							_0: A2(
								_elm_community$elm_test$Test$test,
								'one card is a single',
								function (_p12) {
									var _p13 = _p12;
									return A2(
										_elm_community$elm_test$Expect$equal,
										_user$project$Haggis_Combination$set(
											{
												ctor: '::',
												_0: _user$project$Tests$redSeven,
												_1: {ctor: '[]'}
											}),
										_elm_lang$core$Maybe$Just(
											_user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Seven)));
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_community$elm_test$Test$test,
									'two cards of matching rank are a pair',
									function (_p14) {
										var _p15 = _p14;
										return A2(
											_elm_community$elm_test$Expect$equal,
											_user$project$Haggis_Combination$set(
												{
													ctor: '::',
													_0: _user$project$Tests$blueTwo,
													_1: {
														ctor: '::',
														_0: _user$project$Tests$greenTwo,
														_1: {ctor: '[]'}
													}
												}),
											_elm_lang$core$Maybe$Just(
												_user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Two)));
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_community$elm_test$Test$test,
										'two cards with unmatched ranks are not a combination',
										function (_p16) {
											var _p17 = _p16;
											return A2(
												_elm_community$elm_test$Expect$equal,
												_user$project$Haggis_Combination$set(
													{
														ctor: '::',
														_0: _user$project$Tests$blueTwo,
														_1: {
															ctor: '::',
															_0: _user$project$Tests$redSeven,
															_1: {ctor: '[]'}
														}
													}),
												_elm_lang$core$Maybe$Nothing);
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_community$elm_test$Test$test,
											'two matched number cards with one wild is a triple',
											function (_p18) {
												var _p19 = _p18;
												return A2(
													_elm_community$elm_test$Expect$equal,
													_user$project$Haggis_Combination$set(
														{
															ctor: '::',
															_0: _user$project$Tests$blueTwo,
															_1: {
																ctor: '::',
																_0: _user$project$Tests$greenTwo,
																_1: {
																	ctor: '::',
																	_0: _user$project$Tests$jack,
																	_1: {ctor: '[]'}
																}
															}
														}),
													_elm_lang$core$Maybe$Just(
														_user$project$Haggis_Combination$Triple(_user$project$Haggis_Card$Two)));
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_community$elm_test$Test$test,
												'a wild with a number card is a pair with the number card\'s rank',
												function (_p20) {
													var _p21 = _p20;
													return A2(
														_elm_community$elm_test$Expect$equal,
														_user$project$Haggis_Combination$set(
															{
																ctor: '::',
																_0: _user$project$Tests$jack,
																_1: {
																	ctor: '::',
																	_0: _user$project$Tests$blueTwo,
																	_1: {ctor: '[]'}
																}
															}),
														_elm_lang$core$Maybe$Just(
															_user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Two)));
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_community$elm_test$Test$test,
													'two wild cards is NOT a pair',
													function (_p22) {
														var _p23 = _p22;
														return A2(
															_elm_community$elm_test$Expect$notEqual,
															_user$project$Haggis_Combination$set(
																{
																	ctor: '::',
																	_0: _user$project$Tests$jack,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$queen,
																		_1: {ctor: '[]'}
																	}
																}),
															_elm_lang$core$Maybe$Just(
																_user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Jack)));
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_elm_community$elm_test$Test$test,
														'one wild card is a single',
														function (_p24) {
															var _p25 = _p24;
															return A2(
																_elm_community$elm_test$Expect$equal,
																_user$project$Haggis_Combination$set(
																	{
																		ctor: '::',
																		_0: _user$project$Tests$jack,
																		_1: {ctor: '[]'}
																	}),
																_elm_lang$core$Maybe$Just(
																	_user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Jack)));
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_elm_community$elm_test$Test$test,
															'two matched spot cards plus three wilds is a five-of-a-kind',
															function (_p26) {
																var _p27 = _p26;
																return A2(
																	_elm_community$elm_test$Expect$equal,
																	_user$project$Haggis_Combination$set(
																		{
																			ctor: '::',
																			_0: _user$project$Tests$blueTwo,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$greenTwo,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$jack,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$queen,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$king,
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}),
																	_elm_lang$core$Maybe$Just(
																		_user$project$Haggis_Combination$FiveOfAKind(_user$project$Haggis_Card$Two)));
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_elm_community$elm_test$Test$test,
																'a pair of tens plus two wilds is a four-of-a-kind',
																function (_p28) {
																	var _p29 = _p28;
																	return A2(
																		_elm_community$elm_test$Expect$equal,
																		_user$project$Haggis_Combination$set(
																			{
																				ctor: '::',
																				_0: _user$project$Tests$blueTen,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$greenTen,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$jack,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$king,
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}),
																		_elm_lang$core$Maybe$Just(
																			_user$project$Haggis_Combination$FourOfAKind(_user$project$Haggis_Card$Ten)));
																}),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_community$elm_test$Test$describe,
							'Haggis.Combination.Bomb',
							{
								ctor: '::',
								_0: A2(
									_elm_community$elm_test$Test$test,
									'one wild card is not a bomb',
									function (_p30) {
										var _p31 = _p30;
										return A2(
											_elm_community$elm_test$Expect$equal,
											_user$project$Haggis_Combination$bomb(
												{
													ctor: '::',
													_0: _user$project$Tests$king,
													_1: {ctor: '[]'}
												}),
											_elm_lang$core$Maybe$Nothing);
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_community$elm_test$Test$test,
										'two wild cards is a bomb',
										function (_p32) {
											var _p33 = _p32;
											return A2(
												_elm_community$elm_test$Expect$equal,
												_user$project$Haggis_Combination$bomb(
													{
														ctor: '::',
														_0: _user$project$Tests$queen,
														_1: {
															ctor: '::',
															_0: _user$project$Tests$king,
															_1: {ctor: '[]'}
														}
													}),
												_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$QK));
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_community$elm_test$Test$test,
											'two wild cards is a bomb, order does not matter',
											function (_p34) {
												var _p35 = _p34;
												return A2(
													_elm_community$elm_test$Expect$equal,
													_user$project$Haggis_Combination$bomb(
														{
															ctor: '::',
															_0: _user$project$Tests$king,
															_1: {
																ctor: '::',
																_0: _user$project$Tests$jack,
																_1: {ctor: '[]'}
															}
														}),
													_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$JK));
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_community$elm_test$Test$test,
												'three wild cards is a bomb',
												function (_p36) {
													var _p37 = _p36;
													return A2(
														_elm_community$elm_test$Expect$equal,
														_user$project$Haggis_Combination$bomb(
															{
																ctor: '::',
																_0: _user$project$Tests$jack,
																_1: {
																	ctor: '::',
																	_0: _user$project$Tests$queen,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$king,
																		_1: {ctor: '[]'}
																	}
																}
															}),
														_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$JQK));
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_community$elm_test$Test$test,
													'three wild cards is not a set',
													function (_p38) {
														var _p39 = _p38;
														return A2(
															_elm_community$elm_test$Expect$equal,
															_user$project$Haggis_Combination$set(
																{
																	ctor: '::',
																	_0: _user$project$Tests$jack,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$queen,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$king,
																			_1: {ctor: '[]'}
																		}
																	}
																}),
															_elm_lang$core$Maybe$Nothing);
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_elm_community$elm_test$Test$test,
														'four distinct, same-suited odd cards is a bomb',
														function (_p40) {
															var _p41 = _p40;
															return A2(
																_elm_community$elm_test$Expect$equal,
																_user$project$Haggis_Combination$bomb(
																	{
																		ctor: '::',
																		_0: _user$project$Tests$redThree,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$redFive,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$redSeven,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$redNine,
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}),
																_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$Suited));
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_elm_community$elm_test$Test$test,
															'A suited bomb is a suited bomb regardless of card order',
															function (_p42) {
																var _p43 = _p42;
																return A2(
																	_elm_community$elm_test$Expect$equal,
																	_user$project$Haggis_Combination$bomb(
																		{
																			ctor: '::',
																			_0: _user$project$Tests$redFive,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$redNine,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$redThree,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$redSeven,
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}),
																	_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$Suited));
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_elm_community$elm_test$Test$test,
																'four distinct odd cards, with distinct suits, is a bomb',
																function (_p44) {
																	var _p45 = _p44;
																	return A2(
																		_elm_community$elm_test$Expect$equal,
																		_user$project$Haggis_Combination$bomb(
																			{
																				ctor: '::',
																				_0: _user$project$Tests$blueThree,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$greenFive,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$redSeven,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$yellowNine,
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}),
																		_elm_lang$core$Maybe$Just(_user$project$Haggis_Combination$Rainbow));
																}),
															_1: {
																ctor: '::',
																_0: A2(
																	_elm_community$elm_test$Test$test,
																	'four distinct odd cards, with 2-3 suits, is a not bomb',
																	function (_p46) {
																		var _p47 = _p46;
																		return A2(
																			_elm_community$elm_test$Expect$equal,
																			_user$project$Haggis_Combination$bomb(
																				{
																					ctor: '::',
																					_0: _user$project$Tests$blueThree,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$redFive,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$redSeven,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Tests$yellowNine,
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}),
																			_elm_lang$core$Maybe$Nothing);
																	}),
																_1: {ctor: '[]'}
															}
														}
													}
												}
											}
										}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_community$elm_test$Test$describe,
								'Haggis.Combination.Sequence',
								{
									ctor: '::',
									_0: A2(
										_elm_community$elm_test$Test$test,
										'empty set of cards is not a sequence',
										function (_p48) {
											var _p49 = _p48;
											return A2(
												_elm_community$elm_test$Expect$equal,
												_user$project$Haggis_Combination$sequence(
													{ctor: '[]'}),
												{ctor: '[]'});
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_community$elm_test$Test$test,
											'three consecutive singles with mixed suits is NOT a sequence',
											function (_p50) {
												var _p51 = _p50;
												return A2(
													_elm_community$elm_test$Expect$equal,
													_user$project$Haggis_Combination$sequence(
														{
															ctor: '::',
															_0: _user$project$Tests$blueTwo,
															_1: {
																ctor: '::',
																_0: _user$project$Tests$redThree,
																_1: {
																	ctor: '::',
																	_0: _user$project$Tests$blueFour,
																	_1: {ctor: '[]'}
																}
															}
														}),
													{ctor: '[]'});
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_community$elm_test$Test$test,
												'two consecutive pairs with mixed suits is NOT a sequence',
												function (_p52) {
													var _p53 = _p52;
													return A2(
														_elm_community$elm_test$Expect$equal,
														_user$project$Haggis_Combination$sequence(
															{
																ctor: '::',
																_0: _user$project$Tests$blueTwo,
																_1: {
																	ctor: '::',
																	_0: _user$project$Tests$greenTwo,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$redThree,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$blueThree,
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}),
														{ctor: '[]'});
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_community$elm_test$Test$test,
													'three consecutive singles is a sequence',
													function (_p54) {
														var _p55 = _p54;
														return A2(
															_elm_community$elm_test$Expect$equal,
															_user$project$Haggis_Combination$sequence(
																{
																	ctor: '::',
																	_0: _user$project$Tests$blueTwo,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$blueThree,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$blueFour,
																			_1: {ctor: '[]'}
																		}
																	}
																}),
															{
																ctor: '::',
																_0: A2(_user$project$Haggis_Combination$RunOfSingles, 3, _user$project$Haggis_Card$Four),
																_1: {ctor: '[]'}
															});
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_elm_community$elm_test$Test$test,
														'three nonconsecutive singles is not a sequence',
														function (_p56) {
															var _p57 = _p56;
															return A2(
																_elm_community$elm_test$Expect$equal,
																_user$project$Haggis_Combination$sequence(
																	{
																		ctor: '::',
																		_0: _user$project$Tests$blueTwo,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$blueThree,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$redThree,
																				_1: {ctor: '[]'}
																			}
																		}
																	}),
																{ctor: '[]'});
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_elm_community$elm_test$Test$test,
															'two consecutive singles is not a sequence',
															function (_p58) {
																var _p59 = _p58;
																return A2(
																	_elm_community$elm_test$Expect$equal,
																	_user$project$Haggis_Combination$sequence(
																		{
																			ctor: '::',
																			_0: _user$project$Tests$blueTwo,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$blueThree,
																				_1: {ctor: '[]'}
																			}
																		}),
																	{ctor: '[]'});
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_elm_community$elm_test$Test$test,
																'three wildcards is not a sequence',
																function (_p60) {
																	var _p61 = _p60;
																	return A2(
																		_elm_community$elm_test$Expect$equal,
																		_user$project$Haggis_Combination$sequence(
																			{
																				ctor: '::',
																				_0: _user$project$Tests$jack,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$queen,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$king,
																						_1: {ctor: '[]'}
																					}
																				}
																			}),
																		{ctor: '[]'});
																}),
															_1: {
																ctor: '::',
																_0: A2(
																	_elm_community$elm_test$Test$test,
																	'two consecutive pairs is a sequence',
																	function (_p62) {
																		var _p63 = _p62;
																		return A2(
																			_elm_community$elm_test$Expect$equal,
																			_user$project$Haggis_Combination$sequence(
																				{
																					ctor: '::',
																					_0: _user$project$Tests$blueTwo,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$greenTwo,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$blueThree,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Tests$greenThree,
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}),
																			{
																				ctor: '::',
																				_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Three),
																				_1: {ctor: '[]'}
																			});
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_elm_community$elm_test$Test$test,
																		'card order should not affect sequence identification',
																		function (_p64) {
																			var _p65 = _p64;
																			return A2(
																				_elm_community$elm_test$Expect$equal,
																				_user$project$Haggis_Combination$sequence(
																					{
																						ctor: '::',
																						_0: _user$project$Tests$greenTwo,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$blueThree,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Tests$greenThree,
																								_1: {
																									ctor: '::',
																									_0: _user$project$Tests$blueTwo,
																									_1: {ctor: '[]'}
																								}
																							}
																						}
																					}),
																				{
																					ctor: '::',
																					_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Three),
																					_1: {ctor: '[]'}
																				});
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_elm_community$elm_test$Test$test,
																			'one spot card and two wildcards is a run of singles',
																			function (_p66) {
																				var _p67 = _p66;
																				return A2(
																					_elm_community$elm_test$Expect$equal,
																					_user$project$Haggis_Combination$sequence(
																						{
																							ctor: '::',
																							_0: _user$project$Tests$blueTwo,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Tests$jack,
																								_1: {
																									ctor: '::',
																									_0: _user$project$Tests$king,
																									_1: {ctor: '[]'}
																								}
																							}
																						}),
																					{
																						ctor: '::',
																						_0: A2(_user$project$Haggis_Combination$RunOfSingles, 3, _user$project$Haggis_Card$Four),
																						_1: {ctor: '[]'}
																					});
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_elm_community$elm_test$Test$test,
																				'a ten and three wildcards is a run of singles and a run of pairs',
																				function (_p68) {
																					var _p69 = _p68;
																					return A2(
																						_elm_community$elm_test$Expect$equal,
																						_user$project$Haggis_Combination$sequence(
																							{
																								ctor: '::',
																								_0: _user$project$Tests$blueTen,
																								_1: {
																									ctor: '::',
																									_0: _user$project$Tests$jack,
																									_1: {
																										ctor: '::',
																										_0: _user$project$Tests$queen,
																										_1: {
																											ctor: '::',
																											_0: _user$project$Tests$king,
																											_1: {ctor: '[]'}
																										}
																									}
																								}
																							}),
																						{
																							ctor: '::',
																							_0: A2(_user$project$Haggis_Combination$RunOfSingles, 4, _user$project$Haggis_Card$King),
																							_1: {
																								ctor: '::',
																								_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Jack),
																								_1: {ctor: '[]'}
																							}
																						});
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_elm_community$elm_test$Test$test,
																					'one wildcard can fill a one rank gap between two singles to form a run',
																					function (_p70) {
																						var _p71 = _p70;
																						return A2(
																							_elm_community$elm_test$Expect$equal,
																							_user$project$Haggis_Combination$sequence(
																								{
																									ctor: '::',
																									_0: _user$project$Tests$blueTwo,
																									_1: {
																										ctor: '::',
																										_0: _user$project$Tests$blueFour,
																										_1: {
																											ctor: '::',
																											_0: _user$project$Tests$king,
																											_1: {ctor: '[]'}
																										}
																									}
																								}),
																							{
																								ctor: '::',
																								_0: A2(_user$project$Haggis_Combination$RunOfSingles, 3, _user$project$Haggis_Card$Four),
																								_1: {ctor: '[]'}
																							});
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_elm_community$elm_test$Test$test,
																						'one wildcard CANNOT fill a 2+ rank gap between two singles to form a run',
																						function (_p72) {
																							var _p73 = _p72;
																							return A2(
																								_elm_community$elm_test$Expect$equal,
																								_user$project$Haggis_Combination$sequence(
																									{
																										ctor: '::',
																										_0: _user$project$Tests$greenTwo,
																										_1: {
																											ctor: '::',
																											_0: _user$project$Tests$greenFive,
																											_1: {
																												ctor: '::',
																												_0: _user$project$Tests$king,
																												_1: {ctor: '[]'}
																											}
																										}
																									}),
																								{ctor: '[]'});
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_elm_community$elm_test$Test$test,
																							'two wildcards can fill a 2 rank gap between two singles to form a run',
																							function (_p74) {
																								var _p75 = _p74;
																								return A2(
																									_elm_community$elm_test$Expect$equal,
																									_user$project$Haggis_Combination$sequence(
																										{
																											ctor: '::',
																											_0: _user$project$Tests$greenTwo,
																											_1: {
																												ctor: '::',
																												_0: _user$project$Tests$greenFive,
																												_1: {
																													ctor: '::',
																													_0: _user$project$Tests$king,
																													_1: {
																														ctor: '::',
																														_0: _user$project$Tests$jack,
																														_1: {ctor: '[]'}
																													}
																												}
																											}
																										}),
																									{
																										ctor: '::',
																										_0: A2(_user$project$Haggis_Combination$RunOfSingles, 4, _user$project$Haggis_Card$Five),
																										_1: {ctor: '[]'}
																									});
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_elm_community$elm_test$Test$test,
																								'one spot card and three wildcards could be a run of singles or a run of pairs',
																								function (_p76) {
																									var _p77 = _p76;
																									return A2(
																										_elm_community$elm_test$Expect$equal,
																										_user$project$Haggis_Combination$sequence(
																											{
																												ctor: '::',
																												_0: _user$project$Tests$blueTwo,
																												_1: {
																													ctor: '::',
																													_0: _user$project$Tests$jack,
																													_1: {
																														ctor: '::',
																														_0: _user$project$Tests$queen,
																														_1: {
																															ctor: '::',
																															_0: _user$project$Tests$king,
																															_1: {ctor: '[]'}
																														}
																													}
																												}
																											}),
																										{
																											ctor: '::',
																											_0: A2(_user$project$Haggis_Combination$RunOfSingles, 4, _user$project$Haggis_Card$Five),
																											_1: {
																												ctor: '::',
																												_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Three),
																												_1: {ctor: '[]'}
																											}
																										});
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_elm_community$elm_test$Test$test,
																									'wild card should sub for missing card in run of pairs',
																									function (_p78) {
																										var _p79 = _p78;
																										return A2(
																											_elm_community$elm_test$Expect$equal,
																											_user$project$Haggis_Combination$sequence(
																												{
																													ctor: '::',
																													_0: _user$project$Tests$blueTwo,
																													_1: {
																														ctor: '::',
																														_0: _user$project$Tests$greenTwo,
																														_1: {
																															ctor: '::',
																															_0: _user$project$Tests$jack,
																															_1: {
																																ctor: '::',
																																_0: _user$project$Tests$greenThree,
																																_1: {ctor: '[]'}
																															}
																														}
																													}
																												}),
																											{
																												ctor: '::',
																												_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Three),
																												_1: {ctor: '[]'}
																											});
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_elm_community$elm_test$Test$test,
																										'a pair of tens plus two wilds is a run of pairs (T-T-J-J)',
																										function (_p80) {
																											var _p81 = _p80;
																											return A2(
																												_elm_community$elm_test$Expect$equal,
																												_user$project$Haggis_Combination$sequence(
																													{
																														ctor: '::',
																														_0: _user$project$Tests$blueTen,
																														_1: {
																															ctor: '::',
																															_0: _user$project$Tests$greenTen,
																															_1: {
																																ctor: '::',
																																_0: _user$project$Tests$jack,
																																_1: {
																																	ctor: '::',
																																	_0: _user$project$Tests$king,
																																	_1: {ctor: '[]'}
																																}
																															}
																														}
																													}),
																												{
																													ctor: '::',
																													_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Jack),
																													_1: {ctor: '[]'}
																												});
																										}),
																									_1: {
																										ctor: '::',
																										_0: A2(
																											_elm_community$elm_test$Test$test,
																											'can fill holes in run of pairs that is longer than 3 ranks',
																											function (_p82) {
																												var _p83 = _p82;
																												return A2(
																													_elm_community$elm_test$Expect$equal,
																													_user$project$Haggis_Combination$sequence(
																														{
																															ctor: '::',
																															_0: _user$project$Tests$blueTwo,
																															_1: {
																																ctor: '::',
																																_0: _user$project$Tests$greenTwo,
																																_1: {
																																	ctor: '::',
																																	_0: _user$project$Tests$blueThree,
																																	_1: {
																																		ctor: '::',
																																		_0: _user$project$Tests$greenThree,
																																		_1: {
																																			ctor: '::',
																																			_0: _user$project$Tests$blueFour,
																																			_1: {
																																				ctor: '::',
																																				_0: _user$project$Tests$greenFive,
																																				_1: {
																																					ctor: '::',
																																					_0: _user$project$Tests$jack,
																																					_1: {
																																						ctor: '::',
																																						_0: _user$project$Tests$king,
																																						_1: {ctor: '[]'}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}),
																													{
																														ctor: '::',
																														_0: A2(_user$project$Haggis_Combination$RunOfPairs, 4, _user$project$Haggis_Card$Five),
																														_1: {ctor: '[]'}
																													});
																											}),
																										_1: {
																											ctor: '::',
																											_0: A2(
																												_elm_community$elm_test$Test$test,
																												'a consecutive single and a pair of spot cards, plus 3 wildcards, could be a run of pairs or a run of triples',
																												function (_p84) {
																													var _p85 = _p84;
																													return A2(
																														_elm_community$elm_test$Expect$equal,
																														_user$project$Haggis_Combination$sequence(
																															{
																																ctor: '::',
																																_0: _user$project$Tests$blueTwo,
																																_1: {
																																	ctor: '::',
																																	_0: _user$project$Tests$greenTwo,
																																	_1: {
																																		ctor: '::',
																																		_0: _user$project$Tests$greenThree,
																																		_1: {
																																			ctor: '::',
																																			_0: _user$project$Tests$jack,
																																			_1: {
																																				ctor: '::',
																																				_0: _user$project$Tests$queen,
																																				_1: {
																																					ctor: '::',
																																					_0: _user$project$Tests$king,
																																					_1: {ctor: '[]'}
																																				}
																																			}
																																		}
																																	}
																																}
																															}),
																														{
																															ctor: '::',
																															_0: A2(_user$project$Haggis_Combination$RunOfPairs, 3, _user$project$Haggis_Card$Four),
																															_1: {
																																ctor: '::',
																																_0: A2(_user$project$Haggis_Combination$RunOfTriples, 2, _user$project$Haggis_Card$Three),
																																_1: {ctor: '[]'}
																															}
																														});
																												}),
																											_1: {
																												ctor: '::',
																												_0: A2(
																													_elm_community$elm_test$Test$test,
																													'two consecutive pairs and two wildcards could be a run of pairs or a run of triples',
																													function (_p86) {
																														var _p87 = _p86;
																														return A2(
																															_elm_community$elm_test$Expect$equal,
																															_user$project$Haggis_Combination$sequence(
																																{
																																	ctor: '::',
																																	_0: _user$project$Tests$blueTwo,
																																	_1: {
																																		ctor: '::',
																																		_0: _user$project$Tests$blueThree,
																																		_1: {
																																			ctor: '::',
																																			_0: _user$project$Tests$greenTwo,
																																			_1: {
																																				ctor: '::',
																																				_0: _user$project$Tests$greenThree,
																																				_1: {
																																					ctor: '::',
																																					_0: _user$project$Tests$jack,
																																					_1: {
																																						ctor: '::',
																																						_0: _user$project$Tests$queen,
																																						_1: {ctor: '[]'}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}),
																															{
																																ctor: '::',
																																_0: A2(_user$project$Haggis_Combination$RunOfPairs, 3, _user$project$Haggis_Card$Four),
																																_1: {
																																	ctor: '::',
																																	_0: A2(_user$project$Haggis_Combination$RunOfTriples, 2, _user$project$Haggis_Card$Three),
																																	_1: {ctor: '[]'}
																																}
																															});
																													}),
																												_1: {ctor: '[]'}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_community$elm_test$Test$describe,
									'Haggis.Cards.subsets',
									{
										ctor: '::',
										_0: A2(
											_elm_community$elm_test$Test$test,
											'the subsets of no cards is a set containing the set with no cards',
											function (_p88) {
												var _p89 = _p88;
												return A2(
													_elm_community$elm_test$Expect$equal,
													_user$project$Haggis_Cards$subsets(
														{ctor: '[]'}),
													{
														ctor: '::',
														_0: {ctor: '[]'},
														_1: {ctor: '[]'}
													});
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_community$elm_test$Test$test,
												'the subsets of one card is a set with no cards and a set with the one card',
												function (_p90) {
													var _p91 = _p90;
													return A2(
														_elm_community$elm_test$Expect$equal,
														_user$project$Haggis_Cards$subsets(
															{
																ctor: '::',
																_0: _user$project$Tests$blueTwo,
																_1: {ctor: '[]'}
															}),
														{
															ctor: '::',
															_0: {
																ctor: '::',
																_0: _user$project$Tests$blueTwo,
																_1: {ctor: '[]'}
															},
															_1: {
																ctor: '::',
																_0: {ctor: '[]'},
																_1: {ctor: '[]'}
															}
														});
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_community$elm_test$Test$test,
													'the subsets of a pair is the pair, each of its singles, and the empty set',
													function (_p92) {
														var _p93 = _p92;
														return A2(
															_elm_community$elm_test$Expect$equal,
															_user$project$Haggis_Cards$subsets(
																{
																	ctor: '::',
																	_0: _user$project$Tests$blueTwo,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$greenTwo,
																		_1: {ctor: '[]'}
																	}
																}),
															{
																ctor: '::',
																_0: {
																	ctor: '::',
																	_0: _user$project$Tests$blueTwo,
																	_1: {
																		ctor: '::',
																		_0: _user$project$Tests$greenTwo,
																		_1: {ctor: '[]'}
																	}
																},
																_1: {
																	ctor: '::',
																	_0: {
																		ctor: '::',
																		_0: _user$project$Tests$blueTwo,
																		_1: {ctor: '[]'}
																	},
																	_1: {
																		ctor: '::',
																		_0: {
																			ctor: '::',
																			_0: _user$project$Tests$greenTwo,
																			_1: {ctor: '[]'}
																		},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '[]'},
																			_1: {ctor: '[]'}
																		}
																	}
																}
															});
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_elm_community$elm_test$Test$test,
														'the subsets of 3 cards is the 3 cards, each card paired, each card alone, and the empty set',
														function (_p94) {
															var _p95 = _p94;
															return A2(
																_elm_community$elm_test$Expect$equal,
																_user$project$Haggis_Cards$subsets(
																	{
																		ctor: '::',
																		_0: _user$project$Tests$greenTwo,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$greenThree,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$jack,
																				_1: {ctor: '[]'}
																			}
																		}
																	}),
																{
																	ctor: '::',
																	_0: {
																		ctor: '::',
																		_0: _user$project$Tests$greenTwo,
																		_1: {
																			ctor: '::',
																			_0: _user$project$Tests$greenThree,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$jack,
																				_1: {ctor: '[]'}
																			}
																		}
																	},
																	_1: {
																		ctor: '::',
																		_0: {
																			ctor: '::',
																			_0: _user$project$Tests$greenTwo,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$greenThree,
																				_1: {ctor: '[]'}
																			}
																		},
																		_1: {
																			ctor: '::',
																			_0: {
																				ctor: '::',
																				_0: _user$project$Tests$greenTwo,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$jack,
																					_1: {ctor: '[]'}
																				}
																			},
																			_1: {
																				ctor: '::',
																				_0: {
																					ctor: '::',
																					_0: _user$project$Tests$greenTwo,
																					_1: {ctor: '[]'}
																				},
																				_1: {
																					ctor: '::',
																					_0: {
																						ctor: '::',
																						_0: _user$project$Tests$greenThree,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$jack,
																							_1: {ctor: '[]'}
																						}
																					},
																					_1: {
																						ctor: '::',
																						_0: {
																							ctor: '::',
																							_0: _user$project$Tests$greenThree,
																							_1: {ctor: '[]'}
																						},
																						_1: {
																							ctor: '::',
																							_0: {
																								ctor: '::',
																								_0: _user$project$Tests$jack,
																								_1: {ctor: '[]'}
																							},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '[]'},
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																});
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_elm_community$elm_test$Test$test,
															'can find all sequences contained in a set of cards',
															function (_p96) {
																var _p97 = _p96;
																return A2(
																	_elm_community$elm_test$Expect$equal,
																	_user$project$Haggis_Hand$collectSequences(
																		{
																			ctor: '::',
																			_0: _user$project$Tests$greenTwo,
																			_1: {
																				ctor: '::',
																				_0: _user$project$Tests$greenThree,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$jack,
																					_1: {ctor: '[]'}
																				}
																			}
																		}),
																	{
																		ctor: '::',
																		_0: A2(_user$project$Haggis_Combination$RunOfSingles, 3, _user$project$Haggis_Card$Four),
																		_1: {ctor: '[]'}
																	});
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_elm_community$elm_test$Test$test,
																'can find all sets contained in a set of cards',
																function (_p98) {
																	var _p99 = _p98;
																	return A2(
																		_elm_community$elm_test$Expect$equal,
																		_user$project$Haggis_Hand$collectSets(
																			{
																				ctor: '::',
																				_0: _user$project$Tests$greenTwo,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Tests$greenThree,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$jack,
																						_1: {ctor: '[]'}
																					}
																				}
																			}),
																		{
																			ctor: '::',
																			_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Two),
																			_1: {
																				ctor: '::',
																				_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Two),
																				_1: {
																					ctor: '::',
																					_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Three),
																					_1: {
																						ctor: '::',
																						_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Three),
																						_1: {
																							ctor: '::',
																							_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Jack),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		});
																}),
															_1: {
																ctor: '::',
																_0: A2(
																	_elm_community$elm_test$Test$test,
																	'can find all bombs contained in a set of cards',
																	function (_p100) {
																		var _p101 = _p100;
																		return A2(
																			_elm_community$elm_test$Expect$equal,
																			_user$project$Haggis_Hand$collectBombs(
																				{
																					ctor: '::',
																					_0: _user$project$Tests$redThree,
																					_1: {
																						ctor: '::',
																						_0: _user$project$Tests$greenThree,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Tests$redFive,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Tests$redSeven,
																								_1: {
																									ctor: '::',
																									_0: _user$project$Tests$orangeSeven,
																									_1: {
																										ctor: '::',
																										_0: _user$project$Tests$redNine,
																										_1: {
																											ctor: '::',
																											_0: _user$project$Tests$yellowNine,
																											_1: {ctor: '[]'}
																										}
																									}
																								}
																							}
																						}
																					}
																				}),
																			{
																				ctor: '::',
																				_0: _user$project$Haggis_Combination$Suited,
																				_1: {
																					ctor: '::',
																					_0: _user$project$Haggis_Combination$Rainbow,
																					_1: {ctor: '[]'}
																				}
																			});
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_elm_community$elm_test$Test$test,
																		'can find all sets contained in a full hand of cards',
																		function (_p102) {
																			var _p103 = _p102;
																			return A2(
																				_elm_community$elm_test$Expect$equal,
																				_user$project$Haggis_Hand$collectSets(_user$project$Tests$hand),
																				{
																					ctor: '::',
																					_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Two),
																					_1: {
																						ctor: '::',
																						_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Two),
																						_1: {
																							ctor: '::',
																							_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Two),
																							_1: {
																								ctor: '::',
																								_0: _user$project$Haggis_Combination$Triple(_user$project$Haggis_Card$Three),
																								_1: {
																									ctor: '::',
																									_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Three),
																									_1: {
																										ctor: '::',
																										_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Three),
																										_1: {
																											ctor: '::',
																											_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Three),
																											_1: {
																												ctor: '::',
																												_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Three),
																												_1: {
																													ctor: '::',
																													_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Three),
																													_1: {
																														ctor: '::',
																														_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Three),
																														_1: {
																															ctor: '::',
																															_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Four),
																															_1: {
																																ctor: '::',
																																_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Five),
																																_1: {
																																	ctor: '::',
																																	_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Five),
																																	_1: {
																																		ctor: '::',
																																		_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Five),
																																		_1: {
																																			ctor: '::',
																																			_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Seven),
																																			_1: {
																																				ctor: '::',
																																				_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Seven),
																																				_1: {
																																					ctor: '::',
																																					_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Seven),
																																					_1: {
																																						ctor: '::',
																																						_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Nine),
																																						_1: {
																																							ctor: '::',
																																							_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Nine),
																																							_1: {
																																								ctor: '::',
																																								_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Nine),
																																								_1: {
																																									ctor: '::',
																																									_0: _user$project$Haggis_Combination$Pair(_user$project$Haggis_Card$Ten),
																																									_1: {
																																										ctor: '::',
																																										_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Ten),
																																										_1: {
																																											ctor: '::',
																																											_0: _user$project$Haggis_Combination$Single(_user$project$Haggis_Card$Ten),
																																											_1: {ctor: '[]'}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				});
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_elm_community$elm_test$Test$test,
																			'can find all bombs contained in a full hand of cards',
																			function (_p104) {
																				var _p105 = _p104;
																				return A2(
																					_elm_community$elm_test$Expect$equal,
																					_user$project$Haggis_Hand$collectBombs(_user$project$Tests$hand),
																					{
																						ctor: '::',
																						_0: _user$project$Haggis_Combination$Rainbow,
																						_1: {
																							ctor: '::',
																							_0: _user$project$Haggis_Combination$Rainbow,
																							_1: {
																								ctor: '::',
																								_0: _user$project$Haggis_Combination$Rainbow,
																								_1: {
																									ctor: '::',
																									_0: _user$project$Haggis_Combination$Rainbow,
																									_1: {
																										ctor: '::',
																										_0: _user$project$Haggis_Combination$Suited,
																										_1: {
																											ctor: '::',
																											_0: _user$project$Haggis_Combination$Rainbow,
																											_1: {
																												ctor: '::',
																												_0: _user$project$Haggis_Combination$Rainbow,
																												_1: {ctor: '[]'}
																											}
																										}
																									}
																								}
																							}
																						}
																					});
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_elm_community$elm_test$Test$test,
																				'can find all sequences contained in a full hand of cards',
																				function (_p106) {
																					var _p107 = _p106;
																					return A2(
																						_elm_community$elm_test$Expect$equal,
																						_user$project$Haggis_Hand$collectSequences(_user$project$Tests$hand),
																						{
																							ctor: '::',
																							_0: A2(_user$project$Haggis_Combination$RunOfPairs, 2, _user$project$Haggis_Card$Three),
																							_1: {
																								ctor: '::',
																								_0: A2(_user$project$Haggis_Combination$RunOfSingles, 3, _user$project$Haggis_Card$Four),
																								_1: {ctor: '[]'}
																							}
																						});
																				}),
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		}
	});

var _user$project$Test_Runner_Node_Vendor_Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _p0 = {
				ctor: '_Tuple2',
				_0: getA(nextX),
				_1: getB(nextY)
			};
			_v0_2:
			do {
				if (_p0.ctor === '_Tuple2') {
					if (_p0._0.ctor === 'Just') {
						if (_p0._1.ctor === 'Just') {
							if (_elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0)) {
								var _v1 = getA,
									_v2 = getB,
									_v3 = nextX + 1,
									_v4 = nextY + 1,
									_v5 = {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: nextX, _1: nextY},
									_1: path
								};
								getA = _v1;
								getB = _v2;
								nextX = _v3;
								nextY = _v4;
								path = _v5;
								continue snake;
							} else {
								return {ctor: '_Tuple2', _0: path, _1: false};
							}
						} else {
							break _v0_2;
						}
					} else {
						if (_p0._1.ctor === 'Nothing') {
							return {ctor: '_Tuple2', _0: path, _1: true};
						} else {
							break _v0_2;
						}
					}
				} else {
					break _v0_2;
				}
			} while(false);
			return {ctor: '_Tuple2', _0: path, _1: false};
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$NoChange = function (a) {
	return {ctor: 'NoChange', _0: a};
};
var _user$project$Test_Runner_Node_Vendor_Diff$Removed = function (a) {
	return {ctor: 'Removed', _0: a};
};
var _user$project$Test_Runner_Node_Vendor_Diff$Added = function (a) {
	return {ctor: 'Added', _0: a};
};
var _user$project$Test_Runner_Node_Vendor_Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _p1, path) {
		makeChangesHelp:
		while (true) {
			var _p2 = _p1;
			var _p7 = _p2._1;
			var _p6 = _p2._0;
			var _p3 = path;
			if (_p3.ctor === '[]') {
				return changes;
			} else {
				var _p5 = _p3._0._1;
				var _p4 = _p3._0._0;
				var change = (_elm_lang$core$Native_Utils.eq(_p6 - 1, _p4) && _elm_lang$core$Native_Utils.eq(_p7 - 1, _p5)) ? _user$project$Test_Runner_Node_Vendor_Diff$NoChange(
					getA(_p6)) : (_elm_lang$core$Native_Utils.eq(_p6, _p4) ? _user$project$Test_Runner_Node_Vendor_Diff$Added(
					getB(_p7)) : (_elm_lang$core$Native_Utils.eq(_p7, _p5) ? _user$project$Test_Runner_Node_Vendor_Diff$Removed(
					getA(_p6)) : _elm_lang$core$Native_Utils.crash(
					'Test.Runner.Node.Vendor.Diff',
					{
						start: {line: 169, column: 25},
						end: {line: 169, column: 36}
					})(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'Unexpected path: ',
						_elm_lang$core$Basics$toString(
							{
								ctor: '_Tuple2',
								_0: {ctor: '_Tuple2', _0: _p6, _1: _p7},
								_1: path
							})))));
				var _v8 = {ctor: '::', _0: change, _1: changes},
					_v9 = getA,
					_v10 = getB,
					_v11 = {ctor: '_Tuple2', _0: _p4, _1: _p5},
					_v12 = _p3._1;
				changes = _v8;
				getA = _v9;
				getB = _v10;
				_p1 = _v11;
				path = _v12;
				continue makeChangesHelp;
			}
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$makeChanges = F3(
	function (getA, getB, path) {
		var _p8 = path;
		if (_p8.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A5(
				_user$project$Test_Runner_Node_Vendor_Diff$makeChangesHelp,
				{ctor: '[]'},
				getA,
				getB,
				_p8._0,
				_p8._1);
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$Found = function (a) {
	return {ctor: 'Found', _0: a};
};
var _user$project$Test_Runner_Node_Vendor_Diff$Continue = function (a) {
	return {ctor: 'Continue', _0: a};
};
var _user$project$Test_Runner_Node_Vendor_Diff$step = F4(
	function (snake, offset, k, v) {
		var fromTop = A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			A2(_elm_lang$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			A2(_elm_lang$core$Array$get, (k - 1) + offset, v));
		var _p9 = function () {
			var _p10 = {ctor: '_Tuple2', _0: fromLeft, _1: fromTop};
			if (_p10._0.ctor === '[]') {
				if (_p10._1.ctor === '[]') {
					return {
						ctor: '_Tuple2',
						_0: {ctor: '[]'},
						_1: {ctor: '_Tuple2', _0: 0, _1: 0}
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: fromTop,
						_1: {ctor: '_Tuple2', _0: _p10._1._0._0 + 1, _1: _p10._1._0._1}
					};
				}
			} else {
				if (_p10._1.ctor === '[]') {
					return {
						ctor: '_Tuple2',
						_0: fromLeft,
						_1: {ctor: '_Tuple2', _0: _p10._0._0._0, _1: _p10._0._0._1 + 1}
					};
				} else {
					var _p12 = _p10._1._0._1;
					var _p11 = _p10._0._0._1;
					return (_elm_lang$core$Native_Utils.cmp(_p11 + 1, _p12) > -1) ? {
						ctor: '_Tuple2',
						_0: fromLeft,
						_1: {ctor: '_Tuple2', _0: _p10._0._0._0, _1: _p11 + 1}
					} : {
						ctor: '_Tuple2',
						_0: fromTop,
						_1: {ctor: '_Tuple2', _0: _p10._1._0._0 + 1, _1: _p12}
					};
				}
			}
		}();
		var path = _p9._0;
		var x = _p9._1._0;
		var y = _p9._1._1;
		var _p13 = A3(
			snake,
			x + 1,
			y + 1,
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: x, _1: y},
				_1: path
			});
		var newPath = _p13._0;
		var goal = _p13._1;
		return goal ? _user$project$Test_Runner_Node_Vendor_Diff$Found(newPath) : _user$project$Test_Runner_Node_Vendor_Diff$Continue(
			A3(_elm_lang$core$Array$set, k + offset, newPath, v));
	});
var _user$project$Test_Runner_Node_Vendor_Diff$ondLoopDK = F5(
	function (snake, offset, d, k, v) {
		ondLoopDK:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(k, d) > 0) {
				var _v15 = snake,
					_v16 = offset,
					_v17 = d + 1,
					_v18 = (0 - d) - 1,
					_v19 = v;
				snake = _v15;
				offset = _v16;
				d = _v17;
				k = _v18;
				v = _v19;
				continue ondLoopDK;
			} else {
				var _p14 = A4(_user$project$Test_Runner_Node_Vendor_Diff$step, snake, offset, k, v);
				if (_p14.ctor === 'Found') {
					return _p14._0;
				} else {
					var _v21 = snake,
						_v22 = offset,
						_v23 = d,
						_v24 = k + 2,
						_v25 = _p14._0;
					snake = _v21;
					offset = _v22;
					d = _v23;
					k = _v24;
					v = _v25;
					continue ondLoopDK;
				}
			}
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$ond = F4(
	function (getA, getB, m, n) {
		var v = A2(
			_elm_lang$core$Array$initialize,
			(m + n) + 1,
			_elm_lang$core$Basics$always(
				{ctor: '[]'}));
		return A5(
			_user$project$Test_Runner_Node_Vendor_Diff$ondLoopDK,
			A2(_user$project$Test_Runner_Node_Vendor_Diff$snake, getA, getB),
			m,
			0,
			0,
			v);
	});
var _user$project$Test_Runner_Node_Vendor_Diff$onpLoopK = F4(
	function (snake, offset, ks, v) {
		onpLoopK:
		while (true) {
			var _p15 = ks;
			if (_p15.ctor === '[]') {
				return _user$project$Test_Runner_Node_Vendor_Diff$Continue(v);
			} else {
				var _p16 = A4(_user$project$Test_Runner_Node_Vendor_Diff$step, snake, offset, _p15._0, v);
				if (_p16.ctor === 'Found') {
					return _user$project$Test_Runner_Node_Vendor_Diff$Found(_p16._0);
				} else {
					var _v28 = snake,
						_v29 = offset,
						_v30 = _p15._1,
						_v31 = _p16._0;
					snake = _v28;
					offset = _v29;
					ks = _v30;
					v = _v31;
					continue onpLoopK;
				}
			}
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$onpLoopP = F5(
	function (snake, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (_elm_lang$core$Native_Utils.cmp(delta, 0) > 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$List$reverse(
					A2(_elm_lang$core$List$range, delta + 1, delta + p)),
				A2(_elm_lang$core$List$range, 0 - p, delta)) : A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$List$reverse(
					A2(_elm_lang$core$List$range, delta + 1, p)),
				A2(_elm_lang$core$List$range, (0 - p) + delta, delta));
			var _p17 = A4(_user$project$Test_Runner_Node_Vendor_Diff$onpLoopK, snake, offset, ks, v);
			if (_p17.ctor === 'Found') {
				return _p17._0;
			} else {
				var _v33 = snake,
					_v34 = delta,
					_v35 = offset,
					_v36 = p + 1,
					_v37 = _p17._0;
				snake = _v33;
				delta = _v34;
				offset = _v35;
				p = _v36;
				v = _v37;
				continue onpLoopP;
			}
		}
	});
var _user$project$Test_Runner_Node_Vendor_Diff$onp = F4(
	function (getA, getB, m, n) {
		var delta = n - m;
		var v = A2(
			_elm_lang$core$Array$initialize,
			(m + n) + 1,
			_elm_lang$core$Basics$always(
				{ctor: '[]'}));
		return A5(
			_user$project$Test_Runner_Node_Vendor_Diff$onpLoopP,
			A2(_user$project$Test_Runner_Node_Vendor_Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var _user$project$Test_Runner_Node_Vendor_Diff$diff = F2(
	function (a, b) {
		var arrB = _elm_lang$core$Array$fromList(b);
		var n = _elm_lang$core$Array$length(arrB);
		var getB = function (y) {
			return A2(_elm_lang$core$Array$get, y - 1, arrB);
		};
		var getBOrCrash = function (y) {
			var _p18 = getB(y);
			if (_p18.ctor === 'Just') {
				return _p18._0;
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Test.Runner.Node.Vendor.Diff',
					{
						start: {line: 128, column: 13},
						end: {line: 133, column: 71}
					},
					_p18)(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'Cannot get B[',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(y),
							']')));
			}
		};
		var arrA = _elm_lang$core$Array$fromList(a);
		var m = _elm_lang$core$Array$length(arrA);
		var getA = function (x) {
			return A2(_elm_lang$core$Array$get, x - 1, arrA);
		};
		var getAOrCrash = function (x) {
			var _p20 = getA(x);
			if (_p20.ctor === 'Just') {
				return _p20._0;
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Test.Runner.Node.Vendor.Diff',
					{
						start: {line: 120, column: 13},
						end: {line: 125, column: 71}
					},
					_p20)(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'Cannot get A[',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(x),
							']')));
			}
		};
		var path = A4(_user$project$Test_Runner_Node_Vendor_Diff$onp, getA, getB, m, n);
		return A3(_user$project$Test_Runner_Node_Vendor_Diff$makeChanges, getAOrCrash, getBOrCrash, path);
	});
var _user$project$Test_Runner_Node_Vendor_Diff$diffLines = F2(
	function (a, b) {
		return A2(
			_user$project$Test_Runner_Node_Vendor_Diff$diff,
			_elm_lang$core$String$lines(a),
			_elm_lang$core$String$lines(b));
	});

var _user$project$Test_Reporter_Highlightable$resolve = F2(
	function (_p0, highlightable) {
		var _p1 = _p0;
		var _p2 = highlightable;
		if (_p2.ctor === 'Highlighted') {
			return _p1.fromHighlighted(_p2._0);
		} else {
			return _p1.fromPlain(_p2._0);
		}
	});
var _user$project$Test_Reporter_Highlightable$Plain = function (a) {
	return {ctor: 'Plain', _0: a};
};
var _user$project$Test_Reporter_Highlightable$Highlighted = function (a) {
	return {ctor: 'Highlighted', _0: a};
};
var _user$project$Test_Reporter_Highlightable$map = F2(
	function (transform, highlightable) {
		var _p3 = highlightable;
		if (_p3.ctor === 'Highlighted') {
			return _user$project$Test_Reporter_Highlightable$Highlighted(
				transform(_p3._0));
		} else {
			return _user$project$Test_Reporter_Highlightable$Plain(
				transform(_p3._0));
		}
	});
var _user$project$Test_Reporter_Highlightable$fromDiff = function (diff) {
	var _p4 = diff;
	switch (_p4.ctor) {
		case 'Added':
			return {ctor: '[]'};
		case 'Removed':
			return {
				ctor: '::',
				_0: _user$project$Test_Reporter_Highlightable$Highlighted(_p4._0),
				_1: {ctor: '[]'}
			};
		default:
			return {
				ctor: '::',
				_0: _user$project$Test_Reporter_Highlightable$Plain(_p4._0),
				_1: {ctor: '[]'}
			};
	}
};
var _user$project$Test_Reporter_Highlightable$diffLists = F2(
	function (expected, actual) {
		return A2(
			_elm_lang$core$List$concatMap,
			_user$project$Test_Reporter_Highlightable$fromDiff,
			A2(_user$project$Test_Runner_Node_Vendor_Diff$diff, expected, actual));
	});

var _user$project$Test_Reporter_Console_Format$verticalBar = F3(
	function (comparison, expected, actual) {
		return A2(
			_elm_lang$core$String$join,
			'\n',
			{
				ctor: '::',
				_0: actual,
				_1: {
					ctor: '::',
					_0: '╷',
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Basics_ops['++'], '│ ', comparison),
						_1: {
							ctor: '::',
							_0: '╵',
							_1: {
								ctor: '::',
								_0: expected,
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
	});
var _user$project$Test_Reporter_Console_Format$listDiffToString = F4(
	function (index, description, _p0, originals) {
		listDiffToString:
		while (true) {
			var _p1 = _p0;
			var _p2 = {ctor: '_Tuple2', _0: _p1.expected, _1: _p1.actual};
			if (_p2._0.ctor === '[]') {
				if (_p2._1.ctor === '[]') {
					return A2(
						_elm_lang$core$String$join,
						'',
						{
							ctor: '::',
							_0: 'Two lists were unequal previously, yet ended up equal later.',
							_1: {
								ctor: '::',
								_0: 'This should never happen!',
								_1: {
									ctor: '::',
									_0: 'Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: ',
									_1: {
										ctor: '::',
										_0: '\n',
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Basics$toString(originals.originalExpected),
											_1: {
												ctor: '::',
												_0: '\n',
												_1: {
													ctor: '::',
													_0: _elm_lang$core$Basics$toString(originals.originalActual),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						});
				} else {
					return A3(
						_user$project$Test_Reporter_Console_Format$verticalBar,
						A2(_elm_lang$core$Basics_ops['++'], description, ' was longer than'),
						_elm_lang$core$Basics$toString(originals.originalExpected),
						_elm_lang$core$Basics$toString(originals.originalActual));
				}
			} else {
				if (_p2._1.ctor === '[]') {
					return A3(
						_user$project$Test_Reporter_Console_Format$verticalBar,
						A2(_elm_lang$core$Basics_ops['++'], description, ' was shorter than'),
						_elm_lang$core$Basics$toString(originals.originalExpected),
						_elm_lang$core$Basics$toString(originals.originalActual));
				} else {
					var _p4 = _p2._0._0;
					var _p3 = _p2._1._0;
					if (_elm_lang$core$Native_Utils.eq(_p4, _p3)) {
						var _v2 = index + 1,
							_v3 = description,
							_v4 = {expected: _p2._0._1, actual: _p2._1._1},
							_v5 = originals;
						index = _v2;
						description = _v3;
						_p0 = _v4;
						originals = _v5;
						continue listDiffToString;
					} else {
						return A2(
							_elm_lang$core$String$join,
							'',
							{
								ctor: '::',
								_0: A3(
									_user$project$Test_Reporter_Console_Format$verticalBar,
									description,
									_elm_lang$core$Basics$toString(originals.originalExpected),
									_elm_lang$core$Basics$toString(originals.originalActual)),
								_1: {
									ctor: '::',
									_0: '\n\nThe first diff is at index ',
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Basics$toString(index),
										_1: {
											ctor: '::',
											_0: ': it was `',
											_1: {
												ctor: '::',
												_0: _p3,
												_1: {
													ctor: '::',
													_0: '`, but `',
													_1: {
														ctor: '::',
														_0: _p4,
														_1: {
															ctor: '::',
															_0: '` was expected.',
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							});
					}
				}
			}
		}
	});
var _user$project$Test_Reporter_Console_Format$isFloat = function (str) {
	var _p5 = _elm_lang$core$String$toFloat(str);
	if (_p5.ctor === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var _user$project$Test_Reporter_Console_Format$highlightEqual = F2(
	function (expected, actual) {
		if (_user$project$Test_Reporter_Console_Format$isFloat(expected) && _user$project$Test_Reporter_Console_Format$isFloat(actual)) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var actualChars = _elm_lang$core$String$toList(actual);
			var expectedChars = _elm_lang$core$String$toList(expected);
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '_Tuple2',
					_0: A2(
						_elm_lang$core$List$map,
						_user$project$Test_Reporter_Highlightable$map(_elm_lang$core$String$fromChar),
						A2(_user$project$Test_Reporter_Highlightable$diffLists, expectedChars, actualChars)),
					_1: A2(
						_elm_lang$core$List$map,
						_user$project$Test_Reporter_Highlightable$map(_elm_lang$core$String$fromChar),
						A2(_user$project$Test_Reporter_Highlightable$diffLists, actualChars, expectedChars))
				});
		}
	});
var _user$project$Test_Reporter_Console_Format$format = F3(
	function (formatEquality, description, reason) {
		var _p6 = reason;
		switch (_p6.ctor) {
			case 'Custom':
				return description;
			case 'Equality':
				var _p10 = _p6._0;
				var _p9 = _p6._1;
				var _p7 = A2(_user$project$Test_Reporter_Console_Format$highlightEqual, _p10, _p9);
				if (_p7.ctor === 'Nothing') {
					return A3(_user$project$Test_Reporter_Console_Format$verticalBar, description, _p10, _p9);
				} else {
					var _p8 = A2(formatEquality, _p7._0._0, _p7._0._1);
					var formattedExpected = _p8._0;
					var formattedActual = _p8._1;
					return A3(_user$project$Test_Reporter_Console_Format$verticalBar, description, formattedExpected, formattedActual);
				}
			case 'Comparison':
				return A3(_user$project$Test_Reporter_Console_Format$verticalBar, description, _p6._0, _p6._1);
			case 'TODO':
				return description;
			case 'Invalid':
				if (_p6._0.ctor === 'BadDescription') {
					return _elm_lang$core$Native_Utils.eq(description, '') ? 'The empty string is not a valid test description.' : A2(_elm_lang$core$Basics_ops['++'], 'This is an invalid test description: ', description);
				} else {
					return description;
				}
			case 'ListDiff':
				var _p12 = _p6._0;
				var _p11 = _p6._1;
				return A4(
					_user$project$Test_Reporter_Console_Format$listDiffToString,
					0,
					description,
					{expected: _p12, actual: _p11},
					{originalExpected: _p12, originalActual: _p11});
			default:
				var _p14 = _p6._0.missing;
				var _p13 = _p6._0.extra;
				var missingStr = _elm_lang$core$List$isEmpty(_p14) ? '' : A2(
					_elm_lang$core$Basics_ops['++'],
					'\nThese keys are missing: ',
					function (d) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'[ ',
							A2(_elm_lang$core$Basics_ops['++'], d, ' ]'));
					}(
						A2(_elm_lang$core$String$join, ', ', _p14)));
				var extraStr = _elm_lang$core$List$isEmpty(_p13) ? '' : A2(
					_elm_lang$core$Basics_ops['++'],
					'\nThese keys are extra: ',
					function (d) {
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'[ ',
							A2(_elm_lang$core$Basics_ops['++'], d, ' ]'));
					}(
						A2(_elm_lang$core$String$join, ', ', _p13)));
				return A2(
					_elm_lang$core$String$join,
					'',
					{
						ctor: '::',
						_0: A3(_user$project$Test_Reporter_Console_Format$verticalBar, description, _p6._0.expected, _p6._0.actual),
						_1: {
							ctor: '::',
							_0: '\n',
							_1: {
								ctor: '::',
								_0: extraStr,
								_1: {
									ctor: '::',
									_0: missingStr,
									_1: {ctor: '[]'}
								}
							}
						}
					});
		}
	});

var _user$project$Test_Reporter_Console_Format_Color$fromHighlightable = _user$project$Test_Reporter_Highlightable$resolve(
	{fromHighlighted: _user$project$Test_Runner_Node_Vendor_Console$bgCyan, fromPlain: _elm_lang$core$Basics$identity});
var _user$project$Test_Reporter_Console_Format_Color$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var formattedActual = A2(
			_elm_lang$core$String$join,
			'',
			A2(_elm_lang$core$List$map, _user$project$Test_Reporter_Console_Format_Color$fromHighlightable, highlightedActual));
		var formattedExpected = A2(
			_elm_lang$core$String$join,
			'',
			A2(_elm_lang$core$List$map, _user$project$Test_Reporter_Console_Format_Color$fromHighlightable, highlightedExpected));
		return {ctor: '_Tuple2', _0: formattedExpected, _1: formattedActual};
	});

var _user$project$Test_Reporter_Console_Format_Monochrome$fromHighlightable = function (indicator) {
	return _user$project$Test_Reporter_Highlightable$resolve(
		{
			fromHighlighted: function ($char) {
				return {ctor: '_Tuple2', _0: $char, _1: indicator};
			},
			fromPlain: function ($char) {
				return {ctor: '_Tuple2', _0: $char, _1: ' '};
			}
		});
};
var _user$project$Test_Reporter_Console_Format_Monochrome$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var _p0 = _elm_lang$core$List$unzip(
			A2(
				_elm_lang$core$List$map,
				_user$project$Test_Reporter_Console_Format_Monochrome$fromHighlightable('▼'),
				highlightedActual));
		var formattedActual = _p0._0;
		var actualIndicators = _p0._1;
		var combinedActual = A2(
			_elm_lang$core$String$join,
			'\n',
			{
				ctor: '::',
				_0: A2(_elm_lang$core$String$join, '', actualIndicators),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$String$join, '', formattedActual),
					_1: {ctor: '[]'}
				}
			});
		var _p1 = _elm_lang$core$List$unzip(
			A2(
				_elm_lang$core$List$map,
				_user$project$Test_Reporter_Console_Format_Monochrome$fromHighlightable('▲'),
				highlightedExpected));
		var formattedExpected = _p1._0;
		var expectedIndicators = _p1._1;
		var combinedExpected = A2(
			_elm_lang$core$String$join,
			'\n',
			{
				ctor: '::',
				_0: A2(_elm_lang$core$String$join, '', formattedExpected),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$String$join, '', expectedIndicators),
					_1: {ctor: '[]'}
				}
			});
		return {ctor: '_Tuple2', _0: combinedExpected, _1: combinedActual};
	});

var _user$project$Test_Reporter_TestResults$outcomesFromExpectationsHelp = F2(
	function (expectation, builder) {
		var _p0 = _elm_community$elm_test$Test_Runner$getFailureReason(expectation);
		if (_p0.ctor === 'Just') {
			var _p1 = _p0._0;
			return _elm_community$elm_test$Test_Runner$isTodo(expectation) ? _elm_lang$core$Native_Utils.update(
				builder,
				{
					todos: {ctor: '::', _0: _p1.description, _1: builder.todos}
				}) : _elm_lang$core$Native_Utils.update(
				builder,
				{
					failures: {ctor: '::', _0: _p1, _1: builder.failures}
				});
		} else {
			return _elm_lang$core$Native_Utils.update(
				builder,
				{passes: builder.passes + 1});
		}
	});
var _user$project$Test_Reporter_TestResults$isFailure = function (outcome) {
	var _p2 = outcome;
	if (_p2.ctor === 'Failed') {
		return true;
	} else {
		return false;
	}
};
var _user$project$Test_Reporter_TestResults$isTodo = function (outcome) {
	var _p3 = outcome;
	if (_p3.ctor === 'Todo') {
		return true;
	} else {
		return false;
	}
};
var _user$project$Test_Reporter_TestResults$TestResult = F3(
	function (a, b, c) {
		return {labels: a, outcome: b, duration: c};
	});
var _user$project$Test_Reporter_TestResults$SummaryInfo = F5(
	function (a, b, c, d, e) {
		return {testCount: a, passed: b, failed: c, todos: d, duration: e};
	});
var _user$project$Test_Reporter_TestResults$Failure = F3(
	function (a, b, c) {
		return {given: a, description: b, reason: c};
	});
var _user$project$Test_Reporter_TestResults$OutcomeBuilder = F3(
	function (a, b, c) {
		return {passes: a, todos: b, failures: c};
	});
var _user$project$Test_Reporter_TestResults$Failed = function (a) {
	return {ctor: 'Failed', _0: a};
};
var _user$project$Test_Reporter_TestResults$Todo = function (a) {
	return {ctor: 'Todo', _0: a};
};
var _user$project$Test_Reporter_TestResults$Passed = {ctor: 'Passed'};
var _user$project$Test_Reporter_TestResults$outcomesFromExpectations = function (expectations) {
	var _p4 = expectations;
	if (_p4.ctor === '::') {
		if (_p4._1.ctor === '[]') {
			var _p7 = _p4._0;
			var _p5 = _elm_community$elm_test$Test_Runner$getFailureReason(_p7);
			if (_p5.ctor === 'Nothing') {
				return {
					ctor: '::',
					_0: _user$project$Test_Reporter_TestResults$Passed,
					_1: {ctor: '[]'}
				};
			} else {
				var _p6 = _p5._0;
				return _elm_community$elm_test$Test_Runner$isTodo(_p7) ? {
					ctor: '::',
					_0: _user$project$Test_Reporter_TestResults$Todo(_p6.description),
					_1: {ctor: '[]'}
				} : {
					ctor: '::',
					_0: _user$project$Test_Reporter_TestResults$Failed(
						{
							ctor: '::',
							_0: _p6,
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			}
		} else {
			var builder = A3(
				_elm_lang$core$List$foldl,
				_user$project$Test_Reporter_TestResults$outcomesFromExpectationsHelp,
				{
					passes: 0,
					todos: {ctor: '[]'},
					failures: {ctor: '[]'}
				},
				expectations);
			var failuresList = function () {
				var _p8 = builder.failures;
				if (_p8.ctor === '[]') {
					return {ctor: '[]'};
				} else {
					return {
						ctor: '::',
						_0: _user$project$Test_Reporter_TestResults$Failed(_p8),
						_1: {ctor: '[]'}
					};
				}
			}();
			return _elm_lang$core$List$concat(
				{
					ctor: '::',
					_0: A2(_elm_lang$core$List$repeat, builder.passes, _user$project$Test_Reporter_TestResults$Passed),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$List$map, _user$project$Test_Reporter_TestResults$Todo, builder.todos),
						_1: {
							ctor: '::',
							_0: failuresList,
							_1: {ctor: '[]'}
						}
					}
				});
		}
	} else {
		return {ctor: '[]'};
	}
};

var _user$project$Test_Reporter_Console$withChar = F2(
	function (icon, str) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$String$fromChar(icon),
			A2(
				_elm_lang$core$Basics_ops['++'],
				' ',
				A2(_elm_lang$core$Basics_ops['++'], str, '\n')));
	});
var _user$project$Test_Reporter_Console$stat = F2(
	function (label, value) {
		return _user$project$Console_Text$concat(
			{
				ctor: '::',
				_0: _user$project$Console_Text$dark(
					_user$project$Console_Text$plain(label)),
				_1: {
					ctor: '::',
					_0: _user$project$Console_Text$plain(
						A2(_elm_lang$core$Basics_ops['++'], value, '\n')),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$Test_Reporter_Console$textToValue = F2(
	function (useColor, txt) {
		return _elm_lang$core$Json_Encode$string(
			A2(_user$project$Console_Text$render, useColor, txt));
	});
var _user$project$Test_Reporter_Console$failureLabelsToText = function (_p0) {
	return _user$project$Console_Text$concat(
		A3(
			_elm_community$elm_test$Test_Runner$formatLabels,
			function (_p1) {
				return _user$project$Console_Text$dark(
					_user$project$Console_Text$plain(
						A2(
							_user$project$Test_Reporter_Console$withChar,
							_elm_lang$core$Native_Utils.chr('↓'),
							_p1)));
			},
			function (_p2) {
				return _user$project$Console_Text$red(
					A2(
						_user$project$Test_Reporter_Console$withChar,
						_elm_lang$core$Native_Utils.chr('✗'),
						_p2));
			},
			_p0));
};
var _user$project$Test_Reporter_Console$todoToChalk = function (message) {
	return _user$project$Console_Text$plain(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'◦ TODO: ',
			A2(_elm_lang$core$Basics_ops['++'], message, '\n\n')));
};
var _user$project$Test_Reporter_Console$todoLabelsToText = function (_p3) {
	return _user$project$Console_Text$concat(
		A3(
			_elm_community$elm_test$Test_Runner$formatLabels,
			function (_p4) {
				return _user$project$Console_Text$dark(
					_user$project$Console_Text$plain(
						A2(
							_user$project$Test_Reporter_Console$withChar,
							_elm_lang$core$Native_Utils.chr('↓'),
							_p4)));
			},
			function (_p5) {
				return _user$project$Console_Text$dark(
					_user$project$Console_Text$plain(
						A2(
							_user$project$Test_Reporter_Console$withChar,
							_elm_lang$core$Native_Utils.chr('↓'),
							_p5)));
			},
			_p3));
};
var _user$project$Test_Reporter_Console$todosToText = function (_p6) {
	var _p7 = _p6;
	return _user$project$Console_Text$concat(
		{
			ctor: '::',
			_0: _user$project$Test_Reporter_Console$todoLabelsToText(_p7._0),
			_1: {
				ctor: '::',
				_0: _user$project$Test_Reporter_Console$todoToChalk(_p7._1),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Test_Reporter_Console$summarizeTodos = function (_p8) {
	return _user$project$Console_Text$concat(
		A2(_elm_lang$core$List$map, _user$project$Test_Reporter_Console$todosToText, _p8));
};
var _user$project$Test_Reporter_Console$pluralize = F3(
	function (singular, plural, count) {
		var suffix = _elm_lang$core$Native_Utils.eq(count, 1) ? singular : plural;
		return A2(
			_elm_lang$core$String$join,
			' ',
			{
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(count),
				_1: {
					ctor: '::',
					_0: suffix,
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$Test_Reporter_Console$reportBegin = F2(
	function (useColor, _p9) {
		var _p10 = _p9;
		var prefix = A2(
			_elm_lang$core$Basics_ops['++'],
			'Running ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A3(_user$project$Test_Reporter_Console$pluralize, 'test', 'tests', _p10.testCount),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'. To reproduce these results, run: elm-test --fuzz ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p10.fuzzRuns),
						A2(
							_elm_lang$core$Basics_ops['++'],
							' --seed ',
							_elm_lang$core$Basics$toString(_p10.initialSeed))))));
		return _elm_lang$core$Maybe$Just(
			A2(
				_user$project$Test_Reporter_Console$textToValue,
				useColor,
				_user$project$Console_Text$plain(
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(
							_elm_lang$core$String$join,
							' ',
							{ctor: '::', _0: prefix, _1: _p10.paths}),
						'\n'))));
	});
var _user$project$Test_Reporter_Console$indent = function (str) {
	return A2(
		_elm_lang$core$String$join,
		'\n',
		A2(
			_elm_lang$core$List$map,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$Basics_ops['++'], x, y);
				})('    '),
			A2(_elm_lang$core$String$split, '\n', str)));
};
var _user$project$Test_Reporter_Console$failureToText = F2(
	function (useColor, _p11) {
		var _p12 = _p11;
		var formatEquality = function () {
			var _p13 = useColor;
			if (_p13.ctor === 'Monochrome') {
				return _user$project$Test_Reporter_Console_Format_Monochrome$formatEquality;
			} else {
				return _user$project$Test_Reporter_Console_Format_Color$formatEquality;
			}
		}();
		var messageText = _user$project$Console_Text$plain(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'\n',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Test_Reporter_Console$indent(
						A3(_user$project$Test_Reporter_Console_Format$format, formatEquality, _p12.description, _p12.reason)),
					'\n\n')));
		var _p14 = _p12.given;
		if (_p14.ctor === 'Nothing') {
			return messageText;
		} else {
			return _user$project$Console_Text$concat(
				{
					ctor: '::',
					_0: _user$project$Console_Text$dark(
						_user$project$Console_Text$plain(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\nGiven ',
								A2(_elm_lang$core$Basics_ops['++'], _p14._0, '\n')))),
					_1: {
						ctor: '::',
						_0: messageText,
						_1: {ctor: '[]'}
					}
				});
		}
	});
var _user$project$Test_Reporter_Console$failuresToText = F3(
	function (useColor, labels, failures) {
		return _user$project$Console_Text$concat(
			{
				ctor: '::',
				_0: _user$project$Test_Reporter_Console$failureLabelsToText(labels),
				_1: A2(
					_elm_lang$core$List$map,
					_user$project$Test_Reporter_Console$failureToText(useColor),
					failures)
			});
	});
var _user$project$Test_Reporter_Console$reportComplete = F2(
	function (useColor, _p15) {
		var _p16 = _p15;
		var _p18 = _p16.labels;
		var _p17 = _p16.outcome;
		switch (_p17.ctor) {
			case 'Passed':
				return _elm_lang$core$Json_Encode$null;
			case 'Failed':
				return A2(
					_user$project$Test_Reporter_Console$textToValue,
					useColor,
					A3(_user$project$Test_Reporter_Console$failuresToText, useColor, _p18, _p17._0));
			default:
				return _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'todo',
							_1: _elm_lang$core$Json_Encode$string(_p17._0)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'labels',
								_1: _elm_lang$core$Json_Encode$list(
									A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p18))
							},
							_1: {ctor: '[]'}
						}
					});
		}
	});
var _user$project$Test_Reporter_Console$formatDuration = function (time) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(time),
		' ms');
};
var _user$project$Test_Reporter_Console$reportSummary = F3(
	function (useColor, _p19, autoFail) {
		var _p20 = _p19;
		var _p26 = _p20.todos;
		var _p25 = _p20.failed;
		var individualTodos = (_elm_lang$core$Native_Utils.cmp(_p25, 0) > 0) ? _user$project$Console_Text$plain('') : _user$project$Test_Reporter_Console$summarizeTodos(
			_elm_lang$core$List$reverse(_p26));
		var todoStats = function () {
			var _p21 = _elm_lang$core$List$length(_p26);
			if (_p21 === 0) {
				return _user$project$Console_Text$plain('');
			} else {
				return A2(
					_user$project$Test_Reporter_Console$stat,
					'Todo:     ',
					_elm_lang$core$Basics$toString(_p21));
			}
		}();
		var headlineResult = function () {
			var _p22 = {
				ctor: '_Tuple3',
				_0: autoFail,
				_1: _p25,
				_2: _elm_lang$core$List$length(_p26)
			};
			_v9_4:
			do {
				if (_p22._0.ctor === 'Nothing') {
					if (_p22._1 === 0) {
						switch (_p22._2) {
							case 0:
								return _elm_lang$core$Result$Ok('TEST RUN PASSED');
							case 1:
								return _elm_lang$core$Result$Err(
									{ctor: '_Tuple3', _0: _user$project$Console_Text$yellow, _1: 'TEST RUN INCOMPLETE', _2: ' because there is 1 TODO remaining'});
							default:
								return _elm_lang$core$Result$Err(
									{
										ctor: '_Tuple3',
										_0: _user$project$Console_Text$yellow,
										_1: 'TEST RUN INCOMPLETE',
										_2: A2(
											_elm_lang$core$Basics_ops['++'],
											' because there are ',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p22._2),
												' TODOs remaining'))
									});
						}
					} else {
						break _v9_4;
					}
				} else {
					if (_p22._1 === 0) {
						return _elm_lang$core$Result$Err(
							{
								ctor: '_Tuple3',
								_0: _user$project$Console_Text$yellow,
								_1: 'TEST RUN INCOMPLETE',
								_2: A2(_elm_lang$core$Basics_ops['++'], ' because ', _p22._0._0)
							});
					} else {
						break _v9_4;
					}
				}
			} while(false);
			return _elm_lang$core$Result$Err(
				{ctor: '_Tuple3', _0: _user$project$Console_Text$red, _1: 'TEST RUN FAILED', _2: ''});
		}();
		var headline = function () {
			var _p23 = headlineResult;
			if (_p23.ctor === 'Ok') {
				return _user$project$Console_Text$underline(
					_user$project$Console_Text$green(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'\n',
							A2(_elm_lang$core$Basics_ops['++'], _p23._0, '\n\n'))));
			} else {
				var _p24 = _p23._0._0;
				return _user$project$Console_Text$concat(
					{
						ctor: '::',
						_0: _user$project$Console_Text$underline(
							_p24(
								A2(_elm_lang$core$Basics_ops['++'], '\n', _p23._0._1))),
						_1: {
							ctor: '::',
							_0: _p24(
								A2(_elm_lang$core$Basics_ops['++'], _p23._0._2, '\n\n')),
							_1: {ctor: '[]'}
						}
					});
			}
		}();
		return _elm_lang$core$Json_Encode$string(
			A2(
				_user$project$Console_Text$render,
				useColor,
				_user$project$Console_Text$concat(
					{
						ctor: '::',
						_0: headline,
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$Test_Reporter_Console$stat,
								'Duration: ',
								_user$project$Test_Reporter_Console$formatDuration(_p20.duration)),
							_1: {
								ctor: '::',
								_0: A2(
									_user$project$Test_Reporter_Console$stat,
									'Passed:   ',
									_elm_lang$core$Basics$toString(_p20.passed)),
								_1: {
									ctor: '::',
									_0: A2(
										_user$project$Test_Reporter_Console$stat,
										'Failed:   ',
										_elm_lang$core$Basics$toString(_p25)),
									_1: {
										ctor: '::',
										_0: todoStats,
										_1: {
											ctor: '::',
											_0: individualTodos,
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					})));
	});

var _user$project$Test_Reporter_JUnit$reasonToString = F2(
	function (description, reason) {
		var _p0 = reason;
		switch (_p0.ctor) {
			case 'Custom':
				return description;
			case 'Equality':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p0._0,
					A2(_elm_lang$core$Basics_ops['++'], '\n\nwas not equal to\n\n', _p0._1));
			case 'Comparison':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p0._0,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\n\nfailed when compared with ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							description,
							A2(_elm_lang$core$Basics_ops['++'], ' on\n\n', _p0._1))));
			case 'TODO':
				return A2(_elm_lang$core$Basics_ops['++'], 'TODO: ', description);
			case 'Invalid':
				if (_p0._0.ctor === 'BadDescription') {
					var explanation = _elm_lang$core$Native_Utils.eq(description, '') ? 'The empty string is not a valid test description.' : A2(_elm_lang$core$Basics_ops['++'], 'This is an invalid test description: ', description);
					return A2(_elm_lang$core$Basics_ops['++'], 'Invalid test: ', explanation);
				} else {
					return A2(_elm_lang$core$Basics_ops['++'], 'Invalid test: ', description);
				}
			case 'ListDiff':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p0._0),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\n\nhad different elements than\n\n',
						_elm_lang$core$Basics$toString(_p0._1)));
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p0._0.expected),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\n\nhad different contents than\n\n',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(_p0._0.actual),
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\n\nthese were extra:\n\n',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_p0._0.extra),
									A2(
										_elm_lang$core$Basics_ops['++'],
										'\n\nthese were missing:\n\n',
										_elm_lang$core$Basics$toString(_p0._0.missing)))))));
		}
	});
var _user$project$Test_Reporter_JUnit$encodeTime = function (time) {
	return _elm_lang$core$Json_Encode$string(
		_elm_lang$core$Basics$toString(
			_elm_lang$core$Time$inSeconds(time)));
};
var _user$project$Test_Reporter_JUnit$formatClassAndName = function (labels) {
	var _p1 = labels;
	if (_p1.ctor === '::') {
		return {
			ctor: '_Tuple2',
			_0: A2(
				_elm_lang$core$String$join,
				' ',
				_elm_lang$core$List$reverse(_p1._1)),
			_1: _p1._0
		};
	} else {
		return {ctor: '_Tuple2', _0: '', _1: ''};
	}
};
var _user$project$Test_Reporter_JUnit$formatFailure = function (_p2) {
	var _p3 = _p2;
	var message = A2(_user$project$Test_Reporter_JUnit$reasonToString, _p3.description, _p3.reason);
	var _p4 = _p3.given;
	if (_p4.ctor === 'Just') {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'Given ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p4._0,
				A2(_elm_lang$core$Basics_ops['++'], '\n\n', message)));
	} else {
		return message;
	}
};
var _user$project$Test_Reporter_JUnit$encodeFailureTuple = function (message) {
	return {
		ctor: '_Tuple2',
		_0: 'failure',
		_1: _elm_lang$core$Json_Encode$string(message)
	};
};
var _user$project$Test_Reporter_JUnit$encodeOutcome = function (outcome) {
	var _p5 = outcome;
	switch (_p5.ctor) {
		case 'Passed':
			return {ctor: '[]'};
		case 'Failed':
			var message = A2(
				_elm_lang$core$String$join,
				'\n\n\n',
				A2(_elm_lang$core$List$map, _user$project$Test_Reporter_JUnit$formatFailure, _p5._0));
			return {
				ctor: '::',
				_0: _user$project$Test_Reporter_JUnit$encodeFailureTuple(message),
				_1: {ctor: '[]'}
			};
		default:
			return {
				ctor: '::',
				_0: _user$project$Test_Reporter_JUnit$encodeFailureTuple(
					A2(_elm_lang$core$Basics_ops['++'], 'TODO: ', _p5._0)),
				_1: {ctor: '[]'}
			};
	}
};
var _user$project$Test_Reporter_JUnit$reportComplete = function (_p6) {
	var _p7 = _p6;
	var _p8 = _user$project$Test_Reporter_JUnit$formatClassAndName(_p7.labels);
	var classname = _p8._0;
	var name = _p8._1;
	return _elm_lang$core$Json_Encode$object(
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: '@classname',
					_1: _elm_lang$core$Json_Encode$string(classname)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: '@name',
						_1: _elm_lang$core$Json_Encode$string(name)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: '@time',
							_1: _user$project$Test_Reporter_JUnit$encodeTime(_p7.duration)
						},
						_1: {ctor: '[]'}
					}
				}
			},
			_user$project$Test_Reporter_JUnit$encodeOutcome(_p7.outcome)));
};
var _user$project$Test_Reporter_JUnit$encodeExtraFailure = function (failure) {
	return _user$project$Test_Reporter_JUnit$reportComplete(
		{
			labels: {ctor: '[]'},
			duration: 0,
			outcome: _user$project$Test_Reporter_TestResults$Failed(
				{ctor: '[]'})
		});
};
var _user$project$Test_Reporter_JUnit$reportSummary = F2(
	function (_p9, autoFail) {
		var _p10 = _p9;
		var _p12 = _p10.failed;
		var extraFailures = function () {
			var _p11 = {ctor: '_Tuple2', _0: _p12, _1: autoFail};
			if (((_p11.ctor === '_Tuple2') && (_p11._0 === 0)) && (_p11._1.ctor === 'Just')) {
				return {
					ctor: '::',
					_0: _user$project$Test_Reporter_JUnit$encodeExtraFailure(_p11._1._0),
					_1: {ctor: '[]'}
				};
			} else {
				return {ctor: '[]'};
			}
		}();
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'testsuite',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: '@name',
								_1: _elm_lang$core$Json_Encode$string('elm-test')
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: '@package',
									_1: _elm_lang$core$Json_Encode$string('elm-test')
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: '@tests',
										_1: _elm_lang$core$Json_Encode$int(_p10.testCount)
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: '@failed',
											_1: _elm_lang$core$Json_Encode$int(_p12)
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: '@errors',
												_1: _elm_lang$core$Json_Encode$int(0)
											},
											_1: {
												ctor: '::',
												_0: {
													ctor: '_Tuple2',
													_0: '@time',
													_1: _elm_lang$core$Json_Encode$float(_p10.duration)
												},
												_1: {
													ctor: '::',
													_0: {
														ctor: '_Tuple2',
														_0: 'testcase',
														_1: _elm_lang$core$Json_Encode$list(extraFailures)
													},
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						})
				},
				_1: {ctor: '[]'}
			});
	});
var _user$project$Test_Reporter_JUnit$reportBegin = function (_p13) {
	return _elm_lang$core$Maybe$Nothing;
};

var _user$project$Test_Reporter_Json$encodeReasonType = F2(
	function (reasonType, data) {
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('custom')
				},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'data', _1: data},
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$Test_Reporter_Json$encodeReason = F2(
	function (description, reason) {
		var _p0 = reason;
		switch (_p0.ctor) {
			case 'Custom':
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'Custom',
					_elm_lang$core$Json_Encode$string(description));
			case 'Equality':
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'Equality',
					_elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'expected',
								_1: _elm_lang$core$Json_Encode$string(_p0._0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'actual',
									_1: _elm_lang$core$Json_Encode$string(_p0._1)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'comparison',
										_1: _elm_lang$core$Json_Encode$string(description)
									},
									_1: {ctor: '[]'}
								}
							}
						}));
			case 'Comparison':
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'Comparison',
					_elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'first',
								_1: _elm_lang$core$Json_Encode$string(_p0._0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'second',
									_1: _elm_lang$core$Json_Encode$string(_p0._1)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'comparison',
										_1: _elm_lang$core$Json_Encode$string(description)
									},
									_1: {ctor: '[]'}
								}
							}
						}));
			case 'TODO':
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'TODO',
					_elm_lang$core$Json_Encode$string(description));
			case 'Invalid':
				if (_p0._0.ctor === 'BadDescription') {
					var explanation = _elm_lang$core$Native_Utils.eq(description, '') ? 'The empty string is not a valid test description.' : A2(_elm_lang$core$Basics_ops['++'], 'This is an invalid test description: ', description);
					return A2(
						_user$project$Test_Reporter_Json$encodeReasonType,
						'Invalid',
						_elm_lang$core$Json_Encode$string(explanation));
				} else {
					return A2(
						_user$project$Test_Reporter_Json$encodeReasonType,
						'Invalid',
						_elm_lang$core$Json_Encode$string(description));
				}
			case 'ListDiff':
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'ListDiff',
					_elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'expected',
								_1: _elm_lang$core$Json_Encode$list(
									A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p0._0))
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'actual',
									_1: _elm_lang$core$Json_Encode$list(
										A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p0._1))
								},
								_1: {ctor: '[]'}
							}
						}));
			default:
				return A2(
					_user$project$Test_Reporter_Json$encodeReasonType,
					'CollectionDiff',
					_elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'expected',
								_1: _elm_lang$core$Json_Encode$string(_p0._0.expected)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'actual',
									_1: _elm_lang$core$Json_Encode$string(_p0._0.actual)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'extra',
										_1: _elm_lang$core$Json_Encode$list(
											A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p0._0.extra))
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'missing',
											_1: _elm_lang$core$Json_Encode$list(
												A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p0._0.missing))
										},
										_1: {ctor: '[]'}
									}
								}
							}
						}));
		}
	});
var _user$project$Test_Reporter_Json$encodeFailure = function (_p1) {
	var _p2 = _p1;
	var _p3 = _p2.description;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'given',
				_1: A2(
					_elm_lang$core$Maybe$withDefault,
					_elm_lang$core$Json_Encode$null,
					A2(_elm_lang$core$Maybe$map, _elm_lang$core$Json_Encode$string, _p2.given))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'message',
					_1: _elm_lang$core$Json_Encode$string(_p3)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'reason',
						_1: A2(_user$project$Test_Reporter_Json$encodeReason, _p3, _p2.reason)
					},
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Test_Reporter_Json$reportSummary = F2(
	function (_p4, autoFail) {
		var _p5 = _p4;
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'event',
					_1: _elm_lang$core$Json_Encode$string('runComplete')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'passed',
						_1: _elm_lang$core$Json_Encode$string(
							_elm_lang$core$Basics$toString(_p5.passed))
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'failed',
							_1: _elm_lang$core$Json_Encode$string(
								_elm_lang$core$Basics$toString(_p5.failed))
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'duration',
								_1: _elm_lang$core$Json_Encode$string(
									_elm_lang$core$Basics$toString(_p5.duration))
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'autoFail',
									_1: A2(
										_elm_lang$core$Maybe$withDefault,
										_elm_lang$core$Json_Encode$null,
										A2(_elm_lang$core$Maybe$map, _elm_lang$core$Json_Encode$string, autoFail))
								},
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
	});
var _user$project$Test_Reporter_Json$encodeLabels = function (labels) {
	return _elm_lang$core$Json_Encode$list(
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Json_Encode$string,
			_elm_lang$core$List$reverse(labels)));
};
var _user$project$Test_Reporter_Json$getStatus = function (outcome) {
	var _p6 = outcome;
	switch (_p6.ctor) {
		case 'Failed':
			return 'fail';
		case 'Todo':
			return 'todo';
		default:
			return 'pass';
	}
};
var _user$project$Test_Reporter_Json$encodeFailures = function (outcome) {
	var _p7 = outcome;
	switch (_p7.ctor) {
		case 'Failed':
			return A2(_elm_lang$core$List$map, _user$project$Test_Reporter_Json$encodeFailure, _p7._0);
		case 'Todo':
			return {
				ctor: '::',
				_0: _elm_lang$core$Json_Encode$string(_p7._0),
				_1: {ctor: '[]'}
			};
		default:
			return {ctor: '[]'};
	}
};
var _user$project$Test_Reporter_Json$reportComplete = function (_p8) {
	var _p9 = _p8;
	var _p10 = _p9.outcome;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'event',
				_1: _elm_lang$core$Json_Encode$string('testCompleted')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'status',
					_1: _elm_lang$core$Json_Encode$string(
						_user$project$Test_Reporter_Json$getStatus(_p10))
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'labels',
						_1: _user$project$Test_Reporter_Json$encodeLabels(_p9.labels)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'failures',
							_1: _elm_lang$core$Json_Encode$list(
								_user$project$Test_Reporter_Json$encodeFailures(_p10))
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'duration',
								_1: _elm_lang$core$Json_Encode$string(
									_elm_lang$core$Basics$toString(_p9.duration))
							},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
};
var _user$project$Test_Reporter_Json$reportBegin = function (_p11) {
	var _p12 = _p11;
	return _elm_lang$core$Maybe$Just(
		_elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'event',
					_1: _elm_lang$core$Json_Encode$string('runStart')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'testCount',
						_1: _elm_lang$core$Json_Encode$string(
							_elm_lang$core$Basics$toString(_p12.testCount))
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'fuzzRuns',
							_1: _elm_lang$core$Json_Encode$string(
								_elm_lang$core$Basics$toString(_p12.fuzzRuns))
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'paths',
								_1: _elm_lang$core$Json_Encode$list(
									A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p12.paths))
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'initialSeed',
									_1: _elm_lang$core$Json_Encode$string(
										_elm_lang$core$Basics$toString(_p12.initialSeed))
								},
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}));
};

var _user$project$Test_Reporter_Reporter$TestReporter = F4(
	function (a, b, c, d) {
		return {format: a, reportBegin: b, reportComplete: c, reportSummary: d};
	});
var _user$project$Test_Reporter_Reporter$createReporter = function (report) {
	var _p0 = report;
	switch (_p0.ctor) {
		case 'JsonReport':
			return A4(_user$project$Test_Reporter_Reporter$TestReporter, 'JSON', _user$project$Test_Reporter_Json$reportBegin, _user$project$Test_Reporter_Json$reportComplete, _user$project$Test_Reporter_Json$reportSummary);
		case 'ConsoleReport':
			var _p1 = _p0._0;
			return A4(
				_user$project$Test_Reporter_Reporter$TestReporter,
				'CHALK',
				_user$project$Test_Reporter_Console$reportBegin(_p1),
				_user$project$Test_Reporter_Console$reportComplete(_p1),
				_user$project$Test_Reporter_Console$reportSummary(_p1));
		default:
			return A4(_user$project$Test_Reporter_Reporter$TestReporter, 'JUNIT', _user$project$Test_Reporter_JUnit$reportBegin, _user$project$Test_Reporter_JUnit$reportComplete, _user$project$Test_Reporter_JUnit$reportSummary);
	}
};
var _user$project$Test_Reporter_Reporter$RunInfo = F4(
	function (a, b, c, d) {
		return {paths: a, fuzzRuns: b, testCount: c, initialSeed: d};
	});
var _user$project$Test_Reporter_Reporter$JUnitReport = {ctor: 'JUnitReport'};
var _user$project$Test_Reporter_Reporter$JsonReport = {ctor: 'JsonReport'};
var _user$project$Test_Reporter_Reporter$ConsoleReport = function (a) {
	return {ctor: 'ConsoleReport', _0: a};
};

var _user$project$Test_Runner_JsMessage$todoDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(
		_elm_lang$core$Json_Decode$field,
		'labels',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'todo', _elm_lang$core$Json_Decode$string));
var _user$project$Test_Runner_JsMessage$Summary = F3(
	function (a, b, c) {
		return {ctor: 'Summary', _0: a, _1: b, _2: c};
	});
var _user$project$Test_Runner_JsMessage$Test = function (a) {
	return {ctor: 'Test', _0: a};
};
var _user$project$Test_Runner_JsMessage$decodeMessageFromType = function (messageType) {
	var _p0 = messageType;
	switch (_p0) {
		case 'TEST':
			return A2(
				_elm_lang$core$Json_Decode$map,
				_user$project$Test_Runner_JsMessage$Test,
				A2(_elm_lang$core$Json_Decode$field, 'index', _elm_lang$core$Json_Decode$int));
		case 'SUMMARY':
			return A4(
				_elm_lang$core$Json_Decode$map3,
				_user$project$Test_Runner_JsMessage$Summary,
				A2(_elm_lang$core$Json_Decode$field, 'duration', _elm_lang$core$Json_Decode$float),
				A2(_elm_lang$core$Json_Decode$field, 'failures', _elm_lang$core$Json_Decode$int),
				A2(
					_elm_lang$core$Json_Decode$field,
					'todos',
					_elm_lang$core$Json_Decode$list(_user$project$Test_Runner_JsMessage$todoDecoder)));
		default:
			return _elm_lang$core$Json_Decode$fail(
				A2(_elm_lang$core$Basics_ops['++'], 'Unrecognized message type: ', messageType));
	}
};
var _user$project$Test_Runner_JsMessage$decoder = A2(
	_elm_lang$core$Json_Decode$andThen,
	_user$project$Test_Runner_JsMessage$decodeMessageFromType,
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));

var _user$project$Test_Runner_Node_App$defaultRunCount = 100;
var _user$project$Test_Runner_Node_App$timeToNumericSeed = function (time) {
	return _elm_lang$core$Tuple$first(
		A2(
			_mgold$elm_random_pcg$Random_Pcg$step,
			A2(_mgold$elm_random_pcg$Random_Pcg$int, 100, _mgold$elm_random_pcg$Random_Pcg$maxInt),
			_mgold$elm_random_pcg$Random_Pcg$initialSeed(
				_elm_lang$core$Basics$floor(time))));
};
var _user$project$Test_Runner_Node_App$InitArgs = F7(
	function (a, b, c, d, e, f, g) {
		return {initialSeed: a, processes: b, paths: c, fuzzRuns: d, startTime: e, runners: f, report: g};
	});
var _user$project$Test_Runner_Node_App$RunnerOptions = F5(
	function (a, b, c, d, e) {
		return {seed: a, runs: b, report: c, paths: d, processes: e};
	});
var _user$project$Test_Runner_Node_App$AppOptions = F3(
	function (a, b, c) {
		return {init: a, update: b, subscriptions: c};
	});
var _user$project$Test_Runner_Node_App$SubMsg = function (a) {
	return {ctor: 'SubMsg', _0: a};
};
var _user$project$Test_Runner_Node_App$subscriptions = F2(
	function (subs, model) {
		var _p0 = model;
		if (_p0.ctor === 'Uninitialized') {
			return _elm_lang$core$Platform_Sub$none;
		} else {
			return A2(
				_elm_lang$core$Platform_Sub$map,
				_user$project$Test_Runner_Node_App$SubMsg,
				subs(_p0._1));
		}
	});
var _user$project$Test_Runner_Node_App$Init = function (a) {
	return {ctor: 'Init', _0: a};
};
var _user$project$Test_Runner_Node_App$Uninitialized = F2(
	function (a, b) {
		return {ctor: 'Uninitialized', _0: a, _1: b};
	});
var _user$project$Test_Runner_Node_App$Initialized = F2(
	function (a, b) {
		return {ctor: 'Initialized', _0: a, _1: b};
	});
var _user$project$Test_Runner_Node_App$initOrUpdate = F2(
	function (msg, maybeModel) {
		var _p1 = maybeModel;
		if (_p1.ctor === 'Uninitialized') {
			var _p7 = _p1._1.runs;
			var _p2 = msg;
			if (_p2.ctor === 'Init') {
				var _p5 = _p2._0;
				var numericSeed = function () {
					var _p3 = _p1._1.maybeInitialSeed;
					if (_p3.ctor === 'Just') {
						return _p3._0;
					} else {
						return _user$project$Test_Runner_Node_App$timeToNumericSeed(_p5);
					}
				}();
				var seed = _mgold$elm_random_pcg$Random_Pcg$initialSeed(numericSeed);
				var runners = A3(_elm_community$elm_test$Test_Runner$fromTest, _p7, seed, _p1._1.test);
				var _p4 = _p1._1.init(
					{initialSeed: numericSeed, processes: _p1._1.processes, fuzzRuns: _p7, paths: _p1._1.paths, startTime: _p5, runners: runners, report: _p1._1.report});
				var subModel = _p4._0;
				var subCmd = _p4._1;
				return {
					ctor: '_Tuple2',
					_0: A2(_user$project$Test_Runner_Node_App$Initialized, _p1._0, subModel),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Test_Runner_Node_App$SubMsg, subCmd)
				};
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Test.Runner.Node.App',
					{
						start: {line: 62, column: 13},
						end: {line: 93, column: 70}
					},
					_p2)('Attempted to run a SubMsg pre-Init!');
			}
		} else {
			var _p11 = _p1._0;
			var _p8 = msg;
			if (_p8.ctor === 'SubMsg') {
				var _p9 = A2(_p11, _p8._0, _p1._1);
				var newModel = _p9._0;
				var cmd = _p9._1;
				return {
					ctor: '_Tuple2',
					_0: A2(_user$project$Test_Runner_Node_App$Initialized, _p11, newModel),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Test_Runner_Node_App$SubMsg, cmd)
				};
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Test.Runner.Node.App',
					{
						start: {line: 96, column: 13},
						end: {line: 105, column: 59}
					},
					_p8)('Attempted to init twice!');
			}
		}
	});
var _user$project$Test_Runner_Node_App$run = F3(
	function (_p12, appOpts, test) {
		var _p13 = _p12;
		var init = function (args) {
			var cmd = A2(_elm_lang$core$Task$perform, _user$project$Test_Runner_Node_App$Init, _elm_lang$core$Time$now);
			return {
				ctor: '_Tuple2',
				_0: A2(
					_user$project$Test_Runner_Node_App$Uninitialized,
					appOpts.update,
					{
						maybeInitialSeed: _p13.seed,
						processes: _p13.processes,
						report: _p13.report,
						runs: A2(_elm_lang$core$Maybe$withDefault, _user$project$Test_Runner_Node_App$defaultRunCount, _p13.runs),
						paths: _p13.paths,
						test: test,
						init: appOpts.init
					}),
				_1: cmd
			};
		};
		return _elm_lang$core$Platform$programWithFlags(
			{
				init: init,
				update: _user$project$Test_Runner_Node_App$initOrUpdate,
				subscriptions: _user$project$Test_Runner_Node_App$subscriptions(appOpts.subscriptions)
			});
	});

var _user$project$Test_Runner_Node$init = function (_p0) {
	var _p1 = _p0;
	var testReporter = _user$project$Test_Reporter_Reporter$createReporter(_p1.report);
	var _p2 = function () {
		var _p3 = _p1.runners;
		switch (_p3.ctor) {
			case 'Plain':
				return {
					indexedRunners: A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (v0, v1) {
								return {ctor: '_Tuple2', _0: v0, _1: v1};
							}),
						_p3._0),
					autoFail: _elm_lang$core$Maybe$Nothing
				};
			case 'Only':
				return {
					indexedRunners: A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (v0, v1) {
								return {ctor: '_Tuple2', _0: v0, _1: v1};
							}),
						_p3._0),
					autoFail: _elm_lang$core$Maybe$Just('Test.only was used')
				};
			case 'Skipping':
				return {
					indexedRunners: A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (v0, v1) {
								return {ctor: '_Tuple2', _0: v0, _1: v1};
							}),
						_p3._0),
					autoFail: _elm_lang$core$Maybe$Just('Test.skip was used')
				};
			default:
				return {
					indexedRunners: {ctor: '[]'},
					autoFail: _elm_lang$core$Maybe$Just(_p3._0)
				};
		}
	}();
	var indexedRunners = _p2.indexedRunners;
	var autoFail = _p2.autoFail;
	var testCount = _elm_lang$core$List$length(indexedRunners);
	var model = {
		available: _elm_lang$core$Dict$fromList(indexedRunners),
		runInfo: {testCount: testCount, paths: _p1.paths, fuzzRuns: _p1.fuzzRuns, initialSeed: _p1.initialSeed},
		processes: _p1.processes,
		nextTestToRun: 0,
		results: {ctor: '[]'},
		testReporter: testReporter,
		autoFail: autoFail
	};
	return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
};
var _user$project$Test_Runner_Node$countFailures = F2(
	function (_p4, failures) {
		var _p5 = _p4;
		var _p6 = _p5._1.outcome;
		if (_p6.ctor === 'Failed') {
			return failures + 1;
		} else {
			return failures;
		}
	});
var _user$project$Test_Runner_Node$warn = F2(
	function (str, result) {
		var _p7 = _elm_lang$core$Debug$log(str);
		return result;
	});
var _user$project$Test_Runner_Node$runThunk = _user$project$Native_RunTest.runThunk;
var _user$project$Test_Runner_Node$receive = _elm_lang$core$Native_Platform.incomingPort('receive', _elm_lang$core$Json_Decode$value);
var _user$project$Test_Runner_Node$send = _elm_lang$core$Native_Platform.outgoingPort(
	'send',
	function (v) {
		return v;
	});
var _user$project$Test_Runner_Node$sendResults = F3(
	function (isFinished, testReporter, results) {
		var addToKeyValues = F2(
			function (_p8, list) {
				var _p9 = _p8;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Basics$toString(_p9._0),
						_1: testReporter.reportComplete(_p9._1)
					},
					_1: list
				};
			});
		var typeStr = isFinished ? 'FINISHED' : 'RESULTS';
		return _user$project$Test_Runner_Node$send(
			A2(
				_elm_lang$core$Json_Encode$encode,
				0,
				_elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'type',
							_1: _elm_lang$core$Json_Encode$string(typeStr)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'results',
								_1: _elm_lang$core$Json_Encode$object(
									A3(
										_elm_lang$core$List$foldl,
										addToKeyValues,
										{ctor: '[]'},
										results))
							},
							_1: {ctor: '[]'}
						}
					})));
	});
var _user$project$Test_Runner_Node$sendBegin = function (model) {
	var extraFields = function () {
		var _p10 = model.testReporter.reportBegin(model.runInfo);
		if (_p10.ctor === 'Just') {
			return {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'message', _1: _p10._0},
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	}();
	var baseFields = {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 'type',
			_1: _elm_lang$core$Json_Encode$string('BEGIN')
		},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'testCount',
				_1: _elm_lang$core$Json_Encode$int(model.runInfo.testCount)
			},
			_1: {ctor: '[]'}
		}
	};
	return _user$project$Test_Runner_Node$send(
		A2(
			_elm_lang$core$Json_Encode$encode,
			0,
			_elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$Basics_ops['++'], baseFields, extraFields))));
};
var _user$project$Test_Runner_Node$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {available: a, runInfo: b, testReporter: c, results: d, processes: e, nextTestToRun: f, autoFail: g};
	});
var _user$project$Test_Runner_Node$Complete = F4(
	function (a, b, c, d) {
		return {ctor: 'Complete', _0: a, _1: b, _2: c, _3: d};
	});
var _user$project$Test_Runner_Node$dispatch = F2(
	function (model, startTime) {
		var _p11 = A2(_elm_lang$core$Dict$get, model.nextTestToRun, model.available);
		if (_p11.ctor === 'Nothing') {
			return A3(_user$project$Test_Runner_Node$sendResults, true, model.testReporter, model.results);
		} else {
			var outcomes = _user$project$Test_Reporter_TestResults$outcomesFromExpectations(
				_user$project$Test_Runner_Node$runThunk(_p11._0.run));
			return A2(
				_elm_lang$core$Task$perform,
				A3(_user$project$Test_Runner_Node$Complete, _p11._0.labels, outcomes, startTime),
				_elm_lang$core$Time$now);
		}
	});
var _user$project$Test_Runner_Node$Dispatch = function (a) {
	return {ctor: 'Dispatch', _0: a};
};
var _user$project$Test_Runner_Node$update = F2(
	function (msg, _p12) {
		var _p13 = _p12;
		var _p21 = _p13.testReporter;
		var _p20 = _p13;
		var _p14 = msg;
		switch (_p14.ctor) {
			case 'Receive':
				var _p15 = A2(_elm_lang$core$Json_Decode$decodeValue, _user$project$Test_Runner_JsMessage$decoder, _p14._0);
				if (_p15.ctor === 'Ok') {
					if (_p15._0.ctor === 'Summary') {
						var _p17 = _p15._0._2;
						var _p16 = _p15._0._1;
						var exitCode = (_elm_lang$core$Native_Utils.cmp(_p16, 0) > 0) ? 2 : ((_elm_lang$core$Native_Utils.eq(_p20.autoFail, _elm_lang$core$Maybe$Nothing) && _elm_lang$core$List$isEmpty(_p17)) ? 0 : 3);
						var testCount = _p20.runInfo.testCount;
						var summaryInfo = {
							testCount: testCount,
							passed: (testCount - _p16) - _elm_lang$core$List$length(_p17),
							failed: _p16,
							todos: _p17,
							duration: _p15._0._0
						};
						var summary = A2(_p21.reportSummary, summaryInfo, _p20.autoFail);
						var cmd = _user$project$Test_Runner_Node$send(
							A2(
								_elm_lang$core$Json_Encode$encode,
								0,
								_elm_lang$core$Json_Encode$object(
									{
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'type',
											_1: _elm_lang$core$Json_Encode$string('SUMMARY')
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'exitCode',
												_1: _elm_lang$core$Json_Encode$int(exitCode)
											},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'message', _1: summary},
												_1: {ctor: '[]'}
											}
										}
									})));
						return {ctor: '_Tuple2', _0: _p20, _1: cmd};
					} else {
						var _p18 = _p15._0._0;
						var cmd = A2(_elm_lang$core$Task$perform, _user$project$Test_Runner_Node$Dispatch, _elm_lang$core$Time$now);
						return _elm_lang$core$Native_Utils.eq(_p18, -1) ? {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								_p20,
								{nextTestToRun: _p18 + _p20.processes}),
							_1: _elm_lang$core$Platform_Cmd$batch(
								{
									ctor: '::',
									_0: cmd,
									_1: {
										ctor: '::',
										_0: _user$project$Test_Runner_Node$sendBegin(_p20),
										_1: {ctor: '[]'}
									}
								})
						} : {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								_p20,
								{nextTestToRun: _p18}),
							_1: cmd
						};
					}
				} else {
					var cmd = _user$project$Test_Runner_Node$send(
						A2(
							_elm_lang$core$Json_Encode$encode,
							0,
							_elm_lang$core$Json_Encode$object(
								{
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'type',
										_1: _elm_lang$core$Json_Encode$string('ERROR')
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'message',
											_1: _elm_lang$core$Json_Encode$string(_p15._0)
										},
										_1: {ctor: '[]'}
									}
								})));
					return {ctor: '_Tuple2', _0: _p20, _1: cmd};
				}
			case 'Dispatch':
				return {
					ctor: '_Tuple2',
					_0: _p20,
					_1: A2(_user$project$Test_Runner_Node$dispatch, _p20, _p14._0)
				};
			default:
				var _p19 = _p14._1;
				var nextTestToRun = _p20.nextTestToRun + _p20.processes;
				var isFinished = _elm_lang$core$Native_Utils.cmp(nextTestToRun, _p20.runInfo.testCount) > -1;
				var duration = _p14._3 - _p14._2;
				var prependOutcome = F2(
					function (outcome, results) {
						return {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _p20.nextTestToRun,
								_1: {labels: _p14._0, outcome: outcome, duration: duration}
							},
							_1: results
						};
					});
				var results = A3(_elm_lang$core$List$foldl, prependOutcome, _p20.results, _p19);
				if (isFinished || A2(_elm_lang$core$List$any, _user$project$Test_Reporter_TestResults$isFailure, _p19)) {
					var cmd = A3(_user$project$Test_Runner_Node$sendResults, isFinished, _p21, results);
					return isFinished ? {ctor: '_Tuple2', _0: _p20, _1: cmd} : {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							_p20,
							{
								nextTestToRun: nextTestToRun,
								results: {ctor: '[]'}
							}),
						_1: _elm_lang$core$Platform_Cmd$batch(
							{
								ctor: '::',
								_0: cmd,
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$core$Task$perform, _user$project$Test_Runner_Node$Dispatch, _elm_lang$core$Time$now),
									_1: {ctor: '[]'}
								}
							})
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							_p20,
							{nextTestToRun: nextTestToRun, results: results}),
						_1: A2(_elm_lang$core$Task$perform, _user$project$Test_Runner_Node$Dispatch, _elm_lang$core$Time$now)
					};
				}
		}
	});
var _user$project$Test_Runner_Node$Receive = function (a) {
	return {ctor: 'Receive', _0: a};
};
var _user$project$Test_Runner_Node$runWithOptions = function (options) {
	return A2(
		_user$project$Test_Runner_Node_App$run,
		options,
		{
			init: _user$project$Test_Runner_Node$init,
			update: _user$project$Test_Runner_Node$update,
			subscriptions: function (_p22) {
				return _user$project$Test_Runner_Node$receive(_user$project$Test_Runner_Node$Receive);
			}
		});
};

var _user$project$Test_Generated_Main45040372$main = A2(
	_user$project$Test_Runner_Node$runWithOptions,
	{
		runs: _elm_lang$core$Maybe$Nothing,
		report: _user$project$Test_Reporter_Reporter$ConsoleReport(_user$project$Console_Text$UseColor),
		seed: _elm_lang$core$Maybe$Nothing,
		processes: 2,
		paths: {ctor: '[]'}
	},
	_elm_community$elm_test$Test$concat(
		{
			ctor: '::',
			_0: A2(
				_elm_community$elm_test$Test$describe,
				'Tests',
				{
					ctor: '::',
					_0: _user$project$Tests$all,
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		}))(_elm_lang$core$Json_Decode$value);

var Elm = {};
Elm['Test'] = Elm['Test'] || {};
Elm['Test']['Generated'] = Elm['Test']['Generated'] || {};
Elm['Test']['Generated']['Main45040372'] = Elm['Test']['Generated']['Main45040372'] || {};
if (typeof _user$project$Test_Generated_Main45040372$main !== 'undefined') {
    _user$project$Test_Generated_Main45040372$main(Elm['Test']['Generated']['Main45040372'], 'Test.Generated.Main45040372', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);


return module.exports;
})({});
var initialSeed = null;
var report = "console";
// Make sure necessary things are defined.
if (typeof Elm === "undefined") {
  throw "test runner config error: Elm is not defined. Make sure you provide a file compiled by Elm!";
}

var potentialModuleNames = Object.keys(Elm.Test.Generated);

if (potentialModuleNames.length !== 1) {
  console.error(
    "Multiple potential generated modules to run in the Elm.Test.Generated namespace: ",
    potentialModuleNames,
    " - this should never happen!"
  );
  process.exit(1);
}

var testModule = Elm.Test.Generated[potentialModuleNames[0]];

// Run the Elm app.
var app = testModule.worker({ seed: initialSeed, report: report });

// Use ports for inter-process communication.
app.ports.send.subscribe(function(msg) {
  process.send(msg);
});

process.on("message", function(msg) {
  app.ports.receive.send(msg);
});
