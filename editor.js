(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
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


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
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

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aj.R === region.ap.R)
	{
		return 'on line ' + region.aj.R;
	}
	return 'on lines ' + region.aj.R + ' through ' + region.ap.R;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bD,
		impl.bx,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

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


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		E: func(record.E),
		bw: record.bw,
		br: record.br
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.E;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bw;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.br) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bD,
		impl.bx,
		function(sendToApp, initialModel) {
			var view = impl.bE;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bD,
		impl.bx,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.ai && impl.ai(sendToApp)
			var view = impl.bE;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aY);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bB) && (_VirtualDom_doc.title = title = doc.bB);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bk;
	var onUrlRequest = impl.bl;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		ai: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aJ === next.aJ
							&& curr.av === next.av
							&& curr.aG.a === next.aG.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		be: function(flags)
		{
			return A3(impl.be, flags, _Browser_getUrl(), key);
		},
		bE: impl.bE,
		bD: impl.bD,
		bx: impl.bx
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { bb: 'hidden', a$: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bb: 'mozHidden', a$: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bb: 'msHidden', a$: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bb: 'webkitHidden', a$: 'webkitvisibilitychange' }
		: { bb: 'hidden', a$: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aN: _Browser_getScene(),
		aR: {
			aT: _Browser_window.pageXOffset,
			aU: _Browser_window.pageYOffset,
			aS: _Browser_doc.documentElement.clientWidth,
			au: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aS: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		au: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aN: {
				aS: node.scrollWidth,
				au: node.scrollHeight
			},
			aR: {
				aT: node.scrollLeft,
				aU: node.scrollTop,
				aS: node.clientWidth,
				au: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aN: _Browser_getScene(),
			aR: {
				aT: x,
				aU: y,
				aS: _Browser_doc.documentElement.clientWidth,
				au: _Browser_doc.documentElement.clientHeight
			},
			a8: {
				aT: x + rect.left,
				aU: y + rect.top,
				aS: rect.width,
				au: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.h) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.k),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.k);
		} else {
			var treeLen = builder.h * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.m) : builder.m;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.h);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.k) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.k);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{m: nodeList, h: (len / $elm$core$Array$branchFactor) | 0, k: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {as: fragment, av: host, aE: path, aG: port_, aJ: protocol, aK: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$AG2$ASTne = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$AG2$ASTxy = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$AG2$CommandFuncStart = 1;
var $author$project$AG2$EntryBrick = 1;
var $author$project$AG2$Nil = {$: 0};
var $author$project$AG2$setBrick = function () {
	var pos = _Utils_Tuple2(450, 100);
	return _List_fromArray(
		[
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '1'},
					d: 1,
					a: 0,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '2'},
					d: 1,
					a: 1,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '3'},
					d: 1,
					a: 2,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '4'},
					d: 1,
					a: 3,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '5'},
					d: 1,
					a: 4,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '6'},
					d: 1,
					a: 5,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '7'},
					d: 1,
					a: 6,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '8'},
					d: 1,
					a: 7,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			pos,
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: '9'},
					d: 1,
					a: 8,
					e: 1
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil))
		]);
}();
var $author$project$AG2$init = function (flags) {
	return _Utils_Tuple2(
		{
			x: _Utils_Tuple2(0, 0),
			l: $author$project$AG2$setBrick,
			y: 128,
			g: {
				a: -1,
				C: _Utils_Tuple2(0, 0),
				I: _Utils_Tuple2(0, 0),
				q: false,
				D: _Utils_Tuple2(0, 0)
			},
			A: 0,
			S: 0
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$AG2$GetMessage = function (a) {
	return {$: 9, a: a};
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$AG2$receiveMes = _Platform_incomingPort('receiveMes', $elm$json$Json$Decode$value);
var $author$project$AG2$subscriptions = function (model) {
	return $author$project$AG2$receiveMes($author$project$AG2$GetMessage);
};
var $author$project$AG2$MsgGotFile = function (a) {
	return {$: 13, a: a};
};
var $author$project$AG2$MsgGotString = function (a) {
	return {$: 14, a: a};
};
var $author$project$AG2$AST = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$AG2$Changed = 0;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = 1;
var $author$project$AG2$TailBrick = 2;
var $author$project$AG2$Unchanged = 1;
var $author$project$AG2$cor = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if ((_v0.a === 1) && (_v0.b === 1)) {
			var _v1 = _v0.a;
			var _v2 = _v0.b;
			return 1;
		} else {
			return 0;
		}
	});
var $author$project$AG2$andMap = F2(
	function (_v0, _v1) {
		var a = _v0.a;
		var c = _v0.b;
		var f = _v1.a;
		var d = _v1.b;
		return _Utils_Tuple2(
			f(a),
			A2($author$project$AG2$cor, c, d));
	});
var $author$project$AG2$mergin = 20;
var $author$project$AG2$insideBrick = F2(
	function (_v0, _v1) {
		var x0 = _v0.a;
		var y0 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		return (_Utils_cmp(y0 - $author$project$AG2$mergin, y) < 1) && ((_Utils_cmp(y, y0 + $author$project$AG2$mergin) < 1) && ((_Utils_cmp(x0 - $author$project$AG2$mergin, x) < 1) && (_Utils_cmp(x, x0 + $author$project$AG2$mergin) < 1)));
	});
var $author$project$AG2$interval = function (model) {
	return model.y * 0.9;
};
var $author$project$AG2$unit = function (a) {
	return _Utils_Tuple2(a, 1);
};
var $author$project$AG2$listMapW = function (f) {
	return A2(
		$elm$core$List$foldr,
		F2(
			function (a, mbs) {
				return A2(
					$author$project$AG2$andMap,
					mbs,
					A2(
						$author$project$AG2$andMap,
						f(a),
						$author$project$AG2$unit($elm$core$List$cons)));
			}),
		$author$project$AG2$unit(_List_Nil));
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Basics$not = _Basics_not;
var $author$project$AG2$addASTxy = F2(
	function (astxy, model) {
		return _Utils_update(
			model,
			{
				l: _Utils_ap(
					model.l,
					_List_fromArray(
						[astxy]))
			});
	});
var $author$project$AG2$comparePos = F4(
	function (_v0, t0, _v1, t) {
		var x0 = _v0.a;
		var y0 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		return (_Utils_cmp(y0 - 35, y) < 1) && ((_Utils_cmp(y, y0 + 35) < 1) && ((_Utils_cmp(x0 - 35, x) < 1) && ((_Utils_cmp(x, x0 + 35) < 1) && _Utils_eq(t0, t))));
	});
var $elm$core$List$map4 = _List_map4;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$AG2$fixPos = F3(
	function (_v0, t, listPT) {
		fixPos:
		while (true) {
			var x = _v0.a;
			var y = _v0.b;
			var _v1 = $elm$core$List$unzip(listPT);
			var listxy = _v1.a;
			var listTab = _v1.b;
			var tab = A2(
				$elm$core$List$repeat,
				$elm$core$List$length(listTab),
				t);
			var xy = A2(
				$elm$core$List$repeat,
				$elm$core$List$length(listxy),
				_Utils_Tuple2(x, y));
			var fixResult = A2(
				$elm$core$List$member,
				true,
				A5($elm$core$List$map4, $author$project$AG2$comparePos, xy, tab, listxy, listTab));
			if (fixResult) {
				var $temp$_v0 = _Utils_Tuple2(x + 35, y + 41),
					$temp$t = t,
					$temp$listPT = listPT;
				_v0 = $temp$_v0;
				t = $temp$t;
				listPT = $temp$listPT;
				continue fixPos;
			} else {
				return _Utils_Tuple2(x, y);
			}
		}
	});
var $author$project$AG2$pickPos = function (_v0) {
	var xy = _v0.a;
	var _v1 = _v0.b;
	var node = _v1.a;
	var bottom = _v1.b;
	var right = _v1.c;
	return _Utils_Tuple2(xy, node.a);
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$AG2$removeAST = F2(
	function (astxy, model) {
		return _Utils_update(
			model,
			{
				l: A2(
					$elm$core$List$filter,
					function (a) {
						return !_Utils_eq(a, astxy);
					},
					model.l)
			});
	});
var $author$project$AG2$rePos = F3(
	function (_v0, _new, model) {
		var xy = _v0.a;
		var _v1 = _v0.b;
		var node = _v1.a;
		var bottom = _v1.b;
		var right = _v1.c;
		if ((!model.g.q) && (!_new)) {
			return model;
		} else {
			var model2 = (!_new) ? A2(
				$author$project$AG2$removeAST,
				A2(
					$author$project$AG2$ASTxy,
					xy,
					A3($author$project$AG2$ASTne, node, bottom, right)),
				model) : model;
			var _v2 = xy;
			var pickX = _v2.a;
			var pickY = _v2.b;
			var x0 = (pickX < 400) ? 400 : pickX;
			var y0 = (pickY < 60) ? 60 : pickY;
			var xy0 = _Utils_Tuple2(x0, y0);
			return A2(
				$author$project$AG2$addASTxy,
				A2(
					$author$project$AG2$ASTxy,
					A3(
						$author$project$AG2$fixPos,
						xy0,
						node.a,
						A2($elm$core$List$map, $author$project$AG2$pickPos, model2.l)),
					A3($author$project$AG2$ASTne, node, bottom, right)),
				model2);
		}
	});
var $author$project$AG2$recurAttachAST = F5(
	function (d, f, u, _v0, ast) {
		var x = _v0.a;
		var y = _v0.b;
		if (!ast.$) {
			return u(
				_Utils_Tuple2(x, y));
		} else {
			var node = ast.a;
			var bottom = ast.b;
			var right = ast.c;
			return (node.e !== 2) ? A4(
				f,
				_Utils_Tuple2(x, y),
				node,
				A5(
					$author$project$AG2$recurAttachAST,
					d,
					f,
					u,
					_Utils_Tuple2(x, y + d),
					bottom),
				A5(
					$author$project$AG2$recurAttachAST,
					d,
					f,
					u,
					_Utils_Tuple2(x + d, y),
					right)) : $author$project$AG2$unit(
				A3($author$project$AG2$AST, node, bottom, right));
		}
	});
var $author$project$AG2$attachMe = F4(
	function (_v0, xy1, event, model) {
		var xy = _v0.a;
		var _v1 = _v0.b;
		var node = _v1.a;
		var bottom = _v1.b;
		var right = _v1.c;
		if ((event.M !== 1) || (!model.g.q)) {
			return model;
		} else {
			var u = function (xy0) {
				return A2($author$project$AG2$insideBrick, xy0, xy) ? _Utils_Tuple2(
					A3($author$project$AG2$AST, node, bottom, right),
					0) : $author$project$AG2$unit($author$project$AG2$Nil);
			};
			var t = node.a;
			var model1 = A2(
				$author$project$AG2$removeAST,
				A2(
					$author$project$AG2$ASTxy,
					xy1,
					A3($author$project$AG2$ASTne, node, bottom, right)),
				model);
			var f = F4(
				function (_v7, n, b, r) {
					return A2(
						$author$project$AG2$andMap,
						r,
						A2(
							$author$project$AG2$andMap,
							b,
							$author$project$AG2$unit(
								$author$project$AG2$AST(n))));
				});
			if (node.e === 1) {
				return A3(
					$author$project$AG2$rePos,
					A2(
						$author$project$AG2$ASTxy,
						xy,
						A3($author$project$AG2$ASTne, node, bottom, right)),
					false,
					model1);
			} else {
				var _v2 = event.x;
				var pickX = _v2.a;
				var pickY = _v2.b;
				var _v3 = A2(
					$author$project$AG2$listMapW,
					function (_v4) {
						var _v5 = _v4.a;
						var x = _v5.a;
						var y = _v5.b;
						var _v6 = _v4.b;
						var n = _v6.a;
						var b = _v6.b;
						var r = _v6.c;
						if ((n.e !== 2) && _Utils_eq(n.a, t)) {
							var d = $author$project$AG2$interval(model1);
							return A2(
								$author$project$AG2$andMap,
								A5(
									$author$project$AG2$recurAttachAST,
									d,
									f,
									u,
									_Utils_Tuple2(x + d, y),
									r),
								A2(
									$author$project$AG2$andMap,
									A5(
										$author$project$AG2$recurAttachAST,
										d,
										f,
										u,
										_Utils_Tuple2(x, y + d),
										b),
									$author$project$AG2$unit(
										F2(
											function (ab, ar) {
												return A2(
													$author$project$AG2$ASTxy,
													_Utils_Tuple2(x, y),
													A3($author$project$AG2$ASTne, n, ab, ar));
											}))));
						} else {
							return $author$project$AG2$unit(
								A2(
									$author$project$AG2$ASTxy,
									_Utils_Tuple2(x, y),
									A3($author$project$AG2$ASTne, n, b, r)));
						}
					},
					model1.l);
				var newRoots = _v3.a;
				var isChanged = _v3.b;
				var newModel = _Utils_update(
					model1,
					{l: newRoots});
				return ((!isChanged) || (pickX < 400)) ? A2(
					$author$project$AG2$removeAST,
					A2(
						$author$project$AG2$ASTxy,
						xy,
						A3($author$project$AG2$ASTne, node, bottom, right)),
					newModel) : A3(
					$author$project$AG2$rePos,
					A2(
						$author$project$AG2$ASTxy,
						xy,
						A3($author$project$AG2$ASTne, node, bottom, right)),
					false,
					newModel);
			}
		}
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$AG2$pairMap2 = F3(
	function (f, _v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2(
			A2(f, x1, x2),
			A2(f, y1, y2));
	});
var $author$project$AG2$cloneUs = F3(
	function (_v0, event, model) {
		var n = _v0.a;
		var b = _v0.b;
		var r = _v0.c;
		return ((event.M === 1) || (model.g.q || (n.e === 1))) ? model : A3(
			$author$project$AG2$rePos,
			A2(
				$author$project$AG2$ASTxy,
				A3(
					$author$project$AG2$pairMap2,
					$elm$core$Basics$add,
					A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.ad, event.ag),
					_Utils_Tuple2(30, 30)),
				A3($author$project$AG2$ASTne, n, b, r)),
			true,
			model);
	});
var $author$project$AG2$BasicBrick = 0;
var $author$project$AG2$CommandNone = 4;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $author$project$AG2$CommandAD = 8;
var $author$project$AG2$CommandAF = 5;
var $author$project$AG2$CommandAS = 6;
var $author$project$AG2$CommandAT = 7;
var $author$project$AG2$CommandCEA = 23;
var $author$project$AG2$CommandCEC = 26;
var $author$project$AG2$CommandCED = 24;
var $author$project$AG2$CommandCEF = 27;
var $author$project$AG2$CommandCEH = 29;
var $author$project$AG2$CommandCEM = 22;
var $author$project$AG2$CommandCEO = 25;
var $author$project$AG2$CommandCMH = 28;
var $author$project$AG2$CommandFuncStop = 2;
var $author$project$AG2$CommandMDJ = 13;
var $author$project$AG2$CommandML = 15;
var $author$project$AG2$CommandMLJ = 12;
var $author$project$AG2$CommandMR = 11;
var $author$project$AG2$CommandMS = 9;
var $author$project$AG2$CommandMT = 14;
var $author$project$AG2$CommandMW = 10;
var $author$project$AG2$CommandNOP = 0;
var $author$project$AG2$CommandReboot = 3;
var $author$project$AG2$CommandSA = 17;
var $author$project$AG2$CommandSC = 20;
var $author$project$AG2$CommandSD = 21;
var $author$project$AG2$CommandSM = 16;
var $author$project$AG2$CommandSO = 19;
var $author$project$AG2$CommandSP = 18;
var $author$project$AG2$decodeBrickCommand = function (c) {
	switch (c) {
		case 'CommandNOP':
			return 0;
		case 'CommandFuncStart':
			return 1;
		case 'CommandFuncStop':
			return 2;
		case 'CommandReboot':
			return 3;
		case 'CommandNone':
			return 4;
		case 'CommandAF':
			return 5;
		case 'CommandAS':
			return 6;
		case 'CommandAT':
			return 7;
		case 'CommandAD':
			return 8;
		case 'CommandMS':
			return 9;
		case 'CommandMW':
			return 10;
		case 'CommandMR':
			return 11;
		case 'CommandMLJ':
			return 12;
		case 'CommandMDJ':
			return 13;
		case 'CommandMT':
			return 14;
		case 'CommandML':
			return 15;
		case 'CommandSM':
			return 16;
		case 'CommandSA':
			return 17;
		case 'CommandSP':
			return 18;
		case 'CommandSO':
			return 19;
		case 'CommandSC':
			return 20;
		case 'CommandSD':
			return 21;
		case 'CommandCEM':
			return 22;
		case 'CommandCEA':
			return 23;
		case 'CommandCED':
			return 24;
		case 'CommandCEO':
			return 25;
		case 'CommandCEC':
			return 26;
		case 'CommandCEF':
			return 27;
		case 'CommandCMH':
			return 28;
		case 'CommandCEH':
			return 29;
		default:
			return 4;
	}
};
var $author$project$AG2$CaseBrick = 3;
var $author$project$AG2$decodeBrickType = function (t) {
	switch (t) {
		case 'EntryBrick':
			return 1;
		case 'BasicBrick':
			return 0;
		case 'TailBrick':
			return 2;
		case 'CaseBrick':
			return 3;
		default:
			return 0;
	}
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$AG2$decodeNode = function (node) {
	return {
		b: {
			c: function () {
				var _v0 = A2(
					$elm$json$Json$Decode$decodeValue,
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['getBrickArgument', 'disp']),
						$elm$json$Json$Decode$string),
					node);
				if (!_v0.$) {
					var d = _v0.a;
					return d;
				} else {
					return '';
				}
			}(),
			f: function () {
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['getBrickArgument', 'value']),
						$elm$json$Json$Decode$string),
					node);
				if (!_v1.$) {
					var v = _v1.a;
					return v;
				} else {
					return '';
				}
			}()
		},
		d: function () {
			var _v2 = A2(
				$elm$json$Json$Decode$decodeValue,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['getBrickCommand']),
					$elm$json$Json$Decode$string),
				node);
			if (!_v2.$) {
				var c = _v2.a;
				return $author$project$AG2$decodeBrickCommand(c);
			} else {
				return 4;
			}
		}(),
		a: function () {
			var _v3 = A2(
				$elm$json$Json$Decode$decodeValue,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['getBrickTab']),
					$elm$json$Json$Decode$int),
				node);
			if (!_v3.$) {
				var ta = _v3.a;
				return ta;
			} else {
				return 0;
			}
		}(),
		e: function () {
			var _v4 = A2(
				$elm$json$Json$Decode$decodeValue,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['getBrickType']),
					$elm$json$Json$Decode$string),
				node);
			if (!_v4.$) {
				var ty = _v4.a;
				return $author$project$AG2$decodeBrickType(ty);
			} else {
				return 0;
			}
		}()
	};
};
var $author$project$AG2$decodeAST = function (ast) {
	var right = function () {
		var _v3 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['right']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v3.$) {
			var r = _v3.a;
			return $author$project$AG2$decodeAST(r);
		} else {
			return $author$project$AG2$Nil;
		}
	}();
	var bottom = function () {
		var _v2 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['bottom']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v2.$) {
			var b = _v2.a;
			return $author$project$AG2$decodeAST(b);
		} else {
			return $author$project$AG2$Nil;
		}
	}();
	var _v0 = function () {
		var _v1 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['node']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v1.$) {
			var n = _v1.a;
			return _Utils_Tuple2(
				true,
				$author$project$AG2$decodeNode(n));
		} else {
			return _Utils_Tuple2(
				false,
				{
					b: {c: '', f: ''},
					d: 4,
					a: 0,
					e: 0
				});
		}
	}();
	var check = _v0.a;
	var node = _v0.b;
	return check ? A3($author$project$AG2$AST, node, bottom, right) : $author$project$AG2$Nil;
};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$AG2$decodeASTxy = function (ast) {
	var right = function () {
		var _v4 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['ASTne', 'right']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v4.$) {
			var r = _v4.a;
			return $author$project$AG2$decodeAST(r);
		} else {
			return $author$project$AG2$Nil;
		}
	}();
	var posY = function () {
		var _v3 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['position', 'y']),
				$elm$json$Json$Decode$float),
			ast);
		if (!_v3.$) {
			var y = _v3.a;
			return y;
		} else {
			return 0;
		}
	}();
	var posX = function () {
		var _v2 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['position', 'x']),
				$elm$json$Json$Decode$float),
			ast);
		if (!_v2.$) {
			var x = _v2.a;
			return x;
		} else {
			return 0;
		}
	}();
	var node = function () {
		var _v1 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['ASTne', 'node']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v1.$) {
			var n = _v1.a;
			return $author$project$AG2$decodeNode(n);
		} else {
			return {
				b: {c: '', f: ''},
				d: 4,
				a: 0,
				e: 0
			};
		}
	}();
	var bottom = function () {
		var _v0 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['ASTne', 'bottom']),
				$elm$json$Json$Decode$value),
			ast);
		if (!_v0.$) {
			var b = _v0.a;
			return $author$project$AG2$decodeAST(b);
		} else {
			return $author$project$AG2$Nil;
		}
	}();
	return A2(
		$author$project$AG2$ASTxy,
		_Utils_Tuple2(posX, posY),
		A3($author$project$AG2$ASTne, node, bottom, right));
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$AG2$encodeBrickCommand = function (bc) {
	return $elm$json$Json$Encode$string(
		function () {
			switch (bc) {
				case 0:
					return 'CommandNOP';
				case 1:
					return 'CommandFuncStart';
				case 2:
					return 'CommandFuncStop';
				case 3:
					return 'CommandReboot';
				case 4:
					return 'CommandNone';
				case 5:
					return 'CommandAF';
				case 6:
					return 'CommandAS';
				case 7:
					return 'CommandAT';
				case 8:
					return 'CommandAD';
				case 9:
					return 'CommandMS';
				case 10:
					return 'CommandMW';
				case 11:
					return 'CommandMR';
				case 12:
					return 'CommandMLJ';
				case 13:
					return 'CommandMDJ';
				case 14:
					return 'CommandMT';
				case 15:
					return 'CommandML';
				case 16:
					return 'CommandSM';
				case 17:
					return 'CommandSA';
				case 18:
					return 'CommandSP';
				case 19:
					return 'CommandSO';
				case 20:
					return 'CommandSC';
				case 21:
					return 'CommandSD';
				case 22:
					return 'CommandCEM';
				case 23:
					return 'CommandCEA';
				case 24:
					return 'CommandCED';
				case 25:
					return 'CommandCEO';
				case 26:
					return 'CommandCEC';
				case 27:
					return 'CommandCEF';
				case 28:
					return 'CommandCMH';
				default:
					return 'CommandCEH';
			}
		}());
};
var $author$project$AG2$encodeBrickType = function (bt) {
	return $elm$json$Json$Encode$string(
		function () {
			switch (bt) {
				case 1:
					return 'EntryBrick';
				case 0:
					return 'BasicBrick';
				case 2:
					return 'TailBrick';
				default:
					return 'CaseBrick';
			}
		}());
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $author$project$AG2$encodeAST = function (ast) {
	if (!ast.$) {
		return $elm$json$Json$Encode$string('Nil');
	} else {
		var node = ast.a;
		var bottom = ast.b;
		var right = ast.c;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'node',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'getBrickType',
								$author$project$AG2$encodeBrickType(node.e)),
								_Utils_Tuple2(
								'getBrickCommand',
								$author$project$AG2$encodeBrickCommand(node.d)),
								_Utils_Tuple2(
								'getBrickArgument',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'disp',
											$elm$json$Json$Encode$string(node.b.c)),
											_Utils_Tuple2(
											'value',
											$elm$json$Json$Encode$string(node.b.f))
										]))),
								_Utils_Tuple2(
								'getBrickTab',
								$elm$json$Json$Encode$int(node.a))
							]))),
					_Utils_Tuple2(
					'bottom',
					$author$project$AG2$encodeAST(bottom)),
					_Utils_Tuple2(
					'right',
					$author$project$AG2$encodeAST(right))
				]));
	}
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$AG2$encodeASTxy = function (_v0) {
	var _v1 = _v0.a;
	var posx = _v1.a;
	var posy = _v1.b;
	var _v2 = _v0.b;
	var node = _v2.a;
	var astb = _v2.b;
	var astr = _v2.c;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'position',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'x',
							$elm$json$Json$Encode$float(posx)),
							_Utils_Tuple2(
							'y',
							$elm$json$Json$Encode$float(posy))
						]))),
				_Utils_Tuple2(
				'ASTne',
				$author$project$AG2$encodeAST(
					A3($author$project$AG2$AST, node, astb, astr)))
			]));
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$AG2$encodeASTRoots = function (model) {
	return A2(
		$elm$json$Json$Encode$list,
		function (a) {
			return a;
		},
		A2($elm$core$List$map, $author$project$AG2$encodeASTxy, model.l));
};
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $author$project$AG2$inputValue = F4(
	function (argCode, checkCode, value, ast) {
		if (!ast.$) {
			return $author$project$AG2$Nil;
		} else {
			var n = ast.a;
			var b = ast.b;
			var r = ast.c;
			return _Utils_eq(argCode, checkCode) ? A3(
				$author$project$AG2$AST,
				_Utils_update(
					n,
					{
						b: {c: n.b.c, f: value}
					}),
				b,
				r) : A3(
				$author$project$AG2$AST,
				n,
				A4($author$project$AG2$inputValue, argCode, checkCode + 'b', value, b),
				A4($author$project$AG2$inputValue, argCode, checkCode + 'r', value, r));
		}
	});
var $author$project$AG2$inputValuexy = F4(
	function (xy, argCode, value, model) {
		return _Utils_update(
			model,
			{
				l: A2(
					$elm$core$List$map,
					function (_v0) {
						var p = _v0.a;
						var _v1 = _v0.b;
						var n = _v1.a;
						var b = _v1.b;
						var r = _v1.c;
						return (_Utils_eq(p, xy) && _Utils_eq(n.a, model.A)) ? ($elm$core$String$isEmpty(argCode) ? A2(
							$author$project$AG2$ASTxy,
							p,
							A3(
								$author$project$AG2$ASTne,
								_Utils_update(
									n,
									{
										b: {c: n.b.c, f: value}
									}),
								b,
								r)) : A2(
							$author$project$AG2$ASTxy,
							p,
							A3(
								$author$project$AG2$ASTne,
								n,
								A4($author$project$AG2$inputValue, argCode, 'b', value, b),
								A4($author$project$AG2$inputValue, argCode, 'r', value, r)))) : A2(
							$author$project$AG2$ASTxy,
							p,
							A3($author$project$AG2$ASTne, n, b, r));
					},
					model.l)
			});
	});
var $author$project$AG2$recurAST = F5(
	function (d, f, u, _v0, ast) {
		var x = _v0.a;
		var y = _v0.b;
		if (!ast.$) {
			return u(
				_Utils_Tuple2(x, y));
		} else {
			var node = ast.a;
			var bottom = ast.b;
			var right = ast.c;
			return A4(
				f,
				_Utils_Tuple2(x, y),
				node,
				A5(
					$author$project$AG2$recurAST,
					d,
					f,
					u,
					_Utils_Tuple2(x, y + d),
					bottom),
				A5(
					$author$project$AG2$recurAST,
					d,
					f,
					u,
					_Utils_Tuple2(x + d, y),
					right));
		}
	});
var $author$project$AG2$letMeRoot = F3(
	function (_v0, event, model) {
		var node = _v0.a;
		var bottom = _v0.b;
		var right = _v0.c;
		if ((event.M !== 1) || model.g.q) {
			return _Utils_Tuple3(
				model,
				A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.ad, event.ag),
				node.a);
		} else {
			var xy = A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.ad, event.ag);
			var t = node.a;
			var f = F4(
				function (xy0, n, b, r) {
					return (A2($author$project$AG2$insideBrick, xy0, xy) && _Utils_eq(n.a, t)) ? $author$project$AG2$Nil : A3($author$project$AG2$AST, n, b, r);
				});
			return _Utils_Tuple3(
				A2(
					$author$project$AG2$addASTxy,
					A2(
						$author$project$AG2$ASTxy,
						xy,
						A3($author$project$AG2$ASTne, node, bottom, right)),
					_Utils_update(
						model,
						{
							l: A2(
								$elm$core$List$map,
								function (_v1) {
									var _v2 = _v1.a;
									var x = _v2.a;
									var y = _v2.b;
									var _v3 = _v1.b;
									var n = _v3.a;
									var b = _v3.b;
									var r = _v3.c;
									var d = $author$project$AG2$interval(model);
									return A2(
										$author$project$AG2$ASTxy,
										_Utils_Tuple2(x, y),
										A3(
											$author$project$AG2$ASTne,
											n,
											A5(
												$author$project$AG2$recurAST,
												d,
												f,
												function (_v4) {
													return $author$project$AG2$Nil;
												},
												_Utils_Tuple2(x, y + d),
												b),
											A5(
												$author$project$AG2$recurAST,
												d,
												f,
												function (_v5) {
													return $author$project$AG2$Nil;
												},
												_Utils_Tuple2(x + d, y),
												r)));
								},
								model.l)
						})),
				xy,
				node.a);
		}
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$AG2$moveUs = F2(
	function (event, model) {
		return (!model.g.q) ? model : _Utils_update(
			model,
			{
				g: {
					a: model.g.a,
					C: model.g.C,
					I: A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.x, model.g.D),
					q: model.g.q,
					D: model.g.D
				}
			});
	});
var $author$project$AG2$sendMes = _Platform_outgoingPort('sendMes', $elm$core$Basics$identity);
var $author$project$AG2$startDnD = F2(
	function (event, _v0) {
		var model = _v0.a;
		var xy = _v0.b;
		var t = _v0.c;
		return (event.M !== 1) ? model : _Utils_update(
			model,
			{
				g: {
					a: t,
					C: xy,
					I: xy,
					q: true,
					D: A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.x, xy)
				}
			});
	});
var $author$project$AG2$startDnDxy = F3(
	function (_v0, event, model) {
		var xy = _v0.a;
		var _v1 = _v0.b;
		var node = _v1.a;
		var bottom = _v1.b;
		var right = _v1.c;
		return (event.M !== 1) ? model : A2(
			$author$project$AG2$addASTxy,
			A2(
				$author$project$AG2$ASTxy,
				xy,
				A3(
					$author$project$AG2$ASTne,
					_Utils_update(
						node,
						{a: model.A}),
					bottom,
					right)),
			A2(
				$author$project$AG2$removeAST,
				A2(
					$author$project$AG2$ASTxy,
					xy,
					A3($author$project$AG2$ASTne, node, bottom, right)),
				_Utils_update(
					model,
					{
						g: {
							a: node.a,
							C: xy,
							I: xy,
							q: true,
							D: A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.x, xy)
						}
					})));
	});
var $author$project$AG2$stopDnD = F2(
	function (event, model) {
		return (event.M !== 1) ? model : _Utils_update(
			model,
			{
				g: {
					a: -1,
					C: _Utils_Tuple2(0, 0),
					I: _Utils_Tuple2(0, 0),
					q: false,
					D: _Utils_Tuple2(0, 0)
				}
			});
	});
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $elm$file$File$toString = _File_toString;
var $author$project$AG2$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var ast = msg.a;
				var event = msg.b;
				return _Utils_Tuple2(
					A3($author$project$AG2$cloneUs, ast, event, model),
					$elm$core$Platform$Cmd$none);
			case 4:
				var _v1 = msg.a;
				var xy = _v1.a;
				var _v2 = _v1.b;
				var n = _v2.a;
				var b = _v2.b;
				var r = _v2.c;
				var event = msg.b;
				return ((n.e === 1) || model.g.q) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					A3(
						$author$project$AG2$startDnDxy,
						A2(
							$author$project$AG2$ASTxy,
							xy,
							A3($author$project$AG2$ASTne, n, b, r)),
						event,
						model),
					$elm$core$Platform$Cmd$none);
			case 1:
				var ast = msg.a;
				var xy = msg.b;
				var event = msg.c;
				return _Utils_Tuple2(
					A2(
						$author$project$AG2$startDnD,
						event,
						A3($author$project$AG2$letMeRoot, ast, event, model)),
					$elm$core$Platform$Cmd$none);
			case 2:
				var event = msg.a;
				return _Utils_Tuple2(
					A2(
						$author$project$AG2$moveUs,
						event,
						_Utils_update(
							model,
							{
								x: A3($author$project$AG2$pairMap2, $elm$core$Basics$sub, event.ad, event.x)
							})),
					$elm$core$Platform$Cmd$none);
			case 3:
				var astxy = msg.a;
				var xy1 = msg.b;
				var event = msg.c;
				return _Utils_Tuple2(
					A2(
						$author$project$AG2$stopDnD,
						event,
						A4($author$project$AG2$attachMe, astxy, xy1, event, model)),
					$elm$core$Platform$Cmd$none);
			case 5:
				var xy = msg.a;
				var argCode = msg.b;
				var value = msg.c;
				return _Utils_Tuple2(
					A4($author$project$AG2$inputValuexy, xy, argCode, value, model),
					$elm$core$Platform$Cmd$none);
			case 6:
				var event = msg.a;
				return _Utils_Tuple2(
					A2($author$project$AG2$stopDnD, event, model),
					$elm$core$Platform$Cmd$none);
			case 7:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 8:
				return _Utils_Tuple2(
					model,
					$author$project$AG2$sendMes(
						A2(
							$elm$json$Json$Encode$list,
							function (a) {
								return a;
							},
							_List_fromArray(
								[
									$elm$json$Json$Encode$int(model.A + 1),
									$author$project$AG2$encodeASTRoots(model)
								]))));
			case 9:
				var command = msg.a;
				var _v3 = A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, command);
				if (!_v3.$) {
					var fileName = _v3.a;
					return _Utils_Tuple2(
						model,
						A3(
							$elm$file$File$Download$string,
							fileName,
							'text/json',
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$author$project$AG2$encodeASTRoots(model))));
				} else {
					var _v4 = A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$bool, command);
					if ((!_v4.$) && _v4.a) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{l: $author$project$AG2$setBrick}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				}
			case 10:
				return _Utils_Tuple2(
					model,
					$author$project$AG2$sendMes(
						$elm$json$Json$Encode$string('Reset')));
			case 11:
				var num = msg.a;
				var te = msg.b;
				return te ? _Utils_Tuple2(
					_Utils_update(
						model,
						{S: num}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{A: num}),
					$elm$core$Platform$Cmd$none);
			case 12:
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['text/json']),
						$author$project$AG2$MsgGotFile));
			case 13:
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$AG2$MsgGotString,
						$elm$file$File$toString(file)));
			case 14:
				var str = msg.a;
				var _v5 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					str);
				if (!_v5.$) {
					var jsonData = _v5.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								l: A2($elm$core$List$map, $author$project$AG2$decodeASTxy, jsonData)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(
					model,
					$author$project$AG2$sendMes(
						$elm$json$Json$Encode$string('Save')));
		}
	});
var $author$project$AG2$Encoding = {$: 8};
var $author$project$AG2$MsgCheckReset = {$: 10};
var $author$project$AG2$MsgLoadFile = {$: 12};
var $author$project$AG2$MsgMoveUs = function (a) {
	return {$: 2, a: a};
};
var $author$project$AG2$MsgSaveFile = {$: 15};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 3, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {M: button, x: clientPos, bg: keys, ag: offsetPos, ad: pagePos, bt: screenPos};
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = 4;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = 0;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = 5;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = 2;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = 3;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		default:
			return 0;
	}
};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	$elm$json$Json$Decode$map,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2($elm$json$Json$Decode$field, 'button', $elm$json$Json$Decode$int));
var $mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$Keys = F3(
	function (alt, ctrl, shift) {
		return {aW: alt, a3: ctrl, bu: shift};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $mpizenberg$elm_pointer_events$Internal$Decode$keys = A4(
	$elm$json$Json$Decode$map3,
	$mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$map6 = _Json_map6;
var $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'screenX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'screenY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7($elm$json$Json$Decode$map6, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, $mpizenberg$elm_pointer_events$Internal$Decode$keys, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, $mpizenberg$elm_pointer_events$Internal$Decode$clientPos, $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, $mpizenberg$elm_pointer_events$Internal$Decode$pagePos, $mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			$elm$html$Html$Events$custom,
			event,
			A2(
				$elm$json$Json$Decode$map,
				function (ev) {
					return {
						E: tag(ev),
						br: options.br,
						bw: options.bw
					};
				},
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder));
	});
var $author$project$AG2$on = function (eventName) {
	return A2(
		$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
		eventName,
		{
			br: ((eventName === 'contextmenu') || (eventName === 'mousemove')) ? true : false,
			bw: true
		});
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$AG2$setPallet = function () {
	var y = 100;
	var x = 80;
	return _List_fromArray(
		[
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 5,
					a: 0,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 6,
					a: 0,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 7,
					a: 0,
					e: 2
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 8,
					a: 0,
					e: 2
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 9,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 10,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 11,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 12,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 13,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 450),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 14,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 450),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 15,
					a: 1,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 16,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 17,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 19,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 20,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '?????????', f: '1'},
					d: 18,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 21,
					a: 2,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 22,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 23,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 24,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 25,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '?????????', f: '1'},
					d: 26,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '?????????', f: '1'},
					d: 27,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 450),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '??????', f: '100'},
					d: 28,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x + 150, y + 450),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '??????', f: '100'},
					d: 29,
					a: 3,
					e: 3
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 3,
					a: 4,
					e: 2
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 150),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: 'n = ', f: ''},
					d: 2,
					a: 4,
					e: 2
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil)),
			A2(
			$author$project$AG2$ASTxy,
			_Utils_Tuple2(x, y + 300),
			A3(
				$author$project$AG2$ASTne,
				{
					b: {c: '', f: ''},
					d: 4,
					a: 4,
					e: 0
				},
				$author$project$AG2$Nil,
				$author$project$AG2$Nil))
		]);
}();
var $author$project$AG2$setTabE = _List_fromArray(
	[
		{n: '#00ffff', i: 0, o: '??????1'},
		{n: '#ffff00', i: 1, o: '??????2'},
		{n: '#ff00ff', i: 2, o: '??????3'},
		{n: '#00ffff', i: 3, o: '??????4'},
		{n: '#ffff00', i: 4, o: '??????5'},
		{n: '#ff00ff', i: 5, o: '??????6'},
		{n: '#00ffff', i: 6, o: '??????7'},
		{n: '#ffff00', i: 7, o: '??????8'},
		{n: '#ff00ff', i: 8, o: '??????9'}
	]);
var $author$project$AG2$setTabP = _List_fromArray(
	[
		{n: '#ff0000', i: 0, o: '??????'},
		{n: '#ffff00', i: 1, o: '??????'},
		{n: '#00ffff', i: 2, o: '??????'},
		{n: '#00ff00', i: 3, o: '??????'},
		{n: '#ff00ff', i: 4, o: '?????????'}
	]);
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$AG2$MsgAttachMe = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$AG2$MsgCloneUs = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$AG2$MsgStartDnD = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$AG2$Dummy = function (a) {
	return {$: 6, a: a};
};
var $author$project$AG2$MsgInputChanged = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $author$project$AG2$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $author$project$AG2$viewCommand = function (command) {
	switch (command) {
		case 0:
			return '????????????';
		case 1:
			return '??????n?????????';
		case 2:
			return '??????n?????????';
		case 3:
			return '??????????????????';
		case 4:
			return '??????????????????';
		case 5:
			return '?????????1';
		case 6:
			return '?????????2';
		case 7:
			return '?????????3';
		case 8:
			return '????????????';
		case 9:
			return '???????????????';
		case 10:
			return '???????????????';
		case 11:
			return '???????????????';
		case 12:
			return '???????????????';
		case 13:
			return '???????????????';
		case 14:
			return '???????????????';
		case 15:
			return '?????????????????????';
		case 16:
			return '?????????????????????';
		case 17:
			return '?????????????????????';
		case 18:
			return '?????????????????????';
		case 19:
			return '???????????????';
		case 20:
			return '???????????????';
		case 21:
			return '?????????????????????';
		case 22:
			return '??????????????????';
		case 23:
			return '??????????????????';
		case 24:
			return '??????????????????';
		case 25:
			return '????????????????????????';
		case 26:
			return '???????????????';
		case 27:
			return '???????????????';
		case 28:
			return '?????????????????????';
		default:
			return '?????????????????????';
	}
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$AG2$basicBrickSvg = F5(
	function (size, command, argument, argCode, xy) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$svg,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$viewBox('166 70 336 336')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z'),
									$elm$svg$Svg$Attributes$stroke('gray'),
									$elm$svg$Svg$Attributes$fill('yellow'),
									$elm$svg$Svg$Attributes$strokeWidth('6')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$svg,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$x('166'),
									$elm$svg$Svg$Attributes$y('70'),
									$elm$svg$Svg$Attributes$viewBox('0 0 135 50')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('15'),
											$elm$svg$Svg$Attributes$y('0'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(
											$author$project$AG2$viewCommand(command))
										])),
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('20'),
											$elm$svg$Svg$Attributes$y('25'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(argument.c)
										]))
								]))
						])),
					(!$elm$core$String$isEmpty(argument.c)) ? A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$placeholder('??????'),
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'top', '50px'),
							A2($elm$html$Html$Attributes$style, 'left', '50px'),
							A2($elm$html$Html$Attributes$style, 'width', '40px'),
							$elm$html$Html$Attributes$value(argument.f),
							A2(
							$author$project$AG2$on,
							'mouseup',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$author$project$AG2$on,
							'mousedown',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$elm$html$Html$Events$stopPropagationOn,
							'input',
							A2(
								$elm$json$Json$Decode$map,
								$author$project$AG2$alwaysStop,
								A2(
									$elm$json$Json$Decode$map,
									A2($author$project$AG2$MsgInputChanged, xy, argCode),
									$elm$html$Html$Events$targetValue)))
						]),
					_List_Nil) : A2($elm$html$Html$div, _List_Nil, _List_Nil)
				]));
	});
var $author$project$AG2$caseBrickSvg = F5(
	function (size, command, argument, argCode, xy) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$svg,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$viewBox('166 70 336 336')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z'),
									$elm$svg$Svg$Attributes$stroke('gray'),
									$elm$svg$Svg$Attributes$fill('limegreen'),
									$elm$svg$Svg$Attributes$strokeWidth('6')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$svg,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$x('166'),
									$elm$svg$Svg$Attributes$y('70'),
									$elm$svg$Svg$Attributes$viewBox('0 0 135 50')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('15'),
											$elm$svg$Svg$Attributes$y('0'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(
											$author$project$AG2$viewCommand(command))
										])),
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('70'),
											$elm$svg$Svg$Attributes$y('25'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(argument.c)
										]))
								]))
						])),
					(!$elm$core$String$isEmpty(argument.c)) ? A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$placeholder('??????'),
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'top', '50px'),
							A2($elm$html$Html$Attributes$style, 'left', '20px'),
							A2($elm$html$Html$Attributes$style, 'width', '40px'),
							$elm$html$Html$Attributes$value(argument.f),
							A2(
							$author$project$AG2$on,
							'mouseup',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$author$project$AG2$on,
							'mousedown',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$elm$html$Html$Events$stopPropagationOn,
							'input',
							A2(
								$elm$json$Json$Decode$map,
								$author$project$AG2$alwaysStop,
								A2(
									$elm$json$Json$Decode$map,
									A2($author$project$AG2$MsgInputChanged, xy, argCode),
									$elm$html$Html$Events$targetValue)))
						]),
					_List_Nil) : A2($elm$html$Html$div, _List_Nil, _List_Nil)
				]));
	});
var $author$project$AG2$entryBrickSvg = F5(
	function (size, command, argument, argCode, xy) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$svg,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$viewBox('166 70 336 336')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100  L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632  L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z'),
									$elm$svg$Svg$Attributes$stroke('gray'),
									$elm$svg$Svg$Attributes$fill('skyblue'),
									$elm$svg$Svg$Attributes$strokeWidth('6')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$svg,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$x('166'),
									$elm$svg$Svg$Attributes$y('70'),
									$elm$svg$Svg$Attributes$viewBox('0 0 135 50')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('15'),
											$elm$svg$Svg$Attributes$y('0'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(
											$author$project$AG2$viewCommand(command))
										])),
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('20'),
											$elm$svg$Svg$Attributes$y('25'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(argument.c)
										]))
								]))
						])),
					(!$elm$core$String$isEmpty(argument.c)) ? A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$placeholder('??????'),
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'top', '50px'),
							A2($elm$html$Html$Attributes$style, 'left', '50px'),
							A2($elm$html$Html$Attributes$style, 'width', '40px'),
							$elm$html$Html$Attributes$value(argument.f),
							A2(
							$author$project$AG2$on,
							'mouseup',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$author$project$AG2$on,
							'mousedown',
							function (event) {
								return $author$project$AG2$Dummy(event);
							})
						]),
					_List_Nil) : A2($elm$html$Html$div, _List_Nil, _List_Nil)
				]));
	});
var $elm$virtual_dom$VirtualDom$lazy5 = _VirtualDom_lazy5;
var $elm$html$Html$Lazy$lazy5 = $elm$virtual_dom$VirtualDom$lazy5;
var $author$project$AG2$tailBrickSvg = F5(
	function (size, command, argument, argCode, xy) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$svg,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(size)),
							$elm$svg$Svg$Attributes$viewBox('166 70 336 336')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 Z'),
									$elm$svg$Svg$Attributes$stroke('gray'),
									$elm$svg$Svg$Attributes$fill('pink'),
									$elm$svg$Svg$Attributes$strokeWidth('6')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$svg,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$x('166'),
									$elm$svg$Svg$Attributes$y('70'),
									$elm$svg$Svg$Attributes$viewBox('0 0 135 50')
								]),
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('15'),
											$elm$svg$Svg$Attributes$y('0'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(
											$author$project$AG2$viewCommand(command))
										])),
									A2(
									$elm$svg$Svg$text_,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('20'),
											$elm$svg$Svg$Attributes$y('25'),
											$elm$svg$Svg$Attributes$fill('black')
										]),
									_List_fromArray(
										[
											$elm$svg$Svg$text(argument.c)
										]))
								]))
						])),
					(!$elm$core$String$isEmpty(argument.c)) ? A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$placeholder('??????'),
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'top', '50px'),
							A2($elm$html$Html$Attributes$style, 'left', '50px'),
							A2($elm$html$Html$Attributes$style, 'width', '40px'),
							$elm$html$Html$Attributes$value(argument.f),
							A2(
							$author$project$AG2$on,
							'mouseup',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$author$project$AG2$on,
							'mousedown',
							function (event) {
								return $author$project$AG2$Dummy(event);
							}),
							A2(
							$elm$html$Html$Events$stopPropagationOn,
							'input',
							A2(
								$elm$json$Json$Decode$map,
								$author$project$AG2$alwaysStop,
								A2(
									$elm$json$Json$Decode$map,
									A2($author$project$AG2$MsgInputChanged, xy, argCode),
									$elm$html$Html$Events$targetValue)))
						]),
					_List_Nil) : A2($elm$html$Html$div, _List_Nil, _List_Nil)
				]));
	});
var $author$project$AG2$MsgLetMeRoot = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$AG2$viewAST = F5(
	function (size, isRight, ast, argCode, xy) {
		if (!ast.$) {
			return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		} else {
			var node = ast.a;
			var b = ast.b;
			var r = ast.c;
			return A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2(
						$elm$html$Html$Attributes$style,
						'top',
						(isRight ? '0' : $elm$core$String$fromFloat(size * 0.9)) + 'px'),
						A2(
						$elm$html$Html$Attributes$style,
						'left',
						(isRight ? $elm$core$String$fromFloat(size * 0.9) : '0') + 'px'),
						A2(
						$author$project$AG2$on,
						'contextmenu',
						function (event) {
							return A2(
								$author$project$AG2$MsgCloneUs,
								A3($author$project$AG2$ASTne, node, b, r),
								event);
						}),
						A2(
						$author$project$AG2$on,
						'mousedown',
						function (event) {
							return A3(
								$author$project$AG2$MsgLetMeRoot,
								A3($author$project$AG2$ASTne, node, b, r),
								xy,
								event);
						})
					]),
				_List_fromArray(
					[
						_Utils_Tuple2(
						'N',
						function () {
							var _v1 = node.e;
							switch (_v1) {
								case 0:
									return A5($author$project$AG2$basicBrickSvg, size, node.d, node.b, argCode, xy);
								case 1:
									return A5($author$project$AG2$entryBrickSvg, size, node.d, node.b, argCode, xy);
								case 2:
									return A5($author$project$AG2$tailBrickSvg, size, node.d, node.b, argCode, xy);
								default:
									return A5($author$project$AG2$caseBrickSvg, size, node.d, node.b, argCode, xy);
							}
						}()),
						_Utils_Tuple2(
						'R',
						A6($elm$html$Html$Lazy$lazy5, $author$project$AG2$viewAST, size, true, r, argCode + 'r', xy)),
						_Utils_Tuple2(
						'B',
						A6($elm$html$Html$Lazy$lazy5, $author$project$AG2$viewAST, size, false, b, argCode + 'b', xy))
					]));
		}
	});
var $author$project$AG2$viewASTxy = F3(
	function (model, te, _v0) {
		var _v1 = _v0.a;
		var x1 = _v1.a;
		var y1 = _v1.b;
		var _v2 = _v0.b;
		var n = _v2.a;
		var b = _v2.b;
		var r = _v2.c;
		var _v3 = _Utils_eq(
			_Utils_Tuple2(x1, y1),
			model.g.C) ? model.g.I : _Utils_Tuple2(x1, y1);
		var x = _v3.a;
		var y = _v3.b;
		return ((!te) && (!_Utils_eq(model.A, n.a))) ? A3($elm$html$Html$Keyed$node, 'div', _List_Nil, _List_Nil) : ((te && (!_Utils_eq(model.S, n.a))) ? A3($elm$html$Html$Keyed$node, 'div', _List_Nil, _List_Nil) : A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromFloat(y) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromFloat(x) + 'px'),
					A2(
					$author$project$AG2$on,
					'mouseup',
					function (event) {
						return A3(
							$author$project$AG2$MsgAttachMe,
							A2(
								$author$project$AG2$ASTxy,
								_Utils_Tuple2(x, y),
								A3($author$project$AG2$ASTne, n, b, r)),
							_Utils_Tuple2(x1, y1),
							event);
					}),
					A2(
					$author$project$AG2$on,
					'contextmenu',
					function (event) {
						return A2(
							$author$project$AG2$MsgCloneUs,
							A3($author$project$AG2$ASTne, n, b, r),
							event);
					}),
					A2(
					$author$project$AG2$on,
					'mousedown',
					function (event) {
						return A2(
							$author$project$AG2$MsgStartDnD,
							A2(
								$author$project$AG2$ASTxy,
								_Utils_Tuple2(x, y),
								A3($author$project$AG2$ASTne, n, b, r)),
							event);
					})
				]),
			_List_fromArray(
				[
					_Utils_Tuple2(
					'N',
					function () {
						var _v4 = n.e;
						switch (_v4) {
							case 0:
								return A5(
									$author$project$AG2$basicBrickSvg,
									model.y,
									n.d,
									n.b,
									'',
									_Utils_Tuple2(x, y));
							case 1:
								return A5(
									$author$project$AG2$entryBrickSvg,
									model.y,
									n.d,
									n.b,
									'',
									_Utils_Tuple2(x, y));
							case 2:
								return A5(
									$author$project$AG2$tailBrickSvg,
									model.y,
									n.d,
									n.b,
									'',
									_Utils_Tuple2(x, y));
							default:
								return A5(
									$author$project$AG2$caseBrickSvg,
									model.y,
									n.d,
									n.b,
									'',
									_Utils_Tuple2(x, y));
						}
					}()),
					_Utils_Tuple2(
					'R',
					A6(
						$elm$html$Html$Lazy$lazy5,
						$author$project$AG2$viewAST,
						model.y,
						true,
						r,
						'r',
						_Utils_Tuple2(x, y))),
					_Utils_Tuple2(
					'B',
					A6(
						$elm$html$Html$Lazy$lazy5,
						$author$project$AG2$viewAST,
						model.y,
						false,
						b,
						'b',
						_Utils_Tuple2(x, y)))
				])));
	});
var $author$project$AG2$MsgChangeTab = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $author$project$AG2$viewTab = F3(
	function (tab, now, te) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromInt(
						te ? ((tab.i * 53) + 100) : 0) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromInt(
						te ? 0 : ((tab.i * 53) + 550)) + 'px'),
					A2($elm$html$Html$Attributes$style, 'width', '50px'),
					A2($elm$html$Html$Attributes$style, 'height', '50px'),
					A2(
					$elm$html$Html$Attributes$style,
					'background',
					_Utils_eq(tab.i, now) ? tab.n : '#c0c0c0'),
					A2(
					$author$project$AG2$on,
					'click',
					function (event) {
						return A2($author$project$AG2$MsgChangeTab, tab.i, te);
					})
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(tab.o)
				]));
	});
var $author$project$AG2$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'relative'),
				A2($elm$html$Html$Attributes$style, 'width', '100vh'),
				A2($elm$html$Html$Attributes$style, 'height', '100vh'),
				A2(
				$author$project$AG2$on,
				'mousemove',
				function (event) {
					return $author$project$AG2$MsgMoveUs(event);
				})
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2($elm$html$Html$Attributes$style, 'top', '0px'),
						A2($elm$html$Html$Attributes$style, 'left', '0px'),
						A2($elm$html$Html$Attributes$style, 'width', '400px'),
						A2($elm$html$Html$Attributes$style, 'height', '100vh'),
						A2($elm$html$Html$Attributes$style, 'background', '#f0f0f0')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2($elm$html$Html$Attributes$style, 'top', '0px'),
						A2($elm$html$Html$Attributes$style, 'left', '0px'),
						$elm$html$Html$Events$onClick($author$project$AG2$MsgLoadFile)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('???????????????????????????')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2($elm$html$Html$Attributes$style, 'top', '26px'),
						A2($elm$html$Html$Attributes$style, 'left', '0px'),
						$elm$html$Html$Events$onClick($author$project$AG2$MsgSaveFile)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('?????????????????????')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2($elm$html$Html$Attributes$style, 'top', '52px'),
						A2($elm$html$Html$Attributes$style, 'left', '0px'),
						$elm$html$Html$Events$onClick($author$project$AG2$MsgCheckReset)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('?????????????????????')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
						A2($elm$html$Html$Attributes$style, 'top', '0px'),
						A2($elm$html$Html$Attributes$style, 'left', '400px'),
						$elm$html$Html$Events$onClick($author$project$AG2$Encoding)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('?????????????????????')
					])),
				A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (index, tab) {
							return _Utils_Tuple2(
								$elm$core$String$fromInt(index),
								A3($author$project$AG2$viewTab, tab, model.S, true));
						}),
					$author$project$AG2$setTabP)),
				A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (index, tab) {
							return _Utils_Tuple2(
								$elm$core$String$fromInt(index),
								A3($author$project$AG2$viewTab, tab, model.A, false));
						}),
					$author$project$AG2$setTabE)),
				A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (index, astxy) {
							return _Utils_Tuple2(
								$elm$core$String$fromInt(index),
								A3($author$project$AG2$viewASTxy, model, true, astxy));
						}),
					$author$project$AG2$setPallet)),
				A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (index, astxy) {
							return _Utils_Tuple2(
								$elm$core$String$fromInt(index),
								A3($author$project$AG2$viewASTxy, model, false, astxy));
						}),
					model.l))
			]));
};
var $author$project$AG2$main = $elm$browser$Browser$element(
	{be: $author$project$AG2$init, bx: $author$project$AG2$subscriptions, bD: $author$project$AG2$update, bE: $author$project$AG2$view});
_Platform_export({'AG2':{'init':$author$project$AG2$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));