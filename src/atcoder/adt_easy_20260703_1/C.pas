program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	NN = 100;
var
	n, i, a: int8;
	ans: int32;
	s: char;
	l, r: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := tlist<int8>.create;
	r := tlist<int8>.create;

	for i := 1 to n do begin
		readln(a, s, s);
		case s of
			'L': l.add(a);
			'R': r.add(a);
		end;
	end;

	ans := 0;
	for i := 1 to l.count - 1 do inc(ans, abs(l[i] - l[i-1]));
	for i := 1 to r.count - 1 do inc(ans, abs(r[i] - r[i-1]));
	writeln(ans);

	l.free;
	r.free;
end.
