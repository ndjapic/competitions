program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults, math;
var
	n, t, p, i, li: int8;
	l: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, t, p);
	p := min(p, n);

	l := tlist<int8>.create;
	for i := 0 to n-1 do begin
		read(li);
		l.add(li);
		l.exchange(i, random(i+1));
	end;
	readln;
	l.sort;

	writeln(max(0, t - l[n-p]));
	l.free;
end.
