program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, math;
var
	n, i, a, c, v, h0: int32;
	d: tdictionary<int32, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;
	h0 := random(1 shl 30);

	readln(n);

	d := tdictionary<int32, int32>.create;
	for i := 1 to n do begin
		readln(a, c);
		c := c xor h0;
		if d.trygetvalue(c, v) then a := min(v, a);
		d.addorsetvalue(c, a);
	end;

	v := 0;
	for c in d.keys do v := max(v, d[c]);
	writeln(v);
	d.free;
end.
