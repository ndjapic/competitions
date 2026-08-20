program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
var
	n, i, a: int32;
	d: tdictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	d := tdictionary<int32, boolean>.create;
	for i := 1 to n do begin
		read(a);
		d.addorsetvalue(a, true);
	end;
	readln;

	writeln(d.count);
	d.free;
end.
