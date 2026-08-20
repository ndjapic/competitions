program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashset
uses
	generics.collections;
var
	i, a: int8;
	d: tdictionary<int8, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	d := tdictionary<int8, boolean>.create;

	for i := 1 to 5 do begin
		read(a);
		d.addorsetvalue(a, true);
	end;
	readln;

	writeln(d.count);
	d.free;
end.
