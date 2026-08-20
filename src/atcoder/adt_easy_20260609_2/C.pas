program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	NN = 100;
var
	n, i, j: int8;
	s: array [1 .. NN] of string;
	d: tdictionary<string, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	d := tdictionary<string, boolean>.create;

	for i := 1 to n do begin
		readln(s[i]);
		for j := 1 to i-1 do begin
			d.AddOrSetValue(s[i] + s[j], true);
			d.AddOrSetValue(s[j] + s[i], true);
		end;
	end;

	writeln(d.count);
	d.free;
end.
